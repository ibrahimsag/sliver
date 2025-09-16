#include <SDL.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

#define WINDOW_WIDTH 1920
#define WINDOW_HEIGHT 1080
#define BOUNDING_SIZE 900
#define BOUNDING_PADDING 50
#define CORNER_RADIUS 30
#define HIGHLIGHT_SIZE 12
#define MAX_SQUARES 30

// Vector 2D struct and operations
typedef struct {
    float x, y;
} V2;

typedef struct {
    float start, end;
} Interval;

V2 v2_add(V2 a, V2 b) { return (V2){a.x + b.x, a.y + b.y}; }
V2 v2_sub(V2 a, V2 b) { return (V2){a.x - b.x, a.y - b.y}; }
V2 v2_scale(V2 v, float s) { return (V2){v.x * s, v.y * s}; }
V2 v2_lerp(V2 a, V2 b, float t) { return v2_add(a, v2_scale(v2_sub(b, a), t)); }
float v2_dist(V2 a, V2 b) { V2 d = v2_sub(b, a); return sqrtf(d.x * d.x + d.y * d.y); }
float v2_length(V2 v) { return sqrtf(v.x * v.x + v.y * v.y); }

typedef struct {
    V2 start, end;
} Diagonal;

typedef enum {
    KIND_UNIT = 0,      // Sequence 1: unit squares
    KIND_OFFSET = 1,    // Sequence 2: offset squares
    KIND_HALF = 2,      // Sequence 3: half-unit squares
    KIND_LARGE = 3,     // Sequence 4: large square
    KIND_COUNT = 4
} SquareKind;

typedef struct {
    Interval interval;  // position along diagonal [0,1]
    SDL_Color color;
    SquareKind kind;
} Square;

typedef enum {
    CORNER_TL = 0,
    CORNER_TR = 1,
    CORNER_BR = 2,
    CORNER_BL = 3,
    CORNER_NONE = -1
} Corner;

typedef struct {
    V2 offset;  // Camera translation in world coordinates
    float scale;  // Zoom level (1.0 = default, >1 = zoomed in, <1 = zoomed out)
} Camera;

// Sliver: A 2D view/projection of a 1D parameter space
// Maps the 1D parameter t to a visible window in 2D space
typedef struct {
    float offset;  // Offset in parameter space (shifts the visible range)
    float scale;   // Scale in parameter space (1.0 = show full [0,1], 0.5 = show twice as much)
} SliverCamera;

typedef struct {
    SDL_Window* window;
    SDL_Renderer* renderer;
    V2 bounding_center;
    float bounding_half;
    Corner selected_corner;
    Diagonal diagonal;
    Square squares[MAX_SQUARES];
    int num_squares;
    SquareKind selected_kind;
    SquareKind previous_kind;  // For exit animation
    float radius_animation;  // 0 to 1 animation progress
    Uint32 animation_start_time;
    bool animating;
    Camera camera;  // For 2D viewport movement
    SliverCamera sliver_camera;  // For 1D parameter space windowing
    bool dragging;  // For mouse drag panning
    V2 drag_start;  // Mouse position when drag started
    V2 camera_start;  // Camera offset when drag started
    bool running;
} AppState;

// Transform world coordinates to screen coordinates using camera
V2 world_to_screen(V2 world, Camera* camera) {
    V2 centered = v2_sub(world, camera->offset);
    V2 scaled = v2_scale(centered, camera->scale);
    V2 screen = v2_add(scaled, (V2){WINDOW_WIDTH / 2.0f, WINDOW_HEIGHT / 2.0f});
    return screen;
}

// Transform screen coordinates to world coordinates using camera
V2 screen_to_world(V2 screen, Camera* camera) {
    V2 centered = v2_sub(screen, (V2){WINDOW_WIDTH / 2.0f, WINDOW_HEIGHT / 2.0f});
    V2 unscaled = v2_scale(centered, 1.0f / camera->scale);
    V2 world = v2_add(unscaled, camera->offset);
    return world;
}

// Sliver transformations: Apply 1D windowing to parameter space
float sliver_transform(float t, SliverCamera* sliver) {
    // Transform parameter t through the sliver camera
    // offset shifts the window, scale zooms in/out
    return (t - sliver->offset) * sliver->scale;
}

float sliver_inverse(float t_screen, SliverCamera* sliver) {
    // Inverse transform from screen parameter space back to world parameter space
    return (t_screen / sliver->scale) + sliver->offset;
}

Interval sliver_transform_interval(Interval interval, SliverCamera* sliver) {
    return (Interval){
        .start = sliver_transform(interval.start, sliver),
        .end = sliver_transform(interval.end, sliver)
    };
}

V2 get_corner_position(AppState* state, Corner corner) {
    float left = state->bounding_center.x - state->bounding_half;
    float right = state->bounding_center.x + state->bounding_half;
    float top = state->bounding_center.y - state->bounding_half;
    float bottom = state->bounding_center.y + state->bounding_half;
    
    switch (corner) {
        case CORNER_TL: return (V2){left, top};
        case CORNER_TR: return (V2){right, top};
        case CORNER_BR: return (V2){right, bottom};
        case CORNER_BL: return (V2){left, bottom};
        default: return (V2){0, 0};
    }
}

void calculate_diagonal(AppState* state) {
    if (state->selected_corner == CORNER_NONE) return;
    
    V2 tl = get_corner_position(state, CORNER_TL);
    V2 tr = get_corner_position(state, CORNER_TR);
    V2 br = get_corner_position(state, CORNER_BR);
    V2 bl = get_corner_position(state, CORNER_BL);
    
    // Based on selected corner, set diagonal endpoints
    switch (state->selected_corner) {
        case CORNER_TL:  // Triangle (BL, TR, TL) -> diagonal BL to TR
            state->diagonal.start = bl;
            state->diagonal.end = tr;
            break;
        case CORNER_TR:  // Triangle (TL, BR, TR) -> diagonal TL to BR
            state->diagonal.start = tl;
            state->diagonal.end = br;
            break;
        case CORNER_BR:  // Triangle (TR, BL, BR) -> diagonal TR to BL
            state->diagonal.start = tr;
            state->diagonal.end = bl;
            break;
        case CORNER_BL:  // Triangle (BR, TL, BL) -> diagonal BR to TL
            state->diagonal.start = br;
            state->diagonal.end = tl;
            break;
        default:
            break;
    }
}

// Bounce easing function - creates a bouncing effect at the end
float ease_out_bounce(float t) {
    if (t < 1 / 2.75f) {
        return 7.5625f * t * t;
    } else if (t < 2 / 2.75f) {
        t -= 1.5f / 2.75f;
        return 7.5625f * t * t + 0.75f;
    } else if (t < 2.5 / 2.75f) {
        t -= 2.25f / 2.75f;
        return 7.5625f * t * t + 0.9375f;
    } else {
        t -= 2.625f / 2.75f;
        return 7.5625f * t * t + 0.984375f;
    }
}

// Forward declarations
void draw_rounded_rect(SDL_Renderer* renderer, float x, float y, float w, float h, float radius);
void draw_circle(SDL_Renderer* renderer, V2 center, float radius);

void draw_line(SDL_Renderer* renderer, V2 p1, V2 p2) {
    SDL_RenderDrawLine(renderer, p1.x, p1.y, p2.x, p2.y);
}

// Camera-aware drawing functions that read camera from state
void draw_line_cam(AppState* state, V2 p1, V2 p2) {
    V2 s1 = world_to_screen(p1, &state->camera);
    V2 s2 = world_to_screen(p2, &state->camera);
    draw_line(state->renderer, s1, s2);
}

void draw_square_cam(AppState* state, V2 p1, V2 p2, bool rounded, float animated_radius_factor) {
    V2 s1 = world_to_screen(p1, &state->camera);
    V2 s2 = world_to_screen(p2, &state->camera);
    
    float min_x = fminf(s1.x, s2.x);
    float max_x = fmaxf(s1.x, s2.x);
    float min_y = fminf(s1.y, s2.y);
    float max_y = fmaxf(s1.y, s2.y);
    
    if (rounded) {
        float base_radius = fminf(25.0f, fminf(max_x - min_x, max_y - min_y) * 0.2f);
        float radius = base_radius * animated_radius_factor;
        float extend = radius * (1.0f - 1.0f/sqrtf(2));
        draw_rounded_rect(state->renderer, min_x - extend, min_y - extend, 
                         (max_x - min_x) + 2 * extend, (max_y - min_y) + 2 * extend, radius);
    } else {
        SDL_Rect rect = {(int)min_x, (int)min_y, (int)(max_x - min_x), (int)(max_y - min_y)};
        SDL_RenderDrawRect(state->renderer, &rect);
    }
}

void draw_circle_cam(AppState* state, V2 center, float radius) {
    V2 s_center = world_to_screen(center, &state->camera);
    float s_radius = radius * state->camera.scale;
    
    const int segments = 32;
    for (int i = 0; i < segments; i++) {
        float angle1 = (2.0f * M_PI * i) / segments;
        float angle2 = (2.0f * M_PI * (i + 1)) / segments;
        draw_line(state->renderer, 
                 (V2){s_center.x + s_radius * cosf(angle1), s_center.y + s_radius * sinf(angle1)},
                 (V2){s_center.x + s_radius * cosf(angle2), s_center.y + s_radius * sinf(angle2)});
    }
}

void draw_rounded_rect(SDL_Renderer* renderer, float x, float y, float w, float h, float radius) {
    // Draw rounded rectangle using lines
    int segments = 32;
    
    // Draw four corners with correct positioning
    // Top-left corner (180 to 270 degrees)
    float cx = x + radius;
    float cy = y + radius;
    for (int i = 0; i < segments; i++) {
        float angle1 = M_PI + (M_PI / 2) * i / segments;
        float angle2 = M_PI + (M_PI / 2) * (i + 1) / segments;
        SDL_RenderDrawLine(renderer,
                          cx + radius * cosf(angle1),
                          cy + radius * sinf(angle1),
                          cx + radius * cosf(angle2),
                          cy + radius * sinf(angle2));
    }
    
    // Top-right corner (270 to 360 degrees)
    cx = x + w - radius;
    cy = y + radius;
    for (int i = 0; i < segments; i++) {
        float angle1 = 3 * M_PI / 2 + (M_PI / 2) * i / segments;
        float angle2 = 3 * M_PI / 2 + (M_PI / 2) * (i + 1) / segments;
        SDL_RenderDrawLine(renderer,
                          cx + radius * cosf(angle1),
                          cy + radius * sinf(angle1),
                          cx + radius * cosf(angle2),
                          cy + radius * sinf(angle2));
    }
    
    // Bottom-right corner (0 to 90 degrees)
    cx = x + w - radius;
    cy = y + h - radius;
    for (int i = 0; i < segments; i++) {
        float angle1 = (M_PI / 2) * i / segments;
        float angle2 = (M_PI / 2) * (i + 1) / segments;
        SDL_RenderDrawLine(renderer,
                          cx + radius * cosf(angle1),
                          cy + radius * sinf(angle1),
                          cx + radius * cosf(angle2),
                          cy + radius * sinf(angle2));
    }
    
    // Bottom-left corner (90 to 180 degrees)
    cx = x + radius;
    cy = y + h - radius;
    for (int i = 0; i < segments; i++) {
        float angle1 = M_PI / 2 + (M_PI / 2) * i / segments;
        float angle2 = M_PI / 2 + (M_PI / 2) * (i + 1) / segments;
        SDL_RenderDrawLine(renderer,
                          cx + radius * cosf(angle1),
                          cy + radius * sinf(angle1),
                          cx + radius * cosf(angle2),
                          cy + radius * sinf(angle2));
    }
    
    // Draw straight edges
    SDL_RenderDrawLine(renderer, x + radius, y, x + w - radius, y);  // Top
    SDL_RenderDrawLine(renderer, x + radius, y + h, x + w - radius, y + h);  // Bottom
    SDL_RenderDrawLine(renderer, x, y + radius, x, y + h - radius);  // Left
    SDL_RenderDrawLine(renderer, x + w, y + radius, x + w, y + h - radius);  // Right
}

void draw_square(SDL_Renderer* renderer, V2 p1, V2 p2, bool rounded, float animated_radius_factor) {
    // Draw axis-aligned square given diagonal endpoints
    float min_x = fminf(p1.x, p2.x);
    float max_x = fmaxf(p1.x, p2.x);
    float min_y = fminf(p1.y, p2.y);
    float max_y = fmaxf(p1.y, p2.y);
    
    if (rounded) {
        float base_radius = fminf(30.0f, fminf(max_x - min_x, max_y - min_y) * 0.5f);
        float radius = base_radius * animated_radius_factor;
        // The 45° point on the arc is inset by r*(1-1/√2) from the corner
        // So we need to extend the rectangle by this amount to make arcs meet at diagonal points
        float extend = radius * (1.0f - 1.0f/sqrtf(2));
        draw_rounded_rect(renderer, min_x - extend, min_y - extend, 
                         (max_x - min_x) + 2 * extend, (max_y - min_y) + 2 * extend, radius);
    } else {
        SDL_Rect rect = {(int)min_x, (int)min_y, (int)(max_x - min_x), (int)(max_y - min_y)};
        SDL_RenderDrawRect(renderer, &rect);
    }
}

void draw_circle(SDL_Renderer* renderer, V2 center, float radius) {
    const int segments = 32;
    for (int i = 0; i < segments; i++) {
        float angle1 = (2.0f * M_PI * i) / segments;
        float angle2 = (2.0f * M_PI * (i + 1)) / segments;
        SDL_RenderDrawLine(renderer,
                          center.x + radius * cosf(angle1),
                          center.y + radius * sinf(angle1),
                          center.x + radius * cosf(angle2),
                          center.y + radius * sinf(angle2));
    }
}

void render(AppState* state) {
    // Clear screen
    SDL_SetRenderDrawColor(state->renderer, 30, 30, 30, 255);
    SDL_RenderClear(state->renderer);
    
    // Draw bounding square outline
    SDL_SetRenderDrawColor(state->renderer, 100, 100, 100, 255);
    V2 tl = get_corner_position(state, CORNER_TL);
    V2 tr = get_corner_position(state, CORNER_TR);
    V2 br = get_corner_position(state, CORNER_BR);
    V2 bl = get_corner_position(state, CORNER_BL);
    draw_line_cam(state, tl, tr);
    draw_line_cam(state, tr, br);
    draw_line_cam(state, br, bl);
    draw_line_cam(state, bl, tl);
    
    // Draw corner indicators
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(state, i);
        if (i == state->selected_corner) {
            SDL_SetRenderDrawColor(state->renderer, 255, 200, 100, 255);
            draw_circle_cam(state, corner, HIGHLIGHT_SIZE);
        } else {
            SDL_SetRenderDrawColor(state->renderer, 150, 150, 150, 255);
            draw_circle_cam(state, corner, HIGHLIGHT_SIZE / 2);
        }
    }
    
    if (state->selected_corner != CORNER_NONE) {
        // Draw diagonal
        SDL_SetRenderDrawColor(state->renderer, 200, 200, 255, 255);
        draw_line_cam(state, state->diagonal.start, state->diagonal.end);
        
        // Draw squares along diagonal
        for (int i = 0; i < state->num_squares; i++) {
            Square* sq = &state->squares[i];
            
            // Apply sliver transform to the square's interval
            Interval transformed = sliver_transform_interval(sq->interval, &state->sliver_camera);
            
            // Skip squares completely outside the visible range
            if ((transformed.start > 1.0f && transformed.end > 1.0f) || 
                (transformed.start < 0.0f && transformed.end < 0.0f)) {
                continue;
            }
            
            // Clamp to visible range [0, 1]
            transformed.start = fmaxf(0.0f, fminf(1.0f, transformed.start));
            transformed.end = fmaxf(0.0f, fminf(1.0f, transformed.end));
            
            // Get the two diagonal endpoints for this square
            V2 p1 = v2_lerp(state->diagonal.start, state->diagonal.end, transformed.start);
            V2 p2 = v2_lerp(state->diagonal.start, state->diagonal.end, transformed.end);
            
            SDL_SetRenderDrawColor(state->renderer, sq->color.r, sq->color.g, sq->color.b, sq->color.a);
            
            // Animate both entering and exiting kinds
            bool should_round = false;
            float radius_factor = 1.0f;
            
            if (sq->kind == state->selected_kind) {
                should_round = true;
                radius_factor = state->radius_animation;  // Animating in
            } else if (state->animating && sq->kind == state->previous_kind) {
                should_round = true;
                radius_factor = 1.0f - state->radius_animation;  // Animating out
            }
            
            draw_square_cam(state, p1, p2, should_round, radius_factor);
        }
        
        // Draw orientation edge from selected corner
        V2 selected = get_corner_position(state, state->selected_corner);
        SDL_SetRenderDrawColor(state->renderer, 255, 100, 100, 255);
        draw_line_cam(state, selected, state->diagonal.end);
    }
    
    SDL_RenderPresent(state->renderer);
}

Corner detect_corner_click(AppState* state, V2 mouse) {
    V2 world_mouse = screen_to_world(mouse, &state->camera);
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(state, i);
        if (v2_dist(world_mouse, corner) <= CORNER_RADIUS) {
            return i;
        }
    }
    return CORNER_NONE;
}

void handle_mouse_click(AppState* state, int x, int y) {
    V2 mouse = {(float)x, (float)y};
    Corner clicked = detect_corner_click(state, mouse);
    
    if (clicked != CORNER_NONE) {
        state->selected_corner = clicked;
        calculate_diagonal(state);
    }
}

void init_squares(AppState* state) {
    int idx = 0;
    
    // Sequence 1: 10 unit squares (0-1, 1-2, ..., 9-10) in dark gray
    for (int i = 0; i < 10; i++) {
        state->squares[idx].interval.start = i / 10.0f;
        state->squares[idx].interval.end = (i + 1) / 10.0f;
        state->squares[idx].color = (SDL_Color){60, 60, 60, 255};  // Dark gray
        state->squares[idx].kind = KIND_UNIT;
        idx++;
    }
    
    // Sequence 2: 8 squares of size 1 (0.3-1.3, 1.3-2.3, ...)
    for (int i = 0; i < 8; i++) {
        state->squares[idx].interval.start = (i + 0.3f) / 10.0f;
        state->squares[idx].interval.end = (i + 1.3f) / 10.0f;
        state->squares[idx].color = (SDL_Color){100, 150, 200, 255};  // Blue
        state->squares[idx].kind = KIND_OFFSET;
        idx++;
    }
    
    // Sequence 3: 5 squares of size 0.5 (0.2-0.7, 1.2-1.7, ...)
    for (int i = 0; i < 5; i++) {
        state->squares[idx].interval.start = (i + 0.2f) / 10.0f;
        state->squares[idx].interval.end = (i + 0.7f) / 10.0f;
        state->squares[idx].color = (SDL_Color){200, 150, 100, 255};  // Orange
        state->squares[idx].kind = KIND_HALF;
        idx++;
    }
    
    // Sequence 4: single square at 6.6-8.7
    state->squares[idx].interval.start = 6.6f / 10.0f;
    state->squares[idx].interval.end = 8.7f / 10.0f;
    state->squares[idx].color = (SDL_Color){150, 200, 150, 255};  // Green
    state->squares[idx].kind = KIND_LARGE;
    idx++;
    
    state->num_squares = idx;
}

void start_animation(AppState* state) {
    state->animating = true;
    state->animation_start_time = SDL_GetTicks();
    state->radius_animation = 0.0f;
}

int main(int argc, char* argv[]) {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 1;
    }
    
    AppState state = {0};
    state.running = true;
    state.selected_corner = CORNER_BR;  // Start with bottom-right selected
    state.selected_kind = KIND_UNIT;    // Start with unit squares selected
    state.previous_kind = KIND_UNIT;    // Initialize previous kind
    state.bounding_center = (V2){WINDOW_WIDTH / 2, WINDOW_HEIGHT / 2};
    state.bounding_half = (BOUNDING_SIZE - BOUNDING_PADDING) / 2;
    
    // Initialize camera
    state.camera.offset = (V2){WINDOW_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center of world
    state.camera.scale = 1.0f;  // Default zoom
    state.dragging = false;
    
    // Initialize sliver camera (shows full parameter range by default)
    state.sliver_camera.offset = 0.0f;  // No offset, centered at origin
    state.sliver_camera.scale = 1.0f;   // Show full [0,1] range
    
    // Create window with HiDPI support (from ../sd) 
    state.window = SDL_CreateWindow("Squares on Diagonal",
                                   SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                   WINDOW_WIDTH, WINDOW_HEIGHT,
                                   SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_RESIZABLE);
    if (!state.window) {
        fprintf(stderr, "SDL_CreateWindow failed: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }
    
    state.renderer = SDL_CreateRenderer(state.window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!state.renderer) {
        fprintf(stderr, "SDL_CreateRenderer failed: %s\n", SDL_GetError());
        SDL_DestroyWindow(state.window);
        SDL_Quit();
        return 1;
    }
    
    // Set up logical size for consistent coordinates
    SDL_RenderSetLogicalSize(state.renderer, WINDOW_WIDTH, WINDOW_HEIGHT);
    
    // Check for HiDPI
    int window_w, window_h;
    int render_w, render_h;
    SDL_GetWindowSize(state.window, &window_w, &window_h);
    SDL_GetRendererOutputSize(state.renderer, &render_w, &render_h);
    printf("Window: %dx%d, Renderer: %dx%d\n", window_w, window_h, render_w, render_h);
    if (render_w != window_w || render_h != window_h) {
        printf("HiDPI detected: scale factor %.2fx\n", (float)render_w / window_w);
    }
    
    calculate_diagonal(&state);
    init_squares(&state);
    start_animation(&state);  // Start initial animation
    
    SDL_Event event;
    while (state.running) {
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
                case SDL_QUIT:
                    state.running = false;
                    break;
                    
                case SDL_MOUSEBUTTONDOWN:
                    if (event.button.button == SDL_BUTTON_LEFT && !(SDL_GetModState() & KMOD_SHIFT)) {
                        handle_mouse_click(&state, event.button.x, event.button.y);
                    } else if (event.button.button == SDL_BUTTON_RIGHT || 
                              event.button.button == SDL_BUTTON_MIDDLE ||
                              (event.button.button == SDL_BUTTON_LEFT && (SDL_GetModState() & KMOD_SHIFT))) {
                        // Start dragging with right/middle mouse or shift+left click
                        state.dragging = true;
                        state.drag_start = (V2){(float)event.button.x, (float)event.button.y};
                        state.camera_start = state.camera.offset;
                    }
                    break;
                    
                case SDL_MOUSEBUTTONUP:
                    if (event.button.button == SDL_BUTTON_RIGHT ||
                        event.button.button == SDL_BUTTON_MIDDLE ||
                        event.button.button == SDL_BUTTON_LEFT) {
                        state.dragging = false;
                    }
                    break;
                    
                case SDL_MOUSEMOTION:
                    if (state.dragging) {
                        V2 mouse = {(float)event.motion.x, (float)event.motion.y};
                        V2 delta = v2_scale(v2_sub(mouse, state.drag_start), 1.0f / state.camera.scale);
                        state.camera.offset = v2_sub(state.camera_start, delta);
                    }
                    break;
                    
                case SDL_MOUSEWHEEL:
                    {
                        // Get mouse position for zoom center
                        int mouse_x, mouse_y;
                        SDL_GetMouseState(&mouse_x, &mouse_y);
                        V2 mouse_screen = {(float)mouse_x, (float)mouse_y};
                        V2 mouse_world_before = screen_to_world(mouse_screen, &state.camera);
                        
                        // Smoother zoom with smaller increments
                        float zoom_speed = 0.05f;
                        float zoom_factor = 1.0f + (event.wheel.y * zoom_speed);
                        state.camera.scale *= zoom_factor;
                        
                        // Clamp zoom level
                        if (state.camera.scale < 0.1f) state.camera.scale = 0.1f;
                        if (state.camera.scale > 10.0f) state.camera.scale = 10.0f;
                        
                        // Adjust camera offset to zoom towards mouse position
                        V2 mouse_world_after = screen_to_world(mouse_screen, &state.camera);
                        V2 world_diff = v2_sub(mouse_world_after, mouse_world_before);
                        state.camera.offset = v2_sub(state.camera.offset, world_diff);
                    }
                    break;
                    
                case SDL_KEYDOWN:
                    switch (event.key.keysym.sym) {
                        case SDLK_ESCAPE:
                        case SDLK_q:
                            state.running = false;
                            break;
                        case SDLK_f:
                        case SDLK_F11:
                            // Toggle fullscreen
                            {
                                Uint32 flags = SDL_GetWindowFlags(state.window);
                                if (flags & SDL_WINDOW_FULLSCREEN_DESKTOP) {
                                    SDL_SetWindowFullscreen(state.window, 0);
                                } else {
                                    SDL_SetWindowFullscreen(state.window, SDL_WINDOW_FULLSCREEN_DESKTOP);
                                }
                            }
                            break;
                        case SDLK_TAB:
                            // Cycle through square kinds and trigger animation
                            state.previous_kind = state.selected_kind;
                            state.selected_kind = (state.selected_kind + 1) % KIND_COUNT;
                            start_animation(&state);
                            break;
                        case SDLK_SPACE:
                            // Reset to original square definitions
                            init_squares(&state);
                            break;
                        case SDLK_r:
                            // Reset squares
                            init_squares(&state);
                            break;
                        
                        // Sliver camera controls
                        case SDLK_LEFT:
                            // Pan sliver view left
                            state.sliver_camera.offset -= 0.1f / state.sliver_camera.scale;
                            break;
                        case SDLK_RIGHT:
                            // Pan sliver view right
                            state.sliver_camera.offset += 0.1f / state.sliver_camera.scale;
                            break;
                        case SDLK_UP:
                            // Zoom in sliver (show less range)
                            state.sliver_camera.scale *= 1.2f;
                            if (state.sliver_camera.scale > 10.0f) state.sliver_camera.scale = 10.0f;
                            break;
                        case SDLK_DOWN:
                            // Zoom out sliver (show more range)
                            state.sliver_camera.scale /= 1.2f;
                            if (state.sliver_camera.scale < 0.1f) state.sliver_camera.scale = 0.1f;
                            break;
                        case SDLK_0:
                            // Reset sliver camera
                            state.sliver_camera.offset = 0.0f;
                            state.sliver_camera.scale = 1.0f;
                            break;
                    }
                    break;
            }
        }
        
        // Update animation
        if (state.animating) {
            Uint32 current_time = SDL_GetTicks();
            float elapsed = (current_time - state.animation_start_time) / 1000.0f;  // Convert to seconds
            float animation_duration = 1.0f;  // 1 second animation
            
            if (elapsed >= animation_duration) {
                state.radius_animation = 1.0f;
                state.animating = false;
                
            } else {
                float t = elapsed / animation_duration;
                state.radius_animation = ease_out_bounce(t);
            }
        }
        
        render(&state);
        SDL_Delay(16);  // ~60 FPS
    }
    
    SDL_DestroyRenderer(state.renderer);
    SDL_DestroyWindow(state.window);
    SDL_Quit();
    
    return 0;
}
