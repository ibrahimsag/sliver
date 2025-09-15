#include <SDL.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

#define WINDOW_WIDTH 800
#define WINDOW_HEIGHT 600
#define BOUNDING_SIZE 400
#define CORNER_RADIUS 20
#define HIGHLIGHT_SIZE 8
#define MAX_SQUARES 10

// Vector 2D struct and operations
typedef struct {
    float x, y;
} V2;

V2 v2_add(V2 a, V2 b) { return (V2){a.x + b.x, a.y + b.y}; }
V2 v2_sub(V2 a, V2 b) { return (V2){a.x - b.x, a.y - b.y}; }
V2 v2_scale(V2 v, float s) { return (V2){v.x * s, v.y * s}; }
V2 v2_lerp(V2 a, V2 b, float t) { return v2_add(a, v2_scale(v2_sub(b, a), t)); }
float v2_dist(V2 a, V2 b) { V2 d = v2_sub(b, a); return sqrtf(d.x * d.x + d.y * d.y); }
float v2_length(V2 v) { return sqrtf(v.x * v.x + v.y * v.y); }

typedef struct {
    V2 start, end;
} Diagonal;

typedef struct {
    float t;          // position along diagonal [0,1]
    float size;       // size factor [0,1]
    SDL_Color color;
} Square;

typedef enum {
    CORNER_TL = 0,
    CORNER_TR = 1,
    CORNER_BR = 2,
    CORNER_BL = 3,
    CORNER_NONE = -1
} Corner;

typedef struct {
    SDL_Window* window;
    SDL_Renderer* renderer;
    V2 bounding_center;
    float bounding_half;
    Corner selected_corner;
    Diagonal diagonal;
    Square squares[MAX_SQUARES];
    int num_squares;
    bool running;
} AppState;

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

void draw_line(SDL_Renderer* renderer, V2 p1, V2 p2) {
    SDL_RenderDrawLine(renderer, p1.x, p1.y, p2.x, p2.y);
}

void draw_square(SDL_Renderer* renderer, V2 p1, V2 p2) {
    // Draw axis-aligned square given diagonal endpoints
    float min_x = fminf(p1.x, p2.x);
    float max_x = fmaxf(p1.x, p2.x);
    float min_y = fminf(p1.y, p2.y);
    float max_y = fmaxf(p1.y, p2.y);
    
    SDL_Rect rect = {(int)min_x, (int)min_y, (int)(max_x - min_x), (int)(max_y - min_y)};
    SDL_RenderDrawRect(renderer, &rect);
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
    draw_line(state->renderer, tl, tr);
    draw_line(state->renderer, tr, br);
    draw_line(state->renderer, br, bl);
    draw_line(state->renderer, bl, tl);
    
    // Draw corner indicators
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(state, i);
        if (i == state->selected_corner) {
            SDL_SetRenderDrawColor(state->renderer, 255, 200, 100, 255);
            draw_circle(state->renderer, corner, HIGHLIGHT_SIZE);
        } else {
            SDL_SetRenderDrawColor(state->renderer, 150, 150, 150, 255);
            draw_circle(state->renderer, corner, HIGHLIGHT_SIZE / 2);
        }
    }
    
    if (state->selected_corner != CORNER_NONE) {
        // Draw diagonal
        SDL_SetRenderDrawColor(state->renderer, 200, 200, 255, 255);
        draw_line(state->renderer, state->diagonal.start, state->diagonal.end);
        
        // Draw orientation edge from selected corner
        V2 selected = get_corner_position(state, state->selected_corner);
        SDL_SetRenderDrawColor(state->renderer, 255, 100, 100, 255);
        draw_line(state->renderer, selected, state->diagonal.start);
        
        // Draw squares along diagonal
        SDL_SetRenderDrawColor(state->renderer, 100, 255, 100, 255);
        for (int i = 0; i < state->num_squares; i++) {
            Square* sq = &state->squares[i];
            V2 center = v2_lerp(state->diagonal.start, state->diagonal.end, sq->t);
            float half_diag = sq->size * v2_dist(state->diagonal.start, state->diagonal.end) / 2;
            
            // For 45-degree diagonal, offset is same in x and y
            float offset = half_diag / sqrtf(2);
            V2 p1 = (V2){center.x - offset, center.y - offset};
            V2 p2 = (V2){center.x + offset, center.y + offset};
            
            SDL_SetRenderDrawColor(state->renderer, sq->color.r, sq->color.g, sq->color.b, sq->color.a);
            draw_square(state->renderer, p1, p2);
        }
    }
    
    SDL_RenderPresent(state->renderer);
}

Corner detect_corner_click(AppState* state, V2 mouse) {
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(state, i);
        if (v2_dist(mouse, corner) <= CORNER_RADIUS) {
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
    // Initialize some example squares
    state->num_squares = 5;
    for (int i = 0; i < state->num_squares; i++) {
        state->squares[i].t = (float)i / (state->num_squares - 1);
        state->squares[i].size = 0.1f + 0.15f * (1.0f - fabsf(state->squares[i].t - 0.5f) * 2);
        state->squares[i].color = (SDL_Color){
            100 + i * 30,
            200 - i * 20,
            150 + i * 20,
            255
        };
    }
}

int main(int argc, char* argv[]) {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 1;
    }
    
    AppState state = {0};
    state.running = true;
    state.selected_corner = CORNER_TL;  // Start with top-left selected
    state.bounding_center = (V2){WINDOW_WIDTH / 2, WINDOW_HEIGHT / 2};
    state.bounding_half = BOUNDING_SIZE / 2;
    
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
    
    SDL_Event event;
    while (state.running) {
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
                case SDL_QUIT:
                    state.running = false;
                    break;
                    
                case SDL_MOUSEBUTTONDOWN:
                    if (event.button.button == SDL_BUTTON_LEFT) {
                        handle_mouse_click(&state, event.button.x, event.button.y);
                    }
                    break;
                    
                case SDL_KEYDOWN:
                    switch (event.key.keysym.sym) {
                        case SDLK_ESCAPE:
                        case SDLK_q:
                            state.running = false;
                            break;
                        case SDLK_SPACE:
                            // Randomize square positions
                            for (int i = 0; i < state.num_squares; i++) {
                                state.squares[i].t = (float)rand() / RAND_MAX;
                            }
                            break;
                        case SDLK_r:
                            // Reset squares
                            init_squares(&state);
                            break;
                    }
                    break;
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