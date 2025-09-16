#include <SDL.h>
#include <SDL_ttf.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <stddef.h>

#define WINDOW_WIDTH 1920
#define WINDOW_HEIGHT 1080
#define VIEWPORT_WIDTH 1400  // Left side for sliver viewport
#define UI_PANEL_WIDTH (WINDOW_WIDTH - VIEWPORT_WIDTH)  // Right side for UI
#define BOUNDING_SIZE 900
#define BOUNDING_PADDING 50
#define CORNER_RADIUS 30
#define HIGHLIGHT_SIZE 12

// Vector 2D struct and operations
typedef struct {
    float x, y;
} V2;

typedef struct {
    V2 next;  // Next position to draw at
    V2 max;   // Maximum bounds for layout
} Layout;

typedef struct {
    float start, end;
} Interval;

typedef enum {
    KIND_SHARP = 0,     // Sharp corners
    KIND_ROUNDED = 1,   // Rounded corners
    KIND_DOUBLE = 2,    // Double lines
    KIND_WAVE = 3,      // Wavy lines
    KIND_COUNT = 4
} SquareKind;

typedef struct {
    float start;   // Starting position of first interval
    float size;    // Size of each interval
    float stride;  // Distance between interval starts
    int repeat;    // Number of additional intervals (0 = single interval, n = n+1 total intervals)
    SquareKind kind;  // Drawing style: SHARP, ROUNDED, or DOUBLE
    SDL_Color color;  // Color for squares in this band
} Band;

typedef struct {
    Band* ptr;        // Pointer to dynamically allocated array
    size_t length;    // Number of bands currently in use
    size_t capacity;  // Total allocated capacity
} BandArray;

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
    Interval interval;  // position along diagonal [0,1]
    SDL_Color color;
    SquareKind kind;  // Drawing style: SHARP, ROUNDED, or DOUBLE
} Square;

typedef struct {
    Square* ptr;      // Pointer to dynamically allocated array
    size_t length;    // Number of squares currently in use
    size_t capacity;  // Total allocated capacity
} SquareArray;

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

// Geometry buffer for efficient rendering
typedef struct {
    SDL_Vertex* vertices;
    int* indices;
    size_t vertex_count;
    size_t index_count;
    size_t vertex_capacity;
    size_t index_capacity;
} GeometryBuffer;

typedef struct {
    GeometryBuffer geometry;
    SDL_Texture* white_texture;  // 1x1 white texture for solid colors
} RenderContext;

typedef struct {
    SDL_Window* window;
    SDL_Renderer* renderer;
    TTF_Font* font;
    RenderContext render_ctx;
    V2 bounding_center;
    float bounding_half;
    Corner selected_corner;
    Diagonal diagonal;
    BandArray bands;      // Dynamic array of band definitions
    SquareArray squares;  // Dynamic array of squares
    Camera camera;  // For 2D viewport movement
    SliverCamera sliver_camera;  // For 1D parameter space windowing
    bool dragging;  // For mouse drag panning
    V2 drag_start;  // Mouse position when drag started
    V2 camera_start;  // Camera offset when drag started
    V2 mouse_pos;  // Current mouse position
    bool mouse_pressed;  // Is left mouse button pressed this frame
    bool mouse_was_pressed;  // Was left mouse button pressed last frame
    bool running;
} AppState;

// Transform world coordinates to screen coordinates using camera (within viewport)
V2 world_to_screen(V2 world, Camera* camera) {
    V2 centered = v2_sub(world, camera->offset);
    V2 scaled = v2_scale(centered, camera->scale);
    V2 screen = v2_add(scaled, (V2){VIEWPORT_WIDTH / 2.0f, WINDOW_HEIGHT / 2.0f});
    return screen;
}

// Transform screen coordinates to world coordinates using camera (within viewport)
V2 screen_to_world(V2 screen, Camera* camera) {
    V2 centered = v2_sub(screen, (V2){VIEWPORT_WIDTH / 2.0f, WINDOW_HEIGHT / 2.0f});
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
void render_band_summaries(AppState* state);
void generate_squares_from_bands(AppState* state);

// Geometry buffer functions
void geometry_buffer_init(GeometryBuffer* gb, size_t initial_vertex_capacity, size_t initial_index_capacity) {
    gb->vertex_capacity = initial_vertex_capacity;
    gb->index_capacity = initial_index_capacity;
    gb->vertices = malloc(sizeof(SDL_Vertex) * gb->vertex_capacity);
    gb->indices = malloc(sizeof(int) * gb->index_capacity);
    gb->vertex_count = 0;
    gb->index_count = 0;
}

void geometry_buffer_free(GeometryBuffer* gb) {
    free(gb->vertices);
    free(gb->indices);
    gb->vertices = NULL;
    gb->indices = NULL;
    gb->vertex_capacity = 0;
    gb->index_capacity = 0;
    gb->vertex_count = 0;
    gb->index_count = 0;
}

void geometry_buffer_clear(GeometryBuffer* gb) {
    gb->vertex_count = 0;
    gb->index_count = 0;
}

void geometry_buffer_ensure_capacity(GeometryBuffer* gb, size_t vertex_count, size_t index_count) {
    if (gb->vertex_count + vertex_count > gb->vertex_capacity) {
        size_t new_capacity = gb->vertex_capacity * 2;
        while (new_capacity < gb->vertex_count + vertex_count) {
            new_capacity *= 2;
        }
        gb->vertices = realloc(gb->vertices, sizeof(SDL_Vertex) * new_capacity);
        gb->vertex_capacity = new_capacity;
    }
    
    if (gb->index_count + index_count > gb->index_capacity) {
        size_t new_capacity = gb->index_capacity * 2;
        while (new_capacity < gb->index_count + index_count) {
            new_capacity *= 2;
        }
        gb->indices = realloc(gb->indices, sizeof(int) * new_capacity);
        gb->index_capacity = new_capacity;
    }
}

// Add a thick line as a quad to the geometry buffer
void geometry_buffer_add_line(GeometryBuffer* gb, V2 p1, V2 p2, float thickness, SDL_Color color) {
    geometry_buffer_ensure_capacity(gb, 4, 6);
    
    // Calculate perpendicular direction
    V2 dir = v2_sub(p2, p1);
    float len = v2_length(dir);
    if (len == 0) return;
    
    dir = v2_scale(dir, 1.0f / len);
    V2 perp = {-dir.y * thickness * 0.5f, dir.x * thickness * 0.5f};
    
    // Add vertices
    size_t base_vertex = gb->vertex_count;
    
    gb->vertices[gb->vertex_count++] = (SDL_Vertex){
        .position = {p1.x - perp.x, p1.y - perp.y},
        .color = {color.r, color.g, color.b, color.a},
        .tex_coord = {0, 0}
    };
    gb->vertices[gb->vertex_count++] = (SDL_Vertex){
        .position = {p1.x + perp.x, p1.y + perp.y},
        .color = {color.r, color.g, color.b, color.a},
        .tex_coord = {0, 0}
    };
    gb->vertices[gb->vertex_count++] = (SDL_Vertex){
        .position = {p2.x + perp.x, p2.y + perp.y},
        .color = {color.r, color.g, color.b, color.a},
        .tex_coord = {0, 0}
    };
    gb->vertices[gb->vertex_count++] = (SDL_Vertex){
        .position = {p2.x - perp.x, p2.y - perp.y},
        .color = {color.r, color.g, color.b, color.a},
        .tex_coord = {0, 0}
    };
    
    // Add indices for two triangles
    gb->indices[gb->index_count++] = base_vertex + 0;
    gb->indices[gb->index_count++] = base_vertex + 1;
    gb->indices[gb->index_count++] = base_vertex + 2;
    
    gb->indices[gb->index_count++] = base_vertex + 0;
    gb->indices[gb->index_count++] = base_vertex + 2;
    gb->indices[gb->index_count++] = base_vertex + 3;
}

// Add an arc using multiple quads
void geometry_buffer_add_arc(GeometryBuffer* gb, V2 center, float radius, float start_angle, float end_angle, float thickness, SDL_Color color, int segments) {
    if (segments < 2) segments = 2;
    
    float angle_step = (end_angle - start_angle) / segments;
    
    for (int i = 0; i < segments; i++) {
        float a1 = start_angle + i * angle_step;
        float a2 = start_angle + (i + 1) * angle_step;
        
        float inner_radius = radius - thickness * 0.5f;
        float outer_radius = radius + thickness * 0.5f;
        
        V2 inner1 = {center.x + inner_radius * cosf(a1), center.y + inner_radius * sinf(a1)};
        V2 outer1 = {center.x + outer_radius * cosf(a1), center.y + outer_radius * sinf(a1)};
        V2 inner2 = {center.x + inner_radius * cosf(a2), center.y + inner_radius * sinf(a2)};
        V2 outer2 = {center.x + outer_radius * cosf(a2), center.y + outer_radius * sinf(a2)};
        
        // Add quad for this segment
        geometry_buffer_ensure_capacity(gb, 4, 6);
        size_t base_vertex = gb->vertex_count;
        
        gb->vertices[gb->vertex_count++] = (SDL_Vertex){
            .position = {inner1.x, inner1.y},
            .color = {color.r, color.g, color.b, color.a},
            .tex_coord = {0, 0}
        };
        gb->vertices[gb->vertex_count++] = (SDL_Vertex){
            .position = {outer1.x, outer1.y},
            .color = {color.r, color.g, color.b, color.a},
            .tex_coord = {0, 0}
        };
        gb->vertices[gb->vertex_count++] = (SDL_Vertex){
            .position = {outer2.x, outer2.y},
            .color = {color.r, color.g, color.b, color.a},
            .tex_coord = {0, 0}
        };
        gb->vertices[gb->vertex_count++] = (SDL_Vertex){
            .position = {inner2.x, inner2.y},
            .color = {color.r, color.g, color.b, color.a},
            .tex_coord = {0, 0}
        };
        
        gb->indices[gb->index_count++] = base_vertex + 0;
        gb->indices[gb->index_count++] = base_vertex + 1;
        gb->indices[gb->index_count++] = base_vertex + 2;
        
        gb->indices[gb->index_count++] = base_vertex + 0;
        gb->indices[gb->index_count++] = base_vertex + 2;
        gb->indices[gb->index_count++] = base_vertex + 3;
    }
}

void render_context_init(RenderContext* ctx, SDL_Renderer* renderer) {
    geometry_buffer_init(&ctx->geometry, 1024, 1024 * 3);
    
    // Create a 1x1 white texture for solid colors
    ctx->white_texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, 
                                           SDL_TEXTUREACCESS_STATIC, 1, 1);
    Uint32 white_pixel = 0xFFFFFFFF;
    SDL_UpdateTexture(ctx->white_texture, NULL, &white_pixel, 4);
}

void render_context_free(RenderContext* ctx) {
    geometry_buffer_free(&ctx->geometry);
    if (ctx->white_texture) {
        SDL_DestroyTexture(ctx->white_texture);
        ctx->white_texture = NULL;
    }
}

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

void render_ui_panel(AppState* state) {
    // Draw UI panel background
    SDL_SetRenderDrawColor(state->renderer, 40, 40, 45, 255);
    SDL_Rect panel_rect = {VIEWPORT_WIDTH, 0, UI_PANEL_WIDTH, WINDOW_HEIGHT};
    SDL_RenderFillRect(state->renderer, &panel_rect);
    
    // Draw panel border
    SDL_SetRenderDrawColor(state->renderer, 60, 60, 65, 255);
    SDL_RenderDrawLine(state->renderer, VIEWPORT_WIDTH, 0, VIEWPORT_WIDTH, WINDOW_HEIGHT);
    
    // Render band summaries
    render_band_summaries(state);
}

void render_text(AppState* state, const char* text, V2 position, SDL_Color color) {
    if (!state->font || !text) return;
    
    // Render at 2x size for supersampling
    SDL_Surface* surface = TTF_RenderText_Blended(state->font, text, color);
    if (!surface) return;
    
    SDL_Texture* texture = SDL_CreateTextureFromSurface(state->renderer, surface);
    if (texture) {
        // Scale down to half size for supersampling effect
        SDL_Rect dest = {
            (int)position.x,
            (int)position.y,
            surface->w / 2,  // Half width for supersampling
            surface->h / 2   // Half height for supersampling
        };
        SDL_RenderCopy(state->renderer, texture, NULL, &dest);
        SDL_DestroyTexture(texture);
    }
    SDL_FreeSurface(surface);
}

void advance_layout(Layout* layout, float height) {
    layout->next.y += height;
    if (layout->next.y > layout->max.y) {
        layout->next.y = layout->max.y;
    }
}

// Simple immediate mode button
bool render_button(AppState* state, const char* text, V2 position, V2 size, bool active) {
    SDL_Rect button_rect = {
        (int)position.x,
        (int)position.y,
        (int)size.x,
        (int)size.y
    };
    
    // Check if mouse is over button
    bool hover = state->mouse_pos.x >= button_rect.x && state->mouse_pos.x < button_rect.x + button_rect.w &&
                 state->mouse_pos.y >= button_rect.y && state->mouse_pos.y < button_rect.y + button_rect.h;
    
    // Draw button background
    if (active) {
        SDL_SetRenderDrawColor(state->renderer, 100, 150, 200, 255);
    } else if (hover) {
        SDL_SetRenderDrawColor(state->renderer, 70, 70, 75, 255);
    } else {
        SDL_SetRenderDrawColor(state->renderer, 50, 50, 55, 255);
    }
    SDL_RenderFillRect(state->renderer, &button_rect);
    
    // Draw button border
    SDL_SetRenderDrawColor(state->renderer, 90, 90, 95, 255);
    SDL_RenderDrawRect(state->renderer, &button_rect);
    
    // Draw button text centered
    if (state->font) {
        V2 text_pos = {
            position.x + size.x / 2.0f,
            position.y + size.y / 2.0f - 9  // Rough centering
        };
        
        // Measure text to center it properly
        int text_w, text_h;
        TTF_SizeText(state->font, text, &text_w, &text_h);
        text_pos.x -= (text_w / 2) / 2.0f;  // Adjust for supersampling
        
        SDL_Color text_color = active ? (SDL_Color){255, 255, 255, 255} : (SDL_Color){200, 200, 200, 255};
        render_text(state, text, text_pos, text_color);
    }
    
    // Return true if button was clicked (mouse was pressed last frame, not pressed this frame, and hovering)
    return hover && !state->mouse_pressed && state->mouse_was_pressed;
}

void render_band_summaries(AppState* state) {
    Layout layout = {
        .next = {VIEWPORT_WIDTH + 20, 40},
        .max = {WINDOW_WIDTH - 20, WINDOW_HEIGHT - 40}
    };
    
    SDL_Color white = {255, 255, 255, 255};
    SDL_Color gray = {180, 180, 180, 255};
    
    // Title
    render_text(state, "Band Summary", layout.next, white);
    advance_layout(&layout, 30);
    
    // Render each band
    char buffer[256];
    for (size_t i = 0; i < state->bands.length; i++) {
        Band* band = &state->bands.ptr[i];
        
        // Band header with band number
        snprintf(buffer, sizeof(buffer), "Band %zu:", i + 1);
        render_text(state, buffer, layout.next, band->color);
        
        // Add kind toggle button
        const char* kind_name = "Unknown";
        switch (band->kind) {
            case KIND_SHARP: kind_name = "Sharp"; break;
            case KIND_ROUNDED: kind_name = "Rounded"; break;
            case KIND_DOUBLE: kind_name = "Double"; break;
            case KIND_WAVE: kind_name = "Wave"; break;
            default: kind_name = "Unknown"; break;
        }
        
        V2 button_pos = {layout.next.x + 200, layout.next.y};
        V2 button_size = {80, 22};
        if (render_button(state, kind_name, button_pos, button_size, false)) {
            // Cycle to next kind
            band->kind = (band->kind + 1) % KIND_COUNT;
            // Regenerate squares to apply the change
            generate_squares_from_bands(state);
        }
        
        advance_layout(&layout, 25);
        
        // Band details
        snprintf(buffer, sizeof(buffer), "  Start: %.2f", band->start);
        render_text(state, buffer, layout.next, gray);
        advance_layout(&layout, 20);
        
        snprintf(buffer, sizeof(buffer), "  Size: %.2f", band->size);
        render_text(state, buffer, layout.next, gray);
        advance_layout(&layout, 20);
        
        if (band->repeat > 0) {
            snprintf(buffer, sizeof(buffer), "  Stride: %.2f", band->stride);
            render_text(state, buffer, layout.next, gray);
            advance_layout(&layout, 20);
            
            snprintf(buffer, sizeof(buffer), "  Count: %d intervals", band->repeat + 1);
            render_text(state, buffer, layout.next, gray);
            advance_layout(&layout, 20);
        } else {
            render_text(state, "  Single interval", layout.next, gray);
            advance_layout(&layout, 20);
        }
        
        advance_layout(&layout, 10);  // Space between bands
    }
}

// Draw a wavy line between two points
void geometry_buffer_add_wave_line(GeometryBuffer* gb, V2 p1, V2 p2, float thickness, float amplitude, float wavelength, SDL_Color color) {
    V2 dir = v2_sub(p2, p1);
    float length = v2_length(dir);
    if (length == 0) return;
    
    dir = v2_scale(dir, 1.0f / length);
    V2 perp = {-dir.y, dir.x};  // Perpendicular direction
    
    // Calculate number of complete wavelengths that fit
    float num_waves = length / wavelength;
    int complete_waves = (int)(num_waves + 0.5f);  // Round to nearest integer
    if (complete_waves < 1) complete_waves = 1;
    
    // Adjust wavelength to fit exactly
    float adjusted_wavelength = length / complete_waves;
    
    // Number of segments for smooth sine wave (more segments = smoother curve)
    int segments = (int)(length / 2.0f);  // One segment every 2 pixels
    if (segments < 16) segments = 16;  // Minimum 16 segments for smoothness
    
    for (int i = 0; i < segments; i++) {
        float t1 = (float)i / segments;
        float t2 = (float)(i + 1) / segments;
        
        // Calculate positions along the line
        V2 base1 = v2_lerp(p1, p2, t1);
        V2 base2 = v2_lerp(p1, p2, t2);
        
        // Calculate wave offsets using adjusted wavelength with π/2 phase offset
        float phase1 = (t1 * length / adjusted_wavelength) * 2.0f * M_PI + M_PI;
        float phase2 = (t2 * length / adjusted_wavelength) * 2.0f * M_PI + M_PI;
        float offset1 = sinf(phase1) * amplitude;
        float offset2 = sinf(phase2) * amplitude;
        
        // Apply wave offsets
        V2 wave1 = v2_add(base1, v2_scale(perp, offset1));
        V2 wave2 = v2_add(base2, v2_scale(perp, offset2));
        
        // Draw line segment
        geometry_buffer_add_line(gb, wave1, wave2, thickness, color);
    }
}

// Draw wave rectangle with geometry buffer
void draw_wave_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, SDL_Color color, Camera* camera) {
    float thickness = 2.0f;
    float amplitude = thickness * 3.0f;  // Amplitude is 4x thickness
    float wavelength = thickness * 8.0f;  // Wavelength is 8x thickness
    
    // Convert world coordinates to screen
    V2 tl = world_to_screen((V2){x, y}, camera);
    V2 tr = world_to_screen((V2){x + w, y}, camera);
    V2 br = world_to_screen((V2){x + w, y + h}, camera);
    V2 bl = world_to_screen((V2){x, y + h}, camera);
    
    // Scale amplitude and wavelength for screen space
    float screen_amplitude = amplitude * camera->scale;
    float screen_wavelength = wavelength * camera->scale;
    
    // Draw four wavy sides
    geometry_buffer_add_wave_line(gb, tl, tr, thickness, screen_amplitude, screen_wavelength, color);  // Top
    geometry_buffer_add_wave_line(gb, tr, br, thickness, screen_amplitude, screen_wavelength, color);  // Right
    geometry_buffer_add_wave_line(gb, br, bl, thickness, screen_amplitude, screen_wavelength, color);  // Bottom
    geometry_buffer_add_wave_line(gb, bl, tl, thickness, screen_amplitude, screen_wavelength, color);  // Left
}

// Draw double-line rectangle with geometry buffer
void draw_double_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, SDL_Color color, Camera* camera) {
    float thickness = 2.0f;
    float gap = thickness * 2.0f;  // Gap between lines equals thickness
    
    // Outer rectangle
    V2 tl_outer = world_to_screen((V2){x, y}, camera);
    V2 tr_outer = world_to_screen((V2){x + w, y}, camera);
    V2 br_outer = world_to_screen((V2){x + w, y + h}, camera);
    V2 bl_outer = world_to_screen((V2){x, y + h}, camera);
    
    geometry_buffer_add_line(gb, tl_outer, tr_outer, thickness, color);
    geometry_buffer_add_line(gb, tr_outer, br_outer, thickness, color);
    geometry_buffer_add_line(gb, br_outer, bl_outer, thickness, color);
    geometry_buffer_add_line(gb, bl_outer, tl_outer, thickness, color);
    
    // Inner rectangle (inset by gap)
    float inset = gap / camera->scale;  // Convert gap to world units
    V2 tl_inner = world_to_screen((V2){x + inset, y + inset}, camera);
    V2 tr_inner = world_to_screen((V2){x + w - inset, y + inset}, camera);
    V2 br_inner = world_to_screen((V2){x + w - inset, y + h - inset}, camera);
    V2 bl_inner = world_to_screen((V2){x + inset, y + h - inset}, camera);
    
    geometry_buffer_add_line(gb, tl_inner, tr_inner, thickness, color);
    geometry_buffer_add_line(gb, tr_inner, br_inner, thickness, color);
    geometry_buffer_add_line(gb, br_inner, bl_inner, thickness, color);
    geometry_buffer_add_line(gb, bl_inner, tl_inner, thickness, color);
}

// Draw rounded rectangle with geometry buffer
void draw_rounded_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, float radius, SDL_Color color, Camera* camera) {
    if (radius <= 0) {
        // Draw regular rectangle with thick lines
        V2 tl = world_to_screen((V2){x, y}, camera);
        V2 tr = world_to_screen((V2){x + w, y}, camera);
        V2 br = world_to_screen((V2){x + w, y + h}, camera);
        V2 bl = world_to_screen((V2){x, y + h}, camera);
        
        geometry_buffer_add_line(gb, tl, tr, 2.0f, color);
        geometry_buffer_add_line(gb, tr, br, 2.0f, color);
        geometry_buffer_add_line(gb, br, bl, 2.0f, color);
        geometry_buffer_add_line(gb, bl, tl, 2.0f, color);
    } else {
        // Draw rounded rectangle with arcs at corners
        V2 tl_inner = world_to_screen((V2){x + radius, y + radius}, camera);
        V2 tr_inner = world_to_screen((V2){x + w - radius, y + radius}, camera);
        V2 br_inner = world_to_screen((V2){x + w - radius, y + h - radius}, camera);
        V2 bl_inner = world_to_screen((V2){x + radius, y + h - radius}, camera);
        
        // Top line
        geometry_buffer_add_line(gb, 
            world_to_screen((V2){x + radius, y}, camera),
            world_to_screen((V2){x + w - radius, y}, camera),
            2.0f, color);
        
        // Right line
        geometry_buffer_add_line(gb,
            world_to_screen((V2){x + w, y + radius}, camera),
            world_to_screen((V2){x + w, y + h - radius}, camera),
            2.0f, color);
        
        // Bottom line
        geometry_buffer_add_line(gb,
            world_to_screen((V2){x + w - radius, y + h}, camera),
            world_to_screen((V2){x + radius, y + h}, camera),
            2.0f, color);
        
        // Left line
        geometry_buffer_add_line(gb,
            world_to_screen((V2){x, y + h - radius}, camera),
            world_to_screen((V2){x, y + radius}, camera),
            2.0f, color);
        
        // Calculate screen radius
        float screen_radius = radius * camera->scale;
        
        // Corner arcs
        geometry_buffer_add_arc(gb, tl_inner, screen_radius, M_PI, M_PI * 1.5f, 2.0f, color, 8);
        geometry_buffer_add_arc(gb, tr_inner, screen_radius, M_PI * 1.5f, M_PI * 2.0f, 2.0f, color, 8);
        geometry_buffer_add_arc(gb, br_inner, screen_radius, 0, M_PI * 0.5f, 2.0f, color, 8);
        geometry_buffer_add_arc(gb, bl_inner, screen_radius, M_PI * 0.5f, M_PI, 2.0f, color, 8);
    }
}

void render(AppState* state) {
    // Clear screen
    SDL_SetRenderDrawColor(state->renderer, 30, 30, 30, 255);
    SDL_RenderClear(state->renderer);
    
    // Clear geometry buffer for new frame
    geometry_buffer_clear(&state->render_ctx.geometry);
    
    // Set viewport clipping for left side
    SDL_Rect viewport = {0, 0, VIEWPORT_WIDTH, WINDOW_HEIGHT};
    SDL_RenderSetClipRect(state->renderer, &viewport);
    
    // Draw bounding square outline using geometry buffer
    SDL_Color gray = {100, 100, 100, 255};
    V2 tl = get_corner_position(state, CORNER_TL);
    V2 tr = get_corner_position(state, CORNER_TR);
    V2 br = get_corner_position(state, CORNER_BR);
    V2 bl = get_corner_position(state, CORNER_BL);
    
    V2 tl_s = world_to_screen(tl, &state->camera);
    V2 tr_s = world_to_screen(tr, &state->camera);
    V2 br_s = world_to_screen(br, &state->camera);
    V2 bl_s = world_to_screen(bl, &state->camera);
    
    geometry_buffer_add_line(&state->render_ctx.geometry, tl_s, tr_s, 2.0f, gray);
    geometry_buffer_add_line(&state->render_ctx.geometry, tr_s, br_s, 2.0f, gray);
    geometry_buffer_add_line(&state->render_ctx.geometry, br_s, bl_s, 2.0f, gray);
    geometry_buffer_add_line(&state->render_ctx.geometry, bl_s, tl_s, 2.0f, gray);
    
    // Draw corner indicators using geometry buffer
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(state, i);
        V2 corner_s = world_to_screen(corner, &state->camera);
        if (i == state->selected_corner) {
            SDL_Color highlight = {255, 200, 100, 255};
            geometry_buffer_add_arc(&state->render_ctx.geometry, corner_s, 
                                   HIGHLIGHT_SIZE * state->camera.scale, 
                                   0, M_PI * 2.0f, 2.0f, highlight, 16);
        } else {
            SDL_Color gray = {150, 150, 150, 255};
            geometry_buffer_add_arc(&state->render_ctx.geometry, corner_s,
                                   (HIGHLIGHT_SIZE / 2) * state->camera.scale,
                                   0, M_PI * 2.0f, 2.0f, gray, 12);
        }
    }
    
    if (state->selected_corner != CORNER_NONE) {
        // Draw diagonal using geometry buffer
        SDL_Color diagonal_color = {200, 200, 255, 255};
        V2 diag_start_s = world_to_screen(state->diagonal.start, &state->camera);
        V2 diag_end_s = world_to_screen(state->diagonal.end, &state->camera);
        geometry_buffer_add_line(&state->render_ctx.geometry, diag_start_s, diag_end_s, 2.0f, diagonal_color);
        
        // Draw squares along diagonal
        for (size_t i = 0; i < state->squares.length; i++) {
            Square* sq = &state->squares.ptr[i];
            
            // Apply sliver transform to the square's interval (from 0-10 space to viewport space)
            Interval transformed = sliver_transform_interval(sq->interval, &state->sliver_camera);
            
            // Convert to 0-1 range for diagonal lerp (divide by 10 since intervals are in 0-10)
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
            
            // Draw square using geometry buffer based on kind
            float min_x = fminf(p1.x, p2.x);
            float max_x = fmaxf(p1.x, p2.x);
            float min_y = fminf(p1.y, p2.y);
            float max_y = fmaxf(p1.y, p2.y);
            
            switch (sq->kind) {
                case KIND_ROUNDED: {
                    float base_radius = fminf(25.0f, fminf(max_x - min_x, max_y - min_y) * 0.2f);
                    float extend = base_radius * (1.0f - 1.0f/sqrtf(2));
                    draw_rounded_rect_geometry(&state->render_ctx.geometry, 
                                              min_x - extend, min_y - extend, 
                                              (max_x - min_x) + 2 * extend, 
                                              (max_y - min_y) + 2 * extend, 
                                              base_radius, sq->color, &state->camera);
                    break;
                }
                case KIND_DOUBLE:
                    draw_double_rect_geometry(&state->render_ctx.geometry, 
                                            min_x, min_y, max_x - min_x, max_y - min_y, 
                                            sq->color, &state->camera);
                    break;
                case KIND_WAVE:
                    draw_wave_rect_geometry(&state->render_ctx.geometry,
                                          min_x, min_y, max_x - min_x, max_y - min_y,
                                          sq->color, &state->camera);
                    break;
                case KIND_SHARP:
                default:
                    draw_rounded_rect_geometry(&state->render_ctx.geometry, 
                                              min_x, min_y, max_x - min_x, max_y - min_y, 
                                              0, sq->color, &state->camera);
                    break;
            }
        }
        
        // Draw orientation edge from selected corner using geometry buffer
        V2 selected = get_corner_position(state, state->selected_corner);
        V2 selected_s = world_to_screen(selected, &state->camera);
        V2 orient_end_s = world_to_screen(state->diagonal.end, &state->camera);
        SDL_Color orient_color = {255, 100, 100, 255};
        geometry_buffer_add_line(&state->render_ctx.geometry, selected_s, orient_end_s, 2.0f, orient_color);
    }
    
    // Render all geometry in one batch
    if (state->render_ctx.geometry.index_count > 0) {
        SDL_RenderGeometry(state->renderer, state->render_ctx.white_texture,
                          state->render_ctx.geometry.vertices, state->render_ctx.geometry.vertex_count,
                          state->render_ctx.geometry.indices, state->render_ctx.geometry.index_count);
    }
    
    // Clear clipping rect to draw UI
    SDL_RenderSetClipRect(state->renderer, NULL);
    
    // Render UI panel on the right
    render_ui_panel(state);
    
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
    // Only handle clicks within the viewport
    if (x >= VIEWPORT_WIDTH) {
        // Click is in UI panel - handle UI interactions here
        return;
    }
    
    V2 mouse = {(float)x, (float)y};
    Corner clicked = detect_corner_click(state, mouse);
    
    if (clicked != CORNER_NONE) {
        state->selected_corner = clicked;
        calculate_diagonal(state);
    }
}

// BandArray management functions
void band_array_init(BandArray* arr, size_t initial_capacity) {
    arr->ptr = (Band*)malloc(initial_capacity * sizeof(Band));
    arr->length = 0;
    arr->capacity = initial_capacity;
}

void band_array_free(BandArray* arr) {
    free(arr->ptr);
    arr->ptr = NULL;
    arr->length = 0;
    arr->capacity = 0;
}

void band_array_add(BandArray* arr, Band band) {
    if (arr->length >= arr->capacity) {
        size_t new_capacity = arr->capacity * 2;
        arr->ptr = (Band*)realloc(arr->ptr, new_capacity * sizeof(Band));
        arr->capacity = new_capacity;
    }
    arr->ptr[arr->length++] = band;
}

void band_array_clear(BandArray* arr) {
    arr->length = 0;
}

// SquareArray management functions
void square_array_init(SquareArray* arr, size_t initial_capacity) {
    arr->ptr = (Square*)malloc(initial_capacity * sizeof(Square));
    arr->length = 0;
    arr->capacity = initial_capacity;
}

void square_array_free(SquareArray* arr) {
    free(arr->ptr);
    arr->ptr = NULL;
    arr->length = 0;
    arr->capacity = 0;
}

void square_array_ensure_capacity(SquareArray* arr, size_t required) {
    if (required > arr->capacity) {
        size_t new_capacity = arr->capacity * 2;
        if (new_capacity < required) {
            new_capacity = required;
        }
        arr->ptr = (Square*)realloc(arr->ptr, new_capacity * sizeof(Square));
        arr->capacity = new_capacity;
    }
}

void square_array_clear(SquareArray* arr) {
    arr->length = 0;
}

// Generate intervals from a Band definition into SquareArray
// Band values are in 0-10 range, stored as-is (sliver camera handles the scaling)
void generate_intervals_from_band(SquareArray* arr, Band* band) {
    int count = band->repeat + 1;  // repeat=0 means 1 interval, repeat=n means n+1 intervals
    
    size_t start_idx = arr->length;
    square_array_ensure_capacity(arr, arr->length + count);
    
    for (int i = 0; i < count; i++) {
        float interval_start = band->start + i * band->stride;
        // Store in 0-10 range (sliver camera will handle the transform)
        arr->ptr[start_idx + i].interval.start = interval_start;
        arr->ptr[start_idx + i].interval.end = interval_start + band->size;
        arr->ptr[start_idx + i].color = band->color;  // Use color from Band
        arr->ptr[start_idx + i].kind = band->kind;    // Use kind from Band
    }
    
    arr->length += count;
}

// Generate all squares from bands
void generate_squares_from_bands(AppState* state) {
    square_array_clear(&state->squares);
    
    for (size_t i = 0; i < state->bands.length; i++) {
        generate_intervals_from_band(&state->squares, &state->bands.ptr[i]);
    }
}

void init_bands(AppState* state) {
    // Clear existing bands
    band_array_clear(&state->bands);
    
    // Define bands for each sequence (using natural 0-10 range)
    // Sequence 1: 10 unit squares (0-1, 1-2, ..., 9-10) in dark gray
    band_array_add(&state->bands, (Band){
        .start = 0.0f,
        .size = 1.0f,    // Unit size
        .stride = 1.0f,  // Unit spacing
        .repeat = 9,     // 10 total intervals
        .kind = KIND_SHARP,
        .color = {60, 60, 60, 255}  // Dark gray
    });
    
    // Sequence 2: 8 squares of size 1 (0.3-1.3, 1.3-2.3, ...)
    band_array_add(&state->bands, (Band){
        .start = 0.3f,
        .size = 1.0f,    // Unit size
        .stride = 1.0f,  // Unit spacing
        .repeat = 7,     // 8 total intervals
        .kind = KIND_ROUNDED,
        .color = {100, 150, 200, 255}  // Blue
    });
    
    // Sequence 3: 5 squares of size 0.5 (0.2-0.7, 1.2-1.7, ...)
    band_array_add(&state->bands, (Band){
        .start = 0.2f,
        .size = 0.5f,    // Half size
        .stride = 1.0f,  // Unit spacing
        .repeat = 4,     // 5 total intervals
        .kind = KIND_DOUBLE,
        .color = {200, 150, 100, 255}  // Orange
    });
    
    // Sequence 4: single square at 6.6-8.7
    band_array_add(&state->bands, (Band){
        .start = 6.6f,
        .size = 2.1f,
        .stride = 0.0f,  // No stride needed for single square
        .repeat = 0,     // Single interval
        .kind = KIND_WAVE,
        .color = {150, 200, 150, 255}  // Green
    });
    
    // Generate squares from bands
    generate_squares_from_bands(state);
}


int main(int argc, char* argv[]) {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 1;
    }
    
    if (TTF_Init() != 0) {
        fprintf(stderr, "TTF_Init failed: %s\n", TTF_GetError());
        SDL_Quit();
        return 1;
    }
    
    AppState state = {0};
    state.running = true;
    state.selected_corner = CORNER_BR;  // Start with bottom-right selected
    state.bounding_center = (V2){VIEWPORT_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center in viewport
    state.bounding_half = (BOUNDING_SIZE - BOUNDING_PADDING) / 2;
    
    // Initialize camera (centered in viewport)
    state.camera.offset = (V2){VIEWPORT_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center of viewport
    state.camera.scale = 1.0f;  // Default zoom
    state.dragging = false;
    
    // Initialize sliver camera to show 0-10 range
    state.sliver_camera.offset = 0.0f;  // Start at 0
    state.sliver_camera.scale = 0.1f;   // Scale 1:1 shows full 0-1 in viewport, which maps to 0-10 in our coordinates
    
    // Initialize dynamic arrays
    band_array_init(&state.bands, 10);
    square_array_init(&state.squares, 50);
    
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
    
    // Initialize render context
    render_context_init(&state.render_ctx, state.renderer);
    
    // Set up logical size for consistent coordinates
    SDL_RenderSetLogicalSize(state.renderer, WINDOW_WIDTH, WINDOW_HEIGHT);
    
    // Load font with 2x size for supersampling
    state.font = TTF_OpenFont("SourceCodePro-Regular.ttf", 36);  // 2x size for supersampling
    if (!state.font) {
        fprintf(stderr, "Failed to load SourceCodePro-Regular.ttf: %s\n", TTF_GetError());
        // Continue without font - UI text won't be rendered
    }
    
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
    init_bands(&state);
    
    SDL_Event event;
    while (state.running) {
        // Update mouse state before processing events
        state.mouse_was_pressed = state.mouse_pressed;
        int mouse_x, mouse_y;
        Uint32 mouse_state = SDL_GetMouseState(&mouse_x, &mouse_y);
        state.mouse_pos = (V2){(float)mouse_x, (float)mouse_y};
        state.mouse_pressed = (mouse_state & SDL_BUTTON(SDL_BUTTON_LEFT)) != 0;
        
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
                case SDL_QUIT:
                    state.running = false;
                    break;
                    
                case SDL_MOUSEBUTTONDOWN:
                    if (event.button.button == SDL_BUTTON_LEFT) {
                        if (event.button.x < VIEWPORT_WIDTH) {
                            if (!(SDL_GetModState() & KMOD_SHIFT)) {
                                handle_mouse_click(&state, event.button.x, event.button.y);
                            } else {
                                // Shift+click to drag
                                state.dragging = true;
                                state.drag_start = (V2){(float)event.button.x, (float)event.button.y};
                                state.camera_start = state.camera.offset;
                            }
                        }
                        // UI panel clicks are handled by button hover check
                    } else if (event.button.button == SDL_BUTTON_RIGHT || 
                              event.button.button == SDL_BUTTON_MIDDLE) {
                        // Start dragging only if in viewport
                        if (event.button.x < VIEWPORT_WIDTH) {
                            state.dragging = true;
                            state.drag_start = (V2){(float)event.button.x, (float)event.button.y};
                            state.camera_start = state.camera.offset;
                        }
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
                        
                        // Only zoom if mouse is in viewport
                        if (mouse_x >= VIEWPORT_WIDTH) {
                            break;
                        }
                        
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
                        case SDLK_SPACE:
                            // Reset to original square definitions
                            init_bands(&state);
                            break;
                        case SDLK_r:
                            // Reset squares
                            init_bands(&state);
                            break;
                        
                        // Sliver camera controls
                        case SDLK_LEFT:
                            {
                                // Zoom out sliver from center (show more range)
                                float center = state.sliver_camera.offset + 0.5f / state.sliver_camera.scale;
                                state.sliver_camera.scale /= 1.2f;
                                if (state.sliver_camera.scale < 0.01f) state.sliver_camera.scale = 0.01f;  // Min zoom: show 100 units
                                state.sliver_camera.offset = center - 0.5f / state.sliver_camera.scale;
                            }
                            break;
                        case SDLK_RIGHT:
                            {
                                // Zoom in sliver from center (show less range)
                                float center = state.sliver_camera.offset + 0.5f / state.sliver_camera.scale;
                                state.sliver_camera.scale *= 1.2f;
                                if (state.sliver_camera.scale > 2.0f) state.sliver_camera.scale = 2.0f;  // Max zoom: show 0.5 units
                                state.sliver_camera.offset = center - 0.5f / state.sliver_camera.scale;
                            }
                            break;
                        case SDLK_UP:
                            // Pan sliver view left (move by 1 unit in the 0-10 space)
                            state.sliver_camera.offset -= 0.1f / state.sliver_camera.scale;
                            break;
                        case SDLK_DOWN:
                            // Pan sliver view right (move by 1 unit in the 0-10 space)
                            state.sliver_camera.offset += 0.1f / state.sliver_camera.scale;
                            break;
                        case SDLK_0:
                            // Reset sliver camera to show full 0-10 range
                            state.sliver_camera.offset = 0.0f;
                            state.sliver_camera.scale = 0.1f;  // Show 0-10 range
                            break;
                    }
                    break;
            }
        }
        
        
        render(&state);
        SDL_Delay(16);  // ~60 FPS
    }
    
    // Cleanup
    band_array_free(&state.bands);
    square_array_free(&state.squares);
    render_context_free(&state.render_ctx);
    if (state.font) {
        TTF_CloseFont(state.font);
    }
    SDL_DestroyRenderer(state.renderer);
    SDL_DestroyWindow(state.window);
    TTF_Quit();
    SDL_Quit();
    
    return 0;
}
