#include <SDL.h>
#include <SDL_ttf.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <dirent.h>
#include "color.h"

#define WINDOW_WIDTH 1920
#define WINDOW_HEIGHT 1080
#define VIEWPORT_WIDTH 1400  // Left side for sliver viewport
#define UI_PANEL_WIDTH (WINDOW_WIDTH - VIEWPORT_WIDTH)  // Right side for UI
#define BOUNDING_SIZE 900
#define BOUNDING_PADDING 50
#define CORNER_RADIUS 30
#define HIGHLIGHT_SIZE 12

// Edge visibility flags for culling
#define EDGE_TOP    (1 << 0)  // 0x01
#define EDGE_RIGHT  (1 << 1)  // 0x02
#define EDGE_BOTTOM (1 << 2)  // 0x04
#define EDGE_LEFT   (1 << 3)  // 0x08

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
    V2 next;       // Next position to draw at
    V2 row_start;  // Start position of current row for horizontal layouts
    V2 max;        // Maximum bounds for layout
} Layout;

typedef struct {
    float start, end;
} Interval;

typedef struct {
    char* ptr;         // Pointer to the fixed buffer
    size_t count;      // Current number of 32-byte chunks used
    size_t capacity;   // Total number of 32-byte chunks available
} LabelArray;

typedef struct {
    char* ptr;         // Pointer to the buffer
    size_t len;        // Current length of string in buffer
    size_t capacity;   // Total capacity in bytes
} StringBuilder;

typedef struct {
    float lightness;  // Lightness value in OKLCH (0-1)
    float chroma;     // Chroma value in OKLCH (0-~0.4)
    float hue;        // Hue value in OKLCH color space (0-360)
} LCH;

typedef enum {
    KIND_SHARP = 0,     // Sharp corners
    KIND_ROUNDED = 1,   // Rounded corners
    KIND_DOUBLE = 2,    // Double lines
    KIND_WAVE = 3,      // Wavy lines
    KIND_COUNT = 4
} LineKind;

typedef enum {
    BAND_CLOSED = 0,    // Normal band with finite interval
    BAND_OPEN = 1       // Open band extending to infinity
} BandKind;

typedef enum {
    LABEL_TOP_LEFT = 0,
    LABEL_TOP_CENTER = 1,
    LABEL_TOP_RIGHT = 2,
    LABEL_MIDDLE_LEFT = 3,
    LABEL_CENTER = 4,
    LABEL_MIDDLE_RIGHT = 5,
    LABEL_BOTTOM_LEFT = 6,
    LABEL_BOTTOM_CENTER = 7,
    LABEL_BOTTOM_RIGHT = 8
} LabelAnchor;

typedef enum {
    UI_LENS = 0,
    UI_SHELF = 1
} UIStep;

typedef struct {
    UIStep step;
    bool shelf_mode_save;
    char suggested_filename[256];
} UIState;

typedef struct {
    Interval interval;  // Base interval (start and end positions)
    float stride;  // Distance between interval starts
    int repeat;    // Number of additional intervals (0 = single interval, n = n+1 total intervals)
    BandKind kind;  // BAND_CLOSED or BAND_OPEN
    LineKind line_kind;  // Drawing style: SHARP, ROUNDED, or DOUBLE
    LCH color;     // Color in OKLCH color space
    char* label;  // Label text for the band (pointer into LabelArray)
    int wavelength_scale;  // Multiplier for wavelength when line_kind is WAVE
    bool wave_inverted;  // Whether to use π phase offset for waves
    bool wave_half_period;  // Whether to add extra half period (end at opposite phase)
    bool follow_previous;  // If true, start position automatically follows end of previous band
    LabelAnchor label_anchor;  // Anchor position for label (9 positions in 3x3 grid)
    V2 label_offset;  // Additional offset from anchor position
} Band;

typedef struct {
    Band* ptr;        // Pointer to dynamically allocated array
    size_t length;    // Number of bands currently in use
    size_t capacity;  // Total allocated capacity
} BandArray;

typedef struct Work Work;

struct Work {
    char* arena;           // Single arena allocation
    size_t arena_size;     // Total size of arena
    BandArray bands;       // Band definitions
    LabelArray labels;     // Label strings
    char filename[256];    // Last opened/saved filename
    Work* prev;            // Previous work in linked list
    Work* next;            // Next work in linked list
};

typedef struct {
    V2 start, end;
} Diagonal;

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

typedef struct {
    void* ptr;        // Pointer to the field being edited
    bool is_numeric;  // True for numeric fields, false for text fields
} ActiveField;

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
    Work* work;           // Current work (pointer into linked list)
    BandArray flattened;  // Flattened bands (temporary for rendering)
    StringBuilder string_builder;  // String builder for building strings
    Camera camera;  // For 2D viewport movement
    SliverCamera sliver_camera;  // For 1D parameter space windowing
    bool dragging;  // For mouse drag panning
    V2 drag_start;  // Mouse position when drag started
    V2 camera_start;  // Camera offset when drag started
    V2 mouse_pos;  // Current mouse position
    bool mouse_pressed;  // Is left mouse button pressed this frame
    bool mouse_was_pressed;  // Was left mouse button pressed last frame
    // Input field state
    ActiveField active_field;  // Currently active input field
    char input_buffer[64];     // Text buffer for current input
    int cursor_pos;            // Cursor position in input buffer
    float input_original_value;  // Original value to restore on cancel
    // Input field drag state
    void* dragging_input_field;  // Pointer to the float being dragged
    float drag_start_value;      // Value when drag started
    float drag_start_mouse_x;    // Mouse X position when drag started
    LabelAnchor label_anchor;  // Default anchor for band labels
    int band_offset;  // Offset for band iteration in UI
    UIState ui_state;  // Current UI mode (LENS or SHELF)
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

// StringBuilder functions
void string_builder_init(StringBuilder* sb) {
    sb->capacity = 64 * 1024;  // 64KB buffer
    sb->ptr = malloc(sb->capacity);
    sb->len = 0;
}

void string_builder_clear(StringBuilder* sb) {
    sb->len = 0;
    if (sb->ptr) {
        sb->ptr[0] = '\0';
    }
}

void string_append(StringBuilder* sb, const char* format, ...) {
    if (!sb->ptr || sb->len >= sb->capacity - 1) return;

    va_list args;
    va_start(args, format);
    int written = vsnprintf(sb->ptr + sb->len, sb->capacity - sb->len, format, args);
    va_end(args);

    if (written > 0) {
        sb->len += written;
    }
}

// LabelArray functions

char* label_array_allocate(LabelArray* lb) {
    assert(lb->count < lb->capacity);  // Die if full
    char* label = lb->ptr + (lb->count * 32);
    // Default to a single letter from the alphabet, cycling A-Z
    snprintf(label, 32, "%c", 'A' + (char)(lb->count % 26));
    lb->count++;
    return label;
}

char* label_array_allocate_string(LabelArray* lb, const char* str) {
    char* label = label_array_allocate(lb);
    if (str) {
        strncpy(label, str, 31);
        label[31] = '\0';
    }
    return label;
}

// Work functions
void work_init(Work* work, size_t band_capacity, size_t label_capacity) {
    // Calculate arena size
    size_t bands_size = band_capacity * sizeof(Band);
    size_t labels_size = label_capacity * 32;  // 32 bytes per label
    work->arena_size = bands_size + labels_size;

    // Allocate arena
    work->arena = (char*)calloc(work->arena_size, 1);

    // Set up bands array
    work->bands.ptr = (Band*)work->arena;
    work->bands.length = 0;
    work->bands.capacity = band_capacity;

    // Set up labels array
    work->labels.ptr = work->arena + bands_size;
    work->labels.count = 0;
    work->labels.capacity = label_capacity;

    // Initialize filename and list pointers
    work->filename[0] = '\0';
    work->prev = NULL;
    work->next = NULL;
}

Work* work_new(size_t band_capacity, size_t label_capacity) {
    Work* work = (Work*)malloc(sizeof(Work));
    work_init(work, band_capacity, label_capacity);
    return work;
}

void band_array_add(BandArray* arr, Band band);

Work* work_copy(Work* source) {
    Work* work = work_new(source->bands.capacity, source->labels.capacity);

    for (size_t i = 0; i < source->bands.length; i++) {
        Band* src_band = &source->bands.ptr[i];
        Band new_band = *src_band;

        if (src_band->label) {
            new_band.label = label_array_allocate_string(&work->labels, src_band->label);
        }

        band_array_add(&work->bands, new_band);
    }

    strncpy(work->filename, source->filename, 255);
    work->filename[255] = '\0';

    return work;
}

void work_close(Work* work) {
    if (!work) return;

    if (work->prev) work->prev->next = work->next;
    if (work->next) work->next->prev = work->prev;

    free(work->arena);
    free(work);
}


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

void advance_horizontal(Layout* layout, float width) {
    layout->next.x += width;
    if (layout->next.x > layout->max.x) {
        layout->next.x = layout->max.x;
    }
}

void advance_vertical(Layout* layout, float height) {
    layout->next.y += height;
    layout->next.x = layout->row_start.x;  // Reset to start of row
    if (layout->next.y > layout->max.y) {
        layout->next.y = layout->max.y;
    }
}

// Render 3x3 anchor buttons for label position selection
// Returns the selected position (0-8) or -1 if no selection
int render_anchor_buttons(AppState* state, V2 position, float size, LabelAnchor current) {
    float button_size = size / 3.0f;
    int selected = -1;

    for (int row = 0; row < 3; row++) {
        for (int col = 0; col < 3; col++) {
            int index = row * 3 + col;
            V2 button_pos = {
                position.x + col * button_size,
                position.y + row * button_size
            };

            SDL_Rect rect = {
                (int)button_pos.x,
                (int)button_pos.y,
                (int)button_size,
                (int)button_size
            };

            // Check hover
            bool hover = state->mouse_pos.x >= rect.x && state->mouse_pos.x < rect.x + rect.w &&
                        state->mouse_pos.y >= rect.y && state->mouse_pos.y < rect.y + rect.h;

            // Draw background
            if (index == current) {
                SDL_SetRenderDrawColor(state->renderer, 100, 150, 200, 255);
            } else if (hover) {
                SDL_SetRenderDrawColor(state->renderer, 70, 70, 75, 255);
            } else {
                SDL_SetRenderDrawColor(state->renderer, 45, 45, 50, 255);
            }
            SDL_RenderFillRect(state->renderer, &rect);

            // Draw border
            SDL_SetRenderDrawColor(state->renderer, 80, 80, 85, 255);
            SDL_RenderDrawRect(state->renderer, &rect);

            // Draw small indicator dot in center
            SDL_Rect dot = {
                (int)(button_pos.x + button_size/2 - 2),
                (int)(button_pos.y + button_size/2 - 2),
                4, 4
            };
            SDL_SetRenderDrawColor(state->renderer, 200, 200, 200, 255);
            SDL_RenderFillRect(state->renderer, &dot);

            // Check for click
            if (hover && state->mouse_pressed && !state->mouse_was_pressed &&
                state->dragging_input_field == NULL) {
                selected = index;
            }
        }
    }

    return selected;
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
    // But not if we're dragging an input field
    return hover && !state->mouse_pressed && state->mouse_was_pressed && !state->dragging_input_field;
}

// Full version of input field with disabled option and custom scale
void render_numeric_input_field_full(AppState* state, float* value, V2 position, V2 size, bool disabled, float drag_scale) {
    bool is_active = (state->active_field.ptr == value && state->active_field.is_numeric);
    bool is_dragging = (state->dragging_input_field == value);

    SDL_Rect field_rect = {
        (int)position.x,
        (int)position.y,
        (int)size.x,
        (int)size.y
    };

    // Check if mouse is over field
    bool hover = state->mouse_pos.x >= field_rect.x && state->mouse_pos.x < field_rect.x + field_rect.w &&
                 state->mouse_pos.y >= field_rect.y && state->mouse_pos.y < field_rect.y + field_rect.h;

    // Handle mouse interactions (only if not disabled)
    if (hover && state->mouse_pressed && !state->mouse_was_pressed && !disabled) {
        // Start dragging immediately on press
        state->dragging_input_field = value;
        state->drag_start_value = *value;
        state->drag_start_mouse_x = state->mouse_pos.x;
    } else if (!hover && state->mouse_pressed && is_active) {
        // Click outside - deactivate and apply value
        if (strlen(state->input_buffer) > 0) {
            float new_value = atof(state->input_buffer);
            *value = new_value;
        }
        state->active_field.ptr = NULL;
    }

    // Handle dragging
    if (is_dragging) {
        if (state->mouse_pressed) {
            // Update value based on mouse drag
            float delta_x = state->mouse_pos.x - state->drag_start_mouse_x;
            float scale = drag_scale;  // Use provided scale
            if (SDL_GetModState() & KMOD_CTRL) {
                scale = drag_scale * 0.1f;  // Fine control with Ctrl
            } else if (SDL_GetModState() & KMOD_ALT) {
                scale = drag_scale * 10.0f;    // Coarse control with Alt
            }
            *value = state->drag_start_value + delta_x * scale;


            // Update display if this field is also active
            if (is_active) {
                snprintf(state->input_buffer, sizeof(state->input_buffer), "%.2f", *value);
                state->cursor_pos = strlen(state->input_buffer);
            }
        } else {
            // Mouse released - check if it was a click (minimal movement)
            float movement = fabs(state->mouse_pos.x - state->drag_start_mouse_x);
            if (movement < 3.0f && hover && !is_active) {
                // It was a click, activate text input
                state->active_field.ptr = value;
                state->active_field.is_numeric = true;
                state->input_original_value = *value;
                snprintf(state->input_buffer, sizeof(state->input_buffer), "%.2f", *value);
                state->cursor_pos = strlen(state->input_buffer);
            }
            // Stop dragging
            state->dragging_input_field = NULL;
        }
    }

    // Draw field background
    if (disabled) {
        SDL_SetRenderDrawColor(state->renderer, 25, 25, 30, 255);  // Darker when disabled
    } else if (is_dragging) {
        SDL_SetRenderDrawColor(state->renderer, 70, 55, 70, 255);  // Purple tint when dragging
    } else if (is_active) {
        SDL_SetRenderDrawColor(state->renderer, 60, 65, 70, 255);
    } else if (hover) {
        SDL_SetRenderDrawColor(state->renderer, 45, 45, 50, 255);
    } else {
        SDL_SetRenderDrawColor(state->renderer, 35, 35, 40, 255);
    }
    SDL_RenderFillRect(state->renderer, &field_rect);

    // Draw field border
    if (disabled) {
        SDL_SetRenderDrawColor(state->renderer, 50, 50, 55, 255);  // Dim border when disabled
    } else if (is_dragging) {
        SDL_SetRenderDrawColor(state->renderer, 150, 100, 200, 255);  // Purple border when dragging
    } else if (is_active) {
        SDL_SetRenderDrawColor(state->renderer, 100, 150, 200, 255);
    } else {
        SDL_SetRenderDrawColor(state->renderer, 70, 70, 75, 255);
    }
    SDL_RenderDrawRect(state->renderer, &field_rect);

    // Display text
    char display_text[64];
    if (is_active) {
        // Show input buffer with cursor
        strncpy(display_text, state->input_buffer, sizeof(display_text));
    } else {
        // Show current value
        snprintf(display_text, sizeof(display_text), "%.2f", *value);
    }

    // Render text
    if (state->font && strlen(display_text) > 0) {
        SDL_Color white = {255, 255, 255, 255};
        V2 text_pos = {position.x + 4, position.y + 2};
        render_text(state, display_text, text_pos, white);

        // Draw cursor if active
        if (is_active && state->font) {
            // Calculate cursor position using actual text width
            char temp[64];
            strncpy(temp, state->input_buffer, state->cursor_pos);
            temp[state->cursor_pos] = '\0';

            int text_width = 0;
            if (strlen(temp) > 0) {
                // Measure text width using TTF
                TTF_SizeText(state->font, temp, &text_width, NULL);
                // Scale down because we render at 2x and scale down
                text_width /= 2;
            }

            float cursor_x = text_pos.x + text_width;
            SDL_SetRenderDrawColor(state->renderer, 255, 255, 255, 255);
            SDL_RenderDrawLine(state->renderer, cursor_x, position.y + 2, cursor_x, position.y + size.y - 2);
        }
    }
}

// Text input field for editing strings
void render_text_input_field(AppState* state, char* text, size_t max_len, V2 position, V2 size) {
    bool is_active = (state->active_field.ptr == text && !state->active_field.is_numeric);

    SDL_Rect field_rect = {
        (int)position.x,
        (int)position.y,
        (int)size.x,
        (int)size.y
    };

    // Check if mouse is over field
    bool hover = state->mouse_pos.x >= field_rect.x && state->mouse_pos.x < field_rect.x + field_rect.w &&
                 state->mouse_pos.y >= field_rect.y && state->mouse_pos.y < field_rect.y + field_rect.h;

    // Handle mouse click to activate
    if (hover && state->mouse_pressed && !state->mouse_was_pressed && !is_active) {
        state->active_field.ptr = text;
        state->active_field.is_numeric = false;
        // Copy current text to input buffer
        strncpy(state->input_buffer, text, sizeof(state->input_buffer) - 1);
        state->input_buffer[sizeof(state->input_buffer) - 1] = '\0';
        state->cursor_pos = strlen(state->input_buffer);
        SDL_StartTextInput();
    } else if (!hover && state->mouse_pressed && is_active) {
        // Click outside - deactivate and save
        strncpy(text, state->input_buffer, max_len - 1);
        text[max_len - 1] = '\0';
        state->active_field.ptr = NULL;
        // Keep text input active
    }

    // Draw field background
    if (is_active) {
        SDL_SetRenderDrawColor(state->renderer, 60, 65, 70, 255);
    } else if (hover) {
        SDL_SetRenderDrawColor(state->renderer, 45, 45, 50, 255);
    } else {
        SDL_SetRenderDrawColor(state->renderer, 35, 35, 40, 255);
    }
    SDL_RenderFillRect(state->renderer, &field_rect);

    // Draw field border
    if (is_active) {
        SDL_SetRenderDrawColor(state->renderer, 100, 150, 200, 255);
    } else {
        SDL_SetRenderDrawColor(state->renderer, 70, 70, 75, 255);
    }
    SDL_RenderDrawRect(state->renderer, &field_rect);

    // Display text
    const char* display_text = is_active ? state->input_buffer : text;
    if (state->font && display_text && strlen(display_text) > 0) {
        SDL_Color white = {255, 255, 255, 255};
        V2 text_pos = {position.x + 4, position.y + 2};
        render_text(state, display_text, text_pos, white);

        // Draw cursor if active
        if (is_active) {
            // Calculate cursor position
            char temp[256] = {0};
            strncpy(temp, state->input_buffer, state->cursor_pos);
            temp[state->cursor_pos] = '\0';

            int text_width = 0;
            if (strlen(temp) > 0) {
                TTF_SizeText(state->font, temp, &text_width, NULL);
                text_width /= 2;  // Account for supersampling
            }

            float cursor_x = text_pos.x + text_width;
            SDL_SetRenderDrawColor(state->renderer, 255, 255, 255, 255);
            SDL_RenderDrawLine(state->renderer, cursor_x, position.y + 2, cursor_x, position.y + size.y - 2);
        }
    }
}

// Draw a wavy line between two points
void geometry_buffer_add_wave_line(GeometryBuffer* gb, V2 p1, V2 p2, float thickness, float amplitude, float wavelength, SDL_Color color, bool inverted, bool half_period) {
    V2 dir = v2_sub(p2, p1);
    float length = v2_length(dir);
    if (length == 0) return;

    dir = v2_scale(dir, 1.0f / length);
    V2 perp = {-dir.y, dir.x};  // Perpendicular direction

    // Calculate number of complete wavelengths that fit
    float num_waves = length / wavelength;
    float target_waves;

    if (!half_period) {  // Inverted logic - false means half period
        // Target (2n+1)/2 periods: 0.5, 1.5, 2.5, etc.
        int n = (int)(num_waves);
        target_waves = (n > 0) ? n + 0.5f : 0.5f;
    } else {
        // Target n periods: 1, 2, 3, etc.
        target_waves = (int)(num_waves + 0.5f);
        if (target_waves < 1) target_waves = 1;
    }

    // Adjust wavelength to fit exactly
    float adjusted_wavelength = length / target_waves;

    // Number of segments for smooth sine wave (more segments = smoother curve)
    int segments = (int)(length / 2.0f);  // One segment every 2 pixels
    if (segments < 16) segments = 16;  // Minimum 16 segments for smoothness

    for (int i = 0; i < segments; i++) {
        float t1 = (float)i / segments;
        float t2 = (float)(i + 1) / segments;

        // Calculate positions along the line
        V2 base1 = v2_lerp(p1, p2, t1);
        V2 base2 = v2_lerp(p1, p2, t2);

        // Calculate wave offsets using adjusted wavelength with optional π phase offset
        float phase_offset = !inverted ? M_PI : 0.0f;  // Inverted logic - false means π offset
        float phase1 = (t1 * length / adjusted_wavelength) * 2.0f * M_PI + phase_offset;
        float phase2 = (t2 * length / adjusted_wavelength) * 2.0f * M_PI + phase_offset;
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
void draw_wave_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, SDL_Color color, Camera* camera, int wavelength_scale, bool inverted, bool half_period, int hide_flags) {
    float thickness = 2.0f;
    float amplitude = thickness * 2.0f;  // Amplitude is 3x thickness
    float wavelength = thickness * 8.0f * (1 << wavelength_scale);  // Wavelength is 8x thickness * 2^scale

    // Convert world coordinates to screen
    V2 tl = world_to_screen((V2){x, y}, camera);
    V2 tr = world_to_screen((V2){x + w, y}, camera);
    V2 br = world_to_screen((V2){x + w, y + h}, camera);
    V2 bl = world_to_screen((V2){x, y + h}, camera);

    // Scale amplitude and wavelength for screen space
    // wavelength_scale is already the exponent, so use it directly for amplitude scaling
    float amplitude_scale = 1.0f + wavelength_scale * 0.5f;
    float screen_amplitude = amplitude * amplitude_scale * camera->scale;
    float screen_wavelength = wavelength * camera->scale;

    // Draw four wavy sides (skip hidden edges)
    if (!(hide_flags & EDGE_TOP))
        geometry_buffer_add_wave_line(gb, tl, tr, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Top
    if (!(hide_flags & EDGE_RIGHT))
        geometry_buffer_add_wave_line(gb, tr, br, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Right
    if (!(hide_flags & EDGE_BOTTOM))
        geometry_buffer_add_wave_line(gb, br, bl, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Bottom
    if (!(hide_flags & EDGE_LEFT))
        geometry_buffer_add_wave_line(gb, bl, tl, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Left
}

// Draw double-line rectangle with geometry buffer
void draw_double_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, SDL_Color color, Camera* camera, int hide_flags) {
    float thickness = 2.0f;
    float gap = thickness * 2.0f;  // Gap between lines equals thickness

    // Outer rectangle
    V2 tl_outer = world_to_screen((V2){x, y}, camera);
    V2 tr_outer = world_to_screen((V2){x + w, y}, camera);
    V2 br_outer = world_to_screen((V2){x + w, y + h}, camera);
    V2 bl_outer = world_to_screen((V2){x, y + h}, camera);

    if (!(hide_flags & EDGE_TOP))
        geometry_buffer_add_line(gb, tl_outer, tr_outer, thickness, color);
    if (!(hide_flags & EDGE_RIGHT))
        geometry_buffer_add_line(gb, tr_outer, br_outer, thickness, color);
    if (!(hide_flags & EDGE_BOTTOM))
        geometry_buffer_add_line(gb, br_outer, bl_outer, thickness, color);
    if (!(hide_flags & EDGE_LEFT))
        geometry_buffer_add_line(gb, bl_outer, tl_outer, thickness, color);

    // Inner rectangle (inset by gap)
    float inset = gap / camera->scale;  // Convert gap to world units
    V2 tl_inner = world_to_screen((V2){x + inset, y + inset}, camera);
    V2 tr_inner = world_to_screen((V2){x + w - inset, y + inset}, camera);
    V2 br_inner = world_to_screen((V2){x + w - inset, y + h - inset}, camera);
    V2 bl_inner = world_to_screen((V2){x + inset, y + h - inset}, camera);

    if (!(hide_flags & EDGE_TOP))
        geometry_buffer_add_line(gb, tl_inner, tr_inner, thickness, color);
    if (!(hide_flags & EDGE_RIGHT))
        geometry_buffer_add_line(gb, tr_inner, br_inner, thickness, color);
    if (!(hide_flags & EDGE_BOTTOM))
        geometry_buffer_add_line(gb, br_inner, bl_inner, thickness, color);
    if (!(hide_flags & EDGE_LEFT))
        geometry_buffer_add_line(gb, bl_inner, tl_inner, thickness, color);
}

// Draw rounded rectangle with geometry buffer
void draw_rounded_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, float radius, SDL_Color color, Camera* camera, int hide_flags) {
    if (radius <= 0) {
        // Draw regular rectangle with thick lines
        V2 tl = world_to_screen((V2){x, y}, camera);
        V2 tr = world_to_screen((V2){x + w, y}, camera);
        V2 br = world_to_screen((V2){x + w, y + h}, camera);
        V2 bl = world_to_screen((V2){x, y + h}, camera);

        if (!(hide_flags & EDGE_TOP))
            geometry_buffer_add_line(gb, tl, tr, 2.0f, color);
        if (!(hide_flags & EDGE_RIGHT))
            geometry_buffer_add_line(gb, tr, br, 2.0f, color);
        if (!(hide_flags & EDGE_BOTTOM))
            geometry_buffer_add_line(gb, br, bl, 2.0f, color);
        if (!(hide_flags & EDGE_LEFT))
            geometry_buffer_add_line(gb, bl, tl, 2.0f, color);
    } else {
        // Draw rounded rectangle with arcs at corners
        V2 tl_inner = world_to_screen((V2){x + radius, y + radius}, camera);
        V2 tr_inner = world_to_screen((V2){x + w - radius, y + radius}, camera);
        V2 br_inner = world_to_screen((V2){x + w - radius, y + h - radius}, camera);
        V2 bl_inner = world_to_screen((V2){x + radius, y + h - radius}, camera);

        // Top line
        if (!(hide_flags & EDGE_TOP))
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x + radius, y}, camera),
                world_to_screen((V2){x + w - radius, y}, camera),
                2.0f, color);

        // Right line
        if (!(hide_flags & EDGE_RIGHT))
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x + w, y + radius}, camera),
                world_to_screen((V2){x + w, y + h - radius}, camera),
                2.0f, color);

        // Bottom line
        if (!(hide_flags & EDGE_BOTTOM))
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x + w - radius, y + h}, camera),
                world_to_screen((V2){x + radius, y + h}, camera),
                2.0f, color);

        // Left line
        if (!(hide_flags & EDGE_LEFT))
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x, y + h - radius}, camera),
                world_to_screen((V2){x, y + radius}, camera),
                2.0f, color);

        // Calculate screen radius
        float screen_radius = radius * camera->scale;

        // Corner arcs (only draw if adjacent edges are visible)
        if (!(hide_flags & EDGE_TOP) && !(hide_flags & EDGE_LEFT))
            geometry_buffer_add_arc(gb, tl_inner, screen_radius, M_PI, M_PI * 1.5f, 2.0f, color, 8);
        if (!(hide_flags & EDGE_TOP) && !(hide_flags & EDGE_RIGHT))
            geometry_buffer_add_arc(gb, tr_inner, screen_radius, M_PI * 1.5f, M_PI * 2.0f, 2.0f, color, 8);
        if (!(hide_flags & EDGE_BOTTOM) && !(hide_flags & EDGE_RIGHT))
            geometry_buffer_add_arc(gb, br_inner, screen_radius, 0, M_PI * 0.5f, 2.0f, color, 8);
        if (!(hide_flags & EDGE_BOTTOM) && !(hide_flags & EDGE_LEFT))
            geometry_buffer_add_arc(gb, bl_inner, screen_radius, M_PI * 0.5f, M_PI, 2.0f, color, 8);
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
    assert(arr->length < arr->capacity);  // Fixed size - cannot grow
    arr->ptr[arr->length++] = band;
}

void band_array_clear(BandArray* arr) {
    arr->length = 0;
}

void band_array_remove(BandArray* arr, size_t index) {
    if (index >= arr->length) return;

    // Shift all elements after index down by one
    for (size_t i = index; i < arr->length - 1; i++) {
        arr->ptr[i] = arr->ptr[i + 1];
    }
    arr->length--;
}

void band_array_insert(BandArray* arr, size_t index, Band band) {
    // Ensure capacity
    if (arr->length >= arr->capacity) {
        size_t new_capacity = arr->capacity * 2;
        arr->ptr = (Band*)realloc(arr->ptr, new_capacity * sizeof(Band));
        arr->capacity = new_capacity;
    }

    // Clamp index to valid range
    if (index > arr->length) index = arr->length;

    // Shift elements after index up by one
    for (size_t i = arr->length; i > index; i--) {
        arr->ptr[i] = arr->ptr[i - 1];
    }

    // Insert the new band
    arr->ptr[index] = band;
    arr->length++;
}

void band_array_copy_after(BandArray* arr, size_t index, LabelArray* lb) {
    if (index >= arr->length) return;

    Band original = arr->ptr[index];
    Band copy = original;

    // Allocate new label and copy text
    copy.label = label_array_allocate_string(lb, original.label);

    // Calculate size once and preserve it
    float size = original.interval.end - original.interval.start;
    copy.interval.start = original.interval.end;  // Start where the original ends
    copy.interval.end = copy.interval.start + size;  // Preserve the size
    copy.follow_previous = true;  // This band should follow the previous one

    // Insert right after the original
    band_array_insert(arr, index + 1, copy);
}

void band_array_split(BandArray* arr, size_t index, LabelArray* lb) {
    if (index >= arr->length) return;

    Band* original = &arr->ptr[index];
    float midpoint = (original->interval.start + original->interval.end) / 2.0f;

    // Create second half band
    Band second_half = *original;
    second_half.label = label_array_allocate_string(lb, original->label);  // Copy label
    second_half.interval.start = midpoint;
    second_half.follow_previous = true;  // Second half follows the first half

    // Modify original to be first half
    original->interval.end = midpoint;

    // Insert second half right after the original
    band_array_insert(arr, index + 1, second_half);
}

// Flatten bands into individual bands (one per interval)
void flatten_bands(BandArray* source, BandArray* dest) {
    band_array_clear(dest);

    for (size_t i = 0; i < source->length; i++) {
        Band* band = &source->ptr[i];

        // If this band should follow the previous one, update its start position only
        if (band->follow_previous && i > 0) {
            Band* prev_band = &source->ptr[i - 1];
            band->interval.start = prev_band->interval.end;
            // Keep the end position as-is, don't move it
        }

        // Automatically set band to OPEN if start >= end
        if (band->interval.start >= band->interval.end) {
            band->kind = BAND_OPEN;
        }

        if (band->kind == BAND_OPEN) {
            // For open bands, create two intervals: (-inf, end] and [start, +inf)
            Band flattened1 = *band;  // Copy all fields
            flattened1.interval.start = -1000.0f;  // Large negative value for -infinity
            flattened1.interval.end = band->interval.end;
            flattened1.stride = 0;
            flattened1.repeat = 0;
            flattened1.follow_previous = false;
            flattened1.kind = BAND_CLOSED;  // Flattened bands are always closed
            band_array_add(dest, flattened1);

            Band flattened2 = *band;  // Copy all fields
            flattened2.interval.start = band->interval.start;
            flattened2.interval.end = 1000.0f;  // Large positive value for +infinity
            flattened2.stride = 0;
            flattened2.repeat = 0;
            flattened2.follow_previous = false;
            flattened2.kind = BAND_CLOSED;  // Flattened bands are always closed
            band_array_add(dest, flattened2);
        } else {
            // Normal closed band behavior
            int count = band->repeat + 1;  // repeat=0 means 1 interval
            float size = band->interval.end - band->interval.start;

            for (int j = 0; j < count; j++) {
                Band flattened = *band;  // Copy all fields
                flattened.interval.start = band->interval.start + j * band->stride;
                flattened.interval.end = flattened.interval.start + size;
                flattened.stride = 0;  // No repetition in flattened bands
                flattened.repeat = 0;  // Single interval
                flattened.follow_previous = false;  // Flattened bands don't follow
                // label pointer stays the same - no reallocation

                band_array_add(dest, flattened);
            }
        }
    }
}

// Create a new band with random color
void add_random_band(AppState* state) {
    // Random hue for variety, fixed lightness for consistency
    float random_hue = (rand() % 360);
    float lightness = 0.7f;  // 70% lightness
    float chroma = 0.15f + (rand() % 100) / 500.0f;  // 0.15 to 0.35 chroma

    Band new_band = {
        .interval = {.start = 0.0f, .end = 1.0f},
        .stride = 1.0f,
        .repeat = 0,
        .kind = BAND_CLOSED,  // Default to closed band
        .line_kind = KIND_WAVE,  // Default to wave
        .color = {.lightness = lightness, .chroma = chroma, .hue = random_hue},
        .label = label_array_allocate(&state->work->labels),  // Empty label
        .wavelength_scale = 1,
        .wave_inverted = false,
        .label_anchor = LABEL_BOTTOM_RIGHT,  // Default label anchor
        .label_offset = {0, 0},  // No offset
        .wave_half_period = false,
        .follow_previous = false
    };

    band_array_add(&state->work->bands, new_band);
}

void add_open_band(AppState* state) {
    // Calculate center of sliver camera view
    float center = state->sliver_camera.offset + 0.5f / state->sliver_camera.scale;

    // Random hue for variety, fixed lightness for consistency
    float random_hue = (rand() % 360);
    float lightness = 0.7f;  // 70% lightness
    float chroma = 0.15f + (rand() % 100) / 500.0f;  // 0.15 to 0.35 chroma

    Band new_band = {
        .interval = {.start = center, .end = center},  // Start = end triggers BAND_OPEN
        .stride = 1.0f,
        .repeat = 0,
        .kind = BAND_OPEN,  // Will be set automatically when start >= end
        .line_kind = KIND_WAVE,  // Default to wave
        .color = {.lightness = lightness, .chroma = chroma, .hue = random_hue},
        .label = label_array_allocate(&state->work->labels),  // Empty label
        .wavelength_scale = 1,
        .wave_inverted = false,
        .wave_half_period = false,
        .follow_previous = false,
        .label_anchor = LABEL_BOTTOM_RIGHT,  // Default label anchor
        .label_offset = {0, 0}  // No offset
    };

    band_array_add(&state->work->bands, new_band);
}

void init_bands_rand(AppState* state) {
    // Clear existing bands and reset label buffer
    band_array_clear(&state->work->bands);
    state->work->labels.count = 0;  // Reset label buffer to reclaim all slots
    memset(state->work->labels.ptr, 0, state->work->labels.capacity * 32);  // Clear all label memory

    // Add a single random band
    add_random_band(state);
}

const char* band_kind_to_string(BandKind kind) {
    switch (kind) {
        case BAND_CLOSED: return "CLOSED";
        case BAND_OPEN: return "OPEN";
        default: return "UNKNOWN";
    }
}

const char* line_kind_to_string(LineKind kind) {
    switch (kind) {
        case KIND_SHARP: return "SHARP";
        case KIND_ROUNDED: return "ROUNDED";
        case KIND_DOUBLE: return "DOUBLE";
        case KIND_WAVE: return "WAVE";
        default: return "UNKNOWN";
    }
}

BandKind string_to_band_kind(const char* str) {
    if (strcmp(str, "CLOSED") == 0) return BAND_CLOSED;
    if (strcmp(str, "OPEN") == 0) return BAND_OPEN;
    return BAND_CLOSED;
}

LineKind string_to_line_kind(const char* str) {
    if (strcmp(str, "SHARP") == 0) return KIND_SHARP;
    if (strcmp(str, "ROUNDED") == 0) return KIND_ROUNDED;
    if (strcmp(str, "DOUBLE") == 0) return KIND_DOUBLE;
    if (strcmp(str, "WAVE") == 0) return KIND_WAVE;
    return KIND_SHARP;
}

typedef struct {
    char files[64][256];
    size_t count;
} FileList;

void get_wo_files(FileList* list) {
    list->count = 0;

    DIR* dir = opendir(".");
    if (!dir) {
        printf("Failed to open directory\n");
        return;
    }

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL && list->count < 64) {
        size_t len = strlen(entry->d_name);
        if (len > 3 && strcmp(entry->d_name + len - 3, ".wo") == 0) {
            strncpy(list->files[list->count], entry->d_name, 255);
            list->files[list->count][255] = '\0';
            list->count++;
        }
    }

    closedir(dir);
}

void work_save(Work* work, const char* filename) {
    FILE* file = fopen(filename, "w");
    if (!file) {
        printf("Failed to open file for writing: %s\n", filename);
        return;
    }

    strncpy(work->filename, filename, 255);
    work->filename[255] = '\0';

    fprintf(file, "WORK SLIVER 0001\n");

    for (size_t i = 0; i < work->bands.length; i++) {
        Band* band = &work->bands.ptr[i];
        fprintf(file, "{\n");
        fprintf(file, "  type: band\n");
        fprintf(file, "  interval: %f %f\n", band->interval.start, band->interval.end);
        fprintf(file, "  stride: %f\n", band->stride);
        fprintf(file, "  repeat: %d\n", band->repeat);
        fprintf(file, "  kind: %s\n", band_kind_to_string(band->kind));
        fprintf(file, "  line_kind: %s\n", line_kind_to_string(band->line_kind));
        fprintf(file, "  color: %f %f %f\n", band->color.hue, band->color.lightness, band->color.chroma);
        fprintf(file, "  label: \"%s\"\n", band->label ? band->label : "");
        fprintf(file, "  wavelength_scale: %d\n", band->wavelength_scale);
        fprintf(file, "  wave_inverted: %s\n", band->wave_inverted ? "true" : "false");
        fprintf(file, "  wave_half_period: %s\n", band->wave_half_period ? "true" : "false");
        fprintf(file, "  follow_previous: %s\n", band->follow_previous ? "true" : "false");
        fprintf(file, "  label_anchor: %d\n", band->label_anchor);
        fprintf(file, "  label_offset: %f %f\n", band->label_offset.x, band->label_offset.y);
        fprintf(file, "}\n");
    }

    fclose(file);
    printf("Work saved to %s\n", filename);
}

bool work_load(Work* work, const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        printf("Failed to open file for reading: %s\n", filename);
        return false;
    }

    char line[512];
    if (!fgets(line, sizeof(line), file) || strncmp(line, "WORK SLIVER", 11) != 0) {
        printf("Invalid file format\n");
        fclose(file);
        return false;
    }

    strncpy(work->filename, filename, 255);
    work->filename[255] = '\0';

    work->bands.length = 0;
    work->labels.count = 0;

    Band temp_band = {0};
    char temp_label[256] = {0};
    bool in_record = false;

    while (fgets(line, sizeof(line), file)) {
        char* trimmed = line;
        while (*trimmed == ' ' || *trimmed == '\t') trimmed++;

        if (trimmed[0] == '{') {
            in_record = true;
            memset(&temp_band, 0, sizeof(Band));
            temp_label[0] = '\0';
        } else if (trimmed[0] == '}') {
            if (in_record) {
                char* label_ptr = label_array_allocate_string(&work->labels, temp_label);
                temp_band.label = label_ptr;
                band_array_add(&work->bands, temp_band);
                in_record = false;
            }
        } else if (in_record) {
            if (strncmp(trimmed, "interval:", 9) == 0) {
                sscanf(trimmed + 9, "%f %f", &temp_band.interval.start, &temp_band.interval.end);
            } else if (strncmp(trimmed, "stride:", 7) == 0) {
                sscanf(trimmed + 7, "%f", &temp_band.stride);
            } else if (strncmp(trimmed, "repeat:", 7) == 0) {
                sscanf(trimmed + 7, "%d", &temp_band.repeat);
            } else if (strncmp(trimmed, "kind:", 5) == 0) {
                char kind_str[32];
                sscanf(trimmed + 5, "%31s", kind_str);
                temp_band.kind = string_to_band_kind(kind_str);
            } else if (strncmp(trimmed, "line_kind:", 10) == 0) {
                char line_kind_str[32];
                sscanf(trimmed + 10, "%31s", line_kind_str);
                temp_band.line_kind = string_to_line_kind(line_kind_str);
            } else if (strncmp(trimmed, "color:", 6) == 0) {
                sscanf(trimmed + 6, "%f %f %f", &temp_band.color.hue, &temp_band.color.lightness, &temp_band.color.chroma);
            } else if (strncmp(trimmed, "label:", 6) == 0) {
                char* label_start = strchr(trimmed + 6, '"');
                if (label_start) {
                    label_start++;
                    char* label_end = strchr(label_start, '"');
                    if (label_end) {
                        size_t len = label_end - label_start;
                        if (len >= sizeof(temp_label)) len = sizeof(temp_label) - 1;
                        strncpy(temp_label, label_start, len);
                        temp_label[len] = '\0';
                    }
                }
            } else if (strncmp(trimmed, "wavelength_scale:", 17) == 0) {
                sscanf(trimmed + 17, "%d", &temp_band.wavelength_scale);
            } else if (strncmp(trimmed, "wave_inverted:", 14) == 0) {
                char bool_str[16];
                sscanf(trimmed + 14, "%15s", bool_str);
                temp_band.wave_inverted = (strcmp(bool_str, "true") == 0);
            } else if (strncmp(trimmed, "wave_half_period:", 17) == 0) {
                char bool_str[16];
                sscanf(trimmed + 17, "%15s", bool_str);
                temp_band.wave_half_period = (strcmp(bool_str, "true") == 0);
            } else if (strncmp(trimmed, "follow_previous:", 16) == 0) {
                char bool_str[16];
                sscanf(trimmed + 16, "%15s", bool_str);
                temp_band.follow_previous = (strcmp(bool_str, "true") == 0);
            } else if (strncmp(trimmed, "label_anchor:", 13) == 0) {
                sscanf(trimmed + 13, "%d", (int*)&temp_band.label_anchor);
            } else if (strncmp(trimmed, "label_offset:", 13) == 0) {
                sscanf(trimmed + 13, "%f %f", &temp_band.label_offset.x, &temp_band.label_offset.y);
            }
        }
    }

    fclose(file);
    printf("Work loaded from %s (%zu bands)\n", filename, work->bands.length);
    return true;
}

// Draw Work UI layer - manages Work objects (load, store, init)
Layout draw_work_ui(AppState* state, Layout layout) {
    V2 button_size = {80, 25};

    // Work navigation and management buttons
    V2 nav_button_size = {30, 25};
    if (render_button(state, "<", layout.next, nav_button_size, false)) {
        if (state->work->prev) {
            state->work = state->work->prev;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 5);

    if (render_button(state, ">", layout.next, nav_button_size, false)) {
        if (state->work->next) {
            state->work = state->work->next;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 10);

    // Work preset buttons
    if (render_button(state, "New", layout.next, button_size, false)) {
        Work* new_work = work_new(128, 128);
        new_work->prev = state->work;
        new_work->next = state->work->next;
        if (state->work->next) state->work->next->prev = new_work;
        state->work->next = new_work;
        state->work = new_work;
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(state, "Copy", layout.next, button_size, false)) {
        Work* copied = work_copy(state->work);
        copied->prev = state->work;
        copied->next = state->work->next;
        if (state->work->next) state->work->next->prev = copied;
        state->work->next = copied;
        state->work = copied;
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(state, "Close", layout.next, button_size, false)) {
        if (state->work->prev || state->work->next) {
            Work* to_close = state->work;
            if (state->work->next) {
                state->work = state->work->next;
            } else {
                state->work = state->work->prev;
            }
            work_close(to_close);
        }
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(state, "Store", layout.next, button_size, false)) {
        if (state->work->filename[0] != '\0') {
            strncpy(state->ui_state.suggested_filename, state->work->filename, 255);
            state->ui_state.suggested_filename[255] = '\0';
        } else {
            time_t now = time(NULL);
            struct tm *tm_info = localtime(&now);
            strftime(state->ui_state.suggested_filename, sizeof(state->ui_state.suggested_filename), "work_%Y-%m-%d_%H-%M-%S.wo", tm_info);
        }
        state->ui_state.shelf_mode_save = true;
        state->ui_state.step = UI_SHELF;
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(state, "Load", layout.next, button_size, false)) {
        state->ui_state.shelf_mode_save = false;
        state->ui_state.step = UI_SHELF;
    }

    advance_vertical(&layout, 30);

    return layout;
}

// Draw Shelf layer - file browser for load/save
Layout draw_shelf(AppState* state, Layout layout) {
    V2 button_size = {300, 30};
    SDL_Color white = {255, 255, 255, 255};

    if (state->ui_state.shelf_mode_save) {
        render_text(state, "Save work file:", layout.next, white);
        advance_vertical(&layout, 35);

        V2 input_pos = layout.next;
        V2 input_size = {400, 25};
        render_text_input_field(state, state->ui_state.suggested_filename, 256, input_pos, input_size);
        advance_vertical(&layout, 35);

        V2 save_button_size = {80, 25};
        if (render_button(state, "Save", layout.next, save_button_size, false)) {
            work_save(state->work, state->ui_state.suggested_filename);
            state->ui_state.step = UI_LENS;
        }
        advance_horizontal(&layout, save_button_size.x + 10);

        if (render_button(state, "Cancel", layout.next, save_button_size, false)) {
            state->ui_state.step = UI_LENS;
        }
    } else {
        render_text(state, "Select a work file to load:", layout.next, white);
        advance_vertical(&layout, 35);

        FileList file_list;
        get_wo_files(&file_list);

        if (file_list.count == 0) {
            render_text(state, "No .wo files found", layout.next, white);
        } else {
            for (size_t i = 0; i < file_list.count; i++) {
                if (render_button(state, file_list.files[i], layout.next, button_size, false)) {
                    Work* loaded = work_new(128, 128);
                    if (work_load(loaded, file_list.files[i])) {
                        loaded->prev = state->work;
                        loaded->next = state->work->next;
                        if (state->work->next) state->work->next->prev = loaded;
                        state->work->next = loaded;
                        state->work = loaded;
                    } else {
                        work_close(loaded);
                    }
                    state->ui_state.step = UI_LENS;
                    break;
                }
                advance_vertical(&layout, button_size.y + 5);
            }
        }

        advance_vertical(&layout, 15);

        V2 cancel_button_size = {80, 25};
        if (render_button(state, "Cancel", layout.next, cancel_button_size, false)) {
            state->ui_state.step = UI_LENS;
        }
    }

    return layout;
}

Layout draw_lens(AppState* state, Layout layout) {
    V2 button_size = {80, 25};

    // Add band buttons
    if (render_button(state, "+ Band", layout.next, button_size, false)) {
        add_random_band(state);
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(state, "+ Open", layout.next, button_size, false)) {
        add_open_band(state);
    }

    // Move to next row
    advance_vertical(&layout, 30);
    layout.row_start = layout.next;

    // Band list navigation buttons
    V2 nav_button_size = {30, 22};
    if (render_button(state, "Up", layout.next, nav_button_size, false)) {
        if (state->band_offset > 0) {
            state->band_offset--;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 5);

    if (render_button(state, "Dn", layout.next, nav_button_size, false)) {
        if (state->band_offset < (int)state->work->bands.length - 1) {
            state->band_offset++;
        }
    }
    advance_vertical(&layout, 25);

    // Render each band
    char buffer[256];
    for (size_t i = state->band_offset; i < state->work->bands.length; i++) {
        Band* band = &state->work->bands.ptr[i];

        // Band header with band number (show actual index)
        snprintf(buffer, sizeof(buffer), "    %zu:", i + 1);
        SDL_Color band_color = make_color_oklch(band->color.lightness, band->color.chroma, band->color.hue);
        render_text(state, buffer, layout.next, band_color);

        // Label input field next to band number
        V2 label_pos = {layout.next.x + 100, layout.next.y};
        V2 label_size = {120, 20};
        render_text_input_field(state, band->label, 32, label_pos, label_size);
        advance_vertical(&layout, 25);

        // Add split button (S) near the right side
        V2 split_pos = {WINDOW_WIDTH - 50, layout.next.y + 50};
        V2 split_size = {25, 22};
        if (render_button(state, "S", split_pos, split_size, false)) {
            band_array_split(&state->work->bands, i, &state->work->labels);
            break;  // Exit loop since array has changed
        }

        // Add copy button (C) near the right side
        V2 copy_pos = {WINDOW_WIDTH - 50, layout.next.y + 25};
        V2 copy_size = {25, 22};
        if (render_button(state, "C", copy_pos, copy_size, false)) {
            band_array_copy_after(&state->work->bands, i, &state->work->labels);
            break;  // Exit loop since array has changed
        }

        // Add remove button (X) at the right side
        V2 remove_pos = {WINDOW_WIDTH - 50, layout.next.y};
        V2 remove_size = {25, 22};
        SDL_Color red_tint = {200, 100, 100, 255};
        if (render_button(state, "X", remove_pos, remove_size, false)) {
            band_array_remove(&state->work->bands, i);
            break;  // Exit loop since array has changed
        }

        // Band details
        render_text(state, "Start:", layout.next, band_color);
        V2 input_pos = {layout.next.x + 100, layout.next.y};
        V2 input_size = {80, 20};

        // Add follow_previous toggle button next to start field
        V2 follow_button_pos = {input_pos.x + input_size.x + 5, input_pos.y};
        V2 follow_button_size = {25, 20};
        const char* follow_text = band->follow_previous ? "^" : " ";
        if (render_button(state, follow_text, follow_button_pos, follow_button_size, band->follow_previous)) {
            band->follow_previous = !band->follow_previous;
        }

        // Render start field (disabled if follow_previous is true)
        // Scale drag sensitivity based on sliver camera zoom - more zoom means finer control
        float drag_scale = 0.005f / state->sliver_camera.scale;  // Inversely proportional to zoom
        float old_start = band->interval.start;
        render_numeric_input_field_full(state, &band->interval.start, input_pos, input_size, band->follow_previous, drag_scale);
        // By default, move end to keep size fixed (unless Cmd is held for independent movement)
        if (!(SDL_GetModState() & KMOD_GUI) && band->interval.start != old_start) {
            band->interval.end += (band->interval.start - old_start);
        }
        advance_vertical(&layout, 20);

        render_text(state, "  End:", layout.next, band_color);
        input_pos = (V2){layout.next.x + 100, layout.next.y};
        render_numeric_input_field_full(state, &band->interval.end, input_pos, input_size, false, drag_scale);
        advance_vertical(&layout, 20);

        // Hue input field
        render_text(state, "Color:", layout.next, band_color);
        input_pos = (V2){layout.next.x + 100, layout.next.y};

        // Store old hue to detect changes
        float old_hue = band->color.hue;
        render_numeric_input_field_full(state, &band->color.hue, input_pos, input_size, false, 1.0f);  // Scale of 1.0 for 0-360 range

        // Clamp hue to valid range if it changed
        if (band->color.hue != old_hue) {
            while (band->color.hue < 0) band->color.hue += 360.0f;
            while (band->color.hue >= 360.0f) band->color.hue -= 360.0f;
        }

        input_pos = (V2){layout.next.x + 100 + input_size.x, layout.next.y};
        // Store old lightness to detect changes
        float old_lightness = band->color.lightness;
        render_numeric_input_field_full(state, &band->color.lightness, input_pos, input_size, false, 0.003f);  // Scale of 1.0 for 0-360 range

        // Clamp lightness to valid range if it changed
        if (band->color.lightness != old_lightness) {
            while (band->color.lightness < 0) band->color.lightness = 0.0f;
            while (band->color.lightness > 1.0f) band->color.lightness = 1.0f;
        }
        input_pos = (V2){layout.next.x + 100 + input_size.x*2, layout.next.y};
        // Store old chroma to detect changes
        float old_chroma = band->color.chroma;
        render_numeric_input_field_full(state, &band->color.chroma, input_pos, input_size, false, 0.003f);  // Scale of 1.0 for 0-360 range

        // Clamp chroma to valid range if it changed
        if (band->color.chroma != old_chroma) {
            while (band->color.chroma < 0) band->color.chroma = 0.0f;
            while (band->color.chroma > 1.0f) band->color.chroma = 1.0f;
        }
        advance_vertical(&layout, 25);

        // Add band kind toggle button (CLOSED/OPEN)
        const char* band_kind_name = (band->kind == BAND_OPEN) ? "><" : "<>";
        V2 band_kind_pos = {layout.next.x, layout.next.y};
        V2 band_kind_size = {35, 22};
        if (render_button(state, band_kind_name, band_kind_pos, band_kind_size, false)) {
            // Toggle between BAND_CLOSED and BAND_OPEN
            band->kind = (band->kind == BAND_CLOSED) ? BAND_OPEN : BAND_CLOSED;
        }

        // Add line_kind toggle button
        const char* kind_name = "Unknown";
        switch (band->line_kind) {
            case KIND_SHARP: kind_name = "Sharp"; break;
            case KIND_ROUNDED: kind_name = "Rounded"; break;
            case KIND_DOUBLE: kind_name = "Double"; break;
            case KIND_WAVE: kind_name = "Wave"; break;
            default: kind_name = "Unknown"; break;
        }

        V2 button_pos = {layout.next.x + 45, layout.next.y};
        button_size = (V2){80, 22};
        if (render_button(state, kind_name, button_pos, button_size, false)) {
            // Cycle to next line_kind
            band->line_kind = (band->line_kind + 1) % KIND_COUNT;
        }

        // If WAVE line_kind, show wavelength controls
        if (band->line_kind == KIND_WAVE) {
            // Decrement button (decrease exponent)
            V2 dec_pos = {button_pos.x + button_size.x + 10, layout.next.y};
            V2 small_button_size = {20, 22};
            if (render_button(state, "-", dec_pos, small_button_size, false)) {
                if (band->wavelength_scale > 0) {
                    band->wavelength_scale--;
                }
            }

            // Show current value (as 2^n)
            char scale_text[32];
            snprintf(scale_text, sizeof(scale_text), "%d", 1 << band->wavelength_scale);
            V2 text_pos = {dec_pos.x + small_button_size.x + 5, layout.next.y};
            SDL_Color white = {255, 255, 255, 255};
            render_text(state, scale_text, text_pos, white);

            // Increment button (increase exponent)
            V2 inc_pos = {text_pos.x + 40, layout.next.y};
            if (render_button(state, "+", inc_pos, small_button_size, false)) {
                if (band->wavelength_scale < 8) {
                    band->wavelength_scale++;
                }
            }

            // Phase toggle button (false = 180°, true = 0°)
            V2 phase_pos = {inc_pos.x + small_button_size.x + 10, layout.next.y};
            V2 phase_button_size = {80, 22};
            const char* phase_text = band->wave_inverted ? "sin 0" : "sin pi";
            if (render_button(state, phase_text, phase_pos, phase_button_size, band->wave_inverted)) {
                band->wave_inverted = !band->wave_inverted;
            }

            // Half period toggle button (false = half period, true = full period)
            V2 half_pos = {phase_pos.x + phase_button_size.x + 5, layout.next.y};
            V2 half_button_size = {50, 22};
            const char* half_text = band->wave_half_period ? "Full" : "Half";
            if (render_button(state, half_text, half_pos, half_button_size, band->wave_half_period)) {
                band->wave_half_period = !band->wave_half_period;
            }
        }

        advance_vertical(&layout, 25);

        // Label position control - 3x3 anchor grid with offset inputs
        int selected_pos = render_anchor_buttons(state, layout.next, 45, band->label_anchor);
        if (selected_pos >= 0) {
            band->label_anchor = selected_pos;
        }

        // Label offset inputs next to anchor grid
        V2 offset_input_size = {60, 20};
        V2 x_input_pos = {layout.next.x + 50, layout.next.y + 5};
        V2 y_input_pos = {layout.next.x + 50, layout.next.y + 25};

        render_numeric_input_field_full(state, &band->label_offset.x, x_input_pos, offset_input_size, false, 0.5f);
        render_numeric_input_field_full(state, &band->label_offset.y, y_input_pos, offset_input_size, false, 0.5f);

        advance_vertical(&layout, 50);

        advance_vertical(&layout, 10);  // Space between bands
    }

    return layout;
}

void render_ui_panel(AppState* state) {
    // Draw UI panel background
    SDL_SetRenderDrawColor(state->renderer, 40, 40, 45, 255);
    SDL_Rect panel_rect = {VIEWPORT_WIDTH, 0, UI_PANEL_WIDTH, WINDOW_HEIGHT};
    SDL_RenderFillRect(state->renderer, &panel_rect);

    // Draw panel border
    SDL_SetRenderDrawColor(state->renderer, 60, 60, 65, 255);
    SDL_RenderDrawLine(state->renderer, VIEWPORT_WIDTH, 0, VIEWPORT_WIDTH, WINDOW_HEIGHT);

    Layout layout = {
        .next = {VIEWPORT_WIDTH + 20, 20},
        .row_start = {VIEWPORT_WIDTH + 20, 20},
        .max = {WINDOW_WIDTH - 20, WINDOW_HEIGHT - 40}
    };

    // Layer 1: Work management
    layout = draw_work_ui(state, layout);
    layout.row_start = layout.next;

    // Visual separator
    SDL_SetRenderDrawColor(state->renderer, 80, 80, 85, 255);
    SDL_RenderDrawLine(state->renderer, VIEWPORT_WIDTH + 10, layout.next.y, WINDOW_WIDTH - 10, layout.next.y);
    advance_vertical(&layout, 15);

    // Layer 2: Switch between SHELF (file list) and LENS (band editing)
    if (state->ui_state.step == UI_SHELF) {
        layout = draw_shelf(state, layout);
    } else {
        layout = draw_lens(state, layout);
    }
}

void render(AppState* state) {
    // Regenerate squares from bands every frame (immediate mode)
    flatten_bands(&state->work->bands, &state->flattened);

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

        // Draw flattened bands along diagonal
        for (size_t i = 0; i < state->flattened.length; i++) {
            Band* sq = &state->flattened.ptr[i];

            // Apply sliver transform to the square's interval (from 0-10 space to viewport space)
            Interval transformed = sliver_transform_interval(sq->interval, &state->sliver_camera);

            // Convert to 0-1 range for diagonal lerp (divide by 10 since intervals are in 0-10)
            // Skip bands completely outside the visible range
            if ((transformed.start > 1.0f && transformed.end > 1.0f) ||
                (transformed.start < 0.0f && transformed.end < 0.0f)) {
                continue;
            }

            // Check which endpoints are in range before clamping
            bool start_in_range = (transformed.start >= 0.0f && transformed.start <= 1.0f);
            bool end_in_range = (transformed.end >= 0.0f && transformed.end <= 1.0f);

            // Calculate hide flags based on diagonal orientation
            int hide_flags = 0;
            switch (state->selected_corner) {
                case CORNER_TL:  // BL→TR diagonal
                    if (!start_in_range) hide_flags |= EDGE_LEFT | EDGE_BOTTOM;
                    if (!end_in_range) hide_flags |= EDGE_TOP | EDGE_RIGHT;
                    break;
                case CORNER_TR:  // TL→BR diagonal
                    if (!start_in_range) hide_flags |= EDGE_TOP | EDGE_LEFT;
                    if (!end_in_range) hide_flags |= EDGE_BOTTOM | EDGE_RIGHT;
                    break;
                case CORNER_BR:  // TR→BL diagonal
                    if (!start_in_range) hide_flags |= EDGE_TOP | EDGE_RIGHT;
                    if (!end_in_range) hide_flags |= EDGE_BOTTOM | EDGE_LEFT;
                    break;
                case CORNER_BL:  // BR→TL diagonal
                    if (!start_in_range) hide_flags |= EDGE_BOTTOM | EDGE_RIGHT;
                    if (!end_in_range) hide_flags |= EDGE_TOP | EDGE_LEFT;
                    break;
                default:
                    break;
            }

            // Clamp to visible range [0, 1]
            transformed.start = fmaxf(0.0f, fminf(1.0f, transformed.start));
            transformed.end = fmaxf(0.0f, fminf(1.0f, transformed.end));

            // Get the two diagonal endpoints for this band
            V2 p1 = v2_lerp(state->diagonal.start, state->diagonal.end, transformed.start);
            V2 p2 = v2_lerp(state->diagonal.start, state->diagonal.end, transformed.end);

            // Draw band using geometry buffer based on line_kind
            float min_x = fminf(p1.x, p2.x);
            float max_x = fmaxf(p1.x, p2.x);
            float min_y = fminf(p1.y, p2.y);
            float max_y = fmaxf(p1.y, p2.y);

            // Convert LCH to SDL_Color for rendering
            SDL_Color sdl_color = make_color_oklch(sq->color.lightness, sq->color.chroma, sq->color.hue);

            switch (sq->line_kind) {
                case KIND_ROUNDED: {
                    float base_radius = fminf(25.0f, fminf(max_x - min_x, max_y - min_y) * 0.2f);
                    float extend = base_radius * (1.0f - 1.0f/sqrtf(2));
                    draw_rounded_rect_geometry(&state->render_ctx.geometry,
                                              min_x - extend, min_y - extend,
                                              (max_x - min_x) + 2 * extend,
                                              (max_y - min_y) + 2 * extend,
                                              base_radius, sdl_color, &state->camera, hide_flags);
                    break;
                }
                case KIND_DOUBLE:
                    draw_double_rect_geometry(&state->render_ctx.geometry,
                                            min_x, min_y, max_x - min_x, max_y - min_y,
                                            sdl_color, &state->camera, hide_flags);
                    break;
                case KIND_WAVE:
                    draw_wave_rect_geometry(&state->render_ctx.geometry,
                                          min_x, min_y, max_x - min_x, max_y - min_y,
                                          sdl_color, &state->camera, sq->wavelength_scale,
                                          sq->wave_inverted, sq->wave_half_period, hide_flags);
                    break;
                case KIND_SHARP:
                default:
                    draw_rounded_rect_geometry(&state->render_ctx.geometry,
                                              min_x, min_y, max_x - min_x, max_y - min_y,
                                              0, sdl_color, &state->camera, hide_flags);
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

    // Draw labels for flattened bands (after geometry, still within clipping rect)
    for (size_t i = 0; i < state->flattened.length; i++) {
        Band* sq = &state->flattened.ptr[i];  // Using Band instead of Square

        // Skip if no label
        if (!sq->label || sq->label[0] == '\0') continue;

        // Apply sliver transform to the band's interval
        Interval transformed = sliver_transform_interval(sq->interval, &state->sliver_camera);

        // Skip bands outside visible range
        if ((transformed.start > 1.0f && transformed.end > 1.0f) ||
            (transformed.start < 0.0f && transformed.end < 0.0f)) {
            continue;
        }

        // Clamp to visible range
        float t_start = fmaxf(0.0f, fminf(1.0f, transformed.start));
        float t_end = fmaxf(0.0f, fminf(1.0f, transformed.end));

        // Calculate band corners on diagonal
        V2 p1 = v2_lerp(state->diagonal.start, state->diagonal.end, t_start);
        V2 p2 = v2_lerp(state->diagonal.start, state->diagonal.end, t_end);

        // Get bounding box
        float min_x = fminf(p1.x, p2.x);
        float min_y = fminf(p1.y, p2.y);
        float max_x = fmaxf(p1.x, p2.x);
        float max_y = fmaxf(p1.y, p2.y);

        // Position label based on band's label anchor
        float padding = 3.0f;
        float center_x = (min_x + max_x) / 2.0f;
        float center_y = (min_y + max_y) / 2.0f;

        V2 world_pos;
        switch (sq->label_anchor) {
            case LABEL_TOP_LEFT:
                world_pos = (V2){min_x - padding, min_y - padding};
                break;
            case LABEL_TOP_CENTER:
                world_pos = (V2){center_x, min_y - padding};
                break;
            case LABEL_TOP_RIGHT:
                world_pos = (V2){max_x + padding, min_y - padding};
                break;
            case LABEL_MIDDLE_LEFT:
                world_pos = (V2){min_x - padding, center_y};
                break;
            case LABEL_CENTER:
                world_pos = (V2){center_x, center_y};
                break;
            case LABEL_MIDDLE_RIGHT:
                world_pos = (V2){max_x + padding, center_y};
                break;
            case LABEL_BOTTOM_LEFT:
                world_pos = (V2){min_x - padding, max_y + padding};
                break;
            case LABEL_BOTTOM_CENTER:
                world_pos = (V2){center_x, max_y + padding};
                break;
            case LABEL_BOTTOM_RIGHT:
            default:
                world_pos = (V2){max_x + padding, max_y + padding};
                break;
        }

        // Apply label offset
        world_pos = v2_add(world_pos, sq->label_offset);
        V2 label_pos = world_to_screen(world_pos, &state->camera);

        SDL_Color label_color = make_color_oklch(sq->color.lightness, sq->color.chroma, sq->color.hue);
        render_text(state, sq->label, label_pos, label_color);
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
    state.ui_state.step = UI_LENS;
    state.ui_state.shelf_mode_save = false;
    state.ui_state.suggested_filename[0] = '\0';
    state.selected_corner = CORNER_BR;  // Start with bottom-right selected
    state.label_anchor = LABEL_BOTTOM_RIGHT;  // Start with bottom-right labels
    state.band_offset = 0;  // Start at beginning of band list
    state.bounding_center = (V2){VIEWPORT_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center in viewport
    state.bounding_half = (BOUNDING_SIZE - BOUNDING_PADDING) / 2;

    // Initialize camera (centered in viewport)
    state.camera.offset = (V2){VIEWPORT_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center of viewport
    state.camera.scale = 1.0f;  // Default zoom
    state.dragging = false;

    // Initialize sliver camera to show 0-10 range
    state.sliver_camera.offset = 0.0f;  // Start at 0
    state.sliver_camera.scale = 0.1f;   // Scale 1:1 shows full 0-1 in viewport, which maps to 0-10 in our coordinates

    // Initialize work (bands and labels in single arena)
    state.work = work_new(128, 128);  // 128 bands, 128 labels

    // Initialize flattened array separately (dynamic, not part of work)
    band_array_init(&state.flattened, 128*100);  // Large capacity for flattened bands

    string_builder_init(&state.string_builder);

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
    init_bands_rand(&state);

    // Enable text input for input fields
    SDL_StartTextInput();

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

                case SDL_TEXTINPUT:
                    // Handle text input for active input field
                    if (state.active_field.ptr != NULL) {
                        const char* text = event.text.text;

                        while (*text) {
                            char c = *text++;
                            bool accept = false;

                            if (!state.active_field.is_numeric) {
                                // Accept any printable character for text fields
                                accept = (c >= 32 && c <= 126);
                            } else {
                                // Accept numbers, decimal point, and minus sign for numeric fields
                                accept = ((c >= '0' && c <= '9') || c == '.' || (c == '-' && state.cursor_pos == 0));
                            }

                            if (accept) {
                                // Insert character at cursor position
                                int len = strlen(state.input_buffer);
                                if (len < sizeof(state.input_buffer) - 1) {
                                    memmove(&state.input_buffer[state.cursor_pos + 1],
                                           &state.input_buffer[state.cursor_pos],
                                           len - state.cursor_pos + 1);
                                    state.input_buffer[state.cursor_pos] = c;
                                    state.cursor_pos++;
                                }
                            }
                        }
                    }
                    break;

                case SDL_KEYDOWN:
                    // Handle input field keyboard input first
                    if (state.active_field.ptr != NULL) {
                        float* active_value = state.active_field.is_numeric ? (float*)state.active_field.ptr : NULL;
                        SDL_Keycode key = event.key.keysym.sym;
                        SDL_Keymod mod = SDL_GetModState();

                        switch (key) {
                            case SDLK_ESCAPE: {
                                // Cancel editing - for text fields, no need to restore
                                // For numeric fields, restore original value
                                if (state.active_field.is_numeric && active_value) {
                                    *active_value = state.input_original_value;
                                }
                                state.active_field.ptr = NULL;
                                // Keep text input active
                                break;
                            }

                            case SDLK_RETURN:
                            case SDLK_KP_ENTER: {
                                // Apply value and deactivate
                                if (!state.active_field.is_numeric) {
                                    // It's a text field - copy directly to the pointer
                                    char* text_field = (char*)state.active_field.ptr;
                                    strncpy(text_field, state.input_buffer, 31);  // Band labels are 32 bytes
                                    text_field[31] = '\0';
                                } else if (active_value && strlen(state.input_buffer) > 0) {
                                    *active_value = atof(state.input_buffer);
                                }
                                state.active_field.ptr = NULL;
                                // Don't stop text input - it causes issues with subsequent fields
                                break;
                            }

                            case SDLK_UP:
                                // Increment value (only for numeric fields)
                                if (state.active_field.is_numeric && active_value) {
                                    if (mod & KMOD_SHIFT) {
                                        *active_value += 1.0f;
                                    } else if (mod & KMOD_CTRL) {
                                        *active_value += 0.01f;
                                    } else {
                                        *active_value += 0.1f;
                                    }
                                    snprintf(state.input_buffer, sizeof(state.input_buffer), "%.2f", *active_value);
                                    state.cursor_pos = strlen(state.input_buffer);
                                }
                                break;

                            case SDLK_DOWN:
                                // Decrement value (only for numeric fields)
                                if (state.active_field.is_numeric && active_value) {
                                    if (mod & KMOD_SHIFT) {
                                        *active_value -= 1.0f;
                                    } else if (mod & KMOD_CTRL) {
                                        *active_value -= 0.01f;
                                    } else {
                                        *active_value -= 0.1f;
                                    }
                                    snprintf(state.input_buffer, sizeof(state.input_buffer), "%.2f", *active_value);
                                    state.cursor_pos = strlen(state.input_buffer);
                                }
                                break;

                            case SDLK_BACKSPACE:
                                // Delete character before cursor
                                if (state.cursor_pos > 0) {
                                    memmove(&state.input_buffer[state.cursor_pos - 1],
                                           &state.input_buffer[state.cursor_pos],
                                           strlen(&state.input_buffer[state.cursor_pos]) + 1);
                                    state.cursor_pos--;
                                }
                                break;

                            case SDLK_LEFT:
                                // Move cursor left
                                if (state.cursor_pos > 0) {
                                    state.cursor_pos--;
                                }
                                break;

                            case SDLK_RIGHT:
                                // Move cursor right
                                if (state.cursor_pos < strlen(state.input_buffer)) {
                                    state.cursor_pos++;
                                }
                                break;

                            case SDLK_HOME:
                                state.cursor_pos = 0;
                                break;

                            case SDLK_END:
                                state.cursor_pos = strlen(state.input_buffer);
                                break;

                            default:
                                // Handle text input in SDL_TEXTINPUT event
                                break;
                        }
                        break;  // Don't process other keys when input field is active
                    }

                    // Normal key handling when no input field is active
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

    // Cleanup - free all works in list
    Work* work = state.work;
    while (work && work->prev) work = work->prev;
    while (work) {
        Work* next = work->next;
        work_close(work);
        work = next;
    }
    band_array_free(&state.flattened);
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
