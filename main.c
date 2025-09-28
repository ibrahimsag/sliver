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

#define WORK_FILE_VERSION 1

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

typedef struct {
    V2 next;       // Next position to draw at
    V2 row_start;  // Start position of current row for horizontal layouts
    V2 max;        // Maximum bounds for layout
} Layout;

typedef struct {
    float start, end;
} Interval;


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

typedef enum {
    INPUT_NONE = 0,
    INPUT_DISABLED = 1,
    INPUT_HIGHLIGHTED = 2
} InputFlags;

#define DISABLED(b) ((b) ? INPUT_DISABLED : INPUT_NONE)
#define HIGHLIGHTED(b) ((b) ? INPUT_HIGHLIGHTED : INPUT_NONE)

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
    char label[32];  // Label text for the band
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
    char filename[256];    // Last opened/saved filename
    uint32_t saved_hash;   // Hash of arena at last save/load
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
    const char* label;
    V2 position;  // Screen position
    SDL_Color color;
    LabelAnchor anchor;  // Anchor for text alignment
} LabelDraw;

typedef struct {
    LabelDraw* ptr;
    size_t length;
    size_t capacity;
} LabelDrawArray;

typedef struct {
    GeometryBuffer geometry;
    SDL_Texture* white_texture;  // 1x1 white texture for solid colors
    LabelDrawArray labels;  // Labels to draw
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
    bool one_side;  // Draw only on one side of diagonal
    bool running;
} Atelier;

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

V2 get_corner_position(Atelier* atelier, Corner corner) {
    float left = atelier->bounding_center.x - atelier->bounding_half;
    float right = atelier->bounding_center.x + atelier->bounding_half;
    float top = atelier->bounding_center.y - atelier->bounding_half;
    float bottom = atelier->bounding_center.y + atelier->bounding_half;

    switch (corner) {
        case CORNER_TL: return (V2){left, top};
        case CORNER_TR: return (V2){right, top};
        case CORNER_BR: return (V2){right, bottom};
        case CORNER_BL: return (V2){left, bottom};
        default: return (V2){0, 0};
    }
}

void calculate_diagonal(Atelier* atelier) {
    if (atelier->selected_corner == CORNER_NONE) return;

    V2 tl = get_corner_position(atelier, CORNER_TL);
    V2 tr = get_corner_position(atelier, CORNER_TR);
    V2 br = get_corner_position(atelier, CORNER_BR);
    V2 bl = get_corner_position(atelier, CORNER_BL);

    // Based on selected corner, set diagonal endpoints
    switch (atelier->selected_corner) {
        case CORNER_TL:  // Triangle (BL, TR, TL) -> diagonal BL to TR
            atelier->diagonal.start = bl;
            atelier->diagonal.end = tr;
            break;
        case CORNER_TR:  // Triangle (TL, BR, TR) -> diagonal TL to BR
            atelier->diagonal.start = tl;
            atelier->diagonal.end = br;
            break;
        case CORNER_BR:  // Triangle (TR, BL, BR) -> diagonal TR to BL
            atelier->diagonal.start = tr;
            atelier->diagonal.end = bl;
            break;
        case CORNER_BL:  // Triangle (BR, TL, BL) -> diagonal BR to TL
            atelier->diagonal.start = br;
            atelier->diagonal.end = tl;
            break;
        default:
            break;
    }
}

int calculate_edge_flags(Atelier* atelier, float start, float end) {
    int edge_flags = EDGE_TOP | EDGE_RIGHT | EDGE_BOTTOM | EDGE_LEFT;  // Start with all visible

    bool start_in_range = (start >= 0.0f && start <= 1.0f);
    bool end_in_range = (end >= 0.0f && end <= 1.0f);

    switch (atelier->selected_corner) {
        case CORNER_TL:  // BL→TR diagonal
            if (!start_in_range) edge_flags &= ~(EDGE_LEFT | EDGE_BOTTOM);
            if (!end_in_range) edge_flags &= ~(EDGE_TOP | EDGE_RIGHT);
            if (atelier->one_side) edge_flags &= ~(EDGE_TOP | EDGE_LEFT);
            break;
        case CORNER_TR:  // TL→BR diagonal
            if (!start_in_range) edge_flags &= ~(EDGE_TOP | EDGE_LEFT);
            if (!end_in_range) edge_flags &= ~(EDGE_BOTTOM | EDGE_RIGHT);
            if (atelier->one_side) edge_flags &= ~(EDGE_TOP | EDGE_RIGHT);
            break;
        case CORNER_BR:  // TR→BL diagonal
            if (!start_in_range) edge_flags &= ~(EDGE_TOP | EDGE_RIGHT);
            if (!end_in_range) edge_flags &= ~(EDGE_BOTTOM | EDGE_LEFT);
            if (atelier->one_side) edge_flags &= ~(EDGE_BOTTOM | EDGE_RIGHT);
            break;
        case CORNER_BL:  // BR→TL diagonal
            if (!start_in_range) edge_flags &= ~(EDGE_BOTTOM | EDGE_RIGHT);
            if (!end_in_range) edge_flags &= ~(EDGE_TOP | EDGE_LEFT);
            if (atelier->one_side) edge_flags &= ~(EDGE_BOTTOM | EDGE_LEFT);
            break;
        default:
            break;
    }
    return edge_flags;
}

LabelAnchor rotate_anchor_for_corner(LabelAnchor anchor, Corner corner) {
    if (corner == CORNER_BR) return anchor;

    int r = anchor / 3;
    int c = anchor % 3;

    switch (corner) {
        case CORNER_TR:
              // 90° rotation: (2-c, r) = (r+1, c+1) . (-c, r) . (r-1, c-1)
            return (LabelAnchor)((2 - c) * 3 + r);
        case CORNER_TL:
              // 180° rotation: (2-r, 2-c) = (2-c, r) . (2-c, r)
            return (LabelAnchor)((2 - r) * 3 + (2 - c));
        case CORNER_BL:
              // 270° rotation: (c, 2-r) = (2-r, 2-c) . (2-c, r)
            return (LabelAnchor)(c * 3 + (2 - r));
        default:
            return anchor;
    }
}

V2 anchor_to_position(LabelAnchor anchor, Interval transformed, Diagonal diagonal) {
    // Clamp to visible range [0, 1]
    transformed.start = fmaxf(0.0f, fminf(1.0f, transformed.start));
    transformed.end = fmaxf(0.0f, fminf(1.0f, transformed.end));

    // Get the two diagonal endpoints
    V2 p1 = v2_lerp(diagonal.start, diagonal.end, transformed.start);
    V2 p2 = v2_lerp(diagonal.start, diagonal.end, transformed.end);

    // Calculate bounding box
    float min_x = fminf(p1.x, p2.x);
    float max_x = fmaxf(p1.x, p2.x);
    float min_y = fminf(p1.y, p2.y);
    float max_y = fmaxf(p1.y, p2.y);

    float center_x = (min_x + max_x) / 2.0f;
    float center_y = (min_y + max_y) / 2.0f;
    float padding = 3.0f;

    switch (anchor) {
        case LABEL_TOP_LEFT:
            return (V2){min_x - padding, min_y - padding};
        case LABEL_TOP_CENTER:
            return (V2){center_x, min_y - padding};
        case LABEL_TOP_RIGHT:
            return (V2){max_x + padding, min_y - padding};
        case LABEL_MIDDLE_LEFT:
            return (V2){min_x - padding, center_y};
        case LABEL_CENTER:
            return (V2){center_x, center_y};
        case LABEL_MIDDLE_RIGHT:
            return (V2){max_x + padding, center_y};
        case LABEL_BOTTOM_LEFT:
            return (V2){min_x - padding, max_y + padding};
        case LABEL_BOTTOM_CENTER:
            return (V2){center_x, max_y + padding};
        case LABEL_BOTTOM_RIGHT:
        default:
            return (V2){max_x + padding, max_y + padding};
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

// Layout functions
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

    // Initialize label array
    ctx->labels.capacity = 256;
    ctx->labels.length = 0;
    ctx->labels.ptr = malloc(ctx->labels.capacity * sizeof(LabelDraw));
}

void render_context_free(RenderContext* ctx) {
    geometry_buffer_free(&ctx->geometry);
    if (ctx->white_texture) {
        SDL_DestroyTexture(ctx->white_texture);
        ctx->white_texture = NULL;
    }
    free(ctx->labels.ptr);
}

V2 align_text_position(V2 position, int width, int height, LabelAnchor anchor) {
    int row = anchor / 3;
    int col = anchor % 3;

    float x_offset = 0, y_offset = 0;

    switch (col) {
        case 0: x_offset = 0; break;
        case 1: x_offset = -width / 2.0f; break;
        case 2: x_offset = -width; break;
    }

    switch (row) {
        case 0: y_offset = 0; break;
        case 1: y_offset = -height / 2.0f; break;
        case 2: y_offset = -height; break;
    }

    return (V2){position.x + x_offset, position.y + y_offset};
}

void render_text(Atelier* atelier, const char* text, V2 position, SDL_Color color) {
    if (!atelier->font || !text) return;

    // Render at 2x size for supersampling
    SDL_Surface* surface = TTF_RenderText_Blended(atelier->font, text, color);
    if (!surface) return;

    SDL_Texture* texture = SDL_CreateTextureFromSurface(atelier->renderer, surface);
    if (texture) {
        // Scale down to half size for supersampling effect
        SDL_Rect dest = {
            (int)position.x,
            (int)position.y,
            surface->w / 2,  // Half width for supersampling
            surface->h / 2   // Half height for supersampling
        };
        SDL_RenderCopy(atelier->renderer, texture, NULL, &dest);
        SDL_DestroyTexture(texture);
    }
    SDL_FreeSurface(surface);
}

void render_text_aligned(Atelier* atelier, const char* text, V2 position, SDL_Color color, LabelAnchor anchor) {
    if (!atelier->font || !text) return;

    // Render at 2x size for supersampling
    SDL_Surface* surface = TTF_RenderText_Blended(atelier->font, text, color);
    if (!surface) return;

    int width = surface->w / 2;
    int height = surface->h / 2;
    V2 aligned_pos = align_text_position(position, width, height, anchor);

    SDL_Texture* texture = SDL_CreateTextureFromSurface(atelier->renderer, surface);
    if (texture) {
        SDL_Rect dest = {
            (int)aligned_pos.x,
            (int)aligned_pos.y,
            width,
            height
        };
        SDL_RenderCopy(atelier->renderer, texture, NULL, &dest);
        SDL_DestroyTexture(texture);
    }
    SDL_FreeSurface(surface);
}

// Render 3x3 anchor buttons for label position selection
// Returns the selected position (0-8) or -1 if no selection
LabelAnchor inverse_rotate_anchor_for_corner(LabelAnchor anchor, Corner corner) {
    if (corner == CORNER_BR) return anchor;

    int r = anchor / 3;
    int c = anchor % 3;

    switch (corner) {
        case CORNER_TR:
            return (LabelAnchor)(c * 3 + (2 - r));
        case CORNER_TL:
            return (LabelAnchor)((2 - r) * 3 + (2 - c));
        case CORNER_BL:
            return (LabelAnchor)((2 - c) * 3 + r);
        default:
            return anchor;
    }
}

int render_anchor_buttons(Atelier* atelier, V2 position, float size, LabelAnchor current) {
    float button_size = size / 3.0f;
    int selected = -1;

    for (int row = 0; row < 3; row++) {
        for (int col = 0; col < 3; col++) {
            int index = row * 3 + col;
            LabelAnchor rotated = inverse_rotate_anchor_for_corner(index, atelier->selected_corner);
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
            bool hover = atelier->mouse_pos.x >= rect.x && atelier->mouse_pos.x < rect.x + rect.w &&
                        atelier->mouse_pos.y >= rect.y && atelier->mouse_pos.y < rect.y + rect.h;

            // Draw background
            if (rotated == current) {
                SDL_SetRenderDrawColor(atelier->renderer, 100, 150, 200, 255);
            } else if (hover) {
                SDL_SetRenderDrawColor(atelier->renderer, 70, 70, 75, 255);
            } else {
                SDL_SetRenderDrawColor(atelier->renderer, 45, 45, 50, 255);
            }
            SDL_RenderFillRect(atelier->renderer, &rect);

            // Draw border
            SDL_SetRenderDrawColor(atelier->renderer, 80, 80, 85, 255);
            SDL_RenderDrawRect(atelier->renderer, &rect);

            // Draw small indicator dot in center
            SDL_Rect dot = {
                (int)(button_pos.x + button_size/2 - 2),
                (int)(button_pos.y + button_size/2 - 2),
                4, 4
            };
            SDL_SetRenderDrawColor(atelier->renderer, 200, 200, 200, 255);
            SDL_RenderFillRect(atelier->renderer, &dot);

            // Check for click
            if (hover && atelier->mouse_pressed && !atelier->mouse_was_pressed &&
                atelier->dragging_input_field == NULL) {
                selected = rotated;
            }
        }
    }

    return selected;
}

// Simple immediate mode button
bool render_button(Atelier* atelier, const char* text, V2 position, V2 size, InputFlags flags) {
    SDL_Rect button_rect = {
        (int)position.x,
        (int)position.y,
        (int)size.x,
        (int)size.y
    };

    bool disabled = (flags == INPUT_DISABLED);
    bool highlighted = (flags == INPUT_HIGHLIGHTED);

    // Check if mouse is over button
    bool hover = !disabled && atelier->mouse_pos.x >= button_rect.x && atelier->mouse_pos.x < button_rect.x + button_rect.w &&
                 atelier->mouse_pos.y >= button_rect.y && atelier->mouse_pos.y < button_rect.y + button_rect.h;

    // Draw button background
    if (disabled) {
        SDL_SetRenderDrawColor(atelier->renderer, 35, 35, 38, 255);
    } else if (highlighted) {
        SDL_SetRenderDrawColor(atelier->renderer, 100, 150, 200, 255);
    } else if (hover) {
        SDL_SetRenderDrawColor(atelier->renderer, 70, 70, 75, 255);
    } else {
        SDL_SetRenderDrawColor(atelier->renderer, 50, 50, 55, 255);
    }
    SDL_RenderFillRect(atelier->renderer, &button_rect);

    // Draw button border
    SDL_SetRenderDrawColor(atelier->renderer, 90, 90, 95, 255);
    SDL_RenderDrawRect(atelier->renderer, &button_rect);

    // Draw button text centered
    if (atelier->font) {
        V2 text_pos = {
            position.x + size.x / 2.0f,
            position.y + size.y / 2.0f - 9  // Rough centering
        };

        // Measure text to center it properly
        int text_w, text_h;
        TTF_SizeText(atelier->font, text, &text_w, &text_h);
        text_pos.x -= (text_w / 2) / 2.0f;  // Adjust for supersampling

        SDL_Color text_color = disabled ? (SDL_Color){100, 100, 100, 255} : (SDL_Color){200, 200, 200, 255};
        render_text(atelier, text, text_pos, text_color);
    }

    // Return true if button was clicked (mouse was pressed last frame, not pressed this frame, and hovering)
    // But not if we're dragging an input field
    return !disabled && hover && !atelier->mouse_pressed && atelier->mouse_was_pressed && !atelier->dragging_input_field;
}

// Full version of input field with disabled option and custom scale
void render_numeric_input_field_full(Atelier* atelier, float* value, V2 position, V2 size, bool disabled, float drag_scale) {
    bool is_active = (atelier->active_field.ptr == value && atelier->active_field.is_numeric);
    bool is_dragging = (atelier->dragging_input_field == value);

    SDL_Rect field_rect = {
        (int)position.x,
        (int)position.y,
        (int)size.x,
        (int)size.y
    };

    // Check if mouse is over field
    bool hover = atelier->mouse_pos.x >= field_rect.x && atelier->mouse_pos.x < field_rect.x + field_rect.w &&
                 atelier->mouse_pos.y >= field_rect.y && atelier->mouse_pos.y < field_rect.y + field_rect.h;

    // Handle mouse interactions (only if not disabled)
    if (hover && atelier->mouse_pressed && !atelier->mouse_was_pressed && !disabled) {
        // Start dragging immediately on press
        atelier->dragging_input_field = value;
        atelier->drag_start_value = *value;
        atelier->drag_start_mouse_x = atelier->mouse_pos.x;
    } else if (!hover && atelier->mouse_pressed && is_active) {
        // Click outside - deactivate and apply value
        if (strlen(atelier->input_buffer) > 0) {
            float new_value = atof(atelier->input_buffer);
            *value = new_value;
        }
        atelier->active_field.ptr = NULL;
    }

    // Handle dragging
    if (is_dragging) {
        if (atelier->mouse_pressed) {
            // Update value based on mouse drag
            float delta_x = atelier->mouse_pos.x - atelier->drag_start_mouse_x;
            float scale = drag_scale;  // Use provided scale
            if (SDL_GetModState() & KMOD_CTRL) {
                scale = drag_scale * 0.1f;  // Fine control with Ctrl
            } else if (SDL_GetModState() & KMOD_ALT) {
                scale = drag_scale * 10.0f;    // Coarse control with Alt
            }
            *value = atelier->drag_start_value + delta_x * scale;


            // Update display if this field is also active
            if (is_active) {
                snprintf(atelier->input_buffer, sizeof(atelier->input_buffer), "%.2f", *value);
                atelier->cursor_pos = strlen(atelier->input_buffer);
            }
        } else {
            // Mouse released - check if it was a click (minimal movement)
            float movement = fabs(atelier->mouse_pos.x - atelier->drag_start_mouse_x);
            if (movement < 3.0f && hover && !is_active) {
                // It was a click, activate text input
                atelier->active_field.ptr = value;
                atelier->active_field.is_numeric = true;
                atelier->input_original_value = *value;
                snprintf(atelier->input_buffer, sizeof(atelier->input_buffer), "%.2f", *value);
                atelier->cursor_pos = strlen(atelier->input_buffer);
            }
            // Stop dragging
            atelier->dragging_input_field = NULL;
        }
    }

    // Draw field background
    if (disabled) {
        SDL_SetRenderDrawColor(atelier->renderer, 25, 25, 30, 255);  // Darker when disabled
    } else if (is_dragging) {
        SDL_SetRenderDrawColor(atelier->renderer, 70, 55, 70, 255);  // Purple tint when dragging
    } else if (is_active) {
        SDL_SetRenderDrawColor(atelier->renderer, 60, 65, 70, 255);
    } else if (hover) {
        SDL_SetRenderDrawColor(atelier->renderer, 45, 45, 50, 255);
    } else {
        SDL_SetRenderDrawColor(atelier->renderer, 35, 35, 40, 255);
    }
    SDL_RenderFillRect(atelier->renderer, &field_rect);

    // Draw field border
    if (disabled) {
        SDL_SetRenderDrawColor(atelier->renderer, 50, 50, 55, 255);  // Dim border when disabled
    } else if (is_dragging) {
        SDL_SetRenderDrawColor(atelier->renderer, 150, 100, 200, 255);  // Purple border when dragging
    } else if (is_active) {
        SDL_SetRenderDrawColor(atelier->renderer, 100, 150, 200, 255);
    } else {
        SDL_SetRenderDrawColor(atelier->renderer, 70, 70, 75, 255);
    }
    SDL_RenderDrawRect(atelier->renderer, &field_rect);

    // Display text
    char display_text[64];
    if (is_active) {
        // Show input buffer with cursor
        strncpy(display_text, atelier->input_buffer, sizeof(display_text));
    } else {
        // Show current value
        snprintf(display_text, sizeof(display_text), "%.2f", *value);
    }

    // Render text
    if (atelier->font && strlen(display_text) > 0) {
        SDL_Color white = {255, 255, 255, 255};
        V2 text_pos = {position.x + 4, position.y + 2};
        render_text(atelier, display_text, text_pos, white);

        // Draw cursor if active
        if (is_active && atelier->font) {
            // Calculate cursor position using actual text width
            char temp[64];
            strncpy(temp, atelier->input_buffer, atelier->cursor_pos);
            temp[atelier->cursor_pos] = '\0';

            int text_width = 0;
            if (strlen(temp) > 0) {
                // Measure text width using TTF
                TTF_SizeText(atelier->font, temp, &text_width, NULL);
                // Scale down because we render at 2x and scale down
                text_width /= 2;
            }

            float cursor_x = text_pos.x + text_width;
            SDL_SetRenderDrawColor(atelier->renderer, 255, 255, 255, 255);
            SDL_RenderDrawLine(atelier->renderer, cursor_x, position.y + 2, cursor_x, position.y + size.y - 2);
        }
    }
}

// Text input field for editing strings
void render_text_input_field(Atelier* atelier, char* text, size_t max_len, V2 position, V2 size) {
    bool is_active = (atelier->active_field.ptr == text && !atelier->active_field.is_numeric);

    SDL_Rect field_rect = {
        (int)position.x,
        (int)position.y,
        (int)size.x,
        (int)size.y
    };

    // Check if mouse is over field
    bool hover = atelier->mouse_pos.x >= field_rect.x && atelier->mouse_pos.x < field_rect.x + field_rect.w &&
                 atelier->mouse_pos.y >= field_rect.y && atelier->mouse_pos.y < field_rect.y + field_rect.h;

    // Handle mouse click to activate
    if (hover && atelier->mouse_pressed && !atelier->mouse_was_pressed && !is_active) {
        atelier->active_field.ptr = text;
        atelier->active_field.is_numeric = false;
        // Copy current text to input buffer
        strncpy(atelier->input_buffer, text, sizeof(atelier->input_buffer) - 1);
        atelier->input_buffer[sizeof(atelier->input_buffer) - 1] = '\0';
        atelier->cursor_pos = strlen(atelier->input_buffer);
        SDL_StartTextInput();
    } else if (!hover && atelier->mouse_pressed && is_active) {
        // Click outside - deactivate and save
        strncpy(text, atelier->input_buffer, max_len - 1);
        text[max_len - 1] = '\0';
        atelier->active_field.ptr = NULL;
        // Keep text input active
    }

    // Draw field background
    if (is_active) {
        SDL_SetRenderDrawColor(atelier->renderer, 60, 65, 70, 255);
    } else if (hover) {
        SDL_SetRenderDrawColor(atelier->renderer, 45, 45, 50, 255);
    } else {
        SDL_SetRenderDrawColor(atelier->renderer, 35, 35, 40, 255);
    }
    SDL_RenderFillRect(atelier->renderer, &field_rect);

    // Draw field border
    if (is_active) {
        SDL_SetRenderDrawColor(atelier->renderer, 100, 150, 200, 255);
    } else {
        SDL_SetRenderDrawColor(atelier->renderer, 70, 70, 75, 255);
    }
    SDL_RenderDrawRect(atelier->renderer, &field_rect);

    // Display text
    const char* display_text = is_active ? atelier->input_buffer : text;
    if (atelier->font && display_text && strlen(display_text) > 0) {
        SDL_Color white = {255, 255, 255, 255};
        V2 text_pos = {position.x + 4, position.y + 2};
        render_text(atelier, display_text, text_pos, white);

        // Draw cursor if active
        if (is_active) {
            // Calculate cursor position
            char temp[256] = {0};
            strncpy(temp, atelier->input_buffer, atelier->cursor_pos);
            temp[atelier->cursor_pos] = '\0';

            int text_width = 0;
            if (strlen(temp) > 0) {
                TTF_SizeText(atelier->font, temp, &text_width, NULL);
                text_width /= 2;  // Account for supersampling
            }

            float cursor_x = text_pos.x + text_width;
            SDL_SetRenderDrawColor(atelier->renderer, 255, 255, 255, 255);
            SDL_RenderDrawLine(atelier->renderer, cursor_x, position.y + 2, cursor_x, position.y + size.y - 2);
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
void render_wave_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, SDL_Color color, Camera* camera, int wavelength_scale, bool inverted, bool half_period, int edge_flags) {
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
    if (edge_flags & EDGE_TOP)
        geometry_buffer_add_wave_line(gb, tl, tr, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Top
    if (edge_flags & EDGE_RIGHT)
        geometry_buffer_add_wave_line(gb, tr, br, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Right
    if (edge_flags & EDGE_BOTTOM)
        geometry_buffer_add_wave_line(gb, br, bl, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Bottom
    if (edge_flags & EDGE_LEFT)
        geometry_buffer_add_wave_line(gb, bl, tl, thickness, screen_amplitude, screen_wavelength, color, inverted, half_period);  // Left
}

// Draw double-line rectangle with geometry buffer
void render_double_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, SDL_Color color, Camera* camera, int edge_flags) {
    float thickness = 2.0f;
    float gap = thickness * 2.0f;  // Gap between lines equals thickness

    // Outer rectangle
    V2 tl_outer = world_to_screen((V2){x, y}, camera);
    V2 tr_outer = world_to_screen((V2){x + w, y}, camera);
    V2 br_outer = world_to_screen((V2){x + w, y + h}, camera);
    V2 bl_outer = world_to_screen((V2){x, y + h}, camera);

    if (edge_flags & EDGE_TOP)
        geometry_buffer_add_line(gb, tl_outer, tr_outer, thickness, color);
    if (edge_flags & EDGE_RIGHT)
        geometry_buffer_add_line(gb, tr_outer, br_outer, thickness, color);
    if (edge_flags & EDGE_BOTTOM)
        geometry_buffer_add_line(gb, br_outer, bl_outer, thickness, color);
    if (edge_flags & EDGE_LEFT)
        geometry_buffer_add_line(gb, bl_outer, tl_outer, thickness, color);

    // Inner rectangle (inset by gap)
    float inset = gap / camera->scale;  // Convert gap to world units
    V2 tl_inner = world_to_screen((V2){x + inset, y + inset}, camera);
    V2 tr_inner = world_to_screen((V2){x + w - inset, y + inset}, camera);
    V2 br_inner = world_to_screen((V2){x + w - inset, y + h - inset}, camera);
    V2 bl_inner = world_to_screen((V2){x + inset, y + h - inset}, camera);

    if (edge_flags & EDGE_TOP)
        geometry_buffer_add_line(gb, tl_inner, tr_inner, thickness, color);
    if (edge_flags & EDGE_RIGHT)
        geometry_buffer_add_line(gb, tr_inner, br_inner, thickness, color);
    if (edge_flags & EDGE_BOTTOM)
        geometry_buffer_add_line(gb, br_inner, bl_inner, thickness, color);
    if (edge_flags & EDGE_LEFT)
        geometry_buffer_add_line(gb, bl_inner, tl_inner, thickness, color);
}

// Draw rounded rectangle with geometry buffer
void render_rounded_rect_geometry(GeometryBuffer* gb, float x, float y, float w, float h, float radius, SDL_Color color, Camera* camera, int edge_flags) {
    if (radius <= 0) {
        // Draw regular rectangle with thick lines
        V2 tl = world_to_screen((V2){x, y}, camera);
        V2 tr = world_to_screen((V2){x + w, y}, camera);
        V2 br = world_to_screen((V2){x + w, y + h}, camera);
        V2 bl = world_to_screen((V2){x, y + h}, camera);

        if (edge_flags & EDGE_TOP)
            geometry_buffer_add_line(gb, tl, tr, 2.0f, color);
        if (edge_flags & EDGE_RIGHT)
            geometry_buffer_add_line(gb, tr, br, 2.0f, color);
        if (edge_flags & EDGE_BOTTOM)
            geometry_buffer_add_line(gb, br, bl, 2.0f, color);
        if (edge_flags & EDGE_LEFT)
            geometry_buffer_add_line(gb, bl, tl, 2.0f, color);
    } else {
        // Draw rounded rectangle with arcs at corners
        V2 tl_inner = world_to_screen((V2){x + radius, y + radius}, camera);
        V2 tr_inner = world_to_screen((V2){x + w - radius, y + radius}, camera);
        V2 br_inner = world_to_screen((V2){x + w - radius, y + h - radius}, camera);
        V2 bl_inner = world_to_screen((V2){x + radius, y + h - radius}, camera);

        // Top line
        if (edge_flags & EDGE_TOP)
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x + radius, y}, camera),
                world_to_screen((V2){x + w - radius, y}, camera),
                2.0f, color);

        // Right line
        if (edge_flags & EDGE_RIGHT)
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x + w, y + radius}, camera),
                world_to_screen((V2){x + w, y + h - radius}, camera),
                2.0f, color);

        // Bottom line
        if (edge_flags & EDGE_BOTTOM)
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x + w - radius, y + h}, camera),
                world_to_screen((V2){x + radius, y + h}, camera),
                2.0f, color);

        // Left line
        if (edge_flags & EDGE_LEFT)
            geometry_buffer_add_line(gb,
                world_to_screen((V2){x, y + h - radius}, camera),
                world_to_screen((V2){x, y + radius}, camera),
                2.0f, color);

        // Calculate screen radius
        float screen_radius = radius * camera->scale;

        // Corner arcs (only draw if adjacent edges are visible)
        if ((edge_flags & EDGE_TOP) && (edge_flags & EDGE_LEFT))
            geometry_buffer_add_arc(gb, tl_inner, screen_radius, M_PI, M_PI * 1.5f, 2.0f, color, 8);
        if ((edge_flags & EDGE_TOP) && (edge_flags & EDGE_RIGHT))
            geometry_buffer_add_arc(gb, tr_inner, screen_radius, M_PI * 1.5f, M_PI * 2.0f, 2.0f, color, 8);
        if ((edge_flags & EDGE_BOTTOM) && (edge_flags & EDGE_RIGHT))
            geometry_buffer_add_arc(gb, br_inner, screen_radius, 0, M_PI * 0.5f, 2.0f, color, 8);
        if ((edge_flags & EDGE_BOTTOM) && (edge_flags & EDGE_LEFT))
            geometry_buffer_add_arc(gb, bl_inner, screen_radius, M_PI * 0.5f, M_PI, 2.0f, color, 8);
    }
}

// Label functions

void label_copy(char dest[32], const char* src) {
    strncpy(dest, src, 31);
    dest[31] = '\0';
}

void collect_label(Atelier* atelier, Band* band, Interval transformed, int edge_flags) {
    if (band->label[0] == '\0') return;

    LabelAnchor anchor = rotate_anchor_for_corner(band->label_anchor, atelier->selected_corner);

    switch (anchor) {
        case LABEL_TOP_LEFT:
            if (!(edge_flags & EDGE_TOP) && !(edge_flags & EDGE_LEFT)) return;
            break;
        case LABEL_TOP_RIGHT:
            if (!(edge_flags & EDGE_TOP) && !(edge_flags & EDGE_RIGHT)) return;
            break;
        case LABEL_BOTTOM_LEFT:
            if (!(edge_flags & EDGE_BOTTOM) && !(edge_flags & EDGE_LEFT)) return;
            break;
        case LABEL_BOTTOM_RIGHT:
            if (!(edge_flags & EDGE_BOTTOM) && !(edge_flags & EDGE_RIGHT)) return;
            break;
        case LABEL_TOP_CENTER:
            if (!(edge_flags & EDGE_TOP)) return;
            break;
        case LABEL_BOTTOM_CENTER:
            if (!(edge_flags & EDGE_BOTTOM)) return;
            break;
        case LABEL_MIDDLE_LEFT:
            if (!(edge_flags & EDGE_LEFT)) return;
            break;
        case LABEL_MIDDLE_RIGHT:
            if (!(edge_flags & EDGE_RIGHT)) return;
            break;
        case LABEL_CENTER:
            break;
    }

    V2 world_pos = anchor_to_position(anchor, transformed, atelier->diagonal);
    world_pos = v2_add(world_pos, band->label_offset);
    V2 label_pos = world_to_screen(world_pos, &atelier->camera);

    LabelDraw label = {
        band->label,
        label_pos,
        make_color_oklch(band->color.lightness, band->color.chroma, band->color.hue),
        rotate_anchor_for_corner(anchor, CORNER_TL)
    };
    atelier->render_ctx.labels.ptr[atelier->render_ctx.labels.length++] = label;
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

void band_array_copy_after(BandArray* arr, size_t index) {
    if (index >= arr->length) return;

    Band original = arr->ptr[index];
    Band copy = original;

    // Copy label text
    label_copy(copy.label, original.label);

    // Calculate size once and preserve it
    float size = original.interval.end - original.interval.start;
    copy.interval.start = original.interval.end;  // Start where the original ends
    copy.interval.end = copy.interval.start + size;  // Preserve the size
    copy.follow_previous = true;  // This band should follow the previous one

    // Insert right after the original
    band_array_insert(arr, index + 1, copy);
}

void band_array_split(BandArray* arr, size_t index) {
    if (index >= arr->length) return;

    Band* original = &arr->ptr[index];
    float midpoint = (original->interval.start + original->interval.end) / 2.0f;

    // Create second half band
    Band second_half = *original;
    label_copy(second_half.label, original->label);
    second_half.interval.start = midpoint;
    second_half.follow_previous = true;  // Second half follows the first half

    // Modify original to be first half
    original->interval.end = midpoint;

    // Insert second half right after the original
    band_array_insert(arr, index + 1, second_half);
}

// Flatten bands into individual bands (one per interval)
void apply_band_rules(BandArray* bands) {
    for (size_t i = 0; i < bands->length; i++) {
        Band* band = &bands->ptr[i];

        // If this band should follow the previous one, update its start position only
        if (band->follow_previous && i > 0) {
            Band* prev_band = &bands->ptr[i - 1];
            band->interval.start = prev_band->interval.end;
            // Keep the end position as-is, don't move it
        }

        // Automatically set band to OPEN if start >= end
        if (band->interval.start >= band->interval.end) {
            band->kind = BAND_OPEN;
        }
    }
}

// Create a new band with random color
void add_random_band(Atelier* atelier) {
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
        .label = {0},  // Empty label
        .wavelength_scale = 1,
        .wave_inverted = false,
        .label_anchor = LABEL_BOTTOM_RIGHT,  // Default label anchor
        .label_offset = {0, 0},  // No offset
        .wave_half_period = false,
        .follow_previous = false
    };
    snprintf(new_band.label, 32, "%c", 'A' + (char)(atelier->work->bands.length % 26));

    band_array_add(&atelier->work->bands, new_band);
}

void add_open_band(Atelier* atelier) {
    // Calculate center of sliver camera view
    float center = atelier->sliver_camera.offset + 0.5f / atelier->sliver_camera.scale;

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
        .label = {0},  // Empty label
        .wavelength_scale = 1,
        .wave_inverted = false,
        .wave_half_period = false,
        .follow_previous = false,
        .label_anchor = LABEL_BOTTOM_RIGHT,  // Default label anchor
        .label_offset = {0, 0}  // No offset
    };
    snprintf(new_band.label, 32, "%c", 'A' + (char)(atelier->work->bands.length % 26));

    band_array_add(&atelier->work->bands, new_band);
}

void init_bands_rand(Atelier* atelier) {
    // Clear existing bands
    band_array_clear(&atelier->work->bands);

    // Add a single random band
    add_random_band(atelier);
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

// Work functions
void work_init(Work* work, size_t band_capacity) {
    // Calculate arena size
    size_t bands_size = band_capacity * sizeof(Band);
    work->arena_size = bands_size;

    // Allocate arena
    work->arena = (char*)calloc(work->arena_size, 1);

    // Set up bands array
    work->bands.ptr = (Band*)work->arena;
    work->bands.length = 0;
    work->bands.capacity = band_capacity;

    // Initialize filename, hash, and list pointers
    work->filename[0] = '\0';
    work->saved_hash = 0;
    work->prev = NULL;
    work->next = NULL;
}

Work* work_new(size_t band_capacity) {
    Work* work = (Work*)malloc(sizeof(Work));
    work_init(work, band_capacity);
    return work;
}

Work* work_copy(Work* source) {
    Work* work = work_new(source->bands.capacity);

    for (size_t i = 0; i < source->bands.length; i++) {
        Band* src_band = &source->bands.ptr[i];
        Band new_band = *src_band;

        label_copy(new_band.label, src_band->label);

        band_array_add(&work->bands, new_band);
    }

    strncpy(work->filename, source->filename, 255);
    work->filename[255] = '\0';
    work->saved_hash = source->saved_hash;

    return work;
}

void work_close(Work* work) {
    if (!work) return;

    if (work->prev) work->prev->next = work->next;
    if (work->next) work->next->prev = work->prev;

    free(work->arena);
    free(work);
}

uint32_t work_compute_hash(Work* work) {
    uint32_t hash = 2166136261u;
    const unsigned char* data = (const unsigned char*)work->arena;
    for (size_t i = 0; i < work->arena_size; i++) {
        hash ^= data[i];
        hash *= 16777619u;
    }
    return hash;
}

bool work_is_modified(Work* work) {
    return work_compute_hash(work) != work->saved_hash;
}

void work_update_hash(Work* work) {
    work->saved_hash = work_compute_hash(work);
}

void work_save(Work* work, const char* filename, StringBuilder* sb) {
    string_builder_clear(sb);

    strncpy(work->filename, filename, 255);
    work->filename[255] = '\0';

    string_append(sb, "WORK SLIVER %04d\n", WORK_FILE_VERSION);

    for (size_t i = 0; i < work->bands.length; i++) {
        Band* band = &work->bands.ptr[i];
        string_append(sb, "{\n");
        string_append(sb, "  type: band\n");
        string_append(sb, "  interval: %f %f\n", band->interval.start, band->interval.end);
        string_append(sb, "  stride: %f\n", band->stride);
        string_append(sb, "  repeat: %d\n", band->repeat);
        string_append(sb, "  kind: %s\n", band_kind_to_string(band->kind));
        string_append(sb, "  line_kind: %s\n", line_kind_to_string(band->line_kind));
        string_append(sb, "  color: %f %f %f\n", band->color.hue, band->color.lightness, band->color.chroma);
        string_append(sb, "  label: \"%s\"\n", band->label);
        string_append(sb, "  wavelength_scale: %d\n", band->wavelength_scale);
        string_append(sb, "  wave_inverted: %s\n", band->wave_inverted ? "true" : "false");
        string_append(sb, "  wave_half_period: %s\n", band->wave_half_period ? "true" : "false");
        string_append(sb, "  follow_previous: %s\n", band->follow_previous ? "true" : "false");
        string_append(sb, "  label_anchor: %d\n", band->label_anchor);
        string_append(sb, "  label_offset: %f %f\n", band->label_offset.x, band->label_offset.y);
        string_append(sb, "}\n");
    }

    FILE* file = fopen(filename, "w");
    if (!file) {
        printf("Failed to open file for writing: %s\n", filename);
        return;
    }

    fwrite(sb->ptr, 1, sb->len, file);
    fclose(file);

    work_update_hash(work);
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

    int version;
    if (sscanf(line, "WORK SLIVER %d", &version) != 1) {
        printf("Invalid file format: missing version\n");
        fclose(file);
        return false;
    }

    if (version > WORK_FILE_VERSION) {
        printf("Unsupported file version: %d (expected <= %d)\n", version, WORK_FILE_VERSION);
        fclose(file);
        return false;
    }

    strncpy(work->filename, filename, 255);
    work->filename[255] = '\0';

    work->bands.length = 0;

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
                label_copy(temp_band.label, temp_label);
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
    work_update_hash(work);
    printf("Work loaded from %s (%zu bands)\n", filename, work->bands.length);
    return true;
}

// Draw Work UI layer - manages Work objects (load, store, init)
Layout render_work_ui(Atelier* atelier, Layout layout) {
    V2 button_size = {80, 25};

    // Work navigation and management buttons
    V2 nav_button_size = {30, 25};
    if (render_button(atelier, "<", layout.next, nav_button_size, DISABLED(!atelier->work->prev))) {
        if (atelier->work->prev) {
            atelier->work = atelier->work->prev;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 5);

    if (render_button(atelier, ">", layout.next, nav_button_size, DISABLED(!atelier->work->next))) {
        if (atelier->work->next) {
            atelier->work = atelier->work->next;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 10);

    // Work preset buttons
    if (render_button(atelier, "New", layout.next, button_size, INPUT_NONE)) {
        Work* new_work = work_new(128);
        new_work->prev = atelier->work;
        new_work->next = atelier->work->next;
        if (atelier->work->next) atelier->work->next->prev = new_work;
        atelier->work->next = new_work;
        atelier->work = new_work;
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(atelier, "Load", layout.next, button_size, INPUT_NONE)) {
        atelier->ui_state.shelf_mode_save = false;
        atelier->ui_state.step = UI_SHELF;
    }

    advance_vertical(&layout, 30);

    if (render_button(atelier, "Store", layout.next, button_size, INPUT_NONE)) {
        if (atelier->work->filename[0] != '\0') {
            strncpy(atelier->ui_state.suggested_filename, atelier->work->filename, 255);
            atelier->ui_state.suggested_filename[255] = '\0';
        } else {
            time_t now = time(NULL);
            struct tm *tm_info = localtime(&now);
            strftime(atelier->ui_state.suggested_filename, sizeof(atelier->ui_state.suggested_filename), "work_%Y-%m-%d_%H-%M-%S.wo", tm_info);
        }
        atelier->ui_state.shelf_mode_save = true;
        atelier->ui_state.step = UI_SHELF;
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(atelier, "Copy", layout.next, button_size, INPUT_NONE)) {
        Work* copied = work_copy(atelier->work);
        copied->filename[0] = '\0';
        copied->prev = atelier->work;
        copied->next = atelier->work->next;
        if (atelier->work->next) atelier->work->next->prev = copied;
        atelier->work->next = copied;
        atelier->work = copied;
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(atelier, "Close", layout.next, button_size, INPUT_NONE)) {
        if (atelier->work->prev || atelier->work->next) {
            Work* to_close = atelier->work;
            if (atelier->work->next) {
                atelier->work = atelier->work->next;
            } else {
                atelier->work = atelier->work->prev;
            }
            work_close(to_close);
        }
    }
    advance_horizontal(&layout, button_size.x + 10);

    // Show modified indicator and filename
    SDL_Color white = {255, 255, 255, 255};
    SDL_Color modified_color = {255, 200, 100, 255};
    if (work_is_modified(atelier->work)) {
        render_text(atelier, "*", layout.next, modified_color);
    }
    advance_horizontal(&layout, 20);

    const char* display_name = atelier->work->filename[0] != '\0' ? atelier->work->filename : "(unsaved)";
    render_text(atelier, display_name, layout.next, work_is_modified(atelier->work) ? modified_color : white);

    advance_vertical(&layout, 30);

    return layout;
}

// Draw Shelf layer - file browser for load/save
Layout render_shelf(Atelier* atelier, Layout layout) {
    V2 button_size = {300, 30};
    SDL_Color white = {255, 255, 255, 255};

    if (atelier->ui_state.shelf_mode_save) {
        render_text(atelier, "Save work file:", layout.next, white);
        advance_vertical(&layout, 35);

        V2 input_pos = layout.next;
        V2 input_size = {400, 25};
        render_text_input_field(atelier, atelier->ui_state.suggested_filename, 256, input_pos, input_size);
        advance_vertical(&layout, 35);

        V2 save_button_size = {80, 25};
        if (render_button(atelier, "Save", layout.next, save_button_size, INPUT_NONE)) {
            work_save(atelier->work, atelier->ui_state.suggested_filename, &atelier->string_builder);
            atelier->ui_state.step = UI_LENS;
        }
        advance_horizontal(&layout, save_button_size.x + 10);

        if (render_button(atelier, "Cancel", layout.next, save_button_size, INPUT_NONE)) {
            atelier->ui_state.step = UI_LENS;
        }
    } else {
        render_text(atelier, "Select a work file to load:", layout.next, white);
        advance_vertical(&layout, 35);

        FileList file_list;
        get_wo_files(&file_list);

        if (file_list.count == 0) {
            render_text(atelier, "No .wo files found", layout.next, white);
        } else {
            for (size_t i = 0; i < file_list.count; i++) {
                if (render_button(atelier, file_list.files[i], layout.next, button_size, INPUT_NONE)) {
                    Work* loaded = work_new(128);
                    if (work_load(loaded, file_list.files[i])) {
                        loaded->prev = atelier->work;
                        loaded->next = atelier->work->next;
                        if (atelier->work->next) atelier->work->next->prev = loaded;
                        atelier->work->next = loaded;
                        atelier->work = loaded;
                    } else {
                        work_close(loaded);
                    }
                    atelier->ui_state.step = UI_LENS;
                    break;
                }
                advance_vertical(&layout, button_size.y + 5);
            }
        }

        advance_vertical(&layout, 15);

        V2 cancel_button_size = {80, 25};
        if (render_button(atelier, "Cancel", layout.next, cancel_button_size, INPUT_NONE)) {
            atelier->ui_state.step = UI_LENS;
        }
    }

    return layout;
}

Layout render_lens(Atelier* atelier, Layout layout) {
    V2 button_size = {80, 25};

    // Add band buttons
    if (render_button(atelier, "+ Band", layout.next, button_size, INPUT_NONE)) {
        add_random_band(atelier);
    }
    advance_horizontal(&layout, button_size.x + 10);

    if (render_button(atelier, "+ Open", layout.next, button_size, INPUT_NONE)) {
        add_open_band(atelier);
    }

    // Move to next row
    advance_vertical(&layout, 30);
    layout.row_start = layout.next;

    // Band list navigation buttons
    V2 nav_button_size = {30, 22};
    if (render_button(atelier, "Up", layout.next, nav_button_size, INPUT_NONE)) {
        if (atelier->band_offset > 0) {
            atelier->band_offset--;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 5);

    if (render_button(atelier, "Dn", layout.next, nav_button_size, INPUT_NONE)) {
        if (atelier->band_offset < (int)atelier->work->bands.length - 1) {
            atelier->band_offset++;
        }
    }
    advance_horizontal(&layout, nav_button_size.x + 5);

    if (render_button(atelier, atelier->one_side ? "1" : "2", layout.next, nav_button_size, INPUT_NONE)) {
        atelier->one_side = !atelier->one_side;
    }
    advance_vertical(&layout, 25);

    // Render each band
    char buffer[256];
    for (size_t i = atelier->band_offset; i < atelier->work->bands.length; i++) {
        Band* band = &atelier->work->bands.ptr[i];

        // Band header with band number (show actual index)
        snprintf(buffer, sizeof(buffer), "    %zu:", i + 1);
        SDL_Color band_color = make_color_oklch(band->color.lightness, band->color.chroma, band->color.hue);
        render_text(atelier, buffer, layout.next, band_color);

        // Label input field next to band number
        V2 label_pos = {layout.next.x + 100, layout.next.y};
        V2 label_size = {120, 20};
        render_text_input_field(atelier, band->label, 32, label_pos, label_size);
        advance_vertical(&layout, 25);

        // Add split button (S) near the right side
        V2 split_pos = {WINDOW_WIDTH - 50, layout.next.y + 50};
        V2 split_size = {25, 22};
        if (render_button(atelier, "S", split_pos, split_size, INPUT_NONE)) {
            band_array_split(&atelier->work->bands, i);
            break;  // Exit loop since array has changed
        }

        // Add copy button (C) near the right side
        V2 copy_pos = {WINDOW_WIDTH - 50, layout.next.y + 25};
        V2 copy_size = {25, 22};
        if (render_button(atelier, "C", copy_pos, copy_size, INPUT_NONE)) {
            band_array_copy_after(&atelier->work->bands, i);
            break;  // Exit loop since array has changed
        }

        // Add remove button (X) at the right side
        V2 remove_pos = {WINDOW_WIDTH - 50, layout.next.y};
        V2 remove_size = {25, 22};
        SDL_Color red_tint = {200, 100, 100, 255};
        if (render_button(atelier, "X", remove_pos, remove_size, INPUT_NONE)) {
            band_array_remove(&atelier->work->bands, i);
            break;  // Exit loop since array has changed
        }

        // Band details
        render_text(atelier, "Start:", layout.next, band_color);
        V2 input_pos = {layout.next.x + 100, layout.next.y};
        V2 input_size = {80, 20};

        // Add follow_previous toggle button next to start field
        V2 follow_button_pos = {input_pos.x + input_size.x + 5, input_pos.y};
        V2 follow_button_size = {25, 20};
        const char* follow_text = band->follow_previous ? "^" : " ";
        if (render_button(atelier, follow_text, follow_button_pos, follow_button_size, HIGHLIGHTED(band->follow_previous))) {
            band->follow_previous = !band->follow_previous;
        }

        // Render start field (disabled if follow_previous is true)
        // Scale drag sensitivity based on sliver camera zoom - more zoom means finer control
        float drag_scale = 0.005f / atelier->sliver_camera.scale;  // Inversely proportional to zoom
        float old_start = band->interval.start;
        render_numeric_input_field_full(atelier, &band->interval.start, input_pos, input_size, band->follow_previous, drag_scale);
        // By default, move end to keep size fixed (unless Cmd is held for independent movement)
        if (!(SDL_GetModState() & KMOD_GUI) && band->interval.start != old_start) {
            band->interval.end += (band->interval.start - old_start);
        }
        advance_vertical(&layout, 20);

        render_text(atelier, "  End:", layout.next, band_color);
        input_pos = (V2){layout.next.x + 100, layout.next.y};
        render_numeric_input_field_full(atelier, &band->interval.end, input_pos, input_size, false, drag_scale);
        advance_vertical(&layout, 20);

        // Hue input field
        render_text(atelier, "Color:", layout.next, band_color);
        input_pos = (V2){layout.next.x + 100, layout.next.y};

        // Store old hue to detect changes
        float old_hue = band->color.hue;
        render_numeric_input_field_full(atelier, &band->color.hue, input_pos, input_size, false, 1.0f);  // Scale of 1.0 for 0-360 range

        // Clamp hue to valid range if it changed
        if (band->color.hue != old_hue) {
            while (band->color.hue < 0) band->color.hue += 360.0f;
            while (band->color.hue >= 360.0f) band->color.hue -= 360.0f;
        }

        input_pos = (V2){layout.next.x + 100 + input_size.x, layout.next.y};
        // Store old lightness to detect changes
        float old_lightness = band->color.lightness;
        render_numeric_input_field_full(atelier, &band->color.lightness, input_pos, input_size, false, 0.003f);  // Scale of 1.0 for 0-360 range

        // Clamp lightness to valid range if it changed
        if (band->color.lightness != old_lightness) {
            while (band->color.lightness < 0) band->color.lightness = 0.0f;
            while (band->color.lightness > 1.0f) band->color.lightness = 1.0f;
        }
        input_pos = (V2){layout.next.x + 100 + input_size.x*2, layout.next.y};
        // Store old chroma to detect changes
        float old_chroma = band->color.chroma;
        render_numeric_input_field_full(atelier, &band->color.chroma, input_pos, input_size, false, 0.003f);  // Scale of 1.0 for 0-360 range

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
        if (render_button(atelier, band_kind_name, band_kind_pos, band_kind_size, INPUT_NONE)) {
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
        if (render_button(atelier, kind_name, button_pos, button_size, INPUT_NONE)) {
            // Cycle to next line_kind
            band->line_kind = (band->line_kind + 1) % KIND_COUNT;
        }

        // If WAVE line_kind, show wavelength controls
        if (band->line_kind == KIND_WAVE) {
            // Decrement button (decrease exponent)
            V2 dec_pos = {button_pos.x + button_size.x + 10, layout.next.y};
            V2 small_button_size = {20, 22};
            if (render_button(atelier, "-", dec_pos, small_button_size, INPUT_NONE)) {
                if (band->wavelength_scale > 0) {
                    band->wavelength_scale--;
                }
            }

            // Show current value (as 2^n)
            char scale_text[32];
            snprintf(scale_text, sizeof(scale_text), "%d", 1 << band->wavelength_scale);
            V2 text_pos = {dec_pos.x + small_button_size.x + 5, layout.next.y};
            SDL_Color white = {255, 255, 255, 255};
            render_text(atelier, scale_text, text_pos, white);

            // Increment button (increase exponent)
            V2 inc_pos = {text_pos.x + 40, layout.next.y};
            if (render_button(atelier, "+", inc_pos, small_button_size, INPUT_NONE)) {
                if (band->wavelength_scale < 8) {
                    band->wavelength_scale++;
                }
            }

            // Phase toggle button (false = 180°, true = 0°)
            V2 phase_pos = {inc_pos.x + small_button_size.x + 10, layout.next.y};
            V2 phase_button_size = {80, 22};
            const char* phase_text = band->wave_inverted ? "sin 0" : "sin pi";
            if (render_button(atelier, phase_text, phase_pos, phase_button_size, HIGHLIGHTED(band->wave_inverted))) {
                band->wave_inverted = !band->wave_inverted;
            }

            // Half period toggle button (false = half period, true = full period)
            V2 half_pos = {phase_pos.x + phase_button_size.x + 5, layout.next.y};
            V2 half_button_size = {50, 22};
            const char* half_text = band->wave_half_period ? "Full" : "Half";
            if (render_button(atelier, half_text, half_pos, half_button_size, HIGHLIGHTED(band->wave_half_period))) {
                band->wave_half_period = !band->wave_half_period;
            }
        }

        advance_vertical(&layout, 25);

        // Label position control - 3x3 anchor grid with offset inputs
        int selected_pos = render_anchor_buttons(atelier, layout.next, 45, band->label_anchor);
        if (selected_pos >= 0) {
            band->label_anchor = selected_pos;
        }

        // Label offset inputs next to anchor grid
        V2 offset_input_size = {60, 20};
        V2 x_input_pos = {layout.next.x + 50, layout.next.y + 5};
        V2 y_input_pos = {layout.next.x + 50, layout.next.y + 25};

        render_numeric_input_field_full(atelier, &band->label_offset.x, x_input_pos, offset_input_size, false, 0.5f);
        render_numeric_input_field_full(atelier, &band->label_offset.y, y_input_pos, offset_input_size, false, 0.5f);

        advance_vertical(&layout, 50);

        advance_vertical(&layout, 10);  // Space between bands
    }

    // Apply band rules after all UI changes
    apply_band_rules(&atelier->work->bands);

    return layout;
}

void render_ui_panel(Atelier* atelier) {
    // Draw UI panel background
    SDL_SetRenderDrawColor(atelier->renderer, 40, 40, 45, 255);
    SDL_Rect panel_rect = {VIEWPORT_WIDTH, 0, UI_PANEL_WIDTH, WINDOW_HEIGHT};
    SDL_RenderFillRect(atelier->renderer, &panel_rect);

    // Draw panel border
    SDL_SetRenderDrawColor(atelier->renderer, 60, 60, 65, 255);
    SDL_RenderDrawLine(atelier->renderer, VIEWPORT_WIDTH, 0, VIEWPORT_WIDTH, WINDOW_HEIGHT);

    Layout layout = {
        .next = {VIEWPORT_WIDTH + 20, 20},
        .row_start = {VIEWPORT_WIDTH + 20, 20},
        .max = {WINDOW_WIDTH - 20, WINDOW_HEIGHT - 40}
    };

    // Layer 1: Work management
    layout = render_work_ui(atelier, layout);
    layout.row_start = layout.next;

    // Visual separator
    SDL_SetRenderDrawColor(atelier->renderer, 80, 80, 85, 255);
    SDL_RenderDrawLine(atelier->renderer, VIEWPORT_WIDTH + 10, layout.next.y, WINDOW_WIDTH - 10, layout.next.y);
    advance_vertical(&layout, 15);

    // Layer 2: Switch between SHELF (file list) and LENS (band editing)
    if (atelier->ui_state.step == UI_SHELF) {
        layout = render_shelf(atelier, layout);
    } else {
        layout = render_lens(atelier, layout);
    }
}

void render_band_geometry(GeometryBuffer* gb, Band* band, Interval transformed, Diagonal diagonal, Camera* camera, int edge_flags) {
    // Clamp to visible range [0, 1]
    transformed.start = fmaxf(0.0f, fminf(1.0f, transformed.start));
    transformed.end = fmaxf(0.0f, fminf(1.0f, transformed.end));

    // Get the two diagonal endpoints for this band
    V2 p1 = v2_lerp(diagonal.start, diagonal.end, transformed.start);
    V2 p2 = v2_lerp(diagonal.start, diagonal.end, transformed.end);

    // Calculate bounding box
    float min_x = fminf(p1.x, p2.x);
    float max_x = fmaxf(p1.x, p2.x);
    float min_y = fminf(p1.y, p2.y);
    float max_y = fmaxf(p1.y, p2.y);

    // Convert LCH to SDL_Color for rendering
    SDL_Color sdl_color = make_color_oklch(band->color.lightness, band->color.chroma, band->color.hue);

    switch (band->line_kind) {
        case KIND_ROUNDED: {
            float base_radius = fminf(25.0f, fminf(max_x - min_x, max_y - min_y) * 0.2f);
            float extend = base_radius * (1.0f - 1.0f/sqrtf(2));
            render_rounded_rect_geometry(gb,
                                      min_x - extend, min_y - extend,
                                      (max_x - min_x) + 2 * extend,
                                      (max_y - min_y) + 2 * extend,
                                      base_radius, sdl_color, camera, edge_flags);
            break;
        }
        case KIND_DOUBLE:
            render_double_rect_geometry(gb,
                                    min_x, min_y, max_x - min_x, max_y - min_y,
                                    sdl_color, camera, edge_flags);
            break;
        case KIND_WAVE:
            render_wave_rect_geometry(gb,
                                  min_x, min_y, max_x - min_x, max_y - min_y,
                                  sdl_color, camera, band->wavelength_scale,
                                  band->wave_inverted, band->wave_half_period, edge_flags);
            break;
        case KIND_SHARP:
        default:
            render_rounded_rect_geometry(gb,
                                      min_x, min_y, max_x - min_x, max_y - min_y,
                                      0, sdl_color, camera, edge_flags);
            break;
    }
}

void render_work(Atelier* atelier) {
    // Clear label array
    atelier->render_ctx.labels.length = 0;

    // Draw bands along diagonal
    for (size_t i = 0; i < atelier->work->bands.length; i++) {
        Band* band = &atelier->work->bands.ptr[i];

        if (band->kind == BAND_OPEN) {
            Interval transformed = sliver_transform_interval(band->interval, &atelier->sliver_camera);
            {
                Interval extended = {-2.0f, transformed.end};
                int edge_flags = calculate_edge_flags(atelier, extended.start, extended.end);
                if (edge_flags != 0) {
                    render_band_geometry(&atelier->render_ctx.geometry, band, extended, atelier->diagonal, &atelier->camera, edge_flags);
                    collect_label(atelier, band, extended, edge_flags);
                }
            }
            {
                Interval extended = {transformed.start, 2.0f};
                int edge_flags = calculate_edge_flags(atelier, extended.start, extended.end);
                if (edge_flags != 0) {
                    render_band_geometry(&atelier->render_ctx.geometry, band, extended, atelier->diagonal, &atelier->camera, edge_flags);
                    collect_label(atelier, band, extended, edge_flags);
                }
            }
        } else {
            // Normal closed band behavior with repeat support
            int count = band->repeat + 1;  // repeat=0 means 1 interval
            float size = band->interval.end - band->interval.start;

            for (int j = 0; j < count; j++) {
                Interval interval = {
                    band->interval.start + j * band->stride,
                    band->interval.start + j * band->stride + size
                };
                Interval transformed = sliver_transform_interval(interval, &atelier->sliver_camera);

                // Calculate edge visibility flags and skip if no edges visible
                int edge_flags = calculate_edge_flags(atelier, transformed.start, transformed.end);
                if (edge_flags == 0) {
                    continue;
                }

                // Draw band geometry
                render_band_geometry(&atelier->render_ctx.geometry, band, transformed, atelier->diagonal, &atelier->camera, edge_flags);
                collect_label(atelier, band, transformed, edge_flags);
            }
        }
    }
}

void render_viewport(Atelier* atelier) {
    // Clear geometry buffer for new frame
    geometry_buffer_clear(&atelier->render_ctx.geometry);

    // Set viewport clipping for left side
    SDL_Rect viewport = {0, 0, VIEWPORT_WIDTH, WINDOW_HEIGHT};
    SDL_RenderSetClipRect(atelier->renderer, &viewport);

    // Draw bounding square outline using geometry buffer
    SDL_Color gray = {100, 100, 100, 255};
    V2 tl = get_corner_position(atelier, CORNER_TL);
    V2 tr = get_corner_position(atelier, CORNER_TR);
    V2 br = get_corner_position(atelier, CORNER_BR);
    V2 bl = get_corner_position(atelier, CORNER_BL);

    V2 tl_s = world_to_screen(tl, &atelier->camera);
    V2 tr_s = world_to_screen(tr, &atelier->camera);
    V2 br_s = world_to_screen(br, &atelier->camera);
    V2 bl_s = world_to_screen(bl, &atelier->camera);

    geometry_buffer_add_line(&atelier->render_ctx.geometry, tl_s, tr_s, 2.0f, gray);
    geometry_buffer_add_line(&atelier->render_ctx.geometry, tr_s, br_s, 2.0f, gray);
    geometry_buffer_add_line(&atelier->render_ctx.geometry, br_s, bl_s, 2.0f, gray);
    geometry_buffer_add_line(&atelier->render_ctx.geometry, bl_s, tl_s, 2.0f, gray);

    // Draw corner indicators using geometry buffer
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(atelier, i);
        V2 corner_s = world_to_screen(corner, &atelier->camera);
        if (i == atelier->selected_corner) {
            SDL_Color highlight = {255, 200, 100, 255};
            geometry_buffer_add_arc(&atelier->render_ctx.geometry, corner_s,
                                   HIGHLIGHT_SIZE * atelier->camera.scale,
                                   0, M_PI * 2.0f, 2.0f, highlight, 16);
        } else {
            SDL_Color gray = {150, 150, 150, 255};
            geometry_buffer_add_arc(&atelier->render_ctx.geometry, corner_s,
                                   (HIGHLIGHT_SIZE / 2) * atelier->camera.scale,
                                   0, M_PI * 2.0f, 2.0f, gray, 12);
        }
    }

    if (atelier->selected_corner != CORNER_NONE) {
        // Draw diagonal using geometry buffer
        SDL_Color diagonal_color = {200, 200, 255, 255};
        V2 diag_start_s = world_to_screen(atelier->diagonal.start, &atelier->camera);
        V2 diag_end_s = world_to_screen(atelier->diagonal.end, &atelier->camera);
        geometry_buffer_add_line(&atelier->render_ctx.geometry, diag_start_s, diag_end_s, 2.0f, diagonal_color);

        render_work(atelier);

        // Draw orientation edge from selected corner using geometry buffer
        V2 selected = get_corner_position(atelier, atelier->selected_corner);
        V2 selected_s = world_to_screen(selected, &atelier->camera);
        V2 orient_end_s = world_to_screen(atelier->diagonal.end, &atelier->camera);
        SDL_Color orient_color = {255, 100, 100, 255};
        geometry_buffer_add_line(&atelier->render_ctx.geometry, selected_s, orient_end_s, 2.0f, orient_color);
    }

    // Render all geometry in one batch
    if (atelier->render_ctx.geometry.index_count > 0) {
        SDL_RenderGeometry(atelier->renderer, atelier->render_ctx.white_texture,
                          atelier->render_ctx.geometry.vertices, atelier->render_ctx.geometry.vertex_count,
                          atelier->render_ctx.geometry.indices, atelier->render_ctx.geometry.index_count);
    }

    // Draw collected labels
    for (size_t i = 0; i < atelier->render_ctx.labels.length; i++) {
        LabelDraw* label = &atelier->render_ctx.labels.ptr[i];
        render_text_aligned(atelier, label->label, label->position, label->color, label->anchor);
    }

    // Clear clipping rect to draw UI
    SDL_RenderSetClipRect(atelier->renderer, NULL);
}

void render(Atelier* atelier) {
    // Clear screen
    SDL_SetRenderDrawColor(atelier->renderer, 30, 30, 30, 255);
    SDL_RenderClear(atelier->renderer);

    render_viewport(atelier);

    // Render UI panel on the right
    render_ui_panel(atelier);

    SDL_RenderPresent(atelier->renderer);
}

Corner detect_corner_click(Atelier* atelier, V2 mouse) {
    V2 world_mouse = screen_to_world(mouse, &atelier->camera);
    for (int i = 0; i < 4; i++) {
        V2 corner = get_corner_position(atelier, i);
        if (v2_dist(world_mouse, corner) <= CORNER_RADIUS) {
            return i;
        }
    }
    return CORNER_NONE;
}

void handle_mouse_click(Atelier* atelier, int x, int y) {
    // Only handle clicks within the viewport
    if (x >= VIEWPORT_WIDTH) {
        // Click is in UI panel - handle UI interactions here
        return;
    }

    V2 mouse = {(float)x, (float)y};
    Corner clicked = detect_corner_click(atelier, mouse);

    if (clicked != CORNER_NONE) {
        atelier->selected_corner = clicked;
        calculate_diagonal(atelier);
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

    Atelier atelier = {0};
    atelier.running = true;
    atelier.ui_state.step = UI_LENS;
    atelier.ui_state.shelf_mode_save = false;
    atelier.ui_state.suggested_filename[0] = '\0';
    atelier.selected_corner = CORNER_BR;  // Start with bottom-right selected
    atelier.label_anchor = LABEL_BOTTOM_RIGHT;  // Start with bottom-right labels
    atelier.band_offset = 0;  // Start at beginning of band list
    atelier.one_side = false;  // Draw on both sides of diagonal
    atelier.bounding_center = (V2){VIEWPORT_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center in viewport
    atelier.bounding_half = (BOUNDING_SIZE - BOUNDING_PADDING) / 2;

    // Initialize camera (centered in viewport)
    atelier.camera.offset = (V2){VIEWPORT_WIDTH / 2, WINDOW_HEIGHT / 2};  // Center of viewport
    atelier.camera.scale = 1.0f;  // Default zoom
    atelier.dragging = false;

    // Initialize sliver camera to show 0-10 range
    atelier.sliver_camera.offset = 0.0f;  // Start at 0
    atelier.sliver_camera.scale = 0.1f;   // Scale 1:1 shows full 0-1 in viewport, which maps to 0-10 in our coordinates

    // Initialize work (bands and labels in single arena)
    atelier.work = work_new(128);  // 128 bands

    string_builder_init(&atelier.string_builder);

    // Create window with HiDPI support (from ../sd)
    atelier.window = SDL_CreateWindow("Squares on Diagonal",
                                   SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                   WINDOW_WIDTH, WINDOW_HEIGHT,
                                   SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_RESIZABLE);
    if (!atelier.window) {
        fprintf(stderr, "SDL_CreateWindow failed: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    atelier.renderer = SDL_CreateRenderer(atelier.window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!atelier.renderer) {
        fprintf(stderr, "SDL_CreateRenderer failed: %s\n", SDL_GetError());
        SDL_DestroyWindow(atelier.window);
        SDL_Quit();
        return 1;
    }

    // Initialize render context
    render_context_init(&atelier.render_ctx, atelier.renderer);

    // Set up logical size for consistent coordinates
    SDL_RenderSetLogicalSize(atelier.renderer, WINDOW_WIDTH, WINDOW_HEIGHT);

    // Load font with 2x size for supersampling
    atelier.font = TTF_OpenFont("SourceCodePro-Regular.ttf", 36);  // 2x size for supersampling
    if (!atelier.font) {
        fprintf(stderr, "Failed to load SourceCodePro-Regular.ttf: %s\n", TTF_GetError());
        // Continue without font - UI text won't be rendered
    }

    // Check for HiDPI
    int window_w, window_h;
    int render_w, render_h;
    SDL_GetWindowSize(atelier.window, &window_w, &window_h);
    SDL_GetRendererOutputSize(atelier.renderer, &render_w, &render_h);
    printf("Window: %dx%d, Renderer: %dx%d\n", window_w, window_h, render_w, render_h);
    if (render_w != window_w || render_h != window_h) {
        printf("HiDPI detected: scale factor %.2fx\n", (float)render_w / window_w);
    }

    calculate_diagonal(&atelier);
    init_bands_rand(&atelier);

    // Enable text input for input fields
    SDL_StartTextInput();

    SDL_Event event;
    while (atelier.running) {
        // Update mouse state before processing events
        atelier.mouse_was_pressed = atelier.mouse_pressed;
        int mouse_x, mouse_y;
        Uint32 mouse_state = SDL_GetMouseState(&mouse_x, &mouse_y);
        atelier.mouse_pos = (V2){(float)mouse_x, (float)mouse_y};
        atelier.mouse_pressed = (mouse_state & SDL_BUTTON(SDL_BUTTON_LEFT)) != 0;

        while (SDL_PollEvent(&event)) {
            switch (event.type) {
                case SDL_QUIT:
                    atelier.running = false;
                    break;

                case SDL_MOUSEBUTTONDOWN:
                    if (event.button.button == SDL_BUTTON_LEFT) {
                        if (event.button.x < VIEWPORT_WIDTH) {
                            if (!(SDL_GetModState() & KMOD_SHIFT)) {
                                handle_mouse_click(&atelier, event.button.x, event.button.y);
                            } else {
                                // Shift+click to drag
                                atelier.dragging = true;
                                atelier.drag_start = (V2){(float)event.button.x, (float)event.button.y};
                                atelier.camera_start = atelier.camera.offset;
                            }
                        }
                        // UI panel clicks are handled by button hover check
                    } else if (event.button.button == SDL_BUTTON_RIGHT ||
                              event.button.button == SDL_BUTTON_MIDDLE) {
                        // Start dragging only if in viewport
                        if (event.button.x < VIEWPORT_WIDTH) {
                            atelier.dragging = true;
                            atelier.drag_start = (V2){(float)event.button.x, (float)event.button.y};
                            atelier.camera_start = atelier.camera.offset;
                        }
                    }
                    break;

                case SDL_MOUSEBUTTONUP:
                    if (event.button.button == SDL_BUTTON_RIGHT ||
                        event.button.button == SDL_BUTTON_MIDDLE ||
                        event.button.button == SDL_BUTTON_LEFT) {
                        atelier.dragging = false;
                    }
                    break;

                case SDL_MOUSEMOTION:
                    if (atelier.dragging) {
                        V2 mouse = {(float)event.motion.x, (float)event.motion.y};
                        V2 delta = v2_scale(v2_sub(mouse, atelier.drag_start), 1.0f / atelier.camera.scale);
                        atelier.camera.offset = v2_sub(atelier.camera_start, delta);
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
                        V2 mouse_world_before = screen_to_world(mouse_screen, &atelier.camera);

                        // Smoother zoom with smaller increments
                        float zoom_speed = 0.05f;
                        float zoom_factor = 1.0f + (event.wheel.y * zoom_speed);
                        atelier.camera.scale *= zoom_factor;

                        // Clamp zoom level
                        if (atelier.camera.scale < 0.1f) atelier.camera.scale = 0.1f;
                        if (atelier.camera.scale > 10.0f) atelier.camera.scale = 10.0f;

                        // Adjust camera offset to zoom towards mouse position
                        V2 mouse_world_after = screen_to_world(mouse_screen, &atelier.camera);
                        V2 world_diff = v2_sub(mouse_world_after, mouse_world_before);
                        atelier.camera.offset = v2_sub(atelier.camera.offset, world_diff);
                    }
                    break;

                case SDL_TEXTINPUT:
                    // Handle text input for active input field
                    if (atelier.active_field.ptr != NULL) {
                        const char* text = event.text.text;

                        while (*text) {
                            char c = *text++;
                            bool accept = false;

                            if (!atelier.active_field.is_numeric) {
                                // Accept any printable character for text fields
                                accept = (c >= 32 && c <= 126);
                            } else {
                                // Accept numbers, decimal point, and minus sign for numeric fields
                                accept = ((c >= '0' && c <= '9') || c == '.' || (c == '-' && atelier.cursor_pos == 0));
                            }

                            if (accept) {
                                // Insert character at cursor position
                                int len = strlen(atelier.input_buffer);
                                if (len < sizeof(atelier.input_buffer) - 1) {
                                    memmove(&atelier.input_buffer[atelier.cursor_pos + 1],
                                           &atelier.input_buffer[atelier.cursor_pos],
                                           len - atelier.cursor_pos + 1);
                                    atelier.input_buffer[atelier.cursor_pos] = c;
                                    atelier.cursor_pos++;
                                }
                            }
                        }
                    }
                    break;

                case SDL_KEYDOWN:
                    // Handle input field keyboard input first
                    if (atelier.active_field.ptr != NULL) {
                        float* active_value = atelier.active_field.is_numeric ? (float*)atelier.active_field.ptr : NULL;
                        SDL_Keycode key = event.key.keysym.sym;
                        SDL_Keymod mod = SDL_GetModState();

                        switch (key) {
                            case SDLK_ESCAPE: {
                                // Cancel editing - for text fields, no need to restore
                                // For numeric fields, restore original value
                                if (atelier.active_field.is_numeric && active_value) {
                                    *active_value = atelier.input_original_value;
                                }
                                atelier.active_field.ptr = NULL;
                                // Keep text input active
                                break;
                            }

                            case SDLK_RETURN:
                            case SDLK_KP_ENTER: {
                                // Apply value and deactivate
                                if (!atelier.active_field.is_numeric) {
                                    // It's a text field - copy directly to the pointer
                                    char* text_field = (char*)atelier.active_field.ptr;
                                    strncpy(text_field, atelier.input_buffer, 31);  // Band labels are 32 bytes
                                    text_field[31] = '\0';
                                } else if (active_value && strlen(atelier.input_buffer) > 0) {
                                    *active_value = atof(atelier.input_buffer);
                                }
                                atelier.active_field.ptr = NULL;
                                // Don't stop text input - it causes issues with subsequent fields
                                break;
                            }

                            case SDLK_UP:
                                // Increment value (only for numeric fields)
                                if (atelier.active_field.is_numeric && active_value) {
                                    if (mod & KMOD_SHIFT) {
                                        *active_value += 1.0f;
                                    } else if (mod & KMOD_CTRL) {
                                        *active_value += 0.01f;
                                    } else {
                                        *active_value += 0.1f;
                                    }
                                    snprintf(atelier.input_buffer, sizeof(atelier.input_buffer), "%.2f", *active_value);
                                    atelier.cursor_pos = strlen(atelier.input_buffer);
                                }
                                break;

                            case SDLK_DOWN:
                                // Decrement value (only for numeric fields)
                                if (atelier.active_field.is_numeric && active_value) {
                                    if (mod & KMOD_SHIFT) {
                                        *active_value -= 1.0f;
                                    } else if (mod & KMOD_CTRL) {
                                        *active_value -= 0.01f;
                                    } else {
                                        *active_value -= 0.1f;
                                    }
                                    snprintf(atelier.input_buffer, sizeof(atelier.input_buffer), "%.2f", *active_value);
                                    atelier.cursor_pos = strlen(atelier.input_buffer);
                                }
                                break;

                            case SDLK_BACKSPACE:
                                // Delete character before cursor
                                if (atelier.cursor_pos > 0) {
                                    memmove(&atelier.input_buffer[atelier.cursor_pos - 1],
                                           &atelier.input_buffer[atelier.cursor_pos],
                                           strlen(&atelier.input_buffer[atelier.cursor_pos]) + 1);
                                    atelier.cursor_pos--;
                                }
                                break;

                            case SDLK_LEFT:
                                // Move cursor left
                                if (atelier.cursor_pos > 0) {
                                    atelier.cursor_pos--;
                                }
                                break;

                            case SDLK_RIGHT:
                                // Move cursor right
                                if (atelier.cursor_pos < strlen(atelier.input_buffer)) {
                                    atelier.cursor_pos++;
                                }
                                break;

                            case SDLK_HOME:
                                atelier.cursor_pos = 0;
                                break;

                            case SDLK_END:
                                atelier.cursor_pos = strlen(atelier.input_buffer);
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
                            atelier.running = false;
                            break;
                        case SDLK_f:
                        case SDLK_F11:
                            // Toggle fullscreen
                            {
                                Uint32 flags = SDL_GetWindowFlags(atelier.window);
                                if (flags & SDL_WINDOW_FULLSCREEN_DESKTOP) {
                                    SDL_SetWindowFullscreen(atelier.window, 0);
                                } else {
                                    SDL_SetWindowFullscreen(atelier.window, SDL_WINDOW_FULLSCREEN_DESKTOP);
                                }
                            }
                            break;

                        // Sliver camera controls
                        case SDLK_LEFT:
                            {
                                // Zoom out sliver from center (show more range)
                                float center = atelier.sliver_camera.offset + 0.5f / atelier.sliver_camera.scale;
                                atelier.sliver_camera.scale /= 1.2f;
                                if (atelier.sliver_camera.scale < 0.01f) atelier.sliver_camera.scale = 0.01f;  // Min zoom: show 100 units
                                atelier.sliver_camera.offset = center - 0.5f / atelier.sliver_camera.scale;
                            }
                            break;
                        case SDLK_RIGHT:
                            {
                                // Zoom in sliver from center (show less range)
                                float center = atelier.sliver_camera.offset + 0.5f / atelier.sliver_camera.scale;
                                atelier.sliver_camera.scale *= 1.2f;
                                if (atelier.sliver_camera.scale > 2.0f) atelier.sliver_camera.scale = 2.0f;  // Max zoom: show 0.5 units
                                atelier.sliver_camera.offset = center - 0.5f / atelier.sliver_camera.scale;
                            }
                            break;
                        case SDLK_UP:
                            // Pan sliver view left (move by 1 unit in the 0-10 space)
                            atelier.sliver_camera.offset -= 0.1f / atelier.sliver_camera.scale;
                            break;
                        case SDLK_DOWN:
                            // Pan sliver view right (move by 1 unit in the 0-10 space)
                            atelier.sliver_camera.offset += 0.1f / atelier.sliver_camera.scale;
                            break;
                        case SDLK_0:
                            // Reset sliver camera to show full 0-10 range
                            atelier.sliver_camera.offset = 0.0f;
                            atelier.sliver_camera.scale = 0.1f;  // Show 0-10 range
                            break;
                    }
                    break;
            }
        }


        render(&atelier);
        SDL_Delay(16);  // ~60 FPS
    }

    // Cleanup - free all works in list
    Work* work = atelier.work;
    while (work && work->prev) work = work->prev;
    while (work) {
        Work* next = work->next;
        work_close(work);
        work = next;
    }
    render_context_free(&atelier.render_ctx);
    if (atelier.font) {
        TTF_CloseFont(atelier.font);
    }
    SDL_DestroyRenderer(atelier.renderer);
    SDL_DestroyWindow(atelier.window);
    TTF_Quit();
    SDL_Quit();

    return 0;
}
