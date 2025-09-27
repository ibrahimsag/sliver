// color.h - Single-header color space transformation library
// Focuses on OKLCH color space for perceptually uniform color manipulation
//
// OKLCH is a cylindrical version of OKLab:
// - L: Lightness (0 to 1, where 0 is black and 1 is white)
// - C: Chroma (0 to ~0.4, how colorful - 0 is gray)
// - H: Hue (0 to 360 degrees, the color angle)

#ifndef COLOR_H
#define COLOR_H

#include <math.h>
#include <stdbool.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// Color structures
typedef struct {
    float r, g, b;  // Range [0, 1]
} ColorRGB;

typedef struct {
    float r, g, b;  // Linear RGB [0, 1] 
} ColorLinearRGB;

typedef struct {
    float L, a, b;  // OKLab color space
} ColorOKLab;

typedef struct {
    float L;  // Lightness [0, 1]
    float C;  // Chroma [0, ~0.4]
    float h;  // Hue [0, 360]
} ColorOKLCH;

// Forward declarations
static ColorLinearRGB color_srgb_to_linear(ColorRGB srgb);
static ColorRGB color_linear_to_srgb(ColorLinearRGB linear);
static ColorOKLab color_linear_to_oklab(ColorLinearRGB linear);
static ColorLinearRGB color_oklab_to_linear(ColorOKLab oklab);
static ColorOKLCH color_oklab_to_oklch(ColorOKLab oklab);
static ColorOKLab color_oklch_to_oklab(ColorOKLCH oklch);

// ============================================================================
// sRGB <-> Linear RGB conversions (gamma correction)
// ============================================================================

static inline float srgb_to_linear_channel(float channel) {
    if (channel <= 0.04045f) {
        return channel / 12.92f;
    } else {
        return powf((channel + 0.055f) / 1.055f, 2.4f);
    }
}

static inline float linear_to_srgb_channel(float channel) {
    if (channel <= 0.0031308f) {
        return channel * 12.92f;
    } else {
        return 1.055f * powf(channel, 1.0f / 2.4f) - 0.055f;
    }
}

static ColorLinearRGB color_srgb_to_linear(ColorRGB srgb) {
    return (ColorLinearRGB){
        .r = srgb_to_linear_channel(srgb.r),
        .g = srgb_to_linear_channel(srgb.g),
        .b = srgb_to_linear_channel(srgb.b)
    };
}

static ColorRGB color_linear_to_srgb(ColorLinearRGB linear) {
    return (ColorRGB){
        .r = linear_to_srgb_channel(linear.r),
        .g = linear_to_srgb_channel(linear.g),
        .b = linear_to_srgb_channel(linear.b)
    };
}

// ============================================================================
// Linear RGB <-> OKLab conversions
// ============================================================================

static ColorOKLab color_linear_to_oklab(ColorLinearRGB c) {
    float l = 0.4122214708f * c.r + 0.5363325363f * c.g + 0.0514459929f * c.b;
    float m = 0.2119034982f * c.r + 0.6806995451f * c.g + 0.1073969566f * c.b;
    float s = 0.0883024619f * c.r + 0.2817188376f * c.g + 0.6299787005f * c.b;

    float l_ = cbrtf(l);
    float m_ = cbrtf(m);
    float s_ = cbrtf(s);

    return (ColorOKLab){
        .L = 0.2104542553f * l_ + 0.7936177850f * m_ - 0.0040720468f * s_,
        .a = 1.9779984951f * l_ - 2.4285922050f * m_ + 0.4505937099f * s_,
        .b = 0.0259040371f * l_ + 0.7827717662f * m_ - 0.8086757660f * s_
    };
}

static ColorLinearRGB color_oklab_to_linear(ColorOKLab c) {
    float l_ = c.L + 0.3963377774f * c.a + 0.2158037573f * c.b;
    float m_ = c.L - 0.1055613458f * c.a - 0.0638541728f * c.b;
    float s_ = c.L - 0.0894841775f * c.a - 1.2914855480f * c.b;

    float l = l_ * l_ * l_;
    float m = m_ * m_ * m_;
    float s = s_ * s_ * s_;

    return (ColorLinearRGB){
        .r = +4.0767416621f * l - 3.3077115913f * m + 0.2309699292f * s,
        .g = -1.2684380046f * l + 2.6097574011f * m - 0.3413193965f * s,
        .b = -0.0041960863f * l - 0.7034186147f * m + 1.7076147010f * s
    };
}

// ============================================================================
// OKLab <-> OKLCH conversions (rectangular <-> cylindrical)
// ============================================================================

static ColorOKLCH color_oklab_to_oklch(ColorOKLab oklab) {
    ColorOKLCH oklch;
    oklch.L = oklab.L;
    oklch.C = sqrtf(oklab.a * oklab.a + oklab.b * oklab.b);
    
    // Calculate hue in degrees
    float h_rad = atan2f(oklab.b, oklab.a);
    oklch.h = h_rad * (180.0f / M_PI);
    if (oklch.h < 0) {
        oklch.h += 360.0f;
    }
    
    return oklch;
}

static ColorOKLab color_oklch_to_oklab(ColorOKLCH oklch) {
    float h_rad = oklch.h * (M_PI / 180.0f);
    return (ColorOKLab){
        .L = oklch.L,
        .a = oklch.C * cosf(h_rad),
        .b = oklch.C * sinf(h_rad)
    };
}

// ============================================================================
// High-level conversions: sRGB <-> OKLCH
// ============================================================================

static ColorOKLCH color_srgb_to_oklch(ColorRGB srgb) {
    ColorLinearRGB linear = color_srgb_to_linear(srgb);
    ColorOKLab oklab = color_linear_to_oklab(linear);
    return color_oklab_to_oklch(oklab);
}

static ColorRGB color_oklch_to_srgb(ColorOKLCH oklch) {
    ColorOKLab oklab = color_oklch_to_oklab(oklch);
    ColorLinearRGB linear = color_oklab_to_linear(oklab);
    ColorRGB srgb = color_linear_to_srgb(linear);
    
    // Clamp to valid sRGB range
    srgb.r = fmaxf(0.0f, fminf(1.0f, srgb.r));
    srgb.g = fmaxf(0.0f, fminf(1.0f, srgb.g));
    srgb.b = fmaxf(0.0f, fminf(1.0f, srgb.b));
    
    return srgb;
}

// ============================================================================
// Utility functions for OKLCH manipulation
// ============================================================================

// Interpolate between two colors in OKLCH space
static ColorOKLCH color_oklch_lerp(ColorOKLCH a, ColorOKLCH b, float t) {
    // Handle hue interpolation (shortest path around the circle)
    float h_diff = b.h - a.h;
    if (h_diff > 180.0f) {
        h_diff -= 360.0f;
    } else if (h_diff < -180.0f) {
        h_diff += 360.0f;
    }
    
    float h = a.h + h_diff * t;
    if (h < 0) h += 360.0f;
    if (h >= 360.0f) h -= 360.0f;
    
    return (ColorOKLCH){
        .L = a.L + (b.L - a.L) * t,
        .C = a.C + (b.C - a.C) * t,
        .h = h
    };
}

// Rotate hue by a given angle (in degrees)
static ColorOKLCH color_oklch_rotate_hue(ColorOKLCH color, float angle) {
    color.h += angle;
    while (color.h < 0) color.h += 360.0f;
    while (color.h >= 360.0f) color.h -= 360.0f;
    return color;
}

// Adjust lightness (additive)
static ColorOKLCH color_oklch_adjust_lightness(ColorOKLCH color, float delta) {
    color.L = fmaxf(0.0f, fminf(1.0f, color.L + delta));
    return color;
}

// Adjust chroma (multiplicative)
static ColorOKLCH color_oklch_adjust_chroma(ColorOKLCH color, float factor) {
    color.C *= factor;
    color.C = fmaxf(0.0f, color.C);
    return color;
}

// ============================================================================
// SDL_Color conversion helpers
// ============================================================================

#ifdef SDL_h_
// Convert SDL_Color (0-255) to ColorRGB (0-1)
static ColorRGB color_from_sdl(SDL_Color c) {
    return (ColorRGB){
        .r = c.r / 255.0f,
        .g = c.g / 255.0f,
        .b = c.b / 255.0f
    };
}

// Convert ColorRGB (0-1) to SDL_Color (0-255)
static SDL_Color color_to_sdl(ColorRGB c) {
    return (SDL_Color){
        .r = (Uint8)(c.r * 255.0f + 0.5f),
        .g = (Uint8)(c.g * 255.0f + 0.5f),
        .b = (Uint8)(c.b * 255.0f + 0.5f),
        .a = 255
    };
}

// Convert SDL_Color to OKLCH
static ColorOKLCH color_sdl_to_oklch(SDL_Color c) {
    return color_srgb_to_oklch(color_from_sdl(c));
}

// Convert OKLCH to SDL_Color
static SDL_Color color_oklch_to_sdl(ColorOKLCH oklch) {
    return color_to_sdl(color_oklch_to_srgb(oklch));
}

// Helper to generate harmonious colors using OKLCH
SDL_Color make_color_oklch(float L, float C, float h) {
    ColorOKLCH oklch = {.L = L, .C = C, .h = h};
    return color_oklch_to_sdl(oklch);
}
#endif

// ============================================================================
// Color palette generation helpers
// ============================================================================

// Generate N colors with evenly spaced hues at given lightness and chroma
static void color_oklch_hue_wheel(ColorOKLCH* colors, int count, float L, float C) {
    for (int i = 0; i < count; i++) {
        colors[i] = (ColorOKLCH){
            .L = L,
            .C = C,
            .h = (360.0f * i) / count
        };
    }
}

// Generate a gradient between two colors in OKLCH space
static void color_oklch_gradient(ColorOKLCH* colors, int count, ColorOKLCH start, ColorOKLCH end) {
    for (int i = 0; i < count; i++) {
        float t = (count > 1) ? (float)i / (count - 1) : 0.0f;
        colors[i] = color_oklch_lerp(start, end, t);
    }
}

#endif // COLOR_H
