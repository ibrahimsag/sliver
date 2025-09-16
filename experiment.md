# Squares on Shared Diagonal Experiment

## Concept
Drawing multiple axis-aligned squares that all share a common diagonal at 45 degrees using C and SDL2.

## Constraints
- All squares are axis-aligned (edges parallel to x/y axes)
- The shared diagonal is always at 45° angle
- Squares can be positioned at different points along the diagonal
- Squares can have different sizes

## Mathematical Foundation

### Constants
- `CORNER_RADIUS`: Detection radius for corner selection (e.g., 20 pixels)
- `BOUNDING_SIZE`: Size of the large bounding square (e.g., 400 pixels)
- `HIGHLIGHT_SIZE`: Size of corner indicator circles (e.g., 8 pixels)

### Diagonal Definition via Triangle Selection
- Large bounding square defines the space (e.g., 400x400 pixels centered on screen)
- Clicking within a small radius (e.g., 20 pixels) of a corner selects that corner
- Each corner defines a unique triangle with the clicked corner as the last vertex (CCW order):
  - **Top-left corner**: Triangle with vertices (BL, TR, TL) → diagonal from BL to TR
  - **Top-right corner**: Triangle with vertices (TL, BR, TR) → diagonal from TL to BR  
  - **Bottom-right corner**: Triangle with vertices (TR, BL, BR) → diagonal from TR to BL
  - **Bottom-left corner**: Triangle with vertices (BR, TL, BL) → diagonal from BR to TL
- The selected corner is always the last vertex in CCW order
- The diagonal connects the first two vertices of the triangle
- For orientation clarity: draw the edge from the selected corner to one diagonal endpoint

### Square Construction
For a 45° diagonal from point p1 to p2:
- Since diagonal is at 45°, we have |p2.x - p1.x| = |p2.y - p1.y|
- The four corners of the axis-aligned square are:
  - (min(p1.x, p2.x), min(p1.y, p2.y))
  - (min(p1.x, p2.x), max(p1.y, p2.y))
  - (max(p1.x, p2.x), min(p1.y, p2.y))
  - (max(p1.x, p2.x), max(p1.y, p2.y))
- Or more simply: the bounding box of the two diagonal endpoints

### Positioning Squares Along Diagonal
- Parameter t ∈ [0, 1] defines position along diagonal
- Point on diagonal: v2_lerp(p1, p2, t)
- For square at position t with size factor s:
  - Center point: v2_lerp(diagonal.start, diagonal.end, t)
  - Half-diagonal length: s × v2_dist(diagonal.start, diagonal.end) / 2
  - Diagonal endpoints for this square:
    - p1 = center - (half_diagonal, half_diagonal)
    - p2 = center + (half_diagonal, half_diagonal)
  - Call draw_square(p1, p2)

## Implementation Plan

### Data Structures
```c
typedef struct {
    float x, y;
} V2;

// Vector operations
V2 v2_add(V2 a, V2 b) { return (V2){a.x + b.x, a.y + b.y}; }
V2 v2_sub(V2 a, V2 b) { return (V2){a.x - b.x, a.y - b.y}; }
V2 v2_scale(V2 v, float s) { return (V2){v.x * s, v.y * s}; }
V2 v2_lerp(V2 a, V2 b, float t) { return v2_add(a, v2_scale(v2_sub(b, a), t)); }
float v2_dist(V2 a, V2 b) { V2 d = v2_sub(b, a); return sqrtf(d.x * d.x + d.y * d.y); }
float v2_length(V2 v) { return sqrtf(v.x * v.x + v.y * v.y); }

typedef struct {
    V2 start, end;    // diagonal endpoints
} Diagonal;

typedef struct {
    float t;          // position along diagonal [0,1]
    float size;       // size factor [0,1]
    SDL_Color color;  // square color
} Square;
```

### Key Functions
1. `calculate_diagonal()` - Based on selected corner, compute diagonal endpoints
2. `get_corner_position()` - Return V2 position of a corner (TL, TR, BR, BL)
3. `draw_square()` - Draw axis-aligned square given two diagonal endpoints (V2 p1, V2 p2)
4. `draw_line()` - Draw line between two V2 points
5. `handle_mouse_click()` - Check if V2 mouse position is within radius of any corner
6. `draw_corner_indicators()` - Draw circles at corners, highlight selected

### Rendering Pipeline
1. Clear screen with background color
2. Draw bounding square (outline only)
3. Draw small circles at each corner (highlight selected corner)
4. Draw diagonal line (the selected diagonal)
5. Draw one edge of the triangle (from selected corner to one endpoint of diagonal) for orientation
6. For each square in array:
   - Calculate center from diagonal position
   - Draw axis-aligned square

## Build Setup

### Dependencies
- SDL2
- SDL2_gfx (optional, for anti-aliased drawing)

### Compilation
```bash
gcc -o squares squares.c `sdl2-config --cflags --libs` -lm
```

## Interactive Features
- Click within radius of corners to select corner and change diagonal
- Visual feedback: selected corner highlighted with different color
- Press number keys 1-9 to add squares at different positions
- Press +/- to adjust selected square size
- Press SPACE to randomize square positions
- Press C to cycle through color schemes

## Experiments to Try
1. Uniform spacing: Place squares at regular intervals (t = 0, 0.25, 0.5, 0.75, 1.0)
2. Size gradient: Squares get progressively smaller along diagonal
3. Overlapping squares with transparency
4. Animation: Squares sliding along the diagonal
5. Rotation: Slowly rotate entire configuration around center

## The Sliver Concept

### Definition
A "sliver" is a 2D view/projection of a 1D parameter space. It provides a window into a one-dimensional continuum, allowing you to:
- **Offset**: Shift which portion of the parameter space is visible
- **Scale**: Zoom in to see detail or zoom out to see more of the parameter range

### Mathematical Foundation
- Bands and intervals are defined in natural 0-10 coordinate space
- Sliver camera transforms these coordinates for display
- For a parameter `t` in range [0,10]: `t_visible = (t - offset) * scale`
- The transformed values are then normalized to [0,1] for diagonal interpolation
- Values outside visible range after transformation are clipped

### Application in This Project
The sliver camera controls how squares are positioned along the diagonal:
- Band definitions use intuitive 0-10 coordinates (e.g., start=6.6, size=2.1)
- Intervals are stored in the same 0-10 space
- The sliver transformation windows into this 0-10 space
- Scale of 0.1 shows the full 0-10 range, scale of 1.0 shows 1 unit

### Controls
- **Arrow Keys**: Navigate the sliver space
  - Left/Right: Zoom out/in (scale range: 0.01 to 2.0)
  - Up/Down: Pan through the parameter space
- **'0' Key**: Reset sliver camera to show full 0-10 range

This is separate from the 2D viewport camera which moves the entire rendered scene.

## Lessons Learned

### Rounded Rectangle Corner Alignment
When implementing rounded rectangles for squares on a 45° diagonal, we discovered that the corners don't naturally align when squares share diagonal endpoints. The key insight:

1. **The Problem**: A rounded rectangle's arc at 45° is inset from the actual corner by `r * (1 - 1/√2)` where `r` is the corner radius.

2. **The Solution**: To make the arcs of adjacent squares meet seamlessly at their shared diagonal point, we need to extend the rectangle dimensions by exactly `r * (1 - 1/√2)` on all sides.

3. **Mathematical Basis**: 
   - The 45° point on a quarter circle of radius `r` is at `(r/√2, r/√2)` from the circle center
   - This point is inset from the corner by `r - r/√2 = r(1 - 1/√2)`
   - By extending the rectangle, we move the arc centers outward so the 45° points align with the original diagonal endpoints

### Animation Techniques
1. **Bounce Easing**: Implemented `ease_out_bounce` function for natural, playful animation feel
2. **Dual Animation**: Animating both entering (radius 0→1) and exiting (radius 1→0) elements simultaneously creates smooth transitions

### SDL2 Rendering Optimizations
1. **HiDPI Support**: Inherited from previous project, crucial for crisp rendering on high-resolution displays
2. **Fullscreen Toggle**: F/F11 keys for fullscreen mode, useful for presentations
3. **Logical Rendering Size**: Using `SDL_RenderSetLogicalSize` for consistent coordinate system regardless of window size

### Code Organization Insights
1. **Parameterization**: Using t ∈ [0,1] for positioning along diagonal, but accepting user definitions in different ranges (0-10) for convenience
2. **State Management**: `AppState` struct containing all mutable state makes animation and interaction handling cleaner
3. **Separation of Concerns**: `draw_square` function takes diagonal endpoints and handles all square rendering logic internally
4. **Dual Camera System**: Separating 2D viewport navigation from 1D parameter space navigation provides intuitive control
5. **Interval Abstraction**: Using `Interval` struct for (start, end) pairs makes transformations more composable
6. **Dynamic Arrays**: Custom `SquareArray` and `BandArray` structs manage their own memory with capacity/length tracking
7. **Data-Driven Architecture**: 
   - `Band` struct encapsulates pattern generation (start, size, stride, repeat) with visual properties (kind, color)
   - `repeat` field: 0 generates single interval, n generates n+1 intervals total
   - Bands stored persistently in `BandArray`, squares generated on demand
   - Clean separation between data definition and rendering
8. **Split Layout Architecture**:
   - Viewport on left (1400px) for sliver rendering with clipping
   - UI panel on right (520px) for future controls
   - Mouse interactions bounded to viewport area
   - Camera transforms adjusted for viewport centering
9. **Natural Coordinate System**:
   - Bands defined in intuitive 0-10 range
   - Sliver camera manages view into this space
   - Automatic normalization for diagonal interpolation