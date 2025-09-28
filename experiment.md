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
```

### Key Functions
- `calculate_diagonal()` - Based on selected corner, compute diagonal endpoints
- `get_corner_position()` - Return V2 position of a corner (TL, TR, BR, BL)
- `draw_square()` - Draw axis-aligned square given two diagonal endpoints
- `draw_line()` - Draw line between two V2 points
- `handle_mouse_click()` - Check if mouse position is within radius of any corner
- `draw_corner_indicators()` - Draw circles at corners, highlight selected

### Rendering Pipeline
- Clear screen with background color
- Draw bounding square (outline only)
- Draw small circles at each corner (highlight selected corner)
- Draw diagonal line (the selected diagonal)
- Draw one edge of the triangle for orientation
- For each band/interval: calculate position and draw

## Build Setup

### Dependencies
- SDL2
- SDL2_ttf
- C compiler with C99 support

### Compilation
```bash
./build.sh
```

## Controls

### Mouse
- Click corner to select diagonal orientation
- Click UI elements for band management
- Drag on numeric fields to adjust values (no shift needed)
- Click on numeric fields to enter text input mode

### Keyboard
- **Arrow Keys**: Navigate sliver space (Left/Right: zoom, Up/Down: pan)
- **'0'**: Reset sliver camera to show full 0-10 range
- **F/F11**: Toggle fullscreen
- **ESC/Q**: Quit application

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

- Centered zoom behavior maintains center point during scaling for intuitive navigation

## UI System

### Text Rendering
- SDL_ttf with supersampling for improved quality
- Font loaded at 2x size (36pt) and scaled down for anti-aliasing
- SourceCodePro-Regular.ttf for consistent monospace appearance
- Labels displayed at bottom-right of rendered squares (top-left of text aligned with square's bottom-right corner)

### Layout Management
- `Layout` struct manages UI element positioning with `next` position and `max` bounds
- `advance_layout()` automatically moves to next position with overflow prevention
- Structured vertical layout for band summaries in UI panel

### Immediate Mode UI
- Button widget with hover highlighting and click detection
- Dragging on numeric fields won't trigger button clicks when released
- Mouse position stored in state for cleaner interaction handling
- Per-band controls for switching drawing styles
- Wave-specific controls (wavelength scale, phase, period) appear contextually
- Visual feedback with color-coded borders for different states
- Band start/end movement behavior:
  - Default: Moving start also moves end (preserving band size)
  - Hold Cmd: Move start/end independently

## Color System

### OKLCH Color Space
- Perceptually uniform color manipulation via `color.h` single-header library
- **Lightness** (0-1): Perceptually uniform brightness
- **Chroma** (0-~0.4): Color intensity/saturation  
- **Hue** (0-360°): Color angle on the wheel
- Complete bidirectional transformations: sRGB ↔ Linear RGB ↔ OKLab ↔ OKLCH
- SDL_Color integration for seamless rendering
- Enables harmonious color palettes with consistent perceived brightness

### Input Fields
Numeric input fields with multiple interaction modes:
- **Text Input**: Click to activate field, type values directly
- **Mouse Dragging**: Drag horizontally to adjust values (no shift needed)
  - Drag sensitivity: 0.005 per pixel
  - 3-pixel movement threshold distinguishes click from drag
- **Keyboard Controls**: Arrow keys for increment/decrement when field is active
  - Up/Down: ±0.1 (±1.0 with Shift, ±0.01 with Ctrl)
- **Dynamic Sensitivity**: Drag sensitivity for start/end fields scales with sliver camera zoom
  - More zoomed in = finer control, zoomed out = coarser control
  - Formula: `drag_scale = 0.005f / sliver_camera.scale`
- Visual feedback with color-coded borders for different states
- Pointer-based field identity for immediate-mode UI
- Dragging on numeric fields won't trigger button clicks when released

## Band System
- **Interval-based Bands**: Bands use `Interval` struct with start and end positions
- **Label System**: 
  - Each band has a `char*` label pointing into a centralized `LabelBuffer`
  - Default labels cycle through alphabet (A-Z) based on buffer position
  - Text input field for editing labels in UI
  - Memory managed via fixed 4KB buffer with 32-byte slots
  - **Label Anchoring**: 9-point anchor system for precise label placement
    - Anchors: TOP_LEFT, TOP_CENTER, TOP_RIGHT, MIDDLE_LEFT, CENTER, MIDDLE_RIGHT, BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT
    - Anchor determines which point of the square the label aligns to
    - UI: 3×3 grid button where clicking each cell selects corresponding anchor position
  - **Label Offset**: Fine-tune position with (x, y) pixel offset from anchor point
    - Draggable numeric fields for precise positioning
    - Offset applied after anchor positioning for pixel-perfect control
- **Band Kinds**:
  - `BAND_CLOSED`: Normal finite interval
  - `BAND_OPEN`: Automatically triggered when start ≥ end, creates two intervals extending to ±infinity
  - Toggle button (<> for closed, >< for open) to manually switch between open/closed
- **Auto-positioning**: Bands can automatically follow the previous band's end position
  - `follow_previous` boolean field on Band struct
  - Toggle button (^) next to start field to enable/disable
  - When enabled, start field is disabled and band automatically positions itself
  - If following causes start ≥ end, band automatically becomes BAND_OPEN
- **Band Management UI**:
  - Multiple preset configurations (Weekly, TZ, Backend, Backend2, Random) accessible via buttons
  - Split button (S) divides band at midpoint into two halves
  - Copy button (C) duplicates band with same size, positioned after original
  - Remove button (X) to delete individual bands
  - Add button (+ Band) creates new band with random OKLCH color
  - Add button (+ Open) creates new open band (start=end) at sliver camera center
  - Print button exports current bands to timestamped file (e.g., `bands_2025-01-23_14-30-25.c`)
  - Up/Dn navigation buttons to scroll through band list when there are many bands
  - Labels: TL/BR button toggles label position between top-left and bottom-right of squares
- Per-frame band flattening for immediate updates

## Work Management System

### Multiple Works
- **Linked List**: Works connected via `prev`/`next` pointers for navigation
- **Navigation**: `<` and `>` buttons to move between works (disabled when unavailable)
- **New**: Creates empty work and appends to list after current
- **Copy**: Duplicates current work (clears filename) and appends to list
- **Close**: Removes current work from list (disabled if only one work)
- **Load**: Opens `.wo` file and appends as new work to list

### File Operations
- **Store Button**: Opens save dialog with filename suggestion
  - Uses current filename if available for save-back
  - Otherwise generates timestamped filename: `work_YYYY-MM-DD_HH-MM-SS.wo`
- **Load Button**: Opens file browser showing `.wo` files from current directory
- **Modification Tracking**: 
  - FNV-1a hash computed over work arena memory
  - Hash updated after save/load operations
  - Modified indicator (*) appears in orange when unsaved changes detected
  - Filename displayed with asterisk when modified
  - Shows "(unsaved)" for works without filename
- **File Format**: 
  - Header: `WORK SLIVER 0001` (version 1, zero-padded)
  - Version checking: rejects files with version > current (forward compatibility check)
  - Allows loading older versions (backward compatibility)
  - Each band as structured text block with all properties

### Memory Architecture
- **Arena Allocation**: Single contiguous memory block per work
  - Bands array at start of arena
  - Label buffer follows bands array
  - Single `free()` cleans up entire work
- **Deep Copying**: `work_copy()` remaps label pointers to new arena

## Rendering System

### Geometry Buffer
- SDL2's `SDL_RenderGeometry` for efficient rendering
- Custom `GeometryBuffer` struct manages vertices and indices
- Thick lines rendered as textured quads
- All shapes batched into single draw call per frame
- Anti-aliased arcs and curves through segment approximation

### Drawing Styles (LineKind)
- **SHARP**: Regular rectangles with straight edges
- **ROUNDED**: Rectangles with circular arc corners (radius adapts to square size)
- **DOUBLE**: Two concentric rectangles with spacing equal to line thickness
- **WAVE**: Sine wave edges with configurable parameters

### Wave Rendering
- Wavelength Scale: Integer multiplier (1-10) for wave frequency
- Phase Offset: Toggle between sin(x) and sin(x + π) starting phases
- Period Control: Choose between half or full period endpoints
- Wave amplitude scales with wavelength for visual consistency
- Automatic wavelength adjustment to match edge endpoints exactly

### Edge Visibility Culling
- Handles partially visible squares to avoid visual artifacts at view boundaries
- Bitwise flags (`EDGE_TOP`, `EDGE_RIGHT`, `EDGE_BOTTOM`, `EDGE_LEFT`) control which edges to draw
- Edges connected to diagonal corners outside [0,1] before clamping are hidden
- Intervals appear to extend beyond visible range rather than showing false endpoints
- Correctly handles all 4 diagonal orientations based on selected corner


## Lessons Learned

### Rounded Rectangle Corner Alignment
When implementing rounded rectangles for squares on a 45° diagonal, corners don't naturally align when squares share diagonal endpoints.

- **Problem**: A rounded rectangle's arc at 45° is inset from the actual corner by `r * (1 - 1/√2)` where `r` is the corner radius
- **Solution**: Extend rectangle dimensions by exactly `r * (1 - 1/√2)` on all sides to make arcs meet seamlessly
- **Mathematical Basis**: The 45° point on a quarter circle of radius `r` is at `(r/√2, r/√2)` from the circle center, inset from corner by `r - r/√2 = r(1 - 1/√2)`


### SDL2 Rendering Optimizations
- **HiDPI Support**: Crucial for crisp rendering on high-resolution displays
- **Fullscreen Toggle**: F/F11 keys for fullscreen mode
- **Logical Rendering Size**: Using `SDL_RenderSetLogicalSize` for consistent coordinate system

### Code Organization Insights
 - **Parameterization**: Using t ∈ [0,1] for positioning along diagonal, but accepting user definitions in different ranges (0-10) for convenience
 - **State Management**: `AppState` struct containing all mutable state makes animation and interaction handling cleaner
 - **Separation of Concerns**: `draw_square` function takes diagonal endpoints and handles all square rendering logic internally
 - **Dual Camera System**: Separating 2D viewport navigation from 1D parameter space navigation provides intuitive control
 - **Interval Abstraction**: Using `Interval` struct for (start, end) pairs makes transformations more composable
 - **Dynamic Arrays**: Custom `SquareArray` and `BandArray` structs manage their own memory with capacity/length tracking
 - **Data-Driven Architecture**: 
   - `Band` struct encapsulates pattern generation (start, end, stride, repeat) with visual properties (kind, color)
   - `repeat` field: 0 generates single interval, n generates n+1 intervals total
   - Bands stored persistently in `BandArray`, squares generated on demand
   - Clean separation between data definition and rendering
 - **Dynamic Band Positioning**:
   - `follow_previous` flag enables bands to automatically track previous band's end position
   - Only start position updates, end remains fixed (dynamic sizing)
   - Implemented in `generate_squares_from_bands()` before interval generation
   - Copy operation preserves original size but enables auto-positioning
   - Split operation creates two bands:
     - `band_array_split()` divides at midpoint `(start + end) / 2`
     - Original band becomes first half (end = midpoint)
     - New band becomes second half (start = midpoint, `follow_previous = true`)
     - Both halves retain all visual properties from original
 - **Split Layout Architecture**:
   - Viewport on left (1400px) for sliver rendering with clipping
   - UI panel on right (520px) for future controls
   - Mouse interactions bounded to viewport area
   - Camera transforms adjusted for viewport centering
 - **Natural Coordinate System**:
   - Bands defined in intuitive 0-10 range
   - Sliver camera manages view into this space
   - Automatic normalization for diagonal interpolation
