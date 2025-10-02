# Band Visualization Tool

A real-time band visualization application built with C and SDL2, featuring interactive diagonal-based geometry rendering with anti-aliased graphics.

## Dependencies

This application requires:
- SDL2 
- SDL2_ttf
- A C compiler (clang/gcc)

## Installation

### macOS

Using Homebrew:
```bash
brew install sdl2 sdl2_ttf
```

### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install libsdl2-dev libsdl2-ttf-dev
```

## Building

Once dependencies are installed, build the application:

```bash
./build.sh
```

## Running

```bash
./main
```

## Controls

### Viewport Control
- **Right-click + drag**: Pan the viewport
- **Scroll wheel**: Zoom the viewport (scales the rendered square)
- **Corner selection**: Left-click on corner indicators

### Navigation
- **Up/Down arrows**: Pan through the band range (slide the visible window)
- **Left/Right arrows**: Zoom the band range (change how much of 0-10 is visible)
- **0 key**: Reset to show full 0-10 range

### Band Editing
- **Label field**: Click to edit band labels
- **Numeric fields**: Click to edit start/end values
- **C button**: Copy band (creates duplicate after current)
- **J button**: Join with previous band (extends previous band to current's end)
- **X button**: Delete band
- **1/2 toggle**: Switch between one-side and two-side rendering
- **O/I toggle**: Toggle label position (outside/inside)

### Line Styles
- **Sharp**: Regular rectangular bands
- **Rounded**: Bands with rounded corners
- **Wave**: Sinusoidal wave patterns
- **Double**: Double-line borders

### File Operations
- **Save/Load**: Persistent storage of band configurations
- Works are saved as text files with band parameters

## Features

- **Anti-aliased rendering**: Smooth edges on all geometric shapes using alpha blending
- **Dynamic band management**: Add, remove, copy, and join bands in real-time
- **Flexible positioning**: Bands can be positioned along customizable diagonals
- **Label system**: Attach labels to bands with configurable anchoring
- **Color support**: OKLCH color space for perceptually uniform colors
- **Responsive UI**: Immediate mode GUI with hover states and visual feedback

## Architecture

The application uses:
- Direct geometry generation with vertex buffers
- Connected quad strips for efficient rendering
- Edge feathering for anti-aliasing
- Arena allocation for work-specific memory management
