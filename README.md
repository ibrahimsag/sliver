# sliver

A time-interval visualization application built with C and SDL2, featuring interactive diagonal-based geometry rendering.

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

### File Operations
- **< / > buttons**: previous/next 'Work'(file/document) loaded in the program.
- **Save/Load**: Persistent storage of band configurations
- Works are saved as text files with band parameters
- Flat-memory hash-check for change detection since last read/write.

### Bands
- A band is the basic entity, the application works with a list of.
- **Up/Dn buttons**: Scroll the list
- **1/2 toggle**: Switch between one-side(L shapes) and two-side(squares) of timeline rendering

Band fields:
- **Label field**: Click to edit band labels
- **v/V toggle**: Show/hide the band
- **h/H toggle**: Highlight the band, dims and desaturates the color on others.
- **Numeric fields**: Click(or drag) to edit start/end or label offset values
- **^ toggle**: Sets the start to the end of previous. for adjusting consecutives.
- **3x3 grid buttons**: Select label anchor position, will hide label if the corresponding corner/edge is not visible.
- **O/F toggle**: Toggle label-corner relative to anchor position (outside the edge/corner or fixed from top-left)
- **X button**: Delete band
- **C button**: Copy band
- **S button**: Split the band into two
- **J button**: Join with previous band (extends previous band to current's end)
- **>< / <> toggle**: Open/Close intervals, when the half-intervals from start/end can be closed at the corresponding end/start.

### Line Styles
- **Sharp**: Regular rectangular bands
- **Double**: Double-line borders
- **Rounded**: Bands with rounded corners
- **Wave**: Sinusoidal wave patterns
   - wavelength scale, multiples of two, for distinct frequency patterns
   - toggle phase offset of PI radians, inverts the wave
   - toggle full/half period fitting, matches corners

### Navigation
- **Up/Down arrows**: Pan through the band range (slide the visible window)
- **Left/Right arrows**: Zoom the band range (change how much of timeline is visible)
- **0 key**: Reset to show 0-10 range of timeline

### Viewport Control
- **Right-click + drag**: Pan the viewport
- **Scroll wheel**: Zoom the viewport (scales the drawing as a whole)
- **Corner selection**: Left-click on corner indicators for 4 orientations of timeline.

## Features

- **Dynamic band management**: Add, remove, copy, and join bands in real-time
- **Flexible positioning**: Bands can be positioned along customizable diagonals
- **Label system**: Attach labels to bands with configurable anchoring
- **Color support**: OKLCH color space for perceptually uniform colors
- **Responsive UI**: Immediate mode GUI with hover states and visual feedback
