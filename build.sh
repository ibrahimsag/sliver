#!/bin/bash

# Build script for squares on diagonal visualization

set -e  # Exit on error

echo "Building main..."
gcc -o main main.c `sdl2-config --cflags --libs` -lSDL2_ttf -lm

echo "Build complete. Run with: ./main"