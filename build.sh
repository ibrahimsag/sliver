#!/bin/bash

# Build script for squares on diagonal visualization

set -e  # Exit on error

# Remove trailing whitespace from source files
echo "Cleaning trailing whitespace..."
sed -i '' 's/[[:space:]]*$//' main.c 2>/dev/null || true

echo "Building main..."
gcc -o main main.c `sdl2-config --cflags --libs` -lSDL2_ttf -lm

echo "Build complete. Run with: ./main"