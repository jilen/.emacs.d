#!/bin/bash
set -e # Exit immediately if a command exits with a non-zero status.

# --- Configuration ---
REPO_URL="https://github.com/jadestrong/lsp-proxy.git"
TARGET_DIR="/home/jilen/.emacs.d/site-lisp/lsp-proxy"
TEMP_DIR=$(mktemp -d)

# --- Functions ---
# Ensures the temporary directory is removed when the script exits
cleanup() {
  echo "Cleaning up temporary directory: $TEMP_DIR"
  rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

# --- Main Execution ---
echo "Cloning repository into a temporary directory..."
git clone --depth 1 "$REPO_URL" "$TEMP_DIR"

echo "Creating target directory '$TARGET_DIR' if it doesn't exist..."
mkdir -p "$TARGET_DIR"

echo "Copying .el files, preserving directory structure..."
cd "$TEMP_DIR"
# Find all .el files and copy them with their parent directory structure
find . -type f -name "*.el" -exec cp --parents -t "$TARGET_DIR" {} +
cd - > /dev/null

echo "Successfully copied .el files to $TARGET_DIR."