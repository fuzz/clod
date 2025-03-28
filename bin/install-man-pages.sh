#!/bin/bash
set -e

# Determine the project root directory
PROJECT_ROOT=$(cd "$(dirname "$0")/.." && pwd)

# Create man pages directory in user's XDG config directory
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
USER_MAN_DIR="${XDG_CONFIG_HOME}/clod/man"
mkdir -p "${USER_MAN_DIR}/man1"
mkdir -p "${USER_MAN_DIR}/man7"
mkdir -p "${USER_MAN_DIR}/man8"

# Ensure man pages are generated
"${PROJECT_ROOT}/bin/generate-man-pages.sh"

# Copy the man pages to user's man directory
echo "Installing man pages to ${USER_MAN_DIR}..."
cp "${PROJECT_ROOT}/man/clod.1" "${USER_MAN_DIR}/man1/"
cp "${PROJECT_ROOT}/man/clod.7" "${USER_MAN_DIR}/man7/"
cp "${PROJECT_ROOT}/man/clod.8" "${USER_MAN_DIR}/man8/"

echo "Man pages installed successfully!"

# Detect shell
SHELL_NAME=$(basename "$SHELL")
CONFIG_FILE=""

case "$SHELL_NAME" in
  bash)
    CONFIG_FILE="${HOME}/.bashrc"
    ;;
  zsh)
    CONFIG_FILE="${HOME}/.zshrc"
    ;;
  fish)
    CONFIG_FILE="${HOME}/.config/fish/config.fish"
    ;;
  *)
    echo "Unsupported shell: $SHELL_NAME"
    ;;
esac

# Check if MANPATH already includes our directory
if [ -n "$CONFIG_FILE" ]; then
  if ! grep -q "MANPATH.*${USER_MAN_DIR}" "$CONFIG_FILE" 2>/dev/null; then
    echo ""
    echo "To add the clod man pages to your MANPATH, add this line to $CONFIG_FILE:"
    echo ""
    if [ "$SHELL_NAME" = "fish" ]; then
      echo "set -x MANPATH \$MANPATH ${USER_MAN_DIR}"
    else
      echo "export MANPATH=\$MANPATH:${USER_MAN_DIR}"
    fi
    echo ""
    echo "Or run this command to add it automatically:"
    echo ""
    if [ "$SHELL_NAME" = "fish" ]; then
      echo "echo 'set -x MANPATH \$MANPATH ${USER_MAN_DIR}' >> $CONFIG_FILE"
    else
      echo "echo 'export MANPATH=\$MANPATH:${USER_MAN_DIR}' >> $CONFIG_FILE"
    fi
    echo ""
  else
    echo "Your shell config already includes the clod man path."
  fi
fi

echo "After updating your shell config, you can view the man pages with:"
echo "man -M ${USER_MAN_DIR} 1 clod  # Command reference"
echo "man -M ${USER_MAN_DIR} 7 clod  # Project instructions and safeguards"
echo "man -M ${USER_MAN_DIR} 8 clod  # Complete workflow guide"
echo ""
echo "Or simply run 'man clod' after updating your MANPATH."