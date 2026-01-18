#!/usr/bin/env bash
set -euo pipefail

### Konfiguration ########################################

APP_NAME="BubbleUp"        # Anzeigename
BIN_NAME="GameJam09"             # Name des Cabal-Executables
ICON="assets/frog/frog.png"             # 256x256 PNG
APPIMAGE_TOOL="$HOME/Downloads/appimagetool-x86_64.AppImage"
ASSET_SRC="assets"

##########################################################

echo "==> Building with cabal"
cabal build \
   --disable-shared \
   --disable-library-for-ghci \
   --ghc-options="-O2"

echo "==> Locating binary"
BIN_PATH="$(cabal list-bin exe:${BIN_NAME})"
echo "    Binary: $BIN_PATH"

echo "==> Preparing AppDir"
APPDIR="AppDir"
rm -rf "$APPDIR"
mkdir -p "$APPDIR/usr/bin" "$APPDIR/usr/lib" "$APPDIR/usr/share/${BIN_NAME}"

echo "==> Copying binary"
cp "$BIN_PATH" "$APPDIR/usr/bin/$BIN_NAME"
chmod +x "$APPDIR/usr/bin/$BIN_NAME"

echo "==> Copying assets (*.bmp, *.wav)"
ASSET_DST="$APPDIR/usr/share/${BIN_NAME}/assets"

mkdir -p "$ASSET_DST"

# rekursiv nur bmp und wav kopieren, Verzeichnisstruktur behalten
find "$ASSET_SRC" -type f \( -iname "*.bmp" -o -iname "*.wav" \) \
  | while read -r file; do
      rel="${file#$ASSET_SRC/}"
      mkdir -p "$ASSET_DST/$(dirname "$rel")"
      cp "$file" "$ASSET_DST/$rel"
    done

# -------------------------------------------------------
# Bundle shared libraries
# -------------------------------------------------------
echo "==> Bundling shared libraries"

copy_lib() {
  local lib="$1"
  [[ -f "$lib" ]] || return
  local dest="$APPDIR/usr/lib/$(basename "$lib")"
  [[ -f "$dest" ]] && return
  echo "  + $(basename "$lib")"
  cp "$lib" "$dest"
}

ldd "$BIN_PATH" | awk '/=>/ { print $3 }' | while read -r lib; do
  case "$lib" in
    /lib/*|/usr/lib/libc.so*|/usr/lib/ld-linux* ) ;;
    * ) copy_lib "$lib" ;;
  esac
done

# Also bundle GHC runtime libs (needed!)
GHC_LIBDIR="$(ghc --print-libdir)"
echo "==> Bundling GHC runtime from $GHC_LIBDIR"
cp "$GHC_LIBDIR"/*.so "$APPDIR/usr/lib/" || true
cp "$GHC_LIBDIR"/x86_64-linux-ghc-9.6.6/*.so "$APPDIR/usr/lib/" || true

# -------------------------------------------------------
# AppRun
# -------------------------------------------------------


echo "==> Creating AppRun"
cat > "$APPDIR/AppRun" <<EOF
#!/bin/sh
HERE="\$(dirname "\$(readlink -f "\$0")")"
export LD_LIBRARY_PATH="\$HERE/usr/lib:\$LD_LIBRARY_PATH"
cd "\$HERE/usr/share/${BIN_NAME}" || exit 1
exec "\$HERE/usr/bin/${BIN_NAME}"
EOF
chmod +x "$APPDIR/AppRun"

echo "==> Creating desktop file"
cat > "$APPDIR/${BIN_NAME}.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=${APP_NAME}
Exec=${BIN_NAME}
Icon=${BIN_NAME}
Categories=Game;
Terminal=false
EOF

echo "==> Installing icon"
cp "$ICON" "$APPDIR/${BIN_NAME}.png"

echo "==> Building AppImage"
"$APPIMAGE_TOOL" "$APPDIR"

echo "==> Done!"
ls -lh *.AppImage
