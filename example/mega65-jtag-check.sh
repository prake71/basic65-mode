#!/usr/bin/env bash

set -e

echo "=== MEGA65 JTAG Environment Check ==="
echo

# 1. User & Gruppen
echo "[1] User & Gruppen"
USER_NAME="$(whoami)"
if id -nG "$USER_NAME" | grep -qw uucp; then
  echo "  ✔ User '$USER_NAME' ist in Gruppe 'uucp'"
else
  echo "  ✘ User '$USER_NAME' ist NICHT in Gruppe 'uucp'"
  echo "    → sudo usermod -aG uucp $USER_NAME"
fi
echo

# 2. FTDI per USB
echo "[2] FTDI USB Gerät (lsusb)"
if lsusb | grep -q "0403:6001"; then
  echo "  ✔ FTDI FT232 erkannt (0403:6001)"
else
  echo "  ✘ Kein FTDI FT232 erkannt"
fi
echo

# 3. Kernel-Modul
echo "[3] Kernel-Modul ftdi_sio"
if lsmod | grep -q "^ftdi_sio"; then
  echo "  ✔ ftdi_sio geladen"
else
  echo "  ✘ ftdi_sio NICHT geladen"
  echo "    → sudo modprobe ftdi_sio"
fi
echo

# 4. Device Node
echo "[4] Serial Device"
if [ -e /dev/ttyUSB0 ]; then
  echo "  ✔ /dev/ttyUSB0 existiert"
  ls -l /dev/ttyUSB0
else
  echo "  ✘ /dev/ttyUSB0 nicht gefunden"
fi
echo

# 5. Udev-Regel
echo "[5] udev Regel (FTDI)"
RULE_FILE="/etc/udev/rules.d/40-xilinx.rules"
if [ -f "$RULE_FILE" ]; then
  echo "  ✔ $RULE_FILE vorhanden:"
  grep -E "0403.*6001" "$RULE_FILE" || echo "    ⚠ Regel gefunden, aber kein FTDI-Match?"
else
  echo "  ✘ Keine udev Regel gefunden"
  echo "    → Datei $RULE_FILE anlegen"
fi
echo

# 6. m65 Tool
echo "[6] m65 Tool"
if command -v m65 >/dev/null; then
  echo "  ✔ m65 gefunden: $(command -v m65)"
else
  echo "  ✘ m65 NICHT gefunden"
fi
echo

# 7. Autodiscover Test (ohne sudo)
echo "[7] m65 Autodiscover (ohne sudo)"
if command -v m65 >/dev/null && [ -e /dev/ttyUSB0 ]; then
  if m65 --autodiscover --quiet 2>/dev/null; then
    echo "  ✔ m65 Autodiscover erfolgreich"
  else
    echo "  ✘ m65 Autodiscover fehlgeschlagen (ohne sudo)"
    echo "    → Rechte / Gruppen / udev prüfen"
  fi
else
  echo "  ⚠ Übersprungen"
fi
echo

# 8. Hinweis auf Reboot
echo "[8] Hinweis"
echo "  Nach Gruppenänderungen ist ein Logout/Login oder Reboot nötig."
echo
echo "=== Check abgeschlossen ==="
