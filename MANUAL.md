# basic65-mode Manual

An Emacs Minor-Mode for convenient editing of **MEGA65 BASIC 65**
programs. It extends [`basic-mode`](https://stable.melpa.org/#/basic-mode) with specific functions for the
MEGA65, including:

- automatic lower case (except in strings) 
- syntax highlighting for BASIC 65 keywords 
- PETSCII control code completion 
- integration with `petcat` and the xmega65 emulator `xemu` 
- retro font switching (C64 Pro Mono)
- display of PRG files in `hexl-mode` Unicode tools for PUA inspection

## Inherited from `Basic Mode`

`M-x indent-for-tab-command` indents for BASIC code.

`M-x newline` can automatically insert a fresh line number if
`‘basic-auto-number’` is set.  Default is disabled.

`M-.` looks up the identifier at point.

### Customization
You can customize the indentation of code blocks, see variable
`basic-indent-offset`.  The default value is `4`.

Formatting is also affected by the customizable variables
`basic-delete-trailing-whitespace` and `delete-trailing-lines`
(from `simple.el`).

You can also customize the number of columns to allocate for line
numbers using the variable `basic-line-number-cols`. The default
value of `0`, no space reserved, is appropriate for programs with
no line numbers and for left aligned numbering. Use a larger
value if you prefer right aligned numbers. Note that the value
includes the space after the line number, so 6 right aligns
5-digit numbers.

The other line number features can be configured by customizing
the variables `basic-auto-number`, `basic-renumber-increment` and
`basic-renumber-unnumbered-lines`.

Whether syntax highlighting requires separators between keywords
can be customized with variable
`basic-syntax-highlighting-require-separator`.

### Key-Bindings

| Key     | Binding                  |
|---------|--------------------------|
| RET     | basic-newline-and-number |
| :       | basic-electric-colon     |
| C-c C-f | basic-format-code        |
| C-c C-r | basic-renumber           |


---

## Installation

1. install Emacs ≥ 25.1 
2. install `basic-mode` within Emacs (`M-x package-install`) 
3. install the additional tools:
   - [`petcat`](https://github.com/mist)
   - install [`xemu`](https://github.com/lgblgblgb/xemu) for xmega65 emulator
4. add `basic65-mode.el` to your `load-path` in your `.emacs`

```
(require 'basic65-mode)
(add-hook 'basic-mode-hook #'basic65-mode)
```

## Usage

### File Associations
```
(add-to-list 'auto-mode-alist '("\\.bas65\\'" . basic65-setup))
(add-to-list 'auto-mode-alist '("\\.bas\\'"   . basic65-setup))
```

### PETSCII Completion
Type `{` inside a string to trigger a completion prompt for PETSCII codes like {clr}, {home}, {rvs-on}, etc.

Manual command: `M-x basic65-petscii-insert-code`

### Xemu Integration

`C-c C-r`: Export PRG and run in Xemu
`C-c C-k`: Kill running Xemu process
`M-x basic65-view-prg`: View PRG file in hexl-mode

### Retro Font Toggle
`M-x basic65-toggle-buffer-font`: Switch to/from PETSCII font

### Customization
Set paths to external tools:
```
(setq basic65-petcat-command "petcat")
(setq basic65-xemu-command "xmega65")
```

### Unicode Tools

`M-x basic65-char-info-at-point`: Show Unicode info for character at point

`M-x basic65-show-unicode-pua-range`: Display MEGA65 Unicode Private Use Area (PUA) range


