;;; basic65-mode.el --- Minor mode for MEGA65 BASIC extensions -*- lexical-binding: t; -*-

;; Author: Peter Rake <prake71@gmail.com>
;; Version: 0.1
;; Keywords: languages, basic, mega65
;; Package-Requires: ((emacs "25.1") (basic-mode "1.0"))
;; URL: https://github.com/prake71/emacs-mega65-basic-ide

;;; Commentary:
;; This minor mode extends `basic-mode` with MEGA65-specific features:
;; - Lowercase-only editing
;; - Additional BASIC 65 keywords
;; - PETSCII control code completion
;; - Planned integration with xemu for running code

;;; Code:

(require 'basic-mode)

;; ------------------------------
;; defcustom
;; ------------------------------
(defcustom basic65-petcat-command "petcat"
  "Path to the petcat utility."
  :type 'string
  :group 'basic65)

(defcustom basic65-xemu-command "xmega65"
  "Path to the xemu MEGA65 executable."
  :type 'string
  :group 'basic65)


;; ------------------------------
;; Lowercase enforcement
;; ------------------------------

(defun basic65-force-lowercase (beg end _len)
  "Ensure inserted text is always lowercase in BASIC65 buffers.
Triggered by `after-change-functions`."
  (save-excursion
    (downcase-region beg end)))

(defun basic65-setup-lowercase ()
  "Activate automatic downcasing in BASIC65 buffers."
  (add-hook 'after-change-functions #'basic65-force-lowercase nil t))

(defun basic65-disable-lowercase ()
  "Disable automatic downcasing in BASIC65 buffers."
  (remove-hook 'after-change-functions #'basic65-force-lowercase t))

;; ------------------------------
;; Keywords
;; ------------------------------

(defconst basic65-extra-keywords
  '("end" "for" "next" "data" "input#" "input" "dim" "read" "let" "goto"
    "run" "if" "restore" "gosub" "return" "rem" "stop" "on" "wait" "load"
    "save" "verify" "def" "poke" "print#" "print" "cont" "list" "clr"
    "cmd" "sys" "open" "close" "get" "new" "tab(" "to" "fn" "spc(" "then"
    "not" "step" "+" "-" "*" "/" "^" "and" "or" ">" "=<" "sgn" "int" "abs"
    "usr" "fre" "pos" "sqr" "rnd" "log" "exp" "cos" "sin" "tan" "atn"
    "peek" "len" "str$" "val" "asc" "chr$" "left$" "right$" "mid$" "go"
    "rgraphic" "rcolor" "joy" "rpen" "dec" "hex$" "err$" "instr" "else"
    "resume" "trap" "tron" "troff" "sound" "vol" "auto" "pudef" "graphic"
    "paint" "char" "box" "circle" "paste" "cut" "line" "merge" "color"
    "scnclr" "xor" "help" "do" "loop" "exit" "dir" "dsave" "dload"
    "header" "scratch" "collect" "copy" "rename" "backup" "delete"
    "renumberkey" "monitor" "using" "until" "while" "~" "pot" "bump"
    "lpen" "rsppos" "rsprite" "rspcolor" "log10" "rwindow" "pointer" "mod"
    "pixel" "rpalette" "rspeed" "rplay" "wpeek" "bank" "filter" "play"
    "tempo" "movspr" "sprite" "sprcolor" "rreg" "envelope" "sleep"
    "catalog" "dopen" "append" "dclose" "bsave" "bload" "record" "concat"
    "dverify" "dclear" "sprsav" "collision" "begin" "bend" "window" "boot"
    "fread#" "wpoke" "fwrite#" "dma" "edma" "mem" "off" "fast" "speed"
    "type" "bverify" "ectory" "erase" "find" "change" "set" "screen"
    "polygon" "ellipse" "viewport" "gcopy" "pen" "palette" "dmode" "dpat"
    "format" "genlock" "foreground" "background" "border" "highlight"
    "mouse" "rmouse" "disk" "cursor" "rcursor" "loadiff" "saveiff" "edit"
    "font" "fgoto" "fgosub" "mount" "freezer" "chdir" "dot" "info" "bit"
    "unlock" "lock" "mkdir" "<<" ">>" "vsync")
  "Additional BASIC65 keywords not present in C64 BASIC V2.")

(defconst basic65-extra-font-lock-keywords
  `((,(regexp-opt basic65-extra-keywords 'words) . font-lock-keyword-face)))


;; ------------------------------
;; PETSCII completion
;; ------------------------------

(defvar basic65-petcat-table
  '(("clr" . "{clr}")
    ("home" . "{home}")
    ("ret" . "{return}")
    ("rvs" . "{rvs-on}")
    ("nor" . "{rvs-off}")
    ("pi"  . "{pi}")
    ("f1" . "{f1}") ("f2" . "{f2}") ("f3" . "{f3}") ("f4" . "{f4}")
    ("f5" . "{f5}") ("f6" . "{f6}") ("f7" . "{f7}") ("f8" . "{f8}"))
  "Mapping of short triggers to PETCAT control codes.")

(defun basic65-complete-petcat ()
  "Completion for PETSCII control codes inside {...}."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end   (cdr bounds)))
    (when (and bounds
               (save-excursion
                 (goto-char start)
                 (looking-back "{\\([^}]*\\)" (line-beginning-position))))
      (list start end
            (mapcar #'cdr basic65-petcat-table)
            :exclusive 'no))))

;; ------------------------------
;; Run in xemu
;; ------------------------------

(defun basic65--buffer-to-prg (output-file)
  "Convert current buffer to a PRG file using petcat."
  (let ((tmp-src (make-temp-file "basic65-" nil ".bas")))
    (write-region (point-min) (point-max) tmp-src)
    (call-process basic65-petcat-command nil "*petcat-output*" t
                  "-w65" "-o" output-file "--" tmp-src)))

(defun basic65-run-in-xemu ()
  "Export the current buffer to a PRG and run it in xemu."
  (interactive)
  (let ((prg-file (make-temp-file "basic65-" nil ".prg")))
    (basic65--buffer-to-prg prg-file)
    (message "Running %s in xemu..." prg-file)
    (start-process "basic65-xemu" "*basic65-xemu*"
                   basic65-xemu-command
                   "-prg" prg-file)))

(defun basic65-exit-xemu ()
  "Kill current xemu session."
  (interactive)
  (stop-process "basic65-xemu"))


;; ------------------------------
;; Minor mode definition
;; ------------------------------

(define-minor-mode basic65-mode
  "Minor mode for editing MEGA65 BASIC code on top of `basic-mode`."
  :lighter " BASIC65"
  (if basic65-mode
      (progn
        (basic65-setup-lowercase)
        (font-lock-add-keywords nil basic65-extra-font-lock-keywords)
        (add-hook 'completion-at-point-functions #'basic65-complete-petcat nil t)
        (message "BASIC65 mode enabled"))
    (basic65-disable-lowercase)
    (font-lock-remove-keywords nil basic65-extra-font-lock-keywords)
    (remove-hook 'completion-at-point-functions #'basic65-complete-petcat t)
    (message "BASIC65 mode disabled")))

;; ------------------------------
;; Helper command
;; ------------------------------

(defun basic65-setup ()
  "Start a buffer in BASIC mode with BASIC65 extensions."
  (interactive)
  (basic-mode)
  (basic65-mode 1)
  (font-lock-fontify-buffer))  ;; refresh highlighting

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bas65\\'" . basic65-setup))
(add-to-list 'auto-mode-alist '("\\.bas\\'"   . basic65-setup))


(provide 'basic65-mode)

;;; basic65-mode.el ends here
