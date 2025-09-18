;;; basic65-mode.el --- Minor mode for MEGA65 BASIC extensions -*- lexical-binding: t; -*-

;; Author: Peter Rake <prake71@gmail.com>
;; Version: 0.1
;; Keywords: languages, basic, mega65
;; Package-Requires: ((emacs "25.1") (basic-mode "1.0"))
;; URL: https://github.com/prake71/basic65-mode

;;; Commentary:
;; This minor mode extends `basic-mode` with MEGA65-specific features:
;; - Lowercase-only editing
;; - Additional BASIC 65 keywords
;; - PETSCII control code completion
;; - Planned integration with xemu for running code

;;; History:
;;  2025-09-14 added basic65 keywords
;;             allow editing only in lowercase
;;             integration with petcat and xemu Mega65 emulator
;;             derived from basic-mode at
;;             https://github.com/dykstrom/basic-mode
;;  2025-09-16 xemu already running check
;;             kill xemu from within emacs
;; 

;; TODOS:
;;             - petscii code insertion is not working - fix (done)
;;             - killing xemu session from within emacs (done)
;;             - petcat run only with status buffer
;;



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
;; defvar
;; ------------------------------
(defvar-local retro-font-active nil
  "Ob der Retro-Font im aktuellen Buffer aktiv ist.")

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
  '(("\f" . "{\f}")
    ("$0a" . "{$0a}")
    ("$6a" . "{$6a}")
    ("$6b" . "{$6b}")
    ("$6c" . "{$6c}")
    ("$6d" . "{$6d}")
    ("$6e" . "{$6e}")
    ("$6f" . "{$6f}")
    ("$7a" . "{$7a}")
    ("$7b" . "{$7b}")
    ("$7c" . "{$7c}")
    ("$7d" . "{$7d}")
    ("$7e" . "{$7e}")
    ("$7f" . "{$7f}")
    ("$8f" . "{$8f}")
    ("$60" . "{$60}")
    ("$61" . "{$61}")
    ("$62" . "{$62}")
    ("$63" . "{$63}")
    ("$64" . "{$64}")
    ("$65" . "{$65}")
    ("$66" . "{$66}")
    ("$67" . "{$67}")
    ("$68" . "{$68}")
    ("$69" . "{$69}")
    ("$70" . "{$70}")
    ("$71" . "{$71}")
    ("$72" . "{$72}")
    ("$73" . "{$73}")
    ("$74" . "{$74}")
    ("$75" . "{$75}")
    ("$76" . "{$76}")
    ("$77" . "{$77}")
    ("$78" . "{$78}")
    ("$79" . "{$79}")
    ("$80" . "{$80}")
    ("$82" . "{$82}")
    ("$83" . "{$83}")
    ("$84" . "{$84}")
    ("$a0" . "{$a0}")
    ("$de" . "{$de}")
    ("$e0" . "{$e0}")
    ("$e1" . "{$e1}")
    ("$e2" . "{$e2}")
    ("$e3" . "{$e3}")
    ("$e4" . "{$e4}")
    ("$e5" . "{$e5}")
    ("$e6" . "{$e6}")
    ("$e7" . "{$e7}")
    ("$e8" . "{$e8}")
    ("$e9" . "{$e9}")
    ("$ea" . "{$ea}")
    ("$eb" . "{$eb}")
    ("$ec" . "{$ec}")
    ("$ed" . "{$ed}")
    ("$ee" . "{$ee}")
    ("$ef" . "{$ef}")
    ("$f0" . "{$f0}")
    ("$f1" . "{$f1}")
    ("$f2" . "{$f2}")
    ("$f3" . "{$f3}")
    ("$f4" . "{$f4}")
    ("$f5" . "{$f5}")
    ("$f6" . "{$f6}")
    ("$f7" . "{$f7}")
    ("$f8" . "{$f8}")
    ("$f9" . "{$f9}")
    ("$fa" . "{$fa}")
    ("$fb" . "{$fb}")
    ("$fc" . "{$fc}")
    ("$fd" . "{$fd}")
    ("$fe" . "{$fe}")
    ("blk" . "{blk}")
    ("blu" . "{blu}")
    ("brn" . "{brn}")
    ("CBM--" . "{CBM--}")
    ("CBM-@" . "{CBM-@}")
    ("CBM-*" . "{CBM-*}")
    ("CBM-+" . "{CBM-+}")
    ("CBM-A" . "{CBM-A}")
    ("CBM-B" . "{CBM-B}")
    ("CBM-C" . "{CBM-C}")
    ("CBM-D" . "{CBM-D}")
    ("CBM-E" . "{CBM-E}")
    ("CBM-F" . "{CBM-F}")
    ("CBM-G" . "{CBM-G}")
    ("CBM-H" . "{CBM-H}")
    ("CBM-I" . "{CBM-I}")
    ("CBM-J" . "{CBM-J}")
    ("CBM-K" . "{CBM-K}")
    ("CBM-L" . "{CBM-L}")
    ("CBM-M" . "{CBM-M}")
    ("CBM-N" . "{CBM-N}")
    ("CBM-O" . "{CBM-O}")
    ("CBM-P" . "{CBM-P}")
    ("CBM-POUND" . "{CBM-POUND}")
    ("CBM-Q" . "{CBM-Q}")
    ("CBM-R" . "{CBM-R}")
    ("CBM-S" . "{CBM-S}")
    ("CBM-T" . "{CBM-T}")
    ("CBM-U" . "{CBM-U}")
    ("CBM-V" . "{CBM-V}")
    ("CBM-W" . "{CBM-W}")
    ("CBM-X" . "{CBM-X}")
    ("CBM-Y" . "{CBM-Y}")
    ("CBM-Z" . "{CBM-Z}")
    ("clr" . "{clr}")
    ("CTRL-A" . "{CTRL-A}")
    ("CTRL-B" . "{CTRL-B}")
    ("CTRL-D" . "{CTRL-D}")
    ("CTRL-F" . "{CTRL-F}")
    ("CTRL-G" . "{CTRL-G}")
    ("CTRL-K" . "{CTRL-K}")
    ("CTRL-O" . "{CTRL-O}")
    ("CTRL-P" . "{CTRL-P}")
    ("CTRL-U" . "{CTRL-U}")
    ("CTRL-V" . "{CTRL-V}")
    ("CTRL-W" . "{CTRL-W}")
    ("CTRL-X" . "{CTRL-X}")
    ("CTRL-Y" . "{CTRL-Y}")
    ("CTRL-Z" . "{CTRL-Z}")
    ("cyn" . "{cyn}")
    ("del" . "{del}")
    ("dish" . "{dish}")
    ("down" . "{down}")
    ("ensh" . "{ensh}")
    ("esc" . "{esc}")
    ("f1" . "{f1}")
    ("f2" . "{f2}")
    ("f3" . "{f3}")
    ("f4" . "{f4}")
    ("f5" . "{f5}")
    ("f6" . "{f6}")
    ("f7" . "{f7}")
    ("f8" . "{f8}")
    ("grn" . "{grn}")
    ("gry1" . "{gry1}")
    ("gry2" . "{gry2}")
    ("gry3" . "{gry3}")
    ("home" . "{home}")
    ("inst" . "{inst}")
    ("lblue" . "{lblue}")
    ("left" . "{left}")
    ("een" . "reen}")
    ("lred" . "{lred}")
    ("null" . "{null}")
    ("orng" . "{orng}")
    ("pur" . "{pur}")
    ("red" . "{red}")
    ("rght" . "{rght}")
    ("rvof" . "{rvof}")
    ("rvon" . "{rvon}")
    ("SHIFT--" . "{SHIFT--}")
    ("SHIFT-@" . "{SHIFT-@}")
    ("SHIFT-*" . "{SHIFT-*}")
    ("SHIFT-+" . "{SHIFT-+}")
    ("SHIFT-POUND" . "{SHIFT-POUND}")
    ("space" . "{space}")
    ("stop" . "{stop}")
    ("stret" . "{stret}")
    ("swlc" . "{swlc}")
    ("swuc" . "{swuc}")
    ("up" . "{up}")
    ("wht" . "{wht}"))
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
                  "-ic" "-w65" "-o" output-file "--" tmp-src)))

;; (defun basic65-run-in-xemu ()
;;   "Export the current buffer to a PRG and run it in xemu."
;;   (interactive)
;;   (let ((prg-file (make-temp-file "basic65-" nil ".prg")))
;;     (basic65--buffer-to-prg prg-file)
;;     (message "Running %s in xemu..." prg-file)
;;     (start-process "basic65-xemu" "*basic65-xemu*"
;;                    basic65-xemu-command
;;                    "-prg" prg-file)))

(defun basic65-run-in-xemu ()
  "Export the current buffer to a PRG and run it in xemu."
  (interactive)
  ;; kill old process if still running
  (when-let ((old (get-process "basic65-xemu")))
    (delete-process old)
    (message "Killed old xemu process."))
  (let ((prg-file (make-temp-file "basic65-" nil ".prg")))
    (basic65--buffer-to-prg prg-file)
    (message "Running %s in xemu..." prg-file)
    (start-process "basic65-xemu" "*basic65-xemu*"
                   basic65-xemu-command
                   "-prg" prg-file)))

(defun basic65-view-prg ()
  "Öffnet die PRG-Datei des aktuellen BASIC65-Programms im hexl-mode, ohne sie neu zu erzeugen."
  (interactive)
  (let ((prg-file (concat (file-name-sans-extension (buffer-file-name)) ".prg")))
    (if (file-exists-p prg-file)
        (progn
          (find-file-other-window prg-file)
          (hexl-mode))
      (message "PRG-Datei nicht gefunden: %s" prg-file))))



(defun basic65-kill-xemu ()
  "Kill the running xemu process started by `basic65-run-in-xemu`."
  (interactive)
  (let ((proc (get-process "basic65-xemu")))
    (if proc
        (progn
          (delete-process proc)
          (message "Killed xemu process."))
      (message "No xemu process running."))))


;; ------------------------------
;; Minor mode definition
;; ------------------------------
(defvar basic65-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'basic65-run-in-xemu)  ;; Run in xemu
    (define-key map (kbd "C-c C-k") #'basic65-kill-xemu)    ;; Kill xemu
    map)
  "Keymap for `basic65-mode`.")


(define-minor-mode basic65-mode
  "Minor mode for editing MEGA65 BASIC code on top of `basic-mode`."
  :lighter " BASIC65"
  :keymap basic65-mode-map
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

;; ==============================
;; Corfu Setup (Inline Completion)
;; ==============================
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

;; ------------------------------
;; Helper command
;; ------------------------------

(defun basic65-setup ()
  "Start a buffer in BASIC mode with BASIC65 extensions."
  (interactive)
  (basic-mode)
  (basic65-mode 1)
  (font-lock-fontify-buffer))  ;; refresh highlighting

(defun basic65-buffer-font-retro ()
  "Set C64 Mono font for current buffer."
  (interactive)
  (face-remap-add-relative 'default :family "C64 Pro Mono" :height 120))

(defun basic65-toggle-buffer-font ()
  "Schaltet zwischen Retro-Font und ursprünglichem Font im aktuellen Buffer um."
  (interactive)
  (if buffer-font-remap-cookie
      ;; Font zurücksetzen
      (progn
        (face-remap-remove-relative buffer-font-remap-cookie)
        (setq buffer-font-remap-cookie nil)
        (when buffer-font-original
          (face-remap-add-relative 'default
                                   :family (plist-get buffer-font-original :family)
                                   :height (plist-get buffer-font-original :height)))
        (message "Ursprünglicher Font wiederhergestellt."))
    ;; Font setzen und Original speichern
    (setq buffer-font-original
          (list :family (face-attribute 'default :family)
                :height (face-attribute 'default :height)))
    (setq buffer-font-remap-cookie
          (face-remap-add-relative 'default :family "C64 Pro Mono" :height 110))
    (message "Retro-Font aktiviert.")))

;; (defun basic65-petscii-insert-code ()
;;   "Zeigt eine Auswahl von PETSCII-Codes und fügt das entsprechende Zeichen ein."
;;   (interactive)
;;   (let* ((choice (completing-read "PETSCII: " (mapcar #'car basic65-petcat-table)))
;;          (code (cdr (assoc choice basic65-petcat-table))))
;;     (insert code)))

(defun basic65-petscii-insert-code ()
  "Zeigt eine Auswahl von PETSCII-Codes oder erlaubt freie Eingabe."
  (interactive)
  (let* ((input (completing-read "PETSCII: " (mapcar #'car basic65-petcat-table) nil nil nil nil "{clr}"))
         (code (or (cdr (assoc input basic65-petcat-table))
                   (and (string-match-p "^{.*}$" input) input))))
    (if code
        (insert code)
      (message "Ungültiger PETSCII-Code: %s" input))))


(defun basic65-petscii-trigger ()
  "Fügt '{' ein und zeigt ggf. PETSCII-Auswahl, wenn innerhalb eines Strings."
  (interactive)
  (insert "{")
  (when (nth 3 (syntax-ppss)) ;; Prüft, ob Cursor innerhalb eines Strings ist
    (delete-char -1) ;; '{' wieder löschen
    (basic65-petscii-insert-code)))


;; (defun basic65-petscii-trigger ()
;;   "Prüft, ob '{' innerhalb eines Strings eingegeben wurde, und zeigt Auswahl."
;;   (interactive)
;;   (if (and (eq (char-before) ?\")
;;            (eq (char-after) ?{))
;;       (progn
;;         (delete-char 1) ;; '{' entfernen
;;         (basic65-petscii-insert-code))
;;     (insert "{")))

(define-key basic65-mode-map (kbd "{") #'basic65-petscii-trigger)



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bas65\\'" . basic65-setup))
(add-to-list 'auto-mode-alist '("\\.bas\\'"   . basic65-setup))




(provide 'basic65-mode)

;;; basic65-mode.el ends here
