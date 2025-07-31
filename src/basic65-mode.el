;;; basic65-mode.el --- Major mode for editing BASIC 65 files on the MEGA65

;; Author: prake71
;; Version: 0.1
;; Keywords: languages, basic, MEGA65
;; Package-Requires: ()
;; URL: 

;;; Commentary:

;; Major mode for editing BASIC 65 code used on the MEGA65.
;; - Auto-uppercasing as you type
;; - BASIC 65 keyword highlighting
;; - Handles both REM and ' comments
;; - Highlights line numbers

;;; Code:

(defvar basic65-mode-hook nil)

(defvar basic65-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; ' starts a comment
    (modify-syntax-entry ?' "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defconst basic65-keywords
  '("IF" "THEN" "ELSE" "FOR" "TO" "STEP" "NEXT" "GOTO" "GOSUB"
    "RETURN" "END" "STOP" "REM" "PRINT" "INPUT" "LET" "DIM"
    "READ" "DATA" "RESTORE" "ON" "DEF" "POKE" "PEEK"
    "SYS" "NEW" "RUN" "LIST" "OPEN" "CLOSE"))

(defconst basic65-builtins
  '("ABS" "CHR$" "ASC" "LEFT$" "RIGHT$" "MID$" "INT" "RND"
    "LEN" "VAL" "STR$" "NOT" "AND" "OR" "XOR"))

(defvar basic65-font-lock-keywords
  `((,(regexp-opt basic65-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt basic65-builtins 'words) . font-lock-builtin-face)
    ("\\([0-9]+\\)\\s-+" . font-lock-constant-face)
    ("'[^'\n]*" . font-lock-comment-face)
    ("REM.*$" . font-lock-comment-face)))

(defun basic65-force-uppercase (beg end _len)
  "Convert inserted text to uppercase in BASIC 65 mode."
  (when (and (eq major-mode 'basic65-mode)
             (not undo-in-progress))
    (let ((inhibit-modification-hooks t))
      (save-excursion
        (upcase-region beg end)))))

(defun basic65-enable-uppercase ()
  "Enable auto-uppercase behavior in BASIC 65 mode."
  (add-hook 'after-change-functions #'basic65-force-uppercase nil t))

;;;###autoload
(define-derived-mode basic65-mode fundamental-mode "BASIC65"
  "Major mode for editing BASIC 65 code for the MEGA65."
  :syntax-table basic65-mode-syntax-table
  (setq font-lock-defaults '((basic65-font-lock-keywords)))
  (setq comment-start "'")
  (setq comment-end "")
  (basic65-enable-uppercase))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bas65\\'" . basic65-mode))

(provide 'basic65-mode)

;;; basic65-mode.el ends here
