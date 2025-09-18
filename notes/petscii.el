(defvar basic65-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'basic65-run-in-xemu)  ;; Run in xemu
    (define-key map (kbd "C-c C-k") #'basic65-kill-xemu)    ;; Kill xemu
    map)
  "Keymap for `basic65-mode`.")


(defvar petscii-code-alist
  '(("clr" . "\x93")
    ("home" . "\x13")
    ("shift-q" . "\x91")
    ("reverse-on" . "\x12")
    ("reverse-off" . "\x92"))
  "Alist von PETSCII-Codes und ihren Zeichen.")

(defun petscii-insert-code ()
  "Zeigt eine Auswahl von PETSCII-Codes und fügt das entsprechende Zeichen ein."
  (interactive)
  (let* ((choice (completing-read "PETSCII: " (mapcar #'car petscii-code-alist)))
         (code (cdr (assoc choice petscii-code-alist))))
    (insert code)))

(defun petscii-trigger ()
  "Prüft, ob '{' innerhalb eines Strings eingegeben wurde, und zeigt Auswahl."
  (interactive)
  (if (and (eq (char-before) ?\")
           (eq (char-after) ?{))
      (progn
        (delete-char 1) ;; '{' entfernen
        (petscii-insert-code))
    (insert "{")))

(define-key basic65-mode-map (kbd "{") #'petscii-trigger)
