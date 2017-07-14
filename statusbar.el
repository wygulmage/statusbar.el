;;; statusbar.el --- simple clean statusbar -*- lexical-binding: t -*-
(mapc #'require
      [fac hook-up primary-pane miscellaneous])

(setq-default statusbar-layout mode-line-format)

;;; TODO: Make active mode line inactive on mouse focus loss.

;;; Notes:
;; With `mode-line-format' = nil, things look best with `bottom-divider-width' = 1; the vertical `window-divider' is automatically full height.
;; With only one pane ('window'), things look best with `mode-line' :box nil and :overline and `window-divider' as whatever the box was.

;; For a B&W terminal, the best we can do is make the active mode-line invert gamma and the inactive mode-lines regular gamma. Then we only need a box around inactive mode lines.

;; For a grayscale terminal

;;; Faces

(fac-def-adaptive-faces 'statusbar
  '(default
     "an alias af mode-line and mode-line-inactive faces"
     (:inherit mode-line)
     (:inherit mode-line-inactive))
  '(highlight
    "an emphasized face for the mode-line"
    (:weight bold
             :underline t
             :inherit statusbar-default-active)
    (:weight bold
             :underline t
             :inherit statusbar-default-inactive)
    fac-intensify-foreground)
  '(shadow
    "a dimmed face for the mode-line"
    (:inherit statusbar-default-active)
    (:inherit statusbar-default-inactive)
    fac-fade-foreground))

(defun statusbar--normalize-box (FACE)
  "The :box property of FACE as a list."
  (pcase (face-attribute FACE :box)
    ('nil nil)
    ('t `(:color ,(face-attribute 'default :color)
                 :line-width 1))
    ((and (pred consp) x) x)
    (x `(:color ,x :line-width 1))))

(defun statusbar--‘box’ ()
  "Transfer the :box attribute of the mode line into an :overline and a `bottom-divider'. Unfortunately, because the mode-line does not show left or right dividers, this does not look great with side-by-side panes."
  (when-let
      ((box (statusbar--normalize-box 'mode-line))
       (color (plist-get box :color))
       (width (plist-get box :line-width)))
    (fac-set-faces-attributes
     [mode-line mode-line-inactive]
     :box nil
     :overline color)
    (fac-set-faces-attributes
     [window-divider]
     :foreground color
     :background color)
    (set-frame-parameter nil 'bottom-divider-width width)))

;;; Buffer info

;; (defvar-local statusbar--buffer-line-count nil)
(defun statusbar--buffer-line-count (&rest _) ; `after-change-functions' passes args.
  "Number of lines in the current buffer. If the last line of the buffer is empty, it won't be counted."
  ;; (setq statusbar--buffer-line-count
  (count-lines (point-min) (point-max)))
;; statusbar--buffer-line-count)

;; (hook-up
;;  [buffer-list-update-hook
;;   after-change-functions]
;;  [statusbar--buffer-line-count])

(defun statusbar--buffer-file-like-p ()
  "Is the buffer visiting something that should be a file?"
  (or buffer-file-name
      (derived-mode-p 'prog-mode 'text-mode)))

(defun statusbar--buffer-file-path ()
  "The file path if the current buffer is a file, otherwise nil."
  (and buffer-file-truename (abbreviate-file-name buffer-file-truename)))

(defun statusbar-buffer-name ()
  "The name of the buffer. If it's a file, show the directory on hover and open dired with a click."
  (if buffer-file-truename
      (propertize
       (buffer-name)
       'help-echo (abbreviate-file-name buffer-file-truename)
       'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive)
                              (dired (file-name-directory buffer-file-truename)))))
    (buffer-name)))

(defun statusbar-primary-file-or-buffer-name ()
  "The name of the file or buffer in the primary pane."
  (let ((b (window-buffer primary-pane)))
    (or (statusbar--buffer-file-path b)
        (buffer-name b))))

(defun statusbar-major-mode-name ()
  "The buffer's major-mode"
  (propertize
   mode-name
   'help-echo "Click mouse 1 for mode menu, mouse 2 for mode info, or mouse 3 to toggle minor modes."
   'local-map mode-line-major-mode-keymap))

(defun statusbar-buffer-write-status-string ()
  "Show whether a file-like buffer has been modified since its last save; click to save."
  (if (not (statusbar--buffer-file-like-p))
      " " ; Ignore buffers that aren't files.
    (misc--pad 1 (propertize
                  (concat (when (buffer-modified-p) "◆")
                          (when buffer-read-only "🔒"))
                  'help-echo
                  (concat (when (buffer-modified-p) "modified ")
                          (when buffer-read-only "read-only ")
                          (if buffer-file-name "file " "buffer ")
                          "‑ click to save")
                  'local-map (make-mode-line-mouse-map 'mouse-1 #'save-buffer)))))


;; (defvar-local statusbar--file-vc-status nil
;;   "The version-control status of the current file.")
;; (defun statusbar--file-vc-status ()
;;   "Get and set the version-control status of the file visited by the current buffer."
;;   (let ((f (statusbar--buffer-file-path)))
;;     (setq statusbar--file-vc-status (and f (vc-state f)))
;;     statusbar--file-vc-status))

;; (hook-up
;;  [after-save-hook
;;   find-file-hook
;;   first-change-hook]
;;  [statusbar--file-vc-status])

;; (defun statusbar-file-vc-status-string ()
;;   "A string that represents the VC status of the file visited by the current buffer."
;;   (pcase statusbar--file-vc-status
;;     (`up-to-date "")
;;     (`ignored "")
;;     (`edited "◆ ")
;;     (`needs-update "U ")
;;     (`needs-merge "M ")
;;     (`added "+ ")
;;     (`removed "- ")
;;     (`conflict "! ")
;;     (`missing "? ")
;;     (_ nil))) ; Let me know if I'm missing a state.

(defun statusbar-vc-branch-string ()
  (if (not vc-mode)
      ""
    (concat
     (propertize "(" 'face (statusbar-shadow))
     (propertize
      (concat
       ;; (statusbar-file-vc-status-string)
       (replace-regexp-in-string " Git[:\-]" "" vc-mode))
      'mouse-face (statusbar-default)
      'local-map (make-mode-line-mouse-map 'mouse-1 #'magit-status))
     (propertize ")" 'face (statusbar-shadow)))))


(defun statusbar-line-position-string ()
  "Current line / total lines. Click to toggle line numbers."
  (let ((lines (number-to-string (statusbar--buffer-line-count))))
    (propertize
     (concat
      (misc--pad (length lines) (format-mode-line "%l"))
      (propertize "/" 'face (statusbar-shadow))
      lines)
     'help-echo (if (bound-and-true-p linum-mode)
                    "Hide line numbers."
                  "Show line numbers.")
     'local-map (make-mode-line-mouse-map 'mouse-1 #'linum-mode))))

(if (fboundp 'winum-get-number-string)
    (defun statusbar-window-number-string ()
      "The window's number if we can get it."
      (when (cdr (window-list nil 0))
        (concat
         ;; (propertize "[" 'face (statusbar-shadow))
         (winum-get-number-string)
         " ")))
  ;; (propertize "]" 'face (statusbar-shadow)))))
  (defun statusbar-window-number-string () "Empty string" ""))


;;; Utility procedures

(defun statusbar--exact-width-string (WIDTH STRING)
  "If WIDTH is greater than the width of STRING, pad string with zeros on the right.
If WIDTH is less than the width of STRING, truncate STRING with an ellipsis.
Otherwise return STRING."
  (let ((initial-width (string-width STRING)))
    (cond ((= WIDTH initial-width)
           STRING)
          ((< WIDTH initial-width)
           (concat (substring STRING 0 (- WIDTH 1))
                   "…")) ; more reliable than `truncate-string-ellipsis'
          (t (misc--pad (- WIDTH) STRING)))))

(defun statusbar--concat-when-first (&rest STRINGS)
  (when STRINGS
    (if (or (null (car STRINGS))
            (string= "" (car STRINGS)))
        ""
      (apply #'concat STRINGS))))

  ;;; TODO: Create shortened mode-line faces for a collapsed but visible mode line.


;;; Layouts

(defun statusbar (LEFT &optional RIGHT)
  ;; There are two ways I could do this: with strings or with mode-line constructs. Doing it with strings is probably more efficient, but may be less amenable to reuse.
  (let ((left-string (format-mode-line LEFT)))
    (if (or (not RIGHT) (eq RIGHT ""))
        left-string
      (let ((right-string (format-mode-line RIGHT)))
        (concat (statusbar--exact-width-string (- (window-total-width)
                                                  (string-width right-string))
                                               (format-mode-line LEFT))
                right-string)))))

(defun statusbar-hide ()
  (setq mode-line-format ()))

(defvar statusbar-base-left
  '(:eval
    (concat
     (statusbar-buffer-write-status-string)
     (statusbar-buffer-name)
     " "
     (statusbar-vc-branch-string)
     "  "
     (statusbar-line-position-string))))

(defvar statusbar-base-right
  '(:eval
    (statusbar-window-number-string)))

(defvar statusbar-base-layout
  '(:eval
    (statusbar
     statusbar-base-left
     statusbar-base-right))
  "a simple status bar")

(defun statusbar-use-base-layout ()
  (setq mode-line-format statusbar-base-layout
        statusbar-layout statusbar-base-layout))

(defvar statusbar-prog-mode-layout
  '(:eval
    (statusbar
     `(,statusbar-base-left
       (:eval
        (concat
         "  "
         (statusbar-major-mode-name))))
     `((:eval
        (concat
         (when (bound-and-true-p anzu-mode) (anzu--update-mode-line))
         " "))
       ,statusbar-base-right))))

;; (list
;;  statusbar-base-layout
;;  '(:eval
;;    (concat
;;     "  "
;;     (statusbar-major-mode-name)
;;     "  "
;;     (when (bound-and-true-p anzu-mode) (anzu--update-mode-line))
;;     )))
;; "simple status bar that indicates the current mode")

(defun statusbar-use-prog-mode-layout ()
  (setq mode-line-format statusbar-prog-mode-layout
        statusbar-layout statusbar-prog-mode-layout))

(defun statusbar-use-echo-area ()
  "Use the echo area instead of the mode line. This disables all key maps, of course, since the echo area doesn't have a key map."
  (let ((message-log-max nil))
    (setq mode-line-format nil)
    (message "%s" (format-mode-line statusbar-layout))))

(defun statusbar-set-frame-title ()
  (when (display-graphic-p)
    (setq frame-title-format
          '(:eval (concat
                   (statusbar-buffer-write-status-string)
                   " "
                   (statusbar-primary-file-or-buffer-name)
                   )))))

(provide 'statusbar)
