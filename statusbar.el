;;; statusbar.el --- simple clean statusbar -*- lexical-binding: t -*-
(mapc #'require
      [fac hook-up primary-pane miscellaneous])

(setq-default statusbar-layout mode-line-format)

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
      "" ; Ignore buffers that aren't files.
    (misc--pad 1 (propertize
                  (concat (when (buffer-modified-p) "◆")
                          (when buffer-read-only "🔒"))
                  'help-echo
                  (concat (when (buffer-modified-p) "modified ")
                          (when buffer-read-only "read-only ")
                          (if buffer-file-name "file " "buffer ")
                          "‑ click to save")
                  'local-map (make-mode-line-mouse-map 'mouse-1 #'save-buffer)))))


(defvar-local statusbar--file-vc-status nil
  "The version-control status of the current file.")
(defun statusbar--file-vc-status ()
  "Get and set the version-control status of the file visited by the current buffer."
  (let ((f (statusbar--buffer-file-path)))
    (setq statusbar--file-vc-status (and f (vc-state f)))
    statusbar--file-vc-status))

(hook-up
 [after-save-hook
  find-file-hook
  first-change-hook]
 [statusbar--file-vc-status])

(defun statusbar-file-vc-status-string ()
  "A string that represents the VC status of the file visited by the current buffer."
  (pcase statusbar--file-vc-status
    (`up-to-date "")
    (`ignored "")
    (`edited "◆ ")
    (`needs-update "U ")
    (`needs-merge "M ")
    (`added "+ ")
    (`removed "- ")
    (`conflict "! ")
    (`missing "? ")
    (_ nil))) ; Let me know if I'm missing a state.

(defun statusbar-vc-branch-string ()
  (if (not vc-mode)
      ""
    (concat
     (propertize "(" 'face (statusbar-shadow))
     (propertize
      (concat
       (statusbar-file-vc-status-string)
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
         (propertize "[" 'face (statusbar-shadow))
         (winum-get-number-string)
         (propertize "]" 'face (statusbar-shadow)))))
  (defun statusbar-window-number-string () "Empty string" ""))


;;; Utility procedures

(defun concat-when-first (&rest STRINGS)
  (when STRINGS
    (if (or (null (car STRINGS))
            (string= "" (car STRINGS)))
        ""
      (apply #'concat STRINGS))))

  ;;; TODO: Create shortened mode-line faces for a collapsed but visible mode line.


;;; Layouts

(defun statusbar-hide ()
  (setq mode-line-format ()))

(defvar statusbar-base-layout
  '(:eval
    (concat
     (statusbar-window-number-string)
     " "
     (statusbar-buffer-write-status-string)
     (statusbar-buffer-name)
     " "
     (statusbar-vc-branch-string)
     "  "
     (statusbar-line-position-string)
     ))
  "a simple status bar")

(defun statusbar-use-base-layout ()
  (setq mode-line-format statusbar-base-layout
        statusbar-layout statusbar-base-layout))

(defvar statusbar-prog-mode-layout
  (list
   statusbar-base-layout
   '(:eval
     (concat
      "  "
      (statusbar-major-mode-name)
      "  "
      (when (bound-and-true-p anzu-mode) (anzu--update-mode-line))
      )))
  "simple status bar that indicates the current mode")

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
