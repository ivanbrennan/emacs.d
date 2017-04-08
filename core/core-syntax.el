(setq-default jit-lock-stealth-nice 0.1
              jit-lock-stealth-time 0.2)

(defface font-lock-todo-face
  '((t (:inherit error :slant normal)))
    "Face for TODO items.")

(defface font-lock-note-face
  '((t (:inherit success :slant normal)))
    "Face for NOTE items.")

(defface font-lock-colon-face
  '((t (:inherit font-lock-comment-face :slant normal)))
    "Face for colon following TODO/NOTE items.")

(defun add-todo-and-note-keywords ()
  (let ((todo "\\(\\<TODO\\>\\)\\(?:[ \t]*\\)\\(:?\\)")
        (note "\\(\\<NOTE\\>\\)\\(?:[ \t]*\\)\\(:?\\)"))
    (font-lock-add-keywords
     nil
     `((,todo (1 'font-lock-todo-face  prepend)
              (2 'font-lock-colon-face prepend))
       (,note (1 'font-lock-note-face  prepend)
              (2 'font-lock-colon-face prepend))))))

(dolist (hook '(prog-mode-hook
                emacs-lisp-mode-hook
                css-mode-hook))
  (add-hook hook #'add-todo-and-note-keywords))

(provide 'core-syntax)
