(setq
 whitespace-line-column 90
 whitespace-style '(
                    empty
                    face
                    indentation
                    lines-tail
                    space-after-tab
                    space-before-tab
                    trailing
                    )
 )

(defun ivan-code-whitespace ()
  (setq-default indent-tabs-mode nil)
  (setq indicate-empty-lines     t
        show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'ivan-code-whitespace)
(defun ivan-trim-whitespace-relative-line (count)
  (save-excursion
    (next-line count)
    (delete-trailing-whitespace (line-beginning-position)
                                (line-end-position))))

  (defun ivan-trim-whitespace-current-line (&rest _args)
    (ivan-trim-whitespace-relative-line 0))

  (defun ivan-trim-whitespace-next-line (&rest _args)
    (ivan-trim-whitespace-relative-line 1))

(defvar no-space-before-regexp "^\\|[])]")
(defvar no-space-after-regexp  "$\\|[[(]\\|\\s'")

(defun fixup-no-space? ()
  (or (looking-at no-space-before-regexp)
      (save-excursion (forward-char -1)
                      (looking-at no-space-after-regexp))))

(defun ivan-fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (unless (fixup-no-space?)
      (insert ?\s))))
(advice-add 'fixup-whitespace :override #'ivan-fixup-whitespace)

(provide 'core-whitespace)
