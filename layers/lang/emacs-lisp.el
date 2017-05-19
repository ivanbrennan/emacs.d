(defun ivan-sort-setq ()
  (interactive)
  (let ((orig-line (line-number-at-pos))
        inserted-newline-at-beg
        inserted-newline-at-end)
    (beginning-of-defun)
    (save-match-data
      (re-search-forward "\\>")
      (unless (looking-at "\\s *$")
        (insert "\n")
        (setq inserted-newline-at-beg t))
      (beginning-of-defun)
      (next-line)
      (setq inner-beg (point))
      (end-of-defun)
      (backward-char 2)
      (unless (looking-back "^\\s *" (point-at-bol))
        (insert "\n")
        (setq inserted-newline-at-end t))
      (move-beginning-of-line nil)
      (backward-char)
      (setq inner-end (point))
      (indent-region inner-beg (point))
      (sort-lines nil inner-beg (point))
      (when inserted-newline-at-beg
        (beginning-of-defun)
        (join-line 'following-line))
      (when inserted-newline-at-end
        (end-of-defun)
        (previous-line)
        (join-line))
      (beginning-of-defun)
      (indent-sexp))
    (goto-line orig-line)
    (back-to-indentation)))

(with-eval-after-load
 'bind-map
 (bind-map-for-mode-inherit ivan-lisp-leader-map ivan-leader-map
   :major-modes (emacs-lisp-mode)
   :bindings
   ("m s" #'ivan-sort-setq)))
