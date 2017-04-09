(setq recenter-positions        '(top middle bottom)
      scroll-step               1
      scroll-margin             1
      hscroll-step              1
      hscroll-margin            2
      scroll-conservatively     200
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1))
      isearch-allow-scroll      t)

(add-hook 'compilation-mode-hook
          (lambda () (setq-local scroll-margin 0)))

(defun ivan-scroll-right ()
  (interactive)
  (scroll-right 2))

(defun ivan-scroll-left ()
  (interactive)
  (scroll-left 2))

(defvar ivan-hscroll-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-wheel-right] #'ivan-scroll-left)
    (define-key map [M-wheel-right] #'ivan-scroll-left)
    (define-key map [C-wheel-left]  #'ivan-scroll-right)
    (define-key map [M-wheel-left]  #'ivan-scroll-right)
    map)
  "ivan-hscroll-minor-mode keymap.")

(define-minor-mode ivan-hscroll-minor-mode
  "A minor mode so my horizontal scroll bindings take precedence."
  :init-value t)

(ivan-hscroll-minor-mode +1)

(put 'mac-mwheel-scroll 'isearch-scroll t)
(put 'ivan-scroll-right 'isearch-scroll t)
(put 'ivan-scroll-left  'isearch-scroll t)
(put 'hl-line-mode      'isearch-scroll t)

(provide 'core-scroll)
