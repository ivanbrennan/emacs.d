(setq hscroll-margin                  2
      hscroll-step                    1
      isearch-allow-scroll            t
      mouse-wheel-scroll-amount       '(0.01 ((shift) . 1))
      recenter-positions              '(middle top bottom)
      scroll-conservatively           200
      scroll-margin                   1
      scroll-preserve-screen-position t
      scroll-step                     1)

(defun ivan-recenter-top-bottom (&optional arg)
  (interactive "P")
  (recenter-top-bottom (if (equal arg '(4)) scroll-margin arg)))
(global-set-key (kbd "C-l") #'ivan-recenter-top-bottom)

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
