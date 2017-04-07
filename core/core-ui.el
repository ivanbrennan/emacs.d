(setq inhibit-startup-screen t
      initial-scratch-message nil)

(menu-bar-mode 0)
(tool-bar-mode 0)

(when (display-graphic-p)
  (scroll-bar-mode 0)
  (tooltip-mode    0)
  (setq frame-title-format "%b"))

(setq-default
 fringes-outside-margins t
 fringe-indicator-alist  (assq-delete-all
                          'truncation
                          (assq-delete-all
                           'continuation
                           fringe-indicator-alist)))

(blink-cursor-mode 0)

(setq-default
 cursor-type 'bar
 cursor-in-non-selected-windows nil)

(customize-set-variable
 'minibuffer-prompt-properties
 '(read-only         t
   cursor-intangible t
   face              minibuffer-prompt))

(setq
 confirm-kill-emacs          (lambda (_) (y-or-n-p "››› Quit?"))
 max-mini-window-height      0.3
 mode-line-default-help-echo nil
 resize-mini-windows         t
 show-help-function          nil
 use-dialog-box              nil
 visible-cursor              nil)

(provide 'core-ui)
