(setq inhibit-startup-screen  t
      initial-scratch-message nil)

(menu-bar-mode 0)
(tool-bar-mode 0)

(when (display-graphic-p)
  (scroll-bar-mode 0)
  (tooltip-mode    0)
  (setq frame-title-format "%b"))

(setq-default fringes-outside-margins t
              fringe-indicator-alist (assq-delete-all
                                      'truncation
                                      (assq-delete-all
                                       'continuation
                                       fringe-indicator-alist))

              cursor-in-non-selected-windows nil
              cursor-type 'bar)

(blink-cursor-mode 0)

(setq minibuffer-prompt-properties
      '(read-only         t
        cursor-intangible t
        face              minibuffer-prompt))

(setq confirm-kill-emacs          (lambda (_) (y-or-n-p "››› Quit?"))
      mode-line-default-help-echo nil
      show-help-function          nil
      use-dialog-box              nil
      visible-cursor              nil)

(provide 'core-ui)
