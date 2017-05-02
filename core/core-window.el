(customize-set-variable 'window-divider-default-places t)
(customize-set-variable 'window-divider-default-bottom-width 1)
(customize-set-variable 'window-divider-default-right-width  1)
(window-divider-mode +1)

(setq max-mini-window-height 0.3
      resize-mini-windows    t)

;; splits, frames, windows
(setq split-width-threshold 130)

(defvar ivan-bottom-buffer-patterns
  '((and "*ag " (1+ not-newline) "*")
    "*ggtags-global*"
    "*Help*"
    "*Apropos*"
    "*rake-compilation*"
    "*Pp Eval Output*"))

(defvar ivan-bottom-or-reusable-window-patterns
  '("*rspec-compilation*"
    "*ert*"
    "*quickrun*"))

(defmacro ivan-bottom-buffer-regex ()
  `(rx bos (or ,@ivan-bottom-buffer-patterns) eos))

(defmacro ivan-bottom-or-reusable-window-regex ()
  `(rx bos (or ,@ivan-bottom-or-reusable-window-patterns) eos))

(setq display-buffer-alist
      `(
        ;; Put search results in bottom side-window of the current frame.
        (,(ivan-bottom-buffer-regex)
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (reusable-frames)
         (side . bottom)
         )
        ;; Put test results in reusable window/frame if one is visible,
        ;; otherwise put them in bottom side-window.
        (,(ivan-bottom-or-reusable-window-regex)
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (reusable-frames . visible)
         (inhibit-switch-frame . t)
         (side . bottom)
         )
        ))

(provide 'core-window)
