(customize-set-variable 'window-divider-default-places t)
(customize-set-variable 'window-divider-default-bottom-width 1)
(customize-set-variable 'window-divider-default-right-width  1)
(window-divider-mode +1)

(setq max-mini-window-height 0.3
      resize-mini-windows    t)

;; splits, frames, windows
(setq split-width-threshold 130)
(setq
 display-buffer-alist
 `(
   ;; Put search results in bottom side-window of the current frame.
   (,(rx bos
         (or
          (and "*ag " (1+ not-newline) "*")
          "*ggtags-global*"
          "*Help*"
          "*Apropos*"
          "*rake-compilation*"
          "*Pp Eval Output*"
          )
         eos)
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (reusable-frames)
    (side . bottom)
    )
   ;; Put test results in reusable window/frame if one is visible,
   ;; otherwise put them in bottom side-window.
   (,(rx bos
         (or
          "*rspec-compilation*"
          "*ert*"
          "*quickrun*"
          )
         eos)
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (reusable-frames . visible)
    (inhibit-switch-frame . t)
    (side . bottom)
    )
   ))

(provide 'core-window)
