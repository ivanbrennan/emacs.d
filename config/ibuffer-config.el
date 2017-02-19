(bind-map-for-mode-inherit ivan/ibuffer-leader-map ivan/leader-map
  :major-modes (ibuffer-mode)
  :bindings
  ("/" (lookup-key ibuffer-mode-map "/")))

(evil-set-initial-state 'ibuffer-mode 'motion)

(evil-define-key 'motion ibuffer-mode-map
  (kbd "t") 'ibuffer-toggle-marks
  (kbd "* *") 'ibuffer-unmark-all
  (kbd "* M") 'ibuffer-mark-by-mode
  (kbd "* m") 'ibuffer-mark-modified-buffers
  (kbd "* u") 'ibuffer-mark-unsaved-buffers
  (kbd "* s") 'ibuffer-mark-special-buffers
  (kbd "* r") 'ibuffer-mark-read-only-buffers
  (kbd "* /") 'ibuffer-mark-dired-buffers
  (kbd "* e") 'ibuffer-mark-dissociated-buffers
  (kbd "* h") 'ibuffer-mark-help-buffers
  (kbd "* z") 'ibuffer-mark-compressed-file-buffers

  (kbd "C-d") 'ibuffer-mark-for-delete-backwards

  ;; immediate operations
  (kbd "g") 'ibuffer-update
  "`" 'ibuffer-switch-format
  "-" 'ibuffer-add-to-tmp-hide
  "+" 'ibuffer-add-to-tmp-show
  "b" 'ibuffer-bury-buffer
  (kbd ",") 'ibuffer-toggle-sorting-mode

  "\t" 'ibuffer-forward-filter-group
  (kbd "p") 'ibuffer-yank

  (kbd "% n") 'ibuffer-mark-by-name-regexp
  (kbd "% m") 'ibuffer-mark-by-mode-regexp
  (kbd "% f") 'ibuffer-mark-by-file-name-regexp

  (kbd "|") 'ibuffer-do-shell-command-pipe
  (kbd "!") 'ibuffer-do-shell-command-file
  ;; marked operations
  (kbd "E") 'ibuffer-do-eval
  (kbd "F") 'ibuffer-do-shell-command-file
  (kbd "H") 'ibuffer-do-view-other-frame
  (kbd "J") 'ibuffer-jump-to-buffer
  (kbd "N") 'ibuffer-do-shell-command-pipe-replace
  (kbd "M") 'ibuffer-do-toggle-modified
  (kbd "T") 'ibuffer-do-toggle-read-only
  (kbd "V") 'ibuffer-do-revert
  (kbd "W") 'ibuffer-do-view-and-eval

  (kbd "K") 'ibuffer-do-kill-lines
  (kbd "w") 'ibuffer-copy-filename-as-kill

  (kbd "e") 'ibuffer-visit-buffer
  (kbd "f") 'ibuffer-visit-buffer
  (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
  (kbd "v") 'ibuffer-do-view
)

(provide 'ibuffer-config)
