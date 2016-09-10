;;; elixir-dark-theme.el
;;; Code:

(deftheme elixir-dark "Elixir Dark color theme")

;;; Color Palette

(defvar elixir-dark-default-colors-alist
  '(("navyblue"  . "#183691")
    ("blooo"     . "#5E79C1")
    ("bigblue"   . "#3873C3")
    ("marine"    . "#0085B3")
    ("flatblue"  . "#B0CDE7")
    ("aquablue"  . "#C2E7F3")
    ("aqua"      . "#DEFDFE")
    ("skyblue"   . "#E4F1F7")
    ("bluegrey"  . "#ECF3F7")
    ("fluff"     . "#F4F7FB")
    ("paleblue"  . "#F1F9FC")
    ("spruce"    . "#63A35C")
    ("greentea"  . "#29D045")
    ("leaf"      . "#83FCA7")
    ("melon"     . "#E4FFEA")
    ("limeclay"  . "#F2F9DB")
    ("sand"      . "#F8EEC7")
    ("clay"      . "#F3E5CA")
    ("parchment" . "#FEF9EC")
    ("bloodred"  . "#FF4421")
    ("closedred" . "#BD3128")
    ("marooned"  . "#A71D5D")
    ("merged"    . "#443A8C")
    ("eraser"    . "#FFC8C9")
    ("palepink"  . "#FFEBEB")
    ("ink"       . "#333333")
    ("nicegrey"  . "#393E53") ;; nice background for a dark theme
    ("shale"     . "#555555")
    ("slate"     . "#666666")
    ("pencil1"   . "#969896")
    ("pencil2"   . "#B3B3B3")
    ("pencil3"   . "#D8D8D8")
    ("whisp1"    . "#EDEDED")
    ("whisp2"    . "#F5F5F5")
    ("whisp3"    . "#F8F8F8")
    ("paper"     . "#F7F9FC"))
  "List of Elixir Dark colors.
Each element has the form (NAME . HEX).")

(defvar elixir-dark-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar elixir-dark-colors-alist
  (append elixir-dark-default-colors-alist elixir-dark-override-colors-alist))

(defmacro elixir-dark-with-color-variables (&rest body)
  "`let' bind all colors defined in `elixir-dark-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   elixir-dark-colors-alist))
     ,@body))

;;; Theme Faces
(elixir-dark-with-color-variables
  (custom-theme-set-faces
   'elixir-dark
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,bigblue :underline t :weight bold))))
   `(link-visited ((t (:foreground "white" :underline t :weight normal))))
   `(default ((t (:family "Source Code Pro" :height 140 :foreground "white" :background ,nicegrey))))
   `(fixed-pitch ((t (:family "Source Code Pro"))))
   `(variable-pitch ((t (:family "Avenir Next" :height 170))))
   `(cursor ((t (:background ,paper))))
   `(escape-glyph ((t (:foreground ,marooned :bold t))))
   `(fringe ((t (:foreground ,pencil2 :background ,nicegrey))))
   `(header-line ((t (:foreground ,pencil1 :background ,whisp2))))
   `(info-node ((t (:foreground ,greentea :slant italic :weight bold))))
   `(highlight ((t (:background ,whisp3))))
   `(success ((t (:foreground ,pencil1 :weight bold))))
   `(warning ((t (:foreground "white" :weight bold))))
   `(tooltip ((t (:foreground ,marooned :background ,nicegrey))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,marooned))))
   `(compilation-enter-directory-face ((t (:foreground ,pencil1))))
   `(compilation-error-face ((t (:foreground "white" :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,marooned))))
   `(compilation-info-face ((t (:foreground "white"))))
   `(compilation-info ((t (:foreground ,marine :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,pencil1))))
   `(compilation-line-face ((t (:foreground ,marooned))))
   `(compilation-line-number ((t (:foreground ,marooned))))
   `(compilation-message-face ((t (:foreground "white"))))
   `(compilation-warning-face ((t (:foreground "white" :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,pencil1 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,navyblue :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,marooned :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground "white"))))
;;;;; grep
   `(match ((t (:background ,whisp2 :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,ink :weight bold :background ,skyblue))))
   `(isearch-fail ((t (:background ,eraser))))
   `(lazy-highlight ((t (:foreground ,ink :weight bold :background ,limeclay))))

   `(menu ((t (:foreground ,marooned :background ,nicegrey))))
   `(minibuffer-prompt ((t (:foreground ,ink :weight bold))))
   `(mode-line
     ((,class (:foreground ,slate
                           :background ,bluegrey ;; homerow background
                           :box (:line-width -1 :color ,aquablue)
                           ))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground "#000000" :weight bold)))) ;; todo: not happy with black here
   `(mode-line-highlight ((t (:foreground ,bigblue))))
   `(mode-line-inactive
     ((t (:foreground ,pencil1
                      :weight light
                      :background ,whisp2
                      :box (:line-width -1 :color "#d8d8d8")))))
   `(region ((,class (:background "#E8F1F6"))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,nicegrey))))
   `(trailing-whitespace ((t (:background ,whisp1))))
   `(vertical-border ((t (:foreground "#d0d0d0"))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,marooned :weight normal))))  ;; weight bold (built-in keywords)
   `(font-lock-comment-face ((t (:foreground ,pencil1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,pencil1))))
   `(font-lock-constant-face ((t (:foreground ,marine))))
   `(font-lock-doc-face ((t (:foreground ,pencil1))))
   `(font-lock-function-name-face ((t (:foreground "#7265A2"))))
   `(font-lock-keyword-face ((t (:foreground ,marooned :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,marooned :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,marooned))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,marooned :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,pencil1 :weight bold))))
   `(font-lock-string-face ((t (:foreground ,flatblue))))
   `(font-lock-type-face ((t (:foreground "#0086b0"))))
   `(font-lock-variable-name-face ((t (:foreground "white"))))
   `(font-lock-warning-face ((t (:foreground "white" :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,marooned))))
   `(newsticker-default-face ((t (:foreground ,marooned))))
   `(newsticker-enclosure-face ((t (:foreground ,spruce))))
   `(newsticker-extra-face ((t (:foreground ,nicegrey :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,marooned))))
   `(newsticker-immortal-item-face ((t (:foreground ,pencil1))))
   `(newsticker-new-item-face ((t (:foreground "white"))))
   `(newsticker-obsolete-item-face ((t (:foreground ,navyblue))))
   `(newsticker-old-item-face ((t (:foreground ,nicegrey))))
   `(newsticker-statistics-face ((t (:foreground ,marooned))))
   `(newsticker-treeview-face ((t (:foreground ,marooned))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,pencil1))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,marooned))))
   `(newsticker-treeview-new-face ((t (:foreground "white" :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,navyblue))))
   `(newsticker-treeview-old-face ((t (:foreground ,nicegrey))))
   `(newsticker-treeview-selection-face ((t (:background ,flatblue :foreground ,marooned))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground "white" :background ,nicegrey :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,pencil1 :background ,nicegrey :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground "white"))))
   `(android-mode-error-face ((t (:foreground "white" :weight bold))))
   `(android-mode-info-face ((t (:foreground ,marooned))))
   `(android-mode-verbose-face ((t (:foreground ,pencil1))))
   `(android-mode-warning-face ((t (:foreground ,marooned))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,merged :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,navyblue :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,marooned))))
   `(font-latex-italic-face ((t (:foreground ,merged :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground "white"))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,marooned :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,navyblue))))
   `(agda2-highlight-symbol-face ((t (:foreground "white"))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,marine))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,marooned))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,marooned))))
   `(agda2-highlight-datatype-face ((t (:foreground "white"))))
   `(agda2-highlight-function-face ((t (:foreground "white"))))
   `(agda2-highlight-module-face ((t (:foreground ,marine))))
   `(agda2-highlight-error-face ((t (:foreground ,nicegrey :background "white"))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,nicegrey :background "white"))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,nicegrey :background "white"))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,nicegrey :background "white"))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,nicegrey :background "white"))))
   `(agda2-highlight-typechecks-face ((t (:background "white"))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background "#ffffff" :foreground ,ink :box (:line-width -1 :style released-button)))))
   `(ac-selection-face ((t (:background ,marooned :foreground "#ffffff" :box (:line-width -1 :style released-button)))))
   `(popup-tip-face ((t (:background "#ff0000" :foreground "#00ff00" ))))
   `(popup-scroll-bar-foreground-face ((t (:background "white"))))
   `(popup-scroll-bar-background-face ((t (:background "#ffffff"))))
   `(popup-isearch-match ((t (:background "#ff00ff" :foreground "#00ffff"))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground "white" :background ,nicegrey :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,spruce :background ,nicegrey :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,marooned :background ,nicegrey :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground "white" :background ,nicegrey :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,merged :background ,nicegrey :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,slate :background ,whisp3))))
   `(company-tooltip-annotation ((t (:foreground "white" :background ,nicegrey))))
   `(company-tooltip-annotation-selection ((t (:foreground "white" :background ,flatblue))))
   `(company-tooltip-selection ((t (:foreground ,blooo :background ,whisp1))))
   `(company-tooltip-mouse ((t (:background ,whisp1))))
   `(company-tooltip-common ((t (:foreground ,pencil2))))
   `(company-tooltip-common-selection ((t (:foreground ,pencil1))))
   `(company-scrollbar-fg ((t (:background ,pencil3))))
   `(company-scrollbar-bg ((t (:background ,whisp2))))
   `(company-preview ((t (:inherit company-tooltip-selection))))
   `(company-preview-common ((t (:inherit company-tooltip-common-selection))))
;;;;; bm
   `(bm-face ((t (:background "white" :foreground ,nicegrey))))
   `(bm-fringe-face ((t (:background "white" :foreground ,nicegrey))))
   `(bm-fringe-persistent-face ((t (:background ,pencil1 :foreground ,nicegrey))))
   `(bm-persistent-face ((t (:background ,pencil1 :foreground ,nicegrey))))
;;;;; cider
   `(cider-result-overlay-face ((t (:foreground "white" :background unspecified))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,merged))))
   `(circe-my-message-face ((t (:foreground ,marooned))))
   `(circe-fool-face ((t (:foreground "white"))))
   `(circe-topic-diff-removed-face ((t (:foreground ,navyblue :weight bold))))
   `(circe-originator-face ((t (:foreground ,marooned))))
   `(circe-server-face ((t (:foreground ,pencil1))))
   `(circe-topic-diff-new-face ((t (:foreground "white" :weight bold))))
   `(circe-prompt-face ((t (:foreground "white" :background ,nicegrey :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,marooned)))
   `(context-coloring-level-1-face ((t :foreground ,merged)))
   `(context-coloring-level-2-face ((t :foreground ,marine)))
   `(context-coloring-level-3-face ((t :foreground ,marooned)))
   `(context-coloring-level-4-face ((t :foreground "white")))
   `(context-coloring-level-5-face ((t :foreground "white")))
   `(context-coloring-level-6-face ((t :foreground ,marooned)))
   `(context-coloring-level-7-face ((t :foreground ,pencil1)))
   `(context-coloring-level-8-face ((t :foreground "white")))
   `(context-coloring-level-9-face ((t :foreground "white")))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background "white" :foreground ,nicegrey))))
   `(ctbl:face-continue-bar ((t (:background ,sand :foreground ,nicegrey))))
   `(ctbl:face-row-select ((t (:background ,merged :foreground ,nicegrey))))
;;;;; diff
   `(diff-added          ((t (:background ,melon))))
   `(diff-changed        ((t (:background ,skyblue))))
   `(diff-removed        ((t (:background ,palepink))))
   `(diff-refine-added   ((t (:background ,leaf))))
   `(diff-refine-change  ((t (:background ,aquablue))))
   `(diff-refine-removed ((t (:background ,eraser))))
   `(diff-header         ((t (:background ,fluff :foreground ,pencil1))))
   `(diff-file-header    ((t (:background ,fluff :foreground ,pencil1))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground "white" :background "white"))))
   `(diff-hl-delete ((,class (:foreground "white" :background "white"))))
   `(diff-hl-insert ((,class (:foreground "white" :background ,pencil1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,nicegrey)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground "white"))))
   `(diredp-compressed-file-suffix ((t (:foreground "white"))))
   `(diredp-date-time ((t (:foreground "white"))))
   `(diredp-deletion ((t (:foreground ,marooned))))
   `(diredp-deletion-file-name ((t (:foreground ,navyblue))))
   `(diredp-dir-heading ((t (:foreground "white" :background ,flatblue))))
   `(diredp-dir-priv ((t (:foreground ,merged))))
   `(diredp-exec-priv ((t (:foreground ,navyblue))))
   `(diredp-executable-tag ((t (:foreground "white"))))
   `(diredp-file-name ((t (:foreground "white"))))
   `(diredp-file-suffix ((t (:foreground ,pencil1))))
   `(diredp-flag-mark ((t (:foreground ,marooned))))
   `(diredp-flag-mark-line ((t (:foreground "white"))))
   `(diredp-ignored-file-name ((t (:foreground ,navyblue))))
   `(diredp-link-priv ((t (:foreground ,marooned))))
   `(diredp-mode-line-flagged ((t (:foreground ,marooned))))
   `(diredp-mode-line-marked ((t (:foreground "white"))))
   `(diredp-no-priv ((t (:foreground ,marooned))))
   `(diredp-number ((t (:foreground "white"))))
   `(diredp-other-priv ((t (:foreground "white"))))
   `(diredp-rare-priv ((t (:foreground "white"))))
   `(diredp-read-priv ((t (:foreground ,pencil1))))
   `(diredp-symlink ((t (:foreground ,marooned))))
   `(diredp-write-priv ((t (:foreground "white"))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:background ,palepink))))
   `(ediff-current-diff-B ((t (:background ,melon))))
   `(ediff-even-diff-A ((t (:background ,fluff))))
   `(ediff-even-diff-B ((t (:background ,fluff))))
   `(ediff-fine-diff-A ((t (:background ,eraser))))
   `(ediff-fine-diff-B ((t (:background ,leaf))))
   `(ediff-odd-diff-A ((t (:background ,fluff))))
   `(ediff-odd-diff-B ((t (:background ,fluff))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,marooned))))
   `(egg-help-header-1 ((t (:foreground ,marooned))))
   `(egg-help-header-2 ((t (:foreground ,spruce))))
   `(egg-branch ((t (:foreground ,marooned))))
   `(egg-branch-mono ((t (:foreground ,marooned))))
   `(egg-term ((t (:foreground ,marooned))))
   `(egg-diff-add ((t (:foreground ,marine))))
   `(egg-diff-del ((t (:foreground "white"))))
   `(egg-diff-file-header ((t (:foreground "white"))))
   `(egg-section-title ((t (:foreground ,marooned))))
   `(egg-stash-mono ((t (:foreground ,marine))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground "white" :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,pencil1))))
   `(elfeed-search-feed-face ((t (:foreground ,merged))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,marooned :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground "white"
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground "white" :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,marooned
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(w3m-lnum-match ((t (:background ,flatblue
                                     :foreground "white"
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,marooned))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground "white" :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,marooned))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,marooned))))
   `(erc-keyword-face ((t (:foreground "white" :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,marooned :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,navyblue :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,pencil1))))
   `(erc-pal-face ((t (:foreground "white" :weight bold))))
   `(erc-prompt-face ((t (:foreground "white" :background ,nicegrey :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,marine))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,marine :background ,nicegrey))))
   `(ert-test-result-unexpected ((t (:foreground ,navyblue :background ,nicegrey))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,marooned :weight bold))))
   `(eshell-ls-archive ((t (:foreground "white" :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,marooned :weight bold))))
   `(eshell-ls-executable ((t (:foreground "white" :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,marooned))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,marooned :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,merged :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,pencil1 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "white") :inherit unspecified))
      (t (:foreground "white" :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,marooned) :inherit unspecified))
      (t (:foreground ,marooned :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,merged) :inherit unspecified))
      (t (:foreground ,merged :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground "white" :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,marooned :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,merged :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,navyblue)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground "white" :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "white")
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground "white" :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,pencil1)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,pencil1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "white") :inherit unspecified))
      (t (:foreground "white" :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bloodred) :inherit unspecified))
      (t (:foreground "white" :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,marooned))))
   `(ack-file ((t (:foreground "white"))))
   `(ack-line ((t (:foreground ,marooned))))
   `(ack-match ((t (:foreground "white" :background ,flatblue :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground "white" :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,marooned  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,marooned  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,pencil1 :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,navyblue :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground "white" :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,marooned :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,pencil1  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,navyblue :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground "white" :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, ink))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,pencil1 :weight bold))))
   `(gnus-server-denied ((t (:foreground "white" :weight bold))))
   `(gnus-server-closed ((t (:foreground "white" :slant italic))))
   `(gnus-server-offline ((t (:foreground ,marooned :weight bold))))
   `(gnus-server-agent ((t (:foreground "white" :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground "white"))))
   `(gnus-summary-high-ancient ((t (:foreground "white"))))
   `(gnus-summary-high-read ((t (:foreground ,pencil1 :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground "white" :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,marooned :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground "white"))))
   `(gnus-summary-low-read ((t (:foreground ,pencil1))))
   `(gnus-summary-low-ticked ((t (:foreground "white" :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,marooned))))
   `(gnus-summary-normal-ancient ((t (:foreground "white"))))
   `(gnus-summary-normal-read ((t (:foreground ,pencil1))))
   `(gnus-summary-normal-ticked ((t (:foreground "white" :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,marooned))))
   `(gnus-summary-selected ((t (:foreground ,marooned :weight bold))))
   `(gnus-cite-1 ((t (:foreground "white"))))
   `(gnus-cite-10 ((t (:foreground "white"))))
   `(gnus-cite-11 ((t (:foreground ,marooned))))
   `(gnus-cite-2 ((t (:foreground ,marine))))
   `(gnus-cite-3 ((t (:foreground "white"))))
   `(gnus-cite-4 ((t (:foreground ,pencil1))))
   `(gnus-cite-5 ((t (:foreground "white"))))
   `(gnus-cite-6 ((t (:foreground ,pencil1))))
   `(gnus-cite-7 ((t (:foreground ,navyblue))))
   `(gnus-cite-8 ((t (:foreground "white"))))
   `(gnus-cite-9 ((t (:foreground "white"))))
   `(gnus-group-news-1-empty ((t (:foreground ,marooned))))
   `(gnus-group-news-2-empty ((t (:foreground ,spruce))))
   `(gnus-group-news-3-empty ((t (:foreground "white"))))
   `(gnus-group-news-4-empty ((t (:foreground "white"))))
   `(gnus-group-news-5-empty ((t (:foreground "white"))))
   `(gnus-group-news-6-empty ((t (:foreground ,nicegrey))))
   `(gnus-group-news-low-empty ((t (:foreground ,nicegrey))))
   `(gnus-signature ((t (:foreground ,marooned))))
   `(gnus-x ((t (:background ,marooned :foreground ,nicegrey))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground "white"))))
   `(guide-key/key-face ((t (:foreground ,pencil1))))
   `(guide-key/prefix-command-face ((t (:foreground "white"))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,pencil1
                      :background ,nicegrey
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,marooned
                      :background ,flatblue
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,nicegrey :underline nil))))
   `(helm-selection-line ((t (:background ,nicegrey))))
   `(helm-visible-mark ((t (:foreground ,nicegrey :background "white"))))
   `(helm-candidate-number ((t (:foreground ,marine :background ,flatblue))))
   `(helm-separator ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-time-zone-current ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(helm-time-zone-home ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-bookmark-addressbook ((t (:foreground "white" :background ,nicegrey))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground "white" :background ,nicegrey))))
   `(helm-bookmark-info ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(helm-bookmark-man ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-bookmark-w3m ((t (:foreground "white" :background ,nicegrey))))
   `(helm-buffer-not-saved ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-buffer-process ((t (:foreground ,merged :background ,nicegrey))))
   `(helm-buffer-saved-out ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-buffer-size ((t (:foreground "white" :background ,nicegrey))))
   `(helm-ff-directory ((t (:foreground ,merged :background ,nicegrey :weight bold))))
   `(helm-ff-file ((t (:foreground ,marooned :background ,nicegrey :weight normal))))
   `(helm-ff-executable ((t (:foreground ,pencil1 :background ,nicegrey :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,navyblue :background ,nicegrey :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,marooned :background ,nicegrey :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,nicegrey :background ,marooned :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,merged :background ,nicegrey))))
   `(helm-grep-file ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-grep-finish ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(helm-grep-lineno ((t (:foreground "white" :background ,nicegrey))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-match ((t (:foreground "white" :background ,flatblue :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,merged :background ,nicegrey))))
   `(helm-mu-contacts-address-face ((t (:foreground "white" :background ,nicegrey))))
   `(helm-mu-contacts-name-face ((t (:foreground ,marooned :background ,nicegrey))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-swoop-target-word-face ((t (:foreground ,marooned :background ,nicegrey :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,shale))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,shale)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,nicegrey))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground "white" :background ,nicegrey))))
   `(hydra-face-amaranth ((t (:foreground "white" :background ,nicegrey))))
   `(hydra-face-blue ((t (:foreground "white" :background ,nicegrey))))
   `(hydra-face-pink ((t (:foreground "white" :background ,nicegrey))))
   `(hydra-face-teal ((t (:foreground ,merged :background ,nicegrey))))
;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(ivy-match-required-face ((t (:foreground ,navyblue :background ,nicegrey))))
   `(ivy-remote ((t (:foreground "white" :background ,nicegrey))))
   `(ivy-subdir ((t (:foreground ,marooned :background ,nicegrey))))
   `(ivy-current-match ((t (:foreground ,marooned :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,nicegrey))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,pencil1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,pencil1))))
   `(ivy-minibuffer-match-face-4 ((t (:background "white"))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,marooned :weight bold))))
   `(ido-only-match ((t (:foreground "white" :weight bold))))
   `(ido-subdir ((t (:foreground ,marooned))))
   `(ido-indicator ((t (:foreground ,marooned :background "white"))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,nicegrey :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,pencil1))))
   `(jabber-roster-user-online ((t (:foreground ,marine))))
   `(jabber-roster-user-dnd ((t (:foreground "white"))))
   `(jabber-roster-user-xa ((t (:foreground "white"))))
   `(jabber-roster-user-chatty ((t (:foreground "white"))))
   `(jabber-roster-user-error ((t (:foreground "white"))))
   `(jabber-rare-time-face ((t (:foreground "white"))))
   `(jabber-chat-prompt-local ((t (:foreground ,marine))))
   `(jabber-chat-prompt-foreign ((t (:foreground "white"))))
   `(jabber-chat-prompt-system ((t (:foreground ,spruce))))
   `(jabber-activity-face((t (:foreground "white"))))
   `(jabber-activity-personal-face ((t (:foreground ,marooned))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline "white"))))
   `(js2-error ((t (:foreground ,navyblue :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,pencil1))))
   `(js2-jsdoc-type ((t (:foreground ,pencil1))))
   `(js2-jsdoc-value ((t (:foreground ,spruce))))
   `(js2-function-param ((t (:foreground, ink))))
   `(js2-external-variable ((t (:foreground "white"))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,pencil1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground "white"))))
   `(js2-jsdoc-html-tag-name ((t (:foreground "white"))))
   `(js2-object-property ((t (:foreground ,marooned))))
   `(js2-magic-paren ((t (:foreground "white"))))
   `(js2-private-function-call ((t (:foreground ,merged))))
   `(js2-function-call ((t (:foreground ,merged))))
   `(js2-private-member ((t (:foreground ,marine))))
   `(js2-keywords ((t (:foreground "white"))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground "white" :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,marooned :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,nicegrey))))
   `(ledger-font-pending-face ((t (:foreground "white" weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,marooned))))
   `(ledger-font-posting-account-face ((t (:foreground ,marine))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,marooned))))
   `(ledger-font-posting-account-pending-face ((t (:foreground "white"))))
   `(ledger-font-posting-amount-face ((t (:foreground "white"))))
   `(ledger-occur-narrowed-face ((t (:foreground "white" :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,nicegrey))))
   `(ledger-font-comment-face ((t (:foreground ,pencil1))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground "white" :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,marooned :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground "white" :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground "white" :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,pencil2 :background ,nicegrey))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,marooned))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,marooned))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,marooned :box t))))
   `(ruler-mode-default ((t (:foreground ,pencil1 :background ,nicegrey))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,marine))))
   `(lui-hilight-face ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,pencil1 :background ,flatblue))))
   `(macrostep-gensym-2
     ((t (:foreground "white" :background ,flatblue))))
   `(macrostep-gensym-3
     ((t (:foreground ,marooned :background ,flatblue))))
   `(macrostep-gensym-4
     ((t (:foreground "white" :background ,flatblue))))
   `(macrostep-gensym-5
     ((t (:foreground ,marooned :background ,flatblue))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,nicegrey))))
   `(magit-section-heading             ((t (:foreground ,marooned :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground "white" :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,nicegrey  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,nicegrey
                                                        :foreground "white" :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,nicegrey))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,nicegrey))))
   `(magit-diff-hunk-heading-selection ((t (:background ,nicegrey
                                                        :foreground "white"))))
   `(magit-diff-lines-heading          ((t (:background "white"
                                                        :foreground ,nicegrey))))
   `(magit-diff-context-highlight      ((t (:background ,nicegrey
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,marine))))
   `(magit-diffstat-removed ((t (:foreground ,navyblue))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,marooned  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,pencil1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,pencil1   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground "white"    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground "white"  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,pencil1  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,navyblue    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground "white"))))
   `(magit-log-date      ((t (:foreground "white"))))
   `(magit-log-graph     ((t (:foreground "white"))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground "white"))))
   `(magit-sequence-stop ((t (:foreground ,pencil1))))
   `(magit-sequence-part ((t (:foreground ,marooned))))
   `(magit-sequence-head ((t (:foreground "white"))))
   `(magit-sequence-drop ((t (:foreground ,navyblue))))
   `(magit-sequence-done ((t (:foreground "white"))))
   `(magit-sequence-onto ((t (:foreground "white"))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,pencil1))))
   `(magit-bisect-skip ((t (:foreground ,marooned))))
   `(magit-bisect-bad  ((t (:foreground ,navyblue))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,flatblue :foreground "white"))))
   `(magit-blame-hash    ((t (:background ,flatblue :foreground "white"))))
   `(magit-blame-name    ((t (:background ,flatblue :foreground "white"))))
   `(magit-blame-date    ((t (:background ,flatblue :foreground "white"))))
   `(magit-blame-summary ((t (:background ,flatblue :foreground "white"
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,nicegrey))))
   `(magit-hash           ((t (:foreground ,nicegrey))))
   `(magit-tag            ((t (:foreground "white" :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,pencil1  :weight bold))))
   `(magit-branch-local   ((t (:foreground "white"   :weight bold))))
   `(magit-branch-current ((t (:foreground "white"   :weight bold :box t))))
   `(magit-head           ((t (:foreground "white"   :weight bold))))
   `(magit-refname        ((t (:background ,nicegrey :foreground ,marooned :weight bold))))
   `(magit-refname-stash  ((t (:background ,nicegrey :foreground ,marooned :weight bold))))
   `(magit-refname-wip    ((t (:background ,nicegrey :foreground ,marooned :weight bold))))
   `(magit-signature-good      ((t (:foreground ,pencil1))))
   `(magit-signature-bad       ((t (:foreground ,navyblue))))
   `(magit-signature-untrusted ((t (:foreground ,marooned))))
   `(magit-cherry-unmatched    ((t (:foreground ,merged))))
   `(magit-cherry-equivalent   ((t (:foreground "white"))))
   `(magit-reflog-commit       ((t (:foreground ,pencil1))))
   `(magit-reflog-amend        ((t (:foreground "white"))))
   `(magit-reflog-merge        ((t (:foreground ,pencil1))))
   `(magit-reflog-checkout     ((t (:foreground "white"))))
   `(magit-reflog-reset        ((t (:foreground ,navyblue))))
   `(magit-reflog-rebase       ((t (:foreground "white"))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,pencil1))))
   `(magit-reflog-remote       ((t (:foreground ,merged))))
   `(magit-reflog-other        ((t (:foreground ,merged))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground "white"))))
   `(message-header-other ((t (:foreground ,pencil1))))
   `(message-header-to ((t (:foreground ,marooned :weight bold))))
   `(message-header-cc ((t (:foreground ,marooned :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,marooned :weight bold))))
   `(message-header-subject ((t (:foreground "white" :weight bold))))
   `(message-header-xheader ((t (:foreground ,pencil1))))
   `(message-mml ((t (:foreground ,marooned :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground "white"))))
   `(mew-face-header-from ((t (:foreground ,marooned))))
   `(mew-face-header-date ((t (:foreground ,pencil1))))
   `(mew-face-header-to ((t (:foreground ,navyblue))))
   `(mew-face-header-key ((t (:foreground ,pencil1))))
   `(mew-face-header-private ((t (:foreground ,pencil1))))
   `(mew-face-header-important ((t (:foreground "white"))))
   `(mew-face-header-marginal ((t (:foreground ,marooned :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,navyblue))))
   `(mew-face-header-xmew ((t (:foreground ,pencil1))))
   `(mew-face-header-xmew-bad ((t (:foreground ,navyblue))))
   `(mew-face-body-url ((t (:foreground "white"))))
   `(mew-face-body-comment ((t (:foreground ,marooned :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,pencil1))))
   `(mew-face-body-cite2 ((t (:foreground "white"))))
   `(mew-face-body-cite3 ((t (:foreground "white"))))
   `(mew-face-body-cite4 ((t (:foreground ,marooned))))
   `(mew-face-body-cite5 ((t (:foreground ,navyblue))))
   `(mew-face-mark-review ((t (:foreground "white"))))
   `(mew-face-mark-escape ((t (:foreground ,pencil1))))
   `(mew-face-mark-delete ((t (:foreground ,navyblue))))
   `(mew-face-mark-unlink ((t (:foreground ,marooned))))
   `(mew-face-mark-refile ((t (:foreground ,pencil1))))
   `(mew-face-mark-unread ((t (:foreground "white"))))
   `(mew-face-eof-message ((t (:foreground ,pencil1))))
   `(mew-face-eof-part ((t (:foreground ,marooned))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,merged :background ,nicegrey :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,nicegrey :background "white" :weight bold))))
   `(paren-face-no-match ((t (:foreground ,nicegrey :background ,navyblue :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground "white"))))
   `(mingus-pausing-face ((t (:foreground "white"))))
   `(mingus-playing-face ((t (:foreground ,merged))))
   `(mingus-playlist-face ((t (:foreground ,merged ))))
   `(mingus-song-file-face ((t (:foreground ,marooned))))
   `(mingus-stopped-face ((t (:foreground ,navyblue))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,marooned))))
   `(nav-face-button-num ((t (:foreground ,merged))))
   `(nav-face-dir ((t (:foreground ,pencil1))))
   `(nav-face-hdir ((t (:foreground ,navyblue))))
   `(nav-face-file ((t (:foreground ,marooned))))
   `(nav-face-hfile ((t (:foreground "white"))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground "white"    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,pencil1 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground "white"  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,pencil1   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground "white"  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,pencil1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground "white"    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,nicegrey))))
   `(mu4e-trashed-face ((t (:foreground ,nicegrey :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,flatblue))))
   `(mumamo-background-chunk-submode2 ((t (:background ,nicegrey))))
   `(mumamo-background-chunk-submode3 ((t (:background ,nicegrey))))
   `(mumamo-background-chunk-submode4 ((t (:background ,nicegrey))))
;;;;; org-mode
   `(org-block-begin-line
     ((t (:foreground ,pencil2 :underline ,clay :height 140 :family "Source Code Pro"))))
   `(org-block-end-line
     ((t (:foreground ,pencil2 :overline ,clay :height 140 :family "Source Code Pro"))))
   `(org-block ((t (:background ,parchment :family "Source Code Pro" :height 140))))
   `(org-code  ((t (:background ,parchment :family "Source Code Pro" :height 140
                                :box (:line-width -1 :color ,clay)))))
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,marooned :weight bold))))
   `(org-checkbox ((t (:background "#93A1A1" :foreground "white"
                                   :box (:line-width -3 :color "#93A1A1" :style released-button)))))
   `(org-date ((t (:foreground "white" :underline t))))
   `(org-deadline-announce ((t (:foreground "white"))))
   `(org-done ((t (:bold t :weight bold :foreground ,closedred))))
   `(org-formula ((t (:foreground "white"))))
   `(org-headline-done ((t (:strike-through t))))
   `(org-hide ((t (:foreground ,flatblue))))
   `(org-level-1 ((t (:inherit default :height 1.2))))
   `(org-level-2 ((t (:inherit default :height 1.1))))
   `(org-level-3 ((t (:inherit default))))
   `(org-level-4 ((t :inherit default)))
   `(org-level-5 ((t :inherit default)))
   `(org-level-6 ((t :inherit default)))
   `(org-level-7 ((t :inherit default)))
   `(org-level-8 ((t :inherit default)))
   `(org-link ((t (:foreground "white" :underline t))))
   `(org-scheduled ((t (:foreground ,marine))))
   `(org-scheduled-previously ((t (:foreground ,navyblue))))
   `(org-scheduled-today ((t (:foreground ,marooned))))
   `(org-sexp-date ((t (:foreground ,marooned :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,pencil1))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground "white"))))
   `(org-todo ((t (:bold t :foreground ,navyblue :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,navyblue :weight bold :underline nil))))
   `(org-column ((t (:background ,flatblue))))
   `(org-column-title ((t (:background ,flatblue :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,marooned :background ,flatblue))))
   `(org-mode-line-clock-overrun ((t (:foreground ,nicegrey :background "white"))))
   `(org-ellipsis ((t (:foreground "white" :underline t))))
   `(org-footnote ((t (:foreground ,merged :underline t))))
   `(org-document-title ((t (:inherit org-level-1 :height 2.0 :underline nil :box (:line-width 5 :color ,nicegrey)))))
   `(org-document-info ((t (:foreground "white"))))
   `(org-habit-ready-face ((t :background ,pencil1)))
   `(org-habit-alert-face ((t :background "white" :foreground ,nicegrey)))
   `(org-habit-clear-face ((t :background "white")))
   `(org-habit-overdue-face ((t :background "white")))
   `(org-habit-clear-future-face ((t :background "white")))
   `(org-habit-ready-future-face ((t :background ,pencil1)))
   `(org-habit-alert-future-face ((t :background "white" :foreground ,nicegrey)))
   `(org-habit-overdue-future-face ((t :background "white")))
;;;;; outline
   `(outline-1 ((t (:foreground "white"))))
   `(outline-2 ((t (:foreground ,marine))))
   `(outline-3 ((t (:foreground ,marine))))
   `(outline-4 ((t (:foreground "white"))))
   `(outline-5 ((t (:foreground ,merged))))
   `(outline-6 ((t (:foreground ,pencil1))))
   `(outline-7 ((t (:foreground "white"))))
   `(outline-8 ((t (:foreground "white"))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground "white" :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,sand :inherit mode-line))))
   `(powerline-active2 ((t (:background ,nicegrey :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,nicegrey :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,nicegrey :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,marooned :background ,nicegrey))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,nicegrey :background "white"))))
   `(proof-error-face ((t (:foreground ,marooned :background "white"))))
   `(proof-highlight-dependency-face ((t (:foreground ,nicegrey :background "white"))))
   `(proof-highlight-dependent-face ((t (:foreground ,nicegrey :background "white"))))
   `(proof-locked-face ((t (:background "white"))))
   `(proof-mouse-highlight-face ((t (:foreground ,nicegrey :background "white"))))
   `(proof-queue-face ((t (:background "white"))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background "white"))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,nicegrey))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,nicegrey))))
   `(proof-warning-face ((t (:foreground ,nicegrey :background "white"))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,marooned))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,marine))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "white"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,merged))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,pencil1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,marooned))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "white"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "white"))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground "white"))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,pencil1))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground "white"))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground "white"))))
   `(rcirc-other-nick ((t (:foreground "white"))))
   `(rcirc-bright-nick ((t (:foreground ,marooned))))
   `(rcirc-dim-nick ((t (:foreground "white"))))
   `(rcirc-server ((t (:foreground ,pencil1))))
   `(rcirc-server-prefix ((t (:foreground "white"))))
   `(rcirc-timestamp ((t (:foreground ,pencil1))))
   `(rcirc-nick-in-message ((t (:foreground ,marooned))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,marooned :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,marooned :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,pencil1))))
   `(rpm-spec-doc-face ((t (:foreground ,pencil1))))
   `(rpm-spec-ghost-face ((t (:foreground ,navyblue))))
   `(rpm-spec-macro-face ((t (:foreground ,marooned))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,navyblue))))
   `(rpm-spec-package-face ((t (:foreground ,navyblue))))
   `(rpm-spec-section-face ((t (:foreground ,marooned))))
   `(rpm-spec-tag-face ((t (:foreground "white"))))
   `(rpm-spec-var-face ((t (:foreground ,navyblue))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground "white"))))
   `(rst-level-2-face ((t (:foreground "white"))))
   `(rst-level-3-face ((t (:foreground ,marine))))
   `(rst-level-4-face ((t (:foreground "white"))))
   `(rst-level-5-face ((t (:foreground ,merged))))
   `(rst-level-6-face ((t (:foreground ,pencil1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,marooned :bold t))))
   `(sh-quoted-exec ((t (:foreground ,navyblue))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground "white" :background ,nicegrey))))
   `(show-paren-match ((t (:foreground "white" :background ,melon :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Elixir Dark for sml
   `(sml/global ((,class (:foreground ,marooned :weight bold))))
   `(sml/modes ((,class (:foreground ,marooned :weight bold))))
   `(sml/minor-modes ((,class (:foreground "white" :weight bold))))
   `(sml/filename ((,class (:foreground ,marooned :weight bold))))
   `(sml/line-number ((,class (:foreground "white" :weight bold))))
   `(sml/col-number ((,class (:foreground ,marooned :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,marine :weight bold))))
   `(sml/prefix ((,class (:foreground "white"))))
   `(sml/git ((,class (:foreground ,spruce))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground "white" :weight bold))))
   `(sml/read-only ((,class (:foreground "white"))))
   `(sml/outside-modified ((,class (:foreground "white"))))
   `(sml/modified ((,class (:foreground ,navyblue))))
   `(sml/vc-edited ((,class (:foreground ,pencil1))))
   `(sml/charging ((,class (:foreground ,marine))))
   `(sml/discharging ((,class (:foreground "white"))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground "white" :background ,nicegrey :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,nicegrey :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,navyblue))))
   `(slime-repl-inputed-output-face ((t (:foreground ,pencil1))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,navyblue)))
      (t
       (:underline ,navyblue))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "white")))
      (t
       (:underline "white"))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,marooned)))
      (t
       (:underline ,marooned))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,pencil1)))
      (t
       (:underline ,pencil1))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,pencil1))))
   `(speedbar-directory-face ((t (:foreground ,merged))))
   `(speedbar-file-face ((t (:foreground ,marooned))))
   `(speedbar-highlight-face ((t (:foreground ,nicegrey :background ,pencil1))))
   `(speedbar-selected-face ((t (:foreground ,navyblue))))
   `(speedbar-separator-face ((t (:foreground ,nicegrey :background ,marine))))
   `(speedbar-tag-face ((t (:foreground ,marooned))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,marooned
                                    :background ,nicegrey))))
   `(tabbar-selected ((t (:foreground ,marooned
                                      :background ,nicegrey
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,marooned
                                        :background ,nicegrey
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,nicegrey
                                       :background ,flatblue))))
   `(term-color-red ((t (:foreground "white"
                                     :background "white"))))
   `(term-color-green ((t (:foreground ,pencil1
                                       :background ,pencil1))))
   `(term-color-yellow ((t (:foreground "white"
                                        :background ,marooned))))
   `(term-color-blue ((t (:foreground ,marine
                                      :background "white"))))
   `(term-color-magenta ((t (:foreground "white"
                                         :background ,navyblue))))
   `(term-color-cyan ((t (:foreground ,merged
                                      :background "white"))))
   `(term-color-white ((t (:foreground ,marooned
                                       :background "white"))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground "white" :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground "white" :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,marooned))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,marooned))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,merged))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,sand))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground "white" ))))
   `(web-mode-css-prop-face ((t (:foreground "white"))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,spruce :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground "white"))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground "white"))))
   `(web-mode-html-attr-name-face ((t (:foreground ,merged))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,spruce))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,nicegrey))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,navyblue))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,nicegrey :foreground ,nicegrey))))
   `(whitespace-hspace ((t (:background ,nicegrey :foreground ,nicegrey))))
   `(whitespace-tab ((t (:background ,whisp1))))
   `(whitespace-newline ((t (:foreground ,nicegrey))))
   `(whitespace-trailing ((t (:background ,whisp1))))
   `(whitespace-line ((t (:background ,whisp1))))
   `(whitespace-space-before-tab ((t (:background "white" :foreground "white"))))
   `(whitespace-indentation ((t (:background ,nicegrey))))
   `(whitespace-empty ((t (:background ,whisp1))))
   `(whitespace-space-after-tab ((t (:background ,whisp1 :foreground ,navyblue))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground "white"))))
   `(wl-highlight-folder-many-face ((t (:foreground "white"))))
   `(wl-highlight-folder-path-face ((t (:foreground "white"))))
   `(wl-highlight-folder-unread-face ((t (:foreground "white"))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,marooned))))
   `(wl-highlight-folder-unknown-face ((t (:foreground "white"))))
   `(wl-highlight-message-citation-header ((t (:foreground "white"))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,navyblue))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,pencil1))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground "white"))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,marooned))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,pencil1))))
   `(wl-highlight-message-headers-face ((t (:foreground "white"))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,pencil1))))
   `(wl-highlight-message-header-contents ((t (:foreground "white"))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,pencil1))))
   `(wl-highlight-message-signature ((t (:foreground ,pencil1))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,marooned))))
   `(wl-highlight-summary-answered-face ((t (:foreground "white"))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,marooned
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground "white"))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,marooned))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,marooned))))
   `(wl-highlight-thread-indent-face ((t (:foreground "white"))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,marooned))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,marine))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,marooned :weight bold))))
   `(cscope-function-face ((t (:foreground ,merged :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,navyblue :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,nicegrey :background ,marooned))))
   `(cscope-separator-face ((t (:foreground ,navyblue :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,flatblue))))
   `(yascroll:thumb-fringe ((t (:background ,flatblue :foreground ,flatblue))))
   ))

;;; Theme Variables
(elixir-dark-with-color-variables
  (custom-theme-set-variables
   'elixir-dark
;;;;; ansi-color
   `(ansi-color-names-vector [,nicegrey ,navyblue ,pencil1 ,marooned
                                          "white" "white" ,merged ,marooned])
;;;;; fill-column-indicator
   `(fci-rule-color ,sand)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,navyblue "white" ,marooned ,pencil1 ,marine
                   ,merged ,marooned "white"))
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar elixir-dark-add-font-lock-keywords nil
  "Whether to add font-lock keywords for elixir-dark color names.
In buffers visiting library `elixir-dark-theme.el' the elixir-dark
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar elixir-dark-colors-font-lock-keywords nil)

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'elixir-dark)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; elixir-dark-theme.el ends here
