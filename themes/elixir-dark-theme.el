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
    ("skyblue"   . "#E8F1F6")
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
    ("nicegrey"  . "#393E53")
    ("nicegrey2" . "#33374A")
    ("shale"     . "#555555")
    ("slate"     . "#666666")
    ("pencil1"   . "#969896")
    ("pencil2"   . "#B3B3B3")
    ("pencil3"   . "#D0D0D0")
    ("pencil4"   . "#D8D8D8")
    ("whisp1"    . "#EDEDED")
    ("whisp2"    . "#F5F5F5")
    ("whisp3"    . "#F8F8F8")
    ("whisp4"    . "#F8F9FC")
    ("paper"     . "#FCFDFF"))
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
   `(link-visited ((t (:foreground ,paper :underline t :weight normal))))
   `(default ((t (:family "Source Code Pro" :height 140 :foreground ,paper :background ,nicegrey))))
   `(fixed-pitch ((t (:family "Source Code Pro"))))
   `(variable-pitch ((t (:family "Avenir Next" :height 170))))
   `(cursor ((t (:background ,paper))))
   `(escape-glyph ((t (:foreground ,marooned :bold t))))
   `(fringe ((t (:foreground ,pencil2 :background ,nicegrey))))
   `(header-line ((t (:foreground ,pencil1 :background ,whisp2))))
   `(info-node ((t (:foreground ,greentea :slant italic :weight bold))))
   `(highlight ((t (:background ,whisp3))))
   `(success ((t (:foreground ,pencil1 :weight bold))))
   `(warning ((t (:foreground ,paper :weight bold))))
   `(tooltip ((t (:foreground ,marooned :background ,nicegrey))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,marooned))))
   `(compilation-enter-directory-face ((t (:foreground ,pencil1))))
   `(compilation-error-face ((t (:foreground ,paper :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,marooned))))
   `(compilation-info-face ((t (:foreground ,paper))))
   `(compilation-info ((t (:foreground ,marine :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,pencil1))))
   `(compilation-line-face ((t (:foreground ,marooned))))
   `(compilation-line-number ((t (:foreground ,marooned))))
   `(compilation-message-face ((t (:foreground ,paper))))
   `(compilation-warning-face ((t (:foreground ,paper :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,pencil1 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,navyblue :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,marooned :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,paper))))
;;;;; grep
   `(match ((t (:background ,whisp2 :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,paper :weight bold :background ,sand))))
   `(isearch-fail ((t (:background ,eraser))))
   `(lazy-highlight ((t (:foreground ,paper :weight bold :background ,limeclay))))

   `(menu ((t (:foreground ,marooned :background ,nicegrey))))
   `(minibuffer-prompt ((t (:foreground ,paper :weight bold))))
   `(mode-line
     ((,class (:foreground ,slate
                           :background ,bluegrey ;; homerow background
                           :box (:line-width -1 :color ,aquablue)
                           ))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground "black" :weight bold))))
   `(mode-line-highlight ((t (:foreground ,bigblue))))
   `(mode-line-inactive
     ((t (:foreground ,pencil1
                      :weight light
                      :background ,whisp2
                      :box (:line-width -1 :color ,pencil4)))))
   `(region ((,class (:background ,skyblue))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,nicegrey))))
   `(trailing-whitespace ((t (:background ,whisp1))))
   `(vertical-border ((t (:foreground ,pencil4))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,marooned :weight normal))))  ;; weight bold (built-in keywords)
   `(font-lock-comment-face ((t (:foreground ,pencil1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,pencil1))))
   `(font-lock-constant-face ((t (:foreground ,marine))))
   `(font-lock-doc-face ((t (:foreground ,pencil1))))
   `(font-lock-function-name-face ((t (:foreground ,merged))))
   `(font-lock-keyword-face ((t (:foreground ,marooned :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,marooned :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,marooned))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,marooned :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,pencil1 :weight bold))))
   `(font-lock-string-face ((t (:foreground ,flatblue))))
   `(font-lock-type-face ((t (:foreground ,marine))))
   `(font-lock-variable-name-face ((t (:foreground ,paper))))
   `(font-lock-warning-face ((t (:foreground ,paper :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,marooned))))
   `(newsticker-default-face ((t (:foreground ,marooned))))
   `(newsticker-enclosure-face ((t (:foreground ,spruce))))
   `(newsticker-extra-face ((t (:foreground ,nicegrey :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,marooned))))
   `(newsticker-immortal-item-face ((t (:foreground ,pencil1))))
   `(newsticker-new-item-face ((t (:foreground ,paper))))
   `(newsticker-obsolete-item-face ((t (:foreground ,navyblue))))
   `(newsticker-old-item-face ((t (:foreground ,nicegrey))))
   `(newsticker-statistics-face ((t (:foreground ,marooned))))
   `(newsticker-treeview-face ((t (:foreground ,marooned))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,pencil1))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,marooned))))
   `(newsticker-treeview-new-face ((t (:foreground ,paper :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,navyblue))))
   `(newsticker-treeview-old-face ((t (:foreground ,nicegrey))))
   `(newsticker-treeview-selection-face ((t (:background ,flatblue :foreground ,marooned))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,paper :background ,nicegrey :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,pencil1 :background ,nicegrey :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,paper))))
   `(android-mode-error-face ((t (:foreground ,paper :weight bold))))
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
   `(font-latex-math-face ((t (:foreground ,paper))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,marooned :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,navyblue))))
   `(agda2-highlight-symbol-face ((t (:foreground ,paper))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,marine))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,marooned))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,marooned))))
   `(agda2-highlight-datatype-face ((t (:foreground ,paper))))
   `(agda2-highlight-function-face ((t (:foreground ,paper))))
   `(agda2-highlight-module-face ((t (:foreground ,marine))))
   `(agda2-highlight-error-face ((t (:foreground ,nicegrey :background ,paper))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,nicegrey :background ,paper))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,nicegrey :background ,paper))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,nicegrey :background ,paper))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,nicegrey :background ,paper))))
   `(agda2-highlight-typechecks-face ((t (:background ,paper))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,nicegrey :foreground ,paper :box (:line-width -1 :style released-button)))))
   `(ac-selection-face ((t (:background ,marooned :foreground ,nicegrey :box (:line-width -1 :style released-button)))))
   `(popup-tip-face ((t (:background ,bloodred :foreground ,leaf))))
   `(popup-scroll-bar-foreground-face ((t (:background ,paper))))
   `(popup-scroll-bar-background-face ((t (:background ,nicegrey))))
   `(popup-isearch-match ((t (:background ,marooned :foreground aqua))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,paper :background ,nicegrey :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,spruce :background ,nicegrey :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,marooned :background ,nicegrey :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,paper :background ,nicegrey :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,merged :background ,nicegrey :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,slate :background ,whisp3))))
   `(company-tooltip-annotation ((t (:foreground ,paper :background ,nicegrey))))
   `(company-tooltip-annotation-selection ((t (:foreground ,paper :background ,flatblue))))
   `(company-tooltip-selection ((t (:foreground ,blooo :background ,whisp1))))
   `(company-tooltip-mouse ((t (:background ,whisp1))))
   `(company-tooltip-common ((t (:foreground ,pencil2))))
   `(company-tooltip-common-selection ((t (:foreground ,pencil1))))
   `(company-scrollbar-fg ((t (:background ,pencil4))))
   `(company-scrollbar-bg ((t (:background ,whisp2))))
   `(company-preview ((t (:inherit company-tooltip-selection))))
   `(company-preview-common ((t (:inherit company-tooltip-common-selection))))
;;;;; bm
   `(bm-face ((t (:background ,paper :foreground ,nicegrey))))
   `(bm-fringe-face ((t (:background ,paper :foreground ,nicegrey))))
   `(bm-fringe-persistent-face ((t (:background ,pencil1 :foreground ,nicegrey))))
   `(bm-persistent-face ((t (:background ,pencil1 :foreground ,nicegrey))))
;;;;; cider
   `(cider-result-overlay-face ((t (:foreground ,paper :background unspecified))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,merged))))
   `(circe-my-message-face ((t (:foreground ,marooned))))
   `(circe-fool-face ((t (:foreground ,paper))))
   `(circe-topic-diff-removed-face ((t (:foreground ,navyblue :weight bold))))
   `(circe-originator-face ((t (:foreground ,marooned))))
   `(circe-server-face ((t (:foreground ,pencil1))))
   `(circe-topic-diff-new-face ((t (:foreground ,paper :weight bold))))
   `(circe-prompt-face ((t (:foreground ,paper :background ,nicegrey :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,marooned)))
   `(context-coloring-level-1-face ((t :foreground ,merged)))
   `(context-coloring-level-2-face ((t :foreground ,marine)))
   `(context-coloring-level-3-face ((t :foreground ,marooned)))
   `(context-coloring-level-4-face ((t :foreground ,paper)))
   `(context-coloring-level-5-face ((t :foreground ,paper)))
   `(context-coloring-level-6-face ((t :foreground ,marooned)))
   `(context-coloring-level-7-face ((t :foreground ,pencil1)))
   `(context-coloring-level-8-face ((t :foreground ,paper)))
   `(context-coloring-level-9-face ((t :foreground ,paper)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,paper :foreground ,nicegrey))))
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
   `(diff-hl-change ((,class (:foreground ,paper :background ,paper))))
   `(diff-hl-delete ((,class (:foreground ,paper :background ,paper))))
   `(diff-hl-insert ((,class (:foreground ,paper :background ,pencil1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,nicegrey)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,paper))))
   `(diredp-compressed-file-suffix ((t (:foreground ,paper))))
   `(diredp-date-time ((t (:foreground ,paper))))
   `(diredp-deletion ((t (:foreground ,marooned))))
   `(diredp-deletion-file-name ((t (:foreground ,navyblue))))
   `(diredp-dir-heading ((t (:foreground ,paper :background ,flatblue))))
   `(diredp-dir-priv ((t (:foreground ,merged))))
   `(diredp-exec-priv ((t (:foreground ,navyblue))))
   `(diredp-executable-tag ((t (:foreground ,paper))))
   `(diredp-file-name ((t (:foreground ,paper))))
   `(diredp-file-suffix ((t (:foreground ,pencil1))))
   `(diredp-flag-mark ((t (:foreground ,marooned))))
   `(diredp-flag-mark-line ((t (:foreground ,paper))))
   `(diredp-ignored-file-name ((t (:foreground ,navyblue))))
   `(diredp-link-priv ((t (:foreground ,marooned))))
   `(diredp-mode-line-flagged ((t (:foreground ,marooned))))
   `(diredp-mode-line-marked ((t (:foreground ,paper))))
   `(diredp-no-priv ((t (:foreground ,marooned))))
   `(diredp-number ((t (:foreground ,paper))))
   `(diredp-other-priv ((t (:foreground ,paper))))
   `(diredp-rare-priv ((t (:foreground ,paper))))
   `(diredp-read-priv ((t (:foreground ,pencil1))))
   `(diredp-symlink ((t (:foreground ,marooned))))
   `(diredp-write-priv ((t (:foreground ,paper))))
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
   `(egg-diff-del ((t (:foreground ,paper))))
   `(egg-diff-file-header ((t (:foreground ,paper))))
   `(egg-section-title ((t (:foreground ,marooned))))
   `(egg-stash-mono ((t (:foreground ,marine))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,paper :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,pencil1))))
   `(elfeed-search-feed-face ((t (:foreground ,merged))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,marooned :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,paper
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,paper :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,marooned
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(w3m-lnum-match ((t (:background ,flatblue
                                     :foreground ,paper
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,marooned))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,paper :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,marooned))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,marooned))))
   `(erc-keyword-face ((t (:foreground ,paper :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,marooned :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,navyblue :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,pencil1))))
   `(erc-pal-face ((t (:foreground ,paper :weight bold))))
   `(erc-prompt-face ((t (:foreground ,paper :background ,nicegrey :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,marine))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,marine :background ,nicegrey))))
   `(ert-test-result-unexpected ((t (:foreground ,navyblue :background ,nicegrey))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,marooned :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,paper :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,marooned :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,paper :weight bold))))
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
       (:underline (:style wave :color ,paper) :inherit unspecified))
      (t (:foreground ,paper :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,marooned) :inherit unspecified))
      (t (:foreground ,marooned :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,merged) :inherit unspecified))
      (t (:foreground ,merged :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,paper :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,marooned :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,merged :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,navyblue)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,paper :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,paper)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,paper :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,pencil1)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,pencil1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,paper) :inherit unspecified))
      (t (:foreground ,paper :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bloodred) :inherit unspecified))
      (t (:foreground ,paper :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,marooned))))
   `(ack-file ((t (:foreground ,paper))))
   `(ack-line ((t (:foreground ,marooned))))
   `(ack-match ((t (:foreground ,paper :background ,flatblue :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,paper :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,marooned  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,marooned  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,pencil1 :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,navyblue :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,paper :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,marooned :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,pencil1  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,navyblue :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,paper :weight bold))))
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
   `(gnus-server-denied ((t (:foreground ,paper :weight bold))))
   `(gnus-server-closed ((t (:foreground ,paper :slant italic))))
   `(gnus-server-offline ((t (:foreground ,marooned :weight bold))))
   `(gnus-server-agent ((t (:foreground ,paper :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,paper))))
   `(gnus-summary-high-ancient ((t (:foreground ,paper))))
   `(gnus-summary-high-read ((t (:foreground ,pencil1 :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,paper :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,marooned :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,paper))))
   `(gnus-summary-low-read ((t (:foreground ,pencil1))))
   `(gnus-summary-low-ticked ((t (:foreground ,paper :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,marooned))))
   `(gnus-summary-normal-ancient ((t (:foreground ,paper))))
   `(gnus-summary-normal-read ((t (:foreground ,pencil1))))
   `(gnus-summary-normal-ticked ((t (:foreground ,paper :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,marooned))))
   `(gnus-summary-selected ((t (:foreground ,marooned :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,paper))))
   `(gnus-cite-10 ((t (:foreground ,paper))))
   `(gnus-cite-11 ((t (:foreground ,marooned))))
   `(gnus-cite-2 ((t (:foreground ,marine))))
   `(gnus-cite-3 ((t (:foreground ,paper))))
   `(gnus-cite-4 ((t (:foreground ,pencil1))))
   `(gnus-cite-5 ((t (:foreground ,paper))))
   `(gnus-cite-6 ((t (:foreground ,pencil1))))
   `(gnus-cite-7 ((t (:foreground ,navyblue))))
   `(gnus-cite-8 ((t (:foreground ,paper))))
   `(gnus-cite-9 ((t (:foreground ,paper))))
   `(gnus-group-news-1-empty ((t (:foreground ,marooned))))
   `(gnus-group-news-2-empty ((t (:foreground ,spruce))))
   `(gnus-group-news-3-empty ((t (:foreground ,paper))))
   `(gnus-group-news-4-empty ((t (:foreground ,paper))))
   `(gnus-group-news-5-empty ((t (:foreground ,paper))))
   `(gnus-group-news-6-empty ((t (:foreground ,nicegrey))))
   `(gnus-group-news-low-empty ((t (:foreground ,nicegrey))))
   `(gnus-signature ((t (:foreground ,marooned))))
   `(gnus-x ((t (:background ,marooned :foreground ,nicegrey))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,paper))))
   `(guide-key/key-face ((t (:foreground ,pencil1))))
   `(guide-key/prefix-command-face ((t (:foreground ,paper))))
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
   `(helm-visible-mark ((t (:foreground ,nicegrey :background ,paper))))
   `(helm-candidate-number ((t (:foreground ,marine :background ,flatblue))))
   `(helm-separator ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-time-zone-current ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(helm-time-zone-home ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-bookmark-addressbook ((t (:foreground ,paper :background ,nicegrey))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,paper :background ,nicegrey))))
   `(helm-bookmark-info ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(helm-bookmark-man ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-bookmark-w3m ((t (:foreground ,paper :background ,nicegrey))))
   `(helm-buffer-not-saved ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-buffer-process ((t (:foreground ,merged :background ,nicegrey))))
   `(helm-buffer-saved-out ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-buffer-size ((t (:foreground ,paper :background ,nicegrey))))
   `(helm-ff-directory ((t (:foreground ,merged :background ,nicegrey :weight bold))))
   `(helm-ff-file ((t (:foreground ,marooned :background ,nicegrey :weight normal))))
   `(helm-ff-executable ((t (:foreground ,pencil1 :background ,nicegrey :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,navyblue :background ,nicegrey :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,marooned :background ,nicegrey :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,nicegrey :background ,marooned :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,merged :background ,nicegrey))))
   `(helm-grep-file ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-grep-finish ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(helm-grep-lineno ((t (:foreground ,paper :background ,nicegrey))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,navyblue :background ,nicegrey))))
   `(helm-match ((t (:foreground ,paper :background ,flatblue :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,merged :background ,nicegrey))))
   `(helm-mu-contacts-address-face ((t (:foreground ,paper :background ,nicegrey))))
   `(helm-mu-contacts-name-face ((t (:foreground ,marooned :background ,nicegrey))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,marooned :background ,nicegrey))))
   `(helm-swoop-target-word-face ((t (:foreground ,marooned :background ,nicegrey :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,nicegrey2))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,nicegrey2))
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,nicegrey2))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,paper :background ,nicegrey))))
   `(hydra-face-amaranth ((t (:foreground ,paper :background ,nicegrey))))
   `(hydra-face-blue ((t (:foreground ,paper :background ,nicegrey))))
   `(hydra-face-pink ((t (:foreground ,paper :background ,nicegrey))))
   `(hydra-face-teal ((t (:foreground ,merged :background ,nicegrey))))
;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,pencil1 :background ,nicegrey))))
   `(ivy-match-required-face ((t (:foreground ,navyblue :background ,nicegrey))))
   `(ivy-remote ((t (:foreground ,paper :background ,nicegrey))))
   `(ivy-subdir ((t (:foreground ,marooned :background ,nicegrey))))
   `(ivy-current-match ((t (:foreground ,marooned :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,nicegrey))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,pencil1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,pencil1))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,paper))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,marooned :weight bold))))
   `(ido-only-match ((t (:foreground ,paper :weight bold))))
   `(ido-subdir ((t (:foreground ,marooned))))
   `(ido-indicator ((t (:foreground ,marooned :background ,paper))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,nicegrey :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,pencil1))))
   `(jabber-roster-user-online ((t (:foreground ,marine))))
   `(jabber-roster-user-dnd ((t (:foreground ,paper))))
   `(jabber-roster-user-xa ((t (:foreground ,paper))))
   `(jabber-roster-user-chatty ((t (:foreground ,paper))))
   `(jabber-roster-user-error ((t (:foreground ,paper))))
   `(jabber-rare-time-face ((t (:foreground ,paper))))
   `(jabber-chat-prompt-local ((t (:foreground ,marine))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,paper))))
   `(jabber-chat-prompt-system ((t (:foreground ,spruce))))
   `(jabber-activity-face((t (:foreground ,paper))))
   `(jabber-activity-personal-face ((t (:foreground ,marooned))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,paper))))
   `(js2-error ((t (:foreground ,navyblue :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,pencil1))))
   `(js2-jsdoc-type ((t (:foreground ,pencil1))))
   `(js2-jsdoc-value ((t (:foreground ,spruce))))
   `(js2-function-param ((t (:foreground, ink))))
   `(js2-external-variable ((t (:foreground ,paper))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,pencil1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,paper))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,paper))))
   `(js2-object-property ((t (:foreground ,marooned))))
   `(js2-magic-paren ((t (:foreground ,paper))))
   `(js2-private-function-call ((t (:foreground ,merged))))
   `(js2-function-call ((t (:foreground ,merged))))
   `(js2-private-member ((t (:foreground ,marine))))
   `(js2-keywords ((t (:foreground ,paper))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,paper :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,marooned :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,nicegrey))))
   `(ledger-font-pending-face ((t (:foreground ,paper weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,marooned))))
   `(ledger-font-posting-account-face ((t (:foreground ,marine))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,marooned))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,paper))))
   `(ledger-font-posting-amount-face ((t (:foreground ,paper))))
   `(ledger-occur-narrowed-face ((t (:foreground ,paper :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,nicegrey))))
   `(ledger-font-comment-face ((t (:foreground ,pencil1))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,paper :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,marooned :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,paper :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,paper :weight normal))))
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
     ((t (:foreground ,paper :background ,flatblue))))
   `(macrostep-gensym-3
     ((t (:foreground ,marooned :background ,flatblue))))
   `(macrostep-gensym-4
     ((t (:foreground ,paper :background ,flatblue))))
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
   `(magit-section-heading-selection   ((t (:foreground ,paper :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,nicegrey  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,nicegrey
                                                        :foreground ,paper :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,nicegrey))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,nicegrey))))
   `(magit-diff-hunk-heading-selection ((t (:background ,nicegrey
                                                        :foreground ,paper))))
   `(magit-diff-lines-heading          ((t (:background ,paper
                                                        :foreground ,nicegrey))))
   `(magit-diff-context-highlight      ((t (:background ,nicegrey
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,marine))))
   `(magit-diffstat-removed ((t (:foreground ,navyblue))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,marooned  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,pencil1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,pencil1   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,paper    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,paper  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,pencil1  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,navyblue    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,paper))))
   `(magit-log-date      ((t (:foreground ,paper))))
   `(magit-log-graph     ((t (:foreground ,paper))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,paper))))
   `(magit-sequence-stop ((t (:foreground ,pencil1))))
   `(magit-sequence-part ((t (:foreground ,marooned))))
   `(magit-sequence-head ((t (:foreground ,paper))))
   `(magit-sequence-drop ((t (:foreground ,navyblue))))
   `(magit-sequence-done ((t (:foreground ,paper))))
   `(magit-sequence-onto ((t (:foreground ,paper))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,pencil1))))
   `(magit-bisect-skip ((t (:foreground ,marooned))))
   `(magit-bisect-bad  ((t (:foreground ,navyblue))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,flatblue :foreground ,paper))))
   `(magit-blame-hash    ((t (:background ,flatblue :foreground ,paper))))
   `(magit-blame-name    ((t (:background ,flatblue :foreground ,paper))))
   `(magit-blame-date    ((t (:background ,flatblue :foreground ,paper))))
   `(magit-blame-summary ((t (:background ,flatblue :foreground ,paper
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,nicegrey))))
   `(magit-hash           ((t (:foreground ,nicegrey))))
   `(magit-tag            ((t (:foreground ,paper :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,pencil1  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,paper   :weight bold))))
   `(magit-branch-current ((t (:foreground ,paper   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,paper   :weight bold))))
   `(magit-refname        ((t (:background ,nicegrey :foreground ,marooned :weight bold))))
   `(magit-refname-stash  ((t (:background ,nicegrey :foreground ,marooned :weight bold))))
   `(magit-refname-wip    ((t (:background ,nicegrey :foreground ,marooned :weight bold))))
   `(magit-signature-good      ((t (:foreground ,pencil1))))
   `(magit-signature-bad       ((t (:foreground ,navyblue))))
   `(magit-signature-untrusted ((t (:foreground ,marooned))))
   `(magit-cherry-unmatched    ((t (:foreground ,merged))))
   `(magit-cherry-equivalent   ((t (:foreground ,paper))))
   `(magit-reflog-commit       ((t (:foreground ,pencil1))))
   `(magit-reflog-amend        ((t (:foreground ,paper))))
   `(magit-reflog-merge        ((t (:foreground ,pencil1))))
   `(magit-reflog-checkout     ((t (:foreground ,paper))))
   `(magit-reflog-reset        ((t (:foreground ,navyblue))))
   `(magit-reflog-rebase       ((t (:foreground ,paper))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,pencil1))))
   `(magit-reflog-remote       ((t (:foreground ,merged))))
   `(magit-reflog-other        ((t (:foreground ,merged))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,paper))))
   `(message-header-other ((t (:foreground ,pencil1))))
   `(message-header-to ((t (:foreground ,marooned :weight bold))))
   `(message-header-cc ((t (:foreground ,marooned :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,marooned :weight bold))))
   `(message-header-subject ((t (:foreground ,paper :weight bold))))
   `(message-header-xheader ((t (:foreground ,pencil1))))
   `(message-mml ((t (:foreground ,marooned :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,paper))))
   `(mew-face-header-from ((t (:foreground ,marooned))))
   `(mew-face-header-date ((t (:foreground ,pencil1))))
   `(mew-face-header-to ((t (:foreground ,navyblue))))
   `(mew-face-header-key ((t (:foreground ,pencil1))))
   `(mew-face-header-private ((t (:foreground ,pencil1))))
   `(mew-face-header-important ((t (:foreground ,paper))))
   `(mew-face-header-marginal ((t (:foreground ,marooned :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,navyblue))))
   `(mew-face-header-xmew ((t (:foreground ,pencil1))))
   `(mew-face-header-xmew-bad ((t (:foreground ,navyblue))))
   `(mew-face-body-url ((t (:foreground ,paper))))
   `(mew-face-body-comment ((t (:foreground ,marooned :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,pencil1))))
   `(mew-face-body-cite2 ((t (:foreground ,paper))))
   `(mew-face-body-cite3 ((t (:foreground ,paper))))
   `(mew-face-body-cite4 ((t (:foreground ,marooned))))
   `(mew-face-body-cite5 ((t (:foreground ,navyblue))))
   `(mew-face-mark-review ((t (:foreground ,paper))))
   `(mew-face-mark-escape ((t (:foreground ,pencil1))))
   `(mew-face-mark-delete ((t (:foreground ,navyblue))))
   `(mew-face-mark-unlink ((t (:foreground ,marooned))))
   `(mew-face-mark-refile ((t (:foreground ,pencil1))))
   `(mew-face-mark-unread ((t (:foreground ,paper))))
   `(mew-face-eof-message ((t (:foreground ,pencil1))))
   `(mew-face-eof-part ((t (:foreground ,marooned))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,merged :background ,nicegrey :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,nicegrey :background ,paper :weight bold))))
   `(paren-face-no-match ((t (:foreground ,nicegrey :background ,navyblue :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,paper))))
   `(mingus-pausing-face ((t (:foreground ,paper))))
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
   `(nav-face-hfile ((t (:foreground ,paper))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,paper    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,pencil1 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,paper  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,pencil1   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,paper  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,pencil1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,paper    :slant italic))))
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
     ((t (:foreground ,paper :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,marooned :weight bold))))
   `(org-checkbox ((t (:background ,pencil1 :foreground ,paper
                                   :box (:line-width -3 :color ,pencil1 :style released-button)))))
   `(org-date ((t (:foreground ,paper :underline t))))
   `(org-deadline-announce ((t (:foreground ,paper))))
   `(org-done ((t (:bold t :weight bold :foreground ,closedred))))
   `(org-formula ((t (:foreground ,paper))))
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
   `(org-link ((t (:foreground ,paper :underline t))))
   `(org-scheduled ((t (:foreground ,marine))))
   `(org-scheduled-previously ((t (:foreground ,navyblue))))
   `(org-scheduled-today ((t (:foreground ,marooned))))
   `(org-sexp-date ((t (:foreground ,marooned :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,pencil1))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,paper))))
   `(org-todo ((t (:bold t :foreground ,navyblue :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,navyblue :weight bold :underline nil))))
   `(org-column ((t (:background ,flatblue))))
   `(org-column-title ((t (:background ,flatblue :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,marooned :background ,flatblue))))
   `(org-mode-line-clock-overrun ((t (:foreground ,nicegrey :background ,paper))))
   `(org-ellipsis ((t (:foreground ,paper :underline t))))
   `(org-footnote ((t (:foreground ,merged :underline t))))
   `(org-document-title ((t (:inherit org-level-1 :height 2.0 :underline nil :box (:line-width 5 :color ,nicegrey)))))
   `(org-document-info ((t (:foreground ,paper))))
   `(org-habit-ready-face ((t :background ,pencil1)))
   `(org-habit-alert-face ((t :background ,paper :foreground ,nicegrey)))
   `(org-habit-clear-face ((t :background ,paper)))
   `(org-habit-overdue-face ((t :background ,paper)))
   `(org-habit-clear-future-face ((t :background ,paper)))
   `(org-habit-ready-future-face ((t :background ,pencil1)))
   `(org-habit-alert-future-face ((t :background ,paper :foreground ,nicegrey)))
   `(org-habit-overdue-future-face ((t :background ,paper)))
;;;;; outline
   `(outline-1 ((t (:foreground ,paper))))
   `(outline-2 ((t (:foreground ,marine))))
   `(outline-3 ((t (:foreground ,marine))))
   `(outline-4 ((t (:foreground ,paper))))
   `(outline-5 ((t (:foreground ,merged))))
   `(outline-6 ((t (:foreground ,pencil1))))
   `(outline-7 ((t (:foreground ,paper))))
   `(outline-8 ((t (:foreground ,paper))))
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
   `(persp-selected-face ((t (:foreground ,paper :inherit mode-line))))
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
   `(proof-eager-annotation-face ((t (:foreground ,nicegrey :background ,paper))))
   `(proof-error-face ((t (:foreground ,marooned :background ,paper))))
   `(proof-highlight-dependency-face ((t (:foreground ,nicegrey :background ,paper))))
   `(proof-highlight-dependent-face ((t (:foreground ,nicegrey :background ,paper))))
   `(proof-locked-face ((t (:background ,paper))))
   `(proof-mouse-highlight-face ((t (:foreground ,nicegrey :background ,paper))))
   `(proof-queue-face ((t (:background ,paper))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,paper))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,nicegrey))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,nicegrey))))
   `(proof-warning-face ((t (:foreground ,nicegrey :background ,paper))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,marooned))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,marine))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,paper))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,merged))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,pencil1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,marooned))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,paper))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,paper))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,paper))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,paper))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,pencil1))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,paper))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,paper))))
   `(rcirc-other-nick ((t (:foreground ,paper))))
   `(rcirc-bright-nick ((t (:foreground ,marooned))))
   `(rcirc-dim-nick ((t (:foreground ,paper))))
   `(rcirc-server ((t (:foreground ,pencil1))))
   `(rcirc-server-prefix ((t (:foreground ,paper))))
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
   `(rpm-spec-tag-face ((t (:foreground ,paper))))
   `(rpm-spec-var-face ((t (:foreground ,navyblue))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,paper))))
   `(rst-level-2-face ((t (:foreground ,paper))))
   `(rst-level-3-face ((t (:foreground ,marine))))
   `(rst-level-4-face ((t (:foreground ,paper))))
   `(rst-level-5-face ((t (:foreground ,merged))))
   `(rst-level-6-face ((t (:foreground ,pencil1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,marooned :bold t))))
   `(sh-quoted-exec ((t (:foreground ,navyblue))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,paper :background ,nicegrey))))
   `(show-paren-match ((t (:foreground ,paper :background ,melon :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Elixir Dark for sml
   `(sml/global ((,class (:foreground ,marooned :weight bold))))
   `(sml/modes ((,class (:foreground ,marooned :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,paper :weight bold))))
   `(sml/filename ((,class (:foreground ,marooned :weight bold))))
   `(sml/line-number ((,class (:foreground ,paper :weight bold))))
   `(sml/col-number ((,class (:foreground ,marooned :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,marine :weight bold))))
   `(sml/prefix ((,class (:foreground ,paper))))
   `(sml/git ((,class (:foreground ,spruce))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,paper :weight bold))))
   `(sml/read-only ((,class (:foreground ,paper))))
   `(sml/outside-modified ((,class (:foreground ,paper))))
   `(sml/modified ((,class (:foreground ,navyblue))))
   `(sml/vc-edited ((,class (:foreground ,pencil1))))
   `(sml/charging ((,class (:foreground ,marine))))
   `(sml/discharging ((,class (:foreground ,paper))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,paper :background ,nicegrey :weight bold))))
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
       (:underline (:style wave :color ,paper)))
      (t
       (:underline ,paper))))
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
   `(term-color-red ((t (:foreground ,paper
                                     :background ,paper))))
   `(term-color-green ((t (:foreground ,pencil1
                                       :background ,pencil1))))
   `(term-color-yellow ((t (:foreground ,paper
                                        :background ,marooned))))
   `(term-color-blue ((t (:foreground ,marine
                                      :background ,paper))))
   `(term-color-magenta ((t (:foreground ,paper
                                         :background ,navyblue))))
   `(term-color-cyan ((t (:foreground ,merged
                                      :background ,paper))))
   `(term-color-white ((t (:foreground ,marooned
                                       :background ,paper))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,paper :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,paper :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,marooned))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,marooned))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,merged))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,sand))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,paper ))))
   `(web-mode-css-prop-face ((t (:foreground ,paper))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,spruce :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,paper))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,paper))))
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
   `(whitespace-space-before-tab ((t (:background ,paper :foreground ,paper))))
   `(whitespace-indentation ((t (:background ,nicegrey))))
   `(whitespace-empty ((t (:background ,whisp1))))
   `(whitespace-space-after-tab ((t (:background ,whisp1 :foreground ,navyblue))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,paper))))
   `(wl-highlight-folder-many-face ((t (:foreground ,paper))))
   `(wl-highlight-folder-path-face ((t (:foreground ,paper))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,paper))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,marooned))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,paper))))
   `(wl-highlight-message-citation-header ((t (:foreground ,paper))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,navyblue))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,pencil1))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,paper))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,marooned))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,pencil1))))
   `(wl-highlight-message-headers-face ((t (:foreground ,paper))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,pencil1))))
   `(wl-highlight-message-header-contents ((t (:foreground ,paper))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,pencil1))))
   `(wl-highlight-message-signature ((t (:foreground ,pencil1))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,marooned))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,paper))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,marooned
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,paper))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,marooned))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,marooned))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,paper))))
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
                                          ,paper ,paper ,merged ,marooned])
;;;;; fill-column-indicator
   `(fci-rule-color ,sand)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,navyblue ,paper ,marooned ,pencil1 ,marine
                   ,merged ,marooned ,paper))
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
