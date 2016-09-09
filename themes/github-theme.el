;;; github-theme.el
;;; Code:

(deftheme github "GitHub color theme")

;;; Color Palette

(defvar github-default-colors-alist
  '(("ink"           . "#333333")
    ("github-fg"     . "#A71D5D")
    ("paper"         . "#ffffff")
    ("github-bg-1"   . "#b0cde7")
    ("github-bg-05"  . "#f8eec7")
    ("github-red"    . "#183691")
    ("github-yellow" . "#A71D5D")
    ("pencil1"       . "#969896")
    ("github-green+3". "#63a35c")
    ("github-green+4". "#0085B3")
    ("github-cyan"   . "#7265A2")
    ("github-blue+1" . "#A71D5D")
    ("github-blue-1" . "#0086b0"))
  "List of GitHub colors.
Each element has the form (NAME . HEX).")

;;; Better Colors

;; navyblue  "#183691" (font-lock-string-face foreground)
;; bigblue   "#3873C3" (minibuffer-prompt foreground)
;; aquablue  "#C2E7F3" (mode-line box outline)
;; skyblue   "#E4F1F7" (isearch background)
;; bluegrey  "#ECF3F7" (mode-line background)
;; aqua      "#DEFDFE" (show-paren-match background)
;; paleblue  "#F1F9FC"
;; fluff     "#F4F7FB" (diff-header background)
;; marine    "#0085B3" (font-lock-type-face foreground)
;; greentea  "#29D045" (info-node background)
;; leaf      "#83FCA7" (ediff-fine-diff-B)
;; melon     "#E4FFEA" (ediff-current-diff-B)
;; limeclay  "#F2F9DB" (lazy-highlight background)
;; bloodred  "#FF4421" (flyspell-incorrect underline)
;; closedred "#BD3128"
;; marooned  "#A71D5D" (font-lock-preprocessor-face foreground)
;; merged    "#6E5494"
;; eraser    "#FFC8C9" (ediff-fine-diff-A)
;; palepink  "#FFEBEB" (ediff-current-diff-A)
;; ink       "#333333" (cursor background)
;; slate     "#666666" (mode-line foreground)
;; pencil1   "#969896" (comments foreground)
;; pencil2   "#B3B3B3" (linum)
;; pencil3   "#D8D8D8" (mode-line-inactive outline)
;; whisp1    "#EDEDED" (whitespace-tab background)
;; whisp2    "#F5F5F5" (mode-line-inactive background)
;; whisp3    "#F8F8F8" (hl-line background)

(defvar github-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar github-colors-alist
  (append github-default-colors-alist github-override-colors-alist))

(defmacro github-with-color-variables (&rest body)
  "`let' bind all colors defined in `github-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   github-colors-alist))
     ,@body))

;;; Theme Faces
(github-with-color-variables
  (custom-theme-set-faces
   'github
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground "#3873C3" :underline t :weight bold))))
   `(link-visited ((t (:foreground ,ink :underline t :weight normal))))
   `(default ((t (:family "Source Code Pro" :height 140 :foreground ,ink :background "white"))))
   `(fixed-pitch ((t (:family "Source Code Pro"))))
   `(variable-pitch ((t (:family "Avenir Next" :height 170))))
   `(cursor ((t (:background "#333333"))))
   `(escape-glyph ((t (:foreground ,github-yellow :bold t))))
   `(fringe ((t (:foreground "#B3B3B3" :background ,paper))))
   `(header-line ((t (:foreground "#969896" :background "#F5F5F5"))))
   `(info-node ((t (:foreground "#29D045" :slant italic :weight bold))))
   `(highlight ((t (:background "#F8F8F8"))))
   `(success ((t (:foreground ,pencil1 :weight bold))))
   `(warning ((t (:foreground ,ink :weight bold))))
   `(tooltip ((t (:foreground ,github-fg :background ,paper))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,github-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,pencil1))))
   `(compilation-error-face ((t (:foreground ,ink :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,github-fg))))
   `(compilation-info-face ((t (:foreground ,ink))))
   `(compilation-info ((t (:foreground ,github-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,pencil1))))
   `(compilation-line-face ((t (:foreground ,github-yellow))))
   `(compilation-line-number ((t (:foreground ,github-yellow))))
   `(compilation-message-face ((t (:foreground ,ink))))
   `(compilation-warning-face ((t (:foreground ,ink :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,pencil1 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,github-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,github-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,ink))))
;;;;; grep
   `(match ((t (:background "#F5F5F5" :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,ink :weight bold :background "#E4F1F7"))))
   `(isearch-fail ((t (:background "#FFC8C9"))))
   `(lazy-highlight ((t (:foreground ,ink :weight bold :background "#F2F9DB"))))

   `(menu ((t (:foreground ,github-fg :background ,paper))))
   `(minibuffer-prompt ((t (:foreground "#333333" :weight bold))))
   `(mode-line
     ((,class (:foreground "#666666"
                           :background "#ECF3F7" ;; homerow background
                           :box (:line-width -1 :color "#C2E7F3")
                           ))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground "#000000" :weight bold)))) ;; todo: not happy with black here
   `(mode-line-highlight ((t (:foreground "#3873C3"))))
   `(mode-line-inactive
     ((t (:foreground "#969896"
                      :weight light
                      :background "#F5F5F5"
                      :box (:line-width -1 :color "#d8d8d8")))))
   `(region ((,class (:background "#E8F1F6"))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,paper))))
   `(trailing-whitespace ((t (:background "#EDEDED"))))
   `(vertical-border ((t (:foreground "#d0d0d0"))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,github-fg :weight normal))))  ;; weight bold (built-in keywords)
   `(font-lock-comment-face ((t (:foreground ,pencil1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,pencil1))))
   `(font-lock-constant-face ((t (:foreground "#0085B3"))))
   `(font-lock-doc-face ((t (:foreground ,pencil1))))
   `(font-lock-function-name-face ((t (:foreground "#7265A2"))))
   `(font-lock-keyword-face ((t (:foreground "#A71D5D" :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,github-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground "#A71D5D"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,github-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,pencil1 :weight bold))))
   `(font-lock-string-face ((t (:foreground "#183691"))))
   `(font-lock-type-face ((t (:foreground "#0086b0"))))
   `(font-lock-variable-name-face ((t (:foreground ,ink))))
   `(font-lock-warning-face ((t (:foreground ,ink :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,github-fg))))
   `(newsticker-default-face ((t (:foreground ,github-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,github-green+3))))
   `(newsticker-extra-face ((t (:foreground ,paper :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,github-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,pencil1))))
   `(newsticker-new-item-face ((t (:foreground ,ink))))
   `(newsticker-obsolete-item-face ((t (:foreground ,github-red))))
   `(newsticker-old-item-face ((t (:foreground ,paper))))
   `(newsticker-statistics-face ((t (:foreground ,github-fg))))
   `(newsticker-treeview-face ((t (:foreground ,github-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,pencil1))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,github-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,ink :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,github-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,paper))))
   `(newsticker-treeview-selection-face ((t (:background ,github-bg-1 :foreground ,github-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,ink :background ,paper :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,pencil1 :background ,paper :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,ink))))
   `(android-mode-error-face ((t (:foreground ,ink :weight bold))))
   `(android-mode-info-face ((t (:foreground ,github-fg))))
   `(android-mode-verbose-face ((t (:foreground ,pencil1))))
   `(android-mode-warning-face ((t (:foreground ,github-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,github-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,github-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,github-yellow))))
   `(font-latex-italic-face ((t (:foreground ,github-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,ink))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,github-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,github-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,ink))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,github-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,github-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,github-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,ink))))
   `(agda2-highlight-function-face ((t (:foreground ,ink))))
   `(agda2-highlight-module-face ((t (:foreground ,github-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,paper :background ,ink))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,paper :background ,ink))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,paper :background ,ink))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,paper :background ,ink))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,paper :background ,ink))))
   `(agda2-highlight-typechecks-face ((t (:background ,ink))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background "#ffffff" :foreground "#333333" :box (:line-width -1 :style released-button)))))
   `(ac-selection-face ((t (:background "#A71D5D" :foreground "#ffffff" :box (:line-width -1 :style released-button)))))
   `(popup-tip-face ((t (:background "#ff0000" :foreground "#00ff00" ))))
   `(popup-scroll-bar-foreground-face ((t (:background "#333333"))))
   `(popup-scroll-bar-background-face ((t (:background "#ffffff"))))
   `(popup-isearch-match ((t (:background "#ff00ff" :foreground "#00ffff"))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,ink :background ,paper :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,github-green+3 :background ,paper :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,github-yellow :background ,paper :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,ink :background ,paper :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,github-cyan :background ,paper :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,github-fg :background ,paper))))
   `(company-tooltip-annotation ((t (:foreground ,ink :background ,paper))))
   `(company-tooltip-annotation-selection ((t (:foreground ,ink :background ,github-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,github-fg :background ,github-bg-1))))
   `(company-tooltip-mouse ((t (:background ,github-bg-1))))
   `(company-tooltip-common ((t (:foreground ,pencil1))))
   `(company-tooltip-common-selection ((t (:foreground ,pencil1))))
   `(company-scrollbar-fg ((t (:background ,github-bg-1))))
   `(company-scrollbar-bg ((t (:background ,paper))))
   `(company-preview ((t (:background ,pencil1))))
   `(company-preview-common ((t (:foreground ,pencil1 :background ,github-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,ink :foreground ,paper))))
   `(bm-fringe-face ((t (:background ,ink :foreground ,paper))))
   `(bm-fringe-persistent-face ((t (:background ,pencil1 :foreground ,paper))))
   `(bm-persistent-face ((t (:background ,pencil1 :foreground ,paper))))
;;;;; cider
   `(cider-result-overlay-face ((t (:foreground ,ink :background unspecified))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,github-cyan))))
   `(circe-my-message-face ((t (:foreground ,github-fg))))
   `(circe-fool-face ((t (:foreground ,ink))))
   `(circe-topic-diff-removed-face ((t (:foreground ,github-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,github-fg))))
   `(circe-server-face ((t (:foreground ,pencil1))))
   `(circe-topic-diff-new-face ((t (:foreground ,ink :weight bold))))
   `(circe-prompt-face ((t (:foreground ,ink :background ,paper :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,github-fg)))
   `(context-coloring-level-1-face ((t :foreground ,github-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,github-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,github-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,ink)))
   `(context-coloring-level-5-face ((t :foreground ,ink)))
   `(context-coloring-level-6-face ((t :foreground ,github-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,pencil1)))
   `(context-coloring-level-8-face ((t :foreground ,ink)))
   `(context-coloring-level-9-face ((t :foreground ,ink)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,ink :foreground ,paper))))
   `(ctbl:face-continue-bar ((t (:background ,github-bg-05 :foreground ,paper))))
   `(ctbl:face-row-select ((t (:background ,github-cyan :foreground ,paper))))
;;;;; diff
   `(diff-added          ((t (:background "#E4FFEA"))))
   `(diff-changed        ((t (:background "#E4F1F7"))))
   `(diff-removed        ((t (:background "#FFEBEB"))))
   `(diff-refine-added   ((t (:background "#83FCA7"))))
   `(diff-refine-change  ((t (:background "#C2E7F3"))))
   `(diff-refine-removed ((t (:background "#FFC8C9"))))
   `(diff-header         ((t (:background "#F4F7FB" :foreground "#969896"))))
   `(diff-file-header    ((t (:background "#F4F7FB" :foreground "#969896"))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,ink :background ,ink))))
   `(diff-hl-delete ((,class (:foreground ,ink :background ,ink))))
   `(diff-hl-insert ((,class (:foreground ,ink :background ,pencil1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,paper)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,ink))))
   `(diredp-compressed-file-suffix ((t (:foreground ,ink))))
   `(diredp-date-time ((t (:foreground ,ink))))
   `(diredp-deletion ((t (:foreground ,github-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,github-red))))
   `(diredp-dir-heading ((t (:foreground ,ink :background ,github-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,github-cyan))))
   `(diredp-exec-priv ((t (:foreground ,github-red))))
   `(diredp-executable-tag ((t (:foreground ,ink))))
   `(diredp-file-name ((t (:foreground ,ink))))
   `(diredp-file-suffix ((t (:foreground ,pencil1))))
   `(diredp-flag-mark ((t (:foreground ,github-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,ink))))
   `(diredp-ignored-file-name ((t (:foreground ,github-red))))
   `(diredp-link-priv ((t (:foreground ,github-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,github-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,ink))))
   `(diredp-no-priv ((t (:foreground ,github-fg))))
   `(diredp-number ((t (:foreground ,ink))))
   `(diredp-other-priv ((t (:foreground ,ink))))
   `(diredp-rare-priv ((t (:foreground ,ink))))
   `(diredp-read-priv ((t (:foreground ,pencil1))))
   `(diredp-symlink ((t (:foreground ,github-yellow))))
   `(diredp-write-priv ((t (:foreground ,ink))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:background "#FFEBEB"))))
   `(ediff-current-diff-B ((t (:background "#E4FFEA"))))
   `(ediff-even-diff-A ((t (:background "#F4F7FB"))))
   `(ediff-even-diff-B ((t (:background "#F4F7FB"))))
   `(ediff-fine-diff-A ((t (:background "#FFC8C9"))))
   `(ediff-fine-diff-B ((t (:background "#83FCA7"))))
   `(ediff-odd-diff-A ((t (:background "#F4F7FB"))))
   `(ediff-odd-diff-B ((t (:background "#F4F7FB"))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,github-fg))))
   `(egg-help-header-1 ((t (:foreground ,github-yellow))))
   `(egg-help-header-2 ((t (:foreground ,github-green+3))))
   `(egg-branch ((t (:foreground ,github-yellow))))
   `(egg-branch-mono ((t (:foreground ,github-yellow))))
   `(egg-term ((t (:foreground ,github-yellow))))
   `(egg-diff-add ((t (:foreground ,github-green+4))))
   `(egg-diff-del ((t (:foreground ,ink))))
   `(egg-diff-file-header ((t (:foreground ,ink))))
   `(egg-section-title ((t (:foreground ,github-yellow))))
   `(egg-stash-mono ((t (:foreground ,github-green+4))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,ink :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,pencil1))))
   `(elfeed-search-feed-face ((t (:foreground ,github-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,github-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,ink
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,ink :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,github-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,pencil1 :background ,paper))))
   `(w3m-lnum-match ((t (:background ,github-bg-1
                                     :foreground ,ink
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,github-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,ink :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,github-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,github-yellow))))
   `(erc-keyword-face ((t (:foreground ,ink :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,github-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,github-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,pencil1))))
   `(erc-pal-face ((t (:foreground ,ink :weight bold))))
   `(erc-prompt-face ((t (:foreground ,ink :background ,paper :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,github-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,github-green+4 :background ,paper))))
   `(ert-test-result-unexpected ((t (:foreground ,github-red :background ,paper))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,github-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,ink :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,github-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,ink :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,github-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,github-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,github-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,pencil1 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ink) :inherit unspecified))
      (t (:foreground ,ink :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-yellow) :inherit unspecified))
      (t (:foreground ,github-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-cyan) :inherit unspecified))
      (t (:foreground ,github-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,ink :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,github-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,github-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,ink :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ink)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,ink :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,pencil1)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,pencil1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ink) :inherit unspecified))
      (t (:foreground ,ink :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "#FF4421") :inherit unspecified))
      (t (:foreground ,ink :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,github-fg))))
   `(ack-file ((t (:foreground ,ink))))
   `(ack-line ((t (:foreground ,github-yellow))))
   `(ack-match ((t (:foreground ,ink :background ,github-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,ink :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,github-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,github-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,pencil1 :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,github-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,ink :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,github-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,pencil1  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,github-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,ink :weight bold))))
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
   `(gnus-server-denied ((t (:foreground ,ink :weight bold))))
   `(gnus-server-closed ((t (:foreground ,ink :slant italic))))
   `(gnus-server-offline ((t (:foreground ,github-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,ink :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,ink))))
   `(gnus-summary-high-ancient ((t (:foreground ,ink))))
   `(gnus-summary-high-read ((t (:foreground ,pencil1 :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,ink :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,github-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,ink))))
   `(gnus-summary-low-read ((t (:foreground ,pencil1))))
   `(gnus-summary-low-ticked ((t (:foreground ,ink :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,github-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,ink))))
   `(gnus-summary-normal-read ((t (:foreground ,pencil1))))
   `(gnus-summary-normal-ticked ((t (:foreground ,ink :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,github-fg))))
   `(gnus-summary-selected ((t (:foreground ,github-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,ink))))
   `(gnus-cite-10 ((t (:foreground ,ink))))
   `(gnus-cite-11 ((t (:foreground ,github-yellow))))
   `(gnus-cite-2 ((t (:foreground ,github-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,ink))))
   `(gnus-cite-4 ((t (:foreground ,pencil1))))
   `(gnus-cite-5 ((t (:foreground ,ink))))
   `(gnus-cite-6 ((t (:foreground ,pencil1))))
   `(gnus-cite-7 ((t (:foreground ,github-red))))
   `(gnus-cite-8 ((t (:foreground ,ink))))
   `(gnus-cite-9 ((t (:foreground ,ink))))
   `(gnus-group-news-1-empty ((t (:foreground ,github-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,github-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,ink))))
   `(gnus-group-news-4-empty ((t (:foreground ,ink))))
   `(gnus-group-news-5-empty ((t (:foreground ,ink))))
   `(gnus-group-news-6-empty ((t (:foreground ,paper))))
   `(gnus-group-news-low-empty ((t (:foreground ,paper))))
   `(gnus-signature ((t (:foreground ,github-yellow))))
   `(gnus-x ((t (:background ,github-fg :foreground ,paper))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,ink))))
   `(guide-key/key-face ((t (:foreground ,pencil1))))
   `(guide-key/prefix-command-face ((t (:foreground ,ink))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,pencil1
                      :background ,paper
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,github-yellow
                      :background ,github-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,paper :underline nil))))
   `(helm-selection-line ((t (:background ,paper))))
   `(helm-visible-mark ((t (:foreground ,paper :background ,ink))))
   `(helm-candidate-number ((t (:foreground ,github-green+4 :background ,github-bg-1))))
   `(helm-separator ((t (:foreground ,github-red :background ,paper))))
   `(helm-time-zone-current ((t (:foreground ,pencil1 :background ,paper))))
   `(helm-time-zone-home ((t (:foreground ,github-red :background ,paper))))
   `(helm-bookmark-addressbook ((t (:foreground ,ink :background ,paper))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,ink :background ,paper))))
   `(helm-bookmark-info ((t (:foreground ,pencil1 :background ,paper))))
   `(helm-bookmark-man ((t (:foreground ,github-yellow :background ,paper))))
   `(helm-bookmark-w3m ((t (:foreground ,ink :background ,paper))))
   `(helm-buffer-not-saved ((t (:foreground ,github-red :background ,paper))))
   `(helm-buffer-process ((t (:foreground ,github-cyan :background ,paper))))
   `(helm-buffer-saved-out ((t (:foreground ,github-fg :background ,paper))))
   `(helm-buffer-size ((t (:foreground ,ink :background ,paper))))
   `(helm-ff-directory ((t (:foreground ,github-cyan :background ,paper :weight bold))))
   `(helm-ff-file ((t (:foreground ,github-fg :background ,paper :weight normal))))
   `(helm-ff-executable ((t (:foreground ,pencil1 :background ,paper :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,github-red :background ,paper :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,github-yellow :background ,paper :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,paper :background ,github-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,github-cyan :background ,paper))))
   `(helm-grep-file ((t (:foreground ,github-fg :background ,paper))))
   `(helm-grep-finish ((t (:foreground ,pencil1 :background ,paper))))
   `(helm-grep-lineno ((t (:foreground ,ink :background ,paper))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,github-red :background ,paper))))
   `(helm-match ((t (:foreground ,ink :background ,github-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,github-cyan :background ,paper))))
   `(helm-mu-contacts-address-face ((t (:foreground ,ink :background ,paper))))
   `(helm-mu-contacts-name-face ((t (:foreground ,github-fg :background ,paper))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,github-fg :background ,paper))))
   `(helm-swoop-target-word-face ((t (:foreground ,github-yellow :background ,paper :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background "#F8F8F8"))
                   (t :weight bold)))
   `(hl-line ((,class (:background "#F8F8F8")) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,paper))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,ink :background ,paper))))
   `(hydra-face-amaranth ((t (:foreground ,ink :background ,paper))))
   `(hydra-face-blue ((t (:foreground ,ink :background ,paper))))
   `(hydra-face-pink ((t (:foreground ,ink :background ,paper))))
   `(hydra-face-teal ((t (:foreground ,github-cyan :background ,paper))))
;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,pencil1 :background ,paper))))
   `(ivy-match-required-face ((t (:foreground ,github-red :background ,paper))))
   `(ivy-remote ((t (:foreground ,ink :background ,paper))))
   `(ivy-subdir ((t (:foreground ,github-yellow :background ,paper))))
   `(ivy-current-match ((t (:foreground ,github-yellow :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,paper))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,pencil1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,pencil1))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,ink))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,github-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,ink :weight bold))))
   `(ido-subdir ((t (:foreground ,github-yellow))))
   `(ido-indicator ((t (:foreground ,github-yellow :background ,ink))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,paper :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,pencil1))))
   `(jabber-roster-user-online ((t (:foreground ,github-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,ink))))
   `(jabber-roster-user-xa ((t (:foreground ,ink))))
   `(jabber-roster-user-chatty ((t (:foreground ,ink))))
   `(jabber-roster-user-error ((t (:foreground ,ink))))
   `(jabber-rare-time-face ((t (:foreground ,ink))))
   `(jabber-chat-prompt-local ((t (:foreground ,github-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,ink))))
   `(jabber-chat-prompt-system ((t (:foreground ,github-green+3))))
   `(jabber-activity-face((t (:foreground ,ink))))
   `(jabber-activity-personal-face ((t (:foreground ,github-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,ink))))
   `(js2-error ((t (:foreground ,github-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,pencil1))))
   `(js2-jsdoc-type ((t (:foreground ,pencil1))))
   `(js2-jsdoc-value ((t (:foreground ,github-green+3))))
   `(js2-function-param ((t (:foreground, ink))))
   `(js2-external-variable ((t (:foreground ,ink))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,pencil1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,ink))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,ink))))
   `(js2-object-property ((t (:foreground ,github-blue+1))))
   `(js2-magic-paren ((t (:foreground ,ink))))
   `(js2-private-function-call ((t (:foreground ,github-cyan))))
   `(js2-function-call ((t (:foreground ,github-cyan))))
   `(js2-private-member ((t (:foreground ,github-blue-1))))
   `(js2-keywords ((t (:foreground ,ink))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,ink :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,github-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,paper))))
   `(ledger-font-pending-face ((t (:foreground ,ink weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,github-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,github-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,github-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,ink))))
   `(ledger-font-posting-amount-face ((t (:foreground ,ink))))
   `(ledger-occur-narrowed-face ((t (:foreground ,ink :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,paper))))
   `(ledger-font-comment-face ((t (:foreground ,pencil1))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,ink :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,github-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,ink :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,ink :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground "#B3B3B3" :background ,paper))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,github-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,github-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,github-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,pencil1 :background ,paper))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,github-blue-1))))
   `(lui-hilight-face ((t (:foreground ,pencil1 :background ,paper))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,pencil1 :background ,github-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,ink :background ,github-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,github-blue+1 :background ,github-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,ink :background ,github-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,github-yellow :background ,github-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,paper))))
   `(magit-section-heading             ((t (:foreground ,github-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,ink :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,paper  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,paper
                                                        :foreground ,ink :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,paper))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,paper))))
   `(magit-diff-hunk-heading-selection ((t (:background ,paper
                                                        :foreground ,ink))))
   `(magit-diff-lines-heading          ((t (:background ,ink
                                                        :foreground ,paper))))
   `(magit-diff-context-highlight      ((t (:background ,paper
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,github-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,github-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,github-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,pencil1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,pencil1   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,ink    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,ink  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,pencil1  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,github-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,ink))))
   `(magit-log-date      ((t (:foreground ,ink))))
   `(magit-log-graph     ((t (:foreground ,ink))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,ink))))
   `(magit-sequence-stop ((t (:foreground ,pencil1))))
   `(magit-sequence-part ((t (:foreground ,github-yellow))))
   `(magit-sequence-head ((t (:foreground ,ink))))
   `(magit-sequence-drop ((t (:foreground ,github-red))))
   `(magit-sequence-done ((t (:foreground ,ink))))
   `(magit-sequence-onto ((t (:foreground ,ink))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,pencil1))))
   `(magit-bisect-skip ((t (:foreground ,github-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,github-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,github-bg-1 :foreground ,ink))))
   `(magit-blame-hash    ((t (:background ,github-bg-1 :foreground ,ink))))
   `(magit-blame-name    ((t (:background ,github-bg-1 :foreground ,ink))))
   `(magit-blame-date    ((t (:background ,github-bg-1 :foreground ,ink))))
   `(magit-blame-summary ((t (:background ,github-bg-1 :foreground ,ink
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,paper))))
   `(magit-hash           ((t (:foreground ,paper))))
   `(magit-tag            ((t (:foreground ,ink :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,pencil1  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,ink   :weight bold))))
   `(magit-branch-current ((t (:foreground ,ink   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,ink   :weight bold))))
   `(magit-refname        ((t (:background ,paper :foreground ,github-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,paper :foreground ,github-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,paper :foreground ,github-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,pencil1))))
   `(magit-signature-bad       ((t (:foreground ,github-red))))
   `(magit-signature-untrusted ((t (:foreground ,github-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,github-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,ink))))
   `(magit-reflog-commit       ((t (:foreground ,pencil1))))
   `(magit-reflog-amend        ((t (:foreground ,ink))))
   `(magit-reflog-merge        ((t (:foreground ,pencil1))))
   `(magit-reflog-checkout     ((t (:foreground ,ink))))
   `(magit-reflog-reset        ((t (:foreground ,github-red))))
   `(magit-reflog-rebase       ((t (:foreground ,ink))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,pencil1))))
   `(magit-reflog-remote       ((t (:foreground ,github-cyan))))
   `(magit-reflog-other        ((t (:foreground ,github-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,ink))))
   `(message-header-other ((t (:foreground ,pencil1))))
   `(message-header-to ((t (:foreground ,github-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,github-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,github-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,ink :weight bold))))
   `(message-header-xheader ((t (:foreground ,pencil1))))
   `(message-mml ((t (:foreground ,github-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,ink))))
   `(mew-face-header-from ((t (:foreground ,github-yellow))))
   `(mew-face-header-date ((t (:foreground ,pencil1))))
   `(mew-face-header-to ((t (:foreground ,github-red))))
   `(mew-face-header-key ((t (:foreground ,pencil1))))
   `(mew-face-header-private ((t (:foreground ,pencil1))))
   `(mew-face-header-important ((t (:foreground ,ink))))
   `(mew-face-header-marginal ((t (:foreground ,github-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,github-red))))
   `(mew-face-header-xmew ((t (:foreground ,pencil1))))
   `(mew-face-header-xmew-bad ((t (:foreground ,github-red))))
   `(mew-face-body-url ((t (:foreground ,ink))))
   `(mew-face-body-comment ((t (:foreground ,github-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,pencil1))))
   `(mew-face-body-cite2 ((t (:foreground ,ink))))
   `(mew-face-body-cite3 ((t (:foreground ,ink))))
   `(mew-face-body-cite4 ((t (:foreground ,github-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,github-red))))
   `(mew-face-mark-review ((t (:foreground ,ink))))
   `(mew-face-mark-escape ((t (:foreground ,pencil1))))
   `(mew-face-mark-delete ((t (:foreground ,github-red))))
   `(mew-face-mark-unlink ((t (:foreground ,github-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,pencil1))))
   `(mew-face-mark-unread ((t (:foreground ,ink))))
   `(mew-face-eof-message ((t (:foreground ,pencil1))))
   `(mew-face-eof-part ((t (:foreground ,github-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,github-cyan :background ,paper :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,paper :background ,ink :weight bold))))
   `(paren-face-no-match ((t (:foreground ,paper :background ,github-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,ink))))
   `(mingus-pausing-face ((t (:foreground ,ink))))
   `(mingus-playing-face ((t (:foreground ,github-cyan))))
   `(mingus-playlist-face ((t (:foreground ,github-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,github-yellow))))
   `(mingus-stopped-face ((t (:foreground ,github-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,github-yellow))))
   `(nav-face-button-num ((t (:foreground ,github-cyan))))
   `(nav-face-dir ((t (:foreground ,pencil1))))
   `(nav-face-hdir ((t (:foreground ,github-red))))
   `(nav-face-file ((t (:foreground ,github-fg))))
   `(nav-face-hfile ((t (:foreground ,ink))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,ink    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,pencil1 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,ink  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,pencil1   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,ink  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,pencil1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,ink    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,paper))))
   `(mu4e-trashed-face ((t (:foreground ,paper :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,github-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,paper))))
   `(mumamo-background-chunk-submode3 ((t (:background ,paper))))
   `(mumamo-background-chunk-submode4 ((t (:background ,paper))))
;;;;; org-mode
   `(org-block-begin-line
     ((t (:foreground "#B3B3B3" :family "Source Code Pro" :height 140))))
   `(org-block-end-line
     ((t (:foreground "#B3B3B3" :family "Source Code Pro" :height 140))))
   `(org-block ((t (:background "#F5F5F5" :family "Source Code Pro" :height 140))))
   `(org-code  ((t (:background "#F5F5F5" :family "Source Code Pro" :height 140))))
   `(org-agenda-date-today
     ((t (:foreground ,ink :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,github-fg :weight bold))))
   `(org-checkbox ((t (:background "#93A1A1" :foreground "#333333"
                                   :box (:line-width -3 :color "#93A1A1" :style released-button)))))
   `(org-date ((t (:foreground ,ink :underline t))))
   `(org-deadline-announce ((t (:foreground ,ink))))
   `(org-done ((t (:bold t :weight bold :foreground "#BD3128"))))
   `(org-formula ((t (:foreground ,ink))))
   `(org-headline-done ((t (:strike-through t))))
   `(org-hide ((t (:foreground ,github-bg-1))))
   `(org-level-1 ((t (:inherit default :height 1.2))))
   `(org-level-2 ((t (:inherit default :height 1.1))))
   `(org-level-3 ((t (:inherit default))))
   `(org-level-4 ((t :inherit default)))
   `(org-level-5 ((t :inherit default)))
   `(org-level-6 ((t :inherit default)))
   `(org-level-7 ((t :inherit default)))
   `(org-level-8 ((t :inherit default)))
   `(org-link ((t (:foreground ,ink :underline t))))
   `(org-scheduled ((t (:foreground ,github-green+4))))
   `(org-scheduled-previously ((t (:foreground ,github-red))))
   `(org-scheduled-today ((t (:foreground ,github-blue+1))))
   `(org-sexp-date ((t (:foreground ,github-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,pencil1))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,ink))))
   `(org-todo ((t (:bold t :foreground ,github-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,github-red :weight bold :underline nil))))
   `(org-column ((t (:background ,github-bg-1))))
   `(org-column-title ((t (:background ,github-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,github-fg :background ,github-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,paper :background ,ink))))
   `(org-ellipsis ((t (:foreground ,ink :underline t))))
   `(org-footnote ((t (:foreground ,github-cyan :underline t))))
   `(org-document-title ((t (:inherit org-level-1 :height 2.0 :underline nil :box (:line-width 5 :color "#FFFFFF")))))
   `(org-document-info ((t (:foreground ,ink))))
   `(org-habit-ready-face ((t :background ,pencil1)))
   `(org-habit-alert-face ((t :background ,ink :foreground ,paper)))
   `(org-habit-clear-face ((t :background ,ink)))
   `(org-habit-overdue-face ((t :background ,ink)))
   `(org-habit-clear-future-face ((t :background ,ink)))
   `(org-habit-ready-future-face ((t :background ,pencil1)))
   `(org-habit-alert-future-face ((t :background ,ink :foreground ,paper)))
   `(org-habit-overdue-future-face ((t :background ,ink)))
;;;;; outline
   `(outline-1 ((t (:foreground ,ink))))
   `(outline-2 ((t (:foreground ,github-green+4))))
   `(outline-3 ((t (:foreground ,github-blue-1))))
   `(outline-4 ((t (:foreground ,ink))))
   `(outline-5 ((t (:foreground ,github-cyan))))
   `(outline-6 ((t (:foreground ,pencil1))))
   `(outline-7 ((t (:foreground ,ink))))
   `(outline-8 ((t (:foreground ,ink))))
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
   `(persp-selected-face ((t (:foreground ,ink :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,github-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,paper :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,paper :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,paper :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,github-fg :background ,paper))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,paper :background ,ink))))
   `(proof-error-face ((t (:foreground ,github-fg :background ,ink))))
   `(proof-highlight-dependency-face ((t (:foreground ,paper :background ,ink))))
   `(proof-highlight-dependent-face ((t (:foreground ,paper :background ,ink))))
   `(proof-locked-face ((t (:background ,ink))))
   `(proof-mouse-highlight-face ((t (:foreground ,paper :background ,ink))))
   `(proof-queue-face ((t (:background ,ink))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,ink))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,paper))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,paper))))
   `(proof-warning-face ((t (:foreground ,paper :background ,ink))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,github-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,github-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,ink))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,github-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,pencil1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,github-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,ink))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,ink))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,ink))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,ink))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,pencil1))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,ink))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,ink))))
   `(rcirc-other-nick ((t (:foreground ,ink))))
   `(rcirc-bright-nick ((t (:foreground ,github-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,ink))))
   `(rcirc-server ((t (:foreground ,pencil1))))
   `(rcirc-server-prefix ((t (:foreground ,ink))))
   `(rcirc-timestamp ((t (:foreground ,pencil1))))
   `(rcirc-nick-in-message ((t (:foreground ,github-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,github-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,github-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,pencil1))))
   `(rpm-spec-doc-face ((t (:foreground ,pencil1))))
   `(rpm-spec-ghost-face ((t (:foreground ,github-red))))
   `(rpm-spec-macro-face ((t (:foreground ,github-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,github-red))))
   `(rpm-spec-package-face ((t (:foreground ,github-red))))
   `(rpm-spec-section-face ((t (:foreground ,github-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,ink))))
   `(rpm-spec-var-face ((t (:foreground ,github-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,ink))))
   `(rst-level-2-face ((t (:foreground ,ink))))
   `(rst-level-3-face ((t (:foreground ,github-blue-1))))
   `(rst-level-4-face ((t (:foreground ,ink))))
   `(rst-level-5-face ((t (:foreground ,github-cyan))))
   `(rst-level-6-face ((t (:foreground ,pencil1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,github-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,github-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,ink :background ,paper))))
   `(show-paren-match ((t (:foreground "#333333" :background "#E4FFEA" :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable GitHub for sml
   `(sml/global ((,class (:foreground ,github-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,github-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,ink :weight bold))))
   `(sml/filename ((,class (:foreground ,github-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,ink :weight bold))))
   `(sml/col-number ((,class (:foreground ,github-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,github-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,ink))))
   `(sml/git ((,class (:foreground ,github-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,ink :weight bold))))
   `(sml/read-only ((,class (:foreground ,ink))))
   `(sml/outside-modified ((,class (:foreground ,ink))))
   `(sml/modified ((,class (:foreground ,github-red))))
   `(sml/vc-edited ((,class (:foreground ,pencil1))))
   `(sml/charging ((,class (:foreground ,github-green+4))))
   `(sml/discharging ((,class (:foreground ,ink))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,ink :background ,paper :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,paper :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,github-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,pencil1))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-red)))
      (t
       (:underline ,github-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,ink)))
      (t
       (:underline ,ink))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github-yellow)))
      (t
       (:underline ,github-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,pencil1)))
      (t
       (:underline ,pencil1))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,pencil1))))
   `(speedbar-directory-face ((t (:foreground ,github-cyan))))
   `(speedbar-file-face ((t (:foreground ,github-fg))))
   `(speedbar-highlight-face ((t (:foreground ,paper :background ,pencil1))))
   `(speedbar-selected-face ((t (:foreground ,github-red))))
   `(speedbar-separator-face ((t (:foreground ,paper :background ,github-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,github-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,github-fg
                                    :background ,paper))))
   `(tabbar-selected ((t (:foreground ,github-fg
                                      :background ,paper
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,github-fg
                                        :background ,paper
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,paper
                                       :background ,github-bg-1))))
   `(term-color-red ((t (:foreground ,ink
                                     :background ,ink))))
   `(term-color-green ((t (:foreground ,pencil1
                                       :background ,pencil1))))
   `(term-color-yellow ((t (:foreground ,ink
                                        :background ,github-yellow))))
   `(term-color-blue ((t (:foreground ,github-blue-1
                                      :background ,ink))))
   `(term-color-magenta ((t (:foreground ,ink
                                         :background ,github-red))))
   `(term-color-cyan ((t (:foreground ,github-cyan
                                      :background ,ink))))
   `(term-color-white ((t (:foreground ,github-fg
                                       :background ,ink))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,ink :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,ink :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,github-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,github-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,github-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,github-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,ink ))))
   `(web-mode-css-prop-face ((t (:foreground ,ink))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,github-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,ink))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,ink))))
   `(web-mode-html-attr-name-face ((t (:foreground ,github-cyan))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,github-green+3))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,paper))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,github-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,paper :foreground ,paper))))
   `(whitespace-hspace ((t (:background ,paper :foreground ,paper))))
   `(whitespace-tab ((t (:background "#EDEDED"))))
   `(whitespace-newline ((t (:foreground ,paper))))
   `(whitespace-trailing ((t (:background "#EDEDED"))))
   `(whitespace-line ((t (:background "#EDEDED"))))
   `(whitespace-space-before-tab ((t (:background ,ink :foreground ,ink))))
   `(whitespace-indentation ((t (:background ,paper))))
   `(whitespace-empty ((t (:background "#EDEDED"))))
   `(whitespace-space-after-tab ((t (:background "#EDEDED" :foreground ,github-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,ink))))
   `(wl-highlight-folder-many-face ((t (:foreground ,ink))))
   `(wl-highlight-folder-path-face ((t (:foreground ,ink))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,ink))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,github-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,ink))))
   `(wl-highlight-message-citation-header ((t (:foreground ,ink))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,github-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,pencil1))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,ink))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,github-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,pencil1))))
   `(wl-highlight-message-headers-face ((t (:foreground ,ink))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,pencil1))))
   `(wl-highlight-message-header-contents ((t (:foreground ,ink))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,pencil1))))
   `(wl-highlight-message-signature ((t (:foreground ,pencil1))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,github-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,ink))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,github-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,ink))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,github-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,github-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,ink))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,github-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,github-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,github-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,github-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,github-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,paper :background ,github-blue+1))))
   `(cscope-separator-face ((t (:foreground ,github-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,github-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,github-bg-1 :foreground ,github-bg-1))))
   ))

;;; Theme Variables
(github-with-color-variables
  (custom-theme-set-variables
   'github
;;;;; ansi-color
   `(ansi-color-names-vector [,paper ,github-red ,pencil1 ,github-yellow
                                          ,ink ,ink ,github-cyan ,github-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,github-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,github-red ,ink ,github-yellow ,pencil1 ,github-green+4
                   ,github-cyan ,github-blue+1 ,ink))
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar github-add-font-lock-keywords nil
  "Whether to add font-lock keywords for github color names.
In buffers visiting library `github-theme.el' the github
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar github-colors-font-lock-keywords nil)

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'github)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; github-theme.el ends here
