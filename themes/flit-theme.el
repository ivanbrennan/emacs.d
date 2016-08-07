;;; flit-theme.el --- A simple theme for Emacs based on the Flit theme for Sublime Text

;; Copyright (C) 2013 Greg Chapple

;; Author: Greg Chapple <info@gregchapple.com>
;; URL: http://github.com/gregchapple/flit-emacs
;; Version: 0.1.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Sublime Text Flit theme for Emacs 24

;;; Code:

(deftheme flit "The Flit color theme")

;;; Color Palette

(defvar flit-colors-alist
  '(("flit-bg+3"      . "#353a3d")
    ("flit-bg+2"      . "#2e3235")
    ("flit-bg+1"      . "#2e303a")
    ("flit-bg"        . "#26292c")
    ("flit-bg-1"      . "#1f2124")
    ("flit-bg-05"     . "#202325")
    ("flit-cursor"    . "#bbbcbd")
    ("flit-fg"        . "#f8f8f8")
    ("flit-fg-1"      . "#e0e0e0")
    ("flit-selection" . "#3c3f42")
    ("flit-green+4"   . "#b9d977")
    ("flit-green+3"   . "#b8d977")
    ("flit-green+2"   . "#b7d877")
    ("flit-green+1"   . "#b6d877")
    ("flit-green"     . "#40b83e")
    ("flit-green-1"   . "#41a83e")
    ("flit-blue+1"    . "#cfe2f2")
    ("flit-blue"      . "#afc4db")
    ("flit-blue-1"    . "#8996a8")
    ("flit-blue-2"    . "#72aaca")
    ("flit-blue-3"    . "#65a4a4")
    ("flit-blue-4"    . "#0f0031")
    ("flit-blue-5"    . "#0e2231")
    ("flit-cyan"      . "#93e0e3")
    ("flit-magenta"   . "#dc8cc3")
    ("flit-organge+1" . "#ffb454")
    ("flit-orange"    . "#f6aa11")
    ("flit-orange-1"  . "#ffaa00")
    ("flit-orange-2"  . "#fa9a4b")
    ("flit-orange-3"  . "#df9400")
    ("flit-yellow+4"  . "#ffffaa")
    ("flit-yellow+3"  . "#f7f09d")
    ("flit-yellow+2"  . "#edf080")
    ("flit-yellow+1"  . "#edef7d")
    ("flit-yellow"    . "#f6f080")
    ("flit-yellow-1"  . "#f5f080")
    ("flit-yellow-2"  . "#f1e94b")
    ("flit-yellow-3"  . "#c4b14a")
    ("flit-red+2"     . "#ff3a83")
    ("flit-red+1"     . "#eb939a")
    ("flit-red"       . "#ff4a52")
    ("flit-red-1"     . "#ff0000")
    ("flit-red-2"     . "#d8290d")
    ("flit-red-3"     . "#d03620")
    ("flit-red-4"     . "#c92b14")
    ("flit-white"     . "#ffffff")
    ("flit-white-1"   . "#f8f8f8")
    ("flit-white-2"   . "#b1b3ba")
    ("flit-white-3"   . "#b0b3ba")
    ("flit-white-4"   . "#73817d")
    ("flit-white-5"   . "#798188")
    )
  "List of flit colors")


(defmacro flit-with-color-variables (&rest body)
  "`let' bind all colors defined in `flit-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   flit-colors-alist))
     ,@body))

;;; Theme Faces
(flit-with-color-variables
  (custom-theme-set-faces
   'flit
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,flit-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,flit-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,flit-fg :background ,flit-bg))))
   `(cursor ((t (:foreground ,flit-fg :background ,flit-cursor))))
   `(escape-glyph ((t (:foreground ,flit-yellow :bold t))))
   `(fringe ((t (:foreground ,flit-fg :background ,flit-bg+1))))
   `(header-line ((t (:foreground ,flit-yellow
                                  :background ,flit-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,flit-bg-05))))
   `(success ((t (:foreground ,flit-green :weight bold))))
   `(warning ((t (:foreground ,flit-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,flit-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,flit-green))))
   `(compilation-error-face ((t (:foreground ,flit-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,flit-fg))))
   `(compilation-info-face ((t (:foreground ,flit-blue))))
   `(compilation-info ((t (:foreground ,flit-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,flit-green))))
   `(compilation-line-face ((t (:foreground ,flit-yellow))))
   `(compilation-line-number ((t (:foreground ,flit-yellow))))
   `(compilation-message-face ((t (:foreground ,flit-blue))))
   `(compilation-warning-face ((t (:foreground ,flit-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,flit-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,flit-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,flit-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,flit-fg))))
   `(grep-error-face ((t (:foreground ,flit-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,flit-blue))))
   `(grep-match-face ((t (:foreground ,flit-orange :weight bold))))
   `(match ((t (:background ,flit-bg-1 :foreground ,flit-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,flit-yellow-2 :weight bold :background ,flit-bg-1))))
   `(isearch-fail ((t (:foreground ,flit-fg :background ,flit-red-4))))
   `(lazy-highlight ((t (:foreground ,flit-yellow-2 :weight bold :background ,flit-bg-05))))

   `(menu ((t (:foreground ,flit-fg :background ,flit-bg))))
   `(minibuffer-prompt ((t (:foreground ,flit-yellow))))
   `(mode-line
     ((,class (:foreground ,flit-fg-1
                           :background ,flit-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,flit-fg :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,flit-green-1
                      :background ,flit-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,flit-selection))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,flit-bg+2))))
   `(trailing-whitespace ((t (:background ,flit-red))))
   `(vertical-border ((t (:foreground ,flit-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,flit-orange-2 :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,flit-white-5))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,flit-white-5))))
   `(font-lock-constant-face ((t (:foreground ,flit-green+4))))
   `(font-lock-doc-face ((t (:foreground ,flit-white-5))))
   `(font-lock-function-name-face ((t (:foreground ,flit-blue-2))))
   `(font-lock-keyword-face ((t (:foreground ,flit-orange-2 :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,flit-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,flit-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,flit-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,flit-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,flit-blue+1))))
   `(font-lock-type-face ((t (:foreground ,flit-blue-2))))
   `(font-lock-variable-name-face ((t (:foreground ,flit-yellow))))
   `(font-lock-color-constant-face ((t (:foreground ,flit-red+1))))
   `(font-lock-reference-face ((t (:foreground ,flit-red+1))))
   `(font-lock-other-type-face ((t (:foreground ,flit-red+1))))
   `(font-lock-special-keyword-face ((t (:foreground ,flit-red+1))))
   `(font-lock-other-emphasized-face ((t (:foreground ,flit-red+1))))
   `(font-lock-warning-face ((t (:foreground ,flit-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,flit-fg))))
   `(newsticker-default-face ((t (:foreground ,flit-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,flit-green+3))))
   `(newsticker-extra-face ((t (:foreground ,flit-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,flit-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,flit-green))))
   `(newsticker-new-item-face ((t (:foreground ,flit-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,flit-red))))
   `(newsticker-old-item-face ((t (:foreground ,flit-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,flit-fg))))
   `(newsticker-treeview-face ((t (:foreground ,flit-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,flit-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,flit-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,flit-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,flit-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,flit-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,flit-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,flit-fg-1 :background ,flit-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,flit-green+2 :background ,flit-bg :inverse-video nil))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,flit-cyan :weight bold))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,flit-fg))))
   `(ack-file ((t (:foreground ,flit-blue))))
   `(ack-line ((t (:foreground ,flit-yellow))))
   `(ack-match ((t (:foreground ,flit-orange :background ,flit-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:inherit font-lock-warning))))
   `(font-latex-sectioning-5-face ((t (:foreground ,flit-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,flit-yellow))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,flit-fg-1 :foreground "black"))))
   `(ac-selection-face ((t (:background ,flit-selection :foreground ,flit-fg))))
   `(popup-tip-face ((t (:background ,flit-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,flit-bg+3))))
   `(popup-scroll-bar-background-face ((t (:background ,flit-white))))
   `(popup-isearch-match ((t (:background ,flit-bg :foreground ,flit-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,flit-green+1))))
   `(android-mode-error-face ((t (:foreground ,flit-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,flit-fg))))
   `(android-mode-verbose-face ((t (:foreground ,flit-green))))
   `(android-mode-warning-face ((t (:foreground ,flit-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,flit-yellow-1 :foreground ,flit-bg))))
   `(bm-fringe-face ((t (:background ,flit-yellow-1 :foreground ,flit-bg))))
   `(bm-fringe-persistent-face ((t (:background ,flit-green-1 :foreground ,flit-bg))))
   `(bm-persistent-face ((t (:background ,flit-green-1 :foreground ,flit-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,flit-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,flit-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,flit-green+1 :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,flit-blue :foreground ,flit-bg))))
   `(ctbl:face-continue-bar ((t (:background ,flit-bg-05 :foreground ,flit-bg))))
   `(ctbl:face-row-select ((t (:background ,flit-cyan :foreground ,flit-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,flit-green+4 :background nil))
                 (t (:foreground ,flit-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,flit-yellow))))
   `(diff-removed ((,class (:foreground ,flit-red :background nil))
                   (t (:foreground ,flit-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,flit-bg+2))
                  (t (:background ,flit-fg :foreground ,flit-bg))))
   `(diff-file-header
     ((,class (:background ,flit-bg+2 :foreground ,flit-fg :bold t))
      (t (:background ,flit-fg :foreground ,flit-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,flit-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,flit-orange))))
   `(diredp-date-time ((t (:foreground ,flit-magenta))))
   `(diredp-deletion ((t (:foreground ,flit-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,flit-red))))
   `(diredp-dir-heading ((t (:foreground ,flit-blue :background ,flit-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,flit-cyan))))
   `(diredp-exec-priv ((t (:foreground ,flit-red))))
   `(diredp-executable-tag ((t (:foreground ,flit-green+1))))
   `(diredp-file-name ((t (:foreground ,flit-blue))))
   `(diredp-file-suffix ((t (:foreground ,flit-green))))
   `(diredp-flag-mark ((t (:foreground ,flit-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,flit-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,flit-red))))
   `(diredp-link-priv ((t (:foreground ,flit-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,flit-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,flit-orange))))
   `(diredp-no-priv ((t (:foreground ,flit-fg))))
   `(diredp-number ((t (:foreground ,flit-green+1))))
   `(diredp-other-priv ((t (:foreground ,flit-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,flit-red-1))))
   `(diredp-read-priv ((t (:foreground ,flit-green-1))))
   `(diredp-symlink ((t (:foreground ,flit-yellow))))
   `(diredp-write-priv ((t (:foreground ,flit-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,flit-fg :background ,flit-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,flit-fg :background ,flit-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,flit-fg :background ,flit-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,flit-fg :background ,flit-blue-5))))
   `(ediff-even-diff-A ((t (:background ,flit-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,flit-bg+1))))
   `(ediff-even-diff-B ((t (:background ,flit-bg+1))))
   `(ediff-even-diff-C ((t (:background ,flit-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,flit-fg :background ,flit-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,flit-fg :background ,flit-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,flit-fg :background ,flit-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,flit-fg :background ,flit-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,flit-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,flit-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,flit-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,flit-bg+2))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,flit-green+4 :background ,flit-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,flit-red :background ,flit-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,flit-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,flit-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,flit-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,flit-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,flit-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,flit-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,flit-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,flit-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-red) :inherit unspecified))
      (t (:foreground ,flit-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-orange) :inherit unspecified))
      (t (:foreground ,flit-orange :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,flit-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,flit-orange :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,flit-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,flit-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,flit-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-orange) :inherit unspecified))
      (t (:foreground ,flit-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,flit-red) :inherit unspecified))
      (t (:foreground ,flit-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,flit-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,flit-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,flit-yellow))))
   `(erc-keyword-face ((t (:foreground ,flit-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,flit-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,flit-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,flit-green))))
   `(erc-pal-face ((t (:foreground ,flit-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,flit-orange :background ,flit-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,flit-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,flit-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,flit-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,flit-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,flit-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,flit-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,flit-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,flit-magenta :weight bold))))
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
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,flit-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,flit-blue))))
   `(gnus-summary-high-read ((t (:foreground ,flit-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,flit-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,flit-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,flit-blue))))
   `(gnus-summary-low-read ((t (:foreground ,flit-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,flit-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,flit-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,flit-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,flit-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,flit-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,flit-fg))))
   `(gnus-summary-selected ((t (:foreground ,flit-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,flit-blue))))
   `(gnus-cite-10 ((t (:foreground ,flit-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,flit-yellow))))
   `(gnus-cite-2 ((t (:foreground ,flit-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,flit-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,flit-green+2))))
   `(gnus-cite-5 ((t (:foreground ,flit-green+1))))
   `(gnus-cite-6 ((t (:foreground ,flit-green))))
   `(gnus-cite-7 ((t (:foreground ,flit-red))))
   `(gnus-cite-8 ((t (:foreground ,flit-red-1))))
   `(gnus-cite-9 ((t (:foreground ,flit-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,flit-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,flit-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,flit-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,flit-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,flit-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,flit-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,flit-bg+2))))
   `(gnus-signature ((t (:foreground ,flit-yellow))))
   `(gnus-x ((t (:background ,flit-fg :foreground ,flit-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,flit-blue))))
   `(guide-key/key-face ((t (:foreground ,flit-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,flit-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,flit-green
                      :background ,flit-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,flit-yellow
                      :underline nil
                      :weight bold
                      :box nil))))
   `(helm-selection ((t (:background ,flit-selection :underline nil))))
   `(helm-selection-line ((t (:background ,flit-bg+1))))
   `(helm-visible-mark ((t (:foreground ,flit-bg :background ,flit-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,flit-green+4 :background ,flit-bg-1))))
   `(helm-ff-directory ((t (:foreground ,flit-magenta))))
   `(helm-buffer-directory ((t (:background ,flit-blue-2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,flit-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,flit-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,flit-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,flit-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,flit-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,flit-yellow))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,flit-bg+2 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,flit-orange :weight bold))))
   `(js2-error ((t (:foreground ,flit-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,flit-green-1 :weight bold))))
   `(js2-jsdoc-type ((t (:foreground ,flit-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,flit-green+3))))
   `(js2-function-param ((t (:foreground, flit-green+3))))
   `(js2-external-variable ((t (:foreground ,flit-white))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,flit-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,flit-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,flit-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,flit-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,flit-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,flit-red+1))))
   `(jabber-activity-face((t (:foreground ,flit-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,flit-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,flit-selection :background ,flit-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,flit-green+2 :background ,flit-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,flit-red+1 :background ,flit-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,flit-blue+1 :background ,flit-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,flit-magenta :background ,flit-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,flit-yellow :background ,flit-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,flit-yellow :background ,flit-bg :weight bold :box nil :underline t))))
   `(magit-branch ((t (:foreground ,flit-orange :background ,flit-bg :weight bold :box nil))))
   `(magit-item-highlight ((t (:background ,flit-bg+1 :bold nil))))
   `(magit-blame-header (( t (:box (:line-width 1 :style released-button)))))
   `(magit-blame-sha1 ((t (:foreground ,flit-green :box (:line-width 1 :style released-button)))))
   `(magit-blame-subject ((t (:foreground ,flit-yellow-2 :box (:line-width 1 :style released-button)))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,flit-fg))))
   `(egg-help-header-1 ((t (:foreground ,flit-yellow))))
   `(egg-help-header-2 ((t (:foreground ,flit-green+3))))
   `(egg-branch ((t (:foreground ,flit-yellow))))
   `(egg-branch-mono ((t (:foreground ,flit-yellow))))
   `(egg-term ((t (:foreground ,flit-yellow))))
   `(egg-diff-add ((t (:foreground ,flit-green+4))))
   `(egg-diff-del ((t (:foreground ,flit-red+1))))
   `(egg-diff-file-header ((t (:foreground ,flit-yellow-2))))
   `(egg-section-title ((t (:foreground ,flit-yellow))))
   `(egg-stash-mono ((t (:foreground ,flit-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,flit-green+1))))
   `(message-header-other ((t (:foreground ,flit-green))))
   `(message-header-to ((t (:foreground ,flit-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,flit-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,flit-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,flit-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,flit-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,flit-green))))
   `(message-mml ((t (:foreground ,flit-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,flit-orange))))
   `(mew-face-header-from ((t (:foreground ,flit-yellow))))
   `(mew-face-header-date ((t (:foreground ,flit-green))))
   `(mew-face-header-to ((t (:foreground ,flit-red))))
   `(mew-face-header-key ((t (:foreground ,flit-green))))
   `(mew-face-header-private ((t (:foreground ,flit-green))))
   `(mew-face-header-important ((t (:foreground ,flit-blue))))
   `(mew-face-header-marginal ((t (:foreground ,flit-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,flit-red))))
   `(mew-face-header-xmew ((t (:foreground ,flit-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,flit-red))))
   `(mew-face-body-url ((t (:foreground ,flit-orange))))
   `(mew-face-body-comment ((t (:foreground ,flit-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,flit-green))))
   `(mew-face-body-cite2 ((t (:foreground ,flit-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,flit-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,flit-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,flit-red))))
   `(mew-face-mark-review ((t (:foreground ,flit-blue))))
   `(mew-face-mark-escape ((t (:foreground ,flit-green))))
   `(mew-face-mark-delete ((t (:foreground ,flit-red))))
   `(mew-face-mark-unlink ((t (:foreground ,flit-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,flit-green))))
   `(mew-face-mark-unread ((t (:foreground ,flit-red-2))))
   `(mew-face-eof-message ((t (:foreground ,flit-green))))
   `(mew-face-eof-part ((t (:foreground ,flit-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,flit-cyan :background ,flit-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,flit-bg :background ,flit-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,flit-bg :background ,flit-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,flit-blue))))
   `(mingus-pausing-face ((t (:foreground ,flit-magenta))))
   `(mingus-playing-face ((t (:foreground ,flit-cyan))))
   `(mingus-playlist-face ((t (:foreground ,flit-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,flit-yellow))))
   `(mingus-stopped-face ((t (:foreground ,flit-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,flit-yellow))))
   `(nav-face-button-num ((t (:foreground ,flit-cyan))))
   `(nav-face-dir ((t (:foreground ,flit-green))))
   `(nav-face-hdir ((t (:foreground ,flit-red))))
   `(nav-face-file ((t (:foreground ,flit-fg))))
   `(nav-face-hfile ((t (:foreground ,flit-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,flit-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,flit-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,flit-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,flit-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,flit-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,flit-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,flit-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,flit-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,flit-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,flit-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,flit-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,flit-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,flit-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,flit-fg :weight bold))))
   `(org-checkbox ((t (:background ,flit-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,flit-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,flit-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,flit-green+3))))
   `(org-formula ((t (:foreground ,flit-yellow-2))))
   `(org-headline-done ((t (:foreground ,flit-green+3))))
   `(org-hide ((t (:foreground ,flit-bg-1))))
   `(org-level-1 ((t (:foreground ,flit-orange))))
   `(org-level-2 ((t (:foreground ,flit-green+4))))
   `(org-level-3 ((t (:foreground ,flit-blue-1))))
   `(org-level-4 ((t (:foreground ,flit-yellow-2))))
   `(org-level-5 ((t (:foreground ,flit-cyan))))
   `(org-level-6 ((t (:foreground ,flit-green+2))))
   `(org-level-7 ((t (:foreground ,flit-red-4))))
   `(org-level-8 ((t (:foreground ,flit-blue-4))))
   `(org-link ((t (:foreground ,flit-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,flit-green+4))))
   `(org-scheduled-previously ((t (:foreground ,flit-red-4))))
   `(org-scheduled-today ((t (:foreground ,flit-blue+1))))
   `(org-sexp-date ((t (:foreground ,flit-blue+1 :underline t))))
   `(org-special-keyword ((t (:foreground ,flit-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,flit-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,flit-orange))))
   `(org-todo ((t (:bold t :foreground ,flit-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,flit-red :weight bold :underline nil))))
   `(org-column ((t (:background ,flit-bg-1))))
   `(org-column-title ((t (:background ,flit-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,flit-orange))))
   `(outline-2 ((t (:foreground ,flit-green+4))))
   `(outline-3 ((t (:foreground ,flit-blue-1))))
   `(outline-4 ((t (:foreground ,flit-yellow-2))))
   `(outline-5 ((t (:foreground ,flit-cyan))))
   `(outline-6 ((t (:foreground ,flit-green+2))))
   `(outline-7 ((t (:foreground ,flit-red-4))))
   `(outline-8 ((t (:foreground ,flit-blue-4))))
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
   `(persp-selected-face ((t (:foreground ,flit-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,flit-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,flit-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,flit-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,flit-bg+3 :inherit mode-line-inactive))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,flit-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,flit-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,flit-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,flit-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,flit-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,flit-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,flit-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,flit-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,flit-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,flit-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,flit-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,flit-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,flit-blue))))
   `(rcirc-other-nick ((t (:foreground ,flit-orange))))
   `(rcirc-bright-nick ((t (:foreground ,flit-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,flit-blue-2))))
   `(rcirc-server ((t (:foreground ,flit-green))))
   `(rcirc-server-prefix ((t (:foreground ,flit-green+1))))
   `(rcirc-timestamp ((t (:foreground ,flit-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,flit-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,flit-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,flit-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,flit-green))))
   `(rpm-spec-doc-face ((t (:foreground ,flit-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,flit-red))))
   `(rpm-spec-macro-face ((t (:foreground ,flit-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,flit-red))))
   `(rpm-spec-package-face ((t (:foreground ,flit-red))))
   `(rpm-spec-section-face ((t (:foreground ,flit-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,flit-blue))))
   `(rpm-spec-var-face ((t (:foreground ,flit-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,flit-orange))))
   `(rst-level-2-face ((t (:foreground ,flit-green+1))))
   `(rst-level-3-face ((t (:foreground ,flit-blue-1))))
   `(rst-level-4-face ((t (:foreground ,flit-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,flit-cyan))))
   `(rst-level-6-face ((t (:foreground ,flit-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,flit-red-3 :background ,flit-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,flit-blue-1 :background ,flit-bg :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:background ,flit-bg+3 :foreground ,flit-red-3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,flit-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,flit-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,flit-fg
                                    :background ,flit-bg))))
   `(tabbar-selected ((t (:foreground ,flit-fg
                                      :background ,flit-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,flit-fg
                                        :background ,flit-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,flit-bg
                                       :background ,flit-bg-1))))
   `(term-color-red ((t (:foreground ,flit-red-2
                                       :background ,flit-red-4))))
   `(term-color-green ((t (:foreground ,flit-green
                                       :background ,flit-green+2))))
   `(term-color-yellow ((t (:foreground ,flit-orange
                                       :background ,flit-yellow))))
   `(term-color-blue ((t (:foreground ,flit-blue-1
                                      :background ,flit-blue-4))))
   `(term-color-magenta ((t (:foreground ,flit-magenta
                                         :background ,flit-red))))
   `(term-color-cyan ((t (:foreground ,flit-cyan
                                       :background ,flit-blue))))
   `(term-color-white ((t (:foreground ,flit-fg
                                       :background ,flit-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,flit-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,flit-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,flit-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,flit-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,flit-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,flit-green+2 :background ,flit-bg))))
   `(w3m-lnum-match ((t (:background ,flit-bg-1
                                     :foreground ,flit-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,flit-yellow))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,flit-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,flit-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,flit-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,flit-blue))))
   `(web-mode-css-selector-face ((t (:foreground ,flit-green+1 :weight normal))))
   `(web-mode-css-property-name-face ((t (:foreground ,flit-blue-2))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,flit-green+1))))
   `(web-mode-html-attr-name-face ((t (:foreground ,flit-yellow))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,flit-green+1))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,flit-blue-3))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:foreground ,flit-white))))
   `(web-mode-server-background-face ((t (:background ,flit-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,flit-red))))
   
;;;;; css-mode
   `(css-property ((t (:foreground ,flit-blue-2))))
   `(css-selector ((t (:foreground ,flit-green+1))))

;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,flit-bg+1 :foreground ,flit-bg+1))))
   `(whitespace-hspace ((t (:background ,flit-bg+1 :foreground ,flit-bg+1))))
   `(whitespace-tab ((t (:background ,flit-red-1))))
   `(whitespace-newline ((t (:foreground ,flit-bg+1))))
   `(whitespace-trailing ((t (:background ,flit-red))))
   `(whitespace-line ((t (:background ,flit-bg :foreground ,flit-magenta))))
   `(whitespace-space-before-tab ((t (:background ,flit-orange :foreground ,flit-orange))))
   `(whitespace-indentation ((t (:background ,flit-yellow :foreground ,flit-red))))
   `(whitespace-empty ((t (:background ,flit-yellow))))
   `(whitespace-space-after-tab ((t (:background ,flit-yellow :foreground ,flit-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,flit-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,flit-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,flit-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,flit-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,flit-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,flit-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,flit-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,flit-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,flit-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,flit-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,flit-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,flit-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,flit-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,flit-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,flit-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,flit-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,flit-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,flit-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,flit-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,flit-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,flit-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,flit-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,flit-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,flit-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,flit-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,flit-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,flit-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,flit-bg-1 :foreground ,flit-bg-1))))
   ))

;;; Theme Variables
(flit-with-color-variables
  (custom-theme-set-variables
   'flit
;;;;; ansi-color
   `(ansi-color-names-vector [,flit-bg ,flit-red ,flit-green ,flit-yellow
                                          ,flit-blue ,flit-magenta ,flit-cyan ,flit-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,flit-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,flit-red-1)
       ( 40. . ,flit-red)
       ( 60. . ,flit-orange)
       ( 80. . ,flit-yellow-2)
       (100. . ,flit-yellow-1)
       (120. . ,flit-yellow)
       (140. . ,flit-green-1)
       (160. . ,flit-green)
       (180. . ,flit-green+1)
       (200. . ,flit-green+2)
       (220. . ,flit-green+3)
       (240. . ,flit-green+4)
       (260. . ,flit-cyan)
       (280. . ,flit-blue-2)
       (300. . ,flit-blue-1)
       (320. . ,flit-blue)
       (340. . ,flit-blue+1)
       (360. . ,flit-magenta)))
   `(vc-annotate-very-old-color ,flit-magenta)
   `(vc-annotate-background ,flit-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar flit-add-font-lock-keywords nil
  "Whether to add font-lock keywords for flit color names.
In buffers visiting library `flit-theme.el' the flit
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar flit-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after flit activate)
;;   "Maybe also add font-lock keywords for flit colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or flit-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "flit-theme.el")))
;;     (unless flit-colors-font-lock-keywords
;;       (setq flit-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car flit-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc flit-colors-alist))))))
;;     (font-lock-add-keywords nil flit-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after flit activate)
;;   "Also remove font-lock keywords for flit colors."
;;   (font-lock-remove-keywords nil flit-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'flit)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; flit-theme.el ends here
