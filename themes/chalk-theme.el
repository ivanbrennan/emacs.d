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

;; ---------------------------
;;
;; Chalk: An icy dark blue color theme
;;
;; ----------------------------

(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme chalk
  "A bluecolor theme")

(let ((chalk-background      "#14191F")
      (chalk-foreground      "#AEC2E0")
      (chalk-mid-gray        "#666666")
      (chalk-darker-gray     "#14191f")
      (chalk-white           "#FFFFFF")
      (chalk-off-white       "#F8F8F0")
      (chalk-yellow-white    "#FFE792")
      (chalk-light-gray-blue "#d0dfe6")
      (chalk-lightest-blue   "#effbff")
      (chalk-lighter-blue    "#748aa6")
      (chalk-light-blue      "#6ee2ff")
      (chalk-blue            "#324357")
      (chalk-dark-blue       "#183c66")
      (chalk-darker-blue     "#1b232c")
      (chalk-green           "#95cc5e"))
  (custom-theme-set-faces
   'chalk

   ;; ----------------- Frame stuff --------------------
   `(default ((t (:background ,chalk-background :foreground ,chalk-foreground))))
   `(cursor  ((t (:background ,chalk-off-white))))
   `(hl-line ((t (:background ,chalk-mid-gray))))
   `(modeline ((t (:background ,chalk-dark-blue :foreground ,chalk-white))))
   `(mode-line-inactive ((t (:box nil :background ,chalk-mid-gray :foreground ,chalk-light-gray-blue))))
   `(mode-line ((t (:box nil :foreground ,chalk-white :background ,chalk-dark-blue))))
   `(fringe ((t (:background ,chalk-darker-gray))))
   ;; Dir-ed search prompt
   `(minibuffer-prompt ((default (:foreground ,chalk-white))))
   ;; Highlight region color
   `(region ((t (:foreground ,chalk-yellow-white :background ,chalk-darker-blue))))

   ;; ---------------- Code Highlighting ---------------
   ;; Builtin
   `(font-lock-builtin-face ((t (:foreground ,chalk-light-blue))))
   ;; Comments
   `(font-lock-comment-face ((t (:foreground ,chalk-blue))))
   ;; Function names
   `(font-lock-function-name-face ((t (:foreground ,chalk-lightest-blue))))
   ;; Keywords
   `(font-lock-keyword-face ((t (:foreground ,chalk-lighter-blue))))
   ;; Strings
   `(font-lock-string-face ((t (:foreground ,chalk-light-blue))))
   ;; Variables
   `(font-lock-variable-name-face ((t (:foreground ,chalk-light-gray-blue))))
   `(font-lock-type-face ((t (:foreground ,chalk-green))))
   `(font-lock-warning-face ((t (:foreground ,chalk-white :bold t))))

   `(linum ((t (:foreground ,chalk-mid-gray :height 120 :weight light :underline nil :slant normal))))

   ;; ---------------- Package Specific Stuff -----------
   ;; Powerline
   `(powerline-active1 ((t (:background ,chalk-off-white :foreground ,chalk-background))))))


;;;###Autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'chalk)

;; Local Variables:
;; no-byte-compile: t
;; End:
