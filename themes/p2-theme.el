;;; p2.el --- A minimal Emacs colour theme. -*- lexical-binding: t; -*-
;; Copyright (C) 2015 Göktuğ Kayaalp
;;
;; Author: Göktuğ Kayaalp
;; Keywords: theme p2
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "24") (hexrgb "0"))
;; URL: http://gkayaalp.com/emacs.html#p2
;;
;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:
;;
;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; p2 is a  little, minimal emacs theme that is  meant to be simple
;; and consistent.
;;
;; It  was first  intended  to resemble  the look  of  p2, but  has
;; diverged from  that objective.   Still, though,  I keep  calling it
;; p2, as I like that name.
;;
;; p2  uses a  small colour  palette  over all  the elements.   Org
;; headings  are  specially  treated  with a  palette  of  equidistant
;; colours.  The colours  and heading font sizes  are calculated using
;; base and factor values which can be edited.  See source.
;;
;; It's most adapted for ELisp-Org users, as I'm one such user, though
;; it  works fine  with Markdown,  Textile, Python,  JavaScript, Html,
;; Diff, Magit, etc.
;;
;;; Installation:
;;
;; Install it into a directory that's in the `custom-theme-load-path'.
;; I recommend  that you  put that directory  also in  `load-path', so
;; that you can `require' the  `p2-theme'.  Then adapt this snippet
;; to your configuration.
;;
;;   ;; Not necessary, but silences flycheck errors for referencing free
;;   ;; variables.
;;   (require 'p2-theme)
;;   ;; It's not necessary to modify these variables, they all have sane
;;   ;; defaults.
;;   (setf p2-paper-colour 'p2-parchment ; Custom background.
;;         p2-tint-factor 45)      ; Tint factor for org-level-* faces
;;   ;; Activate the theme.
;;   (load-theme 'p2 t)
;;
;;; Customisation:
;;
;; It is possible to modify the  base font size and the scaling factor
;; for `org-level-faces' via  the variables `p2-base-font-size' and
;; `p2-font-factor' respectively.
;;
;; The factor  for org-level-*  colours are also  configurable, adjust
;; the variable `p2-tint-factor'.
;;
;; Various background colours  are provided, see the  docstring of the
;; variable `p2-paper-colour'  in order to  find out how  to switch
;; them.   You  can add  your  custom  colour for  background  without
;; modifying this module:
;;
;;   (push (list 'my-bgcolour "#000000") p2-colours-alist)
;;   (setf p2-paper-colour 'my-bgcolour)
;;
;; The following snippet will modify org-level-* faces so that initial
;; stars in  org headings are  hidden and  a Sans-serif font  is used.
;; Because  the combination  of heading  font sizes  and colours  make
;; levels  obvious, it  may be  considered superfluous  to have  stars
;; indicating depth:
;;
;;   (setq org-hide-leading-stars nil)
;;   (set-face-attribute
;;    'org-hide nil
;;    :height 0.1 :weight 'light :width 'extracondensed)
;;   (dolist (face org-level-faces)
;;     (set-face-attribute
;;      face nil
;;      :family "Sans Serif"))
;;
;;; Code:
;;
(require 'cl-lib)
(require 'hexrgb)

(deftheme p2
  "An Emacs colour theme that resembles the look of p2.")

(defvar p2-colours-alist
  '((text "#070A01")
    (p2-grey "#FAFAFA")
    (p2-old-dark "#F8ECC2")
    (p2-parchment "#F1F1D4")
    (p2-old-light "#F2EECB")
    (white "#EEEEEE")
    (magenta "#8C0D40")
    (pen "#000F55")
    (light-shadow "#D9DDD9"))
  "The colours used in p2 theme.
The alist of colours where for each pair p (car p) is a
symbol identifying the colour and (cdr p) is the string, the
hexedecimal notation of the colour (i.e. #RRGGBB where R, G and B
are hexedecimal digits).")

(defvar p2-paper-colour 'p2-grey
  "Which p2 colour to use.
The variable `p2-colours-alist' contains a suit of colours
with prefix `p2-'.  This variable's value is supposed to be
set to one of those symbols to specify the colour used for
background.")

(defvar p2-use-varying-heights-for-org-title-headlines nil
  "Whether to use varying heights for Org headlines.")

(defvar p2-base-font-size 100
  "The base size for fonts.")

(defvar p2-font-factor 0.1
  "The font factor for calculating level fonts from base.")

(defvar p2-tint-factor 70
  "The factor for computing tints for org levels.")

(defun p2-colour (colour-identifier)
  "Get colour for COLOUR-IDENTIFIER."
  (cadr (assoc colour-identifier p2-colours-alist)))

(defun p2-colour-paper ()
  "Get the colour for p2.
See `p2-paper-colour' and `p2-colours-alist'."
  (p2-colour p2-paper-colour))

(defconst p2-normal-face
  `((t (:foreground ,(p2-colour 'text) :background ,(p2-colour-paper))))
  "The base colours of p2 theme.")

(defconst p2-inverse-face
  `((t (:foreground ,(p2-colour-paper) :background ,(p2-colour 'text))))
  "The inverse of base colours of p2 theme.")

(defconst p2-pen-face
  `((t (:foreground ,(p2-colour 'pen) :background ,(p2-colour-paper))))
  "Colour couple that resembles pen colour on p2.")

(defconst p2-light-shadow-face
  `((t (:foreground ,(p2-colour 'text) :background ,(p2-colour 'light-shadow))))
  "Colour couple that resembles a light shadow.")

(defconst p2-italicised-pen-face
  `((t (:foreground ,(p2-colour 'pen) :background ,(p2-colour-paper)
                    :slant italic)))
  "Colour couple that resembles pen colour on p2, italicised.")

(defconst p2-magenta-on-paper-face
  `((t (:foreground ,(p2-colour 'magenta) :background ,(p2-colour-paper)))))

(defun p2-tints (hex n &optional darken)
  "Compute equidistant tints of a given colour.
HEX is the hexedecimal RRGGBB string representation of the colour.
N is an integer denoting how many tints to compute.
If DARKEN is non-nil, compute darker tints, otherwise, lighter."
  (cl-loop
   for i from 0 to n
   collect (hexrgb-increment-equal-rgb
            hex 2
            (* i
               (funcall
                (if darken #'- #'identity)
                p2-tint-factor)))))

(defun p2--set-faces ()
  "Set up faces.

May be used to refresh after tweaking some variables."
  (eval
   (let* ((b p2-base-font-size)     ; base
          (f p2-font-factor)        ; factor
          (o "org-level-")
          (org-faces)
          (n 8)
          (tints (p2-tints (p2-colour 'magenta) n)))
     (dolist (n (number-sequence 1 n))
       (push
        `(quote
          (,(intern
             (concat o (number-to-string n)))
           ((t (:slant normal
                :weight light
                :foreground ,(pop tints)
                ,@(when p2-use-varying-heights-for-org-title-headlines
                    (list
                     :height
                     (truncate (+ b (- (* b (+ 1 f)) (* b (* f n))))))))))))
        org-faces))

     `(custom-theme-set-faces
       (quote p2)
       ;; === Frame ===
       (quote (default ,p2-normal-face))
       (quote (cursor ,p2-inverse-face))
       (quote (mode-line ((t (:foreground "gainsboro"
                                          :background "#292B2E"
                                          :box (:line-width 1 :color "#5d4d7a"))))))
       (quote (mode-line-inactive ,p2-light-shadow-face))
       (quote (mode-line-highlight ((t (:foreground "white" :box nil)))))
       (quote (fringe ,p2-normal-face))
       (quote (region ((t (:background "lavender")))))

       ;; === Syntax ===
       (quote (font-lock-builtin-face        ,p2-normal-face))
       (quote (font-lock-comment-face        ,p2-italicised-pen-face))
       (quote (font-lock-string-face         ,p2-pen-face))
       (quote (font-lock-function-name-face  ,p2-pen-face))
       (quote (font-lock-variable-name-face  ,p2-pen-face))
       (quote (font-lock-keyword-face        ,p2-magenta-on-paper-face))
       (quote (font-lock-type-face           ,p2-magenta-on-paper-face))
       (quote (font-lock-constant-face       ,p2-magenta-on-paper-face))

       ;; === Org titles ===
       ,(when p2-use-varying-heights-for-org-title-headlines
          (quote (quote (org-tag ((t (:height 90 :weight light)))))))
       ,@org-faces))))

(p2--set-faces)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'p2-theme)
(provide-theme 'p2)
;;; p2-theme.el ends here
