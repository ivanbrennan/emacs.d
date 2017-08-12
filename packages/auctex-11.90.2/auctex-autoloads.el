;;; auctex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "bib-cite" "../../../../../../.emacs.d/packages/auctex-11.90.2/bib-cite.el"
;;;;;;  "e2418263a7bd30d35d13116faef63f95")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/bib-cite.el

(autoload 'bib-cite-minor-mode "bib-cite" "\
Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display.

\(fn ARG)" t nil)

(autoload 'turn-on-bib-cite "bib-cite" "\
Unconditionally turn on Bib Cite mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "context" "../../../../../../.emacs.d/packages/auctex-11.90.2/context.el"
;;;;;;  "7b59350bbf5f5e4db1c37fa7c7b92c2b")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/context.el

(defalias 'ConTeXt-mode 'context-mode)

(autoload 'context-mode "context" "\
Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of ConTeXt-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "context-en" "../../../../../../.emacs.d/packages/auctex-11.90.2/context-en.el"
;;;;;;  "5ac2595246062777c61ed4104a93cf61")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/context-en.el

(autoload 'context-en-mode "context-en" "\
Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "context-nl" "../../../../../../.emacs.d/packages/auctex-11.90.2/context-nl.el"
;;;;;;  "0a6bd12930a5771f6b89fcf16d479b08")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/context-nl.el

(autoload 'context-nl-mode "context-nl" "\
Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "font-latex" "../../../../../../.emacs.d/packages/auctex-11.90.2/font-latex.el"
;;;;;;  "97305bf83ebb99ea2edbd05decaf6d4d")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/font-latex.el

(autoload 'font-latex-setup "font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "latex" "../../../../../../.emacs.d/packages/auctex-11.90.2/latex.el"
;;;;;;  "1f3ec500b6e19a2aaa0f3bae8abf439c")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/latex.el

(autoload 'BibTeX-auto-store "latex" "\
This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file.

\(fn)" nil nil)

(add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode))

(add-to-list 'auto-mode-alist '("\\.hva\\'" . latex-mode))

(autoload 'TeX-latex-mode "latex" "\
Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode))

(autoload 'docTeX-mode "latex" "\
Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

\(fn)" t nil)

(defalias 'TeX-doctex-mode 'docTeX-mode)

;;;***

;;;### (autoloads nil "multi-prompt" "../../../../../../.emacs.d/packages/auctex-11.90.2/multi-prompt.el"
;;;;;;  "c8c776d8b945271c79bb93186d71c4a0")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/multi-prompt.el

(autoload 'multi-prompt "multi-prompt" "\
Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

\(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil)

(autoload 'multi-prompt-key-value "multi-prompt" "\
Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

;;;***

;;;### (autoloads nil "plain-tex" "../../../../../../.emacs.d/packages/auctex-11.90.2/plain-tex.el"
;;;;;;  "e3324dacb995c956e200d1214c0bc30e")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/plain-tex.el

(autoload 'TeX-plain-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of plain-TeX-mode-hook.

\(fn)" t nil)

(autoload 'ams-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering AmS-tex-mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "preview" "../../../../../../.emacs.d/packages/auctex-11.90.2/preview.el"
;;;;;;  "1248e0cbbb93a0d2d6cc33a0956490f4")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/preview.el

(autoload 'preview-install-styles "preview" "\
Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

\(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil)

(autoload 'LaTeX-preview-setup "preview" "\
Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality.

\(fn)" nil nil)

(autoload 'preview-report-bug "preview" "\
Report a bug in the preview-latex package.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex.el"
;;;;;;  "da1521eeb28ab436f70272b8ec383fd5")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex.el

(defalias 'TeX-assoc-string (symbol-function (if (featurep 'xemacs) 'assoc 'assoc-string)))

(autoload 'TeX-tex-mode "tex" "\
Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'

\(fn)" t nil)

(autoload 'TeX-auto-generate "tex" "\
Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

\(fn TEX AUTO)" t nil)

(autoload 'TeX-auto-generate-global "tex" "\
Create global auto directory for global TeX macro definitions.

\(fn)" t nil)

(autoload 'TeX-submit-bug-report "tex" "\
Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-bar" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-bar.el"
;;;;;;  "1369ffc9c337e7713ed14c5cc1bdef5a")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex-bar.el

(autoload 'TeX-install-toolbar "tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-fold" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-fold.el"
;;;;;;  "d4b9c32827dd222845b9f1ea6206ed49")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex-fold.el
 (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias 'tex-fold-mode 'TeX-fold-mode)

;;;***

;;;### (autoloads nil "tex-font" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-font.el"
;;;;;;  "07c520c682f0590571fa966c42f8a8c2")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex-font.el

(autoload 'tex-font-setup "tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "tex-info" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-info.el"
;;;;;;  "6ba1b408586d78542aef322d202d6f87")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex-info.el

(defalias 'Texinfo-mode 'texinfo-mode)

(autoload 'TeX-texinfo-mode "tex-info" "\
Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-jp" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-jp.el"
;;;;;;  "f97306f5fe3e334a7a0c21d3f9d02e5a")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex-jp.el

(autoload 'japanese-plain-tex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'.

\(fn)" t nil)

(autoload 'japanese-latex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-site" "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-site.el"
;;;;;;  "9979aa2264bbad03202e68b1e6d14d8c")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/tex-site.el
 (require 'tex-site)

;;;***

;;;### (autoloads nil "texmathp" "../../../../../../.emacs.d/packages/auctex-11.90.2/texmathp.el"
;;;;;;  "a16b54e3aed856c924b12da03c242c45")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/texmathp.el

(autoload 'texmathp "texmathp" "\
Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked.

\(fn)" t nil)

(autoload 'texmathp-match-switch "texmathp" "\
Search backward for any of the math switches.
Limit searched to BOUND.

\(fn BOUND)" nil nil)

;;;***

;;;### (autoloads nil "toolbar-x" "../../../../../../.emacs.d/packages/auctex-11.90.2/toolbar-x.el"
;;;;;;  "8decedc248191d5b9a54ebefdf6d60d0")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/auctex-11.90.2/toolbar-x.el
 (autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/auctex-11.90.2/auctex-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/auctex-pkg.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/auctex.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/bib-cite.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/context-en.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/context-nl.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/context.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/font-latex.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/latex.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/multi-prompt.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/plain-tex.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/preview.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/prv-emacs.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/prv-xemacs.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-bar.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-buf.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-fold.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-font.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-info.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-ispell.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-jp.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-mik.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-site.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-style.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex-wizard.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/tex.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/texmathp.el"
;;;;;;  "../../../../../../.emacs.d/packages/auctex-11.90.2/toolbar-x.el")
;;;;;;  (22921 5609 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; auctex-autoloads.el ends here
