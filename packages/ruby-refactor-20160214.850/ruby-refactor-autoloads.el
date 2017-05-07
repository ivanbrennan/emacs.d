;;; ruby-refactor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ruby-refactor" "../../../../../../.emacs.d/packages/ruby-refactor-20160214.850/ruby-refactor.el"
;;;;;;  "221d26b88e7bdf73cd86225a30bf8c18")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/ruby-refactor-20160214.850/ruby-refactor.el

(autoload 'ruby-refactor-extract-to-method "ruby-refactor" "\
Extract region to method

\(fn REGION-START REGION-END)" t nil)

(autoload 'ruby-refactor-add-parameter "ruby-refactor" "\
Add a parameter to the method point is in.

\(fn VARIABLE-NAME)" t nil)

(autoload 'ruby-refactor-extract-to-let "ruby-refactor" "\
Converts initialization on current line to 'let', ala RSpec
When called with a prefix argument, flips the default location
for placement.
If a region is selected, the first line needs to have an assigment.
The let style is then a do block containing the region.
If a region is not selected, the transformation uses the current line.

\(fn &optional FLIP-LOCATION)" t nil)

(autoload 'ruby-refactor-extract-local-variable "ruby-refactor" "\
Extracts selected text to local variable

\(fn)" t nil)

(autoload 'ruby-refactor-extract-constant "ruby-refactor" "\
Extracts selected text to a constant at the top of the current class or module

\(fn)" t nil)

(autoload 'ruby-refactor-remove-inline-temp "ruby-refactor" "\
Replaces temporary variable with direct call to method

\(fn)" t nil)

(autoload 'ruby-refactor-convert-post-conditional "ruby-refactor" "\
Convert post conditional expression to conditional expression

\(fn)" t nil)

(autoload 'ruby-refactor-mode "ruby-refactor" "\
Ruby Refactor minor mode

\(fn &optional ARG)" t nil)

(autoload 'ruby-refactor-mode-launch "ruby-refactor" "\
Turn on `ruby-refactor-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/ruby-refactor-20160214.850/ruby-refactor-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/ruby-refactor-20160214.850/ruby-refactor.el")
;;;;;;  (22770 7753 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ruby-refactor-autoloads.el ends here
