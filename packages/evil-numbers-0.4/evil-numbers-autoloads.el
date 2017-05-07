;;; evil-numbers-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-numbers" "../../../../../../.emacs.d/packages/evil-numbers-0.4/evil-numbers.el"
;;;;;;  "dcf3ef241cd3c2bcc59ec8f73e63758d")
;;; Generated autoloads from ../../../../../../.emacs.d/packages/evil-numbers-0.4/evil-numbers.el

(autoload 'evil-numbers/inc-at-pt "evil-numbers" "\
Increment the number at point or after point before end-of-line by `amount'.
When region is selected, increment all numbers in the region by `amount'

NO-REGION is internal flag that allows
`evil-numbers/inc-at-point' to be called recursively when
applying the regional features of `evil-numbers/inc-at-point'.

\(fn AMOUNT &optional NO-REGION)" t nil)

(autoload 'evil-numbers/dec-at-pt "evil-numbers" "\
Decrement the number at point or after point before end-of-line by `amount'.

If a region is active, decrement all the numbers at a point by `amount'.

This function uses `evil-numbers/inc-at-pt'

\(fn AMOUNT)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/packages/evil-numbers-0.4/evil-numbers-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/packages/evil-numbers-0.4/evil-numbers.el")
;;;;;;  (22770 7715 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-numbers-autoloads.el ends here
