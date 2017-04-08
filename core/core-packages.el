(let* ((trustfile (replace-regexp-in-string
                   "\\\\" "/"
                   (replace-regexp-in-string
                    "\n" ""
                    (shell-command-to-string "python -m certifi"))))
       (gnutls-cli (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)))
  (setq gnutls-trustfiles   (list trustfile)
        gnutls-verify-error t
        tls-checktrust      t
        tls-program         (list gnutls-cli)))

(require 'package)

(setq package--init-file-ensured t
      package-enable-at-startup  nil
      package-user-dir           ivan-packages-directory
      package-archives           '(("gnu"          . "https://elpa.gnu.org/packages/")
                                   ("melpa-stable" . "https://stable.melpa.org/packages/")
                                   ("melpa"        . "https://melpa.org/packages/")
                                   ("marmalade"    . "https://ojab.ru/marmalade/"))
      package-archive-priorities '(("melpa-stable" . 3)
                                   ("gnu"          . 2)
                                   ("marmalade"    . 1)
                                   ("melpa"        . 0)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

(provide 'core-packages)
