(defconst ivan-trustfile
  (eval-when-compile
    (replace-regexp-in-string
     "\\\\" "/"
     (replace-regexp-in-string
      "\n" ""
      (shell-command-to-string "python -m certifi"))))
  "Certificate Authority (CA) bundle for validating SSL certificates.")

(defconst ivan-tls-program
  (eval-when-compile
    (format "gnutls-cli --x509cafile %s -p %%p %%h" ivan-trustfile))
  "Command to start TLS stream to a host.")

(setq tls-checktrust      t
      tls-program         `(,ivan-tls-program)
      gnutls-trustfiles   `(,ivan-trustfile)
      gnutls-verify-error t

      package-archives '(("gnu"         . "https://elpa.gnu.org/packages/")
                        ("melpa-stable" . "https://stable.melpa.org/packages/")
                        ("melpa"        . "https://melpa.org/packages/")
                        ("marmalade"    . "https://ojab.ru/marmalade/"))

      package-archive-priorities '(("melpa-stable" . 3)
                                   ("gnu"          . 2)
                                   ("marmalade"    . 1)
                                   ("melpa"        . 0))

      package-user-dir ivan-packages-directory

      package--init-file-ensured t
      package-enable-at-startup  nil)

(eval-when-compile (require 'package))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-always-ensure t)

(provide 'core-package)
