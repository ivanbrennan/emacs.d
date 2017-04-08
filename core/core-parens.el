(show-paren-mode    +1)
(electric-pair-mode +1)

(setq blink-matching-delay 0.25
      blink-matching-paren 'jump
      underline-minimum-offset 5)

(setq-default show-paren-when-point-inside-paren t)

(provide 'core-parens)
