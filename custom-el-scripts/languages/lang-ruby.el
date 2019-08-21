(use-package enh-ruby-mode
  :ensure t
  :mode
  (("\\.rb\\'" . ruby-mode)))

(use-package robe
  :ensure t
  :config
  (push 'company-robe company-backends))

(use-package rinari
  :ensure t
  )

(provide 'lang-ruby)
