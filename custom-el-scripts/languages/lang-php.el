(use-package ac-php
  :ensure t
  )

(use-package php-mode
  :ensure t
  :mode
  (("\\.php\\'" . php-mode))
  :config
  (add-hook 'php-mode-hook
	    '(lambda ()
	       (require 'company-php)
	       (company-mode t)
	       (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package phpunit
  :ensure t
  :mode
  (("\\.php\\'" . phpunit-mode)))

(provide 'lang-php)
