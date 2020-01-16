(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :mode-hydra
  (go-mode
   (:title "Go Commands")
   ("Doc"
    (("d" godoc-at-point "doc at point"))
    "Imports"
    (("ia" go-import-add "add")
     ("ir" go-remove-unused-imports "cleanup"))))
  :config
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'company-mode)
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook '(lambda ()
			     (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook '(lambda ()
			     (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
  (add-hook 'go-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '(company-go))
			    (company-mode))))

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)
   ("gopls.fuzzyMatching" t t)
   ))

(use-package company-go
  :ensure t
  :after go-mode
  :config
  (setq tab-width 4)
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(use-package flymake-go
  :ensure t)

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun setup-go-mode-compile ()
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(load-if-exists "./go-dlv.el")
(require 'dap-go)
(require 'go-dlv)

(provide 'lang-go)
