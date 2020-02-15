(use-package go-mode
	:ensure t
	:mode "\\.go\\'"
	:mode-hydra
	(go-mode
		(:title "Go Commands")
		(
			"Buffer"
			(("d" lsp-describe-thing-at-point)
				("bf" lsp-format-buffer)
				("ip" lsp-info-under-point)
				("m" lsp-ui-imenu)
				("q" nil "quit"))
			"Errors"
			(("e" hydra-flycheck/body)
				("lf" lsp-ui-flycheck-list))
			"Refactor"
			(("rs" lsp-rename))
			"Find"
			(("fd" lsp-ui-peek-find-definitions)
				("fi" lsp-ui-peek-find-implementation)
				("fr" lsp-ui-peek-find-references)
				("fs" lsp-ui-peek-find-workspace-symbol))
			"Go-to/Jump"
			(("gi" lsp-goto-implementation)
				("gt" lsp-goto-type-definition)
				("jn" lsp-ui-peek-jump-forward)
				("jp" lsp-ui-peek-jump-backward))
			"Imports"
			(("ia" go-import-add "add")
				("ir" go-remove-unused-imports "cleanup"))))
	:config
	;; Use goimports instead of go-fmt
	(setq gofmt-command "goimports")
	(add-hook 'go-mode-hook 'company-mode)
	;; Call Gofmt before saving
	;;(add-hook 'before-save-hook 'gofmt-before-save)
	(add-hook 'go-mode-hook 'setup-go-mode-compile)
	(add-hook 'go-mode-hook #'smartparens-mode)
	(add-hook 'go-mode-hook '(lambda ()
					 (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
	(add-hook 'go-mode-hook '(lambda ()
					 (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
	(add-hook 'go-mode-hook (lambda ()
					(set (make-local-variable 'company-backends) '((company-go company-files :with company-yasnippet)
											      (company-dabbrev-code company-dabbrev)))
					(company-mode))))

(lsp-register-custom-settings
	'(("gopls.completeUnimported" t t)
		 ("gopls.staticcheck" t t)
		 ;;("gopls.fuzzyMatching" t t) ;; commented due to - config fuzzyMatching is deprecated, use matcher instead

		 ))

(use-package company-go
  :defer t
	:ensure t
	:after go-mode
	:config
	(setq tab-width 4)
	:bind (:map go-mode-map
		      ("M-." . godef-jump)))

(use-package flymake-go
  :defer t
	:ensure t)

(use-package go-eldoc
  :defer t
	:ensure t
	:config
	(add-hook 'go-mode-hook 'go-eldoc-setup))

;; integrate go-guru analysis tool to emacs
(use-package go-guru
  :defer t
  )

;; go-rename: extra refactoring commands for go
(use-package go-rename
  :defer t
  )


(defun setup-go-mode-compile ()
	;; Customize compile command to run go build
	(if (not (string-match "go" compile-command))
		(set (make-local-variable 'compile-command)
			"go build -v && go test -v && go vet")))

(load-if-exists "./go-dlv.el")
(require 'dap-go)
(require 'go-dlv)

(provide 'lang-go)
