(defun init-Java ()

  ;; lsp setup
  (setq-local company-manual-completion-fn #'company-lsp)
  (setq lsp-inhibit-message t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-highlight-symbol-at-point nil)

  ;; rest
  (flycheck-mode))

(use-package lsp-mode
  :ensure t)

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point))

(use-package lsp-java
  :ensure t
  :init
  (add-hook 'java-mode-hook 'lsp-java-enable)
  (add-hook 'java-mode-hook 'init-Java))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(require 'dap-java)

;; Support Lombok in our projects, among other things
(setq my/lombok-jar (expand-file-name "~/GitRepos/lombok/lombok.jar"))

(setq lsp-java-vmargs
      (list "-noverify"
            "-Xmx2G"
            "-XX:+UseG1GC"
            "-XX:+UseStringDeduplication"
            (concat "-javaagent:" my/lombok-jar)
            (concat "-Xbootclasspath/a:" my/lombok-jar))
      lsp-file-watch-ignored
      '(".idea" ".ensime_cache" ".eunit" "node_modules" ".git" ".hg" ".fslckout" "_FOSSIL_"
        ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build")

      ;;lsp-java-import-order '["" "java" "javax" "#"]
      ;; Don't organize imports on save
      ;;lsp-java-save-action-organize-imports nil

      ;; Formatter profile
      ;;lsp-java-format-settings-url (concat "file://" my/java-format-settings-file)
      lsp-enable-on-type-formatting t
      lsp-enable-indentation t)


;;(use-package dap-java
;;  :after (lsp-java))


(use-package autodisass-java-bytecode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.aj\\'" . java-mode))
(use-package java-snippets
  :ensure t)
(provide 'init-java)
