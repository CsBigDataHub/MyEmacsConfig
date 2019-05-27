(setq inhibit-startup-message t)

;;(tool-bar-mode 1) ;; to removed tool bar

;; using swiper so ido no longer needed
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(ido-mode 1)

(winner-mode 1) ;;to remember window config

;;(windmove-default-keybindings) ;;uses shift + arrow keys to move around the windows.

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;(use-package monokai-theme
;;  :ensure t
;;  :config (load-theme 'monokai t))

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(use-package linum-relative
  :ensure t
  :config
  (linum-relative-global-mode 1))

(linum-relative-global-mode 1)

;; Use `display-line-number-mode' as linum-mode's backend for smooth performance
(setq linum-relative-backend 'display-line-numbers-mode)

(use-package format-all
  :ensure t)

;;(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(defalias 'list-buffers 'ibuffer-other-window) ; make ibuffer default

(use-package tabbar
  :ensure t
  :config (tabbar-mode 1)
  )

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))


;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure try
  ;;This also can be used to bind keys
  ;;:bind (("C-s" . swiper)
  ;;	 ("C-r" . swiper)
  ;;	 ("C-c C-r" . ivy-resume)
  ;;	 ("M-x" . counsel-M-x)
  ;;	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char)
	 ("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g l" . avy-goto-line))
  :config
  (avy-setup-default))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;;(use-package company
;;  :ensure t
;;  :config (company-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme monokai-theme monokai tabbar format-all which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
