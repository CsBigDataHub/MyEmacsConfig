;; [[file:~/.emacs.d/myinit.org::*repos][repos:1]]
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; repos:1 ends here

;; [[file:~/.emacs.d/myinit.org::*interface%20tweaks][interface tweaks:1]]
(setq inhibit-startup-message t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-auto-revert-mode 1) ;; you might not want this
(setq auto-revert-verbose nil) ;; or thiS
;; using swiper so ido no longer needed

(winner-mode 1)
;;to remember window config
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(ido-mode 1)

;;(windmove-default-keybindings) ;;uses shift + arrow keys to move around the windows.

;;(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(defalias 'list-buffers 'ibuffer-other-window) ; make ibuffer default

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

(setq tramp-default-method "ssh")
;; interface tweaks:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Isearch%20Current%20word%20on%20the%20cursor][Isearch Current word on the cursor:1]]
(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection."
  ;;“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
  ;;URL `http://ergoemacs.org/emacs/modernization_isearch.html'
  ;;Version 2015-04-09"
  (interactive)
  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

(global-set-key (kbd "<f8>") 'xah-search-current-word)
;; Isearch Current word on the cursor:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Theme][Theme:1]]
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;;(use-package monokai-theme
;;  :ensure t
;;  :config (load-theme 'monokai t))

;;(use-package zenburn-theme
;;  :ensure t
;;  :config (load-theme 'zenburn t))


;;(use-package color-theme-modern
;;  :ensure t)
;;
;;(use-package base16-theme
;;  :ensure t
;;  )
;;(use-package moe-theme
;;  :ensure t)
;;
;;(use-package alect-themes
;;  :ensure t)
;;
;;(use-package zerodark-theme
;;  :ensure t)
;;
;;(use-package faff-theme
;;  :ensure t)
;;
;;(use-package poet-theme
;;  :ensure t)
;;
;;(use-package tao-theme
;;  :ensure t)
;;
;;(use-package doom-themes
;;  :ensure t)
;; Theme:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Try][Try:1]]
(use-package try
  :ensure t)
;; Try:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Which%20key][Which key:1]]
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
;; Which key:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Evil%20Mode%20For%20Vi][Evil Mode For Vi:1]]
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
;; Evil Mode For Vi:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Org%20Bullets][Org Bullets:1]]
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; Org Bullets:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Relative%20Line%20number][Relative Line number:1]]
(use-package linum-relative
  :ensure t
  :config
  (linum-relative-global-mode 1))

(linum-relative-global-mode 1)

;; Use `display-line-number-mode' as linum-mode's backend for smooth performance
(setq linum-relative-backend 'display-line-numbers-mode)
;; Relative Line number:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Format-all][Format-all:1]]
(use-package format-all
  :ensure t)
;; Format-all:1 ends here

;; [[file:~/.emacs.d/myinit.org::*tabbar][tabbar:1]]
(use-package tabbar
  :ensure t
  :config (tabbar-mode 1)
  )
;; tabbar:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Ace%20Window][Ace Window:1]]
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))
;; Ace Window:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Counsel/Ivy/Swiper][Counsel/Ivy/Swiper:1]]
;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
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
;; Counsel/Ivy/Swiper:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Avy][Avy:1]]
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g l" . avy-goto-line))
  :config
  (avy-setup-default))
;; Avy:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Auto-Complete][Auto-Complete:1]]
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))
;; Auto-Complete:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Company][Company:1]]
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)

  (global-company-mode t)
  )


(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  )

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; company box mode
;;(use-package company-box
;;:ensure t
;;  :hook (company-mode . company-box-mode))
;; Company:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Reveal.js][Reveal.js:1]]
(use-package ox-reveal
  :ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)
;; Reveal.js:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Org-Config-Easy-Template][Org-Config-Easy-Template:1]]
;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
               "<src lang=\"emacs-lisp\">\n?\n</src>"))
;; add <p for python expansion
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC python :results output org drawer\n?\n#+END_SRC"
               "<src lang=\"python\">\n?\n</src>"))

;; add <r for R expansion
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC r :results output org drawer\n?\n#+END_SRC"
               "<src lang=\"r\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
             '("ao" "#+attr_org: " ""))

(add-to-list 'org-structure-template-alist
             '("al" "#+attr_latex: " ""))

(add-to-list 'org-structure-template-alist
             '("ca" "#+caption: " ""))

(add-to-list 'org-structure-template-alist
             '("tn" "#+tblname: " ""))

(add-to-list 'org-structure-template-alist
             '("n" "#+name: " ""))

(add-to-list 'org-structure-template-alist
             '("o" "#+options: " ""))

(add-to-list 'org-structure-template-alist
             '("ti" "#+title: " ""))
;; Org-Config-Easy-Template:1 ends here

;; [[file:~/.emacs.d/myinit.org::*FlyCheck][FlyCheck:1]]
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
;; FlyCheck:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Pyhton][Pyhton:1]]
(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))
;; make sure to install jedi-server for effective pip lint
;; M-x jedi:install-server
;; Check Elpy if interested in Python Developement

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))
;; Pyhton:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Insert-Date_time][Insert-Date_time:1]]
(defun xah-insert-date-time ()
"Insert current date time.
Insert date in this format: yyyy-mm-dd.
When called with `universal-argument', prompt for a format to use.
If there's text selection, delete it first.

URL `http://ergoemacs.org/emacs/elisp_insert-date-time.html'
version 2018-07-03"
  (interactive)
  (let (($style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 2018-04-12 Thursday"
                  "2 → 20180412224611"
                  "3 → 2018-04-12T22:46:11-07:00"
                  "4 → 2018-04-12 22:46:11-07:00"
                  "5 → Thursday, April 12, 2018"
                  "6 → Thu, Apr 12, 2018"
                  "7 → April 12, 2018"
                  "8 → Apr 12, 2018"
                  )) 0 1))
           0
           )))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 0)
       ;; "2016-10-10"
       (format-time-string "%Y-%m-%d"))
      ((= $style 1)
       ;; "2018-04-12 Thursday"

       (format-time-string "%Y-%m-%d %A"))
      ((= $style 2)
       ;; "20180412224015"
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 3)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
       ;; "2018-04-12T22:45:26-07:00"
       )
      ((= $style 4)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
       ;; "2018-04-12 22:46:11-07:00"
       )
      ((= $style 5)
       (format-time-string "%A, %B %d, %Y")
       ;; "Thursday, April 12, 2018"
       )
      ((= $style 6)
       (format-time-string "%a, %b %d, %Y")
       ;; "Thu, Apr 12, 2018"
       )
      ((= $style 7)
       (format-time-string "%B %d, %Y")
       ;; "April 12, 2018"
       )
      ((= $style 8)
       (format-time-string "%b %d, %Y")
       ;; "Apr 12, 2018"
       )
      (t
       (format-time-string "%Y-%m-%d"))))))
;; Insert-Date_time:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Paste%20or%20Paste%20Previous][Paste or Paste Previous:1]]
(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
  This command calls `yank', and if repeated, call `yank-pop'."
  ;;
  ;;When `universal-argument' is called first with a number arg, paste that many times.
  ;;
  ;;URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
  ;;Version 2017-07-25"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes ($i (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(global-set-key (kbd "C-y") 'xah-paste-or-paste-previous)
;; Paste or Paste Previous:1 ends here

;; [[file:~/.emacs.d/myinit.org::*White%20Space%20and%20Blank%20Lines][White Space and Blank Lines:1]]
;;In emacs, the following commands lets you delete whitespaces around cursor.
  ;;
  ;;    delete-blank-lines 【Ctrl+x Ctrl+o】
  ;;    just-one-space 【Alt+Space】
  ;;    delete-indentation 【Alt+^】
  ;;    delete-horizontal-space 【Alt+\】
  ;;    fixup-whitespace
  ;;    cycle-spacing (emacs 24.4)
  ;;
  ;;Here's a command xah-shrink-whitespaces that combine most of them into one.

  (defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

  URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
  Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (setq $p4 (point))
    (delete-region $p3 $p4)))

  (defun xah-shrink-whitespaces ()
    "Remove whitespaces around cursor to just one, or none.

    Shrink any neighboring space tab newline characters to 1 or none.
    If cursor neighbor has space/tab, toggle between 1 or 0 space.
    If cursor neighbor are newline, shrink them to just 1.
    If already has just 1 whitespace, delete it.

    URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
    Version 2018-04-02T14:38:04-07:00"
    (interactive)
    (let* (
           ($eol-count 0)
           ($p0 (point))
           $p1 ; whitespace begin
           $p2 ; whitespace end
           ($charBefore (char-before))
           ($charAfter (char-after ))
           ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
           $just-1-space-p
)
      (skip-chars-backward " \n\t")
      (setq $p1 (point))
      (goto-char $p0)
      (skip-chars-forward " \n\t")
      (setq $p2 (point))
      (goto-char $p1)
      (while (search-forward "\n" $p2 t )
        (setq $eol-count (1+ $eol-count)))
      (setq $just-1-space-p (eq (- $p2 $p1) 1))
      (goto-char $p0)
      (cond
       ((eq $eol-count 0)
        (if $just-1-space-p
            (delete-horizontal-space)
          (progn (delete-horizontal-space)
                 (insert " "))))
       ((eq $eol-count 1)
        (if $space-neighbor-p
            (delete-horizontal-space)
          (progn (xah-delete-blank-lines) (insert " "))))
       ((eq $eol-count 2)
        (if $space-neighbor-p
            (delete-horizontal-space)
          (progn
            (xah-delete-blank-lines)
            (insert "\n"))))
       ((> $eol-count 2)
        (if $space-neighbor-p
            (delete-horizontal-space)
          (progn
            (goto-char $p2)
            (search-backward "\n" )
            (delete-region $p1 (point))
            (insert "\n"))))
       (t (progn
            (message "nothing done. logic error 40873. shouldn't reach here" ))))))
;; White Space and Blank Lines:1 ends here

;; [[file:~/.emacs.d/myinit.org::*MarkDown%20mode][MarkDown mode:1]]
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;; MarkDown mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Half-Scrolling%20Similar%20to%20VI][Half-Scrolling Similar to VI:1]]
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height)))

(global-set-key (kbd "M-u") 'scroll-up-half)
(global-set-key (kbd "C-u") 'scroll-down-half)
;;Scrolling 4 lines without moving the point
(global-set-key (kbd "M-n")  (lambda () (interactive) (scroll-up   4)) )
(global-set-key (kbd "M-p")  (lambda () (interactive) (scroll-down 4)) )
;; Half-Scrolling Similar to VI:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Undo-tree][Undo-tree:1]]
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))
;; Undo-tree:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Highlight%20cursor%20line][Highlight cursor line:1]]
(global-hl-line-mode t)
;; Highlight cursor line:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Beacon%20Mode][Beacon Mode:1]]
; flashes the cursor's line when you scroll
  (use-package beacon
    :ensure t
    :config
    (beacon-mode 1)
; this color looks good for the zenburn theme but not for the one
; I'm using for the videos
(setq beacon-color "#666600")
    )
;; Beacon Mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Hungy%20Delete%20Mode][Hungy Delete Mode:1]]
; deletes all the whitespace when you hit backspace or delete
  (use-package hungry-delete
    :ensure t
    :config
    (global-hungry-delete-mode))
;; Hungy Delete Mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Expand%20Region][Expand Region:1]]
; expand the marked region in semantic increments (negative prefix to reduce region)
    (use-package expand-region
      :ensure t
      :config
      (global-set-key (kbd "C-=") 'er/expand-region))
;; Expand Region:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Multiple%20Cursors][Multiple Cursors:1]]
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
;; Multiple Cursors:1 ends here

;; [[file:~/.emacs.d/myinit.org::*smart-forward][smart-forward:1]]
(use-package smart-forward
  :ensure t
  :config
  (global-set-key (kbd "M-<up>") 'smart-up)
  (global-set-key (kbd "M-<down>") 'smart-down)
  (global-set-key (kbd "M-<left>") 'smart-backward)
  (global-set-key (kbd "M-<right>") 'smart-forward))
;; smart-forward:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Join%20Line][Join Line:1]]
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
;; Join Line:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Rename%20File%20in%20Current%20buffer][Rename File in Current buffer:1]]
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
;; Rename File in Current buffer:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Move%20Lines%20up%20and%20Down][Move Lines up and Down:1]]
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
;; Move Lines up and Down:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Highlight%20matching%20pair][Highlight matching pair:1]]
;; Complete pair
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

;; highlight brackets
(setq show-paren-style 'parenthesis)

;; highlight entire expression
;;(setq show-paren-style 'expression)

;; highlight brackets if visible, else entire expression
;;(setq show-paren-style 'mixed)
;; Highlight matching pair:1 ends here

;; [[file:~/.emacs.d/myinit.org::*string-inflection%20for%20string%20manipulation][string-inflection for string manipulation:1]]
(use-package string-inflection
  :ensure t
  :bind (("C-c i" . string-inflection-all-cycle))
  )

;; for java
(add-hook 'java-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c i") 'string-inflection-java-style-cycle)))

;; for python
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c i") 'string-inflection-python-style-cycle)))
;; string-inflection for string manipulation:1 ends here

;; [[file:~/.emacs.d/myinit.org::*iedit%20and%20narrow%20/%20widen%20dwim][iedit and narrow / widen dwim:1]]
; mark and edit all copies of the marked region simultaniously.

(use-package iedit
  :ensure t)

; if you're windened, narrow to the region, if you're narrowed, widen
; bound to C-x n
(defun narrow-or-widen-dwim (p)
"If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
;; iedit and narrow / widen dwim:1 ends here

;; [[file:~/.emacs.d/myinit.org::*File-Exists%20Function][File-Exists Function:1]]
(defun load-if-exists(f)
    "load the elip file only if it exits"
    (if (file-readable-p f)
        (load-file f)))
;; You can also use below
;;(when (file-readable-p f) (load-file p))
;; File-Exists Function:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Dump-Keys][Dump-Keys:1]]
(load-if-exists "./custom-el-scripts/dump-keys.el")
;; Dump-Keys:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Web-Mode][Web-Mode:1]]
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t))
;; Web-Mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Mode%20Line][Mode Line:1]]
(setq display-time-24hr-format t)
(display-time-mode +1)

(use-package doom-modeline
  :ensure t)
(require 'doom-modeline)
(doom-modeline-init)
;; Mode Line:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Uniquify%20For%20buffer%20names][Uniquify For buffer names:1]]
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  )
;; Uniquify For buffer names:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Formatting%20and%20white-space][Formatting and white-space:1]]
(setq-default indent-tabs-mode nil)

(defun my/clean-buffer-formatting ()
  "Indent and clean up the buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(global-set-key "\C-cn" 'my/clean-buffer-formatting)

;; by default,
;; highlight trailing whitespace
;; and show form-feed chars as horizontal rules

(defun my/general-formatting-hooks ()
  (setq show-trailing-whitespace 't)
  (my/turn-on 'form-feed))

(dolist (mode-hook (my/normal-mode-hooks))
  (add-hook mode-hook 'my/general-formatting-hooks))

(defun fixup-json ()
  "Re-indent json buffers with broken literal strings. Needs jsonpp installed (available using homebrew)"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "sed -e ':a' -e 'N' -e '$!ba' -e 's/\\n/ /g'|jsonpp"  nil t))
  
(defun my/text-formatting-hooks ()
  (my/turn-on 'auto-fill)) ; turn on automatic hard line wraps

(add-hook 'text-mode-hook
          'my/text-formatting-hooks)
;; Formatting and white-space:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Posframe][Posframe:1]]
(use-package posframe :ensure t)
;; Posframe:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Yasnippet][Yasnippet:1]]
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package auto-yasnippet
  :ensure t)
;; Yasnippet:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Dired][Dired:1]]
(setq dired-dwim-target t)

(use-package dired-narrow
:ensure t
:config
(bind-key "C-c C-n" #'dired-narrow)
(bind-key "C-c C-f" #'dired-narrow-fuzzy)
(bind-key "C-x C-N" #'dired-narrow-regexp)
)

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))
;; Dired:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Projectile][Projectile:1]]
;; projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (counsel-projectile-on)q)
;; Projectile:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Smart-Parens][Smart-Parens:1]]
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config))

(show-paren-mode t)
;; Smart-Parens:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Font%20Scaling][Font Scaling:1]]
;; font scaling
(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

(define-key ctl-x-map [(control ?0)] 'zoom-in/out)
;; Font Scaling:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Hydra][Hydra:1]]
(use-package hydra
  :ensure hydra
  :init
  (global-set-key
   (kbd "C-x t")
   (defhydra toggle (:color blue)
     "toggle"
     ("a" abbrev-mode "abbrev")
     ("s" flyspell-mode "flyspell")
     ("d" toggle-debug-on-error "debug")
     ("c" fci-mode "fCi")
     ("f" auto-fill-mode "fill")
     ("t" toggle-truncate-lines "truncate")
     ("w" whitespace-mode "whitespace")
     ("q" nil "cancel")))
  (global-set-key
   (kbd "C-x j")
   (defhydra gotoline
     ( :pre (linum-mode 1)
            :post (linum-mode -1))
     "goto"
     ("t" (lambda () (interactive)(move-to-window-line-top-bottom 0)) "top")
     ("b" (lambda () (interactive)(move-to-window-line-top-bottom -1)) "bottom")
     ("m" (lambda () (interactive)(move-to-window-line-top-bottom)) "middle")
     ("e" (lambda () (interactive)(end-of-buffer)) "end")
     ("c" recenter-top-bottom "recenter")
     ("n" next-line "down")
     ("p" (lambda () (interactive) (forward-line -1))  "up")
     ("g" goto-line "goto-line")
     ))
  (global-set-key
   (kbd "C-c t")
   (defhydra hydra-global-org (:color blue)
     "Org"
     ("t" org-timer-start "Start Timer")
     ("s" org-timer-stop "Stop Timer")
     ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
     ("p" org-timer "Print Timer") ; output timer value to buffer
     ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
     ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
     ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
     ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
     ("l" (or )rg-capture-goto-last-stored "Last Capture"))

   ))

(defhydra hydra-multiple-cursors (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil)


  ("<mouse-1>" mc/add-cursor-on-click)
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore))
;; Hydra:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Git][Git:1]]
(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)
    ))

(setq magit-status-margin
      '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


(use-package git-timemachine
  :ensure t
  )
(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
  "
  Git gutter:
    _j_: next hunk        _s_tage hunk     _q_uit
    _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
    ^ ^                   _p_opup hunk
    _h_: first hunk
    _l_: last hunk        set start _R_evision
  "
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
   :color blue))
;; Git:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Better%20Shell][Better Shell:1]]
(use-package better-shell
  :ensure t
  :bind (("C-\"" . better-shell-shell)
         ("C-:" . better-shell-remote-open)))
;; Better Shell:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Dumb%20Jump][Dumb Jump:1]]
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  ;; (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :init
  (dumb-jump-mode)
  :ensure
  )
;; Dumb Jump:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Origami%20folding][Origami folding:1]]
(use-package origami
  :ensure t)
;; Origami folding:1 ends here

;; [[file:~/.emacs.d/myinit.org::*IBUFFER][IBUFFER:1]]
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (or

                        (mode . mu4e-compose-mode)
                        (name . "\*mu4e\*")
                        ))
               ("programming" (or
                               (mode . clojure-mode)
                               (mode . clojurescript-mode)
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
                                        ;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)
;; IBUFFER:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ([f2]        . treemacs-toggle)
        ([f9]        . treemacs-projectile-toggle)
        ("<C-M-tab>" . treemacs-toggle)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
;; Treemacs:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Aggresive%20indent][Aggresive indent:1]]
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  ;;(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )
;; Aggresive indent:1 ends here

;; [[file:~/.emacs.d/myinit.org::*All%20the%20Icons][All the Icons:1]]
(use-package all-the-icons
  :ensure t
  :defer 0.5)

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))


(use-package all-the-icons-dired
  :ensure t
  )

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; All the Icons:1 ends here

;; [[file:~/.emacs.d/myinit.org::*FZF][FZF:1]]
(use-package fzf :ensure t)
;; FZF:1 ends here

;; [[file:~/.emacs.d/myinit.org::*RipGrep][RipGrep:1]]
(use-package deadgrep
  :ensure t)

(use-package rg
  :ensure t
  :commands rg)
;; RipGrep:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Easy%20kill][Easy kill:1]]
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))
;; Easy kill:1 ends here

;; [[file:~/.emacs.d/myinit.org::*PATH][PATH:1]]
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )
;; PATH:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Wgrep][Wgrep:1]]
(use-package wgrep
  :ensure t
  )
(use-package wgrep-ag
  :ensure t
  )
(require 'wgrep-ag)
;; Wgrep:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Regex][Regex:1]]
(use-package pcre2el
  :ensure t
  :config
  (pcre-mode)
  )
;; Regex:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Eyebrowse][Eyebrowse:1]]
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode)
  )
;; Eyebrowse:1 ends here

;; [[file:~/.emacs.d/myinit.org::*PDF%20Tools][PDF Tools:1]]
(use-package pdf-tools
  :ensure t)
(use-package org-pdfview
  :ensure t)

(require 'pdf-tools)
(require 'org-pdfview)
;; PDF Tools:1 ends here
