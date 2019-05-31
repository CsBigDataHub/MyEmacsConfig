;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun my/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun mybrowse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun my/dos2unix (buffer)
  "Convert BUFFER from DOS file format to UNIX."
  (interactive "*b")
  (shell-command (format "dos2unix %s" (file-truename buffer))))


;; Never understood why Emacs doesn't have this function, either.
;;
(defun my/move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn	(copy-file filename newname 1)	(delete-file filename)	(set-visited-file-name nil)))))

(defun my/put-current-path-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (expand-file-name file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (expand-file-name dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun my/put-current-filename-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (file-name-nondirectory file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (file-name-nondirectory dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun my/put-current-filename-with-line-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (format "%s:%s"
                             (file-name-nondirectory file-path)
                             (count-lines (point-min) (point))))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (file-name-nondirectory dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun my/upcase-backward-word (arg)
  (interactive "p")
  (upcase-word (- arg)))

(defun my/downcase-backward-word (arg)
  (interactive "p")
  (downcase-word (- arg)))

(defun my/capitalize-backward-word (arg)
  (interactive "p")
  (capitalize-word (- arg)))

;;(global-set-key (kbd "C-M-u")	 'upcase-backward-word)
;;(global-set-key (kbd "C-M-l")	 'downcase-backward-word)
;;(global-set-key (kbd "M-c")	 'capitalize-backward-word)

(defun my/kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~],.、。" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;;(global-set-key (kbd "M-d")  'kill-word-at-point)

(defun my/backward-kill-word-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

;;(global-set-key (kbd "C-w")  'backward-kill-word-or-region)

(defun my/kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

;;eval-and-replace
;; https://emacsredux.com/blog/2013/06/21/eval-and-replace/

(defun my/eval-and-replace (beginning end)
  "Replace the preceding sexp or region with its value."
  (interactive "r")
  (if (region-active-p)
      (delete-region beginning end)
    (backward-kill-sexp))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun my/xah-search-current-word ()
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

(global-set-key (kbd "<f8>") 'my/xah-search-current-word)

(defun my/xah-insert-date-time ()
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


(defun my/xah-paste-or-paste-previous ()
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

(global-set-key (kbd "C-y") 'my/xah-paste-or-paste-previous)

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

(defun my/xah-shrink-whitespaces ()
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


(defun my/rename-current-buffer-file ()
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


(defun my/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun my/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'my/move-line-down)
(global-set-key (kbd "<C-S-up>") 'my/move-line-up)

;;if you're windened, narrow to the region, if you're narrowed, widen;
;;bound to C-x n
(defun my/narrow-or-widen-dwim (p)
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

;; (define-key endless/toggle-map "n" #'my/narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'my/narrow-or-widen-dwim)


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
(defun fixup-json ()
  "Re-indent json buffers with broken literal strings. Needs jsonpp installed (available using homebrew)"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "sed -e ':a' -e 'N' -e '$!ba' -e 's/\\n/ /g'|jsonpp"  nil t))

(defun my/text-formatting-hooks ()
  (my/turn-on 'auto-fill)) ; turn on automatic hard line wraps

(add-hook 'text-mode-hook
          'my/text-formatting-hooks)
