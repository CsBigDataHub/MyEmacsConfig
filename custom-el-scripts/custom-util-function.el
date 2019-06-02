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

(bind-keys*
 ("M-m g R" . my/rename-current-buffer-file))



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


(defun my/open-config ()
  "Opens the configuration file from anywhere"
  (interactive)
  (find-file (concat user-emacs-directory "config.org")))


(defun my/goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
        (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (push-mark)
    (goto-char
     (cond
      ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
      ((and closest-ahead (not closest-behind)) closest-ahead)
      ((and closest-behind (not closest-ahead)) closest-behind)
      ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
      ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
      :else closest-ahead))))

(defun my/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun my/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(bind-keys
 ("C-x 2" . my/split-below-and-move)
 ("C-x 3" . my/split-right-and-move))


(defun my/other-window-down ()
  "Scrolls down in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-up-command)
  (other-window 1))
(defun my/other-window-up ()
  "Scrolls up in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-down-command)
  (other-window 1))

(bind-keys*
 ("M-m g ]" . my/other-window-down)
 ("M-m g [" . my/other-window-up))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)


(defun my/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(bind-keys*
 ("M-m g B" . my/browse-current-file))


(defun my/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defhydra my/hydra-of-windows (:color red
                                      :hint nil)
  "
 ^Move^    ^Size^    ^Change^                    ^Split^           ^Text^
 ^^^^^^^^^^^------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _u_: winner-undo _o_: rotate  _v_: vertical     _+_: zoom in
 _h_ ^+^ _l_   _H_ ^+^ _L_   _r_: winner-redo            _s_: horizontal   _-_: zoom out
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _c_: close                  _z_: zoom         _q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" shrink-window-horizontally)
  ("K" shrink-window)
  ("J" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("v" my/split-right-and-move)
  ("s" my/split-below-and-move)
  ("c" delete-window)
  ("f" my/toggle-frame-fullscreen-non-native :color blue)
  ("o" my/rotate-windows)
  ("z" delete-other-windows)
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :color blue))

(bind-keys*
 ("M-m SPC u" . my/hydra-of-windows/body))

(defhydra my/hydra-bookmarks (:color blue
                                     :hint nil)
  "
 _s_: set  _b_: bookmark   _j_: jump   _d_: delete   _q_: quit
  "
  ("s" bookmark-set)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("q" nil :color blue))

(bind-keys*
 ("M-m `" . my/hydra-bookmarks/body))

(defun my/incs (s &optional num)
  (let* ((inc (or num 1))
         (new-number (number-to-string (+ inc (string-to-number s))))
         (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
        (s-pad-left (length s) "0" new-number)
      new-number)))

(defun my/change-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (my/goto-closest-number))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (my/incs (match-string 0) arg) nil nil)))

(defun my/subtract-number-at-point (arg)
  (interactive "p")
  (my/change-number-at-point (- arg)))

(defun my/remove-mark ()
  "Deactivate the region"
  (interactive)
  (if (region-active-p)
      (deactivate-mark)))

(bind-keys*
 ("M-m E" . my/remove-mark))


(defun my/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun my/align-ampersand (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun my/align-quote-space (start end)
  "Align columns by quote and space"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))

(defun my/align-equals (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))

(defun my/align-comma (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))

(defun my/align-dot (start end)
  "Align columns by dot"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))

(defun my/align-colon (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))

(bind-keys*
 ("M-m g A SPC" . my/align-whitespace)
 ("M-m g A &"   . my/align-ampersand)
 ("M-m g A ,"   . my/align-comma)
 ("M-m g A \""  . my/align-quote-space)
 ("M-m g A      ." . my/align-dot)
 ("M-m g A ="   . my/align-equals)
 ("M-m g A :"   . my/align-colon)
 ("M-m g A A"   . align-regexp))

defun my/sk/insert-date (prefix)
"Insert the current date. With prefix-argument, write out the day and month name."
(interactive "P")
(let ((format (cond
               ((not prefix) "%Y-%m-%d")
               ((equal prefix '(4)) "%A, %d %B %Y")
               ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
  (insert (format-time-string format))))

(bind-keys*
 ("M-m g D" . my/sk/insert-date))


(defun my/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-keys*
 ("M-m g K" . my/delete-current-buffer-file))


(defun my/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

(bind-keys*
 ("M-m g y" . my/copy-current-file-path))

;; Transpose words forward
(defun my/transpose-words-forward ()
  "Transpose words forward"
  (interactive)
  (forward-word 1)
  (forward-char 1)
  (transpose-words 1)
  (backward-word 1))
;; Transpose words backward
(defun my/transpose-words-backward ()
  "Transpose words backward"
  (interactive)
  (transpose-words 1)
  (backward-word 1))

(bind-keys*
 ("M-m [ w" . my/transpose-words-backward)
 ("M-m ] w" . my/transpose-words-forward))


;; Transpose chars forward
(defun my/transpose-chars-forward ()
  "Transpose chars forward"
  (interactive)
  (forward-char 1)
  (transpose-chars 1)
  (backward-char 1))
;; Transpose chars backward
(defun my/transpose-chars-backward ()
  "Transpose chars backward"
  (interactive)
  (transpose-chars 1)
  (backward-char 1))


(bind-keys*
 ("M-m [ c" . my/transpose-chars-backward)
 ("M-m ] c" . my/transpose-chars-forward))


(defun my/duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun my/duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (my/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun my/duplicate-line-or-region (&optional num)
  "Duplicate the current line or region if active"
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (my/duplicate-region num beg end)))
  (my/duplicate-current-line num))

(bind-keys*
 ("M-m g d" . my/duplicate-line-or-region))


(defun my/open-line-above (args)
  "Insert a new line above the current one or open a new line above for editing"
  (interactive "P")
  (if (equal args '(4))
      (save-excursion
        (unless (bolp)
          (beginning-of-line))
        (newline)
        (indent-according-to-mode))
    (unless (bolp)
      (beginning-of-line))
    (newline)
    (forward-line -1)
    (indent-according-to-mode)
    (modalka-mode 0)))

(bind-keys*
 ("M-o" . my/open-line-above))


(defun my/join-line ()
  "Join the current line with the next line"
  (interactive)
  (next-line)
  (delete-indentation))

(bind-keys
 ("C-S-j" . my/join-line))

(defun my/select-inside-line ()
  "Select the current line"
  (interactive)
  (my/smarter-move-beginning-of-line 1)
  (set-mark (line-end-position))
  (exchange-point-and-mark))

(defun my/select-around-line ()
  "Select line including the newline character"
  (interactive)
  (my/select-inside-line)
  (next-line 1)
  (my/smarter-move-beginning-of-line 1))

(bind-keys*
 ("M-m i l" . my/select-inside-line)
 ("M-m a l" . my/select-around-line))

(defun my/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun my/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (my/move-text-internal arg))
(defun my/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (my/move-text-internal (- arg)))

(bind-keys*
 ("M-m [ e" . my/move-text-up)
 ("M-m ] e" . my/move-text-down))

(defun my/replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))


(bind-keys*
 ("M-m g C" . my/replace-next-underscore-with-camel))

(defun my/snakeify-current-word ()
  (interactive)
  (er/mark-word)
  (let* ((beg (region-beginning))
         (end (region-end))
         (current-word (buffer-substring-no-properties beg end))
         (snakified (snake-case current-word)))
    (replace-string current-word snakified nil beg end)))

(bind-keys*
 ("M-m g _" . my/snakeify-current-word))

(defhydra myo/hydra-rectangle (:pre (rectangle-mark-mode 1)
                                    :color pink
                                    :hint nil)
  "
 _p_: paste   _r_: replace  _I_: insert
 _y_: copy    _o_: open     _V_: reset
 _d_: kill    _n_: number   _q_: quit
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle)
  ("x" clear-rectangle)
  ("o" open-rectangle)
  ("p" yank-rectangle)
  ("r" string-rectangle)
  ("n" rectangle-number-lines)
  ("I" string-insert-rectangle)
  ("V" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("q" keyboard-quit :color blue))

(bind-keys*
 ("M-m V" . my/hydra-rectangle/body))

(defhydra my/hydra-of-macros (:color pink
                                     :hint nil)
  "
 _m_: macro  _L_: lossage  _v_: view      _n_: forward    _D_: delete   _q_: quit
 _M_: prev   _E_: edit     _r_: register  _p_: backward   _K_: key
  "
  ("m" kmacro-call-macro)
  ("M" kmacro-call-ring-2nd)
  ("L" kmacro-edit-lossage :color blue)
  ("E" kmacro-edit-macro :color blue)
  ("v" kmacro-view-macro :color blue)
  ("r" kmacro-to-register :color blue)
  ("n" kmacro-cycle-ring-next)
  ("p" kmacro-cycle-ring-previous)
  ("D" kmacro-delete-ring-head :color blue)
  ("K" kmacro-bind-to-key :color blue)
  ("q" nil :color blue))


(bind-keys*
 ("M-m @" . my/hydra-of-macros/body))
