;; This has been copied and modified from https://www.emacswiki.org/emacs/setup-keys.el
;; I only require one function
(require 'doremi-cmd nil t) ;; (no error if not found): doremi-buffers+, doremi-bookmarks+,
;; doremi-color-themes+, doremi-custom-themes+

(eval-after-load "doremi-cmd"
  '(progn
     (unless (fboundp 'doremi-prefix)
       (defalias 'doremi-prefix (make-sparse-keymap))
       (defvar doremi-map (symbol-function 'doremi-prefix)
         "Keymap for Do Re Mi commands."))
     (define-key global-map "\C-cd"  'doremi-prefix)
     (define-key doremi-map "b" 'doremi-buffers+) ; Buffer                        `C-c d b'
     (define-key doremi-map "g" 'doremi-global-marks+) ; Global mark              `C-c d g'
     (define-key doremi-map "m" 'doremi-marks+) ; Mark                            `C-c d m'
     (define-key doremi-map "r" 'doremi-bookmarks+) ; `r' for Reading books       `C-c d r'
     (define-key doremi-map "s" (if (fboundp 'doremi-custom-themes+)
                                    'doremi-custom-themes+
                                  'doremi-color-themes+)) ; `s' for color Schemes `C-c d s'
     (define-key doremi-map "w" 'doremi-window-height+))) ; Window                `C-c d w'

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-keys.el ends here
