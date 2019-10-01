;;; lsp-groovy.el --- Groovy support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 George Pittarelli <g@gjp.cc>

;; Author: George Pittarelli <g@gjp.cc>
;; Version: 1.0
;; Package-Requires: ((lsp-mode "3.0") (groovy-mode "1.0") (emacs "25.1"))
;; Keywords: languages tools
;; URL: https://github.com/emacs-lsp/lsp-groovy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Groovy support for lsp-mode using Palantir's
;; groovy-language-server.

;;; Code:

(require 'lsp-mode)
(require 'groovy-mode)

;;;###autoload
(defcustom lsp-groovy-server-install-dir
  (locate-user-emacs-file "groovy-language-server/")
  "Install directory for groovy-language-server.
A slash is expected at the end.
This directory shoud contain a file matching groovy-language-server-*.jar"
  :group 'lsp-groovy
  :risky t
  :type 'directory)

;;(defconst lsp-groovy--get-root
;;  (lsp-make-traverser #'(lambda (dir)
;;                          (directory-files dir nil "package.json"))))

(defun lsp-groovy--lsp-command ()
  "Generate LSP startup command."
  `("java"
    "-cp" ,(concat (file-truename lsp-groovy-server-install-dir) "*")
    "com.palantir.ls.groovy.GroovyLanguageServer"))

(defun lsp-groovy--render-string (str)
  "Render STR with `groovy-mode' syntax highlighting."
  (ignore-errors
    (with-temp-buffer
      (groovy-mode)
      (insert str)
      (font-lock-ensure)
      (buffer-string))))

(defun lsp-groovy--initialize-client (client)
  "Initial setup for groovy LSP CLIENT."
  (lsp-provide-marked-string-renderer
   client "groovy" 'lsp-groovy--render-string))

(lsp-define-stdio-client
 lsp-groovy "groovy"
 ;;lsp-groovy--get-root
 nil
 :initialize 'lsp-groovy--initialize-client
 :command-fn 'lsp-groovy--lsp-command)

(provide 'lsp-groovy)
;;; lsp-groovy.el ends here
