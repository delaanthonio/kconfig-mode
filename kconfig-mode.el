;;; kconfig-mode.el --- Major mode for editing Kconfig files

;; Copyright © 2014 Yu Peng
;; Copyright © 2014 Michal Sojka
;; Copyright © 2017 Dela Anthonio

;; Author: Dela Anthonio <dell.anthonio@gmail.com>
;; Homepage: https://github.com/beta1440/kernel-modes
;; Keywords: kconfig, linux, kernel
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing kconfig files.

;; Setup:

;; (require 'kconfig-mode)

;;; Code:

(defvar kconfig-mode-font-lock-keywords
  '(("\#.*" . font-lock-comment-face)

    ("\".*\"" . font-lock-string-face)

    ("^[\t, ]*\\_<bool\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<int\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<boolean\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<hex\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<tristate\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<string\\_>" . font-lock-type-face)

    ("^[\t, ]*\\_<def_bool\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<depends on\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<select\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<help\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<---help---\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<default\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<prompt\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<range\\_>" . font-lock-variable-name-face)

    ("\\s-[1-9]+\\s-" . font-lock-constant-face)
    ("\\s-[yn]\\s-" . font-lock-constant-face)

    ("^\\_<choice\\_>" . font-lock-keyword-face)
    ("^\\_<endchoice\\_>" . font-lock-keyword-face)
    ("^\\_<config\\_>" . font-lock-keyword-face)
    ("^\\_<comment\\_>" . font-lock-keyword-face)
    ("^\\_<menu\\_>" . font-lock-keyword-face)
    ("^\\_<endmenu\\_>" . font-lock-keyword-face)
    ("^\\_<if\\_>" . font-lock-keyword-face)
    ("^\\_<endif\\_>" . font-lock-keyword-face)
    ("^\\_<menuconfig\\_>" . font-lock-keyword-face)
    ("^\\_<source\\_>" . font-lock-keyword-face)))

(defvar kconfig-headings
  '("bool" "int" "boolean" "hex" "tristate" "string" "def_bool" "depends on"
    "select" "help" "---help---" "default" "prompt" "range" "choice" "endchoice"
    "config" "comment" "menu" "endmenu" "if" "endif" "menuconfig" "source"))

(defun kconfig-outline-level ()
  (looking-at "[\t ]*")
  (let ((prefix (match-string 0))
	(result 0))
    (dotimes (i (length prefix) result)
      (setq result (+ result
		      (if (equal (elt prefix i) ?\s)
			  1 tab-width))))))

(defun kconfig-completion-at-point ()
  "Kconfig completion function added to `completion-at-point-functions'."
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end kconfig-headings . nil )))

(defconst kconfig-imenu-generic-expression
  '(("Config" "^config *\\(.+\\)" 1)))

(define-derived-mode kconfig-mode text-mode "Kconfig"
  "Major mode for editing Kconfig files in the Linux kernel."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(kconfig-mode-font-lock-keywords t))
  (setq-local outline-regexp (concat "^[\t ]*" (regexp-opt kconfig-headings)))
  (setq-local outline-level 'kconfig-outline-level)
  (setq-local imenu-generic-expression kconfig-imenu-generic-expression)
  (add-hook 'completion-at-point-functions 'kconfig-completion-at-point nil 'local))

;;;###autoload
(add-to-list 'auto-mode-alist '("Kconfig" . kconfig-mode))

(provide 'kconfig-mode)
;;; kconfig-mode.el ends here
