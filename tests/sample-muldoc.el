;;; sample-muldoc.el --- Example implementation of MulDoc -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Keywords: lisp
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is sample implementation for MulDoc.
;; "Samp" is a tiny programming language that similar to C.
;;
;;     foo(1, 2, 3);
;;     bar("hoge", "fuga", "piyo");
;;     echo SAMP_VERSION;
;;

;;; Code:
(require 'muldoc)
(require 'cc-langs)

;; Samp Language definitions:
(eval-and-compile
  (defvar samp-builtin-functions
  '(("foo" . (("a" :type "int") ("b" :type "int") ("c" :type "int")))
    ("bar" . (("x" :type "str") ("y" :type "str") ("z" :type "str")))))
  (defvar samp-lang-keywords
    '("echo"
      "return"))
  (defvar samp-defined-constants
    '(("SAMP_VERSION" "Version number of Samp language")
      ("M_PI" "Approximation of pi (the ratio of a circle's circumference)"))))

;; Helper functions:
;; These functions need to be rewritten for your language syntax.
(defun samp-current-arg-info ()
  "Return parameter information of current position."
  (let ((name (save-excursion
                (beginning-of-line)
                (thing-at-point 'symbol t))))
    (assoc-string name samp-builtin-functions)))

(defun samp-current-arg-pos ()
  "Return 0-origin n-th number of parameters."
  ;; Notice: This function is oversimplified.
  ;; For example, in this simple language, function calls do not exceed newlines and
  ;; there is only one call in a line.  It also does not assume that the string contains ",".
  (save-excursion
    (let ((cur (point))
          (eol (progn (end-of-line) (point))))
      (beginning-of-line)
      (cl-loop for i from 0
               while (search-forward "," eol t)
               when (< cur (point))
               return i
               finally return i))))

;; MulDoc implementations:
(defcustom samp-muldoc-func-form
  '(:function "(" (params ", " :type " " :name) ")")
  "MulDoc display form for Samp function call."
  :group 'samp-muldoc
  :type 'sexp)

(defcustom samp-muldoc-const-form
  '(:constant ": " :desc)
  "MulDoc display form for Samp constant."
  :group 'samp-muldoc
  :type 'sexp)

(define-muldoc samp-muldoc-func
  "MulDoc function for Foo language."
  ;; This function is extremely simplified, but represents the specification of
  ;; the value that an actual implementation should return.
  (when-let (spec (samp-current-arg-info))
    (let ((name (car spec))
          (params (mapcar (lambda (s) (cons :name s)) (cdr spec))))
      (muldoc-list samp-muldoc-func-form
                  :params params
                  :current-param (samp-current-arg-pos)
                  :values (list :function name)))))

(define-muldoc samp-muldoc-const
  "MulDoc function for Foo language."
  ;; This function is extremely simplified, but represents the specification of
  ;; the value that an actual implementation should return.
  (when-let (const (assoc (thing-at-point 'symbol t) samp-defined-constants))
    (let ((name (car const))
          (desc (cadr const)))
      (muldoc-list samp-muldoc-const-form
                  :values (list :constant name :desc desc)))))

;; Major mode for *.samp
(defvar samp-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

(defvar samp-mode-font-lock-keywords
  (eval-when-compile
    `((,(regexp-opt samp-lang-keywords 'symbols) 0 'font-lock-keyword-face)
      (,(regexp-opt (mapcar #'car samp-builtin-functions) 'symbols) 0 'font-lock-builtin-face)
      (,(regexp-opt (mapcar #'car samp-defined-constants) 'symbols) 0 'font-lock-constant-face))))

(define-derived-mode samp-mode prog-mode "Samp"
  "Major mode for editing Samp code."
  :syntax-table samp-mode-syntax-table
  (setq-local font-lock-defaults '(samp-mode-font-lock-keywords)))

;; User function (~/.emacs.d/init.el):
(defun user-setup-samp-mode ()
  "User defined function for hook."
  (muldoc-mode +1)
  (add-to-list 'muldoc-documentation-functions #'samp-muldoc-func)
  (add-to-list 'muldoc-documentation-functions #'samp-muldoc-const))

(with-eval-after-load "sample-muldoc"
  (add-hook 'samp-mode-hook #'user-setup-samp-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.samp\\'" . samp-mode) t)

(provide 'sample-muldoc)
;;; sample-muldoc.el ends here
