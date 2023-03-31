;;; muldoc-test.el --- Tests for MulDoc              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Keywords: maint

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

;; Tests for MulDoc.

;;; Code:
(require 'ert)
(require 'muldoc)
(require 'cl-lib)

(ert-deftest muldoc-test--build-list ()
  (let ((data
         `(("Empty spec and no keywords"
            ,(muldoc--build-list '())
            ,(list))
           ("Spec has a string"
            ,(muldoc--build-list '(""))
            ,(list ""))
           ("Spec has function value"
            ,(muldoc--build-list '(:foo ": " :function)
                           :values (list :function "f" :foo "hoge"))
            ,(list "hoge" ": "
                   (propertize "f" 'face font-lock-function-name-face)))
           ("Spec has arg list"
            ,(muldoc--build-list '(:function "(" (params ", ") ")")
                           :values (list :function "f")
                           :params '("a" "b" "c")
                           :current-param 2)
            ,(list (propertize "f" 'face font-lock-function-name-face)
                   "("
                   (concat  "a, " (propertize "b" 'face '(:weight bold)) ", c")
                   ")"))
           )))
    (cl-loop for (desc actual expected) in data
             do (should (equal (cons desc expected) (cons desc actual))))))

(ert-deftest muldoc-test--propertize-param ()
  (let ((data
         ;; (muldoc--propertize-arg arg is-current-param arg-spec)
         `(("Simple argument"
            ,(muldoc--propertize-param '(:name "a") nil '(:name))
            "a")
           ("Simple argument and current argument"
            ,(muldoc--propertize-param '(:name "a") t '(:name))
            ,(propertize "a" 'face '(:weight bold)))
           ("Argument has :name and :type"
            ,(muldoc--propertize-param '(:name "a" :type "string")
                               nil
                               '(:name " / " :type))
            ,(concat "a / " (propertize "string" 'face font-lock-function-name-face))))))
    (cl-loop for (desc actual expected) in data
             do (should (equal (cons desc expected) (cons desc actual))))))

(ert-deftest muldoc-test--evalute-spec ()
  (let ((data
         `(("if-spec passed `T' as cond, returns 1."
            ,(muldoc--evalute-spec '(if (eq 1 1) 1 2) nil nil)
            1)
           ("if-spec passed `NIL' as cond, returns 2."
            ,(muldoc--evalute-spec '(if (eq 1 2) 1 2) nil nil)
            2)
           ("if-spec passed `NIL' as cond and multiple else clause, returns 3."
            ,(muldoc--evalute-spec '(if (eq 1 2) 1 2 3) nil nil)
            3)
           ("when-spec passed `T', return 1."
            ,(muldoc--evalute-spec '(when (eq 1 1) 1) nil nil)
            1)
           ("when-spec passed `NIL', return NIL."
            ,(muldoc--evalute-spec '(when (eq 1 2) 1) nil nil)
            nil)
           ("unless-spec passed `T', return `NIL'."
            ,(muldoc--evalute-spec '(unless (eq 1 1) 1) nil nil)
            nil)
           ("unless-spec passed `NIL', return 1."
            ,(muldoc--evalute-spec '(unless (eq 1 2) 1) nil nil)
            1)
           ("eval-spec passed `1', return 1."
            ,(muldoc--evalute-spec '(eval 1) nil nil)
            1)
           ("eval-spec passed `emacs-version', return value of emacs-version."
            ,(muldoc--evalute-spec '(eval emacs-version) nil nil)
            ,emacs-version)
           ("function-spec passed `(symbol-value emacs-version)', return value of emacs-version."
            ,(muldoc--evalute-spec '(symbol-value 'emacs-version) nil nil)
            ,emacs-version))))
    (cl-loop for (desc actual expected) in data
             do (should (equal (cons desc expected) (cons desc actual))))))

(provide 'muldoc-test)
;;; muldoc-test.el ends here
