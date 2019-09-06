;;; mldoc-test.el --- Tests for MLDoc                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Friends of Emacs-PHP development

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

;; Tests for MLDoc.

;;; Code:
(require 'ert)
(require 'mldoc)
(require 'cl-lib)

(ert-deftest mldoc-test--build-list ()
  (let ((data
         `(("Empty spec and no keywords"
            ,(mldoc--build-list '())
            ,(list))
           ("Spec has a string"
            ,(mldoc--build-list '(""))
            ,(list ""))
           ("Spec has function value"
            ,(mldoc--build-list '(:foo ": " :function)
                           :function "f" :values (list :foo "hoge"))
            ,(list "hoge" ": "
                   (propertize "f" 'face font-lock-function-name-face))))))
    (cl-loop for (desc actual expected) in data
             do (should (equal (cons desc expected) (cons desc actual))))))

(provide 'mldoc-test)
;;; mldoc-test.el ends here
