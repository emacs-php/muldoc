;;; mldoc.el --- Multi ElDoc integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Version: 0.1.0
;; Keywords: tools, lisp
;; Homepage: https://github.com/emacs-php/mldoc
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
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

;; MLDoc integrates multiple ElDoc providers and helps inline documentation.

;;; Code:
(require 'cl-lib)
(require 'eldoc)
(require 'nadvice)

(defgroup mldoc nil
  "Multi ElDoc integration"
  :group 'tools)

(defcustom mldoc-mode-advice-when-eldoc-is-set nil
  "When T, advice `eldoc-documentation-function' by MLDoc if it is set."
  :type 'boolean)

(defvar mldoc-default-eldoc-propertizers
  '((:function . ((face . font-lock-function-name-face)))
    (:return-type . ((face . font-lock-type-face)))
    (:type . ((face . font-lock-type-face)))
    (:warning . ((face . font-lock-warning-face))))
  "Alist of default propertize spec for MLDoc.")

(defvar-local mldoc-documentation-functions nil
  "Functions to call to doc string.")

(defvar-local mldoc-returns-string t
  "When not-NIL, MLDoc functions return ElDoc complatible string.")

(defmacro define-mldoc (name docstring &rest body)
  "Define `NAME' as a ElDoc-MLDOC compatible function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...)."
  (declare (doc-string 2) (indent defun))
  `(defun ,name ()
     ,docstring
     (let ((doc (save-excursion ,@body)))
       (if mldoc-returns-string
           (mapconcat #'identity (apply #'mldoc-build-list doc) "")
         doc))))

(defun mldoc--propertize-args (args current-arg arg-separator)
  "Return propertized string by `ARGS' list, `CURRENT-ARG' and `ARG-SEPARATOR'."
  (mapconcat
   #'identity
   (cl-loop for arg in args
            for n = 0 then (1+ n)
            collect (if (not (and current-arg (eq n current-arg)))
                        arg
                      (propertize arg 'face '(:weight bold))))
   (or arg-separator ", ")))

(defun mldoc--propertizers-to-list (propertizer)
  "Return list for function `propertize' by `PROPERTIZER' alist."
  (cl-loop for (p . sym) in propertizer
           append (list p (symbol-value sym))))

(defun mldoc--propertize-keyword (values key propertizers)
  "Return propertized string `KEY' in `VALUES' plist, by `FACES'."
  (let* ((val (plist-get values key))
         (str (if (stringp val)
                  val
                (funcall val)))
         (prop (cdr-safe (assq key propertizers))))
    (if (null prop)
        str
      (apply #'propertize str (mldoc--propertizers-to-list prop)))))

(cl-defmacro mldoc-list (spec &key function propertizers args current-arg values)
  ""
  `(list ,spec
         :function ,function
         :propertizers ,propertizers
         :args ,args
         :current-arg ,current-arg
         :values ,values))

(cl-defun mldoc-build-list (spec &key function propertizers args current-arg values)
  "Return a list of propertized string for ElDoc.

integer `:current-arg'
    0-origin offset to current position of arguments.
plist `values'
    Property list of (:name value)
"
  (message "spec: %s" spec)
  (setq values (plist-put values :function function))
  (setq values (plist-put values :args args))
  (setq propertizers (append propertizers mldoc-default-eldoc-propertizers))

  (cl-loop
   for s in spec collect
   (cond
    ((stringp s) s)
    ((symbolp s)
     (if (not (keywordp s))
         (symbol-value s)
       (mldoc--propertize-keyword values s propertizers)))
    ((listp s)
     (if (eq 'args (car s))
         (mldoc--propertize-args args current-arg (nth 1 s))
       (apply (car s) (cdr s)))))))

(defun mldoc-eldoc-function ()
  "ElDoc backend function by MLDoc package."
  (let* ((mldoc-returns-string nil)
         (documentation (cl-loop for f in mldoc-documentation-functions
                                 thereis (funcall f))))
    (when documentation
      (mapconcat #'identity (apply #'mldoc-build-list documentation) ""))))

;;;###autoload
(define-minor-mode mldoc-mode
  "Minor mode for Multi ElDoc."
  (if (and eldoc-documentation-function
           (not mldoc-mode-advice-when-eldoc-is-set))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'mldoc-eldoc-function)
    (setq-local eldoc-documentation-function #'mldoc-eldoc-function))
  (eldoc-mode 1))

(provide 'mldoc)
;;; mldoc.el ends here
