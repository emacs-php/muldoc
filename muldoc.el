;;; muldoc.el --- Multi ElDoc integration            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Version: 0.4.0
;; Keywords: tools, extension
;; Homepage: https://github.com/emacs-php/muldoc
;; Package-Requires: ((emacs "25.1"))
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

;; MulDoc integrates multiple ElDoc providers and helps inline documentation.

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'eldoc)
(require 'nadvice)

;; Custom variables:
(defgroup muldoc nil
  "Multi ElDoc integration."
  :group 'tools)

(defcustom muldoc-mode-advice-when-eldoc-is-set nil
  "When T, advice `eldoc-documentation-function' by MulDoc if it is set."
  :type 'boolean)

(defvar muldoc-lighter " MulDoc")

(defvar muldoc-default-eldoc-propertizers
  '((:function . ((face . font-lock-function-name-face)))
    (:constant . ((face . font-lock-constant-face)))
    (:return-type . ((face . font-lock-type-face)))
    (:type . ((face . font-lock-type-face)))
    (:warning . ((face . font-lock-warning-face))))
  "Alist of default propertize spec for MulDoc.")

(defvar-local muldoc-documentation-functions nil
  "Functions to call to doc string.")

(defvar-local muldoc--old-eldoc-documentation-function nil)

(defvar-local muldoc-returns-string t
  "When not-NIL, MulDoc functions return ElDoc complatible string.")

(defvar-local muldoc-propertizer* nil
  "Dynamic bound list of propertizers.")

;; Utility functions for users
(defsubst muldoc-in-string ()
  "Return non-nil if inside a string.
It is the character that will terminate the string, or t if the string should be
terminated by a generic string delimiter."
  (nth 3 (syntax-ppss)))

(defsubst muldoc-in-comment ()
  "Return NIL if outside a comment, T if inside a non-nestable comment.
Otherwise return an integer (the current comment nesting)."
  (nth 4 (syntax-ppss)))

(defsubst muldoc-in-string-or-comment ()
  "Return character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss)))

;; Macros
(defmacro define-muldoc (name docstring &rest body)
  "Define NAME as a ElDoc-MULDOC compatible function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...)."
  (declare (doc-string 2) (indent defun))
  `(defun ,name ()
     ,docstring
     (let ((doc (save-excursion ,@body)))
       (if muldoc-returns-string
           (mapconcat #'identity (apply #'muldoc--build-list doc) "")
         doc))))

(defun muldoc--propertize-param (param is-current-param doc-form)
  "Propertize PARAM by IS-CURRENT-PARAM and DOC-FORM."
  (mapconcat
   (lambda (spec)
    (if (stringp spec)
        spec
      (let ((v (plist-get param spec)))
        (if (null v)
            ""
          (if (and is-current-param (eq :name spec))
              (propertize v 'face '(:weight bold))
            (muldoc--propertize-keyword param spec))))))
   doc-form
   ""))

(defun muldoc--propertize-params (params current-param param-separator &optional param-form)
  "Return propertized string by PARAMS list, CURRENT-PARAM, PARAM-SEPARATOR.
PARAM-FORM is recursively expanded by `muldoc--build-list' as form."
  (let ((n 0))
    (mapconcat
     (lambda (param)
       (prog1
           (muldoc--propertize-param
            (if (stringp param) (list :name param) param)
            (eq current-param n)
            (or param-form (list :name)))
         (setq n (1+ n))))
     params
     (or param-separator ", "))))

(defun muldoc--propertizers-to-list (propertizer)
  "Return list for function `propertize' by PROPERTIZER alist."
  (cl-loop for (p . sym) in propertizer
           nconc (list p (symbol-value sym))))

(defun muldoc--propertize-keyword (values key)
  "Return propertized string KEY in VALUES plist."
  (let* ((val (plist-get values key))
         (str (if (functionp val)
                  (funcall val)
                val))
         (prop (cdr-safe (assq key muldoc-propertizer*))))
    (if (not (and prop str))
        (or str "")
      (apply #'propertize str (muldoc--propertizers-to-list prop)))))

(cl-defmacro muldoc-list (form &key propertizers params current-param values)
  "Build a list acceptable FORM by MulDoc.

list FORM
    List consisting of strings, keywords and expressions.
alist PROPERTIZERS
    Alist of propertize spec.
list PARAMS
    Repeat list or doc spec.
integer CURRENT-PARAM
    0-origin offset to current position of arguments.
plist VALUES
    Property list of (:name value)"
  `(list ,form
         :propertizers ,propertizers
         :params ,params
         :current-param ,current-param
         :values ,values))

(cl-defun muldoc--build-list (form &key propertizers params current-param values)
  "Return a list of propertized string for ElDoc.

See `muldoc-list' about FORM, PROPERTIZERS, PARAMS, CURRENT-PARAM, VALUES
parameters."
  (setq values (plist-put values :params params))
  (let ((muldoc-propertizer*
         (append propertizers muldoc-default-eldoc-propertizers)))
    (cl-loop
     for s in form collect
     (cond
      ((null s) nil)
      ((stringp s) s)
      ((keywordp s) (muldoc--propertize-keyword values s))
      ((symbolp s) (symbol-value s))
      ((listp s) (muldoc--evalute-spec s params current-param))))))

(defun muldoc--evalute-spec (spec params current-param)
  "Evalute PARAMS and embedded element of SPEC."
  (let ((f (car spec))
        (rest (cdr spec)))
    (cl-case f
      (params (muldoc--propertize-params params current-param (car rest) (cdr rest)))
      (if (if (eval (car rest)) (eval (nth 1 rest)) (eval (cons 'progn (cddr rest)))))
      (when (when (eval (car rest)) (eval (cons 'progn (cdr rest)))))
      (unless (unless (eval (car rest)) (eval (cons 'progn (cdr rest)))))
      (eval (eval (cons 'progn rest)))
      (t (eval spec)))))

(defun muldoc-eldoc-function ()
  "ElDoc backend function by MulDoc package."
  (let (muldoc-returns-string)
    (when-let (documentation (cl-loop for f in muldoc-documentation-functions
                                      thereis (funcall f)))
      (mapconcat #'identity (apply #'muldoc--build-list documentation) ""))))

(defun muldoc--setup-variables ()
  "Setup variables for turn on muldoc-mode."
  (setq muldoc--old-eldoc-documentation-function eldoc-documentation-function)
  (if (and eldoc-documentation-function
           (not muldoc-mode-advice-when-eldoc-is-set))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'muldoc-eldoc-function)
    (setq-local eldoc-documentation-function #'muldoc-eldoc-function))
  (eldoc-mode 1))

(defun muldoc--restore-variables ()
  "Restore variables for turn off muldoc-mode."
  (setq-local eldoc-documentation-function muldoc--old-eldoc-documentation-function))

;;;###autoload
(define-minor-mode muldoc-mode
  "Minor mode for Multi ElDoc."
  :lighter muldoc-lighter
  (if muldoc-mode
      (muldoc--setup-variables)
    (muldoc--restore-variables)))

(provide 'muldoc)
;;; muldoc.el ends here
