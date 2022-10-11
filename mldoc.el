;;; mldoc.el --- Multi ElDoc integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Version: 0.3.0
;; Keywords: tools, extension
;; Homepage: https://github.com/emacs-php/mldoc
;; Package-Requires: ((emacs "24.4"))
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
(eval-when-compile
  (require 'cl-lib))
(require 'eldoc)
(require 'nadvice)

;; Custom variables:
(defgroup mldoc nil
  "Multi ElDoc integration."
  :group 'tools)

(defcustom mldoc-mode-advice-when-eldoc-is-set nil
  "When T, advice `eldoc-documentation-function' by MLDoc if it is set."
  :type 'boolean)

(defvar mldoc-lighter " MLDoc")

(defvar mldoc-default-eldoc-propertizers
  '((:function . ((face . font-lock-function-name-face)))
    (:constant . ((face . font-lock-constant-face)))
    (:return-type . ((face . font-lock-type-face)))
    (:type . ((face . font-lock-type-face)))
    (:warning . ((face . font-lock-warning-face))))
  "Alist of default propertize spec for MLDoc.")

(defvar-local mldoc-documentation-functions nil
  "Functions to call to doc string.")

(defvar-local mldoc--old-eldoc-documentation-function nil)

(defvar-local mldoc-returns-string t
  "When not-NIL, MLDoc functions return ElDoc complatible string.")

(defvar-local mldoc-propertizer* nil
  "Dynamic bound list of propertizers.")

;; Utility functions for users
(defsubst mldoc-in-string ()
  "Return non-nil if inside a string.
It is the character that will terminate the string, or t if the string should be
terminated by a generic string delimiter."
  (nth 3 (syntax-ppss)))

(defsubst mldoc-in-comment ()
  "Return NIL if outside a comment, T if inside a non-nestable comment.
Otherwise return an integer (the current comment nesting)."
  (nth 4 (syntax-ppss)))

(defsubst mldoc-in-string-or-comment ()
  "Return character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss)))

;; Macros
(defmacro define-mldoc (name docstring &rest body)
  "Define NAME as a ElDoc-MLDOC compatible function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...)."
  (declare (doc-string 2) (indent defun))
  `(defun ,name ()
     ,docstring
     (let ((doc (save-excursion ,@body)))
       (if mldoc-returns-string
           (mapconcat #'identity (apply #'mldoc--build-list doc) "")
         doc))))

(defun mldoc--propertize-param (param is-current-param doc-form)
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
            (mldoc--propertize-keyword param spec))))))
   doc-form
   ""))

(defun mldoc--propertize-params (params current-param param-separator &optional param-form)
  "Return propertized string by PARAMS list, CURRENT-PARAM, PARAM-SEPARATOR.
PARAM-FORM is recursively expanded by `mldoc--build-list' as form."
  (let ((n 0))
    (mapconcat
     (lambda (param)
       (prog1
           (mldoc--propertize-param
            (if (stringp param) (list :name param) param)
            (eq current-param n)
            (or param-form (list :name)))
         (setq n (1+ n))))
     params
     (or param-separator ", "))))

(defun mldoc--propertizers-to-list (propertizer)
  "Return list for function `propertize' by PROPERTIZER alist."
  (cl-loop for (p . sym) in propertizer
           nconc (list p (symbol-value sym))))

(defun mldoc--propertize-keyword (values key)
  "Return propertized string KEY in VALUES plist."
  (let* ((val (plist-get values key))
         (str (if (functionp val)
                  (funcall val)
                val))
         (prop (cdr-safe (assq key mldoc-propertizer*))))
    (if (not (and prop str))
        (or str "")
      (apply #'propertize str (mldoc--propertizers-to-list prop)))))

(cl-defmacro mldoc-list (form &key propertizers params current-param values)
  "Build a list acceptable FORM by MLDoc.

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

(cl-defun mldoc--build-list (form &key propertizers params current-param values)
  "Return a list of propertized string for ElDoc.

See `mldoc-list' about FORM, PROPERTIZERS, PARAMS, CURRENT-PARAM, VALUES
parameters."
  (setq values (plist-put values :params params))
  (let ((mldoc-propertizer*
         (append propertizers mldoc-default-eldoc-propertizers)))
    (cl-loop
     for s in form collect
     (cond
      ((null s) nil)
      ((stringp s) s)
      ((keywordp s) (mldoc--propertize-keyword values s))
      ((symbolp s) (symbol-value s))
      ((listp s) (mldoc--evalute-spec s params current-param))))))

(defun mldoc--evalute-spec (spec params current-param)
  "Evalute PARAMS and embedded element of SPEC."
  (let ((f (car spec))
        (rest (cdr spec)))
    (cl-case f
      (params (mldoc--propertize-params params current-param (car rest) (cdr rest)))
      (if (if (eval (car rest)) (eval (nth 1 rest)) (eval (cons 'progn (cddr rest)))))
      (when (when (eval (car rest)) (eval (cons 'progn (cdr rest)))))
      (unless (unless (eval (car rest)) (eval (cons 'progn (cdr rest)))))
      (eval (eval (cons 'progn rest)))
      (t (eval spec)))))

(defun mldoc-eldoc-function ()
  "ElDoc backend function by MLDoc package."
  (let* ((mldoc-returns-string nil)
         (documentation (cl-loop for f in mldoc-documentation-functions
                                 thereis (funcall f))))
    (when documentation
      (mapconcat #'identity (apply #'mldoc--build-list documentation) ""))))

(defun mldoc--setup-variables ()
  "Setup variables for turn on mldoc-mode."
  (setq mldoc--old-eldoc-documentation-function eldoc-documentation-function)
  (if (and eldoc-documentation-function
           (not mldoc-mode-advice-when-eldoc-is-set))
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'mldoc-eldoc-function)
    (setq-local eldoc-documentation-function #'mldoc-eldoc-function))
  (eldoc-mode 1))

(defun mldoc--restore-variables ()
  "Restore variables for turn off mldoc-mode."
  (setq-local eldoc-documentation-function mldoc--old-eldoc-documentation-function))

;;;###autoload
(define-minor-mode mldoc-mode
  "Minor mode for Multi ElDoc."
  :lighter mldoc-lighter
  (if mldoc-mode
      (mldoc--setup-variables)
    (mldoc--restore-variables)))

(provide 'mldoc)
;;; mldoc.el ends here
