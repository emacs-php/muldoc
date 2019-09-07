;;; mldoc.el --- Multi ElDoc integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2019
;; Version: 0.2.1
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
(require 'cl-lib)
(require 'eldoc)
(require 'nadvice)

;; Custom variables:
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

(defvar-local mldoc-propertizer* nil
  "Dynamic bound list of propertizers.")

;; Utility functions
(defsubst mldoc-in-string ()
  "Return non-nil if inside a string.
it is the character that will terminate the string, or t if the string should be terminated by a generic string delimiter."
  (nth 3 (syntax-ppss)))

(defsubst mldoc-in-comment ()
  "Return nil if outside a comment, t if inside a non-nestable comment, else an integer (the current comment nesting)."
  (nth 4 (syntax-ppss)))

(defsubst mldoc-in-string-or-comment ()
  "Return character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss)))

;; Macros
(defmacro define-mldoc (name docstring &rest body)
  "Define `NAME' as a ElDoc-MLDOC compatible function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...)."
  (declare (doc-string 2) (indent defun))
  `(defun ,name ()
     ,docstring
     (let ((doc (save-excursion ,@body)))
       (if mldoc-returns-string
           (mapconcat #'identity (apply #'mldoc--build-list doc) "")
         doc))))

(defun mldoc--propertize-arg (arg is-current-arg arg-spec)
  "Propertize `ARG' by `IS-CURRENT-ARG' and `ARG-SPEC'."
  (cl-loop
   for spec in arg-spec
   collect
   (if (stringp spec)
       spec
     (let ((v (plist-get arg spec)))
       (if (null v)
           ""
         (if (and is-current-arg (eq :name spec))
             (propertize v 'face '(:weight bold))
           (mldoc--propertize-keyword arg spec)))))))

(defun mldoc--propertize-args (args current-arg arg-separator &optional arg-spec)
  "Return propertized string by `ARGS' list, `CURRENT-ARG', `ARG-SEPARATOR' and `ARG-SPEC'."
  (mapconcat
   #'append
   (cl-loop for arg in args
            for n = 0 then (1+ n)
            append
            (mldoc--propertize-arg
             (if (stringp arg) (list :name arg) arg)
             (eq current-arg n)
             (or arg-spec (list :name))))
   (or arg-separator ", ")))

(defun mldoc--propertizers-to-list (propertizer)
  "Return list for function `propertize' by `PROPERTIZER' alist."
  (cl-loop for (p . sym) in propertizer
           append (list p (symbol-value sym))))

(defun mldoc--propertize-keyword (values key)
  "Return propertized string `KEY' in `VALUES' plist, by `FACES'."
  (let* ((val (plist-get values key))
         (str (if (functionp val)
                  (funcall val)
                val))
         (prop (cdr-safe (assq key mldoc-propertizer*))))
    (if (null prop)
        (or str "")
      (apply #'propertize str (mldoc--propertizers-to-list prop)))))

(cl-defmacro mldoc-list (spec &key function propertizers args current-arg values)
  "Build a list acceptable by MLDoc."
  `(list ,spec
         :function ,function
         :propertizers ,propertizers
         :args ,args
         :current-arg ,current-arg
         :values ,values))

(cl-defun mldoc--build-list (spec &key function propertizers args current-arg values)
  "Return a list of propertized string for ElDoc.

integer `:current-arg'
    0-origin offset to current position of arguments.
plist `values'
    Property list of (:name value)
"
  (setq values (plist-put values :function function))
  (setq values (plist-put values :args args))
  (let ((mldoc-propertizer*
         (append propertizers mldoc-default-eldoc-propertizers)))
    (cl-loop
     for s in spec collect
     (cond
      ((stringp s) s)
      ((symbolp s)
       (if (not (keywordp s))
           (symbol-value s)
         (mldoc--propertize-keyword values s)))
      ((listp s) (mldoc--evalute-spec s args current-arg))))))

(defun mldoc--evalute-spec (spec args current-arg)
  "Evalute `ARGS' and embedded element of `SPEC'."
  (let ((f (car spec))
        (rest (cdr spec)))
    (cl-case f
      (args (mldoc--propertize-args args current-arg (car rest) (cdr rest)))
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
