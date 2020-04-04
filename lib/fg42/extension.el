;;; extension --- Extension library of FG42
;;
;; Copyright (c) 2010-2020  Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://gitlab.com/FG42/FG42
;; Version: 0.1.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Acknoledgement:
;; This library is heavily inspired by Kite mini library. Kudos Tung Dao
;; for his great work.
;;
;;; Commentary:
;;; Code:

;; This library provides some basic means to create a new FG42 extensions
(require 'cl-lib)
(require 'fg42/utils)

;; Variables -----------------------------
(defvar activated-extensions ()
  "A list of all activated extensions.")

(defvar disabled-abilities (make-hash-table)
  "A hash of all the disabled abilities.")

;; Structures -----------------------------
(cl-defstruct fg42-extension
  "Each FG42 extension should implement a copy of this structure."
  name

  ;; Let's keep this field for backward compatiblity for a while
  docs

  ;; Each extension should expose a info page.
  doc-index
  ;; Projectile provides a project type that we can use to
  ;; activate/load the extensions based on their registered
  ;; type.
  project-types

  (version nil)

  ;; An instance of fg42-actions structure that describe the
  ;; different actions of the given extension
  (actions nil)
  (path nil)
  ;; Callbacks
  (on-initialize nil)
  (on-load)
  (on-unload))

;; Functions ------------------------------
(defun active-ability? (name)
  "Return t if ability with the given NAME was not in disabled-abilities."
  (if (gethash name disabled-abilities) nil t))


(defun disable (&rest abilities)
  "Add the given ABILITIES to disabled-abilities hash."
  (dolist (abl abilities)
    (puthash abl t disabled-abilities)))


(defun extension-path (extension)
  "Return the path to the given EXTENSION."
  (or (fg42-extension-path extension)
      ;; TODO: should we extract variables such as `fg42-home' to their
      ;;       dedicated ns in order to avoid the warning ?
      (concat fg42-home "/lib/extensions/" (fg42-extension-name extension) ".el")))


(defun load-extension (ext)
  "Load the given EXT which is a `fg42-extension' instance."
  (load-file (extension-path ext))
  (when-let ((init-fn (eval (fg42-extension-on-initialize ext))))
    (apply (symbol-function init-fn) '(ext))))


;; Macros ---------------------------------
(defmacro ability (name deps &rest body)
  "Define an ability with the given NAME, DEPS, and BODY.

*deps* should be a list of abilities with the defined ability dependens
to them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 2) (indent 0))
  `(if (active-ability? (intern ,(symbol-name name)))
       (when (null (delq t (mapcar 'active-ability? (quote ,deps))))
         ,@body)))


(defmacro defability (name deps &optional docs &rest body)
  "Define an ability with the given NAME, DEPS, DOCS and BODY.

*deps* should be a list of abilities with the defined ability dependens
to them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 3) (indent 2))
  ;; TODO: there's no point of using `if' in the quoted code. evaluate
  ;; the `if' in compile time and return nil or evalute the body.
  `(if (active-ability? (intern ,(symbol-name name)))
       (when (null (delq t (mapcar 'active-ability? (quote ,deps))))
         ,@body)))


(defmacro extension (name &rest args)
  "A simple DSL to define new fg42 extension by given NAME and ARGS."
  ;(declare (doc-string 1) (indent 1))
  `(setq ,name (apply 'make-fg42-extension :name ,(symbol-name name) (quote ,args))))


(defmacro defextension (name &optional docstring &rest args)
  "A simple DSL to define new fg42 extension by given NAME, DOCSTRING and ARGS."
  (declare (doc-string 2) (indent 1))
  ;; TODO: Inject the docstring to the current `system' in order
  ;;       to collect it later for `describe-extension' function.
  `(setq ,name (apply 'make-fg42-extension
                      :name ,(symbol-name name)
                      (quote ,args))))


(defmacro with-ability (name &rest body)
  "If the ability with the given NAME is not disabled, Run the BODY."
  `(when (active-ability? (intern ,(symbol-name name)))
     ,@body))


(defun describe-extension (extension)
  "Show the doc-string of the EXTENSION."
  (interactive)
  (let ((doc-file (fg42-extension-docs (symbol-value extension)))
        (b (get-buffer-create (concat "*" (symbol-name extension) " docs*"))))
    (set-buffer b)
    (insert-file-contents (concat fg42-home "/" doc-file))
    (read-only-mode t)
    (switch-to-buffer b)
    (org-mode)))


(comment
    (defextension example-extension
      "Very simple extention as a test"
      :path "~/example-extension.el"
      :on-initialize 'example-extention-init)

    (load-extension example-extension))


(provide 'fg42/extension)
;;; extension ends here
