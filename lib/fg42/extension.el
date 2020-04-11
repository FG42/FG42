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
(defvar fg42/extensions '()
  "A list of official FG42 extensions.")


(deprecated
  "`activated-extensions' is deprecated use `fg42/extensions' instead."
  (defvar activated-extensions ()
    "A list of all activated extensions."))


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


(cl-defstruct fg42-extension-preload
  "This struct describes preload data for each extensions.

FG42 uses this data to install and autoload the extension."
  name

  ;; The docstring for the preload only which describes the extension.
  (docstring "")

  (version nil)
  ;; What project types (projectile types) requires this plugin
  ;; to be active.
  (project-types '())

  ;; If the project type is nil (for example a simple text file) what
  ;; suffixes are related to this extension
  (file-suffixes '())

  ;; The straight reciepe to use in order to install the extension
  (reciepe nil))


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


(defmacro defpreload (name docstring &rest args)
  "Define a fg42 extension preload by given NAME, DOCSTRING and ARGS."
  (declare (doc-string 2) (indent defun))
  (let ((fn-name (intern (format "fg42-preload-fn-%s" name))))
    `(progn
       (defvar ,(intern (format "%s-preload" name))
         (make-fg42-extension-preload :name ,(symbol-name name)
                                      :docstring ,docstring
                                      ,@args))
       ;; I usually don't like this approach but at the moment it's the best
       ;; bet we have. we need a named function to be able to remove it
       ;; later from `auto-mode-alist'
       (defun ,fn-name ()
         (message "IM HEREEEEEEEEEEEEEEEEEEEEEEEEEEEE")
         (use-package ,name
           :straight '(,name
                       :host 'gitlab
                       :repo (format "FG42/%s" ,name)))))))


(comment
  auto-mode-alist
  (macroexpand-1
   '(defpreload fg42-elisp
      "eLisp extension for FG42"
      :version "3.0.0"
      :file-suffixes '("\\.el\\'"))))

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


(defun fg42-register-preload (system preload)
  "Registers the given PRELOAD on the given SYSTEM."
  (setf (fg42-system-preloads system)
        (add-to-list (fg42-system-preloads system) preload)))


(defun fg42--setup-preload (system preload)
  "Setup the given PRELOAD against the given SYSTEM.
It will setup the necessary tools to load the extension related to the given
PRELOAD by looking into project types and file suffixes."
  (mapcar
   (lambda (suffix)
     (add-to-list 'auto-mode-alist
                  (cons
                   suffix
                   (list (intern (format "fg42-preload-fn-%s"
                                         (fg42-extension-preload-name preload)))))))
   (fg42-extension-preload-file-suffixes preload)))


(defun fg42-setup-extensions (system)
  "Setup the preloads for the given SYSTEM."
  (mapcar (lambda (preload)
            (message "herere")
            (fg42--setup-preload system preload))
          (fg42-system-preloads system)))


(comment
    (defextension example-extension
      "Very simple extention as a test"
      :path "~/example-extension.el"
      :on-initialize 'example-extention-init)
    (defsystem testsystem
      "FG42 implemented in term of systems and this is the default system."
      :start (lambda (system)
               (message "AAA")))

    (mapcar (lambda (x) (message "XXXXX"))
            (fg42-system-preloads testsystem))
    (load-extension example-extension))


(provide 'fg42/extension)
;;; extension ends here
