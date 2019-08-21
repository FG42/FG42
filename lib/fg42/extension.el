;;; extension --- Extension library of FG42
;;; Commentary:
;;; Code:

;; This library provides some basic means to create a new FG42 extensions
(require 'cl-lib)

;; Variables -----------------------------
(defvar activated-extensions ()
  "A list of all activated extensions.")

(defvar disabled-abilities (make-hash-table)
  "A hash of all the disabled abilities.")

;; TODO: add a function to extension structure to support for
;; external dependenies list
;; Structures -----------------------------
(cl-defstruct fg42-extension
  "Each FG42 extension should implement a copy of this structure."
  name
  docs
  (version nil)
  ;; Describes
  (major-modes nil)
  ;; Callbacks
  (on-initialize nil)
  (on-load)
  ;; An associated array of major modes to their
  ;; debugger function
  (print-debugger nil))


;; Functions ------------------------------
(defun active-ability? (name)
  "Return t if ability with the given NAME was not in disabled-abilities."
  (if (gethash name disabled-abilities) nil t))


(defun disable (&rest abilities)
  "Add the given ABILITIES to disabled-abilities hash."
  (dolist (abl abilities)
    (puthash abl t disabled-abilities)))


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


(defmacro extension (name &rest args)
  "A simple DSL to define new fg42 extension by given NAME and ARGS."
  ;(declare (doc-string 1) (indent 1))
  `(setq ,name (apply 'make-fg42-extension :name ,(symbol-name name) (quote ,args))))


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


(provide 'fg42/extension)
;;; extension ends here
