;;; extension --- Extension library of FG42
;;; Commentary:
;;; Code:

;; This library provides some basic means to create a new FG42 extensions
(require 'cl)

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
  (print-debugger nil)
  (abilities '()))


(defun ability-doc-string (ability)
  "Return doc string of autoloaded symbol."
      (nth 2 (symbol-function ability)))

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


(defmacro defability (name deps &optional docs &rest body)
  "Define an ability with the given NAME, DEPS, DOCS and BODY.

*deps* should be a list of abilities with the defined ability dependens
to them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 3) (indent 2))
  (autoload name "" docs)
  ;; TODO: there's no point of using `if' in the quoted code. evaluate
  ;; the `if' in compile time and return nil or evalute the body.
  `(if (active-ability? (intern ,(symbol-name name)))
       (when (null (delq t (mapcar 'active-ability? (quote ,deps))))
         ,@body)))

(defmacro extension (name &rest args)
  "A simple DSL to define new fg42 extension by given NAME and ARGS."
  ;(declare (doc-string 1) (indent 1))
  (let* ((abilities (plist-get args :abilities))
         (abilities-with-doc (mapcar (lambda (ability)
                                       (list :name ability :doc (ability-doc-string ability))) abilities))
         (extension-fields (plist-put args :abilities abilities-with-doc)))
    `(setq ,name (apply 'make-fg42-extension :name ,(symbol-name name) (quote ,extension-fields)))))


(defmacro with-ability (name &rest body)
  "If the ability with the given NAME is not disabled, Run the BODY."
  `(when (active-ability? (intern ,(symbol-name name)))
     ,@body))


(defun describe-extension (extension)
  "Show the doc-string of the EXTENSION."
  (interactive "sExtension:")
  (message "%s" (symbol-value (intern extension))))



(provide 'fg42/extension)
;;; extension ends here
