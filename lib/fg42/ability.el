;;; ability --- ability library of FG42
;;; Commentary:
;;;
;;; Code:
(require 'cl-lib)

(defvar fg42--abilities '()
  "Internal data structure to store abilities.")

(defvar fg42--active-abilities-conflicts (make-hash-table)
  "Hash table containing all active abilities and list of their conflicting abilities.")

(defun fg42--check-conflicts (ability)
  "Return list of conflicting abilities with given ABILITY."
  (let ((conflicts '()))
      (maphash (lambda (active-ability ability-conflicts)
                 (when (member ability ability-conflicts)
                   (add-to-list 'conflicts active-ability))
                 ) fg42--active-abilities-conflicts)
      conflicts))

(cl-defstruct fg42-ability
  "Each FG42 ability should implement a copy of this structure."
  name
  docs
  depends-on
  (start! nil)
  (stop! nil))


(defmacro defability (name deps conflicts &optional docs &rest body)
  "Define an ability with the given NAME, DEPS, CONFLICTS DOCS and BODY.

*deps* should be a list of abilities with the defined ability dependens
to them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 3) (indent 2))
  ;; TODO: there's no point of using `if' in the quoted code. evaluate
  ;; the `if' in compile time and return nil or evalute the body.
  `(if (active-ability? (intern ,(symbol-name name)))
       (let ((conflicting-abilitis (fg42--check-conflicts (quote ,name))))
       (if (null conflicting-abilitis)
           (when
               (null (delq t (mapcar 'active-ability? (quote ,deps))))
             ,@body
             (puthash (quote ,name) ,conflicts fg42--active-abilities-conflicts))
         (error (format "%s ability has conflicts with %s" (quote ,name) conflicting-abilitis))))))

(comment
  (defability ivy
    '()
    '(helm ido)
    "Completion using ivy."
    (require 'ivy)
    (require 'counsel)))

(defun register-ability (ability-name)
  "Register the given ABILITY-NAME as an activity."
  (add-to-list
   fg42--abilities
   '(ability-name . (make-fg42-ability :name ability-name))))

(defun start-ability (ability-name))

(provide 'fg42/ability)
;;; ability ends here
