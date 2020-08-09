;;; ability --- ability library of FG42
;;; Commentary:
;;;
;;; Code:
(require 'cl-lib)

(defvar fg42--abilities '()
  "Internal data structure to store abilities.")

(cl-defstruct fg42-ability
  "Each FG42 ability should implement a copy of this structure."
  name
  doc
  ;; deps is a plist of ability dependencies either packages or other abilities.
  ;; deps has two keys :pkgs and :abilities
  deps
  ;; init function initialize the ability.
  init)

(defmacro defability (name doc deps &rest init)
  "Define an ability with the given NAME, DEPS, DOC and INIT.

*deps* should be a list of abilities with the defined ability dependens
to them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 3) (indent 2))
  (if (active-ability? (intern name)
                       `(make-fg42-ability
                         :name ,name
                         :doc ,doc
                         :deps ,deps
                         :init (lambda ()
                                 ,@init))
                       `(message "Ability is not disabled by user."))))



(defun register-ability (ability-name)
  "Register the given ABILITY-NAME as an activity."
  (add-to-list
   fg42--abilities
   '(ability-name . (make-fg42-ability :name ability-name))))

(defun start-ability (ability-name))

(provide 'fg42/ability)
;;; ability ends here
