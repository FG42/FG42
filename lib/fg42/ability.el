;;; ability --- ability library of FG42
;;; Commentary:
;;;
;;; Code:
(require 'cl-lib)
(require 'seq)

(defvar fg42--abilities '()
  "Internal data structure to store abilities.")

(defvar fg42--activated-abilities '()
  "Data structure to hold a list of all abilities activated.")

(cl-defstruct fg42-ability
  "Each FG42 ability should implement a copy of this structure."
  name
  doc
  ;; deps is a plist of ability dependencies either packages or other abilities.
  ;; deps has two keys :pkgs and :abilities
  deps
  ;; conflicts holds a list of ability names.
  conflicts
  ;; init function initialize the ability.
  init
  ;;
  has-initialized
  )

(defun conflicting? (ability1 ability2)
  "Check to see whether ability1 has any conflicts with ability2."
  (let* ((ability1-conflicts (fg42-ability-conflicts ability1))
         (ability2-conflicts (fg42-ability-conflicts ability2))
         (conflicts '()))
    (mapc (lambda (conflict1)
            (when (eq conflict1 (fg42-ability-name ability2))
              (add-to-list 'conflicts conflict1)
              )
            )
          ability1-conflicts
          )
    (mapc (lambda (conflict2)
            (when (eq conflict2 (fg42-ability-name ability1))
              (add-to-list 'conflicts conflict2)
              )) ability2-conflicts)
    conflicts))

(defun check-for-conflicts-with-activated (activated ability)
  "Check if given ABILITY has any conflicts in ACTIVATED abilites."
  (let* ((all-conflicts '()))
    (mapcar (lambda (active)
              (let*
                  ((this-conflicts (conflicting? active ability)))
                (when (> (length this-conflicts) 0)
                  (append this-conflicts all-conflicts)))

            ) activated)))
;; should return a function that has a wrapped lambda that changes the state, outter function checks if ability should be activated, if so runs thet function.
(defmacro defability (name deps doc conflicts &rest init)
  "Define an ability with the given NAME, DEPS, DOC, CONFLICTS and INIT.

*deps* should be a list of abilities with the defined ability dependens
on them.

*conflicts* should be a list of abilities which this ability has conflict with them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 3) (indent 2))
  (if (active-ability? name)

      `(setq ,(intern (concat "fg42--abilities/" (symbol-name name)) (make-fg42-ability
                                                                      :name name
                                                                      :doc ,doc
                                                                      :deps ,deps
                                                                      :conflicts ,conflicts
                                                                      :init (lambda ()
                                                                              ,@init)
                                                                      :has-initialized false))
             `(message "Ability is not disabled by user."))))

(defun resolve-pkgs (ab)
  "Resolve all dependencies."
  (mapc #'depends-on (plist-get (fg42-ability-deps ab) :pkgs)))

(defun resolve-abilities (ab)
  (mapc (lambda (to-resolve-ability)
          (activate-ability to-resolve-ability)) (plist-get (fg42-ability-deps ab) :abilities)))

(defun activate-ability (ab)
  "Activate given AB by checking."
  (unless (fg42-ability-has-initialized ab)

    ;; check if ability has any conflicts.
    (check-for-conflicts-with-activated fg42--activated-abilities ab)

    ;; check if all deps are ready and if not initialze them.
    (resolve-pkgs ab)
    (resolve-abilities ab)

    ;; run initialize function
    (funcall (fg42-ability-init ab))))


(defun register-ability (ability-name)
  "Register the given ABILITY-NAME as an activity."
  (add-to-list
   fg42--abilities
   '(ability-name . (make-fg42-ability :name ability-name))))

(provide 'fg42/ability)
;;; ability ends here
