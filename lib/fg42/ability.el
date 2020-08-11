;;; ability --- ability library of FG42
;;; Commentary:
;;;
;;; Code:
(require 'cl-lib)
(require 'seq)

(defvar fg42--abilities-state '()
  "Data structure to hold a list of all abilities activated.")

(defvar fg42--all-abilities '() "Plist of all abilites defined, not necessarily activated.")

(cl-defstruct fg42-ability
  "Each FG42 ability should implement a copy of this structure. Registering an Ability has 2 stages, updating state using *defability* macro and then running state usin a runner."
  name
  doc
  ;; deps is a plist of ability dependencies either packages or other abilities.
  ;; deps has two keys :pkgs and :abilities
  deps
  ;; conflicts holds a list of ability names.
  conflicts
  ;; init function initialize the ability.
  init
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

(defun conflicts-with-activated (activated ability)
  "Check if given ABILITY has any conflicts in ACTIVATED abilites."
  (let* ((all-conflicts '()))
    (mapcar (lambda (active)
              (let*
                  ((this-conflicts (conflicting? active ability)))
                (when (> (length this-conflicts) 0)
                  (append this-conflicts all-conflicts))

                activated)))))

(defun should-ability-be-activated? (name)
  "Wether given NAME should be activated. An ability should be activated only if it's not in the state yet, and has no conflicts."
  ;; check if ability is already in the state.
  (and (not (member name fg42--abilities-state)) (not (conflicts-with-activated fg42--abilities-state (plist-get fg42--all-abilities name)))))

(defun resolve-abilities (ability)
  "Resolve given ABILITY ability dependencies. Adds given ABILITY ability deps to fg42--activated-abilites if they are not already there."
  (mapc (lambda (to-resolve-ability)
          (when (should-ability-be-activated? to-resolve-ability)
            (activate-ability to-resolve-ability)))
        (plist-get (fg42-ability-deps ability) :abilities)))

(defun activate-ability (ability)
  "Activate given ABILITY."
  ;; Resolve all deps. Package will get installed in runtime since they are side effect.
  (resolve-abilities ability)
  ;; put it in the state to treat that as an active one.
  (plist-put fg42--abilities-state (fg42-ability-name ability) ability))



;; should return a function that has a wrapped lambda that changes the state, outter function checks if ability should be activated, if so runs thet function.
(defmacro defability (name deps doc conflicts &rest init-fn)
  "Define an ability with the given NAME, DEPS, DOC, CONFLICTS and INIT-FN.

*deps* should be a list of abilities with the defined ability dependens
on them.

*conflicts* should be a list of abilities which this ability has conflict with them.

*body* is a block of code which will run as the ability initializer code."
  (declare (doc-string 3) (indent 2))
  (let* ((ability (make-fg42-ability
                   :name name
                   :doc doc
                   :deps deps
                   :conflicts conflicts
                   :init init-fn)))
    (plist-put fg42--all-abilities name ability)

    (if (should-ability-be-activated? name)
        `(progn
           (activate-ability ,ability))

      `(message "ability %s can't be activated." ,name))))

(defun fg42/build (state)
  "fg42/build build up the editor based on given STATE. It basically do a map on the list of abilities and for each of them resolve package dependencies and runs init fcuntion."
  )
(provide 'fg42/ability)
;;; ability ends here
