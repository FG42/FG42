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
  docs
  depends-on
  (start! nil)
  (stop! nil))

(defun register-ability (ability-name)
  "Register the given ABILITY-NAME as an activity."
  (add-to-list
   fg42--abilities
   '(ability-name . (make-fg42-ability :name ability-name))))


(provide 'fg42/ability)
;;; ability ends here
