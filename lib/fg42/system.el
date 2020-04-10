;;; system --- System library of FG42
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

(require 'cl-lib)
(require 'fg42/utils)

(defvar fg42--active-system nil
  "A private variable to store the active system.
Use `fg42-get-current-system' instead")

(defun default-start-system (system)
  "Start the given SYSTEM.

This is the default function to boot a system.  Each system should
provide a different function to boot rather than this one."
  (error "Not implemented"))



(cl-defstruct fg42-system
  "A `system' describes a FG42 instance. Everything that is needed
to load FG42."
  name
  docstring
  (packages '())
  ;; A list of preloads to setup extensions which are not loaded yet.
  ;; For more information on preloads checkout `fg42/extension'
  (preloads '())

  (abilities '())
  ;; A function which takes the `system' and starts it.
  (start (lambda (system) (default-start-system system)))
  (stop nil))


(defmacro defsystem (name &optional docstring &rest body)
  "Define a system with the given NAME, DOCSTRING and BODY."
  (declare (doc-string 2) (indent 1))
  (let ((form (if (boundp (intern (format "%s" name))) 'setq 'defvar)))
    `(,form ,name (make-fg42-system
                   :name ,(symbol-name name)
                   :docstring ,docstring
                   ,@body))))


(defun fg42-get-current-system ()
  "Return the current active system of FG42."
  fg42--active-system)


(defun fg42-set-current-system! (system)
  "Set the current system to the given SYSTEM."
  ;; TODO: In the future when we moved to paralel boot
  ;;       we need to make sure that this funciton
  ;;       sets the state safely.
  (setq fg42--active-system system))


(defun fg42-start-system ()
  "Start the system from `fg42-get-current-system'."
  (debug-message "Starting the default system.")
  (let ((sys (fg42-get-current-system)))
    (funcall (funcall 'fg42-system-start sys) sys)))


(comment
  (macroexpand-1 '(defsystem testsystem
                    "docstring"
                    :preloads '(2 43 4)
                    :packages '(('elisp-extension :version "1.3.3"))
                    :abilities '()))
  (defsystem testsystem
    "docstring1"
    :preloads '(2 43 4)
    :packages '(('elisp-extension :version "1.3.3"))
    :abilities '())

  (make-fg42-system :name "asd" :preloads '(213 452) :abilities '(x y))
  (aset testsystem 2 "sam")
  (setf (fg42-system-preloads testsystem) '(3 3 3 3 3))
  (fg42-set-current-system! testsystem)
  (fg42-system-preloads testsystem)
  (start-system)
  (fg42-system-start testsystem))



(provide 'fg42/system)
;;; system.el ends here
