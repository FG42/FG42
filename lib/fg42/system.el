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


(defun start-system (system)
  "Start the given SYSTEM.

This is the default function to boot a system.  Each system might
provide a different function to boo rather than this one."
  (require 'fg42))


(cl-defstruct fg42-system
  "A `system' describes a FG42 instance. Everything that is needed
to load FG42."
  name
  (packages '())
  (abilities '())
  ;; A function which takes the `system' and starts it.
  (start 'start-system)
  (stop nil))


(defmacro defsystem (name &optional docstring &rest body)
  "Define a system with the given NAME, DOCSTRING and BODY."
  (declare (doc-string 2) (indent 1))
  `(setq ,name (apply 'make-fg42-system
                      :name ,(symbol-name name)
                      (quote ,body))))


(comment
    (defsystem FG42
      "docstring"
      :packages '(('elisp-extension :version "1.3.3"))
      :abilities '()))

(provide 'fg42/system)
;;; system.el ends here
