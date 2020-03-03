;;; actions --- Extension action library of FG42
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


(cl-defstruct fg42-actions
  "This data structure contains all the actions that are common
among different extensions."
  ;; A function that starts the development server for the given extension if
  ;; there's any.
  (start-lang-sever nil)

  ;; A function that stops the development server for the given extension if
  ;; there's any.
  (stop-lang-sever nil)

  ;; Action for inserting a print expr in the code
  (print-expr nil))


(defmacro actions (&rest body)
  "A simple DSL to define a list of actions for an extension and BODY."
  ;(declare (doc-string 1) (indent 1))
  `(setq ,name (apply 'make-fg42-extension :name ,(symbol-name name) (quote ,args))))


(provide 'fg42/actions)
;;; actions.el ends here
