;;; Cube --- Cube library of FG42 -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2020 Sameer Rahmani & Contributors
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://gitlab.com/FG42/FG42
;; Version: 3.0.0
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
;;; Commentary:
;; Cubes are the building blocks of any `FG42' editor.  Each `cube' is a
;; unit which defines different abilities in a deterministic and idempotent
;; way.  Cubes are composable and a composition of cubes creates an editor.
;;
;;; Code:

(require 'seq)
(require 'fg42/utils)
(require 'fg42/state)


(defun fg42/cube-apply (state cube)
  "Apply the given CUBE to the given STATE.
It returns a new state."
  state)


(defun fg42/cube-bind (m1 m2)
  "Bind the M1 to M2.
M1 and M2 are state monads.  See `fg42/utils'"
  (lambda (state)
    (let* ((v (funcall cube1 state)))
      (funcall cube2
        (fg42/cube-apply
                 ;; State
                 (car v)
                 ;; Value of in the monad from M
                 (cdr v))))))


(defun fg42/cube-compose (cube1 cube2)
  "Compose CUBE1 and CUBE2 to create a new cube.
For example `(fg42/cube-compose #\'some-cube #\'some-other-cube)'"
  (lambda ()
    (fg42/cube-bind
     (funcall cube1)
     (funcall cube2))))


(defun fg42/cube-identity ()
  "Cube identity function."
  (lambda (state)
    (cons state '())))


(defun fg42/cubes (&rest cubes)
  "Create a new cube out of the given list of CUBES."
  (seq-reduce  (lambda (cube1 cube2)
                 (fg42/cube-compose cube1 cube2))
               cubes
               #'fg42/cube-identity))



(provide 'fg42/cube)
;;; cube.el ends here
