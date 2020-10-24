;; State --- State library of FG42 -*- lexical-binding: t; -*-
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
(require 'fg42/utils)


(defun fg42/state-return (x)
  "Return a state monad containing X."
  (lambda (state)
    (cons state x)))


(defun fg42/state-bind-maker (binder-fn)
  "Return a bind function for state monad.
It creates a bind function that binds monadic functions by applying BINDER-FN
to the return state and the value inside the monad before binding them.

The BINDER-FN has to return a cons in form of (state . prev-v)."
  (lambda (m f)
    "Applys F on the M monad."
    (lambda (state)
      (let* ((v (funcall m state))
             (new-v (funcall (f (cdr v)) (car v))))
        (funcall binder-fn
                 ;; State
                 (car new-v)
                 ;; Value of in the monad from M
                 (cdr new-v))))))


(defun fg42/state-unit ()
  "Create a new state monad.
This monad describes the entire editor configuration"
  ;; Why not a cl-struct ?
  ;; Because first, we want it to be human readable
  (lambda (state)
    (cons
     (list '(:mode->keybindings . ())
           '(:mode->prefix . ())
           '(:mode->fn . ())
           '(:cubes . ()))
     ())))


(defun fg42/state-get (k &optional default)
  "Return the value of the given K from the STATE.
It will return DEFAULT in case of a missing key."
  (lambda (state)
    (or (cdr (assoc k state)) default)))



(defun fg42/state-cons (k v)
  "Add the given value V to the list addressed by key K on STATE."
  (lambda (state)
    (cons (cons k (cons v (or (fg42/state-get state k) '()))) state)))


(provide 'fg42/state)
;;; state.el ends here
