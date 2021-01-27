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
;; 'M' => (state . value)
;;; Code:

(require 'seq)
(require 'fg42/utils)


(defun fg42/state-value (s &optional v)
  "Return a monadic value from state S and optional value V."
  (cons s v))


(comment
  (fg42/state-value '(2)))


(defun fg42/state-return (x)
  "Return a state monad containing X."
  (lambda (state)
    (fg42/state-value state x)))


(comment
  (funcall (fg42/state-return 4) '(3)))


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
    (let ((init (list '(mode->keybindings . ())
                      '(mode->prefix . ())
                      '(mode->fn . ())
                      '(dependencies . ())
                      '(cubes . ()))))
      (fg42/state-value init init))))


(comment
  (assoc 'cubes (funcall (fg42/state-unit) 3)))


(defun fg42/state-get (k &optional default)
  "Return the value of the given K from the STATE.
It will return DEFAULT in case of a missing key."
  (lambda (state)
    (fg42/state-value
     state
     (or (cdr (assoc k state)) default))))


(comment
  (funcall (fg42/state-get 'name) '((age . 22) (name . "sameer"))))


(defun fg42/state-cons (k v)
  "Add the given value V to the list addressed by key K on STATE."
  (lambda (state)
    (fg42/state-value
     (cons
      (cons k
            (cons v
                  (funcall (fg42/state-get k '()) state)))
      state))))


(defun fg42/state-bind (m f)
  "Bind the monad M to the monadic function F."
  (lambda (state)
    (let ((v (funcall m state)))
      (funcall (funcall f (cdr v)) (car v)))))


(defun fg42/state-compose-states (&rest ms)
  "Bind monad in MS together."
  (lambda (state)
    (cond
     ((= 0 (length ms)) (fg42/state-value state))
     ((= 1 (length ms)) (fg42/state-run (car ms) state))
     (t
      (seq-reduce
       (lambda (composition m)
         (fg42/state-run m (car composition)))
       (cdr ms)
       (fg42/state-run (car ms) state))))))


(comment
  (fg42/state-run
   (fg42/state-compose-states
    (lambda (s) (fg42/state-value (cons 4 s) 14))
    (lambda (s) (fg42/state-value (cons 5 s) 15))
    (lambda (s) (fg42/state-value (cons 6 s) 16)))
   '())

  (funcall
   (fg42/state-bind
    (lambda (state)
      (cons '((age . 3)) '(4 5)))
    (lambda (x)
      (lambda (state)
        (cons
         state
         (+ (car x) (cadr x) 10)))))
   '()))


(defun fg42/state-compose (f1 f2)
  "Compose the given function F1 with F2."
  (lambda (&rest xs)
    (>>= (apply f1 xs) f2)))


(comment
  (setq s 10)
  (defun add (x)
    (lambda (state) (cons (+ state x) (- x 1))))
  (defun mul (x)
    (lambda (state)
      (fg42/state-value (* state x) (- x 1))))

  (funcall
   (funcall (fg42/state-compose #'add #'mul) 20)
   s))


(defun fg42/state-run (m s)
  "Evaluate the given state monad M with the given state S."
  (funcall m s))


(provide 'fg42/state)
;;; state.el ends here
