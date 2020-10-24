;;; Utils --- Utils library of FG42 -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2020 Sameer Rahmani <lxsameer@gnu.org>
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
;; `System' is just a state monad which holds the state of the editor.
;; Each system has to have a `start' function to start the setup process.
;;
;;; Code:

(require 'cl-lib)

(autoload 'seq-partition "seq")
(autoload 'cl-reduce "cl-seq")


(defun buffer-mode (buffer-or-string)
  "Return the major mode associated with a the given BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))


(defun ->buffer (buffer-name data &optional fn)
  "Insert the given DATA into the given buffer provided by BUFFER-NAME.

It will create a the buffer if it doesn't exist.  It will call the given FN
at the end in context of the buffer.  This function accepts only one argument
with is the buffer."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (insert data)
      (when fn
        (funcall fn buf)))))


(defun ->str (&rest args)
  "Convert the given ARGS into string."
  (funcall #'pp-to-string args))


(defmacro inspect-expression (&rest body)
  "Pretty prints the result of the given BODY."
  `(pp-display-expression ,@body (get-buffer-create fg42/inspect-buffer)))


(defun inspect-data-append (data)
  "Append the given DATA to the inspection buffer with padding."
  ;; TODO: Move 'fg42/inspect-buffer' to the somewhere propriate
  ;;       possiblly the system.
  (->buffer
   "fg42/inspect-buffer"
   (format
     "\n;; START ======================================================\n%s%s"
     (pp-to-string data)
     ";; END.\n")))


(defun apply-face (face-symbol text)
  "Apply the given FACE-SYMBOL to the given TEXT."
  (put-text-property 0 (length text) 'face face-symbol text))


(defmacro comment (&rest _body)
  "A macro similar to Clojure's comment macro that ignore the BODY."
  (declare (indent 0))
  `nil)


(defmacro debug-message (&rest params)
  "Print out the given PARAMS only if debug mode is on."
  (if debug-on-error
      `(message ,@params)
    nil))


(defmacro deprecated (msg &rest form)
  "Mark the given FORM as deprecated with the given MSG."
  (declare (indent 0))
  `(progn
     (warn (format "[DEPRECATED]: %s" ,msg))
     ,@form))


;; TODO: A good candidate for an inline function
(defun find-value-for (lst key)
  "Return the value of the given  KEY in the given LST.
For example for a list like (list :x 4 :y 5) we can find the value of
`:x' by doing `(get-value-for lst :x)'."
  (let ((pairs (seq-partition lst 2)))
    (let ((pair (assq key pairs)))
      (when pair
        (cadr pair)))))


(defun comp (&rest fns)
  "Compose the given list of FNS into one function that accepts multiple values.
For example:
 (funcall (compose (lambda (x) (+ 1 x)) (lambda (x) (* x s))) 5)
or
 (funcall (compose #'some-fn #'message) some-value)"
  (lambda (&rest values)
    (cl-reduce 'funcall (butlast fns)
               :from-end t
               :initial-value (apply (car (last fns)) values))))


(defun path-join (&rest paths)
  "Join the given PATHS."
  (apply #'concat
         (append
          (mapcar #'file-name-as-directory (butlast paths))
          (last paths))))


(defmacro -> (x &optional form &rest more)
  "Thread the expr through the forms FORM and rest of form in MORE.
Insert X as the second item in the first form, making a list of
it if it is not a list already.  If there are more forms, insert
the first form as the second item in second form, etc."
  (declare (debug (form &rest [&or symbolp (sexp &rest form)])))
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,(car form) ,x ,@(cdr form))
                  (list form x)))
   (:else `(-> (-> ,x ,form) ,@more))))


(defmacro ->> (x &optional form &rest more)
  "Thread the expr through the forms FORM and the rest at MORE.
Insert X as the last item in the first form, making a list of
it if it is not a list already.  If there are more forms, insert
the first form as the
last item in second form, etc."
  (declare (debug ->))
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,@form ,x)
                  (list form x)))
   (:else `(->> (->> ,x ,form) ,@more))))


(provide 'fg42/utils)
;;; utils.el ends here
