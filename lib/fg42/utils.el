;;; Utils --- A collection of utility functions for FG42
;;; Commentary:
;;; Code:

(require 'cl)
(require 'json)

(require 'fg42/vars)


;;; JSON ----------------------------------------------------------------------
(defun ->json (data)
  "Convert the given DATA to json."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))


(defun <-json (data)
  "Convert the given json DATA to elisp data structure."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))


;;; Buffer helpers ------------------------------------------------------------
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
      (funcall fn buf))))


(defun inspect-as-json-and-switch (data)
  "Convert the given DATA to json and put it in the debug buffer."
  (->buffer fg42/inspect-buffer
            (->json data)
            #'(lambda (buf)
                (require 'json-mode)
                (json-mode)
                (json-pretty-print-buffer)
                (switch-to-buffer buf))))


(defun inspect-as-json (data)
  "Convert the given DATA to json and put it in the debug buffer."
  (->buffer fg42/inspect-buffer
            (->json data)
            #'(lambda (buf)
                (require 'json-mode)
                (json-mode)
                (json-pretty-print-buffer))))

(defmacro inspect-expression (&rest body)
  "Pretty prints the result of the given BODY."
  `(pp-display-expression ,@body (get-buffer-create fg42/inspect-buffer)))

(defun inspect-data-append (data)
  (->buffer fg42/inspect-buffer
            "START ========================================================\n")
  (->buffer fg42/inspect-buffer (pp-to-string data))
  (->buffer fg42/inspect-buffer "END.\n"))


(provide 'fg42/utils)
;;; utils.el ends here