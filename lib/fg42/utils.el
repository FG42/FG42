;;; Utils --- A collection of utility functions for FG42
;;; Commentary:
;;; Code:

(require 'cl)

(require 'fg42/vars)
(require 'fg42/utils/json)

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
      (when fn
        (funcall fn buf)))))


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
  "Append the given DATA to the inspection buffer with padding."
  (->buffer
   fg42/inspect-buffer
   (format
     "\n;; START ======================================================\n%s%s"
     (pp-to-string data)
     ";; END.\n")))


(defun apply-face (face-symbol text)
  "Apply the given FACE-SYMBOL to the given TEXT."
  (put-text-property 0 (length text) 'face face-symbol text))


(defmacro comment (&rest body)
  "A macro similar to Clojure's comment macro that ignore the BODY."
  (declare (indent 0))
  `nil)


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


(defun read-file (path)
  "Return the content of the file at the given PATH as string."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))


(defun file->lisp (path)
  "Read the content of the file at PATH and return the Lisp in it."
  (read (or (read-file path) "()")))


(defun lisp->file (path expr)
  "Write the given EXPR to the given file at PATH."
  (with-temp-file path
    (print expr (current-buffer))))


(provide 'fg42/utils)
;;; utils.el ends here
