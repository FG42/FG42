;;; serene-init --- The entry point for serene extension
;;; Commentary:
;;; Code:
(defvar serene-simple-mode-map
  (make-sparse-keymap))

(defvar serene-nrepl-host "127.0.0.1")
(defvar serene-nrepl-port 5544)
(defvar serene-nrepl-process nil)

(defconst serene-simple-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))


(defface serene-simple-mode-special-froms-face
  '((t :inherit font-lock-builtin-face))
  "Face of special forms."
  :group 'simple-serene-mode)

(defface serene-simple-mode-builtin-fns-face
  '((t :inherit font-lock-keyword-face))
  "Face of builtin functions."
  :group 'simple-serene-mode)

(defface serene-simple-mode-builtin-types-face
  '((t :inherit font-lock-type-face))
  "Face of built in types."
  :group 'simple-serene-mode)

(defvar serene-simple-mode-special-forms
  '("do" "let" "def" "fn" "quote" "cond" "if"))


(defconst serene-simple-mode-builtin-fns
  '("=" ">" "<" ">=" "<=" "and" "or" "not" "first" "rest" "println"
    "quit" "+" "*" "/" "-" "conj" "mod" "new"))


(defconst serene-simple-mode-builtin-types
  '("System" "String" "Boolean"))


(define-derived-mode serene-simple-mode
  scheme-mode "Serene(Simple)"
  "Major mode for Serene simple.")



(defun serene-simple-add-keywords (face-name keyword-rules)
  "Set the FACE-NAME for keywords in serene-simple using KEYWORD-RULES."
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp
          (concat
           "\\("
           (regexp-opt keyword-list)
           "\\)")))
    (font-lock-add-keywords 'serene-simple-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                 'serene-simple-indent-function
                 (car x)))
        keyword-rules))


(serene-simple-add-keywords 'serene-simple-mode-special-froms-face
                            (mapcar (lambda (x) (cons 1 (intern x))) serene-simple-mode-special-forms))

(serene-simple-add-keywords 'serene-simple-mode-builtin-fns-face
                            (mapcar (lambda (x) (cons 1 (intern x))) serene-simple-mode-builtin-fns))

(serene-simple-add-keywords 'serene-simple-mode-builtin-types-face
                            (mapcar (lambda (x) (cons 1 (intern x))) serene-simple-mode-builtin-types))


(defun serene-expr-at-point ()
  "Return sexp before the point."
  (interactive)
  (let ((opoint (point))
	(left-quote ?‘)
	expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
	;; If this sexp appears to be enclosed in `...' or ‘...’
	;; then ignore the surrounding quotes.
	(cond ((eq (preceding-char) ?’)
	       (progn (forward-char -1) (setq opoint (point))))
	      ((or (eq (following-char) ?\')
		   (eq (preceding-char) ?\'))
	       (setq left-quote ?\`)))

        ;; When after a named character literal, skip over the entire
        ;; literal, not only its last word.
        (when (= (preceding-char) ?})
          (let ((begin (save-excursion
                         (backward-char)
                         (skip-syntax-backward "w-")
                         (backward-char 3)
                         (when (looking-at-p "\\\\N{") (point)))))
            (when begin (goto-char begin))))

	(forward-sexp -1)
	;; If we were after `?\e' (or similar case),
	;; use the whole thing, not just the `e'.
	(when (eq (preceding-char) ?\\)
	  (forward-char -1)
	  (when (eq (preceding-char) ??)
	    (forward-char -1)))

	;; Skip over hash table read syntax.
	(and (> (point) (1+ (point-min)))
	     (looking-back "#s" (- (point) 2))
	     (forward-char -2))

	;; Skip over `#N='s.
	(when (eq (preceding-char) ?=)
	  (let (labeled-p)
	    (save-excursion
	      (skip-chars-backward "0-9#=")
	      (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
	    (when labeled-p
	      (forward-sexp -1))))

	(save-restriction
	  (if (eq (following-char) left-quote)
              ;; vladimir@cs.ualberta.ca 30-Jul-1997: Skip ` in `variable' so
              ;; that the value is returned, not the name.
	      (forward-char))
          (when (looking-at ",@?") (goto-char (match-end 0)))
	  (narrow-to-region (point-min) opoint)
	  (setq expr (read (current-buffer)))
          ;; If it's an (interactive ...) form, it's more useful to show how an
          ;; interactive call would use it.
          ;; FIXME: Is it really the right place for this?
          (when (eq (car-safe expr) 'interactive)
	    (setq expr
                  `(call-interactively
                    (lambda (&rest args) ,expr args))))
	  expr)))))

(defun serene-eval-expr-at-point ()
  "Send the expression at point to the nRepl for evaluation."
  (interactive)
  (let* ((buf (process-buffer serene-nrepl-process))
         (expr (format "%s\n" (replace-regexp-in-string
                               (regexp-quote "\n")
                               " "
                               (prin1-to-string (serene-expr-at-point))
                               nil 'literal))))
    (process-send-string serene-nrepl-process expr)))


(defun serene-nrepl-sentinel (process event)
  "The sentinel fn for the given PROCESS to process the EVENT."
  (message (format "Process: %s had the event '%s'" process event)))

(defun serene-process-incoming-result (process value)
  "Process the incoming VALUE of the given PROCESS."
  (if (not (= (length value) 0))
      (let ((status (substring value 0 1)))
        (if (string= status "0")
            (message (format "=> %s" (substring value 1 -1)))
          (message (format "Error: %s" (substring value 1 -1)))))
    (message "Bad response from nRepl.")))

(defun serene-nrepl-connect ()
  "Connect to the running nRepl of Serene and return the process value."
  (interactive)
  (if (not serene-nrepl-process)
      (let ((p (open-network-stream "serene-nrepl"
                                    (get-buffer-create "*serene-repl*")
                                    serene-nrepl-host
                                    serene-nrepl-port)))
        (setq serene-nrepl-process p)
        (set-process-sentinel p 'serene-nrepl-sentinel)
        (set-process-filter p 'serene-process-incoming-result)
        (message (format "Connected to tcp://%s:%s"
                         serene-nrepl-host
                         serene-nrepl-port)))
    serene-nrepl-process))

(defun serene-nrepl-disconnect ()
  "Disconnect from Serene nRepl."
  (interactive)
  (when serene-nrepl-process
    (delete-process serene-nrepl-process)
    (setq serene-nrepl-process nil)
    (message "Disconnected from the nRepl.")))

(defun serene-nrepl-status ()
  "Return the status of the connection to Serene's nRepl."
  (interactive)
  (if serene-nrepl-process
      (message (format "%s" (process-status serene-nrepl-process)))
    (message "Disconnected.")))

(provide 'extensions/serene/serene-simple-mode)
;;; serene-simple-mode.el ends here
