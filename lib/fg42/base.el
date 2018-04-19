(require 'cl-lib)
(require 'fg42/extension)

;; Vars -----------------------------------
(defvar default-theme nil "Default FG42 theme.")

;; Macros ---------------------------------
(defmacro theme (name &optional local)
  "Mark the given theme name as default them.
local should be 't' if theme is on FG42 it self"
  `(progn
     (setq default-theme ',(intern (symbol-name name)))
     (when (not (null ,local))
       (depends-on default-theme))))

;; Functions ------------------------------
(defun load-default-theme ()
  "Load the given theme name"
  (require default-theme)

  ;; Setup the face look up function for spaceline
  (with-ability spaceline
                (let ((other-face (intern (concat (symbol-name default-theme)
                                                  "-spaceline-faces"))))
                  (if (functionp other-face)
                    (setq spaceline-face-func other-face))))

  ;; Call the function name with same name as the them which should
  ;; be responsible for loading the actual "custom-theme"
  (funcall (symbol-function default-theme)))

(defun load--extension (extension)
  "Load a single extension and call its :on-initialize function"
  (let ((lib (concat "extensions/" (symbol-name extension))))
    (require (intern lib))))

(defun initialize--extension (extension)
  "Initialize given extension by calling its :on-initialize function."
  (let ((init-func (fg42-extension-on-initialize (symbol-value extension))))
    (funcall (symbol-function init-func))))

(defun initialize-extensions ()
  "Call the :on-initialize function on all extensions."
  (mapcar 'initialize--extension activated-extensions))

(defun activate-extensions (&rest extensions)
  "Mark given plugins to load on FG42"
  (setq activated-extensions extensions)
  (mapcar 'load--extension extensions))

(defun load-user-config (file)
  "Load the given path as user config file"
  (if (file-exists-p file)
      (load-file file)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;###autoload
(defun env (&rest args)
  "setup environment variables given as params."
  (require 'seq)
  (let ((pairs (seq-partition args 2)))
    (dolist (pair pairs)
      (progn (setenv (substring (symbol-name (car pair)) 1) (car (cdr pair)))))))


(provide 'fg42/base)
