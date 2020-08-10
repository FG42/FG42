;;; Base --- Base library of FG42
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'fg42/extension)
(require 'fg42/ability)
(require 'fg42/vars)


;; Macros ---------------------------------
(defmacro theme (name &optional local)
  "Mark the given theme NAME as default them.
LOCAL should be 't' if theme is on FG42 it self"
  `(progn
     (setq default-theme ',(intern (symbol-name name)))
     (when (not (null ,local))
       (depends-on default-theme))))


;; Functions ------------------------------
(defun load-default-theme ()
  "Load the given theme name."
  (message "Load theme %s" default-theme)
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
  "Load a single EXTENSION and call its :on-initialize function."
  (let ((lib (concat "extensions/" (symbol-name extension))))
    (require (intern lib))))


(defun initialize--extension (extension)
  "Initialize given EXTENSION by calling its :on-initialize function."
  (let ((init-func (fg42-extension-on-initialize (symbol-value extension))))
    (funcall (symbol-function init-func))))


(defun initialize-extensions ()
  "Call the :on-initialize function on all extensions."
  (mapcar 'initialize--extension activated-extensions))


(defun activate-extensions (&rest extensions)
  "Mark given EXTENSIONS to load on FG42."
  (setq activated-extensions extensions)
  (mapcar 'load--extension extensions))


(defun load-user-config (file)
  "Load the given FILE as user config file."
  (if (file-exists-p file)
      (load-file file)))


(provide 'fg42/base)
;;; base ends here
