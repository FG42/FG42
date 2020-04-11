;;; project-manager --- Extends the support for project base functionalities
;;
;; Copyright (C) 2010-2020 Sameer Rahmani <lxsameer@gnu.org>
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; Created: 20 Jan 2020
;; Keywords: project management
;; Homepage: https://fg42.org
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
;;; Change Log:

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'projectile)

;; Vars ------------------------
(defvar project-manager-default-project-file
  ".fg42.project.el"
  "The name of default project file in projects.")

(defvar project-manager-project-name-suffix
  "-project"
  "The string suffix to be attached to the project names.")

;; Structures -----------------------------
(cl-defstruct fg42-project
  "Define a FG42 project properties.
Each fg42 project file should define an instance of this data structure
and bind a name to it that follows the
`<projectile-project-name>-<project-manager-project-name-suffix>'
 format."
  (docs nil)
  (api-version "0.1.0")
  (dependencies '())
  (external-dependencies '())

  ;; Describes
  (on-initialize nil)
  (on-load)
  ;; An associated array of major modes to their
  ;; debugger function
  (print-debugger nil))

;; Functions -------------------
(defun project-manager-load-file (file-name)
  "Find and load the given FILE-NAME relative to the project root dir."
  (let ((root (projectile-project-root)))
    (load-file (concat (file-name-as-directory root) file-name))))


(defmacro defproject (&optional args)
  "Define the different aspects of a project by using ARGS.

Name of the project extracts from projectile.  For more information
about the different properties of the project take a look at
`fg42-project' structure."
  `(setq ,(concat (projectile-project-name)
                  (fg42-project-name-suffix))
         (apply 'make-fg42-project (quote ,args))))


(provide 'project-manager)
;;; project-manager.el ends here
