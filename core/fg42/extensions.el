;;; extensions --- Extension library of FG42  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2020  Sameer Rahmani <lxsameer@gnu.org>
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
;;; Code:

;; This library provides some basic means to create a new FG42 extensions
(require 'fg42/utils)
(require 'fg42/extensions/core)


(defun fg42-extensions/load-index (_system ext-name ext-path)
  "Load the extension EXT-NAME which is in EXT-PATH using SYSTEM.
It will load the main file of the extension and return the `fg42-extension'
instance of the extension and nil otherwise."
  (let ((is-loaded? (require ext-name ext-path t)))
    (when is-loaded?
      (symbol-value ext-name))))


(defun fg42-extensions/load-extension (system ext)
  "Setup the given extension EXT against the given SYSTEM.
At this stage we will install/load the main file of the extensions
and call the `on-initialize'function of extensions in order to setup
the autoloads and hooks."
  (cond
   ((symbolp ext)
    (fg42-extensions/load-index system ext (fg42-extension/path system ext)))

   ((listp ext)
    (fg42-extensions/load-index system (car ext) (cadr ext)))
   (t
    ;; TODO: instead of throwing and error, inject the error into the system
    (throw 'load-extension-failed
           (format "Can't load extension %s" (str ext))))))


(defun fg42-extensions/load-system-extensions (system)
  "Load the extensions defined in the given SYSTEM.

SYSTEM should be an instance of `fg42-system' which contains a list
of extension names on the `extensions' field.  This function finds and
loads the index file of those extensions and returns a new system
containing the `fg42-extension' instances."
  (let ((exts (mapcar (lambda (ext)
                        (fg42-extensions/load-extension system ext))
                      (fg42-system-extensions system))))
    (setf (fg42-system-extensions system)
          exts))
  system)


(defun fg42-extensions/initialize (system ext)
  "Initialize the given extension EXT aginst the given SYSTEM."
  (funcall (fg42-extension-on-initialize ext) system))


(defun fg42-extensions/initialize-extensions (system)
  "Initialize the extensions within SYSTEM and return a new system."
  (mapcar
   (lambda (ext) (fg42-extensions/initialize system ext))
   (fg42-system-extensions system))
  system)


(defun fg42-setup-extensions (system)
  "Setup the preloads for the given SYSTEM."
  (funcall (comp #'fg42-extensions/initialize-extensions
                 #'fg42-extensions/load-system-extensions) system))


(provide 'fg42/extensions)
;;; extensions.el ends here
