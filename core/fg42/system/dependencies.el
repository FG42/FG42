;;; dependencies --- System library of FG42 -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2021 Sameer Rahmani & Contributors
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
(require 'fpkg)
(require 'fg42/system/core)


(defun fg42/system-merge-dependencies (system cube-name deps)
  "Retun an updated SYSTEM with the given dependencies DEPS for CUBE-NAME."
  (if deps
      ;; TODO: Validate the deps here
      (fg42/system-cons-to system :dependencies (cons cube-name deps))
    system))


(defun fg42/system-dependencies (system)
  "Return a flat list of dependencies of the SYSTEM."
  (seq-reduce
   (lambda (lst pair)
     (append lst (cdr pair)))
   (fg42/system-get system :dependencies)
   '()))

(defun fg42/system-install-dependency (dep)
  "Install the given dependency list DEP.
dep is in (cube-name (...dependencies...)) format."
  (message "Installing dependencies of '%s' cube..." (car dep))
  (mapcar #'fpkg/install-package (cdr dep)))


(defun fg42/system-install-dependencies (system)
  "Install the dependencies in the SYSTEM."
  (when (not (fg42/system-dependencies-installed? system))
    (fg42/system-refresh-package-index system)
    (mapc #'fg42/system-install-dependency
          (fg42/system-get system :dependencies)))
  system)


(defun fg42/system-refresh-package-index (system)
  "Refresh the package index of the SYSTEM is dependencies were missing."
  (unless (fg42/system-dependencies-installed? system)
    ;; check for new packages (package versions)
    (message "Refreshing package database...")
    ;; TODO: call different function to update every source
    ;;       in the system. Something similar to what we do
    ;;       with package-install on FPKG
    (fpkg/initialize system)
    (package-refresh-contents)))


(defun fg42/system-dependencies-installed? (system)
  "Return t if all the dependencies of the given SYSTEM are installed."
  (let ((pkgs (fg42/system-dependencies system)))
    (seq-reduce
     (lambda (all-installed? pkg)
       (and all-installed?
            (fpkg/package-installed? pkg)))
     pkgs
     t)))


(provide 'fg42/system/dependencies)
;;; dependencies.el ends here
