;;; fpkg --- a simple package manager for FG42                     -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2010-2020  Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; Keywords: lisp fg42 IDE package manager
;; Version: 1.0.0
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
;;
;; Simple package manager for FG42
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'fg42/extension)
(require 'fg42/utils)
(require 'fpkg/installers)

;; Variables ---------------------------------
(cl-defstruct fpkg-dependency
  "Package structure for FG42."
  name
  (version "0")
  (path nil)
  (source 'elpa))


(defvar bootstrap-version nil
  "Bootstrap version of straight.  This var is used in straight's installer.")


(defvar fpkg-packages-path
  (expand-file-name ".fpkg/" fg42-home)
  "The path to the directory which FPKG will use to store that packages.")

(defvar fpkg-initilized-p nil
  "A boolean flag that indicates whether FPKG is initialized or not.")

(defvar required-packages (make-hash-table)
  "A hash of `fg42-package structure representing required packages.")


;; Functions ----------------------------------
(defun fpkg-initialize ()
  "Initilize the straight.e package manager and setup necessary hooks."
  (let ((bootstrap-file (concat fpkg-packages-path
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 5))

    (make-directory fpkg-packages-path t)
    (setq straight-base-dir fpkg-packages-path)
    (setq straight-use-package-by-default t)
    (if (not (file-exists-p bootstrap-file))
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp))
      (load bootstrap-file nil 'nomessage))))


(defun fpkg-initialize-once ()
  "Initilize FPKG only once."
  (when (not fpkg-initilized-p)
    (fpkg-initialize)
    (straight-use-package 'use-package)))


(defun official-extension-p (args)
  "Predicate to say if ARGS is an official FG42 extension."
  (member args fg42/extensions))


(defun get-receipe (name)
  "Get the receipe for given NAME if that is an official extension."
  (list name :host 'gitlab :repo (format "FG42/%s" name)))


(defmacro fg42-install-extension (args)
  "Install if given ARGS is an official extension."
  ;; TODO: Straight supports reciepe registery it might worth
  ;;       using it.
  (let ((reciepe (get-receipe args)))
    `(use-package ,args :straight ,reciepe)))


(defmacro depends-on (pkgname &rest details)
  "Install the given PKGNAME with the optional DETAILS."
  (declare (indent 1))
  (if (official-extension-p pkgname)
      `(fg42-install-extension ,(eval pkgname))
    `(use-package ,(eval pkgname) ,@details)))

(comment
  ;; depends on now is a wrapper around use-package
  (macroexpand-1 '(depends-on 'exwm))
  (macroexpand-1 '(depends-on 'go-mode :mode "\\.go\\'"))
  (macroexpand-1 '(depends-on 'devops-extension))
  (macroexpand-1 '(fg42-install-extension devops-extension))
  ;; compatible with old calls
  (macroexpand-1 '(depends-on 'cyberpunk-theme))
  ;; official extension
  (depends-on 'devops-extension)
  ;; 3rd party extension
  (depends-on 'go-extension :straight (go-extension :host gitlab :repo "amirrezaask/go-extension")))


(provide 'fpkg)
;;; fpkg.el ends here
