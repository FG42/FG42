;;; FG42 --- a simple package manager for FG42                     -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018  lxsameer

;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; Keywords: lisp fg42 IDE package manager
;; Version: 2.31

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Editor/IDE bundle base on Lovely Gnu/Emacs to make your life easier

;;; Code:
(defvar fg42-home (getenv "FG42_HOME")
  "The pass to fg42-home.")

(defvar fg42-tmp (concat fg42-home "/tmp"))

(defvar fg42-before-initialize-hook nil
  "This hook will be called before FG42 initilization process.")

(defvar fg42-after-initialize-hook nil
  "This hook will be called after FG42 initilization process.")

(defvar fg42-gc-cons-threshold  16777216 "Value of GC threshold of FG42.")




(defun defer-garbage-collection ()
  "Disable garbage collection."
  (setq gc-cons-threshold fg42-gc-cons-threshold))

(defun restore-garbage-collection ()
  "Restore garbage collection to it's default value."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold most-positive-fixnum))))

(defun fg42--startup-optimization ()
  "Optimize FG42 startup."
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
        gc-cons-percentage 0.6) ;; by setting gc threshold to the largest number Emacs can understand we are basically disabling it :).

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold fg42-gc-cons-threshold ; 16mb
                    gc-cons-percentage 0.1))) ;; after initilization phase restore cons threshold to normal
  ;; disable auto initilization of package.el
  (setq package-enable-at-startup nil)
  ;; disable gc when we are in minibuffer using the same method we use for initilization time
  (add-hook 'minibuffer-setup-hook #'defer-garbage-collection)
  ;; just enable gc when exiting minibuffer
  (add-hook 'minibuffer-exit-hook #'restore-garbage-collection)
  ;; we dont need Emacs to check every file type and look for file handlers when we are initializing so we backup the original value and set it to nil
  (setq --file-name-handler-alist file-name-handler-alist)

  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist --file-name-handler-alist))) ;; after initialization we can restore that file-name-handler-alist to original value.

  (setq initial-major-mode 'fundamental-mode) ;; initial mode for emacs can be fundamental mode we have nothing to lose
  )

(require 'fpkg)
(require 'fg42/base)
(require 'fg42/splash)
(require 'fg42/race)
(require 'fg42/utils)

(defun fg42-initialize ()
  "Initialize FG42 editor."
  (setq fg42-start-timestamp (float-time))
  (fg42--startup-optimization)
  (run-hooks 'fg42-before-initialize-hook)
  (mkdir fg42-tmp t)
  (setq package-user-dir (concat fg42-home "/packages"))
  (fpkg-initialize)
  (initialize-extensions)
  (run-hooks 'fg42-after-initialize-hook)
  (message "startup time: %s" (- (float-time) fg42-start-timestamp)))

(provide 'fg42)
;; fg42.el ends here
