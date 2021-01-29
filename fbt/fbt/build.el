;;; FGBuildTool --- The build tool for FG42
;;
;; Copyright (c) 2010-2021  Sameer Rahmani <lxsameer@gnu.org>
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
;;
;;; Commentary:
;;; Code:

(require 'fbt/utils)
(require 'fbt/compile)


(defun fbt-build/-clean (dir)
  "Clean up all the elc files from the given DIR."
  (let ((elcs (elc-files-in dir)))
    (when elcs
      (message
       (shell-command-to-string
        (format "rm -v %s" (apply #'concat
                                  (mapcar (lambda (x) (format " %s" x)) elcs))))))))


(defun fbt-build/clean (dirs)
  "Clean the given DIRS from elc files."
  (mapc #'fbt-build/-clean dirs))


(defun fbt-build/build (&rest params)
  "Compile the core and install the dependencies with the given PARAMS."
  (fbt-compile/compile "core"))



(provide 'fbt/build)
;;; build.el ends here
