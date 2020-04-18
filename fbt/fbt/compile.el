;;; FGBuildTool --- The build tool for FG42
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
;;
;;; Commentary:
;;; Code:

(require 'fbt/utils)


(defun fbt-compile/compile (dir)
  "Compile all the elisp files in the given DIR regardless of timestamp.
The DIR should be relative to FG42_HOME."
  (let ((target (->path dir)))
    (message "Compiling '%s'..." target)
    (add-to-list 'load-path target)

    (message "Load path:\n%s\n" (apply #'concat (mapcar (lambda (x) (format "%s\n" x)) load-path)))
    (byte-recompile-directory target 0 t)))


(provide 'fbt/compile)
;;; compile.el ends here
