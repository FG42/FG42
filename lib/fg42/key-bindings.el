;;; fg42-keybindings --- Keybinding helper macros for FG42
;;
;; Copyright (c) 2019  Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://gitlab.com/FG42/FG42
;; Keywords: keybinding
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
;;; Commentary:
;;; Code:

(require 'fg42/race)

(defun -defkey-god (map key fn)
  "Set the given KEY on key map MAP to FN."
  (define-key map (kbd key) fn))


(defun -defkey-human (map key fn)
  "Set the given KEY on key map MAP to FN."
  (define-key map (kbd key) fn))


(defun -defkey-evil (map state-keys fn)
  "Set the given STATE-KEYS on key map MAP to FN."
  (let (
        (normal-key (plist-get state-keys :normal))
        (visual-key (plist-get state-keys :visual))
        (insert-key (plist-get state-keys :insert))
        (emacs-key (plist-get state-keys :emacs)))
    (when (not (null normal-key)) (evil-define-key 'normal map (kbd normal-key) fn))
    (when (not (null visual-key)) (evil-define-key 'visual map (kbd visual-key) fn))
    (when (not (null insert-key)) (evil-define-key 'insert map (kbd insert-key) fn))
    (when (not (null emacs-key)) (evil-define-key 'emacs map (kbd emacs-key) fn))))




(defmacro defkey (map keys fn)
  "Defines a key binding for FG42 for different types.
Defines a keybinding in the given MAP for the given KEYS that maps
to the given FN with the given DOCSTRING.
Example usage : (defkey `global-map` (:evil (:normal \"SPC s u\") :god \"C-x C-y\") counsel-M-x)"

  (let ((god-key (plist-get keys :god))
        (human-key (plist-get keys :human))
        (evil-state-key  (plist-get keys :evil)))
    (cond
     ((is-god?) `(-defkey-god ,map ,god-key (quote ,fn)))
     ((is-human?) `(-defkey-human ,map ,human-key (quote ,fn)))
     ((is-evil?) `(-defkey-evil ,map (quote ,evil-state-key) (quote ,fn))))))

(provide 'fg42/key-bindings)
;;; key-bindings.el ends here
