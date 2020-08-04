;; ---------------------------------------------------------------------
;; Variables
;; ---------------------------------------------------------------------
(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.tmp/")
  "*Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")


;; ---------------------------------------------------------------------
;; Functions
;; ---------------------------------------------------------------------

(defun my-desktop-save (&optional name)
  "Save desktop with a name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session as: ")))
  (make-directory (concat my-desktop-session-dir name) t)
  (desktop-save (concat my-desktop-session-dir name) t))

(defun my-desktop-read (&optional name)
  "Read desktop with a name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session: ")))
  (desktop-read (concat my-desktop-session-dir name)))

(defun my-desktop-get-session-name (prompt)
  (completing-read prompt (and (file-exists-p my-desktop-session-dir)
                               (directory-files my-desktop-session-dir))
                   nil nil nil my-desktop-session-name-hist))

(define-key-after global-map
  [menu-bar file load-session] '("Load Session" . my-desktop-read)
  'dired)

(define-key-after
  global-map [menu-bar file save-session]
  '("Save Session" . my-desktop-save) 'load-session)

(define-key global-map (kbd "<f11>") 'my-desktop-read)
(define-key global-map (kbd "<f12>") 'my-desktop-save)

(provide 'extensions/editor/session-management)
