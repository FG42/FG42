;; Vars -------------------------------
(defvar about_fg42_msg "
FG42 %%VERSION%% Copyright © 2010-2015 Sameer Rahmani <lxsameer@gnu.org>
FG42 release under the term of GPLv2.

Home page:
\thttp://fg42.lxsameer.com

Credits:
\tSameer Rahmani (lxsameer)
\tNima Nazari (niman)
\tDanial Parsi (intuxticated)
\tKeyvan Hedayati (k1-hedayati)
"
  "About FG42")


;; Functions ---------------------------
(defun about/get_string ()
  "Get the about message string"
  (let (msg)
    (setq msg (replace-regexp-in-string "%%VERSION%%"
					FG42-VERSION about_fg42_msg))))

(defun about-fg42-f ()
  "Show an small about note"
  (interactive)
  (let (buf msg)
    (setq buf (get-buffer-create "*About FG42*"))
    (setq msg (about/get_string))
    (set-buffer buf)
    (insert msg)
    (view-buffer buf)))

(define-key-after global-map
                  [menu-bar help-menu about-fg42]
		  '("About FG42" . about-fg42-f)
		  'about-emacs)

(provide 'extensions/editor/about)
