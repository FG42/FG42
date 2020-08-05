;; IDO configurations ---------------------------------------------
(ability ido ()
         (require 'ido)
         (require 'flx-ido)
         (require 'ido-vertical-mode)

         (ido-everywhere t)

         (require 'ido-completing-read+)
         (ido-ubiquitous-mode 1)

         (ido-mode t)

         (ability smex ()
           (smex-initialize)
           (global-set-key (kbd "M-x") 'smex))

         (flx-ido-mode 1)
         (setq ido-use-faces nil)
         (setq ido-use-filename-at-point nil)
         (setq ido-enable-flex-matching t)
         (ido-vertical-mode 1))

;; Ivy
(ability ivy ()
         "Completion using ivy."
         (require 'ivy)
         (require 'counsel)

         (ivy-mode 1)

         (setq ivy-use-virtual-buffers t)
         (setq enable-recursive-minibuffers t)
         (global-set-key (kbd "M-x") 'counsel-M-x)

         (global-set-key (kbd "<f1> f") 'counsel-describe-function)
         (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
         (global-set-key (kbd "<f1> l") 'counsel-find-library)
         (global-set-key (kbd "C-c k") 'counsel-ag)
         (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; Swiper ---------------------------------------------------
(ability swiper (ivy)
         "Replace default isearch with swiper"
         (global-set-key "\C-s" 'swiper)
         (global-set-key "\C-r" 'swiper))

;; Helm -----------------------------------------------------
(ability helm ()
         "Helm is an emacs incremental completion and selection narrowing framework"
         (require 'helm)
         (require 'helm-flx)
         (global-set-key (kbd "C-c h") 'helm-command-prefix)
         (global-set-key (kbd "M-x") 'helm-M-x)
         (global-set-key (kbd "C-x C-f") 'helm-find-files)
         (global-unset-key (kbd "C-x c"))

         (define-key helm-map (kbd "<tab>")
           'helm-execute-persistent-action)

         (define-key helm-map (kbd "C-i")
           'helm-execute-persistent-action)

         (define-key helm-map (kbd "C-z")
           'helm-select-action)


         (when (executable-find "curl")
           (setq helm-google-suggest-use-curl-p t))

         (setq helm-split-window-in-side-p t
               helm-move-to-line-cycle-in-source t
               helm-ff-search-library-in-sexp t
               helm-scroll-amount 8
               helm-ff-file-name-history-use-recentf t)

         (setq helm-flx-for-helm-find-files t
               helm-flx-for-helm-locate     t)

         (helm-flx-mode +1)
         (helm-mode 1))


(ability icomplete ()
         (require 'icomplete)
         (require 'icomplete-vertical)
         (global-set-key (kbd "C-n") 'icomplete-forward-completions)
         (global-set-key (kbd "C-p") 'icomplete-backward-completions)
         (global-set-key (kbd "C-f") 'icomplete-forward-completions)
         (global-set-key (kbd "C-b") 'icomplete-backward-completions)
         (global-set-key (kbd "<right>") 'icomplete-forward-completions)
         (global-set-key (kbd "<left>") 'icomplete-backward-completions)
         (global-set-key (kbd "<down>") 'icomplete-forward-completions)
         (global-set-key (kbd "<up>") 'icomplete-backward-completions)
         (global-set-key (kbd "<RET>") 'icomplete-force-complete-and-exit)
         (global-set-key (kbd "<tab>") 'icomplete-force-complete)
         (define-key 'icomplete-minibuffer-map (kbd "C-t") 'icomplete-vertical-toggle)
         (setq icomplete-max-delay-chars 2
               icomplete-separator " | "
               icomplete-show-matches-on-no-input t
               icomplete-hide-common-prefix nil
               completion-ignore-case t)
         (when (> emacs-major-version 26)
           (fido-mode -1))
         (icomplete-mode 1)
         (icomplete-vertical-mode 1))


(provide 'extensions/editor/selection-candidates)
