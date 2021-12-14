(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(show-paren-mode 1)

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq x-select-enable-clipboard t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode f)
(setq backward-delete-char-untabify-method 'nil)

; utf8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; start up maximized
(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
;(fullscreen)

(global-prettify-symbols-mode t)

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

; install some cool themes
(use-package spacemacs-theme :defer t)
(use-package doom-themes :defer t)
(use-package modus-themes :defer t)

;; load theme 
(load-theme 'doom-gruvbox t)

; hide irrelevant modes
(use-package diminish
  :ensure t)

; config file hotreloading
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtine"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(use-package dashboard
  :ensure t
  :defer nil
  :preface
  (defun update-config ()
    "Update Witchmacs to the latest version."
    (interactive)
    (let ((dir (expand-file-name user-emacs-directory)))
      (if (file-exists-p dir)
          (progn
            (message "Witchmacs is updating!")
            (cd dir)
            (shell-command "git pull")
            (message "Update finished. Switch to the messages buffer to see changes and then restart Emacs"))
        (message "\"%s\" doesn't exist." dir))))

  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-banner-logo-title "S C A R L E T - W I T C H M A C S")
  (setq dashboard-startup-banner "~/.emacs.d/flanvector.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,nil
            "Witchmacs on github"
            "Open Witchmacs' github page on your browser"
            (lambda (&rest _) (browse-url "https://github.com/feydor/witchmacs"))
            'default)
           (nil
            "Witchmacs crash course"
            "Open Witchmacs' introduction to Emacs"
            (lambda (&rest _) (find-file "~/.emacs.d/Witcheat.org"))
            'default)
           ;(nil
           ; "Update Witchmacs"
           ; "Get the latest Witchmacs update. Check out the github commits for changes!"
           ; (lambda (&rest _) (update-config))
           ; 'default)
           )
          ;; line 2
          ((,nil
            "Open scratch buffer"
            "Switch to the scratch buffer"
            (lambda (&rest _) (create-scratch-buffer))
            'default)
           (nil
            "Open config.el"
            "Open Witchmacs' configuration file for easy editing"
            (lambda (&rest _) (find-file "~/.emacs.d/config.el"))
            'default)))))
            
;; creating a new window switches your cursor to it
(defun split-and-follow-horizontally ()
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

 (defun split-and-follow-vertically ()
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))
 (global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
 
;; enable resize bindings
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; highlight current line
(global-hl-line-mode t)
 
;; transform yes or no questions to yn 
(defalias 'yes-or-no-p 'y-or-n-p)

;; defer packages for quicker start up
(setq use-package-always-defer t)

;; autoupdate automatically updates and removes old packages
(use-package auto-package-update
  :defer nil
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

; init powerline
 (use-package powerline
	:ensure t
	:init
	(powerline-default-theme)
	:hook
	('after-init-hook) . 'powerline-reset)

;; init which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

;; C-s search enhancer
 (use-package swiper
	:ensure t
	:bind ("C-s" . 'swiper))

;; vi bindings
(use-package evil
  :ensure t
  :defer nil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
  
; fix dired in evil mode
(evil-set-initial-state 'dired-mode 'emacs)

; fileswitcher
(use-package ido
  :init
  (ido-mode 1)
  :config
  (setq ido-enable-flex-matching nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-everywhere t))

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
; This enables arrow keys to select while in ido mode. If you want to
; instead use the default Emacs keybindings, change it to
; "'C-n-and-C-p-only"
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package magit
  :ensure t)

;; autocompletion
(use-package company
  :ensure t
  :diminish (meghanada-mode company-mode irony-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort)
  :hook
  ((java-mode c-mode c++-mode) . company-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook
  ((c-mode c++-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

;; c/c++ backend for company
(use-package company-c-headers
  :defer nil
  :ensure t)

(use-package company-irony
  :defer nil
  :ensure t
  :config
  (setq company-backends '((company-c-headers
                            company-dabbrev-code
                            company-irony))))
(use-package irony
  :defer nil
  :ensure t
  :config
  :hook
  ((c++-mode c-mode) . irony-mode)
  ('irony-mode-hook) . 'irony-cdb-autosetup-compile-options)

;; c/c++ indentations
(defun my-c-mode-common-hook ()
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
 (c-set-offset 'substatement-open 0)
 ;; other customizations can go here

 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)                  ;; Default is 2
 (setq c-indent-level 4)                  ;; Default is 2

 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 4)
 (setq indent-tabs-mode f)  ; use spaces only if nil
 )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)



(require 'dired-x)
(defalias 'sh 'eshell)
(defalias 'eb 'eval-buffer)

