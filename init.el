;; We up the gc threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Increase the amount of data which Emacs reads from the process. 
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq comp-deferred-compilation t)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
;;

(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Makes :straight t by default
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)
(setq straight-fix-flycheck t)

;;;;

(require 'package)

;; GCMH - the Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :custom
  (gcmh-idle-delay 1000)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ;; 16 MB
  :hook (after-init . gcmh-mode))

(setq-default shell-file-name "/bin/sh")

;; Add local packages
(add-to-list 'load-path "~/.emacs.d/local")

;; Loads config
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

