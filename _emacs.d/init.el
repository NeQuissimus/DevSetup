;; global variables

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 show-trailing-whitespace t
 sentence-end-double-space nil)

(setq whitespace-style '(face trailing tabs))
(custom-set-faces
 '(whitespace-tab ((t (:background "red")))))
(global-whitespace-mode)

;; Load files from disk when changed
(global-auto-revert-mode t)

;; Remove white-spaces when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2
 buffer-file-coding-system 'utf-8-unix)

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x f") 'projectile-find-file)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Allocate more memory
(setq gc-cons-threshold 20000000)

(ac-config-default)

(require 'smartparens-config)
(smartparens-global-mode)

(projectile-global-mode)
