;; global variables
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

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package groovy-mode
  :mode (("build\\.gradle" . groovy-mode)
         ("Jenkinsfile" . groovy-mode)
         ("Jenkinsfile.test" . groovy-mode)))

(ac-config-default)

