(package-initialize)

(require 'smartparens-config)
(require 'yasnippet)

(beacon-mode 1)
(indent-guide-global-mode)
(global-auto-revert-mode t)
(global-flycheck-mode)
(yas-global-mode 1)
(global-whitespace-mode)
(load-theme 'spacemacs-dark t)
(projectile-global-mode)
(scroll-bar-mode -1)
(set-default-coding-systems 'utf-8)
(set-frame-font "Hasklig")
(set-language-environment "UTF-8")
(smartparens-global-mode)
(global-prettify-symbols-mode +1)

(set-mouse-color "#F06010")

(ac-config-default)
(editorconfig-mode 1)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x f") 'projectile-find-file)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(use-package magit
  :commands magit-status
  :init (setq
         git-commit-style-convention-checks nil
         magit-log-section-commit-count 20
         )
  :bind (("C-x g" . magit-status))
)

(with-eval-after-load 'magit
  (require 'forge))

(custom-set-variables
 '(magit-cherry-pick-arguments (quote ("-x"))))

(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2
 buffer-file-coding-system 'utf-8-unix
)

(setq
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 column-number-mode t
 create-lockfiles nil
 gc-cons-threshold 20000000
 gnutls-verify-error t
 history-length 100
 initial-scratch-message nil
 inhibit-startup-echo-area-message t
 inhibit-startup-message t
 inhibit-startup-screen t
 make-backup-files nil
 ring-bell-function 'ignore
 scroll-error-top-bottom t
 sentence-end-double-space nil
 show-paren-delay 0.5
 show-trailing-whitespace t
 tls-checktrust t
 use-package-always-ensure t
 vc-handled-backends nil
 whitespace-style '(face trailing tabs)
 x-stretch-cursor t
)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq-default mode-line-format
              '("%e" ; print error message about full memory.
                mode-line-front-space
                mode-line-buffer-identification
                "   "
                mode-line-position
                "   "
                mode-line-end-spaces))

(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

(prefer-coding-system 'utf-8)

(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(custom-set-faces
 '(whitespace-tab ((t (:background "red")))))

(defun my-correct-symbol-bounds (pretty-alist)
  (mapcar (lambda (el)
        (setcdr el (string ?\t (cdr el)))
          el)
      pretty-alist))

(defun my-ligature-list (ligatures codepoint-start)
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))

(setq my-hasklig-ligatures
  (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
             "==" "===" "==>" "=>" "=<<" "!!" ">>"
             ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
             "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
             "<<" "<<<" "<+>" ".." "..." "++" "+++"
             "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
(my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; nice glyphs for haskell with hasklig
(defun my-set-hasklig-ligatures ()
  (setq prettify-symbols-alist
    (append my-hasklig-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))

;; http://www.accidentalrebel.com/posts/minifying-buffer-contents-in-emacs.html
(defun minify-buffer()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

(add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook #'nyan-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook (scala-mode . lsp)
  :custom
  (scala-indent:align-forms t)
  (scala-indent:align-parameters t)
  (scala-indent:indent-value-expression t)
  (scala-indent:default-run-on-strategy)
  (scala-indent:operator-strategy)
  :config
  (setq lsp-metals-server-command "~/bin/metals-emacs"))

(use-package sbt-mode
  :after scala-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package company-lsp)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

(use-package forge
  :config
  (add-to-list 'forge-alist '("github.esentire.com" "github.esentire.com/api"
                              "github.esentire.com" forge-github-repository)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
