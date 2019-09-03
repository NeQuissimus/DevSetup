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

(set-mouse-color "#F06010")

(ac-config-default)
(editorconfig-mode 1)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x f") 'projectile-find-file)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") 'kubernetes-overview)
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
 column-number-mode t
 create-lockfiles nil
 gc-cons-threshold 20000000
 history-length 100
 initial-scratch-message nil
 inhibit-startup-echo-area-message t
 inhibit-startup-message t
 inhibit-startup-screen t
 make-backup-files nil
 neo-autorefresh t
 neo-force-change-root t
 neo-theme 'nerd
 neo-window-width 30
 scroll-error-top-bottom t
 sentence-end-double-space nil
 show-paren-delay 0.5
 show-trailing-whitespace t
 use-package-always-ensure t
 vc-handled-backends nil
 whitespace-style '(face trailing tabs)
)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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

(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

;; http://www.accidentalrebel.com/posts/minifying-buffer-contents-in-emacs.html
(defun minify-buffer()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
                (lambda (frame)
                  (let ((neo-window (neo-global--get-window)))
                    (unless (null neo-window)
                      (setq neo-window-width (window-width neo-window)))))))

(add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook #'nyan-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup)
)

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
