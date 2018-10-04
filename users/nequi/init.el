(package-initialize)

(require 'smartparens-config)

(beacon-mode 1)
(electric-indent-mode 0)
(global-auto-revert-mode t)
(global-flycheck-mode)
(global-whitespace-mode)
(load-theme 'zerodark t)
(projectile-global-mode)
(scroll-bar-mode -1)
(set-default-coding-systems 'utf-8)
(set-frame-font "Hasklig")
(set-language-environment "UTF-8")
(smartparens-global-mode)
(zerodark-setup-modeline-format)

(ac-config-default)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x f") 'projectile-find-file)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package magit
  :commands magit-status
  :init (setq
         git-commit-style-convention-checks nil)
  :bind (("C-x g" . magit-status))
)

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
 inhibit-startup-screen t
 make-backup-files nil
 neo-autorefresh t
 neo-force-change-root t
 neo-theme 'nerd
 scroll-error-top-bottom t
 sentence-end-double-space nil
 show-paren-delay 0.5
 show-trailing-whitespace t
 use-package-always-ensure t
 vc-handled-backends nil
 whitespace-style '(face trailing tabs)
)

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

(custom-set-variables
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "\\(\\(?:[Ii]ssue \\|[Ff]ixe[ds] \\|[Rr]esolve[ds]? \\|[Cc]lose[ds]? \\|[Pp]\\(?:ull [Rr]equest\\|[Rr]\\) \\|(\\)#\\([0-9]+\\))?\\)")))))

(add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup)
)
