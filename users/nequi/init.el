(scroll-bar-mode -1)
(package-initialize)

(load-theme 'zerodark t)
(zerodark-setup-modeline-format)

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

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

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

;; org-jira
(setq jiralib-url "https://jira.esentire.com")

;; neotree
(setq neo-theme 'nerd)
(setq neo-force-change-root t)
(setq neo-autorefresh t)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x f") 'projectile-find-file)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x g") 'magit-status)

;; Allocate more memory
(setq gc-cons-threshold 20000000)

(require 'smartparens-config)
(smartparens-global-mode)

(projectile-global-mode)

;; Gitter + Irc
(use-package erc
  :commands erc erc-tls
  :init
  (setq
   erc-prompt-for-password t ;; prefer ~/.authinfo for passwords
   erc-hide-list '("JOIN" "PART" "QUIT")
   erc-autojoin-channels-alist
   '(("irc.gitter.im" "#scalaz/scalaz"))))

(defun gitter()
  "Connect to Gitter."
  (interactive)
  (erc-tls :server "irc.gitter.im" :port 6697))

(set-default-font "Hasklig")

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

(add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)

(add-hook 'prog-mode-hook 'hl-todo-mode)

(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup)
)

;; http://www.accidentalrebel.com/posts/minifying-buffer-contents-in-emacs.html
(defun minify-buffer()
  "Minifies the buffer contents by removing whitespaces."
  (interactive)
  (delete-whitespace-rectangle (point-min) (point-max))
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

(ac-config-default)
