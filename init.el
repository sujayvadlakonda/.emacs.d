;; Sujay Vadlakonda's Emacs Customization

(package-initialize)

(require 'package)
(push '("org" . "http://orgmode.org/elpa/") package-archives)
(push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)
(push '("melpa" . "http://melpa.org/packages/") package-archives)

(define-prefix-command 'package-management)
(define-key package-management "r" 'package-refresh-contents)
(define-key package-management "i" 'package-install)
(define-key package-management "d" 'package-delete)
(define-key package-management "h" 'describe-package)

(require 'use-package)

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :bind (:map evil-motion-state-map
         ("j" . left-char)
         ("k" . next-line)
         ("l" . evil-previous-visual-line)
         (";" . evil-forward-char)
         ("h" . evil-repeat-find-char)
         ("#" . evil-first-non-blank)
         ("q" . super-kill)
         :map evil-normal-state-map
         ("C-p" . package-management)
         ("q" . compile-based-on-mode)
         ("s" . super-save)
         ("<return>" . evil-insert-line-below)
         ("RET" . evil-insert-line-below)
         :map evil-insert-state-map
         ("C-g" . evil-normal-state)
         ("C-h" . backward-delete-char-untabify)
         ("C-o" . insert-minus-sign)
         :map evil-replace-state-map
         ("C-g" . evil-normal-state))
  :config
  (defun super-save ()
    "Removes tabs and trailing whitespace. Saves buffer."
    (interactive)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (save-buffer)
    (message "Super Saved!"))

  (defun super-kill ()
    (interactive)
    (kill-buffer-and-window)
    (message 'nil))

  (defun compile-based-on-mode ()
    (interactive)
    (cond ((eq major-mode 'emacs-lisp-mode) (eval-buffer))
          ((eq major-mode 'help-mode) (kill-buffer-and-window))
          ((eq major-mode 'apropos-mode) (kill-buffer-and-window))
          ((eq major-mode 'dired-mode) (kill-buffer-and-window))
          ((eq major-mode 'messages-buffer-mode) (kill-buffer-and-window))
          ((eq major-mode 'c++-mode) (compile "g++ Main.cpp"))))

  (defun insert-minus-sign () (interactive) (insert "-"))

  (evil-set-initial-state 'eshell-mode 'insert)

  (defun evil-insert-line-below (&optional count)
    (interactive "p")
    (evil-append-line 1)
    (dotimes (i count)
      (newline))
    (evil-normal-state)
    (message nil)))


;; Enable minibuffer vertical completion
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
         ("C-h" . ivy-backward-delete-char)
         ("C-;" . backward-delete-char)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         :map evil-normal-state-map
         ("/" . swiper))
  :config
  ;; Don't start M-x with ^
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode))

(use-package helpful
  :ensure t
  :init
  (setq counsel-describe-function-function 'helpful-callable)
  (setq counsel-describe-variable-function 'helpful-variable)
  (evil-set-initial-state 'helpful-mode 'motion)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind (:map company-active-map
         ;; Some insert state bindings
         ("C-g" . 'evil-normal-state)
         ("C-h" . 'backward-delete-char)
         ;; Arrow keys to scroll through completion options
         ("C-k" . 'company-select-next)
         ("C-l" . 'company-select-previous))
  :config
  (setq company-idle-delay 0.03)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers 1)
  (setq company-dabbrev-char-regexp "[A-z]")
  (setq company-require-match nil))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode))

;; JavaScript
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package rjsx-mode
  :ensure t
  :after js2-mode
  :mode "\\.js\\'"
  :bind (:map rjsx-mode-map
              ("C-/" . rjsx-comment-dwim)))

;; Dired
(require 'dired)
(defun auto-kill-dired-buffers ()
  "kills previous dired buffer after file selection unless opening a file"
  (interactive)
  (dired-find-file)
  (if (eq major-mode 'dired-mode)
      (progn
        (previous-buffer)
        (kill-buffer))))
(define-key dired-mode-map (kbd "<return>") 'auto-kill-dired-buffers)
(define-key dired-mode-map (kbd "t") 'dired-do-flagged-delete)
(define-key dired-mode-map (kbd "G") 'nil)
(define-key dired-mode-map (kbd "g") 'nil)


;; Colors
(set-background-color "#404040")
(set-frame-font "JetBrains Mono 20" nil t)
(set-face-foreground 'default "#EEEEEE")
(set-face-foreground 'font-lock-keyword-face "#C3C392")
(set-face-foreground 'font-lock-comment-delimiter-face "#888A86")
(set-face-foreground 'font-lock-comment-face "#888A86")
(set-face-foreground 'font-lock-function-name-face "#D5B790")
(set-face-foreground 'font-lock-string-face "#FFD1DC")
(set-face-foreground 'font-lock-preprocessor-face "#836952")
(set-face-background 'region "#89CFF0")


;; Backup settings ripped from Sacha Chua
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Emacs Defaults
(global-linum-mode)
(electric-pair-mode)
(setq echo-keystrokes 0.35)
(setq ring-bell-function 'ignore)
(setq initial-buffer-choice "~/.emacs.d/init.el")
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Mode Line Settings
(setq battery-mode-line-format " %b%p%% ")
(display-battery-mode t)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode t)
(setq-default mode-line-format '("%b " mode-name mode-line-misc-info))
