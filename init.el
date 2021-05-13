(global-linum-mode)
(dolist (mode '(eshell-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (linum-mode -1))))

(defun linum-mode-off ()
  "Turn linum mode off"
  (interactive)
  (linum-mode -1))

(electric-pair-mode)
(setq echo-keystrokes 0.35)
(setq ring-bell-function 'ignore)
(setq initial-buffer-choice "~/.emacs.d/init.el")
;(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup settings ripped from Sacha Chua
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Colors
(set-background-color "#404040")
(set-frame-font "JetBrains Mono 12" nil t)
(set-face-foreground 'default "#EEEEEE")
(set-face-foreground 'font-lock-keyword-face "#C3C392")
(set-face-foreground 'font-lock-comment-delimiter-face "#888A86")
(set-face-foreground 'font-lock-comment-face "#888A86")
(set-face-foreground 'font-lock-function-name-face "#D5B790")
(set-face-foreground 'font-lock-string-face "#FFD1DC")
(set-face-foreground 'font-lock-preprocessor-face "#836952")
(set-face-background 'region "#89CFF0")

;; Mode Line Settings
;; Display Battery
(setq battery-mode-line-format " %b%p%% ")
(display-battery-mode t)

;; Display Time
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode t)

;; Apply Mode Line Settings to all buffers
(setq-default mode-line-format '("%b " mode-name mode-line-misc-info))

;; Global Keybindings
;; Kill buffers
(global-set-key (kbd "C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "<escape>") 'abort-recursive-edit); Map Escape to C-g in minibuffer
;; Alternate keys to change active buffer
(global-set-key (kbd "C-j") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)

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

(define-prefix-command 'package-management)
(define-key package-management "r" 'package-refresh-contents)
(define-key package-management "i" 'package-install)
(define-key package-management "d" 'package-delete)
(define-key package-management "h" 'describe-package)

(require 'package)
(push '("org" . "http://orgmode.org/elpa/") package-archives)
(push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)
(push '("melpa" . "http://melpa.org/packages/") package-archives)

(require 'use-package)

(defun next-visual-line ()
  (interactive)
  (next-line 1 t))

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
(use-package evil-numbers)
;; Completion
(use-package company
  :init (global-company-mode)
  :bind (:map company-active-map
         ;; Completion based on number
         ("C-c 1" . (lambda () (interactive) (company-complete-number 1)))
         ("C-c 2" . (lambda () (interactive) (company-complete-number 2)))
         ("C-c 3" . (lambda () (interactive) (company-complete-number 3)))
         ("C-c 4" . (lambda () (interactive) (company-complete-number 4)))
         ("C-c 5" . (lambda () (interactive) (company-complete-number 5)))
         ("C-c 6" . (lambda () (interactive) (company-complete-number 6)))
         ("C-c 7" . (lambda () (interactive) (company-complete-number 7)))
         ("C-c 8" . (lambda () (interactive) (company-complete-number 8)))
         ("C-c 9" . (lambda () (interactive) (company-complete-number 9)))
         ("C-c 0" . (lambda () (interactive) (company-complete-number 10)))
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

;; Enable minibuffer vertical completion
(use-package ivy
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
         ("C-h" . ivy-backward-delete-char)
         ("C-;" . backward-delete-char)))

;; org mode
(use-package org
  :hook (org-mode . linum-mode-off)
  :bind (:map evil-motion-state-map
         ("C-i" . nil)
         :map org-mode-map
         ("C-k" . kill-this-buffer)
         ("C-h" . nil)
         ("C-j" . nil)
         ("C-i" . org-cycle))
  :config
  (set-face-foreground 'org-level-3 "#A0F7F3"))

;; dired mode
(require 'dired)
(defun auto-kill-dired-buffers ()
  "kills previous dired buffer after file selection unless opening a file"
  (interactive)
  (dired-find-file)
  (if (eq major-mode 'dired-mode)
      (progn
        (previous-buffer)
        (kill-buffer))))

;; keybindings
(define-key dired-mode-map (kbd "<return>") 'auto-kill-dired-buffers)
(define-key dired-mode-map (kbd "t") 'dired-do-flagged-delete)
(define-key dired-mode-map (kbd "G") 'nil)
(define-key dired-mode-map (kbd "g") 'nil)

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; JavaScript
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package rjsx-mode
  :ensure t
  :after js2-mode
  :mode "\\.js\\'")

(defun js2-add-semi-colons ()
  "Add missing semi-colons."
  (interactive)
  (--each (--select (string-equal (caar it) "msg.missing.semi") (js2-warnings))
    (let ((pos (nth 1 it))
          (length (nth 2 it)))
      (goto-char (+ pos length))
      (insert ";"))))
(use-package cc-mode)
(use-package magit
  :ensure t)
(use-package osx-clipboard
  :ensure t
  :init (osx-clipboard-mode))

(load "~/.emacs.d/evil-tmux-navigator/navigate.el")
(require 'navigate)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rjsx-mode evil-numbers osx-clipboard which-key use-package rainbow-delimiters pdf-tools magit js2-mode ivy-rich helpful evil enh-ruby-mode counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
