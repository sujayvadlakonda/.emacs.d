;; Package management
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(defun require-package (package)
  (unless (package-installed-p package)
    (if (assoc package package-archive-contents)
	(package-install package)
      (package-refresh-contents)
      (package-install package))))

;; Emacs internal
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq warning-minimum-level :error)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
(setq-default truncate-lines t)
(global-auto-revert-mode)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
;; Keyboard 
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Minibuffer
(require-package 'vertico)
(vertico-mode)

(require-package 'orderless)
(require 'orderless)
(setq completion-styles '(orderless basic))

(defun minibuffer-backward-delete ()
  "Backward delete but by directories when possible."
  (interactive)
  (if (eq ?/ (char-before))
      (ignore-errors
        (backward-delete-char 1)
        (while (not (eq ?/ (char-before)))
          (backward-delete-char 1)))
    (backward-delete-char 1)))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-mode-map [remap delete-backward-char] 'minibuffer-backward-delete))

;; Visual feedback for editing
(global-display-line-numbers-mode)

(setq visible-cursor nil)
(blink-cursor-mode 0)

;; Editing behavior/commands
(electric-pair-mode)

(setq save-abbrevs nil)
(abbrev-mode)

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Vim
(require-package 'evil)
(setq evil-want-insert-state-keybinding nil)
(setq evil-undo-system 'undo-redo)
(evil-mode)


(evil-global-set-key 'motion (kbd "h") 'evil-repeat-find-char)
(evil-global-set-key 'motion (kbd "j") 'evil-backward-char)
(evil-global-set-key 'motion (kbd "k") 'evil-next-line)
(evil-global-set-key 'motion (kbd "l") 'evil-previous-line)
(evil-global-set-key 'motion (kbd ";") 'evil-forward-char)

(evil-define-key '(normal motion) global-map
  (kbd "SPC b") 'switch-to-buffer
  (kbd "SPC f") 'find-file
  (kbd "SPC o") 'other-window
  (kbd "SPC s") 'save-buffer
  (kbd "SPC 0") 'delete-window
  (kbd "SPC 1") 'delete-other-windows
  (kbd "SPC 2") (lambda () (interactive) (split-window-below) (other-window 1) (switch-to-buffer nil))
  (kbd "SPC 3") (lambda () (interactive) (split-window-right) (other-window 1) (switch-to-buffer nil))
  (kbd "SPC -") 'global-text-scale-adjust
  (kbd "SPC +") 'global-text-scale-adjust
  (kbd "SPC =") 'global-text-scale-adjust)

(require-package 'multiple-cursors)
(evil-define-key '(normal visual) global-map
  (kbd "n") 'mc/mark-next-like-this
  (kbd "R") 'mc/mark-all-like-this)


;; Git
(require-package 'magit)


;; Java
(define-skeleton java-sout-skeleton "" nil "System.out.println(" _ ");")
(with-eval-after-load 'cc-mode
  (define-abbrev java-mode-abbrev-table "sout" "" 'java-sout-skeleton))
