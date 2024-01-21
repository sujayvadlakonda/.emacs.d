;; Package managment
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

;; Emacs internal
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq native-comp-async-report-warnings-errors nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Keyboard 
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Minibuffer

;; Visual feedback for editing
(display-line-numbers-mode)

(setq visible-cursor nil)
(blink-cursor-mode 0)

;; Editing behavior/commands
(electric-pair-mode)

(add-hook 'prog-mode-hook 'abbrev-mode)
(setq save-abbrevs nil)

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
(evil-mode)

(evil-global-set-key 'motion (kbd "h") 'evil-repeat-find-char)
(evil-global-set-key 'motion (kbd "j") 'evil-backward-char)
(evil-global-set-key 'motion (kbd "k") 'evil-next-line)
(evil-global-set-key 'motion (kbd "l") 'evil-previous-line)
(evil-global-set-key 'motion (kbd ";") 'evil-forward-char)

(evil-global-set-key 'normal (kbd "SPC b") 'switch-to-buffer)
(evil-global-set-key 'normal (kbd "SPC s") 'save-buffer)

(require-package 'evil-mc)
(global-evil-mc-mode)

;; Java
(define-skeleton java-sout-skeleton "" nil "System.out.println(" _ ");")
(with-eval-after-load 'cc-mode
  (define-abbrev java-mode-abbrev-table "sout" "" 'java-sout-skeleton))
