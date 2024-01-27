;; Package managment
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(defun require-package (package)
  (unless (package-installed-p package)
    (condition-case-unless-debug err
	(progn
	  (when (assoc package (bound-and-true-p
				package-pinned-packages))
	    (package-read-all-archive-contents))
	  (if (assoc package package-archive-contents)
	      (package-install package)
	    (package-refresh-contents)
	    (when (assoc package (bound-and-true-p
				  package-pinned-packages))
	      (package-read-all-archive-contents))
	    (package-install package))
	  t)
      (error
       (display-warning 'use-package
			(format "Failed to install %s: %s"
				name (error-message-string err))
			:error)))))
;; Emacs internal
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq native-comp-async-report-warnings-errors nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
(setq-default truncate-lines t)
(scroll-bar-mode 0)
(global-auto-revert-mode)

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

(evil-global-set-key 'normal (kbd "SPC b") 'switch-to-buffer)
(evil-global-set-key 'normal (kbd "SPC f") 'find-file)
(evil-global-set-key 'normal (kbd "SPC o") 'other-window)
(evil-global-set-key 'normal (kbd "SPC s") 'save-buffer)

(evil-global-set-key 'normal (kbd "SPC 0") 'delete-window)
(evil-global-set-key 'normal (kbd "SPC 1") 'delete-other-windows)

(evil-global-set-key 'normal (kbd "SPC -") 'global-text-scale-adjust)
(evil-global-set-key 'normal (kbd "SPC +") 'global-text-scale-adjust)

(require-package 'evil-mc)
(global-evil-mc-mode)

;; Emacs Lisp

;; Java
(define-skeleton java-sout-skeleton "" nil "System.out.println(" _ ");")
(with-eval-after-load 'cc-mode
  (define-abbrev java-mode-abbrev-table "sout" "" 'java-sout-skeleton))
