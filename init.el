;; -*- lexical-binding: t; -*-

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

(require-package 'gcmh)
(setq gc-cons-threshold most-positive-fixnum)
(setq gcmh-high-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook 'gcmh-mode)
(setq jit-lock-defer-time 0)

(defun create-missing-directories ()
  "Create any missing directories of the visited file."
  (let ((target-directory (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-directory)
      (make-directory target-directory t))))

(add-to-list 'find-file-not-found-functions #'create-missing-directories)

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

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

(require-package 'consult)
(defmacro sanityinc/no-consult-preview (&rest cmds)
  `(with-eval-after-load 'consult
     (consult-customize ,@cmds :preview-key "M-P")))

(sanityinc/no-consult-preview
 consult-ripgrep
 consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

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

(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(recentf-mode)

;; Visual feedback for editing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq visible-cursor nil)
(blink-cursor-mode 0)

(require-package 'which-key)
(which-key-mode)

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
(setq evil-disable-insert-state-bindings t)
(setq evil-undo-system 'undo-redo)
(evil-mode)


(evil-global-set-key 'motion (kbd "h") 'evil-repeat-find-char)
(evil-global-set-key 'motion (kbd "j") 'evil-backward-char)
(evil-global-set-key 'motion (kbd "k") 'evil-next-line)
(evil-global-set-key 'motion (kbd "l") 'evil-previous-line)
(evil-global-set-key 'motion (kbd ";") 'evil-forward-char)

(evil-define-key* '(normal motion) global-map
  (kbd "SPC b") 'consult-buffer
  (kbd "SPC f") 'find-file
  (kbd "SPC k") (lambda () (interactive) (kill-buffer nil) (message "Killed Buffer!"))
  (kbd "SPC o") 'other-window
  (kbd "SPC s") 'save-buffer
  (kbd "SPC B") 'ibuffer
  (kbd "SPC 0") 'delete-window
  (kbd "SPC 1") 'delete-other-windows
  (kbd "SPC 2") (lambda () (interactive) (split-window-below) (other-window 1) (switch-to-buffer nil))
  (kbd "SPC 3") (lambda () (interactive) (split-window-right) (other-window 1) (switch-to-buffer nil))
  (kbd "SPC -") 'global-text-scale-adjust
  (kbd "SPC +") 'global-text-scale-adjust
  (kbd "SPC =") 'global-text-scale-adjust)

(require-package 'evil-multiedit)
(evil-define-key* 'visual 'global
  (kbd "n")   #'evil-multiedit-match-and-next
  (kbd "N")   #'evil-multiedit-match-and-prev
  (kbd "R")   #'evil-multiedit-match-all)
(evil-define-key* 'normal 'global
  (kbd "n")   #'evil-multiedit-match-symbol-and-next
  (kbd "N")   #'evil-multiedit-match-symbol-and-prev)

;; Org
(setq-default org-startup-folded t)
(setq org-cycle-emulate-tab nil) ; Make tab key call org-cycle everywhere in org-mode buffer

(with-eval-after-load 'org
  (evil-define-key* '(normal motion) org-mode-map
    (kbd "TAB") 'org-cycle))

;; Java
(define-skeleton java-sout-skeleton "" nil "System.out.println(" _ ");")
(with-eval-after-load 'cc-mode
  (define-abbrev java-mode-abbrev-table "sout" "" 'java-sout-skeleton))


;; Theme
(require-package 'modus-themes)
(modus-themes-select 'modus-vivendi-tinted)

;; Mode Line
(setq-default mode-line-format
	      (list
	       ;; the buffer name
	       " %b"

	       ;; was this buffer modified since the last save?
	       '(:eval (and (buffer-modified-p)
			    (propertize " M "
					'face nil
					'help-echo "Buffer has been modified")))

	       ;; is this buffer read-only?
	       '(:eval (and buffer-read-only
			    (propertize " R " 'face nil 'help-echo "Buffer is read-only")))
	       ))


;; Font
(cond
 ((find-font (font-spec :name "JetBrains Mono"))
  (set-frame-font "JetBrains Mono 20" nil t))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas 20" nil t))
 (t (set-frame-font "Monospace 20" nil t)))

;; Buffer List
(require-package 'ibuffer-vc)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-hook 'evil-motion-state)
;; Language Server Protocol
(require-package 'eglot)
;; Kotlin
(require-package 'kotlin-mode)
(with-eval-after-load 'eglot
  (add-hook 'kotlin-mode-hook 'eglot))
