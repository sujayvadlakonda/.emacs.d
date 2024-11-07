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
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(require-package 'gcmh)
(setq gc-cons-threshold most-positive-fixnum)
(setq gcmh-high-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook 'gcmh-mode)
(setq jit-lock-defer-time 0)

(load "~/.emacs.d/init-preload-local.el" t)

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
 
(evil-define-key* 'normal 'global (kbd "C-s") #'consult-line)

(evil-define-key* '(normal motion) global-map
  (kbd "SPC b") 'consult-buffer
  (kbd "SPC f") 'find-file
  (kbd "SPC k") (lambda () (interactive) (kill-buffer nil) (message "Killed Buffer!"))
  (kbd "SPC o") 'other-window
  (kbd "SPC s") 'save-buffer
  (kbd "SPC B") 'ibuffer
  (kbd "SPC 0") 'delete-window
  (kbd "SPC 1") 'sanityinc/toggle-delete-other-windows
  (kbd "SPC 2") (lambda () (interactive) (split-window-below) (other-window 1) (switch-to-buffer nil))
  (kbd "SPC 3") (lambda () (interactive) (split-window-right) (other-window 1) (switch-to-buffer nil))
  (kbd "SPC -") 'global-text-scale-adjust
  (kbd "SPC +") 'global-text-scale-adjust
  (kbd "SPC =") 'global-text-scale-adjust)

(require-package 'evil-numbers)


(require-package 'evil-multiedit)

(defun evil-multiedit-match-and-goto-next ()
  "The point of this function is to move the cursor to next selection simultaneously.
Doing so should make `iedit-toggle-selection' skip the most recent selected area.
An unintended benefit could be that we can select targets off the screen because the screen should scroll with the cursor movement."
  (interactive)
  (evil-multiedit-match-and-next)
  (evil-multiedit-next))

(defun evil-multiedit-skip-match-and-goto-next ()
  (interactive)
  (iedit-toggle-selection)
  (evil-multiedit-match-and-goto-next))

(defun evil-multiedit-match-symbol-and-goto-next ()
  (interactive)
  (evil-multiedit-match-symbol-and-next)
  (evil-multiedit-next))

(defun evil-multiedit-skip-match-symbol-and-goto-next ()
  (interactive)
  (iedit-toggle-selection)
  (evil-multiedit-match-symbol-and-goto-next))


(evil-define-key* 'visual 'global
  (kbd "n")   #'evil-multiedit-match-and-goto-next
  (kbd "N")   #'evil-multiedit-skip-match-and-goto-next
  (kbd "R")   #'evil-multiedit-match-all)
(evil-define-key* 'normal 'global
  (kbd "n")   #'evil-multiedit-match-symbol-and-goto-next
  (kbd "N")   #'evil-multiedit-skip-match-symbol-and-goto-next)

(require-package 'evil-mc)
(global-evil-mc-mode)

;; Org
(setq-default org-startup-folded t)
(setq org-cycle-emulate-tab nil) ; Make tab key call org-cycle everywhere in org-mode buffer

(with-eval-after-load 'org
  (evil-define-key* '(normal motion) org-mode-map
    (kbd "TAB") 'org-cycle))

(setq org-todo-keywords
      (quote ((sequence "TODO" "DONE")
	      (sequence "HOLD" "DONE"))))

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
;; Grep -- project search
(setq-default grep-highlight-matches t
              grep-scroll-output t)

(require-package 'wgrep)
(with-eval-after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(when (executable-find "rg")
  (require-package 'rg)
  (global-set-key (kbd "M-?") 'rg-project))

;; Kotlin
(require-package 'kotlin-mode)
(with-eval-after-load 'eglot
  (add-hook 'kotlin-mode-hook 'eglot))

;; Emacs Commands
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(if (fboundp 'rename-visited-file)
    (defalias 'rename-this-file-and-buffer 'rename-visited-file)
  (defun rename-this-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (unless filename
        (error "Buffer '%s' is not visiting a file!" name))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))

;; Editing commands
(require-package 'unfill)

;; Macro performance
(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
	font-lock-mode
	(tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)

;; Windows
(winner-mode) ; provide undo window setup

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

(load "~/.emacs.d/init-local.el" t)
