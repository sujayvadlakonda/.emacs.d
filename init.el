;; -*- lexical-binding: t; -*-

;; Package management
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(defun require-package (package)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

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
 consult-source-recent-file consult-source-project-recent-file consult-source-bookmark)

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
(evil-global-set-key 'motion (kbd "k") 'evil-next-visual-line)
(evil-global-set-key 'motion (kbd "l") 'evil-previous-visual-line)
(evil-global-set-key 'motion (kbd ";") 'evil-forward-char)

(evil-global-set-key 'motion (kbd "TAB") 'evil-toggle-fold)
 
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
(setq org-cycle-emulate-tab nil) ; Let `org-cycle' work outside of headings

(setq-default org-startup-folded t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-default-notes-file "~/gtd/inbox.org")
(setq org-directory "~/gtd")
(setq org-agenda-files (list "inbox.org" "gtd.org"))

(setq org-capture-templates
       `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))))

(evil-define-key* 'normal 'global (kbd "SPC c")
  (lambda () (interactive) (org-capture nil "t") (evil-append 1)))

(evil-define-key* 'normal 'global (kbd "SPC a")
  (lambda () (interactive) (org-agenda nil "g") (evil-motion-state)))

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((tags-todo "inbox" ((org-agenda-overriding-header "\nInbox\n")))))))

(setq org-agenda-search-headline-for-time nil)

;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

;; (when *is-a-mac*
;;   (maybe-require-package 'grab-mac-link))

;; (maybe-require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))



(setq org-support-shift-select t)
;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))


;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5) 
("~/gtd/someday.org" :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(setq org-stuck-projects
      '("-INBOX/PROJECT" ("NEXT")))

(let ((active-project-match '(car org-stuck-projects)))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")




(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  )

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t)))))



;; Writing
(set-fill-column 80)

;; Markdown
(require-package 'markdown-mode)

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
