;; Enable line numbers globally
(global-linum-mode)
;; Disable line numbers for specific modes
(dolist (mode '(eshell-mode-hook
		org-mode-hook))
  (add-hook mode (lambda () (linum-mode -1))))

;; Other Settings
(electric-pair-mode)
(setq echo-keystrokes 0.35)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup settings ripped from Sacha Chua
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Default Buffer Settings
;; Set initial buffer
(setq initial-buffer-choice "~/.emacs.d/init.el")

;; Colors
(set-background-color "#404040")
(set-frame-font "JetBrains Mono 26" nil t)
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
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "<escape>") 'abort-recursive-edit); Map Escape to C-g in minibuffer
;; Alternate keys to change active buffer
(global-set-key (kbd "C-j") 'previous-buffer)
(global-set-key (kbd "C-;") 'next-buffer)

;; Package Management
;; Allow Packages to be downloaded from the Internet
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)
(push '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)

;; Provides the use-package macro, which organizes packages well
(require 'use-package)
(setq use-package-always-ensure t)

;; Vim Emulation
(use-package evil
  :init
  (evil-mode 1)
  (setq evil-regexp-search nil)
  :bind (:map evil-motion-state-map
	 ;; Remap hjkl; to jkl;h
	 ("j" . evil-backward-char)
	 ("k" . evil-next-visual-line)
	 ("l" . evil-previous-visual-line)
	 (";" . evil-forward-char)
	 ("h" . evil-repeat-find-char)
	 ;; Add ^ functionality to #
	 ("#" . evil-first-non-blank)
	 :map evil-normal-state-map
	 ("C-p" . package-management)
	 ("q" . compile-based-on-mode)
	 ("<return>" . evil-insert-line-below)
	 :map evil-insert-state-map
	 ;; Shortcuts that work well w/ home-row Ctrl key
	 ("C-g" . evil-normal-state)
	 ("C-d" . evil-normal-state)
	 ("C-f" . evil-normal-state)
	 ("C-h" . backward-delete-char)
	 ("C-o" . insert-minus-sign))
  :config
  (define-prefix-command 'package-management)
  (define-key package-management "r" 'package-refresh-contents)
  (define-key package-management "i" 'package-install)
  (define-key package-management "d" 'package-delete)

  (defun compile-based-on-mode ()
    (interactive)
    (cond ((eq major-mode 'emacs-lisp-mode) (eval-buffer))
	  ((eq major-mode 'help-mode) (kill-buffer-and-window))
	  ((eq major-mode 'apropos-mode) (kill-buffer-and-window))
	  ((eq major-mode 'dired-mode) (kill-buffer-and-window))
	  ((eq major-mode 'messages-buffer-mode) (kill-buffer-and-window))))

(defun insert-minus-sign () (interactive) (insert "-"))

  (evil-set-initial-state 'eshell-mode 'insert)

  (defun evil-insert-line-below (&optional count)
    (interactive "p") ;; Gets universal argument as count
    (evil-append-line 1);; Shift + A
    (dotimes (i count)
      (newline)) 
  (evil-normal-state nil)))

;; Number editing functions
(use-package evil-numbers
  :after evil
  :bind (:map evil-normal-state-map
	 ("C-a" . evil-numbers/inc-at-pt)
	 ("C-S-a" . evil-numbers/dec-at-pt)))

;; Completion
(use-package company
  :init (global-company-mode)
  :bind (:map company-active-map
	 ;; Completion based on number
	 ("1" . (lambda () (interactive) (company-complete-number 1)))
	 ("2" . (lambda () (interactive) (company-complete-number 2)))
	 ("3" . (lambda () (interactive) (company-complete-number 3)))
	 ("4" . (lambda () (interactive) (company-complete-number 4)))
	 ("5" . (lambda () (interactive) (company-complete-number 5)))
	 ("6" . (lambda () (interactive) (company-complete-number 6)))
	 ("7" . (lambda () (interactive) (company-complete-number 7)))
	 ("8" . (lambda () (interactive) (company-complete-number 8)))
	 ("9" . (lambda () (interactive) (company-complete-number 9)))
	 ("0" . (lambda () (interactive) (company-complete-number 10)))
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
  (setq company-dabbrev-char-regexp "[A-z]"))

;; Enable minibuffer vertical completion
(use-package ivy
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
	 ("C-k" . ivy-next-line)
	 ("C-l" . ivy-previous-line)
	 ("C-h" . backward-delete-char)
	 ("C-;" . backward-delete-char)))

;;(use-package counsel)
;;(use-package ivy-rich
;;  :after (ivy)
;;  :init
;;  (setq ivy-rich-path-style 'abbrev
;;	ivy-virtual-abbreviate 'full)
;;  :config
;;  (ivy-rich-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

;; org mode
(require 'org)
(define-key org-mode-map (kbd "C-k") 'kill-this-buffer)
(define-key org-mode-map (kbd "C-j") 'nil)
(add-hook 'org-mode-hook (lambda () (company-mode -1)))

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

;;(defun disable-company-mode () (company-mode -1))
;;(add-hook 'eshell-mode-hook 'disable-company-mode)

; c++ major mode
;;(require 'cc-mode)
;;(require 'company-c-headers)
;;(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
;;(add-hook 'c++-mode-hook (lambda () (interactive)
;; 			   (setq-local company-backends '(company-c-headers company-clang))))
;;(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
;;
;;;; elisp extensions
;;(defun rnth (list n)
;;  "returns the nth to last item in a list. Short for reverse nth"
;;  (nth (- (safe-length list) n) list))
;;
;;(defun file-name-only ()
;;  "returns the file name without any of the folders"
;;  (rnth (split-string buffer-file-name "/") 1))
;;
;;(defun folder-name-only ()
;;  "returns the immediate containing folder name only"
;;  (rnth (split-string buffer-file-name "/") 2))
;;
;;(defun switch-file ()
;;  "switch between .h and .cpp files"
;;  (interactive)
;;  (if (string= (file-name-extension buffer-file-name) "cpp")
;;      (find-file (concat (file-name-sans-extension buffer-file-name) ".h"))
;;    (find-file (concat (file-name-sans-extension buffer-file-name) ".cpp"))))
;;(evil-define-key 'normal c++-mode-map (kbd "s") 'switch-file)
;;
;;(defun c++-new-header-file (classname)
;;  "creates a new header file with default text"
;;  ;; prompt for any string
;;  (interactive "MClass Name: ")
;;  (let* ((filename (concat classname ".h"))
;;	 (oldfile (file-exists-p filename)))
;;    (find-file filename)
;;    (unless oldfile
;;      (progn
;;	(insert-cs2337-comments)
;;	(insert-header-guard)
;;	(insert-class-text)))))
;;(define-key c++-mode-map (kbd "C-x h") 'c++-new-header-file)
;;
;;(defun insert-cs2337-comments ()
;;  "inserts the necessary header comments for CS2337"
;;  (interactive)
;;  (insert "// Sujay Vadlakonda\n")
;;  (insert "// svv190001\n")
;;  (insert "// " (folder-name-only) "\n"))
;;
;;(defun insert-header-guard ()
;;  "inserts a header guard"
;;  (interactive)
;;  (let ((guard (concat (upcase (file-name-sans-extension (file-name-only))) "_H")))
;;    (insert "#ifndef " guard "\n")
;;    (insert "#define " guard "\n\n")
;;    (insert "#endif//" guard)
;;    (evil-previous-line)
;;    (insert "\n")))
;;
;;(defun insert-class-text ()
;;  "inserts the text to begin creating a class"
;;  (interactive)
;;  (insert "class " (file-name-sans-extension (file-name-only)) " {\n")
;;  (insert "private:\n")
;;  (insert "public:\n")
;;  (insert "};\n")
;;  (evil-previous-line)
;;  (evil-previous-line)
;;  (evil-previous-line))
;;
;;(evil-define-key 'normal 'c++-mode-map (kbd "L") 'c-beginning-of-defun)
;;(evil-define-key '(normal visual motion) c++-mode-map (kbd "K") 'c-end-of-defun)
;;(evil-define-key '(normal visual motion) c++-mode-map (kbd "L") 'c-beginning-of-defun)
