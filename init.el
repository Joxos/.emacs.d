;;; package -- Summary
;;; Commentary:
;; docs:
;; https://zhuanlan.zhihu.com/p/26068894
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup-Copying.html
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;;; Code:

;; OPTIMIZE GC
(defvar original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;;; BASIC SETTINGS

;; set coding system
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; recognize *.g4 as an antlr file
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

;; Enable recursive minibuffers
;; (setq enable-recursive-minibuffers t)

;; show buffer name as window title
(setq frame-title-format "Emacs")

;; some personal settings
(setq user-full-name "Joxos")
(setq user-mail-address "xujunhao61@163.com")

;; do not blink the cursor
(setq blink-cursor-mode nil)

;; add a newline at the end of a file
(setq require-final-newline t)

;; do not use tab
(setq indent-tabs-mode nil)

;; do not ring the warning bell
(setq ring-bell-function 'ignore)

;; do not show the startup GNU page
(setq inhibit-startup-message t)

;; when reach the last 5 lines than scroll one by one
(setq scroll-margin 5)
(setq scroll-conservatively 1)

;; backup files in ~/.em_backup/
;; this is needed because copying and renaming have several differences
;; (setq version-control t)
;; (setq backup-by-copying t)
;; (setq make-backup-files nil)
;; (setq backup-directory-alist '(("." . "~/.em_backup")))
;; (setq delete-old-versions t)
;; (setq kept-new-versions 3)

;; use "y or p" instead of "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; auto indent regexp
;; (setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")

;; set the indent of python
;; (setq python-indent-offset 4)

;; make emacs cleaner
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; display line numbers
(defvar display-line-numbers-type 'absolute)
(global-display-line-numbers-mode)

;; move the mouse to the corner only if the cursor gets too close
;; (mouse-avoidance-mode 'banish)

;; indent automatically
(electric-indent-mode)

;; close a pair automatically
(electric-pair-mode)

;; winner-mode
(winner-mode)
;; (defun transient-winner-undo ()
;;   "Transient version of `winner-undo'."
;;   (interactive)
;;   (let ((echo-keystrokes nil))
;;     (winner-undo)
;;     (message "Winner: [u]ndo [r]edo")
;;     (set-transient-map
;;      (let ((map (make-sparse-keymap)))
;;        (define-key map [?u] #'winner-undo)
;;        (define-key map [?r] #'winner-redo)
;;        map)
;;      t)))

;; remember where we left
(add-hook 'after-init-hook 'save-place-mode)

;; highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; hide or show a block
(define-key prog-mode-map (kbd "M-[") 'hs-hide-block)
(define-key prog-mode-map (kbd "M-]") 'hs-show-block)
(define-key prog-mode-map (kbd "M-{") 'hs-hide-all)
(define-key prog-mode-map (kbd "M-}") 'hs-show-all)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(defvar hs-special-modes-alist (mapcar 'purecopy
				       '((c-mode "{" "}" "/[*/]" nil nil)
					 (c++-mode "{" "}" "/[*/]" nil nil)
					 (rust-mode "{" "}" "/[*/]" nil nil))))
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
	   (info (format " ... #%d " nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
(defvar hs-set-up-overlay 'hideshow-folded-overlay-fn)

;; show the paren matched
(add-hook 'after-init-hook 'show-paren-mode)
(defvar show-paren-when-point-inside-paren t)
(defvar show-paren-when-point-in-periphery t)

;; auto read the files from disk when they're changed
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; recent files recorder
(defvar recentf-filename-handlers '(abbreviate-file-name))
;; (defvar recentf-exclude `("COMMIT_EDITMSG\\'" "TAGS\\'" "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(defvar recentf-exclude `("/ssh:"
			  "/TAGS\\'"
			  "COMMIT_EDITMSG\\'"))
(add-hook 'after-init-hook 'recentf-mode)

;; easy comment
(defun comment-or-uncomment ()
  "Easy comment current region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
(defvar comment-auto-fill-only-comments t)
(global-set-key [remap comment-dwim] #'comment-or-uncomment)

;;; EDITING UTILS

;; to delete the editing file correctly
(defun delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; to rename the editing file correctly
(defun rename-current-file (new-name)
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
      (rename-buffer new-name))))

;; to open current html file as an url
(defun open-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;; PACKAGE SETTINGS

;; ensure that straight.el is installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; THEME
;; (use-package gruvbox-theme
;; :straight t
;; :init
;; (load-theme 'gruvbox t))

;; EVIL
(use-package evil
  :straight t
  :bind ((:map evil-normal-state-map
               ("gl" . 'evil-avy-goto-line)
               ("gc" . 'evil-avy-goto-char)
	       (":" . 'execute-extended-command)))
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode t))
;; (use-package evil-collection
;;   :straight t
;;   :after evil
;;   :custom ((evil-collection-company-setup t))
;;   :config
;;   (evil-collection-init))
(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1)
  (evilmi-load-plugin-rules '(mhtml-mode) '(template simple html)))
(use-package evil-leader
  :straight t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  ;; (hook) key action
  (evil-leader/set-key
    ;; basic
    "b" 'consult-buffer
    "p" 'consult-yank-pop
    "q" 'save-buffers-kill-emacs
    "i" 'indent-region
    "o" 'ace-window
    "s" 'consult-line
    "g" 'magit
    ";" 'comment-or-uncomment
    "=" 'balance-windows

    ;; lsp related
    "af" 'lsp-bridge-code-format
    "rn" 'lsp-bridge-rename

    ;; window
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "wu" 'winner-undo
    "wr" 'winner-redo

    ;; file
    "ff" 'find-file
    "fs" 'save-buffer
    "fd" 'delete-current-file
    "fR" 'rename-current-file
    "fr" 'consult-recent-file
    "fo" 'open-current-file)
  (global-evil-leader-mode))

;; COMPLETION
;; (use-package corfu
;;   :straight t
;;   :bind (:map corfu-map
;; 	      ([tab] . 'corfu-insert)
;;               ("RET" . 'newline))

;;   ;; Optional customizations
;;   :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
;;   (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
;;   (corfu-quit-no-match t)        ;; Automatically quit if there is no match
;;   (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; You may want to enable Corfu only for certain modes.
;;   :hook (((emacs-lisp-mode vue-mode) . corfu-mode)
;;          (eshell-mode . (lambda ()
;; 			  (setq-local corfu-quit-at-boundary t
;; 				      corfu-quit-no-match t
;; 				      corfu-auto nil)
;; 			  (corfu-mode)))))
(use-package posframe
  :straight t)
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  ;; (define-key yas-keymap [tab] nil)
  ;; (define-key yas-keymap (kbd "C-n") 'yas-next-field)
  ;; (define-key yas-keymap (kbd "C-p") 'yas-prev-field)
  (yas-reload-all))
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
;; (use-package tide
;;   :straight t
;;   :after (typescript-mode corfu flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;; 	 (typescript-mode . corfu-mode)
;;          (before-save . tide-format-before-save)))
;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :bind (;; C-c bindings (mode-specific-map)
  ;;        ("C-c h" . consult-history)
  ;;        ("C-c m" . consult-mode-command)
  ;;        ("C-c k" . consult-kmacro)
  ;;        ;; C-x bindings (ctl-x-map)
  ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ;;        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ;;        ("C-x 5 " . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;;        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;;        ;; Custom M-# bindings for fast register access
  ;;        ("M-#" . consult-register-load)
  ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;        ("C-M-#" . consult-register)
  ;;        ;; Other custom bindings
  ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;;        ("<help> a" . consult-apropos)            ;; orig. apropos-command
  ;;        ;; M-g bindings (goto-map)
  ;;        ("M-g e" . consult-compile-error)
  ;;        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ;;        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ;;        ("M-g m" . consult-mark)
  ;;        ("M-g k" . consult-global-mark)
  ;;        ("M-g i" . consult-imenu)
  ;;        ("M-g I" . consult-imenu-multi)
  ;;        ;; M-s bindings (search-map)
  ;;        ("M-s d" . consult-find)
  ;;        ("M-s D" . consult-locate)
  ;;        ("M-s g" . consult-grep)
  ;;        ("M-s G" . consult-git-grep)
  ;;        ("M-s r" . consult-ripgrep)
  ;;        ("M-s l" . consult-line)
  ;;        ("M-s L" . consult-line-multi)
  ;;        ("M-s m" . consult-multi-occur)
  ;;        ("M-s k" . consult-keep-lines)
  ;;        ("M-s u" . consult-focus-lines)
  ;;        ;; Isearch integration
  ;;        ("M-s e" . consult-isearch-history)
  ;;        :map isearch-mode-map
  ;;        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ;;        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ;;        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ;;        ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)

   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  (setq completion-styles '(substring orderless))
  (define-key vertico-map (kbd "TAB") #'minibuffer-complete)
  )
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
(use-package savehist
  :init
  (savehist-mode))

;; GIT
;; (use-package git-gutter
;; :straight t
;; :init
;; (global-git-gutter-mode +1))
(use-package magit
  :straight t)

;; Others
(use-package which-key
  :straight t
  :init
  (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))
(use-package flycheck
  :straight t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package ace-window
  :straight t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; :bind ("C-x o" . 'ace-window)
  )
;; (use-package dired
;;   :bind
;;   (:map dired-mode-map
;;         ("SPC" . dirvish-show-history)
;;         ("f"   . dirvish-menu-file-info-cmds)
;;         ("r"   . dirvish-roam)
;;         ("M-c" . dirvish-ui-config)
;;         ("M-m" . dirvish-toggle-fullscreen)
;;         ([remap dired-summary] . dirvish-dispatch)
;;         ([remap dired-do-copy] . dirvish-yank)
;;         ([remap mode-line-other-buffer] . dirvish-other-buffer)))
;; (use-package dirvish
;;   :straight t)
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

;; RESET GC
(setq gc-cons-threshold original-gc-cons-threshold)

(provide 'init)
;;; init.el ends here
