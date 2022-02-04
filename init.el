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

;;; BASIC SETTINGS

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Add prompt indicator to `completing-read-multiple'.
;; Alternatively try `consult-completing-read-multiple'.
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; ESC to quit
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; remap keys searching history in minibuffer
;; (define-key minibuffer-local-map (kbd "C-n") 'next-history-element)
;; (define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)

;; show buffer name as window title
;; (setq frame-title-format "%b")

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
;(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")

;; set the indent of python
(defvar python-indent-offset 4)

;; assign the checker path manually
;; TODO: change it when needed
;(defvar flycheck-python-flake8-executable "C:/Users/Joxos/AppData/Local/Programs/Python/Python39/Scripts/flake8.exe")

;; make emacs cleaner
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; display line numbers
(defvar display-line-numbers-type 'absolute)
(add-hook 'after-init-hook 'global-display-line-numbers-mode)

;; move the mouse when the cursor gets too close to it
(mouse-avoidance-mode 'exile)

;; indent automatically
(add-hook 'after-init-hook 'electric-indent-mode)

;; close a pair automatically
(add-hook 'after-init-hook 'electric-pair-mode)

;; record the change of windows
(add-hook 'after-init-hook 'winner-mode)

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
  "`OV', I don't know."
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
(setq package-user-dir "~/.emacs.d/straight/")
(defvar recentf-filename-handlers '(abbreviate-file-name))
(defvar recentf-max-saved-items 100)
(defvar recentf-max-menu-items 10)
(defvar recentf-exclude `("COMMIT_EDITMSG\\'" "TAGS\\'" "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
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
(use-package gruvbox-theme
  :straight t
  :init
  (load-theme 'gruvbox t))

;; COMPLETION
(use-package corfu
  :straight t
  :bind (:map corfu-map
	      ("TAB" . 'corfu-insert)
	      ("<backtab>" . 'corfu-complete)
              ("RET" . 'newline))

  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . (lambda ()
			  (setq-local corfu-quit-at-boundary t
				      corfu-quit-no-match t
				      corfu-auto nil)
			  (corfu-mode))))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))
;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package lsp-mode
  :straight t
  :custom
  (lsp-completion-provider :none)
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))  (setq lsp-mode t))
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  (setq-local completion-at-point-functions (list (lsp-completion-at-point)))
  (setq lsp-idle-delay 0.1)
  :hook
  ((lsp-completion-mode . my/lsp-mode-setup-completion)
   (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
   (c++-mode . lsp)))
(use-package lsp-ui
  :straight t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
(use-package lsp-pyright
  :straight t)
(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1))
;; company
;; (use-package company-quickhelp
;;   :straight t
;;   :hook (company-mode . company-quickhelp-mode)
;;   :init
;;   (setq company-quickhelp-delay 0)
;;   (setq company-quickhelp-max-lines 10))
;; (use-package company-jedi
;;   :straight t)
;; (use-package company-mode
;;   :straight t
;;   :hook ((prog-mode . company-mode)
;; 	 (css-mode . (lambda ()
;; 		       (setq company-backends '((company-css)))))
;; 	 (emacs-lisp-mode . (lambda ()
;; 			      (setq company-backends '((company-elisp)))))
;; 	 (c++-mode . (lambda ()
;; 		       (setq company-backends '((company-clang)))))
;; 	 (python-mode . (lambda ()
;; 			  (setq company-backends '((company-jedi))))))
;;   :bind ("C-c /" . 'company-files)
;;   :init
;;   (defvar company-minimum-prefix-length 1)
;;   (defvar company-idle-delay 0)
;;   (defvar company-show-numbers t)
;;   (setq company-backends '((company-capf company-keywords))))
;; (use-package counsel
;;   :straight t
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "(%d/%d) ")
;;   (ivy-mode t))
;; (use-package ivy-rich
;;   :straight t
;;   :config
;;   (ivy-rich-mode t))
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
  ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
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
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
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
  ;; (setq vertico-cycle t)
  )
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; EVIL
(use-package evil
  :straight t
  :bind ((:map evil-motion-state-map
               ("f" . 'evil-avy-goto-char-in-line)
               ("F" . 'evil-avy-goto-char-in-line))
         (:map evil-normal-state-map
               ("gl" . 'evil-avy-goto-line)
               ("gc" . 'evil-avy-goto-char)
	       (":" . 'execute-extended-command)))
  ;; :init
  ;; (setq evil-want-keybinding nil)
  :config
  (evil-mode t))
;; (use-package evil-collection
;;   :straight t
;;   :after evil
;;   :custom ((evil-collection-company-setup t))
;;   :config
;;   (evil-collection-init))
(use-package evil-leader
  :straight t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  ;; (hook) key action
  (evil-leader/set-key
    ;; basic
    ;; "af" 'lsp-format-buffer
    "w" 'save-buffer
    "b" 'consult-buffer
    "p" 'consult-yank-pop
    "q" 'save-buffers-kill-emacs
    "i" 'indent-region
    "o" 'ace-window
    "s" 'consult-line
    "t" 'treemacs-select-window
    "g" 'magit
    ";" 'comment-or-uncomment
    "=" 'balance-windows

    ;; window
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right

    ;; file
    "ff" 'find-file
    "fd" 'delete-current-file
    "fR" 'rename-current-file
    "fr" 'consult-recent-file
    "fo" 'open-current-file)
  (global-evil-leader-mode))
(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1)
  (evilmi-load-plugin-rules '(mhtml-mode) '(template simple html)))

;; GIT
(use-package git-gutter
  :straight t
  :init
  (global-git-gutter-mode +1))
(use-package magit
  :straight t)

;; OTHERS
;; (use-package which-key
;;   :straight t
;;   :init
;;   (setq which-key-idle-secondary-delay 0.05)
;;   :config
;;   (which-key-mode))
(use-package flycheck
  :straight t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package ace-window
  :straight t
  :bind ("C-x o" . 'ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package treemacs
  :straight t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
(use-package treemacs-evil
  :straight t
  :after (treemacs evil))
(use-package lsp-treemacs
  :straight t
  :init
  (lsp-treemacs-sync-mode 1))
;; (use-package projectile
;;   :straight t
;;   :init
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
;; (use-package treemacs-projectile
;;   :straight t
;;   :after (treemacs projectile))
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)
;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))
(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

;; RESET GC
(setq gc-cons-threshold original-gc-cons-threshold)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("729ddf899d07810d66fb6bd048b1cbef228efbcee0dca69d3d6cd0efcff428e1" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
