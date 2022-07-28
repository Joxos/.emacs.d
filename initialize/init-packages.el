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
;; (straight-use-package
;;  '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"))
(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'lsp-bridge)
(setq lsp-bridge-python-lsp-server "jedi")
(global-lsp-bridge-mode)
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
;; (use-package flycheck
;;   :straight t
;;   :init (global-flycheck-mode))
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

(provide 'init-packages)
