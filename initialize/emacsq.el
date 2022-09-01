;; straight.el
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

;; lsp-bridge
(use-package posframe :straight t)
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  (yas-reload-all))
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'lsp-bridge)
(global-lsp-bridge-mode)

;; web-mode
(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode)))
