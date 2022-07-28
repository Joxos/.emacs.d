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

(provide 'init-lsp-bridge)
