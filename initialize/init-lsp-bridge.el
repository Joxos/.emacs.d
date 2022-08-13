(use-package posframe :straight t)
(require 'init-yasnippet)
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
;; (straight-use-package
;;  '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"))
(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'lsp-bridge)
(setq lsp-bridge-python-lsp-server "jedi")
(setq acm-backend-lsp-enable-auto-import nil)
(define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-find-def)
(global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
