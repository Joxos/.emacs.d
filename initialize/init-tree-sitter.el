(use-package tree-sitter :straight t)
(use-package tree-sitter-langs :straight t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'init-tree-sitter)
