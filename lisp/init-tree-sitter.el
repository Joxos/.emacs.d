(use-package tree-sitter :straight t)
(use-package tree-sitter-langs :straight t)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(global-tree-sitter-mode)

(provide 'init-tree-sitter)
