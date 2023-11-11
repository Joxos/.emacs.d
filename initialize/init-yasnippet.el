(use-package yasnippet
  :straight t
  :demand t
  ;; note that this is needed since yasnippet automatically choose the region
  :hook (after-init . delete-selection-mode)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

(provide 'init-yasnippet)
