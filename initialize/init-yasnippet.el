(use-package yasnippet
  :straight t
  ;; note that this is needed since yasnippet automatically choose the region
  :hook (after-init-hook . delete-selection-mode)
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-p") 'yas-prev-field))

(provide 'init-yasnippet)
