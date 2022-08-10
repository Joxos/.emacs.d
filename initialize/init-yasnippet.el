(use-package yasnippet
  :straight t
  ;; note that this is needed since yasnippet automatically choose the region
  :hook (after-init-hook . delete-selection-mode)
  :config
  (yas-global-mode 1)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<tab>") nil)
  (define-key yas-keymap (kbd "C-n") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-p") 'yas-prev-field)
  (yas-reload-all))

(provide 'init-yasnippet)
