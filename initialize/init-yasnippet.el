(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  ;; (define-key yas-keymap [tab] nil)
  ;; (define-key yas-keymap (kbd "C-n") 'yas-next-field)
  ;; (define-key yas-keymap (kbd "C-p") 'yas-prev-field)
  (yas-reload-all))

(provide 'init-yasnippet)
