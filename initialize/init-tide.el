(use-package tide
  :straight t
  :after (typescript-mode)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (setq tide-completion-setup-company-backend nil)
  (setq tide-completion-entry-details nil))

(provide 'init-tide)
