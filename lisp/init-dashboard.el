(use-package dashboard
  :straight t
  :init
  (setq dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
