(use-package dashboard
  :straight t
  ;; :init
  ;; (setq dashboard-vertically-center-content t)
  :config
  (setq dashboard-items '((recents . 20) (bookmarks . 5) (agenda . 5)))
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
