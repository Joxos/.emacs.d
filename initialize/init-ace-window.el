(use-package ace-window
  :straight t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . 'ace-window))

(provide 'init-ace-window)
