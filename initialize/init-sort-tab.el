(use-package sort-tab
  :straight '(sort-tab :type git :host github :repo "manateelazycat/sort-tab")
  :hook (after-init . sort-tab-mode))

(provide 'init-sort-tab)
