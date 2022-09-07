(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))
(use-package ox-hugo
  :straight t
  :after ox)

(provide 'init-org)
