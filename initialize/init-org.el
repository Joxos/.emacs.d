(use-package org-bars
  :straight (:host github :repo "tonyaldon/org-bars")
  :init
  (setq org-bars-stars '(:empty "*"
				:invisible "+"
				:visible "-"))
  :config
  (add-hook 'org-mode-hook #'org-bars-mode))
(provide 'init-org)
