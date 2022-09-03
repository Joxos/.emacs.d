(use-package org-bars
  :straight (:host github :repo "tonyaldon/org-bars")
  :init
  (setq org-bars-stars '(:empty "*"
				:invisible "+"
				:visible "-"))
  :config
  (add-hook 'org-mode-hook #'org-bars-mode)
  :custom-face
  (org-level-1 ((t (:height 1.5))))
  (org-level-2 ((t (:height 1.3))))
  (org-level-3 ((t (:height 1.1))))
  (org-level-4 ((t (:height 1.05)))))
(provide 'init-org)
