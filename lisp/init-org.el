(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))
;; (use-package ox-hugo
;;   :straight t
;;   :after ox)
(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
(setq org-appear-trigger 'manual)
(add-hook 'org-mode-hook 'org-appear-mode)
(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'evil-insert-state-entry-hook
                                     #'org-appear-manual-start
                                     nil
                                     t)
                           (add-hook 'evil-insert-state-exit-hook
                                     #'org-appear-manual-stop
                                     nil
                                     t)))

(provide 'init-org)
