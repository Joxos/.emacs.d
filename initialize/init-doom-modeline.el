(use-package doom-modeline
  :straight t
  :init
  :config
  (doom-modeline-mode 1))
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(provide 'init-doom-modeline)
