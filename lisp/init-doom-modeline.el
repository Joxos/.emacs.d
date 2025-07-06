(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name))
;; since octicons has confict with source code pro, disable icons.
;; (use-package all-the-icons :straight t)

(provide 'init-doom-modeline)
