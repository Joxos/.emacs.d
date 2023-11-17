(use-package doom-themes
  :straight t
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)

  ;; enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'init-doom-themes)
