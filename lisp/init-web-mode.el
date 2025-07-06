;; web mode
(use-package web-mode
  :straight t
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;; CSS mode
(use-package css-mode
  :straight nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :straight t
  :init
  ;; disable complilation on save
  (setq scss-compile-at-save nil))

;; new `less-css-mde' in Emacs 26
(use-package less-css-mode :straight t)

;; JSON mode
(use-package json-mode :straight t)

(provide 'init-web-mode)
