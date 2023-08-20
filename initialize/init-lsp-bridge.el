(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
;; (straight-use-package
;;  '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"))
;; (add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'init-yasnippet)
;; (require 'lsp-bridge)
;; (setq lsp-bridge-python-lsp-server "jedi")
;; (setq acm-backend-lsp-enable-auto-import nil)
;; (append lsp-bridge-multi-lang-server-extension-list '(("html") . "html_emmet"))
;; (setq lsp-bridge-multi-lang-server-extension-list '((("vue") . "volar_emmet")
;; 						    (("html") . "html_emmet")
;; 						    (("css") . "css_emmet")))
;; (define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-find-def)
;; (define-key lsp-bridge-mode-map (kbd "RET") 'newline)
;; (global-lsp-bridge-mode)

;; evil dabbrev
(define-key evil-insert-state-map (kbd "TAB") 'dabbrev-expand)

(provide 'init-lsp-bridge)
