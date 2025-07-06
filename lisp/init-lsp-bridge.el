;; /// script
;; dependencies = [
;;   "epc",
;;   "orjson",
;;   "packaging",
;;   "sexpdata",
;;   "six",
;;   "setuptools",
;;   "paramiko",
;;   "rapidfuzz",
;;   "watchdog",
;; ]
;; ///

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			 :build (:not compile))
  :init
  ;; (setq lsp-bridge-python-lsp-server "jedi")
  (setq lsp-bridge-python-command "python")
  (setq lsp-bridge-python-multi-lsp-server "pyright-langserver")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  (setq acm-backend-lsp-enable-auto-import nil)
  ;; (append lsp-bridge-multi-lang-server-extension-list '(("html") . "html_emmet"))
  ;; (setq lsp-bridge-multi-lang-server-extension-list '((("vue") . "volar_emmet")
  ;; 						      (("html") . "html_emmet")
  ;; 						      (("css") . "css_emmet")))
  :config
  (define-key evil-normal-state-map (kbd "C-]") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "RET") 'newline)
  (global-lsp-bridge-mode))
;; evil dabbrev
;; (define-key evil-insert-state-map (kbd "TAB") 'dabbrev-expand)

(provide 'init-lsp-bridge)
