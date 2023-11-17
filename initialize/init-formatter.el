;; (use-package format-all
;;   :straight (:type git :host github :repo "lassik/emacs-format-all-the-code"))

(straight-use-package 'apheleia)
(apheleia-global-mode +1)
;; (setf (alist-get 'isort apheleia-formatters)
;;       '("isort" "--stdout" "-"))
;; (setf (alist-get 'python-mode apheleia-mode-alist)
;;       '(isort black))

(provide 'init-formatter)
