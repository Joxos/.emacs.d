;; Garbage Collector Magic Hack
(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 10 ; wait for 10s when user do nothing before gc
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(provide 'init-gcmh)
