(straight-use-package
 '(awesome-tray :type git :host github :repo "manateelazycat/awesome-tray"))
(awesome-tray-mode 1)
(setq awesome-tray-active-modules '("git" "location" "file-path" "buffer-name" "mode-name" "battery"))

(provide 'init-awesome-tray)
