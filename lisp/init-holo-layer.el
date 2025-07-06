(use-package holo-layer
  :straight '(holo-layer :type git :host github :repo "manateelazycat/holo-layer"
			 :files (:defaults "*.el" "*.py" "plugin" "resources" "swaymsg-treefetch"))
  :init
  (setq holo-layer-enable-cursor-animation t))

(provide 'init-holo-layer)
