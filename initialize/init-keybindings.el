;;; This file is to bind some keys at last so that they work.

(define-key yas-keymap (kbd "TAB") nil)
(define-key yas-keymap (kbd "<tab>") nil)
(define-key evil-insert-state-map (kbd "C-n") 'yas-next-field-or-maybe-expand)
(define-key evil-insert-state-map (kbd "C-p") 'yas-prev-field)

(provide 'init-keybindings)
