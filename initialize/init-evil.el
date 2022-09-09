(use-package evil
  :straight t
  :demand t
  :bind ((:map evil-normal-state-map
               ("gl" . 'evil-avy-goto-line)
               ("gc" . 'evil-avy-goto-char)
	       (":" . 'execute-extended-command)))
  :init
  (setq evil-want-keybinding nil)
  (setq undo-tree-save-history nil)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode t))
(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1)
  (evilmi-load-plugin-rules '(mhtml-mode) '(template simple html)))
(use-package evil-leader
  :straight t
  :after evil
  :demand t
  :config
  (evil-leader/set-leader "<SPC>")
  ;; (hook) key action
  (evil-leader/set-key
    ;; basic
    "b" 'consult-buffer
    "p" 'consult-yank-pop
    "o" 'ace-window
    "s" 'consult-line
    "g" 'magit
    ";" 'comment-or-uncomment
    "=" 'balance-windows

    ;; lsp related
    "af" 'format-all-buffer
    "rn" 'lsp-bridge-rename

    ;; bookmark
    "mg" 'consult-bookmark
    "ma" 'bookmark-set

    ;; window
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "wu" 'winner-undo
    "wr" 'winner-redo

    ;; file
    "ff" 'find-file
    "fs" 'save-buffer
    "fd" 'delete-current-file
    "fR" 'rename-current-file
    "fr" 'consult-recent-file
    "fo" 'open-current-file

    ;; others
    "rg" 'consult-ripgrep
    )
  (global-evil-leader-mode))
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(provide 'init-evil)
