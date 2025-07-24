;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; no need to run 'doom sync' after modifying this file

;; personal info
(setq user-full-name "Joxos"
      user-mail-address "xujunhao61@163.com")

;; appearance
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Source Code Pro" :size 23))

(setq display-line-numbers-type 'relative)

;; scroll
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 1000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; backup (?)

;; coding system (not seemed as a part of default settings)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; recognize *.g4 as an antlr file
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

;; auto read changed files
(add-hook! 'after-init-hook 'global-auto-revert-mode)

;; evil
(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1)
  (evilmi-load-plugin-rules '(mhtml-mode) '(template simple html)))
(map! :map evil-normal-state-map
      "gl" 'evil-avy-goto-line
      "gc" 'evil-avy-goto-char-2)
(map! :map (evil-normal-state-map evil-visual-state-map)
      ":" 'execute-extended-command)
(map!
  :leader
  ";" 'evilnc-comment-or-uncomment-lines
  "rg" 'consult-ripgrep
  ;; window
  "0" 'evil-window-delete
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right
  "=" 'balance-windows
  "o" 'ace-select-window)

;; lsp-bridge
(after! lsp-bridge
  (map! :map lsp-bridge-mode-map "RET" 'newline-and-indent)
  (map! :map evil-normal-state-map "C-]" 'lsp-bridge-find-def))

(toggle-frame-maximized)

;; kbds
;; basic
;; SPC i y: yank pop

;; lsp related
;; "af" 'apheleia-format-buffer
;; "rn" 'lsp-bridge-rename

;; bookmark
;; SPC RET: jump to bookmark
;; SPC b m: set a bookmark
