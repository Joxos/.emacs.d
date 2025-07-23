;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Joxos"
      user-mail-address "xujunhao61@163.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Source Code Pro" :size 23))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; auto save
;; (setq-default auto-save-timeout 15)
;; (setq-default auto-save-interval 100)
;; (setq require-final-newline t)
;; (setq indent-tabs-mode nil)
;; (setq ring-bell-function 'ignore)
;; (setq inhibit-startup-message t)
;; (setq frame-title-format "Emacs")

;; scroll
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 1000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; backup
;;   (setq make-backup-files nil)
;;   ;; (setq
;;   ;;      backup-by-copying t ;; 自动备份
;;   ;;      backup-directory-alist
;;   ;;      '(("." . "~/.em_backup")) ;; 自动备份在目录"~/.em_backup"下
;;   ;;      delete-old-versions t ;; 自动删除旧的备份文件
;;   ;;      kept-new-versions 3 ;; 保留最近的3个备份文件
;;   ;;      kept-old-versions 1 ;; 保留最早的1个备份文件
;;   ;;      version-control t) ;; 多次备份

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)


;; coding system
;;   (when (fboundp 'set-charset-priority)
;;    (set-charset-priority 'unicode))
;;   (prefer-coding-system 'utf-8)

;;   ;; recognize *.g4 as an antlr file
;;   (add-auto-mode 'antlr-mode "\\.g4\\'")

;;   close a pair automatically
;;   (electric-pair-mode)

;; auto read changed files
(add-hook! 'after-init-hook 'global-auto-revert-mode)

(map! :map evil-normal-state-map
      "gl" 'evil-avy-goto-line
      "gc" 'evil-avy-goto-char-2
      ":" 'execute-extended-command)
;; (use-package! evil-matchit
;;   :straight t
;;   :after evil
;;   :config
;;   (global-evil-matchit-mode 1)
;;   (evilmi-load-plugin-rules '(mhtml-mode) '(template simple html)))
;; (hook) key action
;; basic
;; "p" 'consult-yank-pop
;; SPC i y in doom

;; lsp related
;; "af" 'apheleia-format-buffer
;; "rn" 'lsp-bridge-rename

;; bookmark
;; "mm" 'consult-bookmark
;; "ma" 'bookmark-set
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
