;; some consts
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")
(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")
(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")
(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")
(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")
(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")
(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")
(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

;; version check
(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; show buffer name as window title
(setq frame-title-format "Emacs")

;; set coding system
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; recognize *.g4 as an antlr file
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

;; Enable recursive minibuffers
;; (setq enable-recursive-minibuffers t)

;; some personal settings
(setq user-full-name "Joxos")
(setq user-mail-address "xujunhao61@163.com")

;; do not blink the cursor
(setq blink-cursor-mode nil)

;; add a newline at the end of a file
(setq require-final-newline t)

;; do not use tab
(setq indent-tabs-mode nil)

;; do not ring the warning bell
(setq ring-bell-function 'ignore)

;; do not show the startup GNU page
(setq inhibit-startup-message t)

;; when reach the last 5 lines than scroll one by one
(setq scroll-margin 5)
(setq scroll-conservatively 1)

;; use "y or p" instead of "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; display line numbers
(defvar display-line-numbers-type 'absolute)
(global-display-line-numbers-mode)

;; indent automatically
(electric-indent-mode)

;; close a pair automatically
(electric-pair-mode)

;; winner-mode
(winner-mode)

;; remember where we left
(add-hook 'after-init-hook 'save-place-mode)

;; highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; hide or show a block
(define-key prog-mode-map (kbd "M-[") 'hs-hide-block)
(define-key prog-mode-map (kbd "M-]") 'hs-show-block)
(define-key prog-mode-map (kbd "M-{") 'hs-hide-all)
(define-key prog-mode-map (kbd "M-}") 'hs-show-all)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(defvar hs-special-modes-alist (mapcar 'purecopy
				       '((c-mode "{" "}" "/[*/]" nil nil)
					 (c++-mode "{" "}" "/[*/]" nil nil)
					 (rust-mode "{" "}" "/[*/]" nil nil))))
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
(defun hideshow-folded-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
	   (info (format " ... #%d " nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
(defvar hs-set-up-overlay 'hideshow-folded-overlay-fn)

;; show the paren matched
(add-hook 'after-init-hook 'show-paren-mode)
(defvar show-paren-when-point-inside-paren t)
(defvar show-paren-when-point-in-periphery t)

;; auto read the files from disk when they're changed
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; recent files recorder
(defvar recentf-filename-handlers '(abbreviate-file-name))
;; (defvar recentf-exclude `("COMMIT_EDITMSG\\'" "TAGS\\'" "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(defvar recentf-exclude `("/ssh:"
			  "/TAGS\\'"
			  "COMMIT_EDITMSG\\'"))
(add-hook 'after-init-hook 'recentf-mode)

;; easy comment
(defun comment-or-uncomment ()
  "Easy comment current region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
(defvar comment-auto-fill-only-comments t)
(global-set-key [remap comment-dwim] #'comment-or-uncomment)

(setq history-length 50)
(add-hook 'after-init-hook 'savehist-mode)

;; Optimization
(when sys/win32p
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K and now 64K)
(unless sys/macp
  (setq command-line-ns-option-alist nil))
(unless sys/linuxp
  (setq command-line-x-option-alist nil))

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

(provide 'init-basic)
