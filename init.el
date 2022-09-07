;; docs:
;; https://zhuanlan.zhihu.com/p/26068894
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup-Copying.html
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; git config --global url.https://ghproxy.com/https://github.com/.insteadof https://github.com/
;; git config --global --unset url.https://ghproxy.com/https://github.com/.insteadof https://github.com/

;; installation guide
;; git clone https://github.com/manateelazycat/lsp-bridge.git
;; pip install orjson (optional)
;; install all fonts under fonts/source-code-pro-release/
;; customize default-directory in init-basic.el

;; STRAIGHT SETTINGS
(require 'init-straight)

;;; UTILS
(require 'init-utils)

;;; BASIC SETTINGS
(require 'init-basic)

;;; PACKAGE SETTINGS
(require 'init-packages)
