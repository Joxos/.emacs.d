;; docs:
;; https://zhuanlan.zhihu.com/p/26068894
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup-Copying.html
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; git config --global http.proxy 127.0.0.1:7890
;; git config --global https.proxy 127.0.0.1:7890

;; installation guide
;; pip install orjson cmake-language-server jedi-language-server
;; npm install -g yaml-language-server typescript volar
;; install all fonts under fonts/source-code-pro-release/

;; STRAIGHT SETTINGS
(require 'init-straight)

;;; UTILS
(require 'init-utils)

;;; BASIC SETTINGS
(require 'init-basic)

;;; PACKAGE SETTINGS
(require 'init-packages)
