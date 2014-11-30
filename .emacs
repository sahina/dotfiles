(set-default-font "Monaco 15")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://barisyuksel.com/cppmode/.emacs
;;
; start package.el with emacs
(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
; initialize package.el
(package-initialize)
(require 'auto-complete-config)
(ac-config-default)
(require 'yasnippet)
(yas-global-mode 1)



; http://truongtx.me/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/
(require 'cc-mode)
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

(require 'auto-complete-clang)
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)

(semantic-mode 1)
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; turn on ede mode
(global-ede-mode 1)
