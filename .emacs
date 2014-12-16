;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (inspired from http://www.aaronbedra.com/emacs.d/)
;; https://github.com/abedra/emacs.d/blob/master/abedra.el

(setq user-full-name "Altug Sahin")
(setq user-mail-address "altug@aecoffice.com")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;
(load "package")
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Packages
;;
(defvar altug/packages '(autopair
			 auto-complete
			 web-mode
			 yasnippet
			 color-theme
			 neotree
			 magit
			 flycheck
			 expand-region
			 linum-relative
			 virtualenvwrapper
			 multiple-cursors
			 helm
			 helm-projectile
			 projectile)
  "Default Packages")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install Default Packages
;;

(defun altug/packages-installed-p ()
  (loop for pkg in altug/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (altug/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg altug/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Options
;;
(set-default-font "Inconsolata 18")

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)
(when (window-system)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

;; y or n instead or yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-;") 'comment-region)


;; (setq echo-keystrokes 0.1
;;       use-dialog-box nil
;;       visible-bell t)
(show-paren-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq column-number-mode t)


(setq js-indent-level 2)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu t)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

(require 'yasnippet)
(yas-global-mode 1)

(require 'color-theme)
(color-theme-initialize)

(load-theme 'zenburn t)

(require 'neotree)
(require 'git)
(require 'magit)

;; http://truongtx.me/2014/04/20/emacs-javascript-completion-and-refactoring/
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)

(require 'linum-relative)
(linum-relative-toggle)

;; duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-d") 'duplicate-line)


;; http://tkf.github.io/emacs-jedi/latest/
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support


(autoload 'markdown-mode "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ecb ide
(require 'ecb)

;; emacs ide package
;(eide-start)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(require 'helm-config)
(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)

;;(require 'icicles)
;;(icy-mode 1)

;;(when (require 'elpy nil t)
;;  (elpy-enable)
;;  (elpy-clean-modeline))
