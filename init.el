;;; package manage
(when (>= emacs-major-version 24)
  (require `package)
  (package-initialize)
  (setq package-archives '(
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")
    ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ))
  )
(require `cl)
(defvar creatorlxd/packages `(
			      company
			      hungry-delete
			      color-theme
			      swiper
			      counsel
			      smartparens
			      ) "Default packages")
(setq package-selected-packages creatorlxd/packages)
(defun creatorlxd/packages-installed-p ()
  (loop for pkg in creatorlxd/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))
(unless (creatorlxd/packages-installed-p)
  (message "%s" "Refresh the package database...")
  (package-refresh-contents)
  (dolist (pkg creatorlxd/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
;;;utf-8
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8") 

;;; close tool-bar
(tool-bar-mode -1)

;;; close-spcroll-bar 
(scroll-bar-mode -1)


;;;Front Setting
(set-default-font "Consolas 16")
;;;Color Theme
(require 'color-theme) 
(color-theme-initialize)
(color-theme-gnome2)
;;;hungry-delete
(require `hungry-delete)
(global-hungry-delete-mode)
;;;open speedbar 
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'sr-speedbar) 
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-width 20) 
(setq sr-speedbar-right-side nil)
(defun open-sr-speedbar()
  (interactive)
  (set-frame-width (selected-frame) 150)
  (sr-speedbar-toggle))

;;;org
(require `org)
(setq org-src-fontify-natively t)

;;;recentf
(require `recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;;company
(setq-default company-idle-delay 0.25)
(setq-default company-minimum-prefix-length 1)

;;;swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;;smartparens
(require `smartparens-config)
(add-hook `emacs-lisp-mode `smartparens-mode)
(add-hook `c-mode `smartparens-mode)
(add-hook `c++-mode `smartparens-mode)

;;;other
(global-linum-mode t)
(setq inhibit-splash-screen t)
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-company-mode t)
(setq-default cursor-type `bar)
(setq make-backup-files nil)
(delete-selection-mode t)
(setq initial-frame-alist `((fullscreen . maximized)));;full screen
(add-hook `emacs-lisp-mode-hook `show-paren-mode);;add paren match for elisp mode

;;;key bind

(global-set-key (kbd "M-x") `smex)  ;for smex
(global-set-key (kbd "C-x C-i") `open-init-file) ;open the init file
(global-set-key (kbd "C-x C-r") `recentf-open-files);recent file
(global-set-key (kbd "C-x /") `open-sr-speedbar) ;speed-bar:show file tree in buffer
;;swiper key bind
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
