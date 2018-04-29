;;; package manage
(when (>= emacs-major-version 24)
  (require `package)
  (package-initialize)
  (setq package-archives '(
    ("melpa-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")
    ))
  )
(require `cl)
(defvar creatorlxd/packages `(
			      company
			      hungry-delete
			      color-theme
			      ) "Default packages")
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
;;; company package
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;org
(require `org)
(setq org-src-fontify-natively t)

;;;recentf
(require `recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

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

;;; key bind

(global-set-key (kbd "C-x C-i") `open-init-file) ;open the init file
(global-set-key (kbd "C-x C-r") `recentf-open-files);recent file
(global-set-key (kbd "C-x /") `open-sr-speedbar) ;speed-bar:show file tree in buffer
