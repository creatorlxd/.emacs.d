;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
; start package.el with emacs                                                                   
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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
(add-to-list 'load-path "~/.emacs.d/plugins/color-theme-6.6.0") 
(require 'color-theme) 
(color-theme-initialize)
(color-theme-gnome2)

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

;;; key bind

(global-set-key (kbd "C-x C-i") `open-init-file) ;open the init file

(global-set-key (kbd "C-x /") `open-sr-speedbar) ;speed-bar:show file tree in buffer
