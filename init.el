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
			      ggtags
			      cc-mode
			      popwin
			      exec-path-from-shell
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

;;;close tool-bar
(tool-bar-mode -1)

;;;close-spcroll-bar 
(scroll-bar-mode -1)

;;;Front Setting
(set-default-font "Monaco 16")
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
(smartparens-global-mode t)

;;;ggtags
(require `ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode t))))

;;;popwin
(require `popwin)
(popwin-mode t)

;;;creatorlxd-cpp-mode
(require `cc-mode)
(setq c-basic-offset 4)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list ())
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x 4) tab-stop-list)))
(defconst creatorlxd-cpp-style
  '((c-tab-always-indent . 4)
	(c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
	(c-hanging-semi&comma-criteria . nil)
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "creatorlxd's C++ Programming Style")
(setq c-offsets-alist '((member-init-intro . ++)))
(defun creatorlxd-cpp-mode-common-hook ()
  (c-add-style "creatorlxd-cpp-style" creatorlxd-cpp-style t)
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map "/C-m" 'c-context-line-break)
  )
(add-hook 'c-mode-common-hook 'creatorlxd-cpp-mode-common-hook)

;;;other
;;(when (memq window-system `(mac ns))
;;  (exec-path-from-shell-initialize))
(exec-path-from-shell-initialize);in mac os just use exec...
(global-auto-revert-mode t)
(global-linum-mode t)
(setq inhibit-splash-screen t)
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-company-mode t)
(setq-default cursor-type `bar)
(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode t)
(setq initial-frame-alist `((fullscreen . maximized)));;full screen
(add-hook `emacs-lisp-mode-hook `show-paren-mode);;add paren match for elisp mode

;;;key bind
(global-set-key (kbd "M-x") `smex)  ;for smex
(global-set-key (kbd "C-x C-i") `open-init-file) ;open the init file
(global-set-key (kbd "C-x C-r") `recentf-open-files);recent file
(global-set-key (kbd "C-x /") `open-sr-speedbar) ;speed-bar:show file tree in buffer
(global-set-key (kbd "C-h C-f") `find-function)
(global-set-key (kbd "C-h C-v") `find-variable)
(global-set-key (kbd "C-h C-k") `find-function-on-key)
;;swiper key bind
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
