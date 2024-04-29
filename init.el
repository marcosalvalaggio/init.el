(require 'package)
;; Add MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Uncomment this line to enable MELPA Stable if desired.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Disable menu bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable startup message
(setq inhibit-startup-message t)

;; Disable auto-save
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq backup-inhibited t)

;; Display line numbers
(global-display-line-numbers-mode)
;; (setq-default cursor-type 'bar)

(setq-default indent-tabs-mode nil)

(defun my-c-mode-hook ()
  "Customize C mode."
  (c-set-style "gnu")
  (setq c-basic-offset 4) ;; Set the basic offset to 8 spaces
  (setq c-indent-level 4) ;; Set the indent level to 8 spaces
  (setq-default tab-width 4) ;; Set tab width to 8 spaces (optional)
  (setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs for indentation
  (electric-indent-local-mode 1))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-html-mode-hook ()
  "Customize HTML mode."
  (setq sgml-basic-offset 4) ;; Set indentation to 4 spaces
  (setq-default tab-width 4) ;; Set tab width to 4 spaces
  (setq indent-tabs-mode nil) ;; Use spaces instead of tabs for indentation
  (electric-indent-local-mode 1))

(add-hook 'html-mode-hook 'my-html-mode-hook)

(defun insert-four-spaces ()
  "Insert four spaces."
  (interactive)
  (message "lezzotab")
  (insert "    "))

(global-set-key (kbd "TAB") 'insert-four-spaces)

(defun print-spaces ()
  "Print the number of spaces before the first non-space character in the previous line."
  (interactive)
  (save-excursion
    (forward-line -1) ; Move to the previous line
    (beginning-of-line)
    (skip-chars-forward " ") ; Skip spaces at the beginning of the line
    (let ((num-spaces (current-column))) ; Get the current column position (number of spaces)
      (message "Number of spaces before text: %d" num-spaces))))

;; Any additional customizations can be added below this line

(setq default-directory "C:\\Users\\marsa")

;; org mode
(require 'org)

(set-frame-font "Hack Nerd Font Mono-12")

;; (load "C:\\Users\\marsa\\AppData\\Roaming\\.emacs.d\\funktree.el") ;
;; (global-set-key (kbd "C-c 1") 'funk)

(load-theme 'gruber-darker t)

(global-git-gutter-mode +1)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))


(indent-guide-global-mode)

(global-set-key (kbd "M-;") 'comment-dwim-2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(comment-dwim-2 gruber-darker-theme indent-info company git-gutter doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

