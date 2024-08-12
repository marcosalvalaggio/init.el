;; Initialize package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Uncomment the following line to enable MELPA Stable if desired
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; General settings
(setq inhibit-startup-message t)               ;; Disable startup message
(setq auto-save-default nil)                   ;; Disable auto-save
(setq auto-save-list-file-prefix nil)          ;; Disable auto-save list file
(setq backup-inhibited t)                      ;; Disable backups

;; UI customizations
(menu-bar-mode 0)                             ;; Disable menu bar
(tool-bar-mode 0)                             ;; Disable tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Start maximized
(set-frame-font "Hack Nerd Font Mono-12")     ;; Set default font
(load-theme 'solarized-wombat-dark t)         ;; Load theme

;; Directory settings
(setq default-directory "C:\\Users\\marsa")   ;; Set default directory

(global-display-line-numbers-mode)

;; Mode configurations
(require 'org)                               ;; Load Org mode
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Additional packages
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)) ;; Enable Company mode

(indent-guide-global-mode)                   ;; Enable indent guides

(global-git-gutter-mode +1)                  ;; Enable Git Gutter
(global-git-gutter-mode t)                   ;; Enable Git Gutter again (Redundant, can be removed)

;; Keybindings
(global-set-key (kbd "M-;") 'comment-dwim-2) ;; Bind M-; to comment-dwim-2

;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit solarized-theme git-gutter+ ## comment-dwim-2 indent-guide company git-gutter tree-sitter tree-sitter-langs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
