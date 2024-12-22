;; General settings
(setq inhibit-startup-message t)               ;; Disable startup message
(setq auto-save-default nil)                   ;; Disable auto-save
(setq auto-save-list-file-prefix nil)          ;; Disable auto-save list file
(setq backup-inhibited t)                      ;; Disable backups
(setq ls-lisp-dirs-first t)                    ;; Directory first

;; UI customizations
(menu-bar-mode 0)                              ;; Disable menu bar
(tool-bar-mode 0)                              ;; Disable tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Start maximized
(set-frame-font "Hack Nerd Font Mono-10")      ;; Set default font
(load-theme 'manoj-dark t)                     ;; theme
(global-display-line-numbers-mode)

;; Directory settings
(setq default-directory "C:\\Users\\marsa")    ;; Set default directory

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)

(defun my-setup-indent (n)
  "Change the indentation settings to use N spaces."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width n)
  (setq-local c-basic-offset n)
  (setq-local python-indent-offset n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local css-indent-offset n)
  (setq-local lisp-body-indent n))
(add-hook 'prog-mode-hook (lambda () (my-setup-indent 4)))
(add-hook 'text-mode-hook (lambda () (my-setup-indent 4)))
(add-hook 'html-mode-hook (lambda () (my-setup-indent 2)))
(add-hook 'xml-mode-hook (lambda () (my-setup-indent 2)))
(add-hook 'emacs-lisp-mode-hook (lambda () (my-setup-indent 2)))
(add-hook 'scheme-mode-hook (lambda () (my-setup-indent 2)))

;; Smooth scroll
(setq scroll-step 3)

;; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Split window and setup buffers
(defun my-setup-windows ()
  "Split windows and open desired buffers."
  (split-window-horizontally)                      ;; Split into two windows
  (dired "C:\\Users\\marsa\\code")                 ;; Open directory in left window
  (other-window 1)                                 ;; Switch to right window
  (switch-to-buffer "*scratch*"))                  ;; Open scratch buffer
;; Run window setup after Emacs starts
(add-hook 'emacs-startup-hook 'my-setup-windows)

;; Powershell command
(defun execute-powershell-command (command)
  "Execute a PowerShell command silently."
  (interactive "sEnter PowerShell command: ")
  (let ((output (shell-command-to-string (concat "powershell -Command " (shell-quote-argument command)))))
    (message "Command executed. Output: %s" output)))
;; Powershell command keybind
(global-set-key (kbd "C-c C-x") 'execute-powershell-command)

;; get funciont name in file
(global-set-key (kbd "M-j") 'imenu)

;; Bind Alt-m (M-m) to switch windows
(global-set-key (kbd "M-7") 'other-window)

;; Bind Alt-q (M-q) to kill the current buffer without saving
(global-set-key (kbd "M-q") 'kill-buffer)
