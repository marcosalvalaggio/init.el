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
;;(set-frame-font "Hack Nerd Font Mono-10")      ;; Set default font
(set-frame-font "Cascadia Code-12")
;; (load-theme 'manoj-dark t)                     ;; theme
(global-display-line-numbers-mode)

;; theme;; theme
(load-theme 'misterioso t)
;; Personalizzazione del tema misterioso per farlo assomigliare a melange
(let ((melange-bg "#2A2520")        ;; Sfondo scuro, marrone-grigio
      (melange-fg "#ECE1D7")        ;; Primo piano chiaro, beige
      (melange-comment "#8B7761")   ;; Commenti, beige scuro
      (melange-yellow "#E3B56F")    ;; Giallo caldo per funzioni
      (melange-red "#CE8196")       ;; Rosso per operatori
      (melange-green "#90B99F")     ;; Verde per preprocessore
      (melange-cyan "#8FB3BE")      ;; Ciano per tipi
      (melange-blue "#A3A9CE")      ;; Blu per stringhe
      (melange-magenta "#C39AC9")   ;; Magenta per costanti
      (melange-ui-bg "#362f2b"))    ;; Sfondo UI leggermente più chiaro
      
  ;; Applicare i colori personalizzati
  (custom-set-faces
   ;; Sfondo e testo principale
   `(default ((t (:foreground ,melange-fg :background ,melange-bg))))
   
   ;; Elementi di interfaccia
   `(mode-line ((t (:background ,melange-ui-bg :foreground ,melange-fg))))
   `(mode-line-inactive ((t (:background ,melange-bg :foreground ,melange-comment))))
   `(vertical-border ((t (:foreground ,melange-ui-bg))))
   `(fringe ((t (:background ,melange-bg))))
   `(region ((t (:background "#3D3733"))))
   
   ;; Elementi di sintassi - seguendo la filosofia di melange:
   ;; "Control flow should use warm colors and data should use cold colors"
   `(font-lock-comment-face ((t (:foreground ,melange-comment :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,melange-comment))))
   `(font-lock-function-name-face ((t (:foreground ,melange-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,melange-red))))
   `(font-lock-preprocessor-face ((t (:foreground ,melange-green))))
   `(font-lock-string-face ((t (:foreground ,melange-blue :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,melange-magenta))))
   `(font-lock-type-face ((t (:foreground ,melange-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,melange-fg))))
   `(font-lock-builtin-face ((t (:foreground ,melange-magenta :slant italic))))
   
   ;; Altri elementi di sintassi
   `(highlight ((t (:background "#443D37"))))
   `(isearch ((t (:background ,melange-yellow :foreground ,melange-bg))))
   `(lazy-highlight ((t (:background ,melange-green :foreground ,melange-bg))))
   
   ;; Line numbers
   `(line-number ((t (:foreground ,melange-comment :background ,melange-bg))))
   `(line-number-current-line ((t (:foreground ,melange-fg :background ,melange-ui-bg))))
   
   ;; Supporto per company-mode
   `(company-tooltip ((t (:background ,melange-ui-bg :foreground ,melange-fg))))
   `(company-tooltip-selection ((t (:background "#4A423B" :foreground ,melange-fg))))
   
   ;; Supporto per show-paren-mode
   `(show-paren-match ((t (:background "#573E33" :foreground ,melange-fg))))
   
   ;; Org-mode
   `(org-level-1 ((t (:foreground ,melange-yellow :weight bold))))
   `(org-level-2 ((t (:foreground ,melange-green :weight bold))))
   `(org-level-3 ((t (:foreground ,melange-blue :weight bold))))
   `(org-level-4 ((t (:foreground ,melange-magenta :weight bold))))
   `(org-link ((t (:foreground ,melange-cyan :underline t))))))


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
(add-hook 'js-mode-hook (lambda () (my-setup-indent 2)))

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

;; UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(when (boundp 'buffer-file-coding-system)
  (setq-default buffer-file-coding-system 'utf-8))

;; powershell
(setq explicit-shell-file-name "powershell.exe")
(setq shell-file-name "powershell.exe")
(setq explicit-powershell.exe-args '("-NoLogo" "-ExecutionPolicy" "Bypass"))


;; pdf
(setq doc-view-resolution 300)

;; Bind Alt-q (M-q) to kill the current buffer without saving
(global-set-key (kbd "M-q") 'kill-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elixir-mode tree-sitter-langs nerd-icons highlight-indent-guides go-mode comment-dwim-2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
