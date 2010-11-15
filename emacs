;; Add directories to load-path
(add-to-list 'load-path "~/.elisp/color-theme")

;; color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-deep-blue)))

;; Set indentation style
(setq c-default-style
      '((java-mode . "java") (other . "bsd")))

;; Put all backup files to temporary folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Editing options
(global-font-lock-mode 1)
(show-paren-mode)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; Fonts
(set-default-font "7x14")

;; Keybindings
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "RET") 'newline-and-indent)
