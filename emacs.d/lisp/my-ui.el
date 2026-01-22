(setq inhibit-startup-message t)
(scroll-bar-mode -1)                   ; Disable visible scrollbar
(tool-bar-mode -1)                     ; Disable the toolbar
(tooltip-mode -1)                      ; Disable tooltips
(set-fringe-mode 10)                   ; Give some breathing room
(menu-bar-mode 1)                      ; Enable the menu bar (on Mac it doesn't take any space)
(setq visible-bell t)                  ; Set up the visible bell
(setq column-number-mode t)
(setq line-number-mode t)
(global-display-line-numbers-mode t)

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(let* ((fixed-font (font-candidate "BlexMono Nerd Font" "Inconsolata Nerd Font" "Inconsolata" "Fira Code" "Consolas" "Menlo" "DejaV
u Sans Mono"))
       (variable-font (font-candidate "Segoe UI" "Lucida Grande" "Helvetica Neue" "Helvetica" "Arial"))
       (fixed-font-height 140)
       (variable-font-height 140))
  (set-face-attribute 'default nil
                      :family fixed-font
                      :height fixed-font-height)
  (set-face-attribute 'fixed-pitch nil
                      :family fixed-font
                      :height fixed-font-height)
  (set-face-attribute 'variable-pitch nil
                      :family variable-font
                      :height variable-font-height))

;; Set window size
(if (window-system)
    (set-frame-size (selected-frame) 180 80))

;; doom-themes: Use Doom color themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; all-the-icons: Install icons package
(use-package all-the-icons
  :ensure t)

;; doom-modeline: Install doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; rainbow-delimiters: colorful matching () delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'my-ui)
;;; my-ui.el ends here
