(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(let* ((fixed-font (font-candidate "BlexMono Nerd Font" "Inconsolata Nerd Font" "Inconsolata" "Fira Code" "Consolas" "Menlo" "DejaVu Sans Mono"))
       (variable-font (font-candidate "SF Pro" "Segoe UI" "Ubuntu Sans" "Lucida Grande" "Helvetica Neue" "Helvetica" "Arial"))
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
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (load-theme 'doom-material t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
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

(provide 'my-theme)
;;; my-theme.el ends here
