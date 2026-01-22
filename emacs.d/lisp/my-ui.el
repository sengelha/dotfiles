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

;; treemacs: folder tree
(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :hook (emacs-startup-hook . treemacs))

;; vertico: minibuffer completion
(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; consult: search and navigate via completing-read
(use-package consult
  :ensure t)
  
;; corfu: in-buffer completion
(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; emacs: additional completion-related settings
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; orderless: use `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; marginalia: Provide helpful annotations next to completion candidates
(use-package marginalia
  :config
  (marginalia-mode 1))

;; cape
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; embark: mini-buffer actions rooted in keymaps
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; which-key: interactive help when using C-c, etc.
(use-package which-key
  :after lsp-mode
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; savehist: Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; flycheck: on-the-fly syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; helpful: Better help
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; keyfreq: Track which commands I use; show with M-x keyfreq-show
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'my-ui)
;;; my-ui.el ends here
