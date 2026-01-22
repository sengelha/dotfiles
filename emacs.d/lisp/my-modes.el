;; JSON
(use-package json-mode
  :ensure t)

;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil) ; Query for master file
  (setq safe-local-variable-values '((TeX-command-extra-options . "-shell-escape")))
  (setq reftex-plug-into-AUCTeX t)
  :hook (LaTeX-mode-hook . (lambda ()
                             (setq indent-tabs-mode nil)
                             (setq tab-width 2)
                             (setopt display-fill-column-indicator-column 80)
                             (display-fill-column-indicator-mode)
                             (turn-on-auto-fill)
                             (turn-on-reftex))))

;; PlantUML
(use-package plantuml-mode
  :ensure t
  :custom
  (plantuml-default-exec-mode 'executable))

;; Markdown
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :custom-face
  (markdown-code-face ((t (:foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  :hook (markdown-mode-hook . (lambda ()
                                (setq indent-tabs-mode nil)
                                (setq tab-width 4)
                                (setopt display-fill-column-indicator-column 130)
                                (display-fill-column-indicator-mode))))

;; CMake
(use-package cmake-mode
  :ensure t)

;; Terraform
(use-package terraform-mode
  :ensure t)

;; csharp-mode
(use-package csharp-mode
  :ensure t
  :hook (csharp-mode-hook . (lambda ()
                              (setq indent-tabs-mode t)
                              (setq tab-width 4)
                              (electric-pair-local-mode 1))))

;; C# .csproj support
(use-package csproj-mode
  :ensure t
  :mode (("\\.props\\'" . csproj-mode)))

;; YAML
(use-package yaml-mode
  :ensure t)

;; Makefile mode
(require 'make-mode)

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode))

;; lsp-mode: Language server mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (csproj-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(provide 'my-modes)
;;; my-modes.el ends here
