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
  :hook (markdown-mode . (lambda ()
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
  :hook (csharp-mode . (lambda ()
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

(defun directory-candidate (&rest directories)
  "Return existing directory which first match."
  (seq-find #'file-directory-p directories))

;; org: org mode
(use-package org
  :ensure t
  :custom
  (org-agenda-files
   (directory-files-recursively
    (directory-candidate "~/proj/github/sengelha/org-mode/agenda" "D:\\proj\\github\\sengelha\\org-mode\\agenda")
    "\\.org$"))
  (org-agenda-custom-commands
   '(("d" "Daily agenda"
      ((agenda "" ((org-agenda-span 'day)))))))
  (org-confirm-babel-evaluate nil)
  (org-plantuml-exec-mode 'plantuml)
  (org-startup-with-inline-images t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture))
  :hook (org-mode . (lambda ()
                      (display-fill-column-indicator-mode 1))))

;; org-journal: Journaling
(use-package org-journal
  :ensure t
  :bind
  ("C-c o j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir (directory-candidate "~/proj/github/sengelha/org-mode/journal" "D:\\proj\\github\\sengelha\\org-mode\\journal"))
  (org-journal-date-format "%A, %d %B %Y"))

;; org-roam: Personal knowledge base
(use-package org-roam
  :ensure t
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory (directory-candidate "~/proj/github/sengelha/org-mode/roam" "D:\\proj\\github\\sengelha\\org-mode\\roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
        ("C-c n f" . org-roam-node-find)
        ("C-c n i" . org-roam-node-insert)
        :map org-mode-map
        ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

(provide 'my-modes)
;;; my-modes.el ends here
