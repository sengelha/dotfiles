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
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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
  :hook (org-mode-hook . (lambda ()
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

;; keyfreq: Track which commands I use; show with M-x keyfreq-show
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; lsp-mode: Language server mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; lsp-treemacs
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'my-packages)
;;; my-packages.el ends here

