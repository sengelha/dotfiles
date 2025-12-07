;;; -*- Mode: Emacs-Lisp -*-
;; User Interface
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
(setq make-backup-files nil)           ; Do not generate ~ files

(setq-default indent-tabs-mode nil)    ; Use space for indentation by default
(setq-default tab-width 4)             ; Default tab width when not overriden by modes
(setq-default require-final-newline t) ; Always require newline at end of file

;; Set preferred fonts
(require 'cl-lib)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(let* ((fixed-font (font-candidate "BlexMono Nerd Font" "Inconsolata Nerd Font" "Inconsolata" "Fira Code" "Consolas" "Menlo" "DejaVu Sans Mono"))
       (variable-font (font-candidate "Segoe UI" "Lucida Grande" "Helvetica Neue" "Helvetica" "Arial"))
       (fixed-font-height (cond ((>= (display-pixel-height) 2160) 200)
				(t 140)))
       (variable-font-height (cond ((>= (display-pixel-height) 2160) 200)
				   (t 140))))
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

;; Add homebrew to PATH
(when (file-directory-p "/opt/homebrew/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
  (setq exec-path (append exec-path '("/opt/homebrew/bin"))))

;; Keybindings
(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(end)] 'end-of-line)
(global-set-key [(control tab)] 'bury-buffer)
(global-set-key [(shift control tab)] 'unbury-buffer)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load packages

;; Corfu (auto-completion)
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (cofru-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :bind (:map corfu-map
	      ( "M-SPC" . corfu-insert-separator)
	      ( "RET" . nil)
	      ("TAB" . corfu-next)
	      ([tab] . corfu-next)
	      ("S-TAB" . corfu-previous)
	      ([backtab] . corfu-previous)
	      ("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Emacs package
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Git
(use-package magit
  :defer t)

;; Treemacs (folder tree)
(use-package treemacs
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(add-hook 'emacs-startup-hook 'treemacs)

;; Vertico (autocompletion)
(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; Orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: Provide helpful annotations next to completion candidates
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Which key for interactive help when using C-c, etc.
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; PlantUML
(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'executable))

;; JSON
(use-package json-mode)

;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil) ; Query for master file
  (setq safe-local-variable-values '((TeX-command-extra-options . "-shell-escape")))
  (setq reftex-plug-into-AUCTeX t)
  )
(defun sengelha/latex-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setopt display-fill-column-indicator-column 80)
  (display-fill-column-indicator-mode)
  (turn-on-auto-fill)
  (turn-on-reftex))
(add-hook 'LaTeX-mode-hook 'sengelha/latex-mode-hook)

;; Markdown
(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :custom-face
  (markdown-code-face ((t (:foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  )
(defun sengelha/markdown-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setopt display-fill-column-indicator-column 130)
  (display-fill-column-indicator-mode))
(add-hook 'markdown-mode-hook 'sengelha/markdown-mode-hook)

;; CMake
(use-package cmake-mode)

;; Terraform
(use-package terraform-mode)

;; flycheck: on-the-fly syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; cpputils-cmake: Syntax check and code-completion if you use cmake
(use-package cpputils-cmake
  :after (flycheck))
(defun sengelha/cpputils-cmake-mode-hook ()
  (if (derived-mode-p 'c-mode 'c++-mode)
      (cppcm-reload-all)
    ))
(add-hook 'c-mode-common-hook 'sengelha/cpputils-cmake-mode-hook)

;; C/C++

;; C#
(require 'csharp-mode)
(defun sengelha/my-csharp-mode-hook ()
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (electric-pair-local-mode 1))
(add-hook 'csharp-mode-hook 'sengelha/my-csharp-mode-hook)

;; C# .csproj support
(use-package csproj-mode)

;; dotnet mode
(use-package dotnet)
(add-hook 'csharp-mode-hook 'dotnet-mode)

;; YAML
(use-package yaml-mode)

;; Makefile mode
(require 'make-mode)

;; Editorconfig: Use and respect .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Rainbow delibeters for pretty () matching
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Doom color themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)

  (load-theme 'doom-palenight t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Determine C++ indentation settings from clang-format file
(use-package clang-format
  :after (s)
  :init
  (defun get-clang-format-option (config-str field is-num)
    "Retrieve a config option from a clang-format config.

CONFIG-STR is a string containing the entire clang-format config.
FIELD is specific option, e.g. `IndentWidth'.  IS-NUM is a
boolean that should be set to 1 if the option is numeric,
otherwise assumed alphabetic."
    (if is-num
        (let ((primary-match (s-match (concat "^" field ":[ \t]*[0-9]+") config-str)))
          (if primary-match
              (string-to-number (car (s-match "[0-9]+" (car primary-match))))
            0))
      (let ((primary-match (s-match (concat "^" field ":[ \t]*[A-Za-z]+") config-str)))
        (if primary-match
            (car (s-match "[A-Za-z]+$" (car primary-match)))
          ""))))
  :hook (c-mode-common . (lambda ()
                           (let* ((clang-format-config
                                   (shell-command-to-string "clang-format -dump-config"))
                                  (c-offset (get-clang-format-option clang-format-config "IndentWidth" t))
                                  (tabs-str (get-clang-format-option clang-format-config "UseTab" nil))
                                  (base-style
                                   (get-clang-format-option clang-format-config "BasedOnStyle" nil)))
                             (progn
                               (if (> c-offset 0)
                                   (setq-local c-basic-offset c-offset)
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style))
                                            (setq-local c-basic-offset 2))
                                           ((equal "WebKit" base-style)
                                            (setq-local c-basic-offset 4)))))
                               (if (not (equal "" tabs-str))
                                   (if (not (string-equal "Never" tabs-str))
                                       (setq-local indent-tabs-mode t)
                                     (setq-local indent-tabs-mode nil))
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style)
                                                (equal "WebKit" base-style))
                                            (setq-local indent-tabs-mode nil))))))))))

(defun directory-candidate (&rest directories)
  "Return existing directory which first match."
  (seq-find #'file-directory-p directories))

;; Set up org
(use-package org
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
  (org-todo-keywords '((sequence "TODO" "REVIEW" "DONE")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)))

;; org-mode specific hooks
(defun sengelha/org-mode-hook ()
  (display-fill-column-indicator-mode 1))
(add-hook 'org-mode-hook 'sengelha/org-mode-hook)

;; Setup org-journal for journaling
(use-package org-journal
  :ensure t
  :bind
  ("C-c o j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir (directory-candidate "~/proj/github/sengelha/org-mode/journal" "D:\\proj\\github\\sengelha\\org-mode\\journal"))
  (org-journal-date-format "%A, %d %B %Y"))
  
;; Setup org-roam for personal knowledge base
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

;; Setup org-download
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;; Use deft for searching and filtering
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(provide '.emacs)
;;; .emacs ends here
