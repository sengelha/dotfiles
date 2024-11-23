;;; -*- Mode: Emacs-Lisp -*-

;; User Interface
(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breathing room
(menu-bar-mode 1)     ; Enable the menu bar (on Mac it doesn't take any space)

(setq visible-bell t) ; Set up the visible bell
(setq column-number-mode t)
(setq line-number-mode t)
(global-display-line-numbers-mode t)

(require 'cl-lib)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(set-face-attribute 'default nil :font (font-candidate '"FiraCode-14:weight=normal" "Consolas-14:weight=normal" "DejaVu Sans Mono-14:weight=normal" "Menlo-18:weight=normal"))

;; Add homebrew to PATH
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

;; Keybindings
(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(end)] 'end-of-line)
(global-set-key [(control tab)] 'bury-buffer)

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

;; Company mode (code completion framework)
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; LSP mode (IDE)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t))

;; Projectile for project interaction
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/proj/github/sengelha")
    (setq projectile-project-search-path '("~/proj/github/sengelha")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Git
(use-package magit)

;; Vertico (autocompletion)
(use-package vertico
  :init
  (vertico-mode))

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

;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil) ; Query for master file
  (setq safe-local-variable-values '((TeX-command-extra-options . "-shell-escape")))
  )

;; Markdown
(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :custom-face
  (markdown-code-face ((t (:foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)))))

;; Flymake
(use-package flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; C/C++
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; C#
(use-package csharp-mode) ;; C# mode is part of emacs as of emacs 29 but we still use emacs 27 in some places
(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  )

;; CMake
(use-package cmake-mode)

;; YAML
(use-package yaml-mode)

;; Makefile mode
(require 'make-mode)

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
