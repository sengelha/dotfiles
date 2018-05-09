;;; -*- Mode: Emacs-Lisp -*-

(require 'cl)

;;{{{ Variables describing environment Emacs is running in

(defvar running-xemacs (string-match "XEmacs\\Lucid" emacs-version))
(defvar running-on-windows (eq 'windows-nt system-type))
(defvar running-on-unix (not running-on-windows))
(defvar mule-present (featurep 'mule))
(defvar emacs-flavor
  (concat (if running-xemacs "xemacs" "gnuemacs") "."
	  (if mule-present "mule" "nomule") "-"
	  (int-to-string emacs-major-version) "."
	  (int-to-string emacs-minor-version)))

;;}}}
;;{{{ Customization

;; Store customization information in file specific for emacs version
(setq custom-file
      (concat (expand-file-name "~/.elisp/custom.")
	      emacs-flavor
	      ".el"))

;; Load customization information if it exists
(if (file-exists-p custom-file)
    (load custom-file t))

;; Make XEmacs stop trying to migrate configuration file
(setq load-home-init-file t)

;;}}}
;;{{{ Load Path Setup

(let* ((my-lisp-directory (expand-file-name "~/.elisp"))
       (my-load-path (list my-lisp-directory)))
  (setq my-load-path
	(append (directory-files my-lisp-directory t nil t)
		my-load-path))
  (setq my-load-path (delete (concat my-lisp-directory "/.") my-load-path))
  (setq my-load-path (delete (concat my-lisp-directory "/..") my-load-path))
  (delete-if-not 'file-directory-p my-load-path)
  (setq load-path (append load-path my-load-path)))

;;}}}
;;{{{ Load packages required during initialization

(require 'cc-mode)

;;}}}
;;{{{ Load local packages

;;}}}
;;{{{ Mode hooks

(setq default-major-mode 'text-mode)

;; Override auto-mode-alist based on certain file extensions
(setq auto-mode-alist
      (append '(("\\.tex$" . LaTeX-mode)) ; I never use raw TeX
	      auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.nes$" . hexl-mode))
	      auto-mode-alist))

(require 'cmake-mode)
(require 'cmake-font-lock)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode))
	      '(("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(setq c-default-style "bsd")
(add-hook 'c-mode-common-hook '(lambda()
				 (setq c-basic-offset 4)
				 (setq indent-tabs-mode nil)
				 ))
(add-hook 'text-mode-hook '(lambda()
			     (auto-fill-mode 1)))
(add-hook 'xml-mode-hook '(lambda()
			    (auto-fill-mode -1)
			    (setq indent-tabs-mode nil)
			    ))

;;}}}
;;{{{ Folding

(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)
(folding-add-to-marks-list 'python-mode "# {{{ " "# }}}" nil t)

;;}}}
;;{{{ Ctypes

(require 'ctypes)
(setq ctypes-write-types-at-exit t)
(ctypes-read-file nil nil t t)
(ctypes-auto-parse-mode 1)

;;}}}
;;{{{ Guess-Offset

(require 'guess-offset)

;;}}}
;;{{{ Fonts and Colors

(defun my-dpi ()
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res) (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx) sizex) 25.4))))

(defun my-preferred-font-size ()
  (let ( (dpi (my-dpi)) )
    (cond
     ((< dpi 110) 10)
     ((< dpi 130) 11)
     ((< dpi 160) 12)
     (t 12))))

(defvar my-preferred-font-size (my-preferred-font-size))

(defvar my-regular-font
  (cond
   ((eq window-system 'x)
    (let ((candidate-font (format "DejaVu Sans Mono-%d:weight=normal" my-preferred-font-size)))
      (if (and (fboundp 'find-font) (find-font (font-spec :name candidate-font)))
	  candidate-font "7x14")))
   ((eq window-system 'w32)
    (let ((candidate-font (format "Consolas-%d" my-preferred-font-size)))
      (if (and (fboundp 'find-font) (find-font (font-spec :name candidate-font)))
	  candidate-font (format "Courier New-%d" my-preferred-font-size))))))

; Set the font for this frame and all new frames to be my-regular-font
(cond
 ((or (eq window-system 'x) (eq window-system 'w32))
  (set-frame-font my-regular-font)
  (add-to-list 'default-frame-alist (cons 'font my-regular-font))))

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1) ; Emacs
  (setq font-lock-auto-fontify t)) ; XEmacs

(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

;;}}}
;;{{{ Whitespace

(setq require-final-newline t)
(setq-default show-trailing-whitespace t)
(require 'whitespace)

;;}}}
;;{{{ Parentheses

(require 'paren)
(if (fboundp 'paren-set-mode)
    (paren-set-mode 'paren) ; XEmacs
  (show-paren-mode 1)) ; Emacs

;;}}}
;;{{{ Toolbar

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1) ; Emacs
  (set-specifier default-toolbar-visible-p nil)) ; XEmacs

;;}}}
;;{{{ Modeline

(setq column-number-mode t)
(setq line-number-mode t)

;;}}}
;;{{{ Loading/Saving Files

(setq auto-save-default nil) ; Do not auto-save buffer
(setq make-backup-files nil)
(if (fboundp 'global-auto-revert-mode)
    (global-auto-revert-mode 1)) ; Automatically revert buffer if changed

;;}}}
;;{{{ Keybindings

(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(end)] 'end-of-line)
(global-set-key [(f1)] 'manual-entry)
(global-set-key [(f2)] 'undo)
(global-set-key [(control tab)] 'bury-buffer)

;; C-mode-specific keybindings
(define-key c-mode-base-map [(f5)] 'compile)
(define-key c-mode-base-map [(return)] 'newline-and-indent)

;;}}}
;;{{{ Mouse

(require 'mwheel)

;;}}}
;;{{{ Locally-defined LISP functions

(defun kill-all-buffers ()
  (interactive)
  (setq buf_list (buffer-list))
  (while buf_list
    (kill-buffer (car buf_list))
    (setq buf_list (cdr buf_list))))

;;}}}
;;{{{ Speedbar

(when window-system          ; start speedbar if we're using a window system
  (require 'speedbar)
  (add-to-list 'speedbar-frame-parameters '(width . 40))
  (setq speedbar-show-unknown-files t)
  (speedbar t)
)

;;}}}
