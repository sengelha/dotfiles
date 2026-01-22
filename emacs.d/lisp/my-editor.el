;; Put backup files neatly away
(let ((backup-dir "~/.emacs.d/backups")
      (auto-saves-dir "~/emacs.d/auto-saves"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(setq-default indent-tabs-mode nil)    ; Use space for indentation by default
(setq-default tab-width 4)             ; Default tab width when not overriden by modes
(setq-default require-final-newline t) ; Always require newline at end of file

; Global character encoding
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(provide 'my-editor)
;;; my-editor.el ends here
