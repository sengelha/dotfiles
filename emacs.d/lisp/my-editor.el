(setq make-backup-files nil)           ; Do not generate ~ files
(setq-default indent-tabs-mode nil)    ; Use space for indentation by default
(setq-default tab-width 4)             ; Default tab width when not overriden by modes
(setq-default require-final-newline t) ; Always require newline at end of file

; Global character encoding
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(provide 'my-editor)
;;; my-editor.el ends here
