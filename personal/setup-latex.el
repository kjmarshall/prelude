(provide 'setup-latex)

;; ----------- ;;
;; Latex stuff ;;
;; ----------- ;;
;;default compile as PDF in auctex mode
(setq TeX-PDF-mode t)
;;tex error message
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; Auctex
;; (require 'auctex)
;; (use-package auctex
;;   :ensure t
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :commands (latex-mode LaTeX-mode plain-tex-mode)
;;   :init
;;   (progn
;;     (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
;;     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;     (add-hook 'LaTeX-mode-hook #'flyspell-mode)
;;     (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
;;     (setq TeX-auto-save t
;;           TeX-parse-self t
;;           TeX-save-query nil
;;           TeX-PDF-mode t)
;;     ))

;; ;; Use company-auctex
;; (use-package company-auctex
;;   :ensure t
;;   :config
;;   (company-auctex-init)
;; )
