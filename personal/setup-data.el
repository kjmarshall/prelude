(provide 'setup-data)

;; ------------------------ ;;
;; GROUP: Data -> Saveplace ;;
;; ------------------------ ;;

;; saveplace remembers your location in a file when saving files
(prelude-require-package 'saveplace)
(use-package saveplace
  :ensure t
  :config
  (setq toggle-save-place-globally 1)
  )
