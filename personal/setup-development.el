(provide 'setup-development)

;; ----------------------------------------- ;;
;; GROUP: Development -> Extensions -> Eldoc ;;
;; ----------------------------------------- ;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
