(provide 'setup-help)

;; -------------------- ;;
;; GROUP: Help -> Info+ ;;
;; -------------------- ;;
;; (prelude-require-package 'info+)
;; (require 'info+)

;; -------------------------------- ;;
;; GROUP: Help -> discover-my-major ;;
;; -------------------------------- ;;
(prelude-require-package 'discover-my-major)
(require 'discover-my-major)
;; A quick major mode help with discover-my-major
(global-unset-key (kbd "C-h h"))        ; original "C-h h" displays "hello world" in different languages
(define-key 'help-command (kbd "h m") 'discover-my-major)

;; -------------- ;;
;; Package: help+ ;;
;;                ;;
;; GROUP: Help    ;;
;; -------------- ;;
;; (prelude-require-package 'help+)
;; (require 'help+)

;; ------------------ ;;
;; Package: help-fns+ ;;
;;                    ;;
;; GROUP: Help        ;;
;; ------------------ ;;
;; (prelude-require-package 'help-fns+)
;; (require 'help-fns+)

;; ------------------- ;;
;; Package: help-mode+ ;;
;;                     ;;
;; GROUP: Help         ;;
;; ------------------- ;;
;; (prelude-require-package 'help-mode+)
;; (require 'help-mode+)
