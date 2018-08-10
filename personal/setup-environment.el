(provide 'setup-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Environment -> Initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Environment -> Minibuffer   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (icomplete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Environment -> Minibuffer -> Savehist ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist saves minibuffer history by defaults
(setq savehist-additional-variables '(search ring regexp-search-ring) ; also save your regexp search queries
      savehist-autosave-interval 60     ; save every minute
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Environment -> Windows -> Winner ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Environment -> Mode Line    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode 1)

;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: nyan-mode                    ;;
;;                                       ;;
;; GROUOP: Environment -> Frames -> Nyan ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only turn on if a window system is available
;; this prevents error under terminal that does not support X
(prelude-require-package 'nyan-mode)
(require 'nyan-mode)
(case window-system
  ((x w32) (nyan-mode)))

;; --------------------------------------------- ;;
;; PACKAGE: golden-ratio                         ;;
;;                                               ;;
;; GROUP: Environment -> Windows -> Golden Ratio ;;
;; --------------------------------------------- ;;
;; (prelude-require-package 'golden-ratio)
;; (require 'golden-ratio)

;; (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; (defun pl/helm-alive-p ()
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))

;; ;; do not enable golden-raio in thses modes
;; (setq golden-ratio-exclude-modes '("ediff-mode"
;;                                    "gud-mode"
;;                                    "gdb-locals-mode"
;;                                    "gdb-registers-mode"
;;                                    "gdb-breakpoints-mode"
;;                                    "gdb-threads-mode"
;;                                    "gdb-frames-mode"
;;                                    "gdb-inferior-io-mode"
;;                                    "gud-mode"
;;                                    "gdb-inferior-io-mode"
;;                                    "gdb-disassembly-mode"
;;                                    "gdb-memory-mode"
;;                                    "magit-log-mode"
;;                                    "magit-reflog-mode"
;;                                    "magit-status-mode"
;;                                    "IELM"
;;                                    "eshell-mode"
;; 				   "dired-mode"
;; 				   "helm-mode"
;; 				   "ediff-mode"
;; 				   "ispell-mode"
;; 				   "ecb-mode"))

;; (golden-ratio-mode)
