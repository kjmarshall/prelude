(provide 'setup-faces-and-ui)

;; you won't need any of the bar thingies
;; turn it off to save screen estate
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                                  "%b"))))

;; change font to Inconsolata for better looking text
;; remember to install the font Inconsolata first
;; (setq default-frame-alist '((font . "Inconsolata-14")))
(setq default-frame-alist '((font . "Dejavu Sans Mono-18")))
;; set italic font for italic face, since Emacs does not set italic
;; face automatically
(set-face-attribute 'italic nil
                    :family "Dejavu Sans Mono-Italic")

;; -------------------------------- ;;
;; PACKAGE: highlight-numbers       ;;
;;                                  ;;
;; GROUP: Faces -> Number Font Lock ;;
;; -------------------------------- ;;
(prelude-require-package 'highlight-numbers)
(require 'highlight-numbers)
(use-package highlight-numbers
  :ensure t
  :hook ( prog-mode . highlight-numbers-mode )
  )

;; ------------------------- ;;
;; PACKAGE: highlight-symbol ;;
;;                           ;;
;; GROUP:                    ;;
;; ------------------------- ;;
(prelude-require-package 'highlight-symbol)
(require 'highlight-symbol)
(use-package highlight-symbol
  :ensure t
  :init
  (setq highlight-symbol-idle-delay 0.2
        highlight-symbol-on-navigation-p t)
  :hook (( prog-mode . highlight-symbol-mode)
         ( org-mode . highlight-symbol-mode ))
  :bind (([(control shift mouse-1)] .
          (lambda (event)
            (interactive "e")
            (goto-char (posn-point (event-start event)))
            (highlight-symbol-at-point)))
         ("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev) )
  :config
  (highlight-symbol-nav-mode)
  )

;; ------------------------------ ;;
;; PACKAGE: color-theme-solarized ;;
;; ------------------------------ ;;
(prelude-require-package 'grandshell-theme)
(require 'grandshell-theme)
(load-theme 'grandshell t)
;; (prelude-require-package 'color-theme-sanityinc-tomorrow)
;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-night t)
