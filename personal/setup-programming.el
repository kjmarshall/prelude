(provide 'setup-programming)
;; GROUP: Programming -> Languages -> C

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "stroustrup") ;; BSD/Allman brackets
(setq c-basic-offset 2)      ;; 4-space indent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Gdb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)          ; Non-nil means display source file containing the main routine at startup

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Compilation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation from Emacs
;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation setup ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)
;; (setq compilation-disable-input nil)
;; (setq compilation-scroll-output t)
;; (setq mode-compile-always-save-buffer-p t)
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;; setup compilation-mode used by `compile' command
(require 'compile)
(defun alexott/compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive)
  (save-some-buffers t)
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
         (current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (if prj
        (project-compile-project prj)
      (compile compile-command))))
(global-set-key [f9] 'alexott/compile)

(setq compilation-ask-about-save nil          ; Just save before compiling
      compilation-always-kill t               ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; GROUP: Programming -> Tools -> Makefile
;; takenn from prelude-c.el:48: https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

;; GROUP: Programming -> Tools -> Ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; make c++ mode apply to .hpp and .h header files
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;; ------------------------------------ ;;
;; PACKAGE: magit                       ;;
;;                                      ;;
;; GROUP: Programming -> Tools -> Magit ;;
;; ------------------------------------ ;;
(prelude-require-package 'magit)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(set-default 'magit-stage-all-confirm nil)
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(global-unset-key (kbd "C-x g"))
(global-set-key (kbd "C-x g h") 'magit-log)
(global-set-key (kbd "C-x g f") 'magit-file-log)
(global-set-key (kbd "C-x g b") 'magit-blame-mode)
(global-set-key (kbd "C-x g m") 'magit-branch-manager)
(global-set-key (kbd "C-x g c") 'magit-branch)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g r") 'magit-reflog)
(global-set-key (kbd "C-x g t") 'magit-tag)

;; --------------------------------------- ;;
;; PACKAGE: flycheck                       ;;
;;                                         ;;
;; GROUP: Programming -> Tools -> Flycheck ;;
;; --------------------------------------- ;;
(prelude-require-package 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ---------------------------------------------------------- ;;
;; PACKAGE: flycheck-tip                                      ;;
;;                                                            ;;
;; GROUP: Flycheck Tip, but just consider it part of Flycheck ;;
;; ---------------------------------------------------------- ;;
(prelude-require-package 'flycheck-tip)
(require 'flycheck-tip)
;; (flycheck-tip-use-timer 'verbose)


;; ------------------ ;;
;; PACKAGE: cuda-mode ;;
;;                    ;;
;; GROUP: Programming ;;
;; ------------------ ;;
(prelude-require-package 'cuda-mode)
(require 'cuda-mode)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
