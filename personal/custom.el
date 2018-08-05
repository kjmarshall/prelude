;; -------------------- ;;
;; PACKAGE: use-package ;;
;; -------------------- ;;
(prelude-require-package 'use-package)
(require 'use-package)

;; ------------------------------------- ;;
;; PACKAGE: ein (emacs ipython notebook) ;;
;; ------------------------------------- ;;
(prelude-require-package 'ein)
(require 'ein)

;; -------------------------------------------- ;;
;; PACKAGE: projectile additional configuration ;;
;; -------------------------------------------- ;;
(setq projectile-enable-caching t)

;; --------------------------------------------------------------- ;;
;; Rainbow Delimiters -  have delimiters be colored by their depth ;;
;; --------------------------------------------------------------- ;;
(prelude-require-package 'rainbow-delimiters)
(require 'rainbow-delimiters)
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; ----------------------------------------------------------------- ;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling ;;
;;               the goal is to make it easy to find the cursor      ;;
;; ----------------------------------------------------------------- ;;
(prelude-require-package 'beacon)
(require 'beacon)
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;; ------------------------------------------------------------ ;;
;; which-key: when you pause on a keyboard shortcut it provides ;;
;;             suggestions in a popup buffer                    ;;
;; ------------------------------------------------------------ ;;
(prelude-require-package 'which-key)
(require 'which-key)
(use-package which-key
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;; ---------------- ;;
;; CLANG formatting ;;
;; ---------------- ;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(prelude-require-package 'clang-format)
(require 'clang-format)
(global-set-key (kbd "C-c C-f") 'clang-format-region)

;; ----------------------------- ;;
;; PACKAGE: modern-cpp-font-lock ;;
;; ----------------------------- ;;
(prelude-require-package 'modern-cpp-font-lock)
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;; ----------------------------- ;;
;; PACKAGE: ycmd code completion ;;
;; ----------------------------- ;;
;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; cloned ycmd directory
;; recall list syntax where backquote evaluates elemnts of a list
;; and , tells that the list element is not constant
(prelude-require-package 'ycmd)
(require 'ycmd)
(prelude-require-package 'company-ycmd)
(require 'ycmd)
(prelude-require-package 'flycheck-ycmd)
(require 'flycheck-ycmd)
(defvar my:ycmd-server-command `("python3" ,(file-truename "~/.emacs.d/external/ycmd/ycmd/")))
(defvar my:ycmd-extra-conf-whitelist `( ,(file-truename "~/.emacs.d/ycm_global_extra_conf.py") ) )
(defvar my:ycmd-global-config (file-truename "~/.emacs.d/ycm_global_extra_conf.py") )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;; https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up YouCompleteMe for emacs:
;; https://github.com/Valloric/ycmd
;; https://github.com/abingham/emacs-ycmd
(defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
(if (not my:python-location)
    (message
     "Could not start YouCompleteMeDaemon because the python executable could
not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
appropriately in ~/.emacs.el.\n" (nth 0 my:ycmd-server-command)))
(if (not (file-directory-p (nth 1 my:ycmd-server-command)))
    (message "Could not YouCompleteMeDaemon because the specified directory does
not exist.\nSpecified directory is: '%s'
Please set my:ycmd-server-command appropriately in ~/.emacs.el.\n"
             (nth 1 my:ycmd-server-command)))
(if (and my:python-location
         (file-directory-p (nth 1 my:ycmd-server-command)))
    (use-package ycmd
      :ensure t
      :init
      (eval-when-compile
        ;; Silence missing function warnings
        (declare-function global-ycmd-mode "ycmd.el"))
      (add-hook 'after-init-hook #'global-ycmd-mode)
      :config
      (progn
        (set-variable 'ycmd-server-command my:ycmd-server-command)
        (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
        (set-variable 'ycmd-global-config my:ycmd-global-config)
        (setq ycmd-force-semantic-completion t)
        (use-package company-ycmd
          :ensure t
          :init
          (eval-when-compile
            ;; Silence missing function warnings
            (declare-function company-ycmd-setup "company-ycmd.el"))
          :config
          (company-ycmd-setup)
          )

        (use-package flycheck-ycmd
          :ensure t
          :init
          (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
          )

        ;; Add displaying the function arguments in mini buffer using El Doc
        (require 'ycmd-eldoc)
        (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
        )
      )
  )

;; --------------------- ;;
;; PACKAGE: company-jedi ;;
;; --------------------- ;;
;; Setup loading company-jedi for python completion
;; This requires running jedi:install-server the first time
(prelude-require-package 'company-jedi)
(require 'company-jedi)
(use-package company-jedi
  :ensure t
  :after python
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;; ----------------- ;;
;; PACKAGE: ess-view ;;
;; ----------------- ;;
(prelude-require-package 'ess-view)
(require 'ess-view)

;; -------------- ;;
;; PACKAGE: eldoc ;;
;; -------------- ;;
(prelude-require-package 'eldoc)
(require 'eldoc)
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ycmd-eldoc company-ycmd ycmd zop-to-char zenburn-theme ws-butler which-key volatile-highlights vlf use-package undo-tree sr-speedbar smartrep smartparens shell-pop recentf-ext rebox2 rainbow-mode rainbow-delimiters operate-on-number nyan-mode neotree move-text modern-cpp-font-lock magit json-mode imenu-anywhere hl-todo highlight-symbol highlight-numbers helm-projectile helm-descbinds helm-ag guru-mode grandshell-theme gitignore-mode gitconfig-mode git-timemachine gist geiser flycheck-tip expand-region ess-view elisp-slime-nav ein editorconfig easy-kill duplicate-thing dtrt-indent discover-my-major diminish diff-hl cuda-mode crux counsel-etags company-jedi company-auctex company-anaconda clean-aindent-mode clang-format cdlatex browse-kill-ring beacon anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
