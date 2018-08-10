;; -------------------- ;;
;; PACKAGE: use-package ;;
;; -------------------- ;;
(prelude-require-package 'use-package)
(require 'use-package)

;; ---------- ;;
;; PACKAGE: s ;;
;; ---------- ;;
(prelude-require-package 's)
(use-package s
  :ensure t )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(prelude-require-package 'auto-package-update)
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "I will update packages now")))
  )

;; ------------------------------------- ;;
;; PACKAGE: ein (emacs ipython notebook) ;;
;; ------------------------------------- ;;
(prelude-require-package 'ein)
(require 'ein)

;; -------------------------------------------- ;;
;; PACKAGE: projectile additional configuration ;;
;; -------------------------------------------- ;;
(require 'projectile)
(define-key projectile-mode-map projectile-keymap-prefix nil)
(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
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

;; ----------------------------- ;;
;; PACKAGE: Python Mode Settings ;;
;; ----------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))
(setq-default pdb-command-name "python -m pdb")
(prelude-require-package 'elpy)
(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :after python
  :config
  (elpy-enable)
  )

(prelude-require-package 'yapfify)
(use-package yapfify
  :ensure t
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

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

;; ---------------------- ;;
;; PACKAGE: counsel-etags ;;
;; ---------------------- ;;
;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(prelude-require-package 'counsel-etags)
(use-package counsel-etags
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
    (declare-function counsel-etags-guess-program "counsel-etags.el")
    (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
  :bind (
         ("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  (add-hook
   'prog-mode-hook
   (lambda () (add-hook 'after-save-hook
                        (lambda ()
                          (counsel-etags-virtual-update-tags))))
   )

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  (defun my-scan-dir (src-dir &optional force)
    "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
    (let* ((find-pg (or
                     counsel-etags-find-program
                     (counsel-etags-guess-program "find")))
           (ctags-pg (or
                      counsel-etags-tags-program
                      (format "%s -e -L" (counsel-etags-guess-program
                                          "ctags"))))
           (default-directory src-dir)
           ;; run find&ctags to create TAGS
           (cmd (format
                 "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                 find-pg
                 (mapconcat
                  (lambda (p)
                    (format "-iwholename \"*%s*\"" p))
                  counsel-etags-ignore-directories " -or ")
                 counsel-etags-max-file-size
                 (mapconcat (lambda (n)
                              (format "-not -name \"%s\"" n))
                            counsel-etags-ignore-filenames " ")
                 ctags-pg))
           (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
           (doit (or force (not (file-exists-p tags-file)))))
      ;; always update cli options
      (when doit
        (message "%s at %s" cmd default-directory)
        (shell-command cmd)
        (visit-tags-table tags-file t)
        )
      )
    )

  (setq counsel-etags-update-tags-backend
        (lambda ()
          (interactive)
          (let* ((tags-file (counsel-etags-locate-tags-file)))
            (when tags-file
              (my-scan-dir (file-name-directory tags-file) t)
              (run-hook-with-args
               'counsel-etags-after-update-tags-hook tags-file)
              (unless counsel-etags-quiet-when-updating-tags
                (message "%s is updated!" tags-file))))
          )
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck-ycmd
(prelude-require-package 'flycheck)
(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(prelude-require-package 'flycheck-pyflakes)
(use-package flycheck-pyflakes
  :ensure t
  :after python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'string-inflection)
(use-package string-inflection
  :ensure t
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase)
         )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'web-mode)
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'markdown-mode)
(use-package markdown-mode
  :ensure t
  :mode (".md" ".markdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting in CUDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CUDA mode so we get syntax highlighting in .cu files
(prelude-require-package 'cuda-mode)
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Powerline theme
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; powerline theme where the modes are on the right side.
;; (prelude-require-package 'powerline)
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (defun powerline-right-theme ()
;;     "Setup a mode-line with major and minor modes on the right side."
;;     (interactive)
;;     (setq-default mode-line-format
;;                   '("%e"
;;                     (:eval
;;                      (let* ((active (powerline-selected-window-active))
;;                             (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
;;                             (mode-line (if active 'mode-line 'mode-line-inactive))
;;                             (face0 (if active 'powerline-active0 'powerline-inactive0))
;;                             (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                             (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                             (separator-left (intern (format "powerline-%s-%s"
;;                                                             (powerline-current-separator)
;;                                                             (car powerline-default-separator-dir))))
;;                             (separator-right (intern (format "powerline-%s-%s"
;;                                                              (powerline-current-separator)
;;                                                              (cdr powerline-default-separator-dir))))
;;                             (lhs (list (powerline-raw "%*" face0 'l)
;;                                        (powerline-buffer-size face0 'l)
;;                                        (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
;;                                        (powerline-raw " ")
;;                                        (funcall separator-left face0 face1)
;;                                        (powerline-narrow face1 'l)
;;                                        (powerline-vc face1)))
;;                             (center (list (powerline-raw global-mode-string face1 'r)
;;                                           (powerline-raw "%4l" face1 'r)
;;                                           (powerline-raw ":" face1)
;;                                           (powerline-raw "%3c" face1 'r)
;;                                           (funcall separator-right face1 face0)
;;                                           (powerline-raw " ")
;;                                           (powerline-raw "%6p" face0 'r)
;;                                           (powerline-hud face2 face1)
;;                                           ))
;;                             (rhs (list (powerline-raw " " face1)
;;                                        (funcall separator-left face1 face2)
;;                                        (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
;;                                          (powerline-raw erc-modified-channels-object face2 'l))
;;                                        (powerline-major-mode face2 'l)
;;                                        (powerline-process face2)
;;                                        (powerline-raw " :" face2)
;;                                        (powerline-minor-modes face2 'l)
;;                                        (powerline-raw " " face2)
;;                                        (funcall separator-right face2 face1)
;;                                        ))
;;                             )
;;                        (concat (powerline-render lhs)
;;                                (powerline-fill-center face1 (/ (powerline-width center) 2.0))
;;                                (powerline-render center)
;;                                (powerline-fill face1 (powerline-width rhs))
;;                                (powerline-render rhs)))))))
;;   (powerline-right-theme)
;; )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-mode string-inflection flycheck-pyflakes yapfify elpy zop-to-char zenburn-theme yasnippet-snippets ws-butler which-key volatile-highlights vlf use-package undo-tree sr-speedbar smartrep smartparens shell-pop recentf-ext rebox2 rainbow-mode rainbow-delimiters pyvenv operate-on-number nyan-mode neotree move-text modern-cpp-font-lock markdown-mode magit json-mode imenu-anywhere hl-todo highlight-symbol highlight-numbers highlight-indentation helm-projectile helm-descbinds helm-ag guru-mode grandshell-theme gitignore-mode gitconfig-mode git-timemachine gist geiser flycheck-ycmd flycheck-tip find-file-in-project expand-region ess-view elisp-slime-nav ein editorconfig easy-kill duplicate-thing dtrt-indent discover-my-major diminish diff-hl cuda-mode crux counsel-etags company-ycmd company-jedi company-auctex company-anaconda clean-aindent-mode clang-format cdlatex browse-kill-ring beacon auto-package-update anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
