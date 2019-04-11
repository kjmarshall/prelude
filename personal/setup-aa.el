;; -------------------- ;;
;; PACKAGE: use-package ;;
;; -------------------- ;;
(prelude-require-package 'use-package)
(require 'use-package)

;; ---------------------------- ;;
;; PACKAGE: auto-package-update ;;
;; ---------------------------- ;;
;; Auto update packages once a week
;; (prelude-require-package 'auto-package-update)
;; (use-package auto-package-update
;;   :ensure t
;;   :commands (auto-package-update-maybe)
;;   :init
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe)
;;   (add-hook 'auto-package-update-before-hook
;;             (lambda () (message "I will update packages now")))
;;   )

;; ---------- ;;
;; PACKAGE: s ;;
;; ---------- ;;
;; emacs string manipulation library
(prelude-require-package 's)
(use-package s
  :ensure t
  )

;; ------------------------------------- ;;
;; GROUP: Applications -> c mode editing ;;
;; ------------------------------------- ;;
(prelude-require-package 'cl);
(require 'cl)
(use-package cl
  :ensure t
  )
(prelude-require-package 'cc-mode)
(require 'cc-mode)
(use-package cc-mode
  :ensure t
  )

;; ---------------- ;;
;; PACKAGE: Prelude ;;
;; ---------------- ;;
;; modifications to disable whitespace cleanup
(setq prelude-whitespace nil)
(setq prelude-clean-whitespace-on-save nil)

;; -------------------------------------------- ;;
;; PACKAGE: projectile additional configuration ;;
;; -------------------------------------------- ;;
(prelude-require-package 'projectile)
(require 'projectile)
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  )

;; --------------------------- ;;
;; Package: yasnippet          ;;
;;                             ;;
;; GROUP: Editing -> Yasnippet ;;
;; --------------------------- ;;
(prelude-require-package 'yasnippet)
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  )
(use-package yasnippet-snippets
  :ensure t
  :commands (yas-reload-all)
  :config
  (yas-reload-all)
  :after (yasnippet)
  )
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
;; (use-package company-yasnippet
;;   :ensure t
;;   :bind ("C-M-y" . company-yasnippet)
;;   :after (yasnippet)
;;   )

;; ---------------- ;;
;; PACKAGE: Company ;;
;; ---------------- ;;
;; Set up code completion with company
(prelude-require-package 'company)
(require 'company)
(use-package company
  :ensure t
  :init
  (global-company-mode 1)
  :hook ( after-init . global-company-mode )
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (define-key c-mode-map [C-(tab)] 'company-complete)
  (define-key c++-mode-map [C-(tab)] 'company-complete)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  ;; (setf company-backends '())
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-dabbrev)
  (setq company-transformers '(company-sort-by-backend-importance))
  :bind ("C-;" . company-complete-common)
  )

;; -------------------------- ;;
;; PACKAGE: company-c-headers ;;
;; -------------------------- ;;
(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :after (company)
  )

;; ;; ---------------------- ;;
;; ;; PACKAGE: company-irony ;;
;; ;; ---------------------- ;;
;; (use-package company-irony
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-irony)
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   :after (company)
;;   )

;; ;; -------------------------------- ;;
;; ;; PACKAGE: company-irony-c-headers ;;
;; ;; -------------------------------- ;;
;; (use-package company-irony-c-headers
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-irony-c-headers)
;;   :after (company)
;;   )

;; -------------- ;;
;; PACKAGE: Irony ;;
;; -------------- ;;
;; Use irony for completion
;; (use-package irony
;;   :ensure t
;;   :commands irony-mode
;;   :init
;;   (add-hook 'c++-mode-hook #'irony-mode)
;;   (add-hook 'c-mode-hook #'irony-mode)
;;   (add-hook 'objc-mode-hook #'irony-mode)
;;   :config
;;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;   ;; irony-mode's buffers by irony-mode's function
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook #'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
;;   )

;; ---------------- ;;
;; PACKAGE: Malinka ;;
;; ---------------- ;;
;; (prelude-require-package 'malinka)
;; (require 'malinka)
;; (use-package malinka
;;   :ensure t
;;   :hook ((c-mode-common . malinka-mode))
;;   )
;; (malinka-define-project
;;  :name "ImagingTools"
;;  :root-directory "/nfs/orto/proj/tapeout/cit_dev26/kmarshal/ImagingTools"
;;  :build-directory "/nfs/orto/proj/tapeout/cit_dev26/kmarshal/ImagingTools"
;;  :configure-cmd "cmake . -DCMAKE_BUILD_TYPE=Debug -DHEADLESS=1"
;;  :compile-cmd "gmake cc=gcc type=release ext=none -sj24 redist"
;;  :test-cmd "" )

;; ----------------------------- ;;
;; PACKAGE: ycmd code completion ;;
;; ----------------------------- ;;
;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; cloned ycmd directory
;; recall list syntax where backquote evaluates elements of a list
;; and , tells that the list element is not constant
(prelude-require-package 'ycmd)
(require 'ycmd)
(prelude-require-package 'company-ycmd)
(require 'company-ycmd)
(prelude-require-package 'flycheck-ycmd)
(require 'flycheck-ycmd)
;; (defvar my:ycmd-server-command `("/nfs/pdx/home/kmarshal/km-nfs/python-3.7.0/bin/python3" ,(file-truename "~/.emacs.d/external/ycmd/ycmd/")))
(defvar my:ycmd-server-command `( ,(getenv "EMACS_YCMD_PYTHON") ,(file-truename "~/.emacs.d/external/ycmd/ycmd/")))
(defvar my:ycmd-extra-conf-whitelist `( ,(file-truename "~/.emacs.d/ycm_configs/*")
                                        ,(file-truename "~/km-nfs/ImagingTools/.ycm_extra_conf.py") ) )
(defvar my:ycmd-global-config (file-truename "~/.emacs.d/ycm_global_extra_conf.py") )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package: ycmd (YouCompleteMeDaemon)
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
      :commands (ycmd-mode ycmd-eldoc-setup)
      :init
      ;; (eval-when-compile
      ;;   ;; Silence missing function warnings
      ;;   (declare-function global-ycmd-mode "ycmd.el"))
      ;; (add-hook 'after-init-hook #'global-ycmd-mode)
      (add-hook 'c-mode-common-hook #'ycmd-mode)
      (add-hook 'c-mode-hook #'ycmd-mode)
      (add-hook 'c++-mode-hook #'ycmd-mode)
      (add-hook 'python-mode-hook #'ycmd-mode)
      (add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup)
      (setq ycmd-force-semantic-completion t)
      :config
      (progn
        (set-variable 'ycmd-server-command my:ycmd-server-command)
        (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
        (set-variable 'ycmd-global-config my:ycmd-global-config)
        (use-package company-ycmd
          :ensure t
          :init
          (eval-when-compile
            ;; Silence missing function warnings
            (declare-function company-ycmd-setup "company-ycmd.el"))
          (company-ycmd-setup)
          :config
          (add-to-list 'company-backends 'company-ycmd)
          )

        (use-package flycheck-ycmd
          :ensure t
          :commands flycheck-ycmd-setup
          :init
          (add-hook 'c-mode-common-hook #'flycheck-ycmd-setup)
          )

        ;; Add displaying the function arguments in mini buffer using El Doc
        ;; (use-package ycmd-eldoc
        ;;   :ensure t
        ;;   :commands ycmd-eldoc-setup
        ;;   :init
        ;;   (add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup)
        ;;   )
        )
      )
  )

;; ----------------- ;;
;; PACKAGE: ess-view ;;
;; ----------------- ;;
;; (prelude-require-package 'ess-view)
;; (require 'ess-view)
;; (use-package ess-view
;;   :ensure t
;;   :init
;;   :config
;;   )

;; -------------- ;;
;; PACKAGE: eldoc ;;
;; -------------- ;;
(prelude-require-package 'eldoc)
(require 'eldoc)
(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup)
  )

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
  :bind (("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories "RedistSettings")
  (add-to-list 'counsel-etags-ignore-directories "Midi")
  (add-to-list 'counsel-etags-ignore-directories "Scripts")
  (add-to-list 'counsel-etags-ignore-directories "WebApplications")
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-directories "redist")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  ;; (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  ;; (add-hook
  ;;  'prog-mode-hook
  ;;  (lambda () (add-hook 'after-save-hook
  ;;                       (lambda ()
  ;;                         (counsel-etags-virtual-update-tags))))
  ;;  )

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  ;; (defun my-scan-dir (src-dir &optional force)
  ;;   "Create tags file from SRC-DIR. \
  ;;    If FORCE is t, the commmand is executed without \
  ;;    checking the timer."
  ;;   (let* ((find-pg (or
  ;;                    counsel-etags-find-program
  ;;                    (counsel-etags-guess-program "find")))
  ;;          (ctags-pg (or
  ;;                     counsel-etags-tags-program
  ;;                     (format "%s -e -L" (counsel-etags-guess-program
  ;;                                         "ctags"))))
  ;;          (default-directory src-dir)
  ;;          ;; run find&ctags to create TAGS
  ;;          (cmd (format
  ;;                "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
  ;;                find-pg
  ;;                (mapconcat
  ;;                 (lambda (p)
  ;;                   (format "-iwholename \"*%s*\"" p))
  ;;                 counsel-etags-ignore-directories " -or ")
  ;;                counsel-etags-max-file-size
  ;;                (mapconcat (lambda (n)
  ;;                             (format "-not -name \"%s\"" n))
  ;;                           counsel-etags-ignore-filenames " ")
  ;;                ctags-pg))
  ;;          (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
  ;;          (doit (or force (not (file-exists-p tags-file)))))
  ;;     ;; always update cli options
  ;;     (when doit
  ;;       (message "%s at %s" cmd default-directory)
  ;;       (shell-command cmd)
  ;;       (visit-tags-table tags-file t)
  ;;       )
  ;;     )
  ;;   )

  ;; (setq counsel-etags-update-tags-backend
  ;;       (lambda ()
  ;;         (interactive)
  ;;         (let* ((tags-file (counsel-etags-locate-tags-file)))
  ;;           (when tags-file
  ;;             (my-scan-dir (file-name-directory tags-file) t)
  ;;             (run-hook-with-args
  ;;              'counsel-etags-after-update-tags-hook tags-file)
  ;;             (unless counsel-etags-quiet-when-updating-tags
  ;;               (message "%s is updated!" tags-file))))
  ;;         )
  ;;       )
  ;; (eval-after-load 'counsel-etags
  ;;   '(progn
  ;;      ;; counsel-etags-ignore-directories does NOT support wildcast
  ;;      (add-to-list 'counsel-etags-ignore-directories "RedistSettings")
  ;;      (add-to-list 'counsel-etags-ignore-directories "Midi")
  ;;      (add-to-list 'counsel-etags-ignore-directories "Scripts")
  ;;      (add-to-list 'counsel-etags-ignore-directories "WebApplications")
  ;;      (add-to-list 'counsel-etags-ignore-directories "build")
  ;;      (add-to-list 'counsel-etags-ignore-directories "redist")
  ;;      ;; counsel-etags-ignore-filenames supports wildcast
  ;;      (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  ;;      (add-to-list 'counsel-etags-ignore-filenames "*.json")))
  )

;; ------------------ ;;
;; PACKAGE: flycheck ;;
;; ------------------ ;;
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
;; (use-package flycheck-irony
;;   :ensure t
;;   :commands flycheck-irony-setup
;;   :init
;;   (add-hook 'c++-mode-hook 'flycheck-irony-setup)
;;   (add-hook 'c-mode-hook 'flycheck-irony-setup)
;;   )

;; ------------------------------------------- ;;
;; PACKAGE: string-inflection                  ;;
;; used for switching between different cases, ;;
;; e.g. CamelCase, lowerCamelCase,             ;;
;;      snake_case, and SCREAMING_SNAKE_CASE   ;;
;; ------------------------------------------- ;;
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

;;require section
;; (require 'setup-applications)
;; (require 'setup-communication)
;; (require 'setup-convenience)
;; (require 'setup-data)
;; (require 'setup-development)
;; (require 'setup-editing)
;; (require 'setup-environment)
;; (require 'setup-external)
;; (require 'setup-faces-and-ui)
;; (require 'setup-file-modes)
;; (require 'setup-files)
;; (require 'setup-helm)
;; (require 'setup-help)
;; (require 'setup-latex)
;; (require 'setup-local)
;; (require 'setup-programming)
;; (require 'setup-python)
;; (require 'setup-tempo-c-cpp)
;; (require 'setup-text)
