(provide 'setup-convenience)

;; ---------------------------- ;;
;; GROUP: Convenience -> Revert ;;
;; ---------------------------- ;;
;; update any change made on file to the current buffer
(global-auto-revert-mode)

;; --------------------------------------------------------------------------------------------------------------------------------------- ;;
;; GROUP: Convenience -> Hippe Expand                                                                                                      ;;
;; hippie-expand is a better version of dabbrev-expand.                                                                                    ;;
;; While dabbrev-expand searches for words you already types, in current;; buffers and other buffers, hippie-expand includes more sources, ;;
;; such as filenames, klll ring...                                                                                                         ;;
;; --------------------------------------------------------------------------------------------------------------------------------------- ;;
(global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev ;; Try to expand word "dynamically", searching the current buffer.
   try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
   try-complete-file-name ;; Try to complete text as a file name.
   try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
   try-expand-list ;; Try to complete the current line to an entire line in the buffer.
   try-expand-line ;; Try to complete the current line to an entire line in the buffer.
   try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
   try-complete-lisp-symbol) ;; Try to complete word as an Emacs Lisp symbol.
 )

;; ----------------------------- ;;
;; GROUP: Convenience -> HL Line ;;
;; ----------------------------- ;;
(global-hl-line-mode)

;; ----------------------------- ;;
;; GROUP: Convenience -> Ibuffer ;;
;; ----------------------------- ;;
(setq ibuffer-use-other-window t) ;; always display ibuffer in another window

;; --------------------------- ;;
;; GROUP: Convenience -> Linum ;;
;; --------------------------- ;;
(add-hook 'prog-mode-hook 'linum-mode) ;; enable linum only in programming modes

;; --------------------------------------------------------------------- ;;
;; GROUP: Convenience -> Whitespace                                      ;;
;; whenever you create useless whitespace, the whitespace is highlighted ;;
;; --------------------------------------------------------------------- ;;
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; ------------------------------ ;;
;; GROUP: Convenience -> Windmove ;;
;; easier window navigation       ;;
;; ------------------------------ ;;
(windmove-default-keybindings)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'ed 'eval-defun)
(defalias 'wsm 'whitespace-mode)        ; minor modes

;;set key for ibuffer
;;(global-set-key (kbd "C-x C-b") 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

;; -------------------------------------------- ;;
;; Package: expand-region                       ;;
;;                                              ;;
;; GROUP: Convenience -> Abbreviation -> Expand ;;
;; -------------------------------------------- ;;
(prelude-require-package 'expand-region)
(require 'expand-region)
(use-package expand-region
  :ensure t
  :bind (("M-m" . er/expand-region))
  )

;; ------------------- ;;
;; PACKAGE: ibuffer-vc ;;
;; ------------------- ;;

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modfified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

;; ---------------------------------------------------------------- ;;
;;  PACKAGE: rebox2                                                 ;;
;;  GROUP: Convenience -> boxing regions 10 = simple, 20 = rounded, ;;
;;  30 or 40 = starred                                              ;;
;; ---------------------------------------------------------------- ;;
(prelude-require-package 'rebox2)
(require 'rebox2)
(use-package rebox2
  :ensure t
  :commands (rebox-dwim rebox-cycle)
  :init
  :config
  (bind-key [(meta q)] 'rebox-dwim)
  (bind-key [(shift meta q)] 'rebox-cycle)
  (setq rebox-style-loop '(21 23 31 33 41 43))
  ;; (setq rebox-style-loop '(13 15 23 25 33 35 43 45))
  )

;; -------------------------------- ;;
;; PACKAGE: workgroups2             ;;
;;                                  ;;
;; GROUP: Convenience -> Workgroups ;;
;; -------------------------------- ;;
;; (prelude-require-package 'workgroups2)
;; (require 'workgroups2)
;; (use-package workgroups2
;;   :init
;;   (setq workgroups-mode 1)
;;   )

;; ---------------- ;;
;; PACKAGE: neotree ;;
;; ---------------- ;;
(prelude-require-package 'neotree)
(require 'neotree)
(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :init
  :config
  )

;; -------------------- ;;
;; PACKAGE: sr-speedbar ;;
;; -------------------- ;;
(prelude-require-package 'sr-speedbar)
(require 'sr-speedbar)
(use-package sr-speedbar
  :ensure t
  :config
  (setq sr-speedbar-toggle t)
  (setq sr-speedbar-skip-other-window-p t)
  )

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
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

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
  (beacon-mode t)
  )

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
  (which-key-mode)
  )

;; ---------------- ;;
;; CLANG formatting ;;
;; ---------------- ;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(prelude-require-package 'clang-format)
(require 'clang-format)
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-region))
  )

;; ----------------------------- ;;
;; PACKAGE: modern-cpp-font-lock ;;
;; ----------------------------- ;;
(prelude-require-package 'modern-cpp-font-lock)
(require 'modern-cpp-font-lock)
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (setq modern-c++-font-lock-global-mode t)
  :hook
  (c++-mode-hook . modern-c++-font-lock-mode)
  (c-mode-common-hook . modern-c++-font-lock-mode)
  )

;; -------------------- ;;
;; insert date and time ;;
;; -------------------- ;;
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       ;; (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

(global-set-key "\C-c\C-d" 'insert-current-date-time)
(global-set-key "\C-c\C-t" 'insert-current-time)
