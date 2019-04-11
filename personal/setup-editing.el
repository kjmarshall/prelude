(provide 'setup-editing)
(global-set-key (kbd "<return>") (kbd "RET"))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;; -------------------------- ;;
;; GROUP: Edit -> Parentheses ;;
;; -------------------------- ;;
(prelude-require-package 'paren)
(require 'paren)
(use-package paren
  :ensure t
  :init
  (setq show-paren-mode 1)
  (setq show-paren-delay 0) ;; no delay
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#ff0000")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  :config
  )

(global-hl-line-mode)                ;; Always highlight the current line
(setq-default fci-rule-column 80)    ;; Show column ruler at 80 columns
;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )

;; default to 4 visible spaces to display a tab
(setq-default tab-width 4)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode t)
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq
 kill-ring-max 5000 ; increase kill-ring capacity
 kill-whole-line t  ; if NIL, kill whole line and move the next line up
 )

;; show important whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))


;; -------------------- ;;
;; Customized functions ;;
;; -------------------- ;;
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-o") 'open-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; -------------------------------------- ;;
;; PACAKAGE: duplicate-thing              ;;
;;                                        ;;
;; GROUP: Conveninence -> Duplicate Lines ;;
;; -------------------------------------- ;;
(prelude-require-package 'duplicate-thing)
(require 'duplicate-thing)
(use-package duplicate-thing
  :ensure t
  :bind (("M-c" . duplicate-thing))
  )

;; ------------------------------------- ;;
;; Package: volatile-highlights          ;;
;;                                       ;;
;; GROUP: Editing -> Volatile Highlights ;;
;; ------------------------------------- ;;
(prelude-require-package 'volatile-highlights)
(require 'volatile-highlights)
(use-package volatile-highlights
  :ensure t
  :init
  (setq volatile-highlights-mode t)
  )

;; ------------------------- ;;
;; PACKAGE: highlight-symbol ;;
;; ------------------------- ;;
(prelude-require-package 'highlight-symbol)
(require 'highlight-symbol)
(use-package highlight-symbol
  :ensure t
  )

;; -------------------- ;;
;; Package: smartparens ;;
;; -------------------- ;;
(prelude-require-package 'smartparens)
(require 'smartparens)
(use-package smartparens
  :ensure t
  :init
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (setq smartparens-global-mode t)
  )

;; ----------------------------------------- ;;
;; Package: clean-aindent-mode               ;;
;;                                           ;;
;; GROUP: Editing -> Indent -> Clean Aindent ;;
;; ----------------------------------------- ;;
(prelude-require-package 'clean-aindent-mode)
(require 'clean-aindent-mode)
(use-package clean-aindent-mode
  :ensure t
  :hook ((prog-mode . clean-aindent-mode))
  )

;; -------------------- ;;
;; Package: dtrt-indent ;;
;; -------------------- ;;
(prelude-require-package 'dtrt-indent)
(require 'dtrt-indent)
(use-package dtrt-indent
  :ensure t
  :init
  (setq dtrt-indent-mode 1)
  )

;; ------------------ ;;
;; Package: ws-butler ;;
;; ------------------ ;;
(prelude-require-package 'ws-butler)
(require 'ws-butler)
(use-package ws-butler
  :ensure t
  :hook ((c-mode-common . ws-butler-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: undo-tree                  ;;
;;                                     ;;
;; GROUP: Editing -> Undo -> Undo Tree ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'undo-tree)
(require 'undo-tree)
(use-package undo-tree
  :ensure t
  :config
  (progn
    (defun modi/undo-tree-enable-save-history ()
      "Enable auto saving of the undo history."
      (interactive)

      (setq undo-tree-auto-save-history t)

      ;; Compress the history files as .gz files
      ;; (advice-add 'undo-tree-make-history-save-file-name :filter-return
      ;;             (lambda (return-val) (concat return-val ".gz")))

      ;; Persistent undo-tree history across emacs sessions
      (setq modi/undo-tree-history-dir (let ((dir (concat user-emacs-directory
                                                          "undo-tree-history/")))
                                         (make-directory dir :parents)
                                         dir))
      (setq undo-tree-history-directory-alist `(("." . ,modi/undo-tree-history-dir)))

      (add-hook 'write-file-functions #'undo-tree-save-history-hook)
      (add-hook 'find-file-hook #'undo-tree-load-history-hook))

    (defun modi/undo-tree-disable-save-history ()
      "Disable auto saving of the undo history."
      (interactive)

      (setq undo-tree-auto-save-history nil)

      (remove-hook 'write-file-functions #'undo-tree-save-history-hook)
      (remove-hook 'find-file-hook #'undo-tree-load-history-hook))

    (modi/undo-tree-disable-save-history)

    (global-undo-tree-mode 1))
  )
