(provide 'setup-applications)

;; ------------------------------------- ;;
;; GROUP: Applications -> c mode editing ;;
;; ------------------------------------- ;;
(prelude-require-package 'cl);
(require 'cl)
(prelude-require-package 'cc-mode)
(require 'cc-mode)

;; ---------------------------- ;;
;; GROUP: Applications-> Eshell ;;
;; ---------------------------- ;;
(prelude-require-package 'eshell)
(require 'eshell)
;; (prelude-require-package 'em-alias)
(require 'em-alias)

;; Advise find-file-other-window to accept more than one file
(defadvice find-file-other-window (around find-files activate)
  "Also find all files within a list of files. This even works recursively."
  (if (listp filename)
      (loop for f in filename do (find-file-other-window f wildcards))
    ad-do-it))

;; In Eshell, you can run the commands in M-x
;; Here are the aliases to the commands.
;; $* means accepts all arguments.
(eshell/alias "o" "")
(eshell/alias "o" "find-file-other-window $*")
(eshell/alias "vi" "find-file-other-window $*")
(eshell/alias "vim" "find-file-other-window $*")
(eshell/alias "emacs" "find-file-other-windpow $*")
(eshell/alias "em" "find-file-other-window $*")

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;; change listing switches based on OS
(when (not (eq system-type 'windows-nt))
  (eshell/alias "ls" "ls --color -h --group-directories-first $*"))
