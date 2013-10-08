(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell starter-kit-bindings clojure-mode clojure-test-mode nrepl projectile magit ac-nrepl rainbow-delimiters)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-auto-revert-mode)

(load-theme 'cyberpunk t)


;; Configure nrepl.el
;(setq nrepl-hide-special-buffers t)
;(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
 
;; Some default eldoc facilities
(add-hook 'nrepl-connected-hook
          (defun pnh-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

;; Repl mode hook
;(add-hook 'nrepl-mode-hook 'subword-mode)


;; Auto completion for NREPL
(require 'ac-nrepl)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

(if (featurep 'ns)
    (progn
      (defun ns-raise-emacs ()
        "Raise Emacs."
        (ns-do-applescript "tell application \"Emacs\" to activate"))

      (if (display-graphic-p)
          (progn
            (add-hook 'server-visit-hook 'ns-raise-emacs)
            (add-hook 'before-make-frame-hook 'ns-raise-emacs)
            (ns-raise-emacs)))))


(if (not (getenv "TERM_PROGRAM"))
      (let ((path (shell-command-to-string
              "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
        (setenv "PATH" path)))

