
(message "inside init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(ido-mode (quote both) nil (ido))
 '(package-selected-packages
   (quote
    (key-chord fancy-narrow iedit window-number window-numbering multiple-cursors try counsel ace-window which-key use-package restclient rainbow-delimiters paredit-everywhere magit company-flx color-theme-sanityinc-tomorrow color-identifiers-mode clj-refactor bm beacon aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(setq site-lisp (concat init-dir "site-lisp"))

;; mostly stolen from: https://gitlab.com/buildfunthings/emacs-config/blob/master/loader.org

;; set user info
(setq user-full-name "Jeff Sack")
(setq user-mail-address "jeff@centriqhome.com")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; secure emacs
(require 'cl)
(setq tls-checktrust t)

(setq python (or (executable-find "py.exe")
                 (executable-find "python")))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))



;; set up package management
(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa"))
             (file-exists-p (concat init-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; so we can just type `y` or `n`
(fset 'yes-or-no-p 'y-or-n-p)


(use-package key-chord
  :config (key-chord-mode 1))

;; bookmarks package
;; (use-package bm
;;   :ensure t
;;   :demand t

;;   :init
;;   ;; restore on load (even before you require bm)
;;   (setq bm-restore-repository-on-load t)


;;   :config
;;   ;; Allow cross-buffer 'next'
;;   (setq bm-cycle-all-buffers t)

;;   ;; where to store persistant files
;;   (setq bm-repository-file "~/.emacs.d/bm-repository")

;;   ;; save bookmarks
;;   (setq-default bm-buffer-persistence t)

;;   ;; Loading the repository from file when on start up.
;;   (add-hook' after-init-hook 'bm-repository-load)

;;   ;; Restoring bookmarks when on file find.
;;   (add-hook 'find-file-hooks 'bm-buffer-restore)

;;   ;; Saving bookmarks
;;   (add-hook 'kill-buffer-hook #'bm-buffer-save)

;;   ;; Saving the repository to file when on exit.
;;   ;; kill-buffer-hook is not called when Emacs is killed, so we
;;   ;; must save all bookmarks first.
;;   (add-hook 'kill-emacs-hook #'(lambda nil
;; 				 (bm-buffer-save-all)
;; 				 (bm-repository-save)))

;;   ;; The `after-save-hook' is not necessary to use to achieve persistence,
;;   ;; but it makes the bookmark data in repository more in sync with the file
;;   ;; state.
;;   (add-hook 'after-save-hook #'bm-buffer-save)

;;   ;; Restoring bookmarks
;;   (add-hook 'find-file-hooks   #'bm-buffer-restore)
;;   (add-hook 'after-revert-hook #'bm-buffer-restore)

;;   ;; The `after-revert-hook' is not necessary to use to achieve persistence,
;;   ;; but it makes the bookmark data in repository more in sync with the file
;;   ;; state. This hook might cause trouble when using packages
;;   ;; that automatically reverts the buffer (like vc after a check-in).
;;   ;; This can easily be avoided if the package provides a hook that is
;;   ;; called before the buffer is reverted (like `vc-before-checkin-hook').
;;   ;; Then new bookmarks can be saved before the buffer is reverted.
;;   ;; Make sure bookmarks is saved before check-in (and revert-buffer)
;;   (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


;;   :bind (("<f2>" . bm-next)
;; 	 ("S-<f2>" . bm-previous)
;; 	 ("C-<f2>" . bm-toggle)))


;; Mousewheel scrolling can be quite annoying, lets fix it to scroll smoothly.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(use-package try
  :ensure t)

;; (use-package ace-window
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-x o") 'ace-window))

(use-package window-number
  :ensure t
  :load-path site-lisp
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'avy-goto-char))

;; highlight cursor when it moves
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; turn on line highlighting:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Cursor-Display.html#index-highlight-current-line
(global-hl-line-mode t)

;; comment/uncomment DWIM
(global-set-key (kbd "C-M-/") 'comment-or-uncomment-region)

(global-set-key (kbd "C-x f") 'project-find-file)

;; set up which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.0))


;; TODO: consider switching https://github.com/Fuco1/smartparens
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

;; Ensure paredit is used everywhere!
(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))

(show-paren-mode)

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package color-identifiers-mode
  :ensure t
  :config
  (global-color-identifiers-mode))


;; http://company-mode.github.io/
;; https://github.com/PythonNut/company-flx
(use-package company
  :ensure t
  :bind (("C-c /". company-complete))
  :config
  (global-company-mode)
  ;; (setq company-idle-delay nil)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (use-package company-flx
    :ensure t
    :pin melpa
    :config
    (company-flx-mode +1)))

;; git configuration
;; https://magit.vc/
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (setq cider-repl-use-pretty-printing t)
  ;; In order for Emacs to recognise .boot files as valid Clojure source code
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (setq cider-refresh-before-fn "boot.user/stop"
	cider-refresh-after-fn "boot.user/go")
  (key-chord-define cider-mode-map "cb" 'cider-repl-clear-buffer)
  (key-chord-define cider-mode-map "rs" 'raise-sexp))

(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-eval nil)
  (setq cljr-favor-prefix-notation nil))


(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (color-theme-sanityinc-tomorrow-night))


;; always auto-revert file buffers
(global-auto-revert-mode t)

;; TODO: recentf, expand-region, multiple-cursors, projectile, ivy/swiper/cousel, iedit, smart-parens (remove paredit), neotree
;; TODO: steal most stuff from here: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bos/osx
;; TODO: try this: https://github.com/zk-phi/phi-grep
;; TODO: borrow from this: https://www.reddit.com/r/emacs/comments/5udtw1/usepackageel_about_to_be_moved_to_emacs_core/

(use-package projectile
  :ensure t)

(use-package ag
  :ensure t)

(use-package counsel
  :ensure t)

(use-package swiper
  :pin melpa-stable
  :diminish ivy-mode
  :ensure t
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on)
  (global-set-key (kbd "C-c p SPC") 'counsel-projectile)

  (global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)
  (global-set-key (kbd "C-c p d") 'counsel-projectile-find-dir)
  (global-set-key (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
  (global-set-key (kbd "C-c p s s") 'counsel-projectile-ag)
  (global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project))

(message "done init.el...")


(put 'narrow-to-region 'disabled nil)

;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (key-chord-mode 1)
;;   (key-chord-define-global "cb" 'cider-repl-clear-buffer))
