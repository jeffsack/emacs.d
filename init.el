;; TODO: try https://github.com/Malabarba/paradox
;; TODO: upgrade ivy https://oremacs.com/2017/04/09/ivy-0.9.0/
;; TODO: try https://github.com/Kungsgeten/selected.el (see also https://www.reddit.com/r/emacs/comments/63mx6f/how_do_you_use_the_selectel_package_share_some/)
;; TODO: try: https://github.com/gregsexton/origami.el
;; TODO: https://github.com/nflath/recursive-narrow/blob/master/recursive-narrow.el
;; TODO: better configure ivy/cousel
;; TODO: better configure projectile

;; TODO: multiple-cursors, iedit, smart-parens (remove paredit), neotree
;; TODO: steal most stuff from here: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bos/osx
;; TODO: try this: https://github.com/zk-phi/phi-grep
;; TODO: borrow from this: https://www.reddit.com/r/emacs/comments/5udtw1/usepackageel_about_to_be_moved_to_emacs_core/ (OR https://www.reddit.com/r/emacs/comments/5udtw1/usepackageel_about_to_be_moved_to_emacs_core/?sort=new#bottom-comments)
;; TODO: https://github.com/tam17aki/ace-isearch
;; TODO: borrow from https://github.com/wasamasa/dotemacs/blob/master/init.org
;; TODO: try https://github.com/jacktasia/dumb-jump

;; sources:
;; http://planet.emacsen.org/
;; https://emacs.zeef.com/ehartc
;; https://github.com/emacs-tw/awesome-emacs
;; http://sachachua.com/blog/category/emacs-news/page/12/
;; https://www.reddit.com/r/emacs/




(message "inside init.el")

(toggle-frame-maximized)

(setq inhibit-startup-screen t)

;; moving the customize "mess" outside of init.el (finally)
(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file)

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
;;(setq tls-checktrust nil)

;; https://pip.pypa.io/en/stable/installing/
;; python -m pip install --user certifi
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
  ;;(setq gnutls-verify-error t)
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

;; enable this handy command
(put 'narrow-to-region 'disabled nil)

;; manage backup and autosave files
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)

(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "M-s-R") 'replace-string)

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (setq rm-blacklist ".*"))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(key-chord-define-global "FM" 'toggle-frame-maximized)
(key-chord-define-global "PM" 'paredit-mode)
;;(key-chord-define-global "RS" 'replace-string)

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'avy-goto-char))

;;(key-chord-define-global "JW" 'avy-goto-word-1)
;;(key-chord-define-global "JL" 'avy-goto-line)
;;(key-chord-define-global "JC" 'avy-goto-char)
(global-set-key (kbd "s-g") 'avy-goto-line)
(global-set-key (kbd "M-s-g") 'avy-goto-word-1)
(global-set-key (kbd "C-M-s-g") 'avy-goto-char)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(defun my-ag-search-finished-hook ()
  (next-error-follow-minor-mode)
  (remove-hook 'ag-search-finished-hook 'my-ag-search-finished-hook))

;; (defun my-cider-quit-and-connect ()
;;   "Just cider-quit and then cider-connect"
;;   (interactive)
;;   (cider-quit)
;;   (cider-connect))

(defun my-ag-project-at-point (string)
  "Slightly customized `ag-project-at-point`"
  (interactive (list (ag/read-from-minibuffer "Search string")))
  (let ((ag-arguments (append ag-arguments '("-A 4" "-B 4"))))
    (add-hook 'ag-search-finished-hook 'my-ag-search-finished-hook)
    (ag-project string)))

;;(key-chord-define-global "FF" 'my-ag-project-at-point)
(global-set-key (kbd "C-s-a") 'my-ag-project-at-point)
(global-set-key (kbd "C-M-s-a") 'isearch-forward-symbol-at-point)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-z") 'undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo))

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

(use-package window-number
  :ensure t
  :load-path site-lisp
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

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
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.0))


;; TODO: consider switching https://github.com/Fuco1/smartparens
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

;; Ensure paredit is used everywhere!
(use-package paredit-everywhere
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

;; load Emacs Live paredit extensions

(use-package thingatpt
  :ensure t)

(load (concat site-lisp "/util-fns.el"))

;;use delete-horizontal-space to completely nuke all whitespace
(global-set-key (kbd "M-SPC ") 'live-delete-whitespace-except-one)

(load (concat site-lisp "/paredit-conf.el"))

(define-key paredit-mode-map (kbd "M-P")     'live-paredit-previous-top-level-form)
(define-key paredit-mode-map (kbd "M-N")     'live-paredit-next-top-level-form)
(define-key paredit-mode-map (kbd "M-d")     'live-paredit-forward-kill-sexp)
(define-key paredit-mode-map (kbd "C-M-k")   'live-paredit-copy-sexp-at-point)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
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
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 10000)
  (setq cider-repl-history-file ".cider-repl-history")
  (setq cider-repl-use-pretty-printing t)
  ;; In order for Emacs to recognize .boot files as valid Clojure source code
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  (setq cider-refresh-before-fn "boot.user/stop"
	cider-refresh-after-fn "boot.user/go")
  ;;(key-chord-define cider-repl-mode-map "cb" 'cider-repl-clear-buffer)
  ;;(key-chord-define cider-mode-map "rs" 'raise-sexp)
  (define-key cider-repl-mode-map (kbd "s-k") 'cider-repl-clear-buffer)
  (define-key cider-repl-mode-map (kbd "s-r") 'raise-sexp)
  (define-key cider-mode-map (kbd "s-r") 'raise-sexp)
  (define-key cider-repl-mode-map (kbd "C-M-s-q") 'cider-quit)
  (define-key cider-mode-map (kbd "C-M-s-q") 'cider-quit)
  (define-key cider-repl-mode-map (kbd "C-M-s-c") 'cljr-cycle-coll)
  (define-key cider-mode-map (kbd "C-M-s-c") 'cljr-cycle-coll)
  (define-key cider-mode-map (kbd "C-c M-o") 'cider-eval-print-last-sexp)
  ;; more custom dev stuff
  (defun centriq-web-in-dev ()
    (interactive)
    (cider-interactive-eval
     ;;"(centriq-web.lein/in-dev)"
     "(boot.user/in-dev)"
     )
    (cider-repl-set-ns "centriq-web.dev"))

  (define-key cider-mode-map (kbd "s-d") 'centriq-web-in-dev)
  (define-key cider-repl-mode-map (kbd "s-d") 'centriq-web-in-dev))

(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-eval nil)
  (setq cljr-favor-prefix-notation nil))


;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (color-theme-sanityinc-tomorrow-night))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

(use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))

;; always auto-revert file buffers
(global-auto-revert-mode t)

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (run-at-time nil (* 30 60) 'recentf-save-list) ;; save every 30 minutes
  (setq recentf-save-file (concat user-emacs-directory "recentf." (system-name)))
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))

(use-package expand-region
  :ensure t
  :pin melpa
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package projectile
  :ensure t
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules" ".ignored"))

(use-package swiper
  :pin melpa-stable
  :ensure t
  :ensure counsel
  :ensure ivy-hydra
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
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
  (global-set-key (kbd "C-x l") 'counsel-locate)

  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package counsel-projectile
  :ensure t
  :config
  ;;(counsel-projectile-on)
  (counsel-projectile-mode t)
  (global-set-key (kbd "C-c p SPC") 'counsel-projectile)

  (global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)
  (global-set-key (kbd "C-c p d") 'counsel-projectile-find-dir)
  (global-set-key (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
  (global-set-key (kbd "C-c p s s") 'counsel-projectile-ag)
  (global-set-key (kbd "C-M-s-f") 'counsel-projectile-ag)
  (global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project))


(use-package swift-mode
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ;;("M-0"       . treemacs-select-window)
        ;;("C-c 1"     . treemacs-delete-other-windows)
        ("C-s-t"    . treemacs-toggle)
        ("C-M-s-t"    . treemacs)
        ;;("C-s-f" . treemacs-find-file)
	))

;; (kbd "C-M-s-f")

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("C-M-s-p" . treemacs-projectile)
              ("C-s-p" . treemacs-projectile-toggle)))

;; https://stackoverflow.com/questions/2577394/emacs-split-into-3-even-windows
(defadvice split-window-horizontally
    (after rebalance-windows activate)
  (balance-windows))

(ad-activate 'split-window-horizontally)

;; from http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)
(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)  ; remove trailing whitespace before saving

(message "done init.el...")
