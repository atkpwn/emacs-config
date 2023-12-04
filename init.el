(add-hook 'after-init-hook #'garbage-collect t)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 2 1000 1000))))

;; from jwiegley
(defconst emacs-start-time (current-time))

(defun report-time-since-load (&optional suffix)
  (message "Loading init...done (%.3fs)%s"
           (float-time (time-subtract (current-time) emacs-start-time))
           suffix))

(add-hook 'after-init-hook
          #'(lambda () (report-time-since-load " [after-init]"))
          t)

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (setq package-enable-at-startup nil
        load-path
        (append (list (emacs-path "use-package"))
                (delete-dups load-path)
                (list (emacs-path "lisp")))))

;; from readme
(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package bind-key)

;; from jwiegley
(setq use-package-verbose            init-file-debug
      use-package-expand-minimally   (not init-file-debug)
      use-package-compute-statistics nil
      debug-on-error                 init-file-debug)

;; Disable warnings about obsolete functions when compiling
(eval-when-compile
  (dolist (sym '(cl-flet lisp-complete-symbol))
    (setplist sym (use-package-plist-delete
                   (symbol-plist sym) 'byte-obsolete-info))))

(use-package emacs
  :bind
  (("C-x f" . find-file-other-window)
   ("s-u"   . revert-buffer))
  :hook
  (before-save . delete-trailing-whitespace)
  (after-save  . executable-make-buffer-file-executable-if-script-p)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-hscroll-mode       'current-line)
  (auto-save-interval                 64)
  (auto-save-timeout                   2)
  (enable-recursive-minibuffers        t)
  (fill-column                       100)
  (history-delete-duplicates           t)
  (history-length                    200)
  (kill-do-not-save-duplicates         t)
  (load-prefer-newer                   t)
  (require-final-newline               t)

  :config
  (global-auto-revert-mode 1)
  (delete-selection-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default tab-width 2)
  (setq scroll-conservatively 10000
        scroll-preserve-screen-position t)

  (if (boundp 'use-short-answers)
    (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))

  ;; menu bar & tool bar & Bell
  (tool-bar-mode   -1)
  (scroll-bar-mode -1)

  (setq frame-resize-pixelwise t)
  (when (and (fboundp 'menu-bar-mode)
             (not (eq system-type 'darwin)))
    (menu-bar-mode -1)
    (global-set-key [f10] 'menu-bar-mode)
    (unbind-key "M-`"))

  ;; bell
  (setq visible-bell nil
        ring-bell-function '(lambda ()
                              (invert-face 'mode-line)
                              (run-with-timer 0.1 nil #'invert-face 'mode-line)))

  ;; undo-tree
  (global-undo-tree-mode)

  (set-register ?i (cons 'file (emacs-path "init.el")))
  (setq nix-configuration-file (expand-file-name "/sudo::/etc/nixos/configuration.nix"))
  (set-register ?c (cons 'file nix-configuration-file))
  (set-register ?h (cons 'file (expand-file-name "~/.config/home-manager/programs.nix")))
  (set-register ?t (cons 'file (expand-file-name "~/orgfiles/todo.org")))
  )

(use-package no-littering
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t)
  (no-littering-theme-backups))

(use-package recentf
  :after no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package doom-themes
  :init
  (setq default-frame-alist '(;; (undecorated           . t)
                              (alpha                 . (95 . 95))
                              (drag-internal-border  . t)
                              (internal-border-width . 2)))
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  :config
  (load-theme 'doom-tomorrow-night t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package minions
  :hook
  (doom-modeline-mode . minions-mode))

(use-package display-line-numbers
  :hook
  ((conf-mode prog-mode) . display-line-numbers-mode)
  (org-mode              . (lambda () (display-line-numbers-mode -1)))
  :custom
  (display-line-numbers-grow-only   t)
  (display-line-numbers-type        t)
  (display-line-numbers-width-start 2))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package which-key
  :demand t
  :diminish
  :config
  (which-key-mode))

(use-package ace-window
  :diminish
  :bind
  ("C-x q" . ace-window)
  :config
  (setq aw-keys '(?j ?k ?l ?\; ?a ?s ?d ?f)))

(use-package selected
  :demand t
  :diminish selected-minor-mode
  :bind
  (:map selected-keymap
   ("[" . align-code)
   ("f" . fill-region)
   ("U" . unfill-region)
   ("d" . downcase-region)
   ("u" . upcase-region)
   ("r" . reverse-region)
   ("s" . sort-lines))
  :hook
  (prog-mode    . selected-minor-mode)
  (text-mode . selected-minor-mode))

(use-package multiple-cursors
  :after selected
  :bind
  (("<C-c> m c" . mc/edit-lines)
   ("<C-c> m n" . mc/insert-numbers)
   :map selected-keymap
   ("c"   . mc/edit-lines)
   ("."   . mc/mark-next-like-this)
   (">"   . mc/unmark-next-like-this)
   ("C->" . mc/skip-to-next-like-this)
   (","   . mc/mark-previous-like-this)
   ("<"   . mc/unmark-previous-like-this)
   ("C-<" . mc/skip-to-previous-like-this)
   ("y"   . mc/mark-next-symbol-like-this)
   ("Y"   . mc/mark-previous-symbol-like-this)
   ("w"   . mc/mark-next-word-like-this)
   ("W"   . mc/mark-previous-word-like-this)))

(use-package expand-region
  :bind
  ("C-." . er/expand-region)
  :custom
  (expand-region-fast-keys-enabled nil))

(use-package avy
  :bind
  (("C-:"   . avy-goto-char)
   ("C-;"   . avy-goto-word-1)
   ("C-c ;" . avy-goto-char-timer))
  :custom
  (avy-keys (number-sequence ?a ?z)))

(use-package avy-embark
  :no-require t
  :after
  (avy embark)
  :preface
  (defun avy-action-embark (pt)
    (require 'embark
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t))
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package consult
  :bind
  (("C-s"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-c f"   . consult-project-buffer)
   ("C-c g"   . consult-goto-line)
   ("C-c r r" . consult-ripgrep)
   ("C-c r f" . consult-fd)
   :map minibuffer-local-map
   ("C-r" . consult-history))
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (consult-customize consult-buffer
                     :preview-key "M-."))

(use-package consult-project-extra)

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-dir-vertico
  :no-require t
  :after (consult-dir vertico)
  :defines (vertico-map)
  :bind
  (:map vertico-map
        ("C-x C-j" . consult-dir)
        ("M-g d"   . consult-dir)
        ("M-s f"   . consult-dir-jump-file)))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package embark
  :bind
  (("C-'"   . embark-act)       ;; pick some comfortable binding
   ;; ("C-;"   . embark-dwim)   ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-keymap-alist '(tab . embark-tab-actions)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :config
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package vertico
  :bind
  (:map vertico-map
   ("C-n" . vertico-next)
   ("C-p" . vertico-previous)
   ;; ("C-j" . minibuffer-force-complete-and-exit)
   ("C-j" . vertico-exit)
   :map minibuffer-local-map
   ("C-l" . vertico-directory-delete-word))
  :custom
  (vertico-count  10)
  (vertico-resize nil)
  (vertico-cycle  t)
  :preface
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :init
  (vertico-mode))

(use-package vertico-directory)

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults  nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package rg)

(use-package paren
  :custom
  (show-paren-priority -1)
  :config
  (electric-pair-mode 1) ; auto-insert matching bracket
  (show-paren-mode t)
  (add-hook 'after-save-hook 'check-parens nil t)
  (setq show-paren-delay 0)
  (set-face-foreground 'show-paren-match "#dfd")
  (set-face-attribute  'show-paren-match nil :weight 'extra-bold)
  (set-face-foreground 'show-paren-mismatch "#ff2222")
  (set-face-background 'show-paren-mismatch "#aa0a0a")
  (set-face-attribute  'show-paren-mismatch nil :weight 'extra-bold))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package unfill
  :bind
  ("M-Q" . unfill-paragraph))

(use-package prog-mode
  :hook
  (prog-mode . set-highlight-keywords)
  :config
  (defvar font-lock-todo-face 'font-lock-todo-face
    "Face name to use for TODOs.")
  (defface font-lock-todo-face
    '((t :foreground "#ff3a11" :weight bold))
    "Font Lock mode face used to highlight TODOs."
    :group 'font-lock-faces)
  (defun set-highlight-keywords ()
    (font-lock-add-keywords
     nil
     '(("\\(FIX\\|FIXME\\|NOTE\\|TODO\\|WARNING\\|!!!\\):" 1 font-lock-todo-face t)))))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 5)
  :config
  ;; from https://emacs.stackexchange.com/a/43975
  (transient-define-suffix magit-submodule-update-all ()
    "Update all submodules"
    :description "Update all     git submodule update --init --recursive"
    (interactive)
    (magit-with-toplevel
      (magit-run-git-async "submodule" "update" "--init" "--recursive")))

  (transient-append-suffix 'magit-submodule "u"
    '("U" magit-submodule-update-all)))

(use-package magit-delta
  :hook
  (magit-mode . magit-delta-mode))

(use-package treemacs
  :defer t
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  :custom-face
  (treemacs-root-face ((t (:underline nil :bold t :height 1.1))))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :disabled
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package dired
  :commands (dired dired-jump)
  :bind
  (:map dired-mode-map
        ("C-j" . dired-find-file)))

(use-package dired-x
  :after dired)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package yasnippet
  :demand
  :hook
  (c++-mode  . yas-minor-mode)
  (java-mode . yas-minor-mode)
  (nix-mode  . yas-minor-mode)
  :custom
  (yas-snippet-dirs (list (emacs-path "snippets")))
  :config
  (yas-reload-all))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  ("C-x k"   . persp-kill-buffer*)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-initial-frame-name "main")
  :init
  (persp-mode))

;; ** Window control
;; *** Toggle split window
;;     The following code is taken from this [[https://emacs.stackexchange.com/a/5372][answer]] by [[https://emacs.stackexchange.com/users/253/dan][Dan]] on StackExchange.
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
(global-set-key (kbd "C-x %") 'window-split-toggle)

;; *** Toggle full screen
;;     From [[https://www.emacswiki.org/emacs/FullScreen#h5o-27][EmacsWiki]].
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "C-S-f") 'toggle-fullscreen)

;; *** Save window configuration
;;    The configuration here is (again) from John Wiegley's [[https://github.com/jwiegley/dot-emacs/blob/master/init.org#push-and-pop-window-configurations]].
(defvar saved-window-configuration nil)

(defun push-window-configuration ()
  (interactive)
  (push (current-window-configuration) saved-window-configuration))

(defun pop-window-configuration ()
  (interactive)
  (let ((config (pop saved-window-configuration)))
    (if config
        (set-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))

(use-package zoom-window
  :bind
  ("C-x C-z" . zoom-window-zoom)
  :custom
  (zoom-window-mode-line-color "#3a4a50"))

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))

(use-package winner
  :bind
  (("M-[" . winner-undo)
   ("M-]" . winner-redo))
  :config
  (winner-mode 1))

(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package eglot)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-popupinfo-delay '(0.5 . 0))
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind
  (("M-/"   . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package nerd-icons-corfu
  :custom
  (corfu-right-margin-width 1)
  :config
  (require 'nerd-icons)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.
  )

(use-package eldoc)

(use-package lisp-mode
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq-local prettify-symbols-alist lisp--prettify-symbols-alist)
                       (prettify-symbols-mode 1)))
  :init
  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)
      ("."       . ?•))))

(use-package sly
  :hook
  (lisp-mode . sly-editing-mode)
  (lisp-mode . aggressive-indent-mode)
  :config
  (require 'sly-quicklisp)
  (require 'sly-repl-ansi-color)
  (require 'sly-asdf))

;; python setup
;; https://gist.github.com/habamax/290cda0e0cdc6118eb9a06121b9bc0d7
(use-package pyvenv
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . pyvenv-tracking-mode)
  (python-mode . (lambda ()
                   (eldoc-mode)
                   (eglot-ensure)))
  :custom
  (pyvenv-default-virtual-env-name "venv")
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode))))

(use-package blacken
  :hook
  (python-mode . blacken-mode))

(use-package go-mode
  :hook
  (go-mode . (lambda ()
               (eglot-ensure)
               (add-hook 'before-save-hook eglot-format-buffer nil 'local))))

(use-package rust-mode
  :hook
  (rust-mode . prettify-symbols-mode)
  (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package tex-site
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode . (lambda ()
                  (smartparens-mode)
                  (prettify-symbols-mode 1)
                  (display-line-numbers-mode)
                  (visual-line-mode)
                  (LaTeX-math-mode)))
  :config
  (setq TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t)
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (autoload 'predictive-mode "predictive" "predictive" t)
  (set-default 'predictive-auto-add-to-dict t)
  (setq predictive-main-dict 'rpg-dictionary
        predictive-auto-learn t
        predictive-add-to-dict-ask nil
        predictive-use-auto-learn-cache nil
        predictive-which-dict t))

(use-package reftex
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (reftex-isearch-minor-mode)))
  :config
  (setq reftex-plug-into-AUCTeX t) ;; https://www.gnu.org/software/emacs/manual/html_node/reftex/AUCTeX_002dRefTeX-Interface.html
  (setq reftex-cite-prompt-optional-args t))

(use-package json-mode
  :mode "\\.json\\'")

(use-package jq-format
  :after json-mode)

;; Macintoch
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(global-unset-key (kbd "s-q"))
