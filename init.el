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
          #'(lambda ()
              (report-time-since-load " [after-init]")
              ;; see https://karthinks.com/software/it-bears-repeating/
              (repeat-mode))
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
   ("s-y"   . revert-buffer))
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

  (recenter-positions '(middle top 0.15 bottom 0.85))

  ;; from corfu
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :config
  (setq confirm-kill-emacs 'yes-or-no-p)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (delete-selection-mode)

  (setq-default indent-tabs-mode nil)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default tab-width 2))

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

;; disable UI dialog
(setq use-dialog-box nil)

(set-register ?i (cons 'file (emacs-path "init.org")))
(setq nix-configuration-file (expand-file-name "/sudo::/etc/nixos/configuration.nix"))
(set-register ?c (cons 'file nix-configuration-file))
(set-register ?h (cons 'file (expand-file-name "~/.config/home-manager/programs.nix")))
(set-register ?t (cons 'file (expand-file-name "~/orgfiles/todo.org")))

(use-package which-key
  :demand t
  :diminish
  :config
  (which-key-mode))

(use-package no-littering
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (load custom-file t)
  (no-littering-theme-backups))

(use-package recentf
  :after no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package diminish)

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
  (column-number-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package minions
  :hook
  (doom-modeline-mode . minions-mode))

(use-package display-line-numbers
  :hook
  (prog-mode    . display-line-numbers-mode)
  (conf-mode    . display-line-numbers-mode)
  (yaml-ts-mode . display-line-numbers-mode)
  (org-mode     . (lambda () (display-line-numbers-mode -1)))
  :custom
  (display-line-numbers-grow-only   t)
  (display-line-numbers-type        t)
  (display-line-numbers-width-start 2))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

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
  (prog-mode . selected-minor-mode)
  (conf-mode . selected-minor-mode)
  (text-mode . selected-minor-mode))

(use-package expand-region
  :bind
  ("C-." . er/expand-region)
  :custom
  (expand-region-fast-keys-enabled nil))

(use-package multiple-cursors
  :after selected
  :bind
  (("C-c m c" . mc/edit-lines)
   ("C-c m n" . mc/insert-numbers)
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

(use-package avy
  :bind
  (("C-:"   . avy-goto-char)
   ("C-;"   . avy-goto-word-1)
   ("C-c ;" . avy-goto-char-timer))
  :custom
  (avy-keys (number-sequence ?a ?z)))

(use-package avy-zap
  :bind
  (("M-z" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-to-char-dwim)))

(use-package highlight-indentation
  :hook
  (yaml-ts-mode . highlight-indentation-current-column-mode)
  :config
  (set-face-background 'highlight-indentation-current-column-face "#2b2e2e"))

(use-package consult
  :bind
  (("C-s"     . consult-line)
   ("C-x B"   . consult-buffer)
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

(use-package embark
  :bind
  (("M-'"   . embark-act)       ;; pick some comfortable binding
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
  (add-to-list 'embark-keymap-alist '(tab . embark-tab-actions))

  ;; which-key-like menu prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  )

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

(use-package nerd-icons-completion
  :after marginalia
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

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
  (setq history-length 25)
  (savehist-mode))

(save-place-mode 1)

(use-package orderless
  :demand t
  :custom
  (orderless-matching-styles '(orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless flex)))))

(use-package paren
  :hook
  (prog-mode . electric-pair-local-mode)
  (conf-mode . electric-pair-local-mode)
  (json-mode . electric-pair-local-mode)
  :custom
  (show-paren-priority -1)
  :config
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

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 5)
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

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

(use-package blamer
  :bind
  (("s-i"   . blamer-show-commit-info)
   ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background "unspecified"
                   :height 140
                   :italic t))))

(use-package diff-hl
  :commands
  (diff-hl-mode diff-hl-dired-mode)
  :hook
  (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (prog-mode    . diff-hl-mode)
  (conf-mode    . diff-hl-mode)
  (yaml-ts-mode . diff-hl-mode)
  (nix-mode     . diff-hl-mode)
  (org-mode     . diff-hl-mode))

(use-package diff-hl-flydiff
  :commands
  diff-hl-flydiff-mode
  :init
  (diff-hl-flydiff-mode))

;; from https://github.com/jwiegley/dot-emacs/blob/master/init.org#diffview
(use-package diffview
  :commands
  (diffview-current diffview-region diffview-message))

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

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  )

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-hide-cursor  nil)
  (dirvish-hide-details nil)
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-header-line-height '(15 . 25))
  (dirvish-mode-line-height 10)
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  (dired-listing-switches
   "-l --almost-all --human-readable -o --group-directories-first --no-group")

  :config
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'

  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))

  ;; mouse settings
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq mouse-1-click-follows-link nil)
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dired-up-directory)
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("["   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  )

(use-package dired-x
  :after dirvish)

(use-package yasnippet
  :demand
  :hook
  (c++-ts-mode        . yas-minor-mode)
  (java-ts-mode       . yas-minor-mode)
  (nix-mode           . yas-minor-mode)
  (python-ts-mode     . yas-minor-mode)
  (typescript-ts-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs (list (emacs-path "snippets")))
  :config
  (yas-reload-all))

(use-package perspective
  :bind
  ("C-x b"   . persp-switch-to-buffer*)
  ("C-x C-b" . persp-list-buffers)
  ("C-x k"   . persp-kill-buffer*)
  (:map perspective-map
        ("b" . persp-switch-to-buffer))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-initial-frame-name "main")
  :init
  (persp-mode))

(use-package ace-window
  :diminish
  :bind
  (("C-x q"   . ace-window)
   ("C-x C-o" . ace-swap-window))
  :config
  (setq aw-keys '(?j ?k ?l ?\; ?a ?s ?d ?f)))

;; Taken from this [[https://emacs.stackexchange.com/a/5372][answer]] by [[https://emacs.stackexchange.com/users/253/dan][Dan]] on StackExchange.
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

;; From [[https://www.emacswiki.org/emacs/FullScreen#h5o-27][EmacsWiki]].
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "C-S-f") 'toggle-fullscreen)

;; Again from John Wiegley's [[https://github.com/jwiegley/dot-emacs/blob/master/init.org#push-and-pop-window-configurations]].
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

(use-package popper
  :bind
  (("C-`"     . popper-toggle)
   ("C-<tab>" . popper-cycle)
   ("C-~"     . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Flymake diagnostics for `[^/]+'\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package winner
  :bind
  (("M-[" . winner-undo)
   ("M-]" . winner-redo))
  :config
  (winner-mode 1))

(use-package openwith
  :disabled
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
               "gthumb"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
               "evince"
               '(file))
         ))
  (openwith-mode 1))

(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package rg)

(use-package unfill
  :bind
  ("M-Q" . unfill-paragraph))

(use-package eglot
  :demand t
  :bind
  (:map eglot-mode-map
        ("C-c e f n" . flymake-goto-next-error)
        ("C-c e f p" . flymake-goto-prev-error)
        ("C-c e r"   . eglot-rename)
        ("C-c e f"   . eglot-format)))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto  t)
  (corfu-popupinfo-delay '(0.5 . 0))
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :custom
  (corfu-right-margin-width 1)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; ...
          ;; Remember to add an entry for `t', the library uses that as default.
          (t :style "cod" :icon "code" :face font-lock-warning-face))))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind
  (("M-/"   . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package eldoc)

(use-package markdown-mode) ;; required to display eldoc properly

;; TODO: cape

(use-package prog-mode
  :hook
  (prog-mode . set-highlight-keywords)
  (conf-mode . set-highlight-keywords)
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

(use-package sh-mode
  :hook
  (sh-mode . eglot-ensure))

(use-package lisp-mode
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq prettify-symbols-alist lisp--prettify-symbols-alist)
                       (eldoc-mode)))
  :init
  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)
      ("."       . ?•)))
  (global-prettify-symbols-mode))

(use-package sly
  :hook
  (lisp-mode . sly-editing-mode)
  (lisp-mode . aggressive-indent-mode)
  :config
  (require 'sly-quicklisp)
  (require 'sly-repl-ansi-color)
  (require 'sly-asdf))

(use-package clojure-ts-mode
  :no-require t)

(use-package cider
  :no-require t)

(use-package clj-refactor
  :no-require t
  :hook
  (clojure-ts-mode . (lambda ()
                       (clj-refactor-mode 1)
                       (cljr-add-keybindings-with-prefix "C-c r"))))

(use-package flycheck-clojure
  :no-require t)

(use-package nix-ts-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :hook
  (nix-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(nix-ts-mode . ("nixd"))))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

(use-package python
  :hook
  (python-ts-mode . (lambda ()
                      (pyvenv-mode)
                      (pyvenv-tracking-mode)))
  (python-ts-mode . (lambda ()
                      (eldoc-mode)
                      (eglot-ensure)))
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (push '(python-mode . python-ts-mode) major-mode-remap-alist))

(use-package pyvenv
  :custom
  (pyvenv-default-virtual-env-name "venv")
  :config
  (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python))

(use-package cmake-ts-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  :hook
  (cmake-ts-mode . eglot-ensure))

(use-package cmake-font-lock
  :hook
  (cmake-mode . cmake-font-lock-activate))

(use-package c-ts-mode
  :hook
  ((c-ts-mode c++-ts-mode) . (lambda ()
                               (eglot-ensure)
                               (setq-local cpp-format-on-save-p t)
                               (add-hook 'before-save-hook #'cpp-format nil t)))
  :config
  (push '(c-mode . c-ts-mode) major-mode-remap-alist)
  (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
  (push '(c-or-c++-mode . c-or-c++-ts-mode) major-mode-remap-alist)
  (defun cpp-format ()
    (if cpp-format-on-save-p
        (eglot-format-buffer))))

(use-package kotlin-mode
  :hook
  (kotlin-mode . eglot-ensure))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook
  (go-ts-mode . (lambda ()
                  (subword-mode)
                  (eglot-ensure)
                  (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
  :config
  (setq go-ts-mode-indent-offset 2)
  (setq project-vc-extra-root-markers '(".project.el")))

(use-package rust-ts-mode
  :mode
  ("\\.rs\\'"  . rust-ts-mode)
  :hook
  (rust-ts-mode . prettify-symbols-mode)
  (rust-ts-mode . (lambda ()
                    (eglot-ensure)
                    (setq indent-tabs-mode nil)))
  :config
  (setq project-vc-extra-root-markers '(".project.el"))
  (setq rust-format-on-save t))

(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode)
  :bind
  (:map cargo-mode-map
        ("C-c C-c r" . cargo-process-run)))

(use-package js
  :mode
  ("\\.js\\'"  . js-ts-mode)
  :hook
  (js-ts-mode . eglot-ensure)
  :config
  (push '(js-mode . js-ts-mode) major-mode-remap-alist))

(use-package deno-ts-mode
  :hook
  (deno-ts-mode     . eglot-ensure)
  (deno-tsx-ts-mode . eglot-ensure))

(use-package tex-site
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode . (lambda ()
                  ;; (smartparens-mode) ;; TODO: needed?
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

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package jq-format
  :after json-ts-mode)

(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (push '(yaml-mode . yaml-ts-mode) major-mode-remap-alist))

(use-package nginx-mode
  :commands nginx-mode)

(use-package dockerfile-mode)

(use-package terraform-mode
  :mode "\\.tf\\'"
  :custom
  (terraform-indent-level 2)
  :hook
  (terraform-mode . eglot-ensure)
  (terraform-mode . outline-minor-mode))

(use-package org-config)

(use-package citar
  :bind
  ("C-c [" . citar-insert-citation)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode   . citar-capf-setup)
  :custom
  (citar-bibliography '("~/Dropbox/papers/bibliography/references.bib")))

(use-package citar-embark
  :after citar embark
  :no-require
  :config
  (citar-embark-mode))

(use-package grip-mode
  :bind
  ;; Make a keybinding: `C-c C-c g'
  (:map markdown-mode-command-map
        ("g" . grip-mode))
  :custom
  (grip-command 'go-grip) ;; auto, grip, go-grip or mdopen
  ;; (grip-preview-use-webkit t)
  )

(use-package lean4-mode
  :commands lean4-mode
  :mode
  "\\.lean'")

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '(("https://allthingsdistributed.com/atom.xml" aws dev)
          ("https://www.breakds.org/index.xml" nix c++ dev)
          ("https://blog.alexellis.io/rss/" github dev)
          ("https://blog.colinbreck.com/rss/" kubernetes dev)
          ("https://corrode.dev/rss.xml" rust dev)

          ("http://www.howardism.org/index.xml" emacs)
          ("https://tsdh.org/rss.xml" emacs dev)
          ("http://sachachua.com/blog/category/emacs-news/feed" emacs)
          ("http://www.masteringemacs.org/feed" emacs)
          ("http://emacsredux.com/atom.xml" emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://karthinks.com/index.xml" emacs)
          ("https://themkat.net/feed.xml" emacs dev)
          ("https://cestlaz.github.io/rss.xml" emacs dev)

          ("https://brandur.org/articles.atom" go database dev)
          ("https://nipafx.dev/feed.xml" java dev)
          ("https://vogella.com/blog/feed.xml" java dev)
          ("https://belief-driven-design.com/posts/index.xml" java dev)
          ("https://fzakaria.com/feed.xml" nix java dev)
          ("https://joshaustin.tech/index.xml" java dev)

          ("https://eclecticlight.co/mac-problem-solving/feed" macs dev)

          ("https://www.tweag.io/rss.xml" nix dev)

          ("https://waylonwalker.com/archive/rss.xml" tmux dev)
          ("https://martinfowler.com/feed.atom" dev architecture)

          ("https://lisyarus.github.io/blog/feed.xml" dev gamedev math)
          ("https://highscalability.com/rss/" dev architecture)
          ("https://jvns.ca/atom.xml" dev)
          ("https://www.somkiat.cc/feed/" dev)
          ("https://ayats.org/feed.xml" dev nix rust)
          ("https://www.brendangregg.com/blog/rss.xml" dev system)
          ("https://blog.cloudflare.com/rss" architecture network)
          )))

(use-package elfeed-webkit
  :demand
  :init
  (setq elfeed-webkit-auto-tags '(webkit comics))
  :config
  (elfeed-webkit-auto-enable-by-tag)
  :bind
  (:map elfeed-show-mode-map
        ("t" . elfeed-webkit-toggle)))

(setq mac-option-modifier 'none)
(global-set-key "¥" 'revert-buffer)
(setq mac-command-modifier 'meta)
(global-unset-key (kbd "s-q"))
