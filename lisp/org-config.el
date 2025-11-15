(use-package org
  :bind
  (("C-c s"   . org-store-link)
   ("C-c l"   . org-insert-link)
   ("C-c o a" . org-agenda)
   ("C-c c"   . org-capture)
   :map org-src-mode-map
   ("C-x C-s" . (lambda ()
                  (interactive)
                  (org-edit-src-exit)
                  (save-buffer)))
   :map org-mode-map
   ("C-c h" . consult-org-heading))
  :hook
  (org-mode . (lambda ()
                (eldoc-mode nil)
                (variable-pitch-mode)
                (prettify-symbols-mode 1)))
  (org-agenda-mode . variable-pitch-mode)
  (org-babel-post-tangle . (lambda ()
                             (save-buffer)))
  :custom
  (org-agenda-files `(,(expand-file-name "~/orgfiles/todo.org")))
  (org-confirm-babel-evaluate nil)
  (org-log-done t)
  (org-fontify-quote-and-verse-blocks t)
  :config
  (defun org-babel-tangel-buffer()
    (interactive)
    (org-babel-tangle-file (buffer-file-name)))

  (setq org-export-with-smart-quotes t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t)
     (plantuml   . t)))

  (add-to-list 'org-babel-default-header-args:python
               '(:results . "output"))

  ;; https://orgmode.org/manual/Clean-View.html
  (setq org-adapt-indentation  t
        org-hide-leading-stars t)
  (setq org-hide-emphasis-markers nil))

(use-package org-src
  :config
  (add-to-list 'org-src-lang-modes '("go" . go-ts))
  (add-to-list 'org-src-lang-modes '("python" . python-ts))
  (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts))
  (add-to-list 'org-src-lang-modes '("yml" . yaml-ts))
  (add-to-list 'org-src-lang-modes '("pem" . x509))

  ;; input block 😅
  (add-to-list 'org-src-lang-modes '("input" . fundamental))

  ;; modified from https://emacs.stackexchange.com/a/61442
  (defun org-babel-execute:input (body params)
    "Output BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
    (let ((in-file (org-babel-temp-file "i" ".input")))
      (with-temp-file in-file
        (insert body))
      (org-babel-eval
       (format "cat %s" (org-babel-process-file-name in-file))
       ""))))

(setq org-lowest-priority ?E)

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)

  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
  (org-modern-todo-faces '(("TODO" :background "#d22222")))

  :init
  (global-org-modern-mode)

  :config
  ;; Minimal UI
  (package-initialize)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; (modus-themes-load-operandi)

  ;; Choose some fonts
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "…"))

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("b"  . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("cp" . "src c++"))
  (add-to-list 'org-structure-template-alist '("n"  . "src nix"))
  (add-to-list 'org-structure-template-alist '("j"  . "src java"))
  (add-to-list 'org-structure-template-alist '("go" . "src go")))

(use-package org-roam
  :custom
  (org-roam-directory "~/orgfiles/roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "defualt" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n")
      :unnarrowed t)
     ("t" "theory" plain
      "%?"
      :if-new (file+head "theory/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: :theory:\n")
      :unnarrowed t)
     ("g" "guitar" plain
      "%?"
      :if-new (file+head "guitar/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: :guitar:\n")
      :unnarrowed t)
     ("dev" "dev" plain
      "%?"
      :if-new (file+head "dev/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: :dev:\n")
      :unnarrowed t)
     ("c" "cooking" plain
      "%?"
      :if-new (file+head "cooking/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: :cooking:\n")
      :unnarrowed t)
     ("n" "note" plain
      "%?"
      :if-new (file+head "note/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: :note:\n")
      :unnarrowed t)
     ("p" "personal" plain
      "%?"
      :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+title: ${title}\n#+filetags: :personal:\n")
      :unnarrowed t)
     ))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i"   . completion-at-point))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-modern-indent
  :disabled
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(provide 'org-config)
