(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 50 1000 1000))

(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

(setq inhibit-startup-message t)

(defvar laptop-font-height nil)
(defvar monitor-font-height nil)

(eval-and-compile
  (defsubst atkpwn/setup-font-size (height)
    (set-face-attribute 'default nil
                        :family "JetBrainsMono NF" ;; on mac "JetBrainsMono Nerd Font"
                        :height height)
    (set-face-attribute 'variable-pitch nil
                        :font "FiraCode Nerd Font"
                        :weight 'light
                        :height height))
  (defsubst personal-font-setup ()
    (interactive)
    (pcase system-type
      ('gnu/linux (setq laptop-font-height  140
                        monitor-font-height 150))
      ('darwin    (setq laptop-font-height  130
                        monitor-font-height 180))))

  (defsubst share-screen-font-setup ()
    (interactive)
    (global-hl-line-mode)
    (pcase system-type
      ('gnu/linux (setq laptop-font-height  150
                        monitor-font-height 170))
      ('darwin    (setq laptop-font-height  150
                        monitor-font-height 170))))

  (defsubst atkpwn/font-height-from-width (w)
    (if (< w 1440)
        laptop-font-height
      monitor-font-height))

  (defsubst atkpwn/adjust-font-size (frame)
    (let* ((geometry (frame-monitor-attribute 'geometry))
           (width    (cl-fourth geometry))
           (height   (atkpwn/font-height-from-width width)))
      (atkpwn/setup-font-size height))))

(personal-font-setup)
(atkpwn/setup-font-size laptop-font-height)
(add-hook 'window-size-change-functions #'atkpwn/adjust-font-size)

(when (display-graphic-p)
  (setq initial-frame-alist '((width  . 90)
                              (height . 40))))
