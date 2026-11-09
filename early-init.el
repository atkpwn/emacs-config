;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 50 1000 1000))

(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-jit-compilation)

(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happen asynchronously
  (setq native-comp-jit-compilation t)

  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

(setq inhibit-startup-message t)

(defvar laptop-font-height nil)
(defvar monitor-font-height nil)

(defun atkpwn/setup-font-size (height)
  (set-face-attribute 'default nil
                      :family "JetBrainsMono NF"
                      :height height)
  (set-face-attribute 'variable-pitch nil
                      :font "FiraCode Nerd Font"
                      :weight 'light
                      :height height))

(defun personal-font-setup ()
  (interactive)
  (pcase system-type
    ('gnu/linux (setq laptop-font-height  140
                      monitor-font-height 150))
    ('darwin    (setq laptop-font-height  190
                      monitor-font-height 190))))

(defun share-screen-font-setup ()
  (interactive)
  (global-hl-line-mode 1)
  (setq laptop-font-height  150
        monitor-font-height 170))

(defun atkpwn/font-height-from-width (w)
  (if (< w 1440)
      laptop-font-height
    monitor-font-height))

(defun atkpwn/adjust-font-size (_frame)
  (let* ((geometry (frame-monitor-attribute 'geometry))
         (width    (nth 3 geometry))
         (height   (atkpwn/font-height-from-width width)))
    (atkpwn/setup-font-size height)))

(personal-font-setup)
(atkpwn/setup-font-size laptop-font-height)
(add-hook 'window-size-change-functions #'atkpwn/adjust-font-size)

(when (display-graphic-p)
  (setq initial-frame-alist '((width  . 90)
                              (height . 40))))
