;;; -*- lexical-binding: t -*-
(require 'cl-lib)

(setq backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      package-enable-at-startup nil
      custom-file "~/.emacs.d/custom.el")

;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-start.el
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   (not (file-directory-p (concat dir subdir)))
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))
          (add-to-list 'load-path subdir-path t))
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path "~/.emacs.d")
(require 'init-straight)

;; UI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq inhibit-splash-screen t)


(straight-use-package
 '(joker-theme :type git :host github :repo "DogLooksGood/joker-theme"))
(require 'joker-theme)
(load-theme 'joker t)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))
;; (toggle-frame-fullscreen)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-frame-parameter nil 'alpha '(90 . 100))
(global-set-key [(f8)] 'loop-alpha) ;; init-keys.el
(setq alpha-list '((70 50) (60 40) (100 100) (90 60)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
  )

(setq make-backup-files nil           ;; Don't let Emacs make up backup file
      create-lockfiles nil            ;; Don't make lockfile
      auto-save-default nil           ;; Don't auto save the file
      auto-save-list-file-prefix nil) ;; Don't make auto-save-list folder

(setq display-time-day-and-date t)
(display-time-mode 1)
(setq display-time-format "%H:%M")
(setq display-time-24hr-format nil)

(straight-use-package 'wakatime-mode)
(global-wakatime-mode 1)

(fset #'yes-or-no-p #'y-or-n-p)
(delete-selection-mode 1)
(show-paren-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\( . ?\))
        (?\< . ?\>)
        (?\{ . ?\})))
(electric-pair-mode 1)

(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method nil)
(setq load-prefer-newer t)
(save-place-mode 1)
(setq ring-bell-function 'ignore
      blink-cursor-mode nil)
(setq inhibit-compacting-font-caches t)
(global-auto-revert-mode t)
(setq x-underline-at-descent-line t)
(setq underline-minimum-offset 0)
(setq mouse-yank-at-point nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq en-font-name "Fira Code Nerd Font Mono"
      en-font-style "Regular"
      en-font-size 24)

(setq zh-font-name "Sarasa Mono SC"
      zh-font-style "Regular"
      zh-font-size 24)

(progn
  (if (fontp (font-spec
              :name en-font-name
              :style en-font-style
              :size en-font-size))
      (progn
        (set-face-attribute 'default nil
                            :font (font-spec
                                   :name en-font-name
                                   :style en-font-style
                                   :size en-font-size))
        (set-fontset-font t 'han (font-spec
                                  :name zh-font-name
                                  :style zh-font-style
                                  :size zh-font-size))
        (set-fontset-font t ?中 (font-spec
                                 :name zh-font-name
                                 :style zh-font-style
                                 :size zh-font-size))
        (set-fontset-font "fontset-default" ?༼ (font-spec
                                                :name "Noto Serif Tibetan"
                                                :size 0)))
    (message "Can't find %s font." en-font-name)))

;; The Quick Brown Fox Jumps Over The Lazy Dog
;; 中英文等宽字体测试 - 敏捷的棕色狐狸跳过懒狗

(straight-use-package 'smooth-scrolling)
(setq smooth-scroll-margin 3)
(smooth-scrolling-mode 1)

(advice-add #'display-startup-echo-area-message :override #'(lambda () (interactive) (message "")))
(setq initial-scratch-message (format ";; Emacs Startup Time: %.2fs

" (float-time (time-subtract after-init-time before-init-time))))

(straight-use-package 'which-key)
(which-key-mode 1)

(straight-use-package 'window-numbering)
(window-numbering-mode 1)

(straight-use-package 'meow)
(require 'meow)
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
(meow-setup)
;;(meow-global-mode 1)

(require 'init-modeline)
