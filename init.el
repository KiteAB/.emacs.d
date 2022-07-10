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
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

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

(set-frame-parameter nil 'alpha 0.85)
(global-set-key [(f8)] 'loop-alpha) ;; init-keys.el
(setq alpha-list '((65 45) (55 35) (100 100) (85 55)))
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
