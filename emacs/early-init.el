;;; early-init.el --- Early Initialization. -*- lexical-binding: t; no-byte-compile: t -*-

(with-eval-after-load 'scroll-bar
  (scroll-bar-mode -1))

(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))

(with-eval-after-load 'menu-bar
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                (lambda nil (menu-bar-mode -1)))
    (menu-bar-mode -1)))

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

(setq frame-inhibit-implied-resize t)

(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
			(setq inhibit-message nil)
			(redisplay)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
