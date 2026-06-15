;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; leaf (package manager)

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config (leaf-keywords-init))

  (leaf leaf-convert
    :ensure t)

  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

;;; package manager

(leaf *cus-edit
  :init
  (setq custom-file (concat user-emacs-directory "custom.el")))

(leaf *builtin
  :init
  (global-auto-revert-mode)
  (show-paren-mode)
  (delete-selection-mode)
  (line-number-mode)
  (column-number-mode)
  (electric-pair-mode t)
  (auto-save-visited-mode t)
  (savehist-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t)
  (setq inhibit-startup-message t)
  (setq display-buffer-alist '(("*Warnings*"
		                        (display-buffer-reuse-window display-buffer-same-window)
                                (inhibit-same-window . nil))
                               ("*Async Shell Command*"
                                (display-buffer-no-window))
                               ("*auto-async-byte-compile*"
                                (display-buffer-no-window))))
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t)))
  (setq backup-directory-alist `(("." . ,(expand-file-name
                                          (concat user-emacs-directory "backups")))))
  (setq dired-listing-switches "-lha")
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq delete-by-moving-to-trash t)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq auto-save-timeout 15)
  (setq auto-save-interval 120)
  (setq auto-save-visited-interval 30)
  (setq auto-save-list-file-prefix nil)
  (setq frame-resize-pixelwise t)
  (setq frame-title-format "%f")
  (setq warning-minimum-level :error)
  (global-unset-key (kbd "C-z"))
  :bind
  ("C-x C-1" . async-shell-command)
  ("s-n" . make-frame)
  ("s-w" . delete-frame)
  ("s-@" . other-frame)
  ([?¥] . [?\\])
  ("s-z" . undo)
  ("s-c" . kill-ring-save)
  ("s-[" . indent-region)
  ("s-r" . replace-string)
  ("s-R" . replace-regexp)
  ("s-/" . comment-region)
  ("C-x C-0" . global-text-scale-adjust)
  ("C-x C-M-0" . text-scale-adjust)
  ("M-z" . toggle-truncate-lines)
  :custom
  (debug-on-error . t)
  (init-file-debug . t)
  (ring-bell-function . 'ignore)
  (create-lockfiles . nil)
  (tab-width . 4)
  (history-length . 1000)
  (history-delete-duplicates . t)
  (indent-tabs-mode . nil)
  (use-dialog-box . nil)
  (use-file-dialog . nil)
  (truncate-lines . t)
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (text-mode-hook . display-line-numbers-mode)
  (conf-mode-hook . display-line-numbers-mode)
  :config
  (set-language-environment "Japanese")
  (set-terminal-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8)
  (setq gc-cons-threshold (* 100 1000 1000)))

(leaf *sys
  :config
  (leaf exec-path-from-shell
    :ensure t
    :when (memq window-system '(mac ns x))
    :config
    (exec-path-from-shell-initialize)
    (dolist (env '("PATH" "MANPATH"))
      (exec-path-from-shell-copy-env env)))

  (leaf auto-async-byte-compile
    :preface
    (defun auto-compile-inits ()
      "Byte-compile Lisp files modified in the directory."
      (interactive)
      (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))
    :ensure t
    :hook
    (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode)
    (kill-emacs-hook . auto-compile-inits)
    :config
    (setq load-prefer-newer t)
    (add-to-list 'display-buffer-alist
                 '("\\*compilation\\*"
                   (display-buffer-no-window))))

  (leaf auth-source
    :config
    (setq auth-file-path "~/.authinfo")))

(leaf *interface
  :config
  (leaf *appearance
    :config
    (leaf fontaine
      :ensure t
      :hook
      (emacs-startup-hook . fontaine-restore-latest-preset)
      (after-init-hook . fontaine-mode)
      (kill-emacs-hook . fontaine-store-latest-preset)
      :custom
      (fontaine-presets . '((regular
                             :default-family "Source Han Code JP"
                             :default-width normal
                             :default-height 115)))
      :config
      (fontaine-set-preset 'regular))
    ;; ずれ確認用 半角40字、全角20字
    ;; AIfUEaiueoAIUEOaiueoAIUEOaiueoAIUEOaiueo ASCII英字
    ;; 0123456789012345678901234567890123456789 ASCII数字
    ;; ｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵ JIS X 0201ｶﾅ
    ;; あいうえおあいうえおあいうえおあいうえお JIS X 0208ひらがな
    ;; アイウエオアイウエオアイウエオアイウエオ 同カタカナ
    ;; ＡＢＣＤＥＡＢＣＤＥＡＢＣＤＥＡＢＣＤＥ 同英字
    ;; 亜唖娃阿哀亜唖娃阿哀亜唖娃阿哀亜唖娃阿哀 同漢字
    ;; 𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽 JIS X 0213漢字

    (leaf paren
      :init (show-paren-mode)
      :custom
      (show-paren-style . 'parenthesis)
      (show-paren-when-point-inside-paren . nil)
      (show-paren-when-point-in-periphery . t))

    (leaf modus-themes
      :ensure t)
    
    (leaf ef-themes
      :ensure t
      :config
      (ef-themes-take-over-modus-themes-mode)
      (modus-themes-load-theme 'ef-oreore))
    
    (leaf rainbow-delimiters
      :ensure t
      :hook
      (prog-mode-hook . rainbow-delimiters-mode)
      (LaTeX-mode-hook . rainbow-delimiters-mode)
      :custom (rainbow-delimiters-outermost-only-face-count . 1))

    (leaf rainbow-mode
      :ensure t
      :hook (elisp-mode-hook . rainbow-mode))
    
    (leaf doom-modeline
      :ensure t
      :hook (after-init-hook . doom-modeline-mode))

    (leaf nerd-icons
      :ensure t)

    (leaf nerd-icons-corfu
      :ensure t
      :config
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

    (leaf nerd-icons-dired
      :ensure t
      :hook (dired-mode-hook . nerd-icons-dired-mode))

    (leaf volatile-highlights
      :ensure t
      :init (volatile-highlights-mode))

    ;; (leaf highlight-indent-guides
    ;;   :ensure t
    ;;   :hook ((text-mode-hook . highlight-indent-guides-mode)
    ;;          (prog-mode-hook . highlight-indent-guides-mode))
    ;;   :custom ((highlight-indent-guides-method . 'fill)))
    )

  (leaf *help
    :config
    (leaf helpful
      :ensure t
      :bind
      ("C-h f" . helpful-callable)
      ("C-h v" . helpful-variable)
      ("C-h k" . helpful-key)
      ("C-h C" . helpful-command)
      :custom
      (helpful-max-buffers . 5))

    (leaf which-key
      :init (which-key-mode)))

  (leaf *ja
    :config
    (leaf mozc
      :ensure t
      :config
      (set-language-environment "Japanese")
      (setq default-input-method "japanese-mozc")
      (prefer-coding-system 'utf-8)))
  
  (leaf *layout
    :config
    (leaf winner-mode
      :init (winner-mode))

    (leaf win-switch
      :ensure t
      :config (win-switch-setup-keys-ijkl "\C-xo"))

    (leaf popwin
      :ensure t
      :init (popwin-mode)
      :config (setq display-buffer-function 'popwin:display-buffer)))

  (leaf *nav
    :config
    (leaf block-nav
      :ensure t
      :bind
      ("C-c C-n" . block-nav-next-block)
      ("C-c C-p" . block-nav-previous-block)
      ("C-c M-a" . block-nav-previous-indentation-level)
      ("C-c M-e" . block-nav-next-indentation-level))

    (leaf puni
      :ensure t
      :init (puni-global-mode)
      :hook (term-mode-hook . puni-disable-puni-mode)))
  
  (leaf *modeline
    :config
    (leaf vertico
      :ensure t
      :hook ((after-init-hook . vertico-mode))
      :custom
      (vertico-count . 10)
      (vertico-cycle . t)
      (vertico-resize . nil)
      :bind (:vertico-map
             ("DEL" . vertico-directory-delete-char)))

    (leaf consult
      :ensure t
      :bind
      ("S-l" . consult-goto-line)
      ("C-c s" . consult-ripgrep)
      ("C-c h" . consult-outline))

    (leaf marginalia
      :ensure t
      :hook (after-init-hook . marginalia-mode)))
  
  (leaf *completion
    :config
    (leaf corfu
      :ensure t
      :init
      (global-corfu-mode)
      (corfu-history-mode)
      :hook (corfu-mode-hook . corfu-popupinfo-mode)
      :custom
      (corfu-auto . t)
      (corfu-auto-delay . 0.2)
      (corfu-auto-prefix . 2)
      (corfu-cycle . t)
      (corfu-on-exact-match . nil)
      (corfu-quit-no-match . t)
      (tab-always-indent . 'complete)
      :bind (:corfu-map
             ("RET" . corfu-insert)
             ("<return>" . corfu-insert)))
    
    (leaf cape
      :ensure t
      :bind ("M-p" . completion-at-point)
      :init
      (add-hook 'completion-at-point-functions #'cape-file)
      (add-hook 'completion-at-point-functions #'cape-elisp-block)
      (add-hook 'completion-at-point-functions #'cape-emoji)
      (add-hook 'completion-at-point-functions #'cape-tex))
    
    (leaf orderless
      :ensure t
      :custom
      (completion-category-defaults . nil)
      (completion-category-overrides '((file (styles partial-completion))))
      (completion-styles . '(orderless basic)))))

(leaf *history
  :config
  (leaf saveplace
    :ensure t
    :hook (after-init-hook . save-place-mode))

  (leaf undo-fu-session
    :ensure t
    :hook (after-init-hook . undo-fu-session-global-mode)))

(leaf *edit
  :config
  (leaf tempel
    :preface
    (defun tempel-setup-capf ()
      (setq-local completion-at-point-functions
                  (cons #'tempel-expand
                        completion-at-point-functions)))
    (defun tempel-include (elt)
      (when (eq (car-safe elt) 'i)
        (if-let (template (alist-get (cadr elt) (tempel--templates)))
            (cons 'l template)
          (message "Template %s not found" (cadr elt))
          nil)))
    :ensure t
    :hook
    (text-mode-hook . tempel-setup-capf)
    (prog-mode-hook . tempel-setup-capf)
    (conf-mode-hook . tempel-setup-capf)
    :bind
    ("M-+" . tempel-complete)
	("M-*" . temple-insert)
    (:tempel-map
     ("<tab>" . tempel-next)
     ("C-<tab>" . tempel-previous))
    :custom (tempel-path . "~/.emacs.d/templates")
    :config (add-to-list 'tempel-user-elements #'tempel-include))

  (leaf anzu
    :ensure t
    :hook (after-init-hook . global-anzu-mode)))

(leaf *prog
  :config
  (leaf magit
    :ensure t
    :bind ("C-x g" . magit-status))

  (leaf diff-hl
    :ensure t
    :hook
    (after-init-hook . global-diff-hl-mode)
    (dired-mode-hook . diff-hl-dired-mode)
    (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
    (magit-post-refresh-hook . diff-hl-magit-post-refresh))

  (leaf vterm
    :ensure t
    :bind (:vterm-mode-map
           ("M-v" . vterm-send-prior)
           ("C-v" . vterm-send-next))
    :custom
    (vterm-max-scrollback . 100000)
    (vterm-buffer-name-string . "*vterm*: %s")
    (vterm-keymap-exceptions . '("<f1>" "<f2>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "C-y" "M-y")))

  (leaf lsp-mode
    :ensure t
    :init
    (defun my/orderless-dispatch-flex-first (_pattern index _total)
      (and (eq index 0) 'orderless-flex))
    
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))
      ;; Optionally configure the first word as flex filtered.
      (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
      ;; Optionally configure the cape-capf-buster.
      (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
    ;; Reference: https://github.com/minad/corfu/wiki
    :hook
    (dired-mode-hook . lsp-dired-mode)
    (lsp-mode-hook . lsp-enable-which-key-integration)
    (lsp-mode-hook . lsp-inlay-hints-mode)
    (lsp-completion-mode-hook . my/lsp-mode-setup-completion)
    :custom
    (lsp-ocaml-lsp-server-command . '("ocamllsp" "--fallback-read-dot-merlin"))
    (lsp-completion-provider . :none)
    :config
    (setq lsp-idle-delay 0.5)
    (setq lsp-log-io nil))

  (leaf lsp-ui
    :ensure t
    :hook lsp-mode-hook)
  
  (leaf eglot
    :ensure t
    :config
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(LaTeX-mode . ("harper-ls" "--stdio"))))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (leaf flycheck
    :ensure t
    :hook (lsp-mode-hook . flycheck-mode)
    :bind (:flycheck-mode-map
           ("C-c M-n" . flycheck-next-error)
	       ("C-c M-p" . flycheck-previous-error))
    :custom
    (flycheck-grammarly-check-time . 0.8))

  (leaf flycheck-eglot
    :ensure t)

  (leaf projectile
    :ensure t
    :hook (after-init-hook . projectile-mode)
    :bind (:projectile-mode-map
           ("C-c p" . projectile-command-map)))
  
  (leaf dumb-jump
    :ensure t
    :hook (xref-backend-functions . dumb-jump-xref-activate)
    :custom (dumb-jump-force-searcher . 'rg)
    :bind ("C-c g" . dumb-jump-go))

  (leaf copilot
    :ensure t
    :bind (:copilot-completion-map
           ("<tab>" . copilot-accept-completion)
           ("TAB" . copilot-accept-completion)
           ("C-<tab>" . copilot-accept-completion-by-word)
           ("C-TAB" . copilot-accept-completion-by-word)
           ("C-n" . copilot-next-completion)
           ("C-p" . copilot-previous-completion))))

(leaf *lang
  :config
  (leaf tuareg
    :mode
    ("\\.ml\\'" . tuareg-mode)
    ("\\.mli\\'" . tuareg-mode)
    ("\\.mly\\'" . tuareg-mode)
    :ensure t
    :hook
    (tuareg-mode-hook . lsp)
    (tuareg-mode-hook . copilot-mode))
  
  (leaf dune
    :mode ("\\dune\\'" . dune-mode)
    :hook
    (dune-mode-hook . copilot-mode))

  (leaf elisp-mode
    :hook
    (emacs-lisp-mode-hook . flycheck-mode)
    (emacs-lisp-mode-hook . rainbow-delimiters-mode)
    :config
    (leaf macrostep
      :ensure t
      :bind ("C-c e" . macrostep-expand)))

  (leaf nix-mode
    :ensure t
    :mode ("\\.nix\\'" . nix-mode)
    :hook
    (nix-mode-hook . lsp)
    (nix-mode-hook . copilot-mode))

  (leaf haskell-mode
    :ensure t
    :custom (haskell-stylish-on-save . t)
    :hook
    (haskell-mode-hook . lsp)
    (haskell-mode-hook . copilot-mode)
    :config
    (leaf lsp-haskell
      :ensure t))

  (leaf *latex
    :mode
    ("\\.tex\\'" . LaTeX-mode)
    ("\\.sty\\'" . LaTeX-mode)
    ("\\.cls\\'" . LaTeX-mode)
    ("\\.ltx\\'" . LaTeX-mode)
    ("\\.bib\\'" . bibtex-mode)
    :hook
    (LaTeX-mode-hook . lsp)
    (LaTeX-mode-hook . copilot-mode)
    :config
    (leaf auctex
      :ensure t
      :custom
      (TeX-auto-save . t)
      (TeX-parse-self . t)
      (TeX-master . nil)
      (TeX-PDF-mode . t)
      (TeX-brace-indent-level . 2)
      (TeX-source-correlate-mode . t)
      (TeX-source-correlate-method . 'synctex)
      (LaTeX-item-indent . 2)
      (LaTeX-indent-level . 2)
      (LaTeX-item-indent-level . 0))
    
    (leaf ebib
      :ensure t)

    (leaf bibtex
      :ensure t
      :custom
      (bibtex-autokey-year-length . 4)
      (bibtex-autokey-name-year-separator . "-")
      (bibtex-autokey-year-title-separator . "-")
      (bibtex-autokey-titleword-separator . "-")
      (bibtex-autokey-titlewords . 2)
      (bibtex-autokey-titlewords-stretch . 1)
      (bibtex-autokey-titleword-length . 5)))

  (leaf pdf-tools
    :ensure t
    :init (pdf-tools-install)
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :hook
    (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
    (pdf-view-mode-hook . (lambda () (auto-revert-mode 1)))
    :bind (:pdf-view-mode-map
           ("C-s" . isearch-forward)
           ("+" . pdf-view-enlarge))
    :custom (pdf-view-resize-factor . 1.1))

  (leaf typescript-mode
    :ensure t
    :preface
    (define-derived-mode typescript-tsx-mode typescript-mode
      "typescript-tsx")
    :mode
    ("\\.tsx?\\'" . typescript-tsx-mode)
    :hook
    (typescript-mode-hook . lsp)
    (typescript-mode-hook . (lambda ()
                              (setq-local lsp-enabled-clients '(ts-ls))
                              (lsp))))

  (leaf fsharp-mode
    :ensure t)
  
  (leaf eglot-fsharp
	:hook (fsharp-mode-hook)
	:ensure t)

  (leaf markdown-mode
    :ensure t
    :mode ("\\.md\\'" . gfm-mode))

  (leaf *prolog
    :mode ("\\.pl\\'" . prolog-mode)
    :hook
    (prolog-mode-hook . copilot-mode))

  (leaf rust-mode
    :ensure t
    :hook
    (rust-mode-hook . lsp)
    :custom
    (lsp-rust-server . 'rls))

  (leaf cargo
    :ensure t
    :hook (rust-mode-hook . cargo-minor-mode))

  (leaf rocq ;coq
    :custom (coq-prog-name . "~/.opam/default/bin/coqtop")
    :hook
    (coq-mode-hook . copilot-mode)
    :config
    (leaf proof-general
      :ensure t))

  (leaf c-mode
    :hook
    (c-mode-hook . lsp)
    (c-mode-hook . copilot-mode)
    )

  (leaf scala-mode
    :ensure t)

  (leaf sbt-mode
    :ensure t)

  (leaf lsp-metals
    :ensure t
    :hook
    (scala-mode-hook . lsp)
    :config
    (setq lsp-metals-server-command (or (executable-find "metals") "metals"))
    (setq lsp-warn-no-matched-clients nil))

  (leaf groovy-mode
    :ensure t
    :hook
    (groovy-mode-hook . lsp)
    (groovy-mode-hook . copilot-mode))

  (leaf lsp-java
    :ensure t
    :hook
    (java-mode-hook . lsp-mode))

  (leaf lsp-sonarlint
    :ensure t
    :custom
    (lsp-sonarlint-auto-download . t)
    :config
    (lsp-sonarlint-enabled-analyzers '("java" "cfamily" "python" "text"))
    (setq lsp-sonarlint-analyzers
          '("~/sonar-extension/sonarjava.jar"
            "~/sonar-extension/sonarxml.jar"
            "~/sonar-extension/sonartext.jar"
            "~/sonar-extension/sonarhtml.jar"
            "~/sonar-extension/sonarjs.jar"
            ))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("sonarlint-language-server"))
      :major-modes '(java-mode java-ts-mode)
      :add-on? t
      :server-id 'sonarlint-java)))

  (leaf fish-mode
    :ensure t)
  
  (leaf python-mode
    :hook
    (python-mode-hook . lsp)
    (python-mode-hook . copilot-mode))

  (leaf lsp-pyright
      :ensure t
      :custom (lsp-pyright-langserver-command . "pyright")))

(leaf *org
  :config
  (leaf org-fragtog
    :url "https://github.com/io12/org-fragtog"
    :ensure t
    :hook (org-mode-hook . org-fragtog-mode))

  (leaf org-appear
    :url "https://github.com/awth13/org-appear"
    :ensure t
    :hook (org-mode-hook . org-appear-mode)
    :config
    (setq org-appear-trigger 'always))

  (define-key global-map "\C-cl" 'org-store-link)
  (customize-set-variable 'org-startup-folded 'showall)
  (customize-set-variable 'org-outline-path-complete-in-steps nil)
  (customize-set-variable 'org-adapt-indentation nil)
  (customize-set-variable 'org-refile-targets `((,(myorg-files) :maxlevel . 4))) ;; https://doc.endlessparentheses.com/Var/org-refile-targets.html
  (customize-set-variable 'org-refile-use-outline-path 'file)
  (customize-set-variable 'org-use-tag-inheritance "^@")
  (customize-set-variable 'org-startup-with-inline-images t)
  (customize-set-variable 'org-log-done 'time)
  (customize-set-variable 'org-todo-keywords
                          '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCEL(c)" "Postponed(p)")))
  (customize-set-variable 'org-startup-indented nil)
  (customize-set-variable 'org-startup-latex-with-latex-preview nil)
  (customize-set-variable 'org-format-latex-options '(plist-put org-format-latex-options :scale 1.5))
  (customize-set-variable 'org-priority-highest 1)
  (customize-set-variable 'org-priority-lowest 100)
  (customize-set-variable 'org-priority-default 10)

  (defun gen-uuid ()
    "Generate a new UUID string."
    (interactive)
    (nth 0 (split-string (shell-command-to-string "uuidgen") "\n")))

  (define-key global-map "\C-ca" 'org-agenda)
  (customize-set-variable 'org-agenda-start-on-weekday 0)
  (customize-set-variable 'org-agenda-skip-additional-timestamps-same-entry nil)
  (customize-set-variable 'org-agenda-span 1)
  (customize-set-variable 'org-agenda-include-diary t)
  (customize-set-variable 'org-agenda-prefix-format `((agenda . " %i %-12:c%?-12t% s")
                                                      (todo . " %i %-12:c %t %s")
                                                      (tags . " %i %-12:c")
                                                      (search . " %i %-12:c")))
  (customize-set-variable 'org-agenda-use-time-grid nil)
  (customize-set-variable 'org-agenda-time-grid `((daily today require-timed remove-match)
                                                  ,(cl-loop for m from 0 below 1440 by 30
                                                            collect
                                                            (format "%02d%02d" (/ m 60)
                                                                    (% m 60)))
                                                  "......" "----------------"))
  (customize-set-variable 'org-agenda-date-format "%Y-%m-%d (%a)")
  (customize-set-variable 'org-agenda-window-setup 'other-window)
  (customize-set-variable 'org-agenda-columns-add-appointments-to-effort-sum t)
  (customize-set-variable 'org-columns-default-format "%68ITEM(Task) %6Effort(Effort){:} %6CLOCKSUM(Clock){:}")
  (customize-set-variable 'org-agenda-files (myorg-files))
  (customize-set-variable 'org-complete-tags-always-offer-all-agenda-tags t)
  (customize-set-variable 'org-agenda-columns-add-appointments-to-effort-sum t)
  (defvar org-agenda-agenda-column `((:discard (:not (:todo "TODO")))
                                     (:discard (:and (:scheduled future :deadline future)))
                                     (:name "💀 Missed tasks" :scheduled past :deadline past :order 3)
                                     (:name "Today's tasks" :order 4)))
  (customize-set-variable
   'org-agenda-custom-commands
   `(("yf" "🫤 Forgotten tasks"
      ((alltodo "" ((org-agenda-overriding-header "🫤 Forgotten tasks")
                    (org-super-agenda-groups
                     `((:todo "POSTPONED")
                       (:discard (:anything t))))))))
     ("yt" "🏷️ All Tags"
      ((tags "-ARCHIVE-toc" ((org-agenda-overriding-header "🏷️ All tags")
                             (org-super-agenda-groups
                              `((:auto-tags)))))))
     ("ye" "🔥 Not estimated tasks"
      ((alltodo "" ((org-agenda-overriding-header "🔥 Not estimated tasks")
                    (org-super-agenda-groups
                     `((:discard (:todo "DONE"))
                       (:discard (:effort> "0:00"))
                       (:auto-tags t)))))))
     ("ys" "📝 Non-scheduled tasks"
      ((alltodo "" ((org-agenda-overriding-header "📅 Non-scheduled tasks")
                    (org-super-agenda-groups
                     `((:discard (:todo "DONE"))
                       (:discard (:scheduled t))
                       (:discard (:deadline t))
                       (:auto-ts t)))))))
     ("yw" "🗓️ Agenda (1 day)"
      ((agenda "" ((org-agenda-overriding-header "🗓️ Agenda")
                   (org-agenda-span 1)
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-super-agenda-groups org-agenda-agenda-column)))))
     ("ya" "🗓️ Agenda (1 week)"
      ((agenda "" ((org-agenda-span 7)
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-agenda-overriding-header "🗓️ Agenda")
                   (org-super-agenda-groups org-agenda-agenda-column)))))
     ("yz" "Scheduled tasks (1 week)"
      ((agenda "" ((org-agenda-overriding-header "Scheduled tasks (1 week)")
                   (org-agenda-span 7)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
                   (org-agenda-include-deadlines nil)
                   (org-super-agenda-groups
                    `((:name "Plan" :time-grid t :tag "plan" :tag "@plan")
                      (:name "Scheduled task" :scheduled t :time-grid t)
                      (:discard (:anything t))))))))
     ("yd" "🕣 Plan"
      ((agenda "" ((org-agenda-overriding-header "🕣 Plan")
                   (org-agenda-use-time-grid nil)
                   (org-super-agenda-groups
                    `((:name "Plan" :time-grid t :tag "plan" :tag "@plan")
                      (:discard (:anything t))))))))))

  (org-babel-do-load-languages 'org-babel-load-languages '((ocaml . t)
	                                                       (haskell . t)
	                                                       (emacs-lisp . t)))

  (global-set-key (kbd "C-c c") #'org-capture)

  ;; https://orgmode.org/manual/Capture-templates.html
  (setopt org-capture-templates
          `(("c" "Short memo" entry (file ,(expand-file-name "CAPTURE_SHORT_MEMO.org" myorg-dir)) "* TODO %^{Title} \n:PROPERTIES:\n:ID: %(gen-uuid)\n:END:\n\n%?\n\n%U")
            ("f" "File template" plain (here) ":PROPERTIES:\n:ID: %(gen-uuid)\n:END:\n#+TITLE: %^{Title}\n#+DATE: %U\n#+FILETAGS: \n\n%?")
            ("h" "New Heading" entry (here) "* %^{Title} \n#+PROPERTIES:\n:ID: %(gen-uuid)\n:END:\n\n%?")
            ("t" "Task" entry (here) "* TODO %^{Title} \nSCHEDULED: %t DEADLINE: %t\n:PROPERTIES:\n:ID: %(gen-uuid)\n:Effort: 0:30\n:END:\n\n%?"))))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init)
;;; init.el ends here
