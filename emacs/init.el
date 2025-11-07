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

(leaf *mylib
  :init (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  :config
  (leaf mytime
    :require t))

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
  (global-unset-key (kbd "C-z"))
  (setopt desktop-save-mode t)
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
  (debug-on-error . nil)
  (init-file-debug . t)
  (ring-bell-function . 'ignore)
  (create-lockfiles . nil)
  (tab-width . 4)
  (history-length . 1000)
  (history-delete-duplicates . t)
  (indent-tabs-mode . nil)
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (text-mode-hook . display-line-numbers-mode)
  (conf-mode-hook . display-line-numbers-mode))

(leaf *sys
  :config
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
                   (display-buffer-no-window)))))

(leaf *interface
  :config
  (leaf *appearance
    :when (display-graphic-p)
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
                             :default-height 120)))
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

    (leaf ef-themes
      :ensure t
      :config
      (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
      (ef-themes-select 'ef-oreore)
      ;; (ef-themes-select 'ef-oreoredark)
      (load-theme 'ef-oreore :no-confirm))
    
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
      :after corfu nerd-icons
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
      (helpful-max-buffers . 5)))

  (leaf *ja
    (leaf mozc
      :ensure t
      :config
      (set-language-environment "Japanese")
      (setq default-input-method "japanese-mozc")
      (prefer-coding-system 'utf-8)))
  
  (leaf *layout
    :config
    (leaf *my/move-frame
      :preface
      (defun move-and-maximize-all-frames-to-monitor (monitor-index)
        (let* ((frame-list (frame-list))
               (screen-attributes (display-monitor-attributes-list)))
          (if (and (>= monitor-index 0)
                   (< monitor-index (length screen-attributes)))
              (let ((target-monitor (nth monitor-index screen-attributes)))
                (let* ((geometry (cdr (assq 'geometry target-monitor)))
                       (x (nth 0 geometry))
                       (y (nth 1 geometry)))
                  (dolist (frame frame-list)
                    (set-frame-position frame x y)
                    (toggle-frame-maximized frame))))
            (error "Invalid monitor index"))))

      (defun move-and-maximize-all-frames-to-next-monitor ()
        "Move all Emacs frames to the next monitor in sequence and maximize them."
        (interactive)
        (let* ((screen-attributes (display-monitor-attributes-list))
               (num-monitors (length screen-attributes)))
          (if (= num-monitors 0)
              (error "No monitors detected")
            (let* ((current-monitor (frame-monitor-attributes))
                   (current-geometry (assq 'geometry current-monitor))
                   (current-index (cl-position current-geometry screen-attributes
                                               :test (lambda (a b) (equal (cdr a) (cdr (assq 'geometry b)))))))
              (if (null current-index)
                  (error "Unable to determine current monitor index")
                (let ((next-index (mod (1+ current-index) num-monitors)))
                  (move-and-maximize-all-frames-to-monitor next-index)
                  (message "Moved all frames to monitor %d" next-index)))))))

      :bind ("C-c m" . move-and-maximize-all-frames-to-next-monitor))

    (leaf activities
      :ensure t
      :hook (after-init-hook . activities-mode)
      :bind
      ("C-x C-a C-n" . activities-new)
      ("C-x C-a C-d" . activities-define)
      ("C-x C-a C-a" . activities-resume)
      ("C-x C-a C-s" . activities-suspend)
      ("C-x C-a C-k" . activities-kill)
      ("C-x C-a <return>" . activities-switch)
      ("C-x C-a b" . activities-switch-buffer)
      ("C-x C-a g" . activities-revert)
      ("C-x C-a l" . activities-list))

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
      ("C-c M-e" . block-nav-next-indentation-level)))
  
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
      (corfu-auto-delay . 0)
      (corfu-auto-prefix . 1)
      (corfu-cycle . t)
      (corfu-on-exact-match . nil)
      (corfu-quit-no-match . t)
      (tab-always-indent . 'complete)
      :bind (:corfu-map
             ("TAB" . corfu-next)
             ("<tab>" . corfu-next)
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

(leaf *programming
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
    :ensure t)

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
    (lsp-mode-hook . lsp-enable-which-key-integration)
    (lsp-completion-mode-hook . my/lsp-mode-setup-completion)
    :custom
    (lsp-ocaml-lsp-server-command . '("ocamllsp" "--fallback-read-dot-merlin"))
    (lsp-completion-provider . :none))

  (leaf lsp-ui
    :after lsp-mode
    :ensure t
    :hook lsp-mode-hook)
  
  (leaf eglot
    :ensure t
    :config
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(LaTeX-mode . ("harper-ls" "--stdio")))))

  (leaf flycheck
    :ensure t
    :hook (lsp-mode-hook . flycheck-mode)
    :bind (:flycheck-mode-map
           ("C-c M-n" . flycheck-next-error)
	       ("C-c M-p" . flycheck-previous-error))
    :custom
    (flycheck-grammarly-check-time . 0.8))

  (leaf flycheck-eglot
    :after eglot
    :ensure t)

  (leaf projectile
    :ensure t
    :hook (after-init-hook . projectile-mode)
    :bind (:projectile-mode-map
           ("C-c p" . projectile-command-map)))

  (leaf copilot
    :el-get (copilot
             :type github
             :pkgname "zerolfx/copilot.el")
    :bind (:copilot-completion-map
           ("<tab>" . copilot-accept-completion)
           ("TAB" . copilot-accept-completion))
    :config
    (setopt copilot-indent-offset-warning-disable t))

  (leaf reformatter
    :ensure t
    :config
    (reformatter-define ocamlformat
      :program "ocamlformat"
      :args '("--enable-outside-detected-project" "--name" ,buffer-file-name "-"))
    (reformatter-define latexformat
      :program "latexindent"
      :args `("-m" "-l" "-w"))))

(leaf *lang
  :config
  (leaf tuareg
    :ensure t
    :bind (:tuareg-mode-map
           ("C-<tab>" . ocamlformat)
           ("C-TAB" . ocamlformat))
    :hook
    (tuareg-mode-hook . copilot-mode)
    (tuareg-mode-hook . lsp)
    (tuareg-mode-hook . ocamlformat-on-save-mode)
    :config
    (leaf dune
      :mode ("\\dune\\'" . dune-mode)))

  (leaf elisp-mode
    :hook
    (emacs-lisp-mode-hook . flycheck-mode)
    (emacs-lisp-mode-hook . copilot-mode)
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
    :bind (:LaTeX-mode-map
           ("C-<tab>" . latexformat)
           ("C-TAB" . latexformat))
    :hook
    (LaTeX-mode-hook . lsp)
    ;; (LaTeX-mode-hook . latexformat-on-save-mode)
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
    :after lsp
    :ensure t
    :hook
    (typescript-mode-hook . lsp)
    (typescript-mode-hook . copilot-mode))

  (leaf svelte-mode
    :mode ("\\.svelte\\'" . svelte-mode)
    :ensure t
    :custom (svelte-basic-offset . 2)
    :hook
    (svelte-mode-hook . lsp)
    (svelte-mode-hook . copilot-mode))

  (leaf sass-mode
    :ensure t)
  
  (leaf fsharp-mode
    :ensure t
    :hook (fsharp-mode-hook . copilot-mode)
    :config
    (leaf eglot-fsharp
	  :hook (fsharp-mode-hook)
	  :ensure t))

  (leaf markdown-mode
    :ensure t
    :mode ("\\.md\\'" . gfm-mode))

  (leaf *yaml
    :preface
    (el-get-bundle yaml-mode
      :type github
      :pkgname "yoshiki/yaml-mode")
    :mode
    ("\\.yml\\'" . yaml-mode)
    ("\\.yaml\\'" . yaml-mode))

  (leaf *prolog
    :mode ("\\.pl\\'" . prolog-mode)
    :hook (prolog-mode-hook . copilot-mode))

  (leaf rust-mode
    :ensure t
    :after lsp-mode
    :hook
    (rust-mode-hook . copilot-mode)
    (rust-mode-hook . lsp)
    :custom
    (rust-format-on-save . t)
    (lsp-rust-server . 'rls)
    :config
    (leaf cargo
      :ensure t
      :hook (rust-mode-hook . cargo-minor-mode)))

  (leaf rocq ;coq
    :hook (coq-mode-hook . copilot-mode)
    :custom (coq-prog-name . "~/.opam/default/bin/coqtop")
    :config
    (leaf proof-general
      :ensure t))

  (leaf c-mode
    :hook
    (c-mode-hook . lsp)
    (c-mode-hook . copilot-mode))

  (leaf scala-mode
    :ensure t)
  
  ;; (leaf *agda
  ;;   :config
  ;;   (load-file (let ((coding-system-for-read 'utf-8))
  ;;                (shell-command-to-string "agda --emacs-mode locate"))))

  (leaf fish-mode
    :ensure t
    :hook (fish-mode-hook . copilot-mode))

  (leaf python-mode
    :hook
    (python-mode-hook . copilot-mode)
    (python-mode-hook . lsp)
    :config
    (leaf lsp-pyright
      :ensure t
      :custom (lsp-pyright-langserver-command . "pyright"))))


(leaf *org
  :config
  (leaf myorg
    :require t
    :custom (myorg-dir . "~/pro/memo/"))

  (leaf org-gcal
    :ensure t
    :config
    (setq org-gcal-client-id (plist-get (nth 0 (auth-source-search :host "googleusercontent.com")) :user))
    (setq org-gcal-client-secret (funcall (plist-get (nth 0 (auth-source-search :host "googleusercontent.com" :max 1)) :secret))))
  
  (leaf org-ref
    :ensure t
    :bind (:org-mode-map
           ("C-c ]" . org-ref-insert-link))
    :custom
    (bibtex-completion-bibliography . '("~/pro/memo/common/my.bib"))
    (bibtex-completion-pdf-open-function . (lambda (fpath) (call-process "open" nil 0 nil fpath))))
  
  (leaf org-ql
    :ensure t
    :url "alphapapa/org-ql")

  (leaf org-super-agenda
    :ensure t
	:init (org-super-agenda-mode))

  (leaf org-modern
    :ensure t
    :init (global-org-modern-mode))

  (leaf org-fragtog
    :url "https://github.com/io12/org-fragtog"
    :ensure t
    :hook (org-mode-hook . org-fragtog-mode))

  (leaf org-remark
    :ensure t
    :init (org-remark-global-tracking-mode t)
    :bind ("C-c n m" . org-remark-mark))

  (leaf org-transclusion
    :ensure t
    :bind
    ("<f12>" . org-transclusion-add-all)
    ("C-c o t m" . org-transclusion-mode))
  
  (leaf org-timeblock
    :ensure t))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init)
;;; init.el ends here
