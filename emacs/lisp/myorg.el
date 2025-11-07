;;; myorg.el --- Summary Utilities for Org mode -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-id)


;;; Basic inputs

(defcustom myorg-dir "~/pro/memo"
  "A destination of a directory which you place org files."
  :type 'string
  :group 'myorg)

(defcustom exclude-dir-names '("org-archives")
  "A list of directory names to exclude from org files search."
  :type '(repeat string)
  :group 'myorg)

;;; Basic setting

(defun myorg-files ()
  "Return a list of all org files in the `myorg-dir` directory, excluding any that are under a directory named `org-archives`."
  (let ((result '()))
    (dolist (f (directory-files-recursively myorg-dir "org$"))
      (unless (cl-some (lambda (dir) (string-match-p (concat "/" dir "/") f)) exclude-dir-names)
        (push f result)))
    result))

(define-key global-map "\C-cl" 'org-store-link)
;; (add-hook 'org-mode-hook #'org-indent-mode)
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
(customize-set-variable 'org-hide-emphasis-markers t)

;; (defun myorg-face ()
;;   "Set font face for Org mode."
;;   (set-face-attribute 'org-level-1 nil  :height 1.75)
;;   (set-face-attribute 'org-level-2 nil  :height 1.5)
;;   (set-face-attribute 'org-level-3 nil  :height 1.25)
;;   (set-face-attribute 'org-level-4 nil  :height 1.1)
;;   (set-face-attribute 'org-level-5 nil  :height 1.0)
;;   (set-face-attribute 'org-level-6 nil  :height 1.0)
;;   (set-face-attribute 'org-level-7 nil  :height 1.0)
;;   (set-face-attribute 'org-level-8 nil  :height 1.0))
;; (add-hook 'org-mode-hook #'myorg-face)
(add-hook 'org-mode-hook #'org-indent-mode)

;;; Structure editing

(defun gen-uuid ()
  "Generate a new UUID string."
  (interactive)
  (nth 0 (split-string (shell-command-to-string "uuidgen") "\n")))

;;; Org-agenda

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
(defvar org-agenda-plan-column `(:name "Plan" :time-grid t :tag "plan" :tag "@plan"))
(defvar org-agenda-agenda-column `(,org-agenda-plan-column
                                   (:name "🔥 Urgent tasks" :deadline today :time-grid t :order 1)
                                   (:name "🗓️ Scheduled tasks" :scheduled today :time-grid t :order 2)
                                   (:name "🚩 High priority tasks" :priority "A" :time-grid t :order 3)
                                   (:name "⚡ Upcoming tasks" :deadline future :scheduled future :time-grid t :order 4)
                                   (:discard (:todo "DONE"))
                                   (:discard (:todo "POSTPONED"))))
(customize-set-variable
 'org-agenda-custom-commands
 `(("ym" "☑️ Summary of tasks (1m)"
    ((agenda "" ((org-agenda-overriding-header "☑️ Summary of tasks")
                 (org-agenda-overriding-header "🗓️ Agenda")
                 (org-agenda-span 30)
                 (org-agenda-repeating-timestamp-show-all t)
                 (org-super-agenda-groups org-agenda-agenda-column)
                 (org-super-agenda-groups org-agenda-agenda-column)))))
   ("yw" "☑️ Summary of tasks (1w)"
    ((agenda "" ((org-agenda-overriding-header "☑️ Summary of tasks")
                 (org-agenda-overriding-header "🗓️ Agenda")
                 (org-agenda-span 7)
                 (org-agenda-repeating-timestamp-show-all t)
                 (org-super-agenda-groups org-agenda-agenda-column)
                 (org-super-agenda-groups org-agenda-agenda-column)))))
   ("yd" "☑️ Summary of tasks (1d)"
    ((agenda "" ((org-agenda-overriding-header "☑️ Summary of tasks")
                 (org-agenda-overriding-header "🗓️ Agenda")
                 (org-agenda-span 1)
                 (org-agenda-repeating-timestamp-show-all t)
                 (org-super-agenda-groups org-agenda-agenda-column)
                 (org-super-agenda-groups org-agenda-agenda-column)))))
   ("yt" "🏷️ All todo"
    ((alltodo "" ((org-agenda-overriding-header "🏷️ All tags")
                  (org-super-agenda-groups
                   `((:auto-tags)))))))
   ("ye" "❌⌛ Non-estimated tasks"
    ((alltodo "" ((org-agenda-overriding-header "🔥 Not estimated tasks")
                  (org-super-agenda-groups
                   `((:discard (:todo "DONE"))
                     (:discard (:effort> "0:00"))
                     (:auto-tags t)))))))
   ("ys" "❌🕐 Non-scheduled tasks"
    ((alltodo "" ((org-agenda-overriding-header "📅 Non-scheduled tasks")
                  (org-super-agenda-groups
                   `((:discard (:todo "DONE"))
                     (:discard (:scheduled t))
                     (:discard (:deadline t))
                     (:auto-ts t)))))))
   ("yc" "📆 Calendars"
    ((agenda "" ((org-agenda-overriding-header "Calendars")
                 (org-agenda-use-time-grid nil)
                 (org-agenda-span 7)
                 (org-super-agenda-groups
                  `((:name "Plan" :time-grid t :tag "plan" :tag "@plan")
                    (:discard (:anything t))))))))))

;;; Org babel

(org-babel-do-load-languages 'org-babel-load-languages '((ocaml . t)
	                                                     (haskell . t)
	                                                     (emacs-lisp . t)))

;;; Org capture

(global-set-key (kbd "C-c c") #'org-capture)

;; https://orgmode.org/manual/Capture-templates.html
(setopt org-capture-templates
        `(("c" "Short memo" entry (file ,(expand-file-name "CAPTURE_SHORT_MEMO.org" myorg-dir)) "* TODO %^{Title} \n:PROPERTIES:\n:ID: %(gen-uuid)\n:END:\n\n%?\n\n%U")
          ("f" "File template" plain (here) ":PROPERTIES:\n:ID: %(gen-uuid)\n:END:\n#+TITLE: %^{Title}\n#+DATE: %U\n#+FILETAGS: \n\n%?")
          ("h" "New Heading" entry (here) "* %^{Title} \n#+PROPERTIES:\n:ID: %(gen-uuid)\n:END:\n\n%?")
          ("t" "Task" entry (here) "* TODO %^{Title} \nSCHEDULED: %t DEADLINE: %t\n:PROPERTIES:\n:ID: %(gen-uuid)\n:Effort: 0:30\n:END:\n\n%?")))

;;; Org link TODO annotation

(defun my/org--append-state-to-desc (desc state)
  "Append STATE to DESC, removing any existing (TODO)/(DONE)/(WIP)... annotations."
  (let ((desc (or desc "")))
    ;; Remove existing (XXX) at the end
    (setq desc (replace-regexp-in-string
                " ([A-Z]+)\\s-*$" "" desc))
    (let ((new (if (string= desc "") (format "(%s)" state)
                 (format "%s (%s)" desc state))))
      (message "[my/org--append-state-to-desc] updated-desc=%s" new)
      new)))

(defun my/org--get-todo-state-of-link (raw-link)
  "Search the TODO keyword of the heading pointed to by RAW-LINK."
  (message "[my/org--get-todo-state-of-link] checking: %s" raw-link)
  (cond
   ;; Local reference [[*Heading]...]
   ((and raw-link (string-match "\\`\\*+\\(.*\\)\\'" raw-link))
    (let ((heading (match-string 1 raw-link))
          kw)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format "^\\*+\\s-+\\(?:\\(%s\\)\\s-+\\)?%s\\(?:\\s-+\\|$\\)"
                       (regexp-opt org-todo-keywords-1)
                       (regexp-quote heading))
               nil t)
          (setq kw (match-string 1))))
      (when kw (message "[my/org--get-todo-state-of-link] found local -> %s" kw))
      kw))

   ;; file link (with or without file:)
   ((and raw-link (string-match "\\`\\(?:file:\\)?\\([^:]+\\.org\\)::\\*+\\(.*\\)\\'" raw-link))
    (let* ((file (expand-file-name (match-string 1 raw-link)))
           (heading (match-string 2 raw-link))
           kw)
      (message "[my/org--get-todo-state-of-link] file-link file=%s heading=%s" file heading)
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (delay-mode-hooks (org-mode))
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*+\\s-+\\(?:\\(%s\\)\\s-+\\)?%s\\(?:\\s-+\\|$\\)"
                         (regexp-opt org-todo-keywords-1)
                         (regexp-quote heading))
                 nil t)
            (setq kw (match-string 1)))))
      (when kw (message "[my/org--get-todo-state-of-link] found in file -> %s" kw))
      kw))

   ;; id:ID
   ((and raw-link (string-match "\\`id:\\(.+\\)\\'" raw-link))
    (let* ((id (match-string 1 raw-link))
           (marker (ignore-errors (org-id-find id 'marker)))
           kw)
      (when (and marker (markerp marker))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (setq kw (ignore-errors (org-get-todo-state))))))
      (when kw (message "[my/org--get-todo-state-of-link] found by id -> %s" kw))
      kw))

   (t
    (message "[my/org--get-todo-state-of-link] unsupported raw-link format: %s" raw-link)
    nil)))

(defun my/org-refresh-link-todo-annotations ()
  "Added TODO/DONE/WIP annotations to org links' descriptions in the current buffer."
  (interactive)
  (message "[my/org-refresh-link-todo-annotations] start scanning buffer: %s" (buffer-name))
  (let ((count 0)
        (orig-pos (point)))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        ;; Scan for all org links in the buffer
        (while (re-search-forward org-link-bracket-re nil t)
          (let* ((raw-link (match-string 1))
                 (desc (match-string 2))
                 (desc-beg (and (match-beginning 2) (match-beginning 2)))
                 (desc-end (and (match-end 2) (match-end 2))))
            (message "[scan] raw=%s desc=%s beg=%s end=%s" raw-link desc desc-beg desc-end)
            (when raw-link
              (let ((state (my/org--get-todo-state-of-link raw-link)))
                (when state
                  (let* ((current-desc (or desc ""))
                         (new-desc (my/org--append-state-to-desc current-desc state)))
                    (unless (string= new-desc current-desc)
                      ;; description 部分だけを in-place で書き換える
                      (if (and desc-beg desc-end)
                          (progn
                            (message "[update] replacing region %d..%d with '%s'" desc-beg desc-end new-desc)
                            (goto-char desc-beg)
                            (delete-region desc-beg desc-end)
                            (insert new-desc)
                            (goto-char (+ desc-beg (length new-desc))))
                        ;; No description part, replace whole link
                        (let ((whole-beg (match-beginning 0))
                              (whole-end (match-end 0))
                              (new-whole (format "[[%s][%s]]" raw-link new-desc)))
                          (message "[update] replacing whole match %d..%d with '%s'" whole-beg whole-end new-whole)
                          (goto-char whole-beg)
                          (delete-region whole-beg whole-end)
                          (insert new-whole)
                          (goto-char (+ whole-beg (length new-whole)))))
                      (setq count (1+ count)))))))))))
    (goto-char orig-pos)
    (message "[my/org-refresh-link-todo-annotations] done. annotated %d links in %s" count (buffer-name))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my/org-refresh-link-todo-annotations nil t)))

;;; Finalize

(provide 'myorg)

;;; myorg.el ends here
