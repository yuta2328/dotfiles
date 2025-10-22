;;; myorg.el --- Summary Utilities for Org mode -*- lexical-binding: t; -*-

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-capture)

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
  "Return a list of all org files in the `myorg-dir` directory,
excluding any that are under a directory named `org-archives`."
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

;;; Org heading generator

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

(provide 'myorg)

;;; myorg.el ends here
