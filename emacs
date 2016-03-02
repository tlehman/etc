;; tlehman's .emacs file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       KEYBOARD                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c C-e") 'eval-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)
;; for traversing through a universe of parens
(global-set-key (kbd "C-%") 'backward-list)
(global-set-key (kbd "C-5") 'forward-list)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'compile-rspec)

(setq mac-command-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       EVIL                                                                 ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       APPEARANCE                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 150)
(set-face-attribute 'default nil :font "Inconsolata 14")

(global-hl-line-mode 1)

(set-background-color "#222222")
(set-foreground-color "white")
(set-face-background 'hl-line "#333333")

;; The use of auto-fill-mode is so that lines don't get longer than 70
;; characters, because wrapping is annoying, but so is manually
;; managing the length of the line, therefore, the Editor shall do it.
(auto-fill-mode)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       WINDOW MANAGEMENT                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; winner-mode reemembers your previous window configurations
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; numbered windows 
(require 'window-number)
(window-number-meta-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       INITIALIZATION                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load-file "~/.emacs.d/vendor/graphviz-dot-mode.el")

(setq inhibit-splash-screen t)
(setq initial-scratch-message ";; Don't Panic.\n\n(floor (* 6 (sqrt 49.0)))")

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string 
			   "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system 
    (progn
      (set-exec-path-from-shell-PATH)
      (tool-bar-mode -1)))

(defun split-shell-toggle ()
  (interactive)
  (split-window-vertically)
  (eshell)
  (other-window))

  

(global-set-key (kbd "<f3>") 'split-shell-toggle)

;; Run in server mode so that other processes can spawn emacs 
;; buffers in this instance 
(server-start)

(auto-fill-mode)

(column-number-mode)

(setq-default tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       RUBY                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CamelCaseWordBoundaries
(add-hook 'ruby-mode-hook 'subword-mode)

(add-to-list 'load-path "~/.emacs.d/rspec-mode/")
(require 'rspec-mode)

(defun compile-rspec ()
  "run rspec on current file"
  (interactive)
  (let ((path-name (substring (buffer-file-name) 0 
			      (string-match "spec/" (buffer-file-name))))
	(spec-name (substring (buffer-file-name) 
			      (string-match "spec/" 
					    (buffer-file-name)))))
    (compile (concat "cd " path-name " && bundle exec rspec -fd " spec-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SCHEME STUFF                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(global-font-lock-mode 1)

;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

(setq scheme-program-name "\/usr\/local\/bin\/scheme")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      BACKUP                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       SHELL STUFF                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*shell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (shell (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

;(global-set-key (kbd "C-c t") 'visit-term-buffer)

(defun buffer-mode (buffer-or-string)
  "Returns major mode associated with a buffer"
  (with-current-buffer buffer-or-string
    major-mode))

(defun buffer-shell-p (buffer-or-string)
  "Returns true if buffer is in shell-mode"
  (or (eq (buffer-mode buffer-or-string) 'shell-mode)
      (eq (buffer-mode buffer-or-string) 'term-mode)))

(defun shell-buffer-list ()
  "Return list of shell buffers"
  (remove-if-not 'buffer-shell-p (buffer-list)))

(defun ido-switch-shell-buffer ()
  "switch between shell buffers interactively"
  (interactive)
  (let ((next 
	 (ido-completing-read "Switch to shell: " 
			      (mapcar 'buffer-name (shell-buffer-list))
			      nil t)))
    (switch-to-buffer next)))

(global-set-key (kbd "C-c b") 'ido-switch-shell-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      UTILITY                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; filename stuff
(defun show-buffer-file-name ()
  (interactive)
  (message buffer-file-name))

; display filename in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

; OSX only
(defun open-in-finder ()
  (shell-command "open `pwd`"))

(defun pasteboard-filename ()
  "Similar to Sublime Text's  'copy file name' feature, this function
copies the full path of the file into OSX's global clipboard (called
  pasteboard by Apple)"
  (interactive)
  (shell-command (concat "echo " buffer-file-name " | pbcopy")))  ; OSX only

(global-set-key (kbd "C-c f") 'pasteboard-filename)

(defun display-buffer-as-graphviz ()
  (interactive)
  (shell-command
   (concat "display <(dot " buffer-file-name " -Tsvg)")))

; interactive do
(require 'ido)
(ido-mode t)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)

; recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(defun recentf-ido-find-file ()
  "find recent file using ido"
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list
				   nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c C-f") 'recentf-ido-find-file)



;; User-setting area is below this line.

(defun length-of-region ()
  "Get number of characters in region (highlighted)"
  (interactive)
  (message "%d characters long" (- (region-end) (region-beginning))))


;; git stuff
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'evil)
(require 'projectile-rails)

(global-set-key (kbd "C-x p") 'find-file-in-project)

(defgroup evil-rails nil
  "Evil Rails customizations."
  :prefix "evil-rails-"
  :group 'evil-rails)



(evil-mode 1)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-e" nil)))

(define-key evil-normal-state-map "tt" 'projectile-toggle-between-implementation-and-test)
(define-key evil-normal-state-map "\t" 'compile-rspec)


(require 'key-chord)
(key-chord-mode 1)

;; allow for export=>beamer by placing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       BEAMER                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n          
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")
     
     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
     
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
