;; tlehman's .emacs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       APPEARANCE                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       INITIALIZATION                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run in server mode so that other processes can spawn emacs 
;; buffers in this instance 
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       HASKELL                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

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




;; User-setting area is below this line.

(defun length-of-region ()
  "Get number of characters in region (highlighted)"
  (interactive)
  (message "%d characters long" (- (region-end) (region-beginning))))


(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-e" nil)))

(define-key evil-normal-state-map "tt" 'projectile-toggle-between-implementation-and-test)
(define-key evil-normal-state-map "\t" 'compile-rspec)



;; allow for export=>beamer by placing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       BEAMER                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #+LaTeX_CLASS: beamer in org files

(setq org-src-preserve-indentation t)

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
      \\usepackage{tikz}
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

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))


