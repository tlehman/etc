;; AUTHOR: tlehman
;; DATE: 2024-03-22
;; DESCRIPTION: .emacs file with everything I need, a note-taking system, custom functions and configs
;;              spacemacs is bloated and I wanted something _fast_er, especially since it's Lent.
;;
;; UTILITY FUNCTIONS
(defun any? (list)
  "returns t if list has any non-nil values"
  (seq-reduce (lambda (a b) (or a b)) list nil))
(defun dark-mode ()
  (interactive)
  (load-theme 'ample t))
(defun light-mode ()
  (interactive)
  (load-theme 'aircon t))

;; APPEARANCE
(set-frame-font "Inconsolata 22")
(when window-system (set-frame-size (selected-frame) 120 49))
(global-hl-line-mode)
(setq inhibit-startup-screen t)
(light-mode)

;; STARTUP
(tab-bar-mode)
(server-start)

(if (file-exists-p "~/.emacs.d/puppet-mode.el")
    (load-file "~/.emacs.d/puppet-mode.el"))

;; STARTUP FRAME CONFIG (where, how big?)
(setq default-frame-alist
       '((height . 55)
         (width . 120)
         (left . 0)
         (top . 0)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)))

;; CONFIGURATION
(setq backup-directory-alist
      '(("." . "~/.emacs.backups.d")))
(setq evil-default-state 'emacs)
(evil-mode)
(add-hook 'term-mode-hook #'evil-emacs-state)
(add-hook 'eshell-mode-hook #'evil-emacs-state)


;; KEYBOARD MAPPING
(defun new-tab-and-find-file ()
  "Open new tab and drop into find-file"
  (interactive)
  (tab-bar-new-tab)
  (call-interactively 'find-file))

(defun eshell-tab-is-open? ()
  "Check if eshell tab is currently open"
  (let ((tab-names (mapcar (lambda (tab) (cdr (car (cdr tab)))) (tab-bar-tabs))))
    (any? (mapcar (lambda (tab-name) (string= "*eshell*" tab-name)) tab-names))))

(defun new-tab-and-open-eshell ()
  "open *eshell* tab, or switch to it if it exists, otherwise close the *eshell* tab"
  (interactive)
  (if (string= "*eshell*" (buffer-name))
      (tab-bar-close-tab)
      (if (eshell-tab-is-open?) 
	  (tab-bar-switch-to-tab "*eshell*")
	(progn
	  (tab-bar-new-tab)
	  (call-interactively 'eshell)))))

(defun in-situ-eshell ()
  "vscode refugees LOVE this function"
  (interactive)
  (if (string= "*eshell*" (buffer-name))
      (delete-window)
    (progn
      (split-window)
      (other-window 1)
      (call-interactively 'eshell t [default-directory]))))

(global-set-key (kbd "s-t") 'new-tab-and-find-file)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "<f4>") 'new-tab-and-open-eshell)
(global-set-key (kbd "C-`") 'in-situ-eshell)

;; NOTES & TIME
(require 'subr-x)
(require 'dash)
(defvar today "~/Desktop/today.org")
(defvar past "~/Desktop/past.org")

;; NOTES & TIME: today.org functions
(defun date-now ()  (string-trim (shell-command-to-string "date +'%Y-%m-%d'")))
(defun today-contents ()
  "Show the contents of ~/Desktop/today.org"
  (let ((command (string-join (list "cat" today) " ")))
    (shell-command-to-string command)))
(defun today-contents-outdated ()
  "Check if ~/Desktop/today.org is out of date"
  (let ((command (list "grep '" (date-now) "' " today " | wc -c")))
    (-> command
	(string-join)
	(string-trim)
	(shell-command-to-string) ; execute grep today.org | wc -c
	(string-trim)
	(string-to-number)        ; get integer line count
	(= 0)                     ; check if output is empty, if so, then the file is out of date
	)))
(defun today-move-to-past ()
  (shell-command-to-string		; append today.org to past.org
   "cd ~/Desktop && cat today.org past.org > temp.org && mv temp.org past.org")
  (shell-command-to-string		; write new today.org for today
   "printf \"* $(date +'%Y-%m-%d')\n\" > ~/Desktop/today.org"
   ))

;; NOTES & TIME: move outdated today.org file to past.org, reset today.org for TODAY
(defun idempotent-today-move-to-past ()
  (interactive)
  (if (today-contents-outdated)
      (today-move-to-past)))

(idempotent-today-move-to-past)

(defun show-today-and-past-notes () 
  (switch-to-buffer "*scratch*")
  (split-window-below)
  (find-file "~/Desktop/today.org")
  (switch-to-buffer "today.org")
  (split-window-right)
  (find-file "~/Desktop/past.org")
  (other-window 1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e7ce09ff7426c9a290d06531edc4934dd05d9ea29713f9aabff834217dbb08e4" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8122fb61548fe36171d9cf24cdb9b5f24d053b626d4cda739c3815e080623209" "e80ef6dcd230438866130c6b3b48c05616a5fecfbc5a8169385367723f15fc61" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" default))
 '(package-selected-packages
   '(markdown-mode projectile llm ellama ample-theme nlinum aircon-theme spacemacs-theme treemacs cider clojure-mode magit evil-visual-mark-mode dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
