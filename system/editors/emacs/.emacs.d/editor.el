;; General editor settings

;; Customize Indentation levels via Emacs:
;; M-x customize
;; Then, choose "Programming," and then "Languages," and then select a language/mode to customize. Edit the options as you see fit. When done, choose either "Save for current session" or "Save for future sessions."
;; via: http://stackoverflow.com/questions/4177929/how-to-change-the-indentation-width-in-emacs-javascript-mode

;use spaces instead of tabs:
;http://stackoverflow.com/questions/45861/how-do-i-get-js2-mode-to-use-spaces-instead-of-tabs-in-emacs
(setq-default indent-tabs-mode nil)

;; https://emacs.stackexchange.com/questions/21095/how-do-i-make-javascript-mode-not-turn-all-8-spaces-into-tabs
;; This didn't seem to have the desired effect. Next section sets it globally
(defun my-js-mode-hook ()
  "Custom `js-mode' behaviours."
  (setq indent-tabs-mode nil))
(add-hook 'js-mode-hook 'my-js-mode-hook)

;; previously set in programming.el
;; need it either way!
(setq-default indent-tabs-mode nil)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;JAVASCRIPT indentation
;http://stackoverflow.com/questions/4177929/how-to-change-the-indentation-width-in-emacs-javascript-mode
(setq js-indent-level 2)




;; Disable backup files:
(setq make-backup-files nil) ; stop creating backup~ files
;; via: http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html

;; Update the frame title:

;; https://www.emacswiki.org/emacs/FrameTitle
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html

(setq frame-title-format
      '(
        (:eval (if (buffer-modified-p) 
                   "• "))

        (:eval (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 "%f"))
        " ("
        (:eval (if (buffer-file-name)
                   (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory buffer-file-name)))))
        ")")
      )

;; tool-bar-mode not found... m-x tool-bar-mode
;; feel free to re-enable if it causes problems
(tool-bar-mode -1) 
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; newer versions of emacs are opening new frames for multiple files
;; this suppresses that behavior
(setq ns-pop-up-frames nil)

;;; Frame size:

;; this works, but is global for all instances
;; unless frame-setup overrides

;; use minimum of all maximums of viable height for machine
;; it is difficult to shrink the frame if too tall

(set-frame-height (selected-frame) 54)
(set-frame-width (selected-frame) 80)

(message "SYSTEM NAME:")
(setq hostname (car (split-string (system-name) "\\.")))
(message hostname)

;; frame setup for different computers
;(defun setup-frame-for (name h w font)
(defun setup-frame-for (name h w)
  (if (equal hostname name)
      (progn
        (set-frame-height (selected-frame) h)
        (set-frame-width (selected-frame) w)
	;; this applies the height to new frames too 
	(add-to-list 'default-frame-alist (cons 'height h))
	(add-to-list 'default-frame-alist (cons 'width w))
	;(custom-set-faces font)
	)
    )
  )

(defun frame-setup (list)
  (when window-system
    (dolist (conf list)
      ;(setup-frame-for (car conf) (cadr conf) (caddr conf)))))
      ;(setup-frame-for (car conf) (cadr conf) (caddr conf) (cadddr conf) ))))

      ;(setup-frame-for (car conf) (cadr conf) (cadr (cdr conf)) (cadr (cdr (cdr conf))) ))))
      (setup-frame-for (car conf) (cadr conf) (cadr (cdr conf)) ))))


;(name height width font)
;where height and width are for the frame
(frame-setup
 '(("blank" 29 98 '() ) ;; example
   ("machine1" 34 125 ) ;; netbook
   ("machine2" 32 80  ) ;; laptop
   ("machine3" 43 80  )
   )
)

;:height 97 :width normal :foundry "unknown" 




;; Prevent accidentally killing emacs.
(global-set-key [(control x) (control c)]
                '(lambda ()
                   (interactive)
                   (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
                   
    (save-buffers-kill-emacs))))

(add-hook 'text-mode-hook 'flyspell-mode) 

;http://www.emacswiki.org/cgi-bin/wiki/RecentFiles
(require 'recentf)
(recentf-mode 1)

; Tab completion:
; http://www.emacswiki.org/cgi-bin/wiki/Icicles
; consider investigating icicles


;*2012.02.05 14:25:26 
; switch to only one window (in the emacs sense) open (one editing panel)
; (delete-other-windows) is what ctl-x 1 does
; but it must happen too early in the sequence (before other files are loaded)
; 
; this works:
(add-hook 'window-setup-hook 'delete-other-windows)
; via: 
; http://stackoverflow.com/questions/1144729/how-do-i-prevent-emacs-from-horizontally-splitting-the-screen-when-opening-multi


;;;; Themes

;; (load-file "~/.emacs.d/theme.el")

;; ;keeping this around to undo the bad settings done elsewhere
;; (my-color-theme-light)

;https://github.com/whitlockjc/atom-dark-theme-emacs
(load-file "~/.emacs.d/atom-dark-theme.el")

;(color-theme-calm-forest)
;;set default color theme
;
;(color-theme-gray30)
;(color-theme-calm-forest)
;(color-theme-euphoria)

;;black bagkgrounds:netbook
;(color-theme-oswald)
;(color-theme-lawrence)
;(color-theme-hober)
;(color-theme-charcoal-black)
;(color-theme-black)
;(color-theme-billw)
;(color-theme-midnight)
;(color-theme-late-night)






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#E5786D" "#95E454" "#CAE682" "#8AC6F2" "#333366" "#CCAA8F" "#F6F3E8"])
 '(custom-enabled-themes nil)
 '(global-font-lock-mode t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(tool-bar-mode nil))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :family "Monaco")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
