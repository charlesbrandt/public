;; .emacs

;; This is the entry point to your emacs editor configuration

;; Single ';' are meant to be to the right
;; Full comment / documentation lines start with ';;'
;; For more notes about ';' and comments, see:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html

;; see also programming.el for commands for toggling comments

;; After making any changes to any of these configurations,
;; be sure to load a new instance of emacs.
;; Make sure everything still loads as expected, 
;; and that key functionality is still in place.
;; i.e. TEST!

;; -or-

;; reload .emacs
;; call this anytime with 'M-x reload'

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun reload ()
  (interactive)
  "Reload .emacs"
  (if (file-exists-p "~/.emacs")
      (load-file "~/.emacs")
  )
)

;; reloading can also be accomplished by selecting a specific region,
;; then executing it using the "Emacs-Lisp->Evaluate Region" option/command


;; Receiving a warning message for this setting;
;; Warning (initialization): Your ‘load-path’ seems to contain
;; your ‘.emacs.d’ directory: ~/.emacs.d/
;; This is likely to cause problems...
;; Consider using a subdirectory instead, e.g.: /data/data/com.termux/files/home/.emacs.d/lisp
;; but not having it set causes many other issues
;; set load-path .emacs.d/ first
;; (setq load-path (cons "~/.emacs.d/" load-path))
;; now that "~/.emacs.d/" is in the load path,
;; it's not necessary to include the full path to load-file
;; but it helps for readability
;; this is also what enables e.g. (require 'markdown-mode)
;; TODO: is require equivalent to load-file in elisp?

(setq load-path (cons "~/.emacs.d/ergoemacs-mode" load-path))
(setq load-path (cons "~/.emacs.d/ergoemacs-extras" load-path))
(setq load-path (cons "~/.emacs.d/themes" load-path))


(load-file "~/.emacs.d/keymaps.el")

;; Journal Related functions:
(load-file "~/.emacs.d/moments.el")

(load-file "~/.emacs.d/indentation.el")

;; programming. see also indentation.el for indentation specific settings
(load-file "~/.emacs.d/programming.el")

;; themes and start up settings that don't fall into above
(load-file "~/.emacs.d/editor.el")


;; TODO:
;; way to automatically restore previous sessions?
;;  (persistent-session-save-alist-to-file)
  
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
 '(js-indent-level 2 t)
 '(package-selected-packages
   (quote
    (undo-tree yaml-mode web-mode vue-mode scss-mode sass-mode markdown-mode)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
