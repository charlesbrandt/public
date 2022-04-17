;; .emacs

;; This is the entry point to your emacs editor configuration

;; Single ';' are meant to be to the right
;; Full comment / documentation lines start with ';;'
;; For more notes about ';' and comments, see:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html

;; see also programming.el for commands for toggling comments

;; After making any changes to emacs configurations,
;; be sure to load a new instance of emacs.
;; Make sure everything still loads as expected, 
;; and that key functionality is still in place.
;; i.e. TEST!

;; -or-

;; reload .emacs
;; call this anytime with 'M-x reload'

(defun reload ()
  (interactive)
  "Reload .emacs"
  (if (file-exists-p "~/.emacs")
      (load-file "~/.emacs")
  )
)

;; reloading can also be accomplished by selecting a specific region,
;; then executing it using the "Emacs-Lisp->Evaluate Region" option/command


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; https://github.com/kiwanami/emacs-epc/issues/35
;; For message: Package cl is deprecated emacs
(setq byte-compile-warnings '(cl-functions))

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

;; https://melpa.org/#/
;; Note that you'll need to run M-x package-refresh-contents or M-x package-list-packages to ensure that Emacs has fetched the MELPA package list before you can install packages with M-x package-install or similar. 
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(setq confirm-kill-emacs 'y-or-n-p)

(load-file "~/.emacs.d/keymaps.el")

;; Journal Related functions:
(load-file "~/.emacs.d/moments.el")

;; (load-file "~/.emacs.d/indentation.el")

(setq load-path (cons "~/.emacs.d/programming" load-path))
;; programming. see also indentation.el for indentation specific settings
(load-file "~/.emacs.d/programming.el")

;; themes and start up settings that don't fall into above
(load-file "~/.emacs.d/editor.el")

;; (setq load-path (cons "~/.emacs.d/themes" load-path))


;; TODO:
;; way to automatically restore previous sessions?
;;  (persistent-session-save-alist-to-file)
;; tmux can help somewhat with this

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
