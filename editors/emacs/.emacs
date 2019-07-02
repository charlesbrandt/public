;; .emacs

;; This is the entry point to your emacs editor configuration

;; Single ';' are meant to be to the right
;; Full comment / documentation lines start with ';;'
;; For more notes about ';' and comments, see:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html

;; After making any changes to any of these configurations,
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

;; set load-path .emacs.d/ first
(setq load-path (cons "~/.emacs.d/" load-path))
;; now that "~/.emacs.d/" is in the load path,
;; it's not necessary to include the full path to load-file
;; but it helps for readability
;; this is also what enables e.g. (require 'markdown-mode)
;; TODO: is require equivalent to load-file in elisp?

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
  
