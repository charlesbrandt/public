;; Keymaps

;; this is useful in termux:
(global-set-key "\C-h" 'backward-kill-word)
;; via: https://www.reddit.com/r/termux/comments/cnnkao/is_it_possible_to_change_what_ctrlbackspace_does/



;; To see what key syntax to use when defining a new keymap:
;; Alt+x describe-key 【Ctrl+h k】, then press the key you want. Emacs will then display its syntax.
;; This also lets you see what a key is currently bound to

;; The terminal may not pass all characters to emacs, which can make it seem like bindings are not taking effect:
;; https://stackoverflow.com/questions/11110801/why-does-ctrl-not-work-when-i-bind-it-to-a-command-in-emacs

(global-set-key "\C-b" 'switch-to-buffer)


;; Comments

;;this is how things were in python mode... those are stuck
;; always have comment region bound to a shortcut
;; (global-set-key "\C-c#" 'comment-region)
;; comment 'do what i mean' is a bit more versatile -- toggles comments
(global-set-key "\C-c#" 'comment-dwim)

;; these appear to break things pretty badly
(global-unset-key (kbd "C-;"))
;; (global-set-key "\C-;" 'comment-dwim)
(global-set-key (kbd "C-;") 'comment-dwim)
;;(global-set-key "\C-/" 'comment-dwim)

;; TODO:
;; could also configure ctrl-/
;; often used in other languages for commenting
;; maybe also need a ctrl-; (for lisp languages)
;; but all should use the mode to determine the comment type to use
;; and all should work interchangably



;; Ergoemacs is a good place to start. It defines many of the more common bindings used elsewhere so that you don't have to do that manually.
;; https://ergoemacs.github.io/
;; https://www.emacswiki.org/emacs/ErgoemacsKeybindings
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html
;; http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html

;; (setq load-path (cons "~/.emacs.d/ergoemacs-mode" load-path))
;; (setq load-path (cons "~/.emacs.d/ergoemacs-extras" load-path))
;; (add-to-list 'load-path "~/.emacs.d/ergoemacs-mode")
(require 'ergoemacs-mode) 

(setq ergoemacs-theme nil)
;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us")
;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

;; this fixes arrow keys resulting in 'A', 'B', etc:
;; https://github.com/ergoemacs/ergoemacs-mode/issues/280
;; this only happens when running emacs in a terminal
;; "M-O" (Alt-Shift-o) is the one that fixed it for me. 
(global-unset-key (kbd "M-O"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-["))

;; from here, it's important to know that "Alt-A" is the replacement for "Alt-x" (M-x) for executing commands

;; not sure if I can let go of the following:
;; ctrl-a move-beginning-of-line
;; ctrl-e move-end-of-line
;; ctrl-d delete-character
;; some of these even show up in the mac os default.

(global-set-key (kbd "C-d") 'delete-char)
(global-set-key (kbd "C-a") 'move-beginning-of-line)
(global-set-key (kbd "C-e") 'move-end-of-line)

;; kill-line is non-standard in other editors
;; but it is one I often miss from emacs
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-<") 'beginning-of-buffer)


(setq confirm-kill-emacs 'y-or-n-p)

(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)

;; I'm not sure why ctrl-space is considered sub-optimal by ergo-emacs.
;; I like it though
(global-set-key (kbd "C-SPC") 'set-mark-command)



;; A bit of history of how ergo emacs came up with the keybindings they chose:
;; http://ergoemacs.org/emacs/emacs_keybinding_redesign.html
;; http://ergoemacs.org/emacs/emacs_keybinding_redesign_2.html



;; The following are now handled by ergoemacs

;; ctrl-f to search / find
;; (global-set-key (kbd "C-f") 'isearch-forward)

;; ctrl-s to save
;; (global-set-key (kbd "C-s") 'save-buffer)

;; (global-set-key (kbd "C-z") 'undo)

;; ctrl-o to open file (and switch buffer?)
;; ctrl-b to switch buffer
;; ctrl-w to close window (with "Are you sure?")

;; remap ctrl-x and ctrl-c to be something else
;; the default emacs way makes it difficult to switch to another editor. 
;; then map ctrl-c, ctrl-x, ctrl-v to copy, cut, paste




;(global-set-key (kbd "C-x w") 'browse-url-default-macosx-browser)
(setq-default browse-url-browser-function 'browse-url-firefox)
(setq-default browse-url-firefox-new-window-is-tab t)
(global-set-key (kbd "C-x w") 'browse-url-firefox)

;*2009.12.27 17:49:12 
;(global-set-key (kbd "C-x r") 'search-backward)
;(global-set-key (kbd "C-r") 'revert-buffer)

(setq mac-option-key-is-meta nil) 
(setq mac-command-key-is-meta t) 
(setq mac-command-modifier 'meta) 
(setq mac-option-modifier nil) 

