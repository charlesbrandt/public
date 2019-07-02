;; keymaps

;; ctrl-f to search / find
(global-set-key (kbd "C-f") 'isearch-forward)

;; ctrl-s to save
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-z") 'undo)

;; ctrl-o to open file (and switch buffer?)
;; ctrl-b to switch buffer
;; ctrl-w to close window (with "Are you sure?")


;; Todo
;; remap ctrl-x and ctrl-c to be something else
;; can be a little awkward
;; whenever you hit it, use the opportunity to create a new key combo

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

