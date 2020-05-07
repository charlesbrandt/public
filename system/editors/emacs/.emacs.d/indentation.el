;use spaces instead of tabs:
;http://stackoverflow.com/questions/45861/how-do-i-get-js2-mode-to-use-spaces-instead-of-tabs-in-emacs
(setq-default indent-tabs-mode nil)


;; Customize Indentation levels via Emacs:
;; M-x customize
;; Then, choose "Programming," and then "Languages," and then select a language/mode to customize. Edit the options as you see fit. When done, choose either "Save for current session" or "Save for future sessions."
;; via: http://stackoverflow.com/questions/4177929/how-to-change-the-indentation-width-in-emacs-javascript-mode

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

;JAVASCRIPT
;http://stackoverflow.com/questions/4177929/how-to-change-the-indentation-width-in-emacs-javascript-mode
(setq js-indent-level 2)
