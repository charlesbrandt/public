;; Journal Related Functions

;; 2019.07.03 09:00:53 
;; This may have some interesting techniques for printing a timestamp:
;; http://ergoemacs.org/emacs/elisp_insert-date-time.html

(defun today ()
  (interactive)
  (insert (format-time-string "%Y%m%d"))
)

(defun open-today ()
  (interactive)
  (setq todayPath (concat "/c/outgoing/" (format-time-string "%Y%m%d") ".txt"))
  (find-file todayPath)
)
(global-set-key "\C-xt" 'open-today)

(defun date ()
  (interactive)
  (insert (format-time-string "%Y.%m.%d %T"))
)
(defun tstamp ()
  (interactive)
  (insert "# ")
  (date)
  (insert " ")
)
(defun now ()
  (interactive)
  (tstamp)
)

(defun ttag ()
  (interactive)
  (insert "[")
  (date)
  (insert "]")
)
(defun journal ()
  (interactive)
  (beginning-of-buffer)
  (insert "\n\n")
  (beginning-of-buffer)
  (tstamp)
)
(global-set-key "\C-xj" 'journal)
(global-set-key "\C-j" 'journal)

(defun also ()
  (interactive)
  (insert "also ")
  (interactive)
  (insert "[")
  (date)
  (insert "] ")
)

;this is not currently working with seconds
(defun s2t ()
  (interactive)
  (delete-char 1)
  (insert "[")
  (forward-char 16)
  (insert "]")
)
(defun requested ()
  (interactive)
  (insert "requested ")
  (s2t)
)

(defun ss2t ()
  (interactive)
  ;; this depends on the format of the stamp
  ;; being used.
  (delete-char 2)
  (insert "[")
  (forward-char 19)
  (insert "]")
)
(defun req ()
  (interactive)
  (insert "requested ")
  (ss2t)
)
(defun start ()
  (interactive)
  (insert "started ")
  (ss2t)
)
(defun com ()
  (interactive)
  (tstamp)
  (insert "complete\n")
  (req)
)
(defun finish ()
  (interactive)
  (tstamp)
  (insert "\n")
  (start)
)

(defun paste-beginning ()
  ;; modified yank to delete first two lines from buffer
  ;; useful for copy all urls... ignores first tab
  (interactive)
  (beginning-of-buffer)
  (yank)
  (insert "\n")
  (beginning-of-buffer)

  ;see note below
  (kill-line 2)
  (journal)
  (forward-char 1)
  ;this will kill only the text, or a blankline
  ;but if a number is after it, it will kill both text and blankline for that
  ;many lines
  (kill-line)
  ;get the cursor ready for adding a tag
  (backward-char)
  ;this is handy, but saving is a good habit... don't want to get lazy
  ;(save-buffer)
)
(global-set-key "\C-xy" 'paste-beginning)

(defun paste-end ()
  ;; modified yank to delete first two lines from buffer
  ;; useful for copy all urls... ignores first tab
  (interactive)
  (end-of-buffer)
  (tstamp)
  (insert "\n")
  (yank)
  (beginning-of-buffer)
)
(global-set-key "\C-xn" 'paste-end)



;should remember to put a start stamp when editing lisp code
;*2008.12.02 12:35:49 
(defun print-list (list)
  (while list
    ;(print (car list))
    ;(prin1 (car list))
    (princ (car list))
    (setq list (cdr list)))
)

;(defun open-files ()
;(defun list-open ()
(defun jo ()
  (interactive)
  (setq list (split-string (car kill-ring-yank-pointer) "\n"))
  (while list
    ;open a buffer:
    (find-file (car list))
    (setq list (cdr list)))
)

; could make a new frame:
;http://www.gnu.org/software/emacs/elisp/html_node/Creating-Frames.html#Creating-Frames
(defun jof ()
  (interactive)
  (setq list (split-string (car kill-ring-yank-pointer) "\n"))
  (while list
    (find-file (car list))
    (make-frame)
    (setq list (cdr list)))
)

;; (defun list-print ()
(defun pf ()
  (interactive)
  (setq list (mapcar (function buffer-file-name) (buffer-list)))
  (reverse list)
  (while list
    (if (car list)
	(progn (insert (car list))
	 (insert "\n") )
    )
    (setq list (cdr list))
    )
  
  (let ((case-fold-search t)) ; or nil
  (goto-char 0)
  (while (search-forward (getenv "HOME") nil t)
    (replace-match "~"))
  )
;;  (insert (print-list ))
)

(defun jp ()
  ;; old way of calling print files
  (pf)  
)


(defun jc ()
  (interactive)
  (setq list (split-string (car kill-ring-yank-pointer) "\n"))
  (while list
    (kill-buffer (find-file (car list)))
    (setq list (cdr list)))
)
;*2008.12.02 14:46:34 
