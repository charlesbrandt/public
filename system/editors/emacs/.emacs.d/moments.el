;; Journal Related Functions

;; This may have some interesting techniques for printing a timestamp:
;; http://ergoemacs.org/emacs/elisp_insert-date-time.html

(defun date ()
  (interactive)
  (insert (format-time-string "%Y.%m.%d %T"))
)
(defun now ()
  (interactive)
  (date)
)
(defun tstamp ()
  (interactive)
  (insert "# ")
  (date)
  (insert " ")
)
(global-set-key "\C-n" 'tstamp)

(defun ttag ()
  (interactive)
  (insert "[")
  (date)
  (insert "]")
  )

(defun journal ()
  (interactive)
  (beginning-of-buffer)
  ;; could consider a 'clear screen' here (some # of newlines)
  (insert "\n\n\n")
  (beginning-of-buffer)
  (tstamp)
  )
;; if you use transpose, this will interfere
;; didn't like having a 'move-to-top-timestamp' all controlled with one hand
;; that was a nice feature about ctrl-j
;; (global-set-key "\C-t" 'journal)

;; can learn new habit as needed -- nothing else pressing here in emacs:
(global-set-key  (kbd "C-j") 'journal) 
;; these should be used to open a terminal
;; similar to vscode
;; ideally it should toggle open and close a split frame in emacs
;;(global-set-key "\C-xj" 'journal)
;;(global-set-key "\C-j" 'journal)

(defun top ()
  ;; creating an alias
  (journal)  
)



(defun today ()
  (interactive)
  (insert (format-time-string "%Y.%m.%d"))
)

(defun journal-day-only ()
  (interactive)
  (beginning-of-buffer)
  ;; could consider a 'clear screen' here (some # of newlines)
  (insert "\n\n\n")
  (beginning-of-buffer)
  (insert "# ")
  (today)
  (insert " ")
  )

(global-unset-key (kbd "C-u"))
(global-set-key  (kbd "C-u") 'journal-day-only) 




(defun also ()
  (interactive)
  (insert "also ")
  (interactive)
  (insert "[")
  (date)
  (insert "] ")
)

;; this is not currently working with seconds
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

(defun clearmind ()
  (interactive)
  (beginning-of-buffer)
  ;; could consider a 'clear screen' here (some # of newlines)
  (insert "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
  (beginning-of-buffer)
  )









;should remember to put a start stamp when editing lisp code
;*2008.12.02 12:35:49 
(defun print-list (list)
  (while list
    ;(print (car list))
    ;(prin1 (car list))
    (princ (car list))
    (setq list (cdr list)))
)

;; (defun open-files ()
;; (defun list-open ()
(defun jo ()
  (interactive)
  (setq flist (split-string (car kill-ring-yank-pointer) "\n"))
  ;; 'reverse does not alter the original structure of the list (returns a new list)
  ;; 'nreverse operates in place
  ;; https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-20-2.5/html_chapter/elisp_6.html
  ;(nreverse flist)

  (setq flipped (reverse flist))
  
  (while flipped
    ;open a buffer:
    (find-file (car flipped))
    (setq flipped (cdr flipped)))
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
  (setq flist (mapcar (function buffer-file-name) (buffer-list)))
  ;; I don't think this works, and was an old attempt to make opening work
  ;; but would have placed most important items at the bottom instead of top
  ;; (reverse list)
  ;; however, it would be nice to move the first item ('instance.md')
  ;; to the end of the list
  (setq instance (car flist))
  (setq flist (cdr flist))
  ;; attempts at appending to the end of a list in lisp
  (push instance (cdr (last flist)))
  ;; (nconc flist (list instance))
  (while flist
    (if (car flist)
	(progn (insert (car flist))
	 (insert "\n") )
    )
    (setq flist (cdr flist))
    )
  
  (let ((case-fold-search t)) ; or nil
  (goto-char 0)
  (while (search-forward (getenv "HOME") nil t)
    (replace-match "~"))
  )
  (next-line)
;;  (insert (print-list ))
)

(defun jp ()
  ;; old way of calling print files
  (pf)  
)
;; causes all open buffers to be print after every 'enter' key
;; (global-unset-key (kbd "C-m"))
;; (global-set-key  (kbd "C-m") 'pf) 


(defun jc ()
  (interactive)
  (setq list (split-string (car kill-ring-yank-pointer) "\n"))
  (while list
    (kill-buffer (find-file (car list)))
    (setq list (cdr list)))
)
;*2008.12.02 14:46:34 


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




(defun open-today ()
  (interactive)
  (setq todayPath (concat "/c/outgoing/" (format-time-string "%Y%m%d") ".txt"))
  (find-file todayPath)
)
;(global-set-key "\C-xt" 'open-today)




