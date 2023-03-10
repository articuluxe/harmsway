;; dbcnx-mode-map :: keymap
(defvar dbcnx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'dbcnx/connect-via-filename-maybe)
    map))

;; dbcnx/connect-via-filename-maybe :: void -> void
(defun dbcnx/connect-via-filename-maybe ()
  "Connect to the database specified by the filename, maybe."
  (interactive)
  (let* ((label (intern (string-remove-suffix ".dbcnx" (car (last (split-string (buffer-file-name) "/"))))))
         (pair (assoc label sql-connection-alist)))
    (when (listp pair)
      (dbcnx/connect label))))

;; dbcnx/connect :: label -> void
;; label = (symbol) The key of an entry in the `sql-connection-alist`.
(defun dbcnx/connect (label)
  "Connect to the database associated with the given `label`."
  (interactive)
  (let ((product (car (cdr (assoc label sql-connection-alist)))))
    (setq sql-product product)
    (sql-connect label)))

;; dbcnx-mode just presents a way for conveniently connecting to databases.
;; All the functionality derives from sql-mode.
(define-derived-mode dbcnx-mode sql-mode "dbcnx"
  "A convenience mode for connecting to databases.")

(provide 'dbcnx-mode)
