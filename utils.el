(defun awsel/aws-read-file (f)
  ;; Helper for read file
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun not-nil (value)
  "Replace nil by empty string if value is nil."
  (if value
      value
      ""))
