(defun awsel/list-security-groups ()
  (interactive)
  (message "Refreshing security groups...")
  (lexical-let ((aws-buf (get-buffer-create "*aws2-security-groups*"))
                (f (make-temp-file "aws-json"))
                (buf (generate-new-buffer " *aws2-security-groups*")))
    (set-process-sentinel

     (start-process "aws2-security-groups"
                    buf
                    shell-file-name
                    shell-command-switch
                    (concat "aws2 ec2 describe-security-groups --query \"SecurityGroups[].{Name:GroupName, Description:Description,GroupId:GroupId,VpcId:VpcId}\" > " f))

     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer aws-buf
           (setq tabulated-list-entries
                 (awsel/aws-security-groups-list-tabulated-list-entries
                  (json-read-from-string (awsel/aws-read-file f))))
           (tabulated-list-print t)
	   )
         (delete-file f)
         (kill-buffer buf)
         (message "Instances refreshed.")))
     )))

(defun  awsel/aws-security-groups-list-tabulated-list-entry (instance)
    (list (not-nil (alist-get 'GroupId instance))
          (vector
           (not-nil (alist-get 'Name instance))
           (not-nil (alist-get 'Description instance))
           (not-nil (alist-get 'GroupId instance))
	   (not-nil (alist-get 'VpcId instance))
	   )))

(defun awsel/aws-security-groups-list-tabulated-list-entries (instances)
  (mapcar #'awsel/aws-security-groups-list-tabulated-list-entry instances))



(defun awsel/ec2-security-groups ()
  (interactive)
  (pop-to-buffer "*aws2-security-groups*" nil)
  (awsel/aws-security-groups-mode)
  (awsel/list-security-groups))

(defvar awsel/aws-security-groups-mode-mapprogn nil "Keymap for `awsel/aws-mode'.")
(progn
  (setq awsel/aws-security-groups-mode-map (make-sparse-keymap))
  (define-key awsel/aws-security-groups-mode-map (kbd "RET") 'awsel/security-group-detail)
)

(defun security-group-detail ()
  (message "todo"))

(define-derived-mode awsel/aws-security-groups-mode tabulated-list-mode "aws2-security-groups" "aws2-security-groups"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "awsel-security-groups")
  (setq major-mode 'awsel/aws--security-groups-mode)
  (hl-line-mode t)
  (setq tabulated-list-format [("Name" 40 t)
			       ("Description" 30 t)
                               ("GroupId" 10 t)
                               ("VpcId" 10 t)
			       ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (use-local-map awsel/aws-security-groups-mode-map))


(provide 'awsel-security-group)
