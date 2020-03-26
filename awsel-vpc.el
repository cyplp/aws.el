(defun awsel/list-vpcs ()
  (interactive)
  (message "Refreshing instances ...")
  (lexical-let ((aws-buf (get-buffer-create "*aws2-vpcs*"))
                (f (make-temp-file "aws-json"))
                (buf (generate-new-buffer " *aws2-vpcs*")))
    (set-process-sentinel

     (start-process "aws2-vpcs"
                    buf
                    shell-file-name
                    shell-command-switch
                    (concat "aws2 ec2 describe-vpcs --output json --query \"Vpcs[].{Name:Tags[?Key=='Name']|[0].Value, VpcId:VpcId, Status:State, CidrBlock:CidrBlock}\" > " f))

     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer aws-buf
           (setq tabulated-list-entries
                 (awsel/aws-vpcs-list-tabulated-list-entries
                  (json-read-from-string (awsel/aws-read-file f))))
           (tabulated-list-print t)
	   )
         (delete-file f)
         (kill-buffer buf)
         (message "Instances refreshed.")))
     )))

(defun  awsel/aws-vpcs-list-tabulated-list-entry (instance)
    (list (not-nil (alist-get 'Name instance))
          (vector
           (not-nil (alist-get 'Name instance))
           (not-nil (alist-get 'VpcId instance))
           (not-nil (alist-get 'Status instance))
	   (not-nil (alist-get 'CidrBlock instance))
	   )))

(defun awsel/aws-vpcs-list-tabulated-list-entries (instances)
  (mapcar #'awsel/aws-vpcs-list-tabulated-list-entry instances))



(defun awsel/ec2-vpcs ()
  (interactive)
  (pop-to-buffer "*aws2-vpcs*" nil)
  (awsel/aws-vpcs-mode)
  (awsel/list-vpcs))

(defvar awsel/aws-vpcs-mode-mapprogn nil "Keymap for `awsel/aws-mode'.")
(progn
  (setq awsel/aws-vpcs-mode-map (make-sparse-keymap))
  (define-key awsel/aws-vpcs-mode-map (kbd "RET") 'awsel/vpc-detail)
)

(defun vpc-detail ()
  (message "todo"))

(define-derived-mode awsel/aws-vpcs-mode tabulated-list-mode "aws2-vpcs" "aws2-vpcs"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "awsel-vpcs")
  (setq major-mode 'awsel/aws--vpcs-mode)
  (hl-line-mode t)
  (setq tabulated-list-format [("Name" 40 t)
                               ("VpcId" 30 t)
                               ("Status" 10 t)
                               ("CidrBlock" 10 t)
			       ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (use-local-map awsel/aws-vpcs-mode-map))


(provide 'awsel-vpc)
