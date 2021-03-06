;;; package --- aws.el
;;; Commentary: some draft

;;; Code:

(require 'async)
(require 'parse-time)
(require 'transient)
(require 'term)

(with-no-warnings
  (require 'cl))

(setq lexical-binding t)

(load (expand-file-name "utils.el") nil nil t)

(defun  awsel/aws-list-tabulated-list-entry (instance)
    (list (not-nil (alist-get 'InstanceId instance))
          (vector
           (not-nil (alist-get 'Name instance))
           (not-nil (alist-get 'Type instance))
           (not-nil (alist-get 'Status instance))
	   (not-nil (alist-get 'DNS instance))
	   (not-nil (alist-get 'InstanceId instance))
	   ;; (not-nil (alist-get 'LaunchTime instance))
	   )))

(defun awsel/aws-list-tabulated-list-entries (instances)
  (mapcar #'awsel/aws-list-tabulated-list-entry instances))


(defun awsel/list-instances ()
  (interactive)
  (message "Refreshing instances ...")
  (lexical-let ((aws-buf (get-buffer-create "*aws2*"))
                (f (make-temp-file "aws-json"))
                (buf (generate-new-buffer " *aws2*")))
    (set-process-sentinel

     (start-process "aws2"
                    buf
                    shell-file-name
                    shell-command-switch
                    (concat "aws2 ec2 describe-instances --output json --query \"Reservations[].Instances[].{Name:Tags[?Key=='Name']|[0].Value, Type:InstanceType, Status:State.Name, DNS:PublicDnsName,InstanceId: InstanceId,LaunchTime:LaunchTime}\" > " f))

     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer aws-buf
           (setq tabulated-list-entries
                 (awsel/aws-list-tabulated-list-entries
                  (json-read-from-string (awsel/aws-read-file f))))
           (tabulated-list-print t)
	   )
         (delete-file f)
         (kill-buffer buf)
         (message "Instances refreshed.")))
     )))

(defun awsel/ec2-instances ()
  (interactive)
  (pop-to-buffer "*aws2*" nil)
  (awsel/aws-mode)
  (awsel/list-instances)
  )

(defun plop (&optional plop)
  (interactive "P")
  (message plop))

(defun awsel/ec2-instance-detail (&optional plop)
  (interactive "P")
  (message (concat "arg is : " plop ".")))


(defvar awsel/aws-mode-mapprogn nil "Keymap for `awsel/aws-mode'.")
(progn
  (setq awsel/aws-mode-map (make-sparse-keymap))
  (define-key awsel/aws-mode-map (kbd "RET") 'awsel/ec2-instance-detail)
  (define-key awsel/aws-mode-map (kbd "g") 'awsel/list-instances)
  (define-key awsel/aws-mode-map (kbd "d") 'plop)
)


(define-derived-mode awsel/aws-mode tabulated-list-mode "aws2" "aws2"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "aws.el")
  (setq major-mode 'awsel/aws-mode)
  (hl-line-mode t)
  (setq tabulated-list-format [("Name" 45 t)
                               ("Type" 10 t)
                               ("Status" 10 t)
                               ("DNS" 40 t)
                               ("InstanceId" 5 t)
                               ;; ("LaunchTime" 0 t)
			       ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (use-local-map awsel/aws-mode-map))

(provide 'awsel)
;;; awsel.el ends here
