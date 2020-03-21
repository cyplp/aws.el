;;; -*- lexical-binding: t; -*-


(require 'async)
(require 'parse-time)
(require 'transient)
(require 'term)
(with-no-warnings
  (require 'cl))

(setq lexical-binding t)

(defun cyplp-aws-read-file (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun not-nil (value)
  (if value
      value
      ""))

(defun  cyplp-aws-list-tabulated-list-entry (instance)
    (list (not-nil (alist-get 'Name instance))
          (vector
           (not-nil (alist-get 'Name instance))
           (not-nil (alist-get 'Type instance))
           (not-nil (alist-get 'Status instance))
	   (not-nil (alist-get 'DNS instance))
	   (not-nil (alist-get 'InstanceId instance))
	   ;; (not-nil (alist-get 'LaunchTime instance))
	   )))

(defun cyplp-aws-list-tabulated-list-entries (instances)
  (mapcar #'cyplp-aws-list-tabulated-list-entry instances))


(defun cyplp-list-instances ()
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
                 (cyplp-aws-list-tabulated-list-entries
                  (json-read-from-string (cyplp-aws-read-file f))))
           (tabulated-list-print t)
	   )
         (delete-file f)
         (kill-buffer buf)
         (message "Instances refreshed.")))
     )))

(defun cyplp-ec2-instances ()
  (interactive)
  (pop-to-buffer "*aws2*" nil)
  (cyplp-aws-mode)
  (cyplp-list-instances)
  )
(defvar cyplp-aws-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'cyplp-list-instances)
    map)
  "Keymap for `cyplp-aws-mode'.")



(define-derived-mode cyplp-aws-mode tabulated-list-mode "aws" "aws"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "aws")
  (setq major-mode 'cyplp-aws-mode)
  (use-local-map cyplp-aws-mode-map)
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
  (tabulated-list-print))

(provide 'cyplp-aws)
