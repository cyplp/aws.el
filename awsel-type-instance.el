(defun awsel/list-instance-types ()
  (interactive)
  (message "Refreshing instances ...")
  (lexical-let ((aws-buf (get-buffer-create "*aws2-instance-types*"))
                (f (make-temp-file "aws-json"))
                (buf (generate-new-buffer " *aws2-instance-types*")))
    (set-process-sentinel

     (start-process "aws2-instance-types"
                    buf
                    shell-file-name
                    shell-command-switch
                    (concat "aws2 ec2 describe-instance-types --output json --query \"InstanceTypes[].{InstanceType:InstanceType, CurrentGeneration:CurrentGeneration, FreeTierEligible:FreeTierEligible,Hypervisor:Hypervisor,Architecture:ProcessorInfo.SupportedArchitectures, CPU:ProcessorInfo.SustainedClockSpeedInGhz, VCpuInfo:VCpuInfo.DefaultVCpus, MemoryInfo:MemoryInfo.SizeInMiB}\" > " f))

     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer aws-buf
           (setq tabulated-list-entries
                 (awsel/aws-instance-types-list-tabulated-list-entries
                  (json-read-from-string (awsel/aws-read-file f))))
           (tabulated-list-print t)
	   )
         (delete-file f)
         (kill-buffer buf)
         (message "Instances refreshed.")))
     )))

(defun  awsel/aws-instance-types-list-tabulated-list-entry (instance)
    (list (not-nil (alist-get 'Name instance))
          (vector
           (not-nil (alist-get 'InstanceType instance))
           (not-nil (alist-get 'CurrentGeneration instance))
           (not-nil (alist-get 'FreeTierEligible instance))
	   (not-nil (alist-get 'Hypervisor instance))
	   (not-nil (alist-get 'Architecture instance))
	   (not-nil (concat (alist-get 'CPU instance)))
	   (not-nil (alist-get 'VCpuInfo instance))
	   (not-nil (alist-get 'MemoryInfo instance))
	   )))

(defun awsel/aws-instance-types-list-tabulated-list-entries (instances)
  (mapcar #'awsel/aws-instance-types-list-tabulated-list-entry instances))


(defun awsel/ec2-instance-types ()
  (interactive)
  (pop-to-buffer "*aws2-instance-types*" nil)
  (awsel/aws-instance-types-mode)
  (awsel/list-instance-types))

(defvar awsel/aws-instance-types-mode-mapprogn nil "Keymap for `awsel/aws-mode'.")
(progn
  (setq awsel/aws-instance-types-mode-map (make-sparse-keymap))
  (define-key awsel/aws-instance-types-mode-map (kbd "RET") 'awsel/instance-type-detail)
)

(defun instance-type-detail ()
  (message "todo"))

(define-derived-mode awsel/aws-instance-types-mode tabulated-list-mode "aws2-instance-types" "aws2-instance-types"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "awsel-instance-types")
  (setq major-mode 'awsel/aws--instance-types-mode)
  (hl-line-mode t)
  (setq tabulated-list-format [("InstanceType" 20 t)
                               ("CurrentGeneration" 5 t)
                               ("FreeTierEligible" 5 t)
                               ("Hypervisor" 5 t)
			       ("Architecture" 10 t)
			       ("CPU" 5 t)
			       ("VCpuInfo" 5 t)
			       ("MemoryInfo" 10 t)
			       ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "InstanceType" nil))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (use-local-map awsel/aws-instance-types-mode-map))


(provide 'awsel-instance-type)
