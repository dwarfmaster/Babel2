(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions implementing ssh and scp interfacing (via shell) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(scp-get ssh-clear-dir))

(defun scp-get (host username password remote-file local-file)
  "Downloads a file from a remote machine (e.g. robot) on your local machine."
  ;; Check for sshpass
  (unless (sshpass-installed-p)
    (error "sshpass is not installed or not in the path. You can install it e.g. with homebrew."))
  ;; Ensure the local-file directory exists
  (ensure-directories-exist local-file)
  ;; Do the scp request, avoiding the password prompt with sshpass
  (let ((arg (format nil "sshpass -p ~a /usr/bin/scp ~a@~a:~a ~a"
                     password username host remote-file local-file)))
    ;; If exec-and-return returns something, then something went wrong (it should just have done its job)
    (let ((error
           #+lispworks (exec-and-return  arg)
           #-lispworks (exec-and-return "/bin/sh" "-c" arg)
           ))
      (when error (error (format nil "SCP command failed: ~{~a ~}" error))))))

(defun ssh-clear-dir (host username password remote-dir)
  "Clears a given dir on a remote machine (e.g. robot). Will execute 'rm remote-dir/*'"
  ;; Check for sshpass
  (unless (sshpass-installed-p)
    (error "sshpass is not installed or not in the path. You can install it e.g. with homebrew."))
  ;; Do the scp request, avoiding the password prompt with sshpass
  (let* ((arg (format nil "sshpass -p ~a ssh ~a@~a 'rm ~a/*'" password username host remote-dir))
         (error
            #+lispworks (exec-and-return arg)
            #-lispworks (exec-and-return "/bin/sh" "-c" arg)
            ))
      (when error
        (error (format nil "RM command failed: ~{~a ~}" error)))
      t))

(defun sshpass-installed-p ()
  "Returns true if sshpass is installed and in the path, nil otherwise."
  (exec-and-return "which" "sshpass"))
