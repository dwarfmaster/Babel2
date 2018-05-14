(in-package :robot-interface)

(export '(make-robot make-new-connection disconnect-robot))

(defun make-robot (&key ip server-port
                        (port "9559")
                        (username "nao")
                        (password "nao")
                        (server-host "localhost")
                        (connect-automatically t))
  #+nao (make-instance 'nao
                       :ip ip
                       :port port
                       :username username
                       :password password
                       :server-host server-host
                       :server-port server-port
                       :connect-automatically connect-automatically))

(defun make-new-connection (robot &key (test-connection t))
  #+nao (start-nao-server robot :test-connection test-connection))

(defun disconnect-robot (robot)
  #+nao (stop-nao-server robot))