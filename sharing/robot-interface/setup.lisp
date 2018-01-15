(in-package :robot-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In this file, you specify which robot you are using. It is then added to *features*, ;;
;; so you can use the #+ syntax. It also has functions for starting the connection with ;;
;; the robot                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We are using the NAO robot
(pushnew :nao *features*)
