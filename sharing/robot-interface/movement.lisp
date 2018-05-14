(in-package :robot-interface)

(export '(sit stand stand-init stand-zero crouch current-posture look-up-down look-left-right say-yes say-no point
              point-and-speak say-yes-and-speak say-no-and-speak))

(defun sit (robot)
  "Go to a sitting posture"
  #+nao (nao-go-posture robot :sit))

(defun stand (robot)
  "Go to a standing posture"
  #+nao (nao-go-posture robot :stand))

(defun stand-init (robot)
  "Stand with more balance"
  #+nao (nao-go-posture robot :stand-init))

(defun stand-zero (robot)
  "Stand with stretched arms"
  #+nao (nao-go-posture robot :stand-zero))

(defun crouch (robot)
  "Go to a crouching posture"
  #+nao (nao-go-posture robot :crouch))

(defun current-posture (robot)
  "Return the current posture"
  #+nao (nao-get-posture robot))

(defun look-up-down (robot value &key (unit :degrees))
  "Move the head up or down. Default unit: degrees. Radians also possible."
  #+nao (cond ((eq unit :degrees)
               (nao-set-joint robot :head :head-pitch :value (deg-to-rad value)))
              ((eq unit :radians)
               (nao-set-joint robot :head :head-pitch :value value))
              (t
               (error (format nil "~a is not a valid unit" unit)))))

(defun look-left-right (robot value &key (unit :degrees))
  " Move the head left or down. Default unit: degrees. Radians also possible. "
  #+nao (cond ((eq unit :degrees)
              (nao-set-joint robot :head :head-yaw :value (deg-to-rad value)))
             ((eq unit :radians)
              (nao-set-joint robot :head :head-yaw :value value))
             (t
              (error (format nil "~a is not a valid unit" unit)))))

(defun say-yes (robot)
  " Say yes using the robot's head "
  #+nao (nao-head-say robot :yes))

(defun say-no (robot)
  " Say no using the robot's head "
  #+nao (nao-head-say robot :no))

(defun point (robot left-or-right)
  "Raise left or right arm."
  #+nao (if (> left-or-right 0.0)
          (nao-raise-arm robot :left)
          (nao-raise-arm robot :right)))

;; These methods are hacked together
(defun point-and-speak (robot left-or-right speech)
  "Raise arm and speak at the same time"
  #+nao (if (> left-or-right 0.0)
          (raise-left-and-speak robot speech)
          (raise-right-and-speak robot speech)))

(defun say-yes-and-speak (robot speech)
  #+nao (head-say-and-speak robot "yes" speech))

(defun say-no-and-speak (robot speech)
  #+nao (head-say-and-speak robot "no" speech))
