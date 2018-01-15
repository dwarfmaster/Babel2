(in-package :robot-interface)

(export '(sit stand crouch current-posture look-up-down look-left-right say-yes say-no point
              point-and-speak say-yes-and-speak say-no-and-speak))

(defmethod sit ((robot robot) &key)
  " Go to a sitting posture "
  (go-to-posture robot "Sit"))

(defmethod stand ((robot robot) &key)
  (go-to-posture robot "Stand"))

(defmethod stand-init ((robot robot) &key)
  " Go to a standing posture "
  (go-to-posture robot "StandInit"))

(defmethod stand-zero ((robot robot) &key)
  (go-to-posture robot "StandZero"))

(defmethod crouch ((robot robot) &key)
  " Go to a crouching posture "
  (go-to-posture robot "Crouch"))

(defmethod current-posture ((robot robot) &key)
  " Get the current posture "
  (get-posture robot))

(defmethod look-up-down ((robot robot) value &key (unit :degrees))
  " Move the head up or down. Default unit: degrees. Radians also possible. "
  (cond ((eq unit :degrees)
         (set-head-joint robot "HeadPitch" (deg-to-rad value)))
        ((eq unit :radians)
         (set-head-joint robot "HeadPitch" value))
        (t
         (error (format nil "~a is not a valid unit" unit)))))

(defmethod look-left-right ((robot robot) value &key (unit :degrees))
  " Move the head left or down. Default unit: degrees. Radians also possible. "
  (cond ((eq unit :degrees)
         (set-head-joint robot "HeadYaw" (deg-to-rad value)))
        ((eq unit :radians)
         (set-head-joint robot "HeadYaw" value))
        (t
         (error (format nil "~a is not a valid unit" unit)))))

(defmethod say-yes ((robot robot) &key)
  " Say yes using the robot's head "
  (head-say robot "yes"))

(defmethod say-no ((robot robot) &key)
  " Say no using the robot's head "
  (head-say robot "no"))

(defmethod point ((robot robot) y &key)
  "Raise left or right arm."
  (if (> y 0.0)
    (raise-left-arm robot)
    (raise-right-arm robot)))

;; These methods are hacked together
(defmethod point-and-speak ((robot robot) y speech &key)
  "Raise arm and speak at the same time"
  (if (> y 0.0)
    (raise-left-and-speak robot speech)
    (raise-right-and-speak robot speech)))

(defmethod say-yes-and-speak ((robot robot) speech &key)
  (head-say-and-speak robot "yes" speech))

(defmethod say-no-and-speak ((robot robot) speech &key)
  (head-say-and-speak robot "no" speech))