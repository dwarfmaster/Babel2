
(in-package :robot-interface)

(export '(head-touch-front head-touch-middle head-touch-rear head-yes-no))

(defun head-touch-front (robot)
  "Detect if the front head sensor is touched. Blocking call."
  #+nao (nao-detect-touch robot :head :front))

(defun head-touch-middle (robot)
  "Detect if the middle head sensor is touched. Blocking call."
  #+nao (nao-detect-touch robot :head :middle))

(defun head-touch-rear (robot)
  "Detect if the rear head sensor is touched. Blocking call."
  #+nao (nao-detect-touch robot :head :rear))

(defun head-yes-no (robot)
  "Give yes/no feedback to the robot through head touch. Yes = front. No = rear."
  #+nao (nao-head-yes-no robot))