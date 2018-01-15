#!python2.7
#!/usr/bin/env python

from __future__ import print_function

import json
import time

import naoqi
from naoqi import ALProxy


class NaoMovement(object):

    def __init__(self, ip, port):
        # Init proxies.
        try:
            self.motionProxy = ALProxy("ALMotion", ip, port)
        except Exception as e:
            print("Could not create proxy to ALMotion")
            print("Error was: ", e)

        try:
            self.postureProxy = ALProxy("ALRobotPosture", ip, port)
        except Exception as e:
            print("Could not create proxy to ALRobotPosture")
            print("Error was: ", e)

    def stiffness_on(self, proxy):
        # We use the "Body" name to signify the collection of all joints
        pNames = "Body"
        pStiffnessLists = 1.0
        pTimeLists = 1.0
        proxy.stiffnessInterpolation(pNames, pStiffnessLists, pTimeLists)


class NaoPosture(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoPosture, self).__init__(ip, port, *args, **kwargs)

    def set(self, posture="Stand"):
        ''' Go to the given posture '''
        self.stiffness_on(self.motionProxy)
        success = self.postureProxy.goToPosture(str(posture), 0.5)
        return json.dumps({'response': int(success)})

    def get(self):
        ''' Get the current posture '''
        posture = self.postureProxy.getPosture()
        return json.dumps({'posture': posture})

    def do(self, action="", **kwargs):
        if str(action) == "get":
            return self.get(**kwargs)
        elif str(action) == "set":
            return self.set(**kwargs)


class NaoJoints(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoJoints, self).__init__(ip, port, *args, **kwargs)

    def do(self, joint="HeadPitch", value=0.0):
        ''' Move the given joint to a given value '''
        self.stiffness_on(self.motionProxy)
        self.motionProxy.setAngles(str(joint), value, 0.3)
        return json.dumps({'response': 1})


class NaoSpeak(object):

    def __init__(self, ip, port):
        # Init proxies.
        try:
            self.ttsProxy = ALProxy("ALTextToSpeech", ip, port)
        except Exception as e:
            print("Could not create proxy to ALTextToSpeech")
            print("Error was: ", e)

    def do(self, speech=""):
        ''' Say something '''
        self.ttsProxy.say(str(speech))
        return json.dumps({'speech': speech})


class NaoRaiseArm(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoRaiseArm, self).__init__(ip, port, *args, **kwargs)

    def raise_left(self):
        SPcurrentAngle = self.motionProxy.getAngles("LShoulderPitch", False)[0]

        jointList = ["LShoulderPitch"]
        angleList = [0.0, SPcurrentAngle]
        timeList = [2.0, 6.0]
        isAbsolute = True

        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return json.dumps({'response': 1})

    def raise_right(self):
        SPcurrentAngle = self.motionProxy.getAngles("RShoulderPitch", False)[0]

        jointList = ["RShoulderPitch"]
        angleList = [0.0, SPcurrentAngle]
        timeList = [2.0, 6.0]
        isAbsolute = True

        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return json.dumps({'response': 1})

    def raise_both(self):
        LSPcurrentAngle = self.motionProxy.getAngles("LShoulderPitch", False)[0]
        LSRcurrentAngle = self.motionProxy.getAngles("LShoulderRoll", False)[0]
        RSPcurrentAngle = self.motionProxy.getAngles("RShoulderPitch", False)[0]
        RSRcurrentAngle = self.motionProxy.getAngles("RShoulderRoll", False)[0]

        jointList = ["LShoulderPitch", "LShoulderRoll", "RShoulderPitch", "RShoulderRoll"]
        angleList = [[0.0, LSPcurrentAngle], [-0.31, LSRcurrentAngle],
                     [0.0, RSPcurrentAngle], [0.31, RSRcurrentAngle]]
        timeList = [[2.0, 6.0], [2.0, 6.0],
                    [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True
        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return json.dumps({'response': 1})

    def do(self, arm=""):
        ''' Raise the left or right arm '''

        # Set Stiffness on
        self.stiffness_on(self.motionProxy)
        # Choose the correct shoulder joint
        if arm == "LArm":
            return self.raise_left()
        elif arm == "RArm":
            return self.raise_right()
        elif arm == "Both":
            return self.raise_both()


# class NaoRaiseBothArms(NaoMovement):


#     def __init__(self, ip, port, *args, **kwargs):
#         super(NaoRaiseBothArms, self).__init__(ip, port, *args, **kwargs)

#     def do(self, arm=""):
#         ''' Raise both arms. '''

#         # Set Stiffness on
#         self.stiffness_on(self.motionProxy)
 
#         # Raise the arm and put back
#         LcurrentAngle = self.motionProxy.getAngles("LShoulderPitch", False)[0]
#         RcurrentAngle = self.motionProxy.getAngles("RShoulderPitch", False)[0]
#         LScurrentAngle = self.motionProxy.getAngles("LShoulderRoll", False)[0]
#         RScurrentAngle = self.motionProxy.getAngles("RShoulderRoll", False)[0]
#         LangleList = [0.0, LcurrentAngle]
#         RangleList = [0.0, RcurrentAngle]
#         LSangleList = [-0.31, LScurrentAngle]
#         RSangleList = [0.31, RScurrentAngle]
#         timeList = [2.0, 6.0]
#         isAbsolute = True
# 	    self.motionProxy.post.angleInterpolation("LShoulderPitch", LangleList, timeList, isAbsolute)
#         self.motionProxy.post.angleInterpolation("RShoulderPitch", RangleList, timeList, isAbsolute)
# 	    self.motionProxy.post.angleInterpolation("LShoulderRoll", LSangleList, timeList, isAbsolute)
#         self.motionProxy.post.angleInterpolation("RShoulderRoll", RSangleList, timeList, isAbsolute)
#         return json.dumps({'response': 1})


class NaoHeadTouch(object):

    def __init__(self, ip, port):
        # Set memoryproxy
        try:
            self.memoryProxy = ALProxy("ALMemory", ip, port)
        except Exception as e:
            print("Could not create proxy to ALMemory")
            print("Error was: ", e)

        try:
            self.ledProxy = ALProxy("ALLeds", ip, port)
        except Exception as e:
            print("Could not create proxy to ALLeds")
            print("Error was: ", e)

        self.front_sensor = "Device/SubDeviceList/Head/Touch/Front/Sensor/Value"
        self.middle_sensor = "Device/SubDeviceList/Head/Touch/Middle/Sensor/Value"
        self.rear_sensor = "Device/SubDeviceList/Head/Touch/Rear/Sensor/Value"
        self.front_leds = "BrainLedsFront"
        self.middle_leds = "BrainLedsMiddle"
        self.back_leds = "BrainLedsBack"
        self.delay = 0.5

    def all_leds_on(self):
        for led in [self.front_leds, self.middle_leds, self.back_leds]:
            self.ledProxy.on(led)

    def all_leds_off(self):
        for led in [self.front_leds, self.middle_leds, self.back_leds]:
            self.ledProxy.off(led)

    def front_or_back(self):
        self.all_leds_on()
        self.ledProxy.off(self.middle_leds)
        while True:
            if self.memoryProxy.getData(self.front_sensor) > 0.5:
                self.all_leds_on()
                return json.dumps({'detected': 1})
            elif self.memoryProxy.getData(self.rear_sensor) > 0.5:
                self.all_leds_on()
                return json.dumps({'detected': 0})
            time.sleep(self.delay)

    def detect_touch(self, region="Front"):
        self.all_leds_off()
        if region == "Front":
            self.ledProxy.on(self.front_leds)
            while self.memoryProxy.getData(self.front_sensor) < 0.5:
                time.sleep(self.delay)
            self.all_leds_on()
            return json.dumps({'touch': 1})
        elif region == "Rear":
            self.ledProxy.on(self.back_leds)
            while self.memoryProxy.getData(self.rear_sensor) < 0.5:
                time.sleep(self.delay)
            self.all_leds_on()
            return json.dumps({'touch': 1})
        elif region == "Middle":
            self.ledProxy.on(self.middle_leds)
            while self.memoryProxy.getData(self.middle_sensor) < 0.5:
                time.sleep(self.delay)
            self.all_leds_on()
            return json.dumps({'touch': 1})

    def do(self, action="", **kwargs):
        if str(action) == "detect":
            return self.detect_touch(**kwargs)
        elif str(action) == "front-back":
            return self.front_or_back(**kwargs)


class NaoSpeechRecognition(object):

    def __init__(self, ip, port):
        try:
            self.asrProxy = ALProxy("ALSpeechRecognition", ip, port)
        except Exception as e:
            print("Could not create proxy to ALSpeechRecognition")
            print("Error was: ", e)

        try:
            self.memoryProxy = ALProxy("ALMemory", ip, port)
        except Exception as e:
            print("Could not create proxy to ALMemory")
            print("Error was: ", e)

        try:
            autoMovesProxy = ALProxy("ALAutonomousMoves", ip, port)
            autoMovesProxy.setExpressiveListeningEnabled(False)
        except Exception as e:
            print("Could not create proxy to ALAutonomousMoves")
            print("Error was: ", e)

    def start_speech_recognition(self, vocabulary=[]):
        ''' Start the speech recognition, given a vocabulary '''
        self.asrProxy.setLanguage("English")
        vocab = [str(v) for v in vocabulary]
        self.asrProxy.setVocabulary(vocab, False)
        subscriber = "Nao_ASR_" + str(int(time.time()))
        self.asrProxy.subscribe(subscriber)
        return json.dumps({'subscriber': subscriber})

    def stop_speech_recognition(self, subscriber=""):
        ''' Stop the speech recognition, get the detected word(s) '''
        detected = self.memoryProxy.getData("WordRecognized")
        self.asrProxy.unsubscribe(str(subscriber))
        return json.dumps({'detected': detected})

    def do(self, action="", **kwargs):
        if str(action) == "start":
            return self.start_speech_recognition(**kwargs)
        elif str(action) == "stop":
            return self.stop_speech_recognition(**kwargs)

class NaoMoveHead(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoMoveHead, self).__init__(ip, port, *args, **kwargs)

    def say_yes(self):
        current_head_pitch = self.motionProxy.getAngles("HeadPitch", False)[0]
        names = ["HeadPitch"]
        angleList = [-0.4, 0.3, -0.4, 0.3, current_head_pitch]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.angleInterpolation(names, angleList, timeList, True)
        return json.dumps({'response': 1})

    def say_no(self):
        current_head_yaw = self.motionProxy.getAngles("HeadYaw", False)[0]
        names = ["HeadYaw"]
        angleList = [1.0, -1.0, 1.0, -1.0, current_head_yaw]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.angleInterpolation(names, angleList, timeList, True)
        return json.dumps({'response': 1})

    def do(self, yesno=""):
        self.motionProxy.setStiffnesses("Head", 1.0)
        if yesno == "yes":
            return self.say_yes()
        elif yesno == "no":
            return self.say_no()


### These two classes could be merged together
### NaoMovementAndSpeak
class NaoPointSpeak(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoPointSpeak, self).__init__(ip, port, *args, **kwargs)
        try:
            self.ttsProxy = ALProxy("ALTextToSpeech", ip, port)
        except Exception as e:
            print("Could not create proxy to ALTextToSpeech")
            print("Error was: ", e)

    def do(self, arm="", speech=""):
        self.stiffness_on(self.motionProxy)
        if arm == "LArm":
            jointName = "LShoulderPitch"
        elif arm == "RArm":
            jointName = "RShoulderPitch"
        # Raise the arm and put back
        currentAngle = self.motionProxy.getAngles(jointName, False)[0]
        angleList = [0.0, currentAngle]
        timeList = [2.0, 6.0]
        isAbsolute = True
        self.motionProxy.post.angleInterpolation(jointName, angleList, timeList, isAbsolute)
        self.ttsProxy.say(str(speech))
        return json.dumps({'speech': speech})

class NaoHeadSpeak(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoHeadSpeak, self).__init__(ip, port, *args, **kwargs)
        try:
            self.ttsProxy = ALProxy("ALTextToSpeech", ip, port)
        except Exception as e:
            print("Could not create proxy to ALTextToSpeech")
            print("Error was: ", e)

    def say_yes(self):
        current_head_pitch = self.motionProxy.getAngles("HeadPitch", False)[0]
        names = ["HeadPitch"]
        angleList = [-0.4, 0.3, -0.4, 0.3, current_head_pitch]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.post.angleInterpolation(names, angleList, timeList, True)

    def say_no(self):
        current_head_yaw = self.motionProxy.getAngles("HeadYaw", False)[0]
        names = ["HeadYaw"]
        angleList = [1.0, -1.0, 1.0, -1.0, current_head_yaw]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.post.angleInterpolation(names, angleList, timeList, True)

    def do(self, yesno="", speech=""):
        self.motionProxy.setStiffnesses("Head", 1.0)
        if yesno == "yes":
            self.say_yes()
        elif yesno == "no":
            self.say_no()
        self.ttsProxy.say(str(speech))
        return json.dumps({'speech': speech})


# class NaoPoint(NaoMovement):

#     def __init__(self, ip, port, *args, **kwargs):
#         super(NaoPoint, self).__init__(ip, port, *args, **kwargs)

#     def do(self, x=0, y=0, z=0, arm=""):

#         # Set NAO in Stiffness On
#         self.stiffness_on(self.motionProxy)

#         effector = str(arm)
#         space = motion.FRAME_TORSO
#         axisMask = almath.AXIS_MASK_VEL # just control position
#         isAbsolute = False

#         # Since we are in relative, the current position is zero
#         currentPos = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

#         # Define the changes relative to the current position
#         dx         =  x      # translation axis X (meters)
#         dy         =  y      # translation axis Y (meters)
#         dz         =  z      # translation axis Z (meters)
#         dwx        =  0.00      # rotation axis X (radians)
#         dwy        =  0.00      # rotation axis Y (radians)
#         dwz        =  0.00      # rotation axis Z (radians)
#         targetPos  = [dx, dy, dz, dwx, dwy, dwz]

#         # Go to the target and back again
#         path       = [targetPos, currentPos]
#         times      = [2.0, 4.0] # seconds

#         self.motionProxy.positionInterpolation(effector, space, path, axisMask, times, isAbsolute)
#         return json.dumps({'response': 1})

