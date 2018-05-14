#!python2.7
# !/usr/bin/env python

from __future__ import print_function

import cv2
import numpy as np
import imutils
import argparse
import json
import time

import naoqi
from naoqi import ALProxy


class NaoVision(object):

    def __init__(self, ip, port):
        # Init proxies.
        try:
            self.photoCaptureProxy = ALProxy("ALPhotoCapture", ip, port)
        except Exception as e:
            print("Error when creating ALPhotoCapture proxy:")
            print(str(e))
        # Set camera parameters
        self.photoCaptureProxy.setResolution(2)
        self.photoCaptureProxy.setPictureFormat("jpg")

    def detect_sides(self, c):
        # initialize the shape name and approximate the contour
        peri = cv2.arcLength(c, True)
        approx = cv2.approxPolyDP(c, 0.04 * peri, True)
        return len(approx)

    def get_color(self, image, c):
        # Calculate the mean color of the given contour c
        mask = np.zeros(image.shape[:2], dtype="uint8")
        cv2.drawContours(mask, [c], -1, 255, -1)
        mask = cv2.erode(mask, None, iterations=2)
        mean = cv2.mean(image, mask=mask)[:3]
        return mean

    def capture(self):
        # Create a unique name for picture -> current timestamp
        img_name = str(int(time.time()))
        # Take the picture
        img_path = "/home/nao/recordings/cameras/"
        self.photoCaptureProxy.takePicture(img_path, img_name)
        # We want path, name and ext separately (?)
        # Could also return _, but then need parsing in Lisp
        # to get filename and extension out
        return json.dumps({'path': img_path, 'name': img_name, 'ext': "jpg"})

    def analyze(self, filename=""):
        # Get the following data for each object:
        # Position of the center (X,Y), Shape, Color, Width, Height, ID

        # load the image and resize it to a smaller factor so that
        # the shapes can be approximated better

        img_dir = '/naoqi/src/img/'

        path = img_dir + filename
        image = cv2.imread(path)
        dst = cv2.fastNlMeansDenoisingColored(image, None, 10, 10, 7, 21)
        hsv = cv2.cvtColor(dst, cv2.COLOR_BGR2HSV)
        h, s, v = cv2.split(hsv)
        retval, thresholded = cv2.threshold(s, 0, 255,
                                            cv2.THRESH_BINARY+cv2.THRESH_OTSU)
        medianFiltered = cv2.medianBlur(thresholded, 5)
        cnts = cv2.findContours(medianFiltered,
                                cv2.RETR_TREE,
                                cv2.CHAIN_APPROX_SIMPLE)
        cnts = cnts[0] if imutils.is_cv2() else cnts[1]

        # loop over the contours and collect data
        data = []
        for i, c in enumerate(cnts):
            # Get the contour's Moments
            M = cv2.moments(c)
            if M["m00"] > 0.0:
                # Assign a label to the contour
                label = "obj-{}".format(i)

                # compute the center of the contour
                cX = int(M["m10"] / M["m00"])
                cY = int(M["m01"] / M["m00"])

                # detect the shape of the contour and label the color
                sides = self.detect_sides(c)
                color = self.get_color(image, c)
                area = cv2.contourArea(c)

                # Get Width and Height using Rotated Bounding Box
                (_, _), (w, h), angle = cv2.minAreaRect(c)
                bb_area = w * h
                area_ratio = area / bb_area
                ratio = w / h if w < h else h / w
                if w < 30 or h < 30:
                    continue

                # multiply the contour (x, y)-coordinates by the resize ratio,
                # then draw the contours and the name of the shape and labeled
                # color on the image
                c = c.astype("float")
                c = c.astype("int")
                json_data = {label: {
                        'xpos': cX,
                        'ypos': cY,
                        'width': w,
                        'height': h,
                        'angle': angle,
                        'sides': sides,
                        'area': area,
                        'bb-area': bb_area,
                        'area-ratio': area_ratio,
                        'size-ratio': ratio,
                        'color': color[::-1]
                    }}
                data.append(json_data)
                cv2.drawContours(dst, [c], -1, (0, 255, 0), 2)
                cv2.circle(dst, (cX, cY), 7, (0, 255, 0), -1)
                cv2.putText(dst, label, (cX + 10, cY - 10),
                            cv2.FONT_HERSHEY_SIMPLEX,
                            0.5, (0, 255, 0), 2)

        unix = filename.split('.')[0]
        outname = unix + '-analysis'
        outfile = outname + '.jpg'
        outpath = img_dir + outfile
        cv2.imwrite(outpath, dst)

        return json.dumps({"filename": outname, "data": data})

    def do(self, action="", **kwargs):
        if str(action) == "capture":
            return self.capture(**kwargs)
        elif str(action) == "analyze":
            return self.analyze(**kwargs)
