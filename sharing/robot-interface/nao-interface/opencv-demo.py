#!python2.7
#!/usr/bin/env python

from __future__ import print_function

import cv2
import numpy as np
import imutils
import argparse
import json


def detect_shape(c):
    # initialize the shape name and approximate the contour
    shape = "unidentified"
    peri = cv2.arcLength(c, True)
    approx = cv2.approxPolyDP(c, 0.04 * peri, True)

    # if the shape is a triangle, it will have 3 vertices
    if len(approx) == 3:
        shape = "triangle"

    # if the shape has 4 vertices, it is either a square or
    # a rectangle
    elif len(approx) == 4:
        # compute the bounding box of the contour and use the
        # bounding box to compute the aspect ratio
        (x, y, w, h) = cv2.boundingRect(approx)
        ar = w / float(h)

        # a square will have an aspect ratio that is approximately
        # equal to one, otherwise, the shape is a rectangle
        shape = "square" if ar >= 0.9 and ar <= 1.1 else "rectangle"

    # if the shape is a pentagon, it will have 5 vertices
    elif len(approx) == 5:
        shape = "pentagon"

    # otherwise, we assume the shape is a circle
    else:
        shape = "circle"

    # return the name of the shape
    return shape


def get_color(image, c):
    mask = np.zeros(image.shape[:2], dtype="uint8")
    cv2.drawContours(mask, [c], -1, 255, -1)
    mask = cv2.erode(mask, None, iterations=2)
    mean = cv2.mean(image, mask=mask)[:3]
    return mean


def process(image_path):
    # Get the following data for each object:
    # Position of the center (X,Y), Shape, Color, Width, Height, ID

    # load the image and resize it to a smaller factor so that
    # the shapes can be approximated better
    image = cv2.imread(image_path)
    resized = imutils.resize(image, width=300)
    ratio = image.shape[0] / float(resized.shape[0])

    # convert it to grayscale, blur it slightly, and threshold it
    blurred = cv2.GaussianBlur(resized, (5, 5), 0)
    gray = cv2.cvtColor(blurred, cv2.COLOR_BGR2GRAY)
    # thresh = cv2.threshold(gray, 60, 255, cv2.THRESH_BINARY_INV)[1]
    # Currently, the args 31 and 10 are based on trial-and-error + black magic
    # We have to use THRESH_BINARY_INV,
    # since the objects are on a white background
    athresh = cv2.adaptiveThreshold(gray, 255, cv2.ADAPTIVE_THRESH_MEAN_C,
                                    cv2.THRESH_BINARY_INV, 31, 10)

    # find contours in the thresholded image
    cnts = cv2.findContours(athresh.copy(), cv2.RETR_EXTERNAL,
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
            cX = int((M["m10"] / M["m00"]) * ratio)
            cY = int((M["m01"] / M["m00"]) * ratio)

            # detect the shape of the contour and label the color
            shape = detect_shape(c)
            color = get_color(blurred, c)

            # Get Width and Height using Bounding Box
            # x, y, width, height = cv2.boundingRect(c)

            # Draw Bounding Box
            # cv2.rectangle(image, (int(x*ratio), int(y*ratio)),
            #               (int((x*ratio)+(width*ratio)),
            #                int((y*ratio)+(height*ratio))),
            #               (0,255,0), 2)

            # Get Width and Height using Rotated Bounding Box
            (_, _), (w, h), angle = cv2.minAreaRect(c)
            w = w * ratio
            h = h * ratio

            # Draw Rotated Bounding Box
            # rect = ((x,y), (w,h), angle)
            # box = cv2.boxPoints(rect)
            # box = np.int0(box)
            # cv2.drawContours(image, [box], 0, (0,0,255), 2)

            # multiply the contour (x, y)-coordinates by the resize ratio,
            # then draw the contours and the name of the shape and labeled
            # color on the image
            c = c.astype("float")
            c *= ratio
            c = c.astype("int")
            text = "{}".format(label)
            json_data = {
                'id': label,
                'x': cX,
                'y': cY,
                'width': w,
                'height': h,
                'angle': angle,
                'shape': shape,
                'average-b': color[0],
                'average-g': color[1],
                'average-r': color[2]
            }
            data.append(json_data)
            cv2.drawContours(image, [c], -1, (0, 255, 0), 2)
            cv2.circle(image, (cX, cY), 7, (255, 255, 255), -1)
            cv2.putText(image, text, (cX + 10, cY - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 2)

            cv2.imwrite('./analyzed.jpg', image)
    return

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--image-path',
                        action="store",
                        dest="image",
                        help="The image")

    cmd = parser.parse_args()
    if cmd.image is not None:
        process(cmd.image)
