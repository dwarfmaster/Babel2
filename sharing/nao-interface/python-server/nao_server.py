#!python2.7
#!/usr/bin/env python

from __future__ import print_function

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import json
import argparse

import naoqi
from naoActions import NaoPosture, NaoJoints, NaoSpeak, NaoRaiseArm, NaoHeadTouch, NaoSpeechRecognition, NaoPointSpeak,\
    NaoMoveHead, NaoHeadSpeak
from naoTests import NaoTestConnection
from naoVision import NaoVision


class NaoActionHandler(object):
    """
    This class dispatches all requests to the Nao
    to the correct handler class. Each handler class
    should implement a do method that returns some json
    data. The json data is send back to Babel2.
    """

    def __init__(self, robot_ip, robot_port):
        self._robot_ip = robot_ip
        self._robot_port = robot_port
        self.handlers = {
            "testConnection": NaoTestConnection,
            "vision": NaoVision,
            "speak": NaoSpeak,
            "posture": NaoPosture,
            "setJoint": NaoJoints,
            "raiseArm": NaoRaiseArm,
            "headTouch": NaoHeadTouch,
            "speechRecognition": NaoSpeechRecognition,
            "moveHead": NaoMoveHead,
            # Combined actions
            # To Do: figure out a way to make this more compositional
            "pointSpeak": NaoPointSpeak,
            "headSpeak": NaoHeadSpeak
        }

    def handle(self, action, data_kwargs):
        if action in self.handlers:
            nao = self.handlers[action](self._robot_ip, self._robot_port)
            return nao.do(**data_kwargs)


def handler_class_factory(action_handler):

    class NaoHTTPHandler(BaseHTTPRequestHandler, object):
        """ This class receives the HTTP requests for the Nao. """

        def __init__(self, *args, **kwargs):
            self.action_handler = action_handler
            super(NaoHTTPHandler, self).__init__(*args, **kwargs)

        def _set_headers(self, length):
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(length))
            self.end_headers()

        def do_GET(self):
            self._set_headers(0)

        def do_HEAD(self):
            self._set_headers(0)

        def do_POST(self):
            content_len = int(self.headers.getheader('Content-Length'))
            post_body = self.rfile.read(content_len)
            json_data = json.loads(post_body)
            action = json_data['action']
            data_kwargs = json_data['data'] if json_data['data'] is not None else {}
            json_response = self.action_handler.handle(action, data_kwargs)
            self._set_headers(len(json_response))
            self.wfile.write(json_response)

    return NaoHTTPHandler


class NaoServer(object):

    def __init__(self, robot_ip,
                 robot_port=9559,
                 server_host="0.0.0.0",  # Host inside Docker
                 server_port=80):  # Port 80 inside Docker
        self._robot_ip = robot_ip
        self._robot_port = robot_port
        self._server_port = server_port
        self._server_host = server_host

    def run(self):
        server_address = (self._server_host, self._server_port)
        nao_action_handler = NaoActionHandler(self._robot_ip, self._robot_port)
        nao_http_handler = handler_class_factory(nao_action_handler)
        httpd = HTTPServer(server_address, nao_http_handler)
        httpd.serve_forever()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--robot-ip',
                        action="store",
                        dest="robot_ip",
                        default="192.168.1.2",
                        help="The robot's IP address")
    parser.add_argument('--robot-port',
                        action="store",
                        dest="robot_port",
                        default=9559,
                        type=int,
                        help="The robot's port number")
    cmd = parser.parse_args()

    if cmd.robot_ip is not None:
        nao_server = NaoServer(cmd.robot_ip, robot_port=cmd.robot_port)
        nao_server.run()
