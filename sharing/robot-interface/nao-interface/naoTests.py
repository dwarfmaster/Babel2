#!/usr/bin/env python

from __future__ import print_function

import json


class NaoTestConnection():

    def __init__(self, ip, port):
        self.ip = ip
        self.port = port

    def do(self, testMessage=""):
        """ Test the connection. Simply sends back the received message """
        data = {"testMessage": testMessage}
        json_data = json.dumps(data)
        return json_data
