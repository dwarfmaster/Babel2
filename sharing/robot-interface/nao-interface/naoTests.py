#!/usr/bin/env python

from __future__ import print_function

import json


class NaoTestConnection():

    def __init__(self, ip, port):
        self.ip = ip
        self.port = port

    def do(self, testMessage=""):
        ''' Test the connection. Simply sends back the received message '''
        self.data = {}
        self.data['testMessage'] = testMessage
        self.json_data = json.dumps(self.data)
        return self.json_data
