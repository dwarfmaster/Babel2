from __future__ import print_function

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import json

class SimpleHandler(BaseHTTPRequestHandler, object):

    def __init__(self, *args, **kwargs):
        super(SimpleHandler, self).__init__(*args, **kwargs)

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
        print(post_body)
        json_data = json.loads(post_body)
        print(json_data)
        json_response = json.dumps({'hello': 'world'})
        self._set_headers(len(json_response))
        self.wfile.write(json_response)

def run():
    server_address = ("0.0.0.0", 80)
    httpd = HTTPServer(server_address, SimpleHandler)
    httpd.serve_forever()

if __name__ == '__main__':
    run()

