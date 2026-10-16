"""Loopback-only static preview, including the site's custom 404 response."""
from pathlib import Path
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from functools import partial
import argparse

repo = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument('--port', type=int, default=1114)
parser.add_argument('--directory', default='public-preview')
args = parser.parse_args()
directory = (repo / args.directory).resolve()
if not directory.is_relative_to(repo) or not directory.is_dir():
    raise SystemExit('Build directory must exist inside this repository.')

class PreviewHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('X-Robots-Tag', 'noindex, nofollow, noarchive')
        self.send_header('Cache-Control', 'no-store')
        super().end_headers()

    def send_error(self, code, message=None, explain=None):
        page = directory / '404.html'
        if code == 404 and page.is_file():
            body = page.read_bytes()
            self.send_response(404)
            self.send_header('Content-Type', 'text/html; charset=utf-8')
            self.send_header('Content-Length', str(len(body)))
            self.end_headers()
            if self.command != 'HEAD':
                self.wfile.write(body)
            return
        super().send_error(code, message, explain)

server = ThreadingHTTPServer(('127.0.0.1', args.port), partial(PreviewHandler, directory=str(directory)))
print(f'Private local preview: http://127.0.0.1:{args.port}/', flush=True)
try:
    server.serve_forever()
except KeyboardInterrupt:
    pass
finally:
    server.server_close()
