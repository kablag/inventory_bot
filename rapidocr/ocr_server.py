import base64
import json
import logging
import os
import tempfile
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

from rapidocr import RapidOCR

logging.basicConfig(level=logging.INFO)
engine = RapidOCR()


class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path != "/health":
            self.send_error(404)
            return
        self.reply(200, {"ok": True})

    def do_POST(self):
        if self.path != "/ocr":
            self.send_error(404)
            return

        path = None
        try:
            size = int(self.headers.get("Content-Length", "0"))
            if size < 1 or size > 25 * 1024 * 1024:
                self.reply(413, {"error": "Invalid image size"})
                return

            request = json.loads(self.rfile.read(size))
            image = base64.b64decode(
                request["image"], validate=True
            )

            with tempfile.NamedTemporaryFile(
                suffix=".jpg", delete=False
            ) as f:
                f.write(image)
                path = f.name

            result = engine(path)
            lines = []

            if result.txts is not None:
                for txt, score, box in zip(
                    result.txts,
                    result.scores,
                    result.boxes
                ):
                    lines.append({
                        "text": str(txt),
                        "confidence": round(float(score), 4),
                        "box": box.tolist()
                    })

            self.reply(200, {
                "full_text": "\n".join(
                    line["text"] for line in lines
                ),
                "lines": lines
            })

        except Exception:
            logging.exception("OCR failed")
            self.reply(500, {"error": "OCR failed"})
        finally:
            if path and os.path.exists(path):
                os.unlink(path)

    def reply(self, status, data):
        payload = json.dumps(
            data, ensure_ascii=False
        ).encode("utf-8")

        self.send_response(status)
        self.send_header(
            "Content-Type", "application/json; charset=utf-8"
        )
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)


if __name__ == "__main__":
    ThreadingHTTPServer(
        ("0.0.0.0", 8080), Handler
    ).serve_forever()
