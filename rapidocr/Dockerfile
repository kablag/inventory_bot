FROM python:3.12-slim-bookworm

RUN pip install --no-cache-dir \
    rapidocr \
    onnxruntime

WORKDIR /data
ENTRYPOINT ["rapidocr"]
