FROM augustorocha/gnucobol:latest

WORKDIR /oscobol/src/
COPY src /oscobol/src/
ENTRYPOINT /bin/bash