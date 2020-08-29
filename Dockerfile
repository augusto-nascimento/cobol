FROM opensourcecobol/opensource-cobol

WORKDIR /oscobol/src/
COPY src /oscobol/src/
ENTRYPOINT tail -f /dev/null