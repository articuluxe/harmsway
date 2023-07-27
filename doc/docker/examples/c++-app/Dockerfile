FROM ubuntu:23.10

ENV PORT 31234
EXPOSE $PORT/udp

RUN apt-get update && apt-get install --no-install-recommends -y \
    build-essential cmake wget git

RUN apt-get install --reinstall ca-certificates -y && \
    mkdir /usr/local/share/ca-certificates/cacert.org && \
    wget -P /usr/local/share/ca-certificates/cacert.org http://www.cacert.org/certs/root.crt http://www.cacert.org/certs/class3.crt && \
    update-ca-certificates && \
    git config --global http.sslCAinfo /etc/ssl/certs/ca-certificates.crt

WORKDIR /tmp
RUN git clone https://github.com/google/googletest.git
RUN mkdir googletest/build
WORKDIR /tmp/googletest/build
RUN cmake .. \
    && make && make install && rm -rf /tmp/googletest

WORKDIR /work
COPY . .
RUN make clean && make all
RUN chmod a+x <exe>
RUN make test

CMD "./exe"
