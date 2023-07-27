# HOW TO USE #

Add a makefile.

1. Build image: `docker build -t repository/tag:version .`
2. Run image: `docker run --rm -p 127.0.0.1:51234:31234/udp repository/tag`
   Maps container's udp port 31234 to localhost.
3. Test sending data to image: `netcat -u 127.0.0.1 51234`
   Then type in input to app.
