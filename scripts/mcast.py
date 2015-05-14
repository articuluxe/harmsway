#!/usr/bin/python

import socket
import struct
import sys

if len(sys.argv) != 4:
   sys.exit("Usage: mcast.py local_address multicast_group port\n\nExample: ./mcast.py 10.10.102.74 239.10.25.6 10001")

multicast_group = sys.argv[2]
server_address = (multicast_group,int(sys.argv[3]))
local_address = sys.argv[1]

# Create the socket
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

# Bind to the server address
sock.bind(server_address)

# Tell the operating system to add the socket to the multicast group
# on specified interface.
group = socket.inet_aton(multicast_group)
local_if = socket.inet_aton(local_address)
# The packing format must be 4s4s. Many examples show a pack format of "ll"
# which makes it fail to pack the local_if unless it is carefully crafted.
mreq = struct.pack('4s4s', group, local_if)
sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)

# Receive/respond loop
while True:
    print >>sys.stderr, '\nwaiting to receive message'
    data, address = sock.recvfrom(1024)

    print >>sys.stderr, 'received %s bytes from %s' % (len(data), address)
    #print >>sys.stderr, data

    #print >>sys.stderr, 'sending acknowledgement to', address
    #sock.sendto('ack', address)

