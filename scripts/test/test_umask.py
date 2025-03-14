#!/bin/env python3
# -*- Mode: python -*-
# test_umask.py --- test umask settings in python
# Copyright (C) 2019  Dan Harms (dan.harms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, September 27, 2019
# Version: 1.0
# Modified Time-stamp: <2019-10-15 10:46:31 dan.harms>
# Modified by: Dan Harms
# Keywords: tools

import os
import sys


def main(dir):
    old = os.umask(0o02)
    print("Prior umask was ", oct(old))
    os.makedirs(dir)


if __name__ == '__main__':
    main(sys.argv[1])

# code ends here
