// -*- Mode: c++ -*-
// kbhit.hpp --- wait for console input (unbuffered)
// Copyright (C) 2017  Dan Harms (dan.harms)
// Author: Dan Harms <danielrharms@gmail.com>
// Created: Friday, July 28, 2017
// Version: 1.0
// Modified Time-stamp: <2017-07-28 11:52:26 dan.harms>
// Modified by: Dan Harms
// Keywords:

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Commentary:
// Wait for any key to be pressed.

// Code:
#include <cstdio>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>

int kbhit()
{
    struct termios oldt;
    tcgetattr(STDIN_FILENO, &oldt);
    struct termios newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    int oldf = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, oldf | O_NONBLOCK);
    int ch = getchar();
    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    fcntl(STDIN_FILENO, F_SETFL, oldf);
    if (ch != EOF)
    {
        ungetc(ch, stdin);
        return 1;
    }
    return 0;
}

// code ends here
