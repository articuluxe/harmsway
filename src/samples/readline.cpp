// -*- Mode: c++ -*-
// readline.cpp --- Integrate readline
// Copyright (C) 2019  Dan Harms (dharms)
// Author: Dan Harms <enniomore@icloud.com>
// Created: Friday, October 11, 2019
// Version: 1.0
// Modified Time-stamp: <2019-10-11 22:55:47 dharms>
// Modified by: Dan Harms
// Keywords: readline development

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

// Code:

#include <readline/readline.h>
#include <iostream>

/* compile: g++ -o readline.cpp -lreadline */
int main(int argc, char* argv[])
{
   char* line = readline("prompt> ");
   std::cout << "You entered \"" << line << "\"" << std::endl;
   free(line);
   return 0;
}

// code ends here
