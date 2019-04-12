// -*- Mode: c++ -*-
// simple_enum.hpp --- a simple enum
// Copyright (C) 2018  Dan Harms (dan.harms)
// Author: Dan Harms <dan.harms@xrtrading.com>
// Created: Wednesday, September 26, 2018
// Version: 1.0
// Modified Time-stamp: <2018-09-26 10:13:42 dan.harms>
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

// Code:

struct State
{
    State() : all(0)
    {}

    bool isFirst() const { return first; }

    union
    {
        char all;
        struct
        {
            char first  : 1;
            char second : 1;
            char third  : 1;
            char fourth : 1;
        };
    };

};

#include <iostream>

int main(int argc, char* argv[])
{
    State state;
    state.first = !state.first;
    if (state.isFirst())
        return 1;
    return 0;
}

// code ends here
