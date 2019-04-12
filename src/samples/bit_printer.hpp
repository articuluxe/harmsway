#pragma once

#include <limits>
#include <sstream>

template <typename T>
static std::string print_bits(T t)
{
    std::ostringstream out("0x", std::ios_base::ate);
    const size_t bits = std::numeric_limits<T>::digits;
    for (size_t i = 1 << (bits-1); i > 0; i >>= 1)
    {
        out << !!(t & i);
    }
    return out.str();
}
