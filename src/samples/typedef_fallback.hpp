#pragma once

template <typename, typename Fallback, typename = void>
struct type_or_default
{
    using type = Fallback;
};

template <typename Class, typename Fallback>
struct type_or_default<Class, Fallback, std::void_t<typename Class::type>>
{
    using type = typename Class::type;
};

template <typename T>
using type = type_or_default<T, int>::type;
