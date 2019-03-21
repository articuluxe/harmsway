#include <typeinfo>
#include <iostream>

template <typename T>
struct trait;

template<>
struct trait<bool>
{
    using type = bool;
};

struct not_specialized {};

template <typename, typename = void>
struct has_trait_helper : std::false_type {};

template <typename T>
//struct has_trait_helper<T, decltype(trait<T>::type)> : std::true_type {};
struct has_trait_helper<T, typename trait<T>::type> : std::true_type {};

template <typename T>
struct has_trait : has_trait_helper<T, T> {};

int main()
{
    std::cout << "Bool:" << has_trait<bool>::value << std::endl;
    std::cout << "Other:" << has_trait<not_specialized>::value << std::endl;
}
