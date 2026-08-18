#pragma once

#include <vector>
#include <algorithm>  // for lower_bound
#include <iterator>    // std::distance

class array_helper
{
public:

    template <typename Container, typename T>
    static int findIndexInSortedArray(const Container& arr, const T& value)
    {
        auto it = std::lower_bound(arr.begin(), arr.end(), value);

        if (it != arr.end() && *it == value)
        {
            return static_cast<int>(std::distance(arr.begin(), it));
        }
        return -1;
    }

    template <typename Container, typename T>
    static int findIndexInUnsortedArray(const Container& arr, const T& value)
    {
        auto it = std::find(arr.begin(), arr.end(), value);

        if (it != arr.end())
        {
            return static_cast<int>(std::distance(arr.begin(), it));
        }
        return -1;
    }
};