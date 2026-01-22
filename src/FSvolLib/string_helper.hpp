#pragma once

#include <string>
#include <algorithm> // For std::transform
#include <cctype>    // For ::toupper

static class string_helper
{
public:
	static std::string StrToUpper(std::string s)
	{
		std::transform(s.begin(), s.end(), s.begin(), ::toupper);
		return s;
	}
};

