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

	static std::string pad3(int n) {
		std::ostringstream oss;
		oss << std::setw(3) << std::setfill('0') << (n < 0 ? 0 : (n % 1000));
		return oss.str();
	}

};

