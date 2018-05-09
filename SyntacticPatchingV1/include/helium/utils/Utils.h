#ifndef __UTIL_H__
#define __UTIL_H__

#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>

#include <pugixml.hpp>
#include <stdlib.h>
#include <sys/types.h>

// #include "RandUtils.h"

// #include "XMLUtils.h"
// #include "StringUtils.h"
// #include "ThreadUtils.h"
// #include "FSUtils.h"
// #include "ColorUtils.h"

namespace utils {
  bool is_number(const std::string& s);

  int get_line_number(std::string filename, std::string pattern);
  std::vector<int> get_line_numbers(std::string filename, std::string pattern);

  double get_time();
  void debug_time(std::string id="");
}

#endif
