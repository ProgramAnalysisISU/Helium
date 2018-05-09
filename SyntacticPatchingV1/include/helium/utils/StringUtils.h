#ifndef STRING_UTILS_H
#define STRING_UTILS_H

#include <string>
#include <algorithm>
#include <vector>

#include <set>

#include <boost/algorithm/string/trim.hpp>

namespace utils {
  /**
   * Trim a string. Modify in position
   */
  // trim from start
  inline std::string &ltrim(std::string &s) {
    // s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
    boost::trim_left(s);
    return s;
  }

  // trim from end
  inline std::string &rtrim(std::string &s) {
    // s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
    boost::trim_right(s);
    return s;
  }

  // trim from both ends
  inline std::string &trim(std::string &s) {
    // return ltrim(rtrim(s));
    boost::trim(s);
    return s;
  }

  /*******************************
   ** string
   *******************************/

  bool ends_with(const std::string &s, const std::string &pattern);
  bool starts_with(const std::string &s, const std::string &pattern);
  // std::vector<std::string>& split(const std::string &s, char delim, std::vector<std::string> &elems);
  std::vector<std::string> split(const std::string &s, char delim);
  std::vector<std::string> split(std::string s, std::string delim);
  std::vector<std::string> split_by_string(std::string s, std::string delim);
  /*
   * This is the split function that use any white space as delimiter
   * Use completely different implementation from the other two
   */
  std::vector<std::string> split(const std::string &s);
  void remove(std::string& s, const std::string& pattern);
  void replace(std::string &s, std::string pattern, std::string replacement);
  std::string indent_string(std::string raw, int indent=1);
  
  std::set<std::string> extract_id_to_resolve(std::string code);


  std::string lisp_pretty_print(std::string str);
}

#endif /* STRING_UTILS_H */
