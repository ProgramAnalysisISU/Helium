#include "helium/utils/StringUtils.h"
#include <gtest/gtest.h>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include <boost/foreach.hpp>
#include <boost/algorithm/string/join.hpp>

/*******************************
 ** string utils
 *******************************/

namespace utils {


  // gtest document says the test_case_name and test_name should not contain _
  TEST(utils_test_case, trim_test) {
    std::string s = " ab cd  ";
    trim(s);
    EXPECT_EQ(s, "ab cd");
    s = "\tabcd\t   \n";
    trim(s);
    EXPECT_EQ(s, "abcd");
    s = "   \t\n";
    trim(s);
    EXPECT_TRUE(s.empty());
  }



  /**
   * spilt string based, delimiter is *space*
   * @param s [in] string to be spliteed. Not modified.
   * @return a vector of string
   */
  std::vector<std::string>
  split(const std::string &s) {
    std::istringstream iss(s);
    std::vector<std::string> tokens{
      std::istream_iterator<std::string>{iss},
        std::istream_iterator<std::string>{}
    };
    return tokens;
  }

  TEST(utils_test_case, split_test) {
    std::string s = "  ab  cd \n \t ef";
    std::vector<std::string> vs = split(s);
    EXPECT_FALSE(vs.empty());
    EXPECT_EQ(vs.size(), 3);
    EXPECT_EQ(vs[0], "ab");
    EXPECT_EQ(vs[1], "cd");
    EXPECT_EQ(vs[2], "ef");
    vs = split("hello= ", '=');
    ASSERT_EQ(vs.size(), 2);
    vs = split("hello=", '=');
    ASSERT_EQ(vs.size(), 2);
  }

  /**
   * split string based on delimeter given
   * @param[in] s string to be split
   * @param[in] delim delimeter
   * @return elems store the splits
   */
  std::vector<std::string>
  split(const std::string &s, char delim) {
    std::vector<std::string> ret;
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
      ret.push_back(item);
    }
    // in the case the last one is the delim, push an empty string.
    // This is useful for csv file
    if (!s.empty() && s.back() == delim) {
      ret.push_back("");
    }
    return ret;
  }

  TEST(UtilsTestCase, SplitTest) {
    std::vector<std::string> result;
    result = split("|Hello|World|", '|');
    ASSERT_EQ(result.size(), 4);
  }

  /**
   * Delim by ANY characters in delim string
   */
  std::vector<std::string>
  split(std::string s, std::string delim) {
    std::size_t prev = 0, pos;
    std::vector<std::string> ret;
    while ((pos = s.find_first_of(delim, prev)) != std::string::npos)
      {
        if (pos > prev)
          ret.push_back(s.substr(prev, pos-prev));
        prev = pos+1;
      }
    if (prev < s.length()) {
      ret.push_back(s.substr(prev, std::string::npos));
    }
    return ret;
  }


  /**
   * Split by the delim as a whole
   */
  std::vector<std::string>
  split_by_string(std::string s, std::string delim) {
    std::size_t prev = 0, pos;
    std::vector<std::string> ret;
    while ((pos = s.find(delim, prev)) != std::string::npos) {
      if (pos > prev)
        ret.push_back(s.substr(prev, pos-prev));
      prev = pos+delim.size();
    }
    if (prev < s.length()) {
      ret.push_back(s.substr(prev, std::string::npos));
    }
    return ret;
  }

  TEST(UtilsCase, SplitByStringTest) {
    std::vector<std::string> v = split_by_string("hello world 123 hello world", "or");
    ASSERT_EQ(v.size(), 3);
    EXPECT_EQ(v[0], "hello w");
    EXPECT_EQ(v[1], "ld 123 hello w");
    EXPECT_EQ(v[2], "ld");
  }


  /**
   * If string s ends with string pattern.
   */
  bool ends_with(const std::string &s, const std::string &pattern) {
    if (s.length() >= pattern.length()) {
      return (0 == s.compare (s.length() - pattern.length(), pattern.length(), pattern));
    } else {
      return false;
    }
  }
  /**
   * If string s starts with string pattern.
   */
  bool starts_with(const std::string &s, const std::string &pattern) {
    if (s.length() >= pattern.length()) {
      return (0 == s.compare (0, pattern.length(), pattern));
    } else {
      return false;
    }
  }

  /**
   * remove all currence of pattern from s
   */
  void remove(std::string& s, const std::string& pattern) {
    while (s.find(pattern) != std::string::npos) {
      s.erase(s.find(pattern), pattern.length());
    }
  }

  /**
   * Replace all occurrence of pattern with replacement, in the input/output string s.
   */
  void replace(std::string &s, std::string pattern, std::string replacement) {
    while (s.find(pattern) != std::string::npos) {
      s.replace(s.find(pattern), pattern.length(), replacement);
    }
  }
  std::string indent_string(std::string raw, int indent) {
    assert(indent >= 0);
    std::string indention = std::string("  ", indent);
    std::vector<std::string> lines = utils::split(raw, '\n');
    for (std::string line : lines) {
      line = indention + line;
    }
    return boost::algorithm::join(lines, "\n");
  }



  const std::set<std::string> c_common_keywords = {
    // def, head
    "define", "undef", "ifdef", "ifndef",
    "main",  "include",
    // control branch keyword
    "if", "else", "switch", "case", "default", "for", "do", "while", "break", "goto", "break", "continue",
    // type
    "bool", "true", "false"
    // storage class specifier
    "auto", "register", "static", "extern", "typedef",
    // type specifier
    "void", "char", "short", "int", "long", "float", "double", "signed", "unsigned",
    "struct", "enum",
    // type qualifier
    "const", "volatile",
    // undefined
    "sizeof", "return", "asm", "NULL"
  };

  const std::set<std::string> c_extend_keywords = {
    "stderr", "stdout", "fprintf"
  };
  /**
   * Extract id which is not c keyword
   * This is the master copy of this resolving
   * The other one calls it.
   *
   * @param code [in] input code
   * @return a set of IDs
   */
  std::set<std::string>
  extract_id_to_resolve(std::string code) {
    // shit, why i need to remove the newline? The regexp does not cross lines???
    // FIXME the code here contains my instrumentation ...
    utils::replace(code, "\n", "");
    // std::cout << "extracting code from: " << code  << "\n";
    // TODO move to trace --verbose
    // print_trace("extract_id_to_resolve");

    // Before doing the pattern matching, I want to first remove comments
    // UPDATE I don't have comment after pre-processing, removing this
    // XMLDoc *doc = XMLDocReader::CreateDocFromString(code);
    // assert(doc);
    // code = get_text_except(doc->document_element(), NK_Comment);
    // delete doc;
  
    static boost::regex id_reg("\\b[_a-zA-Z][_a-zA-Z0-9]*\\b");
    boost::smatch match;
    boost::sregex_iterator begin(code.begin(), code.end(), id_reg);
    boost::sregex_iterator end = boost::sregex_iterator();
    std::set<std::string> ss;
    for (boost::sregex_iterator it=begin;it!=end;it++) {
      std::string tmp = (*it).str();
      if (c_common_keywords.find(tmp) == c_common_keywords.end()) {
        // std::cout << tmp << "\n";
        ss.insert(tmp);
      }
    }
    return ss;
  }

  std::string lisp_pretty_print(std::string str) {
    // join line if ) is on a single line
    std::vector<std::string> lines = utils::split(str, '\n');
    std::vector<std::string> retvec;
    std::string tmp;
    for (std::string line : lines) {
      utils::trim(line);
      if (line.size() == 1 && line[0] == ')') {
        tmp += ')';
      } else if (line.empty()) {
        continue;
      } else {
        retvec.push_back(tmp);
        tmp = line;
      }
    }
    retvec.push_back(tmp);
    std::string ret;
    // indent
    int indent = 0;
    for (std::string line : retvec) {
      int open = std::count(line.begin(), line.end(), '(');
      int close = std::count(line.begin(), line.end(), ')');
      ret += std::string(indent*2, ' ') + line + "\n";
      indent = indent + open - close;
    }
    return ret;
  }  
}
