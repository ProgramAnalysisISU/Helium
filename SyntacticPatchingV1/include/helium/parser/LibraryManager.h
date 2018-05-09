#ifndef LIBRARYMANAGER_H
#define LIBRARYMANAGER_H

#include <vector>
#include "helium/parser/AST.h"

#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

#include <gtest/gtest.h>

#include <rapidjson/istreamwrapper.h>
#include <rapidjson/document.h>
#include <rapidjson/prettywriter.h>

namespace fs = boost::filesystem;

#include <regex>



class Library {
public:
  Library(std::string name, std::set<std::string> includes, std::set<std::string> libs)
    : m_name(name), m_includes(includes) {
    for (std::string inc : includes) {
      if (!fs::exists(inc)) m_exist=false;
    }
    // for now, only consider /usr/lib/libxxx.so
    std::regex libreg("^/usr/lib/lib(.*)\\.so$");
    for (std::string lib : libs) {
      std::smatch match;
      if (std::regex_match(lib, match, libreg)) {
        std::string libname = match[1].str();
        m_libs.insert("-l" + libname);
      }
    }
  }
  ~Library() {}
  bool find(std::string inc);
  bool exists() {return m_exist;}
  std::string getFlags() {
    std::string ret;
    for (std::string flag : m_libs) {
      ret += flag + " ";
    }
    return ret;
  }
private:
  std::string m_name;
  std::set<std::string> m_includes;
  std::set<std::string> m_libs;
  bool m_exist = true;
};

/**
 * Manage the system available library
 */
class LibraryManager {
public:
  LibraryManager() {}
  ~LibraryManager() {}
  void parse(fs::path jsonfile);
 Library* findLibraryByInclude(std::string inc);
private:
  std::vector<Library*> m_libs;
};


#endif /* LIBRARYMANAGER_H */
