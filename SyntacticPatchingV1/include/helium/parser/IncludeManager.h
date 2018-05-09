#ifndef INCLUDEMANAGER_H
#define INCLUDEMANAGER_H

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

/**
 * manage local includes
 * 1. local includes, manage the dependence
 * 2. system includes, record what is used
 */
class IncludeManager {
public:
  IncludeManager() {}
  ~IncludeManager() {}
  void parse(fs::path benchmark);
  
  void dump(std::ostream &os);
  void load(fs::path jsonfile);

  std::vector<std::string> getSortedLocalIncludes() {
    return SortedLocalIncludes;
  }
  std::set<std::string> getSystemIncludes() {
    return SystemIncludes;
  }
private:
  std::set<std::string> LocalIncludes;
  std::set<std::string> SystemIncludes;
  std::vector<std::string> SortedLocalIncludes;
};


#endif /* INCLUDEMANAGER_H */
