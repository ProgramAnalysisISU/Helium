#include "helium/parser/LibraryManager.h"

#include "helium/utils/StringUtils.h"

#include "helium/type/Snippet.h"

#include "helium/utils/Graph.h"

const std::set<std::string> valid_include_path = {
  "/usr/include",
  "/usr/lib/gcc/x86_64-pc-linux-gnu/6.3.1/include",
  "/usr/include/glib-2.0/",
  "/usr/lib/glib-2.0/include"
};


bool Library::find(std::string header) {
  for (const std::string &inc : m_includes) {
    if (utils::match_suffix(inc, header)) {
      fs::path tmp = utils::substract_suffix(inc, header);
      if (valid_include_path.count(tmp.string()) == 1) {
        return true;
      }
    }
  }
  return false;
}



void LibraryManager::parse(fs::path jsonfile) {
  rapidjson::Document document;
  std::ifstream ifs(jsonfile.string());
  rapidjson::IStreamWrapper isw(ifs);
  document.ParseStream(isw);
  assert(document.IsArray());

  for (rapidjson::Value &v : document.GetArray()) {
    assert(v.IsObject());
    std::string name;
    std::set<std::string> includes;
    std::set<std::string> libs;
    if (v.HasMember("package")) {
      name = v["package"].GetString();
    }
    if (v.HasMember("includes")) {
      for (rapidjson::Value &header : v["includes"].GetArray()) {
        includes.insert(header.GetString());
      }
    }
    if (v.HasMember("libs")) {
      for (rapidjson::Value &lib : v["libs"].GetArray()) {
        libs.insert(lib.GetString());
      }
    }
    m_libs.push_back(new Library(name, includes, libs));
  }
}

Library* LibraryManager::findLibraryByInclude(std::string inc) {
  for (Library *lib : m_libs) {
    if (lib->find(inc)) {
      return lib;
    }
  }
  return nullptr;
}
