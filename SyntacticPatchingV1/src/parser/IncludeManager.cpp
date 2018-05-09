#include "helium/parser/IncludeManager.h"

#include "helium/utils/StringUtils.h"

#include "helium/type/Snippet.h"

#include "helium/utils/Graph.h"


// FIXME adding - for filename, fix this for other regular expressions
static std::regex include_reg("#\\s*include\\s*[\"<]([\\w/-]+\\.h)[\">]");
static std::regex include_quote_reg("#\\s*include\\s*\"([\\w/-]+\\.h)\"");
static std::regex include_angle_reg("#\\s*include\\s*<([\\w/-]+\\.h)>");

static bool has_suffix(const std::string &str, const std::string &suffix) {
    return str.size() >= suffix.size() &&
           str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
}

static std::set<std::string> filter_suffix(std::set<std::string> &all, std::string sub) {
  std::set<std::string> ret;
  std::string str,suffix;
  for (std::string s : all) {
    // FIXME suffix is not enough. it should be path suffix
    // if (has_suffix(s, sub)) {
    //   ret.insert(s);
    // }
    // I'm using path
    if (utils::match_suffix(s, sub)) {
      ret.insert(s);
    }
  }
  return ret;
}

static std::set<std::string> get_all_files(fs::path benchmark) {
  std::set<std::string> ret;
  fs::recursive_directory_iterator it(benchmark), eod;
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (p.extension() == ".h" || p.extension() == ".c") {
      ret.insert(p.string());
    }
  }
  return ret;
}


static std::vector<std::string>
topo_sort(std::map<std::string, std::set<std::string> > deps) {
  hebigraph::Graph<std::string> graph;
  // FIXME if a file does not depend on others, it should also be added to the graph
  for (auto &m : deps) {
    graph.addNode(m.first);
  }
  for (auto &m : deps) {
    std::string from = m.first;
    if (!graph.hasNode(from)) graph.addNode(from);
    for (std::string to : m.second) {
      // arrow means "used by", while FileDep means "depend on",
      // they are opposite, reflected here
      if (!graph.hasNode(to)) graph.addNode(to);
      // std::cout << "adding edge " << to << " => " << from << "\n";
      graph.addEdge(to, from);
    }
  }
  return graph.topoSort();
}


void IncludeManager::parse(fs::path benchmark) {
  fs::recursive_directory_iterator it(benchmark), eod;
  std::set<std::string> all_files = get_all_files(benchmark);


  std::map<std::string, std::set<std::string> > deps;
  
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (p.extension() == ".h" || p.extension() == ".c") {
      std::ifstream ifs(p.string());
      assert(ifs.is_open());
      std::string line;
      while (std::getline(ifs, line)) {
        std::smatch match;
        // quotes "a.h"
        if (std::regex_search(line, match, include_quote_reg)) {
          std::string file = match[1];
          LocalIncludes.insert(file);
          std::set<std::string> matches = filter_suffix(all_files, file);
          // Now the to and p are both absolute path
          // I want to remove the benchmark prefix
          for (std::string m : matches) {
            if (p.string() != m) {
              fs::path from = fs::relative(p, benchmark);
              fs::path to = fs::relative(fs::path(m), benchmark);
              deps[from.string()].insert(to.string());
            }
          }
        }
        // angle <a.h>
        if (std::regex_search(line, match, include_angle_reg)) {
          std::string file = match[1];
          SystemIncludes.insert(file);
        }
      }
    }
  }
  SortedLocalIncludes = topo_sort(deps);
}


/**
 *
 {
 "LocalIncludes": ["a.h", "b.h", "c.h"],
 "SystemIncludes": ["stdio.h", "stdlib.h"],
 "SortedLocalIncludes": ["c.h", "a.h"]
 }
 */
void IncludeManager::dump(std::ostream &os) {
  rapidjson::Document doc;
  rapidjson::Document::AllocatorType &allocator = doc.GetAllocator();
  doc.SetObject();
  rapidjson::Value local(rapidjson::kArrayType);
  for (std::string inc : LocalIncludes) {
    rapidjson::Value str;
    str.SetString(inc.c_str(), allocator);
    local.PushBack(str, allocator);
  }
  doc.AddMember("LocalIncludes", local, allocator);
  rapidjson::Value system(rapidjson::kArrayType);
  for (std::string inc : SystemIncludes) {
    rapidjson::Value str;
    str.SetString(inc.c_str(), allocator);
    system.PushBack(str, allocator);
  }
  doc.AddMember("SystemIncludes", system, allocator);
  rapidjson::Value sorted_local(rapidjson::kArrayType);
  for (std::string inc : SortedLocalIncludes) {
    rapidjson::Value str;
    str.SetString(inc.c_str(), allocator);
    sorted_local.PushBack(str, allocator);
  }
  doc.AddMember("SortedLocalIncludes", sorted_local, allocator);

  rapidjson::StringBuffer sb;
  rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb);
  doc.Accept(writer);
  os << sb.GetString() << "\n";
}
void IncludeManager::load(fs::path jsonfile) {
  rapidjson::Document doc;
  std::ifstream ifs (jsonfile.string());
  rapidjson::IStreamWrapper isw(ifs);
  doc.ParseStream(isw);
  assert(doc.IsObject());

  LocalIncludes.clear();
  SystemIncludes.clear();
  SortedLocalIncludes.clear();
  for (rapidjson::Value &field : doc["LocalIncludes"].GetArray()) {
    LocalIncludes.insert(field.GetString());
  }
  for (rapidjson::Value &field : doc["SystemIncludes"].GetArray()) {
    SystemIncludes.insert(field.GetString());
  }
  for (rapidjson::Value &field : doc["SortedLocalIncludes"].GetArray()) {
    SortedLocalIncludes.push_back(field.GetString());
  }
}

