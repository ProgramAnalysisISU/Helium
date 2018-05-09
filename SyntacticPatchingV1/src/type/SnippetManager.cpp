#include "helium/type/SnippetManager.h"

#include "helium/type/SnippetAction.h"
#include "helium/parser/IncludeManager.h"
#include "helium/parser/LibraryManager.h"
#include "helium/utils/StringUtils.h"




static std::vector<Snippet*>
parse_bench_for_snippets(fs::path benchmark) {
  std::vector<Snippet*> ret;
  fs::recursive_directory_iterator it(benchmark), eod;
  BOOST_FOREACH(fs::path const &p, std::make_pair(it, eod)) {
    if (p.extension() == ".c" || p.extension() == ".h") {
      std::vector<Snippet*> snippets = clang_parse_file_for_snippets(p);
      ret.insert(ret.end(), snippets.begin(), snippets.end());
    }
  }
  return ret;
}


static std::vector<Snippet*>
sort_snippets_by_location_same_file(std::vector<Snippet*> snippets) {
  std::sort(snippets.begin(), snippets.end(), [](Snippet *lhs, Snippet *rhs) {
      SourceLocation loc1 = lhs->getBeginLoc();
      SourceLocation loc2 = rhs->getBeginLoc();
      return loc1 < loc2;
    });
  return snippets;
}

static std::vector<Snippet*>
sort_snippets_by_location(std::vector<Snippet*> snippets,
                          IncludeManager *inc_manager) {
  std::vector<Snippet*> ret;

  std::vector<std::string> sorted_local_includes
    = inc_manager->getSortedLocalIncludes();
  // 1. group snippets by file
  std::map<std::string, std::vector<Snippet*> > file2snippets;
  for (Snippet *s : snippets) {
    file2snippets[s->getFile()].push_back(s);
  }
  // 2. sort
  for (std::string inc : sorted_local_includes) {
    if (file2snippets.count(inc) == 1) {
      std::vector<Snippet*> v = file2snippets[inc];
      v = sort_snippets_by_location_same_file(v);
      ret.insert(ret.end(), v.begin(), v.end());
      file2snippets.erase(inc);
    }
  }
  for (auto &m : file2snippets) {
    std::vector<Snippet*> v = m.second;
    v = sort_snippets_by_location_same_file(v);
    ret.insert(ret.end(), v.begin(), v.end());
  }
  return ret;
}

static void snippet_create_deps(std::vector<Snippet*> snippets) {
  // create key map
  std::map<std::string, std::vector<Snippet*> > key2snippets;
  for (Snippet *s : snippets) {
    for (std::string key : s->getKeys()) {
      key2snippets[key].push_back(s);
    }
  }
  // - for each, get a list of dependence key
  for (Snippet *s : snippets) {
    s->getCode();
    std::string name = s->getName();
    std::set<std::string> ids = utils::extract_id_to_resolve(s->getCode());
    // do not query its name itself
    ids.erase(name);
    for (auto &id : ids) {
      if (key2snippets.count(id) == 1) {
        s->addDep(key2snippets[id]);
      }
    }
    // remove itself
    s->removeDep(s);
  }
}

static void snippet_create_outers(std::vector<Snippet*> snippets) {
  // 1. enclosing
  // - go through all snippets, split them by files
  std::map<std::string, std::vector<Snippet*> > file2snippets;
  for (Snippet *s : snippets) {
    file2snippets[s->getFile()].push_back(s);
  }
  // - order them based on begin loc
  for (auto &m : file2snippets) {
    std::vector<Snippet*> v = m.second;
    std::sort(v.begin(), v.end(), [](Snippet *lhs, Snippet *rhs) {
        SourceLocation loc1_begin = lhs->getBeginLoc();
        SourceLocation loc1_end = lhs->getEndLoc();
        SourceLocation loc2_begin = rhs->getBeginLoc();
        SourceLocation loc2_end = rhs->getEndLoc();
        if (loc1_begin < loc2_begin) return true;
        else if (loc1_begin == loc2_begin) return loc1_end > loc2_end;
        else return false;
      });
    // - for each, go through and find until the begin loc of the next is larget then its end, record that
    for (int i=0;i<v.size();i++) {
      for (int j=i+1;j<v.size();j++) {
        Snippet *s1 = v[i];
        Snippet *s2 = v[j];
        SourceLocation loc1_begin = s1->getBeginLoc();
        SourceLocation loc1_end = s1->getEndLoc();
        SourceLocation loc2_begin = s2->getBeginLoc();
        SourceLocation loc2_end = s2->getEndLoc();
        if (loc2_begin < loc1_end) {
          // Outers[s2].insert(s1);
          s2->addOuter(s1);
        }
      }
    }
  }
}

void SnippetManager::parse(fs::path benchmark, IncludeManager *inc_manager) {
  // 1. get all snippets
  std::vector<Snippet*> snippets = parse_bench_for_snippets(benchmark);
  // 2. sort them based on location in files
  snippets = sort_snippets_by_location(snippets, inc_manager);
  // 3. assign IDs
  for (int i=0;i<snippets.size();i++) {
    snippets[i]->setId(i);
  }
  // 4. create dependence
  snippet_create_deps(snippets);
  // 5. create outers
  snippet_create_outers(snippets);

  m_key2snippets.clear();
  for (Snippet *s : snippets) {
    for (std::string key : s->getKeys()) {
      m_key2snippets[key].push_back(s);
    }
  }
  m_snippets = snippets;
}

void SnippetManager::dump(std::ofstream &os) {
  rapidjson::Document document;
  rapidjson::Document::AllocatorType &allocator = document.GetAllocator();
  document.SetArray();
  assert(document.IsArray());
  for (Snippet *s : m_snippets) {
    rapidjson::Value obj = s->saveJson(allocator);
    document.PushBack(obj, allocator);
  }

  rapidjson::StringBuffer sb;
  rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb);
  document.Accept(writer);
  os << sb.GetString() << "\n";
}

void SnippetManager::load(fs::path jsonfile) {
  m_jsonfile = fs::canonical(jsonfile);
  rapidjson::Document document;
  std::ifstream ifs(jsonfile.string());
  rapidjson::IStreamWrapper isw(ifs);
  document.ParseStream(isw);
  assert(document.IsArray());
  // load snippets
  for (auto &v : document.GetArray()) {
    Snippet *s = nullptr;
    std::string kind = v["kind"].GetString();
    if (kind == "FunctionSnippet") s = new FunctionSnippet();
    else if (kind == "VarSnippet") s = new VarSnippet();
    else if (kind == "TypedefSnippet") s = new TypedefSnippet();
    else if (kind == "RecordSnippet") s = new RecordSnippet();
    else if (kind == "EnumSnippet") s = new EnumSnippet();
    else if (kind == "FunctionDeclSnippet") s = new FunctionDeclSnippet();
    else if (kind == "RecordDeclSnippet") s = new RecordDeclSnippet();
    else if (kind == "MacroSnippet") s = new MacroSnippet();
    else {assert(false);}

    s->loadJson(v);
    m_snippets.push_back(s);
  }
  // load deps and outers
  for (auto &v : document.GetArray()) {
    int id = v["id"].GetInt();
    for (auto &dep : v["deps"].GetArray()) {
      int depId = dep.GetInt();
      m_snippets[id]->addDep(m_snippets[depId]);
    }
    for (auto &outer : v["outers"].GetArray()) {
      int outerId = outer.GetInt();
      m_snippets[id]->addOuter(m_snippets[outerId]);
    }
  }
  // key map
  for (Snippet *s : m_snippets) {
    for (std::string key : s->getKeys()) {
      m_key2snippets[key].push_back(s);
    }
  }
}

std::vector<Snippet*> SnippetManager::sort(std::set<Snippet*> snippets) {
  std::vector<Snippet*> ret;
  for (Snippet *s : m_snippets) {
    if (snippets.count(s) == 1) ret.push_back(s);
  }
  return ret;
}

bool SnippetManager::checkValid(std::string &reason) {
  for (auto &m : m_key2snippets) {
    std::string key = m.first;
    std::vector<Snippet*> snippets = m.second;
    // if two functions, report
    bool func=false;
    for (Snippet *s : snippets) {
      if (dynamic_cast<FunctionSnippet*>(s)) {
        if (func) {
          reason = "function " + key + " is declared multiple times.";
          return false;
        } else {
          func = true;
        }
      }
    }
  }
  return true;
}

std::set<Snippet*> SnippetManager::getAllDeps(std::set<Snippet*> snippets) {
  std::set<Snippet*> deps;
  for (auto *s : snippets) {
    std::set<Snippet*> dep = s->getAllDeps();
    deps.insert(dep.begin(), dep.end());
  }
  snippets.insert(deps.begin(), deps.end());
  return snippets;
}

std::set<Snippet*> SnippetManager::replaceNonOuters(std::set<Snippet*> ss) {
  std::set<Snippet*> ret;
  std::vector<Snippet*> worklist(ss.begin(), ss.end());
  std::set<Snippet*> done;
  while (!worklist.empty()) {
    Snippet *s = worklist.back();
    worklist.pop_back();
    done.insert(s);
    std::set<Snippet*> outers = s->getOuters();
    if (outers.empty()) ret.insert(s);
    for (Snippet *o : outers) {
      if (done.count(o) == 0) worklist.push_back(o);
    }
  }
  return ret;
}
