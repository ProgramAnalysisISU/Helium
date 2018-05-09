#ifndef SNIPPETMANAGER_H
#define SNIPPETMANAGER_H

#include "helium/type/Snippet.h"
#include "helium/parser/IncludeManager.h"
#include "helium/parser/LibraryManager.h"

/**
 * The snippet manager should maintain:
 * - dependence
 * - relation
 *   - same type
 *   - function def, decl
 *   - call graph
 */
class SnippetManager {
public:
  // FIXME Instance()??? make the constructor private
  SnippetManager() {}
  ~SnippetManager() {}

  // TODO NOW
  void parse(fs::path benchmark, IncludeManager *inc_manager);
  void load(fs::path jsonfile);
  void dump(std::ofstream &os);

  std::vector<Snippet*> getAll(std::string key) {
    if (m_key2snippets.count(key)==1) return m_key2snippets[key];
    return {};
  }
  std::vector<Snippet*> getAll(std::string key, std::string kind) {
    std::vector<Snippet*> ret;
    for (Snippet *s : m_key2snippets[key]) {
      if (s->getSnippetName() == kind) ret.push_back(s);
    }
    return ret;
  }
  Snippet* getone(std::string key, std::string kind) {
    for (Snippet *s : m_key2snippets[key]) {
      if (s->getSnippetName() == kind) return s;
    }
    return nullptr;
  }
  /**
   * Recursively get all deps
   */
  std::set<Snippet*> getAllWithDep(Snippet *s);
  /**
   * if there's a outer of any of them, use that outer
   */
  std::set<Snippet*> replaceNonOuters(std::set<Snippet*> ss);
  std::vector<Snippet*> sort(std::set<Snippet*> snippets);

  std::set<Snippet*> getAllDeps(std::set<Snippet*> snippets);

  bool checkValid(std::string &reason);
  
  int size() {return m_snippets.size();}

  fs::path getJsonFile() {return m_jsonfile;}
private:
  // sorted
  std::vector<Snippet*> m_snippets;
  std::map<std::string, std::vector<Snippet*> > m_key2snippets;
  fs::path m_jsonfile;
};


#endif /* SNIPPETMANAGER_H */
