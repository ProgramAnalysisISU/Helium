#ifndef DUMP_H
#define DUMP_H

#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>


/**
 * echo "benchmark, file count, func count, compile success, compile error, build rate, time(s)"
 * ALso
 * Now I want to dump the max,min,mean (leafsize, snippet size) of the successfully built code.
 */
class ExpASTDump {
public:
  static ExpASTDump *Instance() {
    if (!m_instance) {
      m_instance = new ExpASTDump();
    }
    return m_instance;
  }
  std::string dump();
  std::string GetHeader();
  /**
   * Data
   */
  std::string benchmark;
  int file_count = 0;
  int func_count = 0;
  int compile_success = 0;
  int compile_error = 0;
  double time = 0;


  std::vector<int> suc_leaf_size;
  std::vector<int> err_leaf_size;
  std::vector<int> suc_snippet_size;
  std::vector<int> err_snippet_size;
  void AppendData();


  /**
   * Now I want this kind of data

   leafsize, snippet_size, build_or_not
   12, 34, true
   15, 76, false
   */
  

private:
  ExpASTDump() {}
  static ExpASTDump *m_instance;
};

class BuildRatePlotDump {
public:
  struct Data {
    Data(int leaf, int node, int mainloc,
         int snippet, int sniloc, bool build)
      : leaf_size(leaf), node_size(node), main_loc(mainloc),
        snippet_size(snippet), snippet_loc(sniloc), build_or_not(build) {}
    int leaf_size = -1; // leaf nodes in AST
    int node_size = -1; // all nodes in AST
    int main_loc = -1; // "main" function size
    int snippet_size = -1; // number of snippets
    int snippet_loc = -1; // snippet size (not support.h)
    bool build_or_not = false;
  };

  void Push(int leaf_size, int node_size, int main_loc,
            int snippet_size, int snippet_loc, bool build_or_not) {
    struct Data data = {leaf_size, node_size, main_loc,
                        snippet_size, snippet_loc, build_or_not};
    m_data.push_back(data);
  }
  static std::string GetHeader() {
    return "leaf_size, node_size, main_loc, snippet_size, snippet_loc, build_or_not";
  }
  std::string dump() {
    std::string ret;
    ret += GetHeader() + "\n";
    for (int i=0;i<(int)m_data.size();i++) {
      ret += std::to_string(m_data[i].leaf_size) + ", "
        + std::to_string(m_data[i].node_size) + ", "
        + std::to_string(m_data[i].main_loc) + ", "
        + std::to_string(m_data[i].snippet_size) + ", "
        + std::to_string(m_data[i].snippet_loc) + ", "
        + (m_data[i].build_or_not?"true":"false") + "\n";
    }
    return ret;
  }
  static BuildRatePlotDump *Instance() {
    if (!m_instance) {
      m_instance = new BuildRatePlotDump();
    }
    return m_instance;
  }
private:
  std::vector<struct Data> m_data;
  static BuildRatePlotDump *m_instance;
};


#endif /* DUMP_H */
