#ifndef AGGGRAPH_H
#define AGGGRAPH_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include <pugixml.hpp>

enum NodeType {
  NT_Branch,
  NT_LoopHead,
  NT_Stmt,
  NT_Helper
};

static std::vector<std::string> NodeTypeName = {
  "NT_Branch", "NT_LoopHead", "NT_Stmt", "NT_Helper"
};

enum EdgeType {
  ET_Forward,
  ET_Backward
};

static std::vector<std::string> EdgeTypeName = {
  "ET_Forward", "ET_Backward"
};

class AggNode {
public:
  AggNode(std::string id, std::string label, NodeType type)
    : id(id), label(label), type(type) {}
  ~AggNode() {}
// private:
  std::string id;
  std::string label;
  NodeType type;
};
class AggEdge {
public:
  AggEdge(AggNode *from, AggNode *to, std::string label, EdgeType type)
    : from(from), to(to), label(label), type(type){}
  ~AggEdge() {}
// private:
  AggNode *from = nullptr;
  AggNode *to = nullptr;
  std::string label;
  EdgeType type;
};

class AggGraph {
public:
  AggGraph() {}
  ~AggGraph() {}

  void addNode(std::string id, std::string label);
  void addEdge(std::string from, std::string to, std::string label="");
  void fillGraphNode(pugi::xml_node graph_node);
  std::string dump();
  std::string dump_grs();
private:
  std::map<std::string, AggNode*> Id2Node;
  std::set<AggNode*> Nodes;
  std::set<AggEdge*> Edges;
};

#endif /* AGGGRAPH_H */
