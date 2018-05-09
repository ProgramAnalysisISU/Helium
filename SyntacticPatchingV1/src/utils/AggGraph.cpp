#include "helium/utils/AggGraph.h"
#include <pugixml.hpp>

#include <iostream>
#include <sstream>

using std::string;

using namespace pugi;

void AggGraph::addNode(std::string id, std::string label) {
  // deciding the type of node
  NodeType type;
  if (label.find("IfStmt") != std::string::npos) {
    type = NT_Branch;
  } else if (label.find("SwitchStmt") != std::string::npos) {
    type = NT_Branch;
  } else if (label.find("ForStmt") != std::string::npos
             || label.find("WhileStmt") != std::string::npos
             || label.find("DoStmt") != std::string::npos
             ) {
    type = NT_LoopHead;
  } else if (label.find("DeclStmt") != std::string::npos
             || label.find("ExprStmt") != std::string::npos
             || label.find("BreakStmt") != std::string::npos
             || label.find("ContinueStmt") != std::string::npos
             || label.find("ReturnStmt") != std::string::npos
             ) {
    type = NT_Stmt;
  } else {
    type = NT_Helper;
  }
  AggNode *node = new AggNode(id, label, type);
  Id2Node[id] = node;
  Nodes.insert(node);
}

void AggGraph::addEdge(std::string from, std::string to,
                       std::string label) {
  EdgeType type;
  // determine the type
  if (label.find("back") != std::string::npos) {
    type = ET_Backward;
  } else {
    type = ET_Forward;
  }
  // edge
  AggEdge *edge = new AggEdge(Id2Node[from], Id2Node[to], label, type);
  Edges.insert(edge);
}

void fill_type_node(xml_node type_node) {
  for (int i=0;i<NodeTypeName.size();i++) {
    std::string id = "NT_" + std::to_string(i);
    std::string name = NodeTypeName[i];
    xml_node node = type_node.append_child();
    node.set_name("NodeType");
    node.append_attribute("ID").set_value(id.c_str());
    node.append_attribute("name").set_value(name.c_str());
  }
  for (int i=0;i<EdgeTypeName.size();i++) {
    std::string id = "ET_" + std::to_string(i);
    std::string name = EdgeTypeName[i];
    xml_node node = type_node.append_child();
    node.set_name("EdgeType");
    node.append_attribute("ID").set_value(id.c_str());
    node.append_attribute("name").set_value(name.c_str());
  }
}

void AggGraph::fillGraphNode(xml_node graph_node) {
  // nodes
  for (AggNode *aggnode : Nodes) {
    std::string id = aggnode->id;
    std::string type = "NT_"
      + std::to_string(static_cast<int>(aggnode->type));
    xml_node node = graph_node.append_child();
    node.set_name("Node");
    node.append_attribute("ID").set_value(id.c_str());
    node.append_attribute("type").set_value(type.c_str());
    node.append_child(pugi::node_pcdata)
      .set_value(aggnode->label.c_str());
  }
  // edges
  int ID=0;
  for (AggEdge *aggedge : Edges) {
    std::string id = "E" + std::to_string(ID);
    std::string source = aggedge->from->id;
    std::string target = aggedge->to->id;
    std::string type = "ET_"
      + std::to_string(static_cast<int>(aggedge->type));
    xml_node node = graph_node.append_child();
    node.set_name("Edge");
    node.append_attribute("ID").set_value(id.c_str());
    node.append_attribute("source").set_value(source.c_str());
    node.append_attribute("target").set_value(target.c_str());
    node.append_attribute("type").set_value(type.c_str());
    node.append_child(pugi::node_pcdata).set_value(aggedge->label.c_str());
    ID++;
  }
}

std::string AggGraph::dump_grs() {
  string ret;
  for (AggNode *aggnode : Nodes) {
    string id = "s" + aggnode->id;
    string type = NodeTypeName[aggnode->type].substr(3);
    ret += "new " + id + ":" + type + "\n";
  }
  for (AggEdge *aggedge : Edges) {
    string source = "s" + aggedge->from->id;
    string target = "s" + aggedge->to->id;
    string type=EdgeTypeName[aggedge->type].substr(3);
    ret += "new " + source + "-:" + type + "->" + target + "\n";
  }
  return ret;
}

std::string AggGraph::dump() {
  pugi::xml_document doc;
  doc.load_string("<Document></Document>");
  pugi::xml_node root = doc.document_element();
  // root name should be Document

  xml_node tmpnode = root.append_child();
  tmpnode.set_name("GraphTransformationSystem");
  tmpnode.append_attribute("ID").set_value("GTS1");
  tmpnode.append_attribute("name").set_value("Class");

  
  xml_node type_node = tmpnode.append_child();
  type_node.set_name("Types");
  fill_type_node(type_node);
  xml_node graph_node = tmpnode.append_child();
  graph_node.set_name("Graph");
  graph_node.append_attribute("ID").set_value("Graph0");
  graph_node.append_attribute("name").set_value("Graph");
  fillGraphNode(graph_node);

  // dump
  // std::cout << "The AggGraph Source:" << "\n";
  // doc.save(std::cout);
  std::ostringstream oss;
  doc.save(oss);
  return oss.str();
  // std::cout << "Agg Done" << "\n";
}
