#include "helium/utils/Dot.h"
#include <gtest/gtest.h>
#include "helium/utils/Utils.h"
#include "helium/utils/StringUtils.h"
#include "helium/utils/FSUtils.h"

#include "helium/utils/ThreadUtils.h"

static std::string dot_id_filter(std::string s) {
  utils::replace(s, "\"", "");
  return "\"" + s + "\"";
}
TEST(DotGraphTestCase, DotGraphTest) {
  DotGraph dotgraph;
  dotgraph.AddNode("id_1", "label 1");
  dotgraph.AddNode("id_2", "label 2");
  dotgraph.AddEdge("id_1", "id_2");
  std::string dot = dotgraph.dump();
  // std::cout << dot  << "\n";
}


std::string DotEdge::GetCode() {
  std::string ret;
  if (m_from && m_to) {
    ret += m_from->GetID() + " -> " + m_to->GetID();
    if (!m_label.empty()) {
      ret += "[label=" + dot_id_filter(m_label) + "]";
    }
    ret += ";\n";
  }
  return ret;
}


std::string DotNode::GetCode() {
  std::string ret;
  std::string prefix,suffix;
  if (!m_text.empty()) {
    std::string cluster = dot_id_filter("cluster" + m_id);
    prefix += "subgraph " + cluster + " {\n";
    prefix += "label=" + dot_id_filter(m_text) + ";\n";
    suffix = "}\n";
  }
  ret += prefix;
  ret += m_id + "[label=" + dot_id_filter(m_label);
  switch (m_color) {
  case DNCK_Yellow: ret += ", style=\"filled,dotted\", fillcolor=greenyellow"; break;
  case DNCK_Cyan:   ret += ", style=\"filled,dotted\", fillcolor=cyan"; break;
  default: break;
  }
  ret += + "];\n";
  ret += suffix;
  return ret;
}


void DotGraph::AddNode(std::string id, std::string label) {
  if (m_nodes.count(id) == 0) {
    DotNode *node = new DotNode(id, label);
    m_nodes[id] = node;
  }
}
void DotGraph::AddEdge(std::string id_from, std::string id_to, std::string label) {
  if (m_nodes.count(id_from) == 1 && m_nodes.count(id_to) == 1) {
    DotNode *from = m_nodes[id_from];
    DotNode *to = m_nodes[id_to];
    m_edges.push_back(new DotEdge(from, to, label));
  }
}
void DotGraph::AddText(std::string id, std::string text) {
  if (m_nodes.count(id) == 1) {
    m_nodes[id]->AddText(text);
  } else {
    std::cout << "[WW] Dot graph No node found: " << id  << "\n";
  }
}
std::string DotGraph::dump() {
  std::string ret;
  ret += "digraph {\n";
  for (auto m : m_nodes) {
    DotNode* node = m.second;
    ret += node->GetCode();
  }
  for (DotEdge *edge : m_edges) {
    ret += edge->GetCode();
  }
  ret += "}\n";
  return ret;
}




/**
 * Visualize dot graph, create a tmp dir to hold the filename
 * @return the dot file generated
 */
std::string visualize_dot_graph(const std::string& dot, bool open, std::string filename) {
  std::string dir = utils::create_tmp_dir();
  std::string png_filename = dir + "/" + filename + ".png";
  filename =  dir + "/" + filename + ".dot";
  std::string png_convert_cmd = "dot -Tpng -o " + png_filename + " " + filename;
  utils::write_file(filename, dot);

  ThreadExecutor(png_convert_cmd).run();
  if (open) {
#ifdef __MACH__
    std::string display_cmd = "open " + filename;
#else
    std::string display_cmd = "feh -F --zoom 100 "+ png_filename;
#endif
    ThreadExecutor(display_cmd).run();
  }
  return png_filename;
}


void dot2png(fs::path dotfile, fs::path pngfile) {
  std::string png_convert_cmd = "dot -Tpng -o " + pngfile.string() + " " + dotfile.string();
  ThreadExecutor(png_convert_cmd).run();
}
