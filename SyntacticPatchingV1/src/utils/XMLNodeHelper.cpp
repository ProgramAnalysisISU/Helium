#include "helium/utils/XMLNodeHelper.h"
#include "helium/utils/XMLNode.h"
#include <deque>

#include "helium/utils/Utils.h"
#include "helium/utils/FSUtils.h"
#include "helium/utils/XMLDocReader.h"

#include <gtest/gtest.h>

/*******************************
 ** srcml specific
 *******************************/

std::string get_filename(XMLNode node) {
  XMLNode unit_node = node.select_node("//unit").node();
  return unit_node.attribute("filename").value();
}
std::string get_filename(XMLDoc &doc) {
  XMLNode unit_node = doc.select_node("//unit").node();
  return unit_node.attribute("filename").value();
}

/**
 * disable because the output is "tmp/heli..." from srcml
 */
TEST(ast_test_case, DISABLED_srcml_test) {
  std::string dir = utils::create_tmp_dir("/tmp/helium-test.XXXXXX");
  utils::write_file(dir+"/a.c", "int a;");
  XMLDoc *doc = XMLDocReader::CreateDocFromFile(dir + "a.c");
  
  std::string filename = get_filename(*doc);
  EXPECT_EQ(filename, dir+"/a.c");

  doc = XMLDocReader::CreateDocFromFile(dir + "/a.c");

  // will still have double "//"
  filename = get_filename(doc->document_element());
  EXPECT_EQ(filename, dir+"/a.c");
}





/*******************************
 ** traversal
 *******************************/

/**
 * Next sibling on AST level
 */
XMLNode next_sibling(XMLNode node) {
  XMLNode n = node;
  while ((n = n.next_sibling())) {
    if (is_valid_ast(n)) return n;
  }
  return XMLNode();
}
XMLNode previous_sibling(XMLNode node) {
  XMLNode n =node;
  while ((n = n.previous_sibling())) {
    if (is_valid_ast(n)) return n;
  }
  return XMLNode();
}
XMLNode parent(XMLNode node) {
  XMLNode n = node;
  while ((n = n.parent())) {
    if (is_valid_ast(n)) return n;
  }
  return XMLNode();
}

/**
 * valid ast includes: expr, decl, break, macro, for, while, if, function
 */

bool is_valid_ast(const char* name) {
  if (strcmp(name, "expr_stmt") == 0
      || strcmp(name, "decl_stmt") == 0
      || strcmp(name, "break") == 0
      || strcmp(name, "macro") == 0
      || strcmp(name, "for") == 0
      || strcmp(name, "while") == 0
      || strcmp(name, "if") == 0
      || strcmp(name, "function") == 0
      ) return true;
  else return false;
}


bool is_valid_ast(XMLNode node) {
  // return is_valid_ast(node.name());
  if (node.type() == pugi::node_element) return true;
  else return false;
}


/*******************************
 ** helium specific ast
 ** using helium_valid_ast
 *******************************/
bool helium_is_valid_ast(XMLNode n) {
  if (helium_valid_ast.find(xmlnode_to_kind(n)) != helium_valid_ast.end()) return true;
  else return false;
}
XMLNode helium_next_sibling(XMLNode node) {
  XMLNode n = node;
  while ((n = n.next_sibling())) {
    // case is special. It is not parent of its block. It's sibling of the block.
    if (xmlnode_to_kind(n) == NK_Case) return XMLNode();
    if (helium_is_valid_ast(n)) return n;
  }
  return XMLNode();
}
XMLNode helium_previous_sibling(XMLNode node) {
  XMLNode n = node;
  while ((n = n.previous_sibling())) {
    if (xmlnode_to_kind(n) == NK_Case) return XMLNode();
    if (helium_is_valid_ast(n)) return n;
  }
  return XMLNode();
}
XMLNode helium_parent(XMLNode node) {
  XMLNode n = node;
  while ((n = n.parent())) {
    if (helium_is_valid_ast(n)) {
      // block directly under switch is not valid, because case: is invalid outside switch.
      if (xmlnode_to_kind(n) == NK_Block) {
        if (xmlnode_to_kind(n.parent()) == NK_Switch) {
          continue;
        }
      }
      return n;
    }
  }
  return XMLNode();
}


/**
 * least upper bound of two nodes
 */
pugi::xml_node
lub(pugi::xml_node n1, pugi::xml_node n2) {
  if (n1.root() != n2.root()) return pugi::xml_node();
  pugi::xml_node root = n1.root();
  int num1=0, num2=0;
  pugi::xml_node n;
  n = n1;
  while (n!=root) {
    n = n.parent();
    num1++;
  }
  n = n2;
  while(n!=root) {
    n = n.parent();
    num2++;
  }
  if (num1 > num2) {
    // list 1 is longer
    while(num1-- != num2) {
      n1 = n1.parent();
    }
  } else {
    while(num2-- != num1) {
      n2 = n2.parent();
    }
  }
  // will end because the root is the same
  while (n1 != n2) {
    n1 = n1.parent();
    n2 = n2.parent();
  }
  return n1;
}



/**
 * Check if node is a sub node of any one of parent_nodes
 */
bool contains(XMLNodeList parent_nodes, XMLNode node) {
  for (XMLNode parent : parent_nodes) {
    if (contains(parent, node)) return true;
  }
  return false;
}
/**
 * Check if child is a sub node of parent
 */
bool contains(XMLNode parent, XMLNode child) {
  if (lub(parent, child) == parent) return true;
  else return false;
}

/*******************************
 ** get_text family
 *******************************/
  
std::string get_text(XMLNode node) {
  std::string text;
  if (!node) return "";
  for (pugi::xml_node n : node.children()) {
    if (n.type() == pugi::node_element) {
      if (!node.attribute("helium-omit")) {
        // add text only if it is not in helium-omit
        text += get_text(n);
      } else if (strcmp(node.name(), "else") == 0 ||
                 strcmp(node.name(), "then") == 0 ||
                 strcmp(node.name(), "elseif") == 0 ||
                 strcmp(node.name(), "default") == 0
                 ) {
        // FIXME Why??????
        // For simplification of code.
        // I will add "helium-omit" attribute on the AST to mark deletion.
        // Those tag will be deleted.
        // But to make the syntax valid, I need to add some "{}"
        text += "{}";
      } else if (strcmp(node.name(), "case") == 0) {
        // FIXME why??????
        text += "{break;}";
      }
    } else {
      text += n.value();
    }
  }
  return text;
}

/**
 * get test of a list of nodes.
 * These nodes should be in the same level, i.e. nodes are siblings
 * Should get the pcdata after each node.
 * The nodes should be contineous? Not necessary.
 * FIXME deprecated! very buggy!
 */
std::string get_text(XMLNodeList nodes) {
  std::string result;
  for (XMLNode n : nodes) {
    result += get_text(n);
    XMLNode _n = n.next_sibling();
    while (_n.type() == pugi::node_pcdata) {
      result += _n.value();
      _n = _n.next_sibling();
    }
  }
  // but should not have the last, because that might be the end of function, }
  // if (nodes.empty()) return "";
  // if (nodes.size() == 1) return get_text(nodes[0]);
  // XMLNode node = nodes[0];
  // XMLNode node_end = *(nodes.end()-1);
  // XMLNode n;
  // for (n=node;n!=node_end;n=n.next_sibling()) {
  //   assert(n && "n must exist, or the nodes are not in the same level");
  //   if (n.type() == pugi::node_element) {
  //     result += get_text(n);
  //   } else if (n.type() == pugi::node_pcdata) {
  //     result += n.value();
  //   } else {
  //     assert(false);
  //   }
  // }
  return result;
}

/**
 * Add line break between every node.
 */
std::string get_text_ln(XMLNodeList nodes) {
  std::string result;
  for (XMLNode n : nodes) {
    result += get_text(n);
    result += '\n';
  }
  return result;
}
// std::string get_text_except(XMLNode node, std::string tag) {
//   if (!node) return "";
//   std::string text;
//   for (pugi::xml_node n : node.children()) {
//     if (n.type() == pugi::node_element) {
//       if (!node.attribute("helium-omit")) {
//         if (strcmp(n.name(), tag.c_str()) != 0) {
//           text += get_text_except(n, tag);
//         }
//       }
//       // TODO this version does not use the trick for simplification,
//       // so it doesnot work with simplification
//     } else {
//       text += n.value();
//     }
//   }
//   return text;
// }
std::string get_text_except(XMLNode node, XMLNodeKind k) {
  return get_text_except(node, std::vector<XMLNodeKind>(k));
}
std::string get_text_except(XMLNode node, std::vector<XMLNodeKind> kinds) {
  if (!node) return "";
  std::string result;
  for (XMLNode n : node.children()) {
    if (n.type() == pugi::node_element) {
      for (XMLNodeKind k : kinds) {
        if (xmlnode_to_kind(n) == k) {
          continue;
        }
      }
      result += get_text_except(n, kinds);
    } else {
      result += n.value();
    }
  }
  return result;
}
std::string get_text_except(XMLNodeList nodes, XMLNodeKind k) {
  std::string result;
  for (XMLNode n : nodes) {
    result += get_text_except(n, k);
  }
  return result;
}
std::string get_text_except(XMLNodeList nodes, std::vector<XMLNodeKind> kinds) {
  std::string result;
  for (XMLNode n : nodes) {
    result += get_text_except(n, kinds);
  }
  return result;
}


/**
 * True if node is inside a node of kind "kind"
 */
bool in_node(XMLNode node, XMLNodeKind kind) {
  while ((node = parent(node))) {
    if (xmlnode_to_kind(node) == kind) return true;
  }
  return false;
}

/*******************************
 ** --position related
 *******************************/

/*
 * use depth-first-search for the first pos:line attribute
 * return -1 if no pos:line attr found
 */
int
get_node_line(pugi::xml_node node) {
  assert(node);
  // check if pos:line is enabled on this xml
  assert(node.root().child("unit").attribute("xmlns:pos"));
  // the node itself has pos:line attr, just use it
  if (node.attribute("pos:line")) {
    return atoi(node.attribute("pos:line").value());
  } else {
    pugi::xml_node n = node.select_node(".//*[@pos:line]").node();
    if (n) {
      return atoi(n.attribute("pos:line").value());
    }
  }
  return -1;
}



/*
 * The last pos:line in the current node element
 * Useful to track the range of the current AST node.
 */
int
get_node_last_line(pugi::xml_node node) {
  assert(node);
  assert(node.root().child("unit").attribute("xmlns:pos"));
  pugi::xml_node n = node.select_node("(.//*[@pos:line])[last()]").node();
  
  // pugi::xpath_node_set nodes = node.select_nodes("//*[@pos:line]");
  // pugi::xml_node last_node = nodes[nodes.size()-1].node();
  // return atoi(last_node.attribute("pos:line").value());
  
  if (n) {
    return atoi(n.attribute("pos:line").value());
  } else if (node.attribute("pos:line")) {
    return atoi(node.attribute("pos:line").value());
  }
  return -1;
}

std::pair<int, int> get_node_position(pugi::xml_node node) {
  std::string line_str = node.attribute("pos:line").value();
  std::string column_str = node.attribute("pos:column").value();
  if (line_str.empty() || column_str.empty()) {
    return std::make_pair(-1,-1);
  }
  return std::make_pair(stoi(line_str), stoi(column_str));
}

/**
 * find the first pos:line
 */
std::pair<int, int> get_node_begin_position(pugi::xml_node node) {
  assert(node);
  assert(node.root().child("unit").attribute("xmlns:pos"));
  int line = -1;
  int column = -1;
  if (node.attribute("pos:line")) {
    // return atoi(node.attribute("pos:line").value());
    line = atoi(node.attribute("pos:line").value());
    column = atoi(node.attribute("pos:column").value());
  } else {
    pugi::xml_node n = node.select_node(".//*[@pos:line]").node();
    if (n) {
      // return atoi(n.attribute("pos:line").value());
      line = atoi(n.attribute("pos:line").value());
      column = atoi(n.attribute("pos:column").value());
    }
  }
  return std::make_pair(line, column);
}

/**
 * find the last pos:line
 * TODO should I ge the beginning of next? will that be more accurate?
 */
std::pair<int, int> get_node_end_position(pugi::xml_node node) {
  assert(node);
  assert(node.root().child("unit").attribute("xmlns:pos"));
  pugi::xml_node n = node.select_node("(.//*[@pos:line])[last()]").node();
  int line = -1;
  int column = -1;
  if (n) {
    line = atoi(n.attribute("pos:line").value());
    column = atoi(n.attribute("pos:column").value());
  } else if (node.attribute("pos:line")) {
    line = atoi(node.attribute("pos:line").value());
    column = atoi(node.attribute("pos:column").value());
  }
  return std::make_pair(line, column);
}


/**
 * Disabled because absolute path
 */
TEST(ASTHelperTestCase, DISABLED_GetXMLNodeLine) {
  XMLDoc *doc = nullptr;
  const char *raw = R"prefix(
int a;
int a=0;
int a,b;
int a=0,b;
int a,b=0;
int a=0,b=1;
int a=0,b,c=3;
// 8
u_char *eom, *cp, *cp1, *rdatap;
// 9
int a[3][8];
)prefix";

  doc = XMLDocReader::CreateDocFromString(raw);
  XMLNode root = doc->document_element();
  // root's name is <unit>
  // std::cout << root.name()  << "\n";
  // for (XMLXMLNode node : root.children()) {
  //   std::cout << node.name()  << "\n";
  // }
  // also note the attribute have only ONE colon
  // std::cout << root.root().name()  << "\n";
  // for (XMLXMLNode node : root.root().children()) {
  //   std::cout << node.name()  << "\n";
  // }
  ASSERT_TRUE(root.root().child("unit").attribute("xmlns:pos"));
  for (XMLNode node : root.children()) {
    get_node_line(node);
    get_node_last_line(node);
  }

  std::string filename = "/Users/hebi/github/Helium/utils/mlslice/benchmark/gzip-1.7/lib/asnprintf.c";
  XMLDoc *docp = XMLDocReader::CreateDocFromFile(filename);
  root = docp->document_element();
  ASSERT_TRUE(root.root().child("unit").attribute("xmlns:pos"));
  get_node_line(root);
  get_node_last_line(root);
}


/*******************************
 ** Find nodes
 *******************************/


// find the outmost nodes
// this doesnot use the select_nodes API
// instead, it manually go through the structure in breadth first search
XMLNode find_first_node_bfs(XMLNode node, std::string tag) {
  std::deque<XMLNode> worklist;
  if (tag == node.name()) return node;
  worklist.push_back(node);
  while (!worklist.empty()) {
    node = worklist.front();
    worklist.pop_front();
    for (XMLNode child : node.children()) {
      worklist.push_back(child);
      if (tag == child.name()) {
        return child;
      }
    }
  }
  return XMLNode();
}
/**
 * Find the children of node of kind "kind".
 * It doesn't need to be direct child.
 */
XMLNodeList find_nodes(XMLNode node, XMLNodeKind kind) {
  XMLNodeList result;
  std::string tag = ".//";
  tag += xmlnode_kind_to_name(kind);
  pugi::xpath_node_set nodes = node.select_nodes(tag.c_str());
  for (auto it=nodes.begin();it!=nodes.end();++it) {
    result.push_back(it->node());
  }
  return result;
}
XMLNodeList find_nodes(const XMLDoc& doc, XMLNodeKind kind) {
  XMLNode root = doc.document_element();
  return find_nodes(root, kind);
}
XMLNodeList find_nodes_from_root(XMLNode node, XMLNodeKind kind) {
  XMLNode root = node.root();
  return find_nodes(root, kind);
}

/**
 * find nodes based on a list of kinds.
 */
XMLNodeList find_nodes(XMLNode node, std::vector<XMLNodeKind> kinds) {
  XMLNodeList result;
  for (XMLNodeKind k : kinds) {
    XMLNodeList tmp = find_nodes(node, k);
    result.insert(result.end(), tmp.begin(), tmp.end());
  }
  return result;
}
XMLNodeList find_nodes(const XMLDoc& doc, std::vector<XMLNodeKind> kinds) {
  XMLNode root = doc.document_element();
  return find_nodes(root, kinds);
}
XMLNodeList find_nodes_from_root(XMLNode node, std::vector<XMLNodeKind> kinds) {
  XMLNode root = node.root();
  return find_nodes(root, kinds);
}


/*******************************
 ** based on line
 *******************************/

/**
 * find node based on kind, but also *on* the line.
 */
XMLNode find_node_on_line(XMLNode node, XMLNodeKind k, int line_number) {
  XMLNodeList nodes = find_nodes(node, k);
  for (XMLNode n : nodes) {
    if (get_node_line(n) == line_number) {
      return n;
    }
  }
  return XMLNode();
}

/**
 * find nodes whose kind is one of kinds, on that line.
 */
XMLNode find_node_on_line(XMLNode node, std::vector<XMLNodeKind> kinds, int line_number) {
  XMLNodeList nodes = find_nodes(node, kinds);
  for (XMLNode n : nodes) {
    if (get_node_line(n) == line_number) {
      return n;
    }
  }
  return XMLNode();
}
/**
 * find one node on each one line in lines. The nodes must of kind `k`
 */
XMLNodeList find_nodes_on_lines(XMLNode node, XMLNodeKind k, std::vector<int> lines) {
  XMLNodeList result;
  for (int l : lines) {
    result.push_back(find_node_on_line(node, k, l));
  }
  return result;
}
/**
 * find one node on each one line in lines. The node just need to satisfy one of the kinds.
 */
XMLNodeList find_nodes_on_lines(XMLNode node, std::vector<XMLNodeKind> kinds, std::vector<int> lines) {
  XMLNodeList result;
  for (int l : lines) {
    result.push_back(find_node_on_line(node, kinds, l));
  }
  return result;
}
/**
 * The smallest tag enclosing line.
 */
XMLNode find_node_enclosing_line(XMLNode node, XMLNodeKind k, int line_number) {
  XMLNodeList nodes = find_nodes(node, k);
  XMLNode result;
  int cur_line = -1;
  for (XMLNode n : nodes) {
    int first_line = get_node_line(n);
    int last_line = get_node_last_line(n);
    if (first_line <= line_number && last_line >= line_number) {
      // found a inner node, replace
      if (first_line > cur_line) {
        result = n;
        cur_line = first_line;
      }
    }
  }
  // may be an empty one
  return result;
}
/**
 * The outmost tag enclosing line
 */
XMLNode find_outer_node_enclosing_line(XMLNode node, XMLNodeKind k, int line_number) {
  XMLNodeList nodes = find_nodes(node, k);
  XMLNode result;
  int cur_line = line_number+1; // different from above function
  for (XMLNode n : nodes) {
    int first_line = get_node_line(n);
    int last_line = get_node_last_line(n);
    if (first_line <= line_number && last_line >= line_number) {
      // found a inner node, replace
      if (first_line < cur_line) { // different from above function
        result = n;
        cur_line = first_line;
      }
    }
  }
  // may be an empty one
  return result;
}

/*******************************
 ** based on content (mainly comment)
 *******************************/
/**
 * find the *first* node of kind k, under "node", whose text contains "s".
 */
XMLNode find_node_containing_str(XMLNode node, XMLNodeKind k, std::string s) {
  XMLNodeList nodes = find_nodes(node, k);
  for (XMLNode n : nodes) {
    std::string text = get_text(n);
    if (text.find(s) != std::string::npos) return n;
  }
  return XMLNode();
}
/**
 * find *all* the nodes of kind k, under "node", whose text contains "s".
 */
XMLNodeList find_nodes_containing_str(XMLNode node, XMLNodeKind k, std::string s) {
  XMLNodeList result;
  XMLNodeList nodes = find_nodes(node, k);
  for (XMLNode n : nodes) {
    std::string text = get_text(n);
    if (text.find(s) != std::string::npos) result.push_back(n);
  }
  return result;
}
XMLNode find_node_containing_str(const XMLDoc &doc, XMLNodeKind k, std::string s) {
  return find_node_containing_str(doc.document_element(), k, s);
}
XMLNodeList find_nodes_containing_str(const XMLDoc &doc, XMLNodeKind k, std::string s) {
  return find_nodes_containing_str(doc.document_element(), k, s);
}


  
XMLNode find_callsite(pugi::xml_document &doc, std::string func) {
  XMLNodeList call_nodes = find_nodes(doc, NK_Call);
  for (XMLNode call_node : call_nodes) {
    std::string callee = call_get_name(call_node);
    if (callee == func) {
      return call_node;
    }
  }
  return XMLNode();
}

XMLNode find_callsite(pugi::xml_node node, std::string func) {
  XMLNodeList call_nodes = find_nodes(node, NK_Call);
  for (XMLNode call_node : call_nodes) {
    std::string callee = call_get_name(call_node);
    if (callee == func) {
      return call_node;
    }
  }
  return XMLNode();
}

XMLNodeList find_callsites(pugi::xml_node node, std::string func) {
  XMLNodeList ret;
  XMLNodeList call_nodes = find_nodes(node, NK_Call);
  for (XMLNode call_node : call_nodes) {
    std::string callee = call_get_name(call_node);
    if (callee == func) {
      ret.push_back(call_node);
    }
  }
  return ret;
}

/*******************************
 ** build doc from scratch
 *******************************/

using namespace utils;
/**
 * Get code as string of <tag_name> node in filename that encloses line_number
 * FIXME <tag> <tag> xxx </tag> </tag>, return which one?
 */
std::string get_code_enclosing_line(const std::string& filename, int line_number, std::string tag_name) {
  // pugi::xml_document doc;
  // utils::file2xml(filename, doc);
  XMLDoc *doc = XMLDocReader::CreateDocFromFile(filename);
  pugi::xml_node root =doc->document_element();
  std::string query = ".//" + tag_name;
  pugi::xpath_node_set nodes = root.select_nodes(query.c_str());
  for (auto it=nodes.begin();it!=nodes.end();it++) {
    pugi::xml_node node = it->node();
    int first_line = get_node_line(node);
    int last_line = get_node_last_line(node);
    // FIXME the equal is necessary? Be precise.
    if (first_line <= line_number && last_line >= line_number) {
      return get_text(node);
    }
  }
  return "";
}

/**
 * This is to test the conditional compilation which includes extra unmatched braces.
 * E.g.

 #ifdef NO_FSTAT
 if (stat(ofname, &ostat) != 0) {
 #else
 if (fstat(ofd, &ostat) != 0) {
 #endif

 * 
 */
TEST(ASTHelperTestCase, DISABLED_CodeEnclosingLineTest) {
  std::string filename = "/Users/hebi/benchmark/gzip-1.2.4/src/gzip.c";
  int linum = 1557;
  std::string tagname = "function";
  std::string code = get_code_enclosing_line(filename, linum, tagname);
  std::cout << code  << "\n";
}

/**
 * This require the benchmark, so disable it.
 * It is used to find the unbalanced braces caused by conditional compilation.
 * We actually cannot do much for it unless we perform the preprocess, but the output is not easily parsable.
 */
TEST(ASTHelperCase, DISABLED_GetCodeEnclosingLine) {
  // std::string code = get_code_enclosing_line("/Users/hebi/github/Helium/benchmark/real-programs/bugbench/gzip-1.2.4/src/util.c", 279, "function");
  std::string code = get_code_enclosing_line("/Users/hebi/github/Helium/benchmark/real-programs/bugbench/gzip-1.2.4/src/gzip.c", 1556, "function");
  std::cout << code  << "\n";
}
