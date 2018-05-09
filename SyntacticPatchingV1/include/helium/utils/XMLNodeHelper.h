#ifndef XMLNODE_HELPER_H
#define XMLNODE_HELPER_H

#include "XMLNode.h"

/*******************************
 ** Help function
 *******************************/
bool is_valid_ast(XMLNode node);


XMLNode next_sibling(XMLNode node);
XMLNode previous_sibling(XMLNode node);
XMLNode parent(XMLNode node);

// helium specific ast
// using helium_valid_ast
bool helium_is_valid_ast(XMLNode n);
XMLNode helium_next_sibling(XMLNode n);
XMLNode helium_previous_sibling(XMLNode n);
XMLNode helium_parent(XMLNode n);

/**
 * Check if node is a sub node of any one of parent_nodes
 */
bool contains(XMLNodeList parent_nodes, XMLNode node);
/**
 * Check if child is a sub node of parent
 */
bool contains(XMLNode parent, XMLNode child);

std::string get_text(XMLNode n);
std::string get_text(XMLNodeList nodes);
std::string get_text_ln(XMLNodeList nodes);


// deprecated
// std::string get_text_except(Node n, std::string tag);
// std::string get_text_except(NodeList nodes, std::string tag);

std::string get_text_except(XMLNode n, XMLNodeKind k);
std::string get_text_except(XMLNode n, std::vector<XMLNodeKind> kinds);
std::string get_text_except(XMLNodeList nodes, XMLNodeKind k);
std::string get_text_except(XMLNodeList nodes, std::vector<XMLNodeKind> kinds);

/**
 * True if node is inside a node of kind "kind"
 */
bool in_node(XMLNode node, XMLNodeKind kind);


int get_node_line(pugi::xml_node node);
int get_node_last_line(pugi::xml_node node);

std::pair<int, int> get_node_position(pugi::xml_node node);
std::pair<int, int> get_node_begin_position(pugi::xml_node node);
std::pair<int, int> get_node_end_position(pugi::xml_node node);

/*******************************
 ** find nodes
 *******************************/

XMLNode find_first_node_bfs(XMLNode node, std::string tag);


// by kind
XMLNodeList find_nodes(XMLNode node, XMLNodeKind kind);
XMLNodeList find_nodes(const XMLDoc& doc, XMLNodeKind kind);
XMLNodeList find_nodes_from_root(XMLNode node, XMLNodeKind kind);
// by kinds
XMLNodeList find_nodes(XMLNode node, std::vector<XMLNodeKind> kinds);
XMLNodeList find_nodes(const XMLDoc& doc, std::vector<XMLNodeKind> kinds);
XMLNodeList find_nodes_from_root(XMLNode node, std::vector<XMLNodeKind> kinds);

// based on line
XMLNode find_node_on_line(XMLNode node, XMLNodeKind k, int line_number);
XMLNode find_node_on_line(XMLNode node, std::vector<XMLNodeKind> kinds, int line_number);
XMLNodeList find_nodes_on_lines(XMLNode node, XMLNodeKind k, std::vector<int> lines);
XMLNodeList find_nodes_on_lines(XMLNode node, std::vector<XMLNodeKind> kinds, std::vector<int> lines);
// enclosing line
// TODO kinds
XMLNode find_node_enclosing_line(XMLNode node, XMLNodeKind k, int line_number);
XMLNode find_outer_node_enclosing_line(XMLNode node, XMLNodeKind k, int line_number);

// based on content (mainly comment)
XMLNode find_node_containing_str(XMLNode node, XMLNodeKind k, std::string s);
XMLNodeList find_nodes_containing_str(XMLNode node, XMLNodeKind k, std::string s);
XMLNode find_node_containing_str(const XMLDoc &doc, XMLNodeKind k, std::string s);
XMLNodeList find_nodes_containing_str(const XMLDoc &doc, XMLNodeKind k, std::string s);

/**
 * Find the first <call> whose name is func.
 */
XMLNode find_callsite(pugi::xml_document &doc, std::string func);
XMLNode find_callsite(pugi::xml_node node, std::string func);
XMLNodeList find_callsites(pugi::xml_node node, std::string func);
  

std::string get_code_enclosing_line(const std::string& filename, int line_number, std::string tag_name);

// srcml specific
std::string get_filename(XMLNode node);
std::string get_filename(XMLDoc &doc);


#endif /* XMLNODE_HELPER_H */
