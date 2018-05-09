#include "helium/utils/XMLNode.h"

#include <cstring>

/*******************************
 ** DomUtil
 *******************************/

/**
 * Get the call place of the function in node.
 * @param[in] node the <function> node in xml
 * @return the node <call> of the function. Or null_node is not found.
 */
// pugi::xml_node get_function_call(pugi::xml_node node) {
//   const char *func_name = node.child_value("name");
//   pugi::xpath_node_set call_nodes = node.root().select_nodes("//call");
//   for (auto it=call_nodes.begin();it!=call_nodes.end();it++) {
//     if (strcmp(it->node().child_value("name"), func_name) == 0) {
//       return get_parent_ast_element(it->node());
//     }
//   }
//   pugi::xml_node null_node;
//   return null_node;
// }

/**
 * Get text content of node.
 * Will traverse xml structure.
 * But will avoid a tag with "helium-omit" ATTR.
 * Add some structure to make the syntax correct. FIXME
 */
std::string
get_text_content(pugi::xml_node node) {
  std::string text;
  if (!node) return "";
  for (pugi::xml_node n : node.children()) {
    if (n.type() == pugi::node_element) {
      if (!node.attribute("helium-omit")) {
        // add text only if it is not in helium-omit
        text += get_text_content(n);
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
 * Get text content of node, except <name> tag
 */
std::string
get_text_content_except_tag(pugi::xml_node node, std::string name) {
  if (!node) return "";
  std::string text;
  for (pugi::xml_node n : node.children()) {
    if (n.type() == pugi::node_element) {
      if (!node.attribute("helium-omit")) {
        if (strcmp(n.name(), name.c_str()) != 0) {
          text += get_text_content_except_tag(n, name);
        }
      }
      // TODO this version does not use the trick for simplification,
      // so it doesnot work with simplification
    } else {
      text += n.value();
    }
  }
  return text;
}


// struct simple_walker: pugi::xml_tree_walker {
//   std::string text;
//   virtual bool for_each(pugi::xml_node& node) {
//     if (!node.attribute("helium-omit")) {
//       text += node.value();
//     }
//     return true; // continue traversal
//   }
// };

// std::string GetTextContent(pugi::xml_node node) {
//   simple_walker walker;
//   node.traverse(walker);
//   return walker.text;
// }



