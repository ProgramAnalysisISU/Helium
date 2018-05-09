#include "helium/utils/XMLNode.h"

#include <gtest/gtest.h>
#include "helium/utils/Utils.h"
#include "helium/utils/XMLDocReader.h"

#include "helium/utils/XMLNodeHelper.h"

static const std::map<XMLNodeKind, std::string> kind_to_name_map {
  {NK_Function, "function"}
  , {NK_ArgList,  "argument_list"}
  , {NK_Arg,      "argument"}
  , {NK_DeclStmt, "decl_stmt"}
  , {NK_Decl,     "decl"}
  , {NK_ExprStmt, "expr_stmt"}
  , {NK_Expr,     "expr"}
  , {NK_For,      "for"}
  , {NK_Control,  "control"}
  , {NK_Type,     "type"}
  , {NK_Name,     "name"}
  , {NK_Block,    "block"}
  , {NK_Stmt,     "stmt"}
  , {NK_Operator, "operator"}
  , {NK_Specifier,"specifier"}
  , {NK_Literal,  "literal"}
  , {NK_If,       "if"}
  , {NK_Condition, "condition"}
  , {NK_Then,     "then"}
  , {NK_Else,     "else"}
  , {NK_Case,     "case"}
  , {NK_Default,  "default"}
  , {NK_Switch,   "switch"}
  , {NK_While,    "while"}
  , {NK_Do,       "do"}
  , {NK_Call,     "call"}
  , {NK_ParamList, "parameter_list"}
  , {NK_Param,    "parameter"}
  , {NK_Break,    "break"}
  , {NK_Continue, "continue"}
  , {NK_Return,   "return"}
  , {NK_Label,    "label"}
  , {NK_Goto,     "goto"}
  , {NK_Typedef,  "typedef"}
  , {NK_Struct,   "struct"}
  , {NK_Union,    "union"}
  , {NK_Enum,     "enum"}
  , {NK_Comment,  "comment"}
  , {NK_Define,   "cpp:define"}
  , {NK_IfnDef,   "cpp:ifndef"}
  , {NK_IfDef,    "cpp:ifdef"}
  , {NK_DefElse,  "cpp:else"}
  , {NK_EndIf,    "cpp:endif"}
  , {NK_Include,  "cpp:include"}
  , {NK_Position, "pos:position"}
  , {NK_Unit,     "unit"}
  , {NK_Null,     "NULL"}

  , {NK_Extern,   "extern"}
  , {NK_EmptyStmt, "empty_stmt"}
  , {NK_FuncDecl, "function_decl"}
  , {NK_Modifier, "modifier"}
  , {NK_Range,    "range"}
  , {NK_StructDecl, "struct_decl"}
  , {NK_UnionDecl, "union_decl"}
  , {NK_Asm,      "asm"}
  , {NK_Op,       "op:operator"}
  , {NK_Sizeof,   "sizeof"}
  , {NK_Pragma,   "cpp:pragma"}
  , {NK_Init,     "init"}
  , {NK_Undef,    "cpp:undef"}
  , {NK_Directive, "cpp:directive"}
  , {NK_HeliumInstrument, "helium_instrument"}
  , {NK_CppIf, "cpp:if"}
  , {NK_ElseIf, "elseif"}
  , {NK_CppElif, "cpp:elif"}
  , {NK_ForIncr, "incr"}
  // two special
  , {NK_Other, "HELIUM-OTHER"}
  , {NK_Invalid, "HELIUM_INVALID"}
};

static const std::map<std::string, XMLNodeKind> name_to_kind_map {
  {"function",  NK_Function}
  , {"argument_list", NK_ArgList}
  , {"argument",  NK_Arg}
  , {"decl_stmt", NK_DeclStmt}
  , {"decl",      NK_Decl}
  , {"expr_stmt", NK_ExprStmt}
  , {"expr",      NK_Expr}
  , {"for",       NK_For}
  , {"control",   NK_Control}
  , {"type",      NK_Type}
  , {"name",      NK_Name}
  , {"block",     NK_Block}
  , {"stmt",      NK_Stmt}
  , {"operator",  NK_Operator}
  , {"specifier", NK_Specifier}
  , {"literal",   NK_Literal}
  , {"if",        NK_If}
  , {"condition", NK_Condition}
  , {"then",      NK_Then}
  , {"else",      NK_Else}
  , {"case",      NK_Case}
  , {"default",   NK_Default}
  , {"switch",    NK_Switch}
  , {"while",     NK_While}
  , {"do",        NK_Do}
  , {"call",      NK_Call}
  , {"parameter_list", NK_ParamList}
  , {"parameter",     NK_Param}
  , {"break",     NK_Break}
  , {"continue",  NK_Continue}
  , {"return",    NK_Return}
  , {"label",     NK_Label}
  , {"goto",      NK_Goto}
  , {"typedef",   NK_Typedef}
  , {"struct",    NK_Struct}
  , {"union",     NK_Union}
  , {"enum",      NK_Enum}
  , {"comment",   NK_Comment}
  , {"cpp:define",    NK_Define}
  , {"cpp:ifndef", NK_IfnDef}
  , {"cpp:ifdef", NK_IfDef}
  , {"cpp:else",  NK_DefElse}
  , {"cpp:endif", NK_EndIf}
  , {"cpp:include", NK_Include}
  , {"pos:position", NK_Position}
  , {"unit"       , NK_Unit}
  , {"NULL",      NK_Null}
  
  , {"extern",    NK_Extern}
  , {"empty_stmt", NK_EmptyStmt}
  , {"function_decl", NK_FuncDecl}
  , {"modifier",  NK_Modifier}
  , {"range",    NK_Range}
  , {"struct_decl", NK_StructDecl}
  , {"union_decl", NK_UnionDecl}
  , {"asm",   NK_Asm}
  , {"op:operator", NK_Op}
  , {"sizeof",  NK_Sizeof}
  , {"cpp:pragma", NK_Pragma}
  , {"init",     NK_Init}
  , {"cpp:undef", NK_Undef}
  , {"cpp:directive", NK_Directive}
  , {"helium_instrument", NK_HeliumInstrument}
  , {"cpp:if", NK_CppIf}
  , {"elseif", NK_ElseIf}
  , {"cpp:elif", NK_CppElif}
  , {"incr", NK_ForIncr}
};
/*
 * To make sure the above two mapping are consistant
 * Do not equal any more. Because NK_Other do not have a tag name.
 */
TEST(ast_test_case, DISABLED_kind_name_test) {
  ASSERT_EQ(kind_to_name_map.size(), name_to_kind_map.size());
  for (auto m : kind_to_name_map) {
    EXPECT_EQ(name_to_kind_map.at(m.second), m.first);
  }
}


std::string xmlnode_kind_to_name(XMLNodeKind k) {
  std::string result;
  try {
    result = kind_to_name_map.at(k);
  } catch (const std::out_of_range& e) {
    std::cerr << "Not supported kind.\n";
    assert(false);
  }
  return result;
}

  
XMLNodeKind xmlnode_to_kind(XMLNode node) {
  if (!node) return NK_Invalid;
  if (!node.name()) return NK_Invalid;
  const char *name = node.name();
  if (strlen(name) == 0) return NK_Invalid;
  // if (name_to_kind.find(name) != name_to_kind.end()) return name_to_kind.at(name);
  try {
    return name_to_kind_map.at(name);
  } catch (const std::out_of_range& e) {
    return NK_Other;
  }
}

XMLNodeList element_children(XMLNode node) {
  XMLNodeList ret;
  for (XMLNode n : node.children()) {
    if (n.type() == pugi::node_element) {
      ret.push_back(n);
    }
  }
  return ret;
}
XMLNode first_element_child(XMLNode node) {
  for (XMLNode n : node.children()) {
    if (n.type() == pugi::node_element) {
      return n;
    }
  }
  return XMLNode();
}

XMLNode next_element_sibling(XMLNode node) {
  XMLNode n = node.next_sibling();
  while (n) {
    if (n.type() == pugi::node_element) {
      return n;
    }
    n = n.next_sibling();
  }
  return XMLNode();
}

/*******************************
 ** Specific node
 *******************************/

/*******************************
 ** Expression <expr>
 *******************************/
  
static void
simplify_variable_name(std::string& s) {
  s = s.substr(0, s.find('['));
  s = s.substr(0, s.find("->"));
  s = s.substr(0, s.find('.'));
  // TODO wiki the erase-remove-idiom
  s.erase(std::remove(s.begin(), s.end(), '('), s.end());
  s.erase(std::remove(s.begin(), s.end(), ')'), s.end());
  s.erase(std::remove(s.begin(), s.end(), '*'), s.end());
  s.erase(std::remove(s.begin(), s.end(), '&'), s.end());
}

// std::set<std::string>
// expr_stmt_get_ids(Node node) {
//   // TODO an expr_stmt can only have <expr> as child? Only have one?
//   Node expr = node.child("expr");
//   return expr_get_ids(expr);
// }

// /**
//  * Expr
//  */
// std::set<std::string> expr_get_ids(Node node) {
//   std::set<std::string> result;
//   for (Node n : node.children("name")) {
//     std::string s = get_text(n);
//     simplify_variable_name(s);
//     result.insert(s);
//   }
//   return result;
// }

std::set<std::string> expr_stmt_get_var_ids(XMLNode node) {
  std::set<std::string> result;
  if (xmlnode_to_kind(node) == NK_Expr) {
    for (XMLNode n : node.children("name")) {
      result.insert(n.child_value());
    }
  }
  pugi::xpath_node_set expr_nodes = node.select_nodes(".//expr");
  for (auto n : expr_nodes) {
    std::set<std::string> tmp = expr_get_var_ids(n.node());
    result.insert(tmp.begin(), tmp.end());
  }
  return result;
}

/**
 * Deprecated?
 */
std::set<std::string> expr_get_var_ids(XMLNode node) {
  std::set<std::string> result;
  for (XMLNode n : node.children("name")) {
    std::string s = get_text(n);
    simplify_variable_name(s);
    result.insert(s);
  }
  return result;
}

static bool name_valid(XMLNode node) {
  assert(std::string("name") == node.name());
  for (auto child : node.children()) {
    if (child.type() != pugi::node_pcdata
        && std::string("pos:position") != child.name()) {
      return false;
    }
  }
  return true;
}
/**
 * Get all the innermost <name> value
 * <name><name></name> <other>...</other> </name> will only get the inner most name
 */
std::set<std::string> get_used_names(XMLNode node) {
  std::set<std::string> ret;
  if (std::string("name") == node.name()) {
    if (name_valid(node)) {
      std::string text = node.child_value();
      if (!text.empty()) {
        ret.insert(text);
      }
    }
  }
  for (auto name_xpath_node : node.select_nodes(".//name")) {
    pugi::xml_node n = name_xpath_node.node();
    if (name_valid(n)) {
      std::string text = n.child_value();
      if (!text.empty()) {
        ret.insert(text);
      }
    }
  }
  return ret;
}


TEST(XMLNodeTest, GetUsedNameTest) {
  pugi::xml_document doc;
  const char *str = R"prefix(
<decl_stmt>
	<decl>
		<type>
			<name pos:line="15" pos:column="5">size_t<pos:position pos:line="15" pos:column="11" />
			</name>
		</type> <name pos:line="15" pos:column="12">i<pos:position pos:line="15" pos:column="13" />
		</name>
	</decl>;<pos:position pos:line="15" pos:column="14" />
</decl_stmt>
)prefix";
  doc.load_string(str);
  std::set<std::string> names = get_used_names(doc.document_element());
  ASSERT_EQ(names.size(), 2);
}

/**
 * <name> and nested <expr><name>, and also <expr><name><name> for structures.
 * Will ensure no empty items in the set.
 * FIXME This is very ugly
 * FIXME (sizeof name) will not be a <expr><name> ... but a <sizeof><name>xxx
 */
std::set<std::string> get_var_ids(XMLNode node) {
  std::set<std::string> result;
  if (xmlnode_to_kind(node) == NK_Expr) {
    for (XMLNode n : node.children("name")) {
      std::string name = n.child_value();
      if (!name.empty()) result.insert(name);
      else {
        // fix the bug that, this level name has structure aa->bb
        name = n.child_value("name");
        if (!name.empty()) result.insert(name);
      }
    }
  }
  for (auto name_node : node.select_nodes(".//expr/name")) {
    // this may be empty string, e.g. a.b
    // which will be <expr><name><name>
    std::string name = name_node.node().child_value();
    // this will get the first nested name, which is ~a~
    // if (name.empty()) name = name_node.node().child_value("name");
    if (!name.empty()) {result.insert(name);}
  }
  // fix bug: ii < name->aa
  // now in this exp, the <name><name> is not the first name, but the second.
  // also, if there're many such structures in an expr, we need all of them.
  // FIXME expr/name/name may be something else other than aa->bb?
  // note this will match <expr><name><name></name><name></name></name></expr>
  // two times, but actually I only need the first <name> under <name>
  // That is the aa in aa->bb, we don't need bb.
  // The [1] select the first.
  for (auto struct_name_node : node.select_nodes(".//expr/name/name[1]")) {
    std::string name = struct_name_node.node().child_value();
    if (!name.empty()) result.insert(name);
  }
  assert(result.count("") == 0);
  return result;
}

std::set<std::string> get_var_ids(XMLNodeList nodes) {
  std::set<std::string> result;
  for (XMLNode n : nodes) {
    std::set<std::string> tmp = get_var_ids(n);
    result.insert(tmp.begin(), tmp.end());
  }
  return result;
}

/**
 * //type/name
 */
std::set<std::string> get_type_ids(XMLNode node) {
  std::set<std::string> result;
  if (xmlnode_to_kind(node) == NK_Type) {
    for (XMLNode n : node.children("name")) {
      result.insert(n.child_value());
    }
  }
  // FIXME not sure if this will reach the root
  for (auto name_node : node.select_nodes(".//type/name")) {
    result.insert(name_node.node().child_value());
  }
  return result;
}
std::set<std::string> get_type_ids(XMLNodeList nodes) {
  std::set<std::string> result;
  for (XMLNode n : nodes) {
    std::set<std::string> tmp = get_type_ids(n);
    result.insert(tmp.begin(), tmp.end());
  }
  return result;
}


/**
 * //call/name
 */
std::set<std::string> get_call_ids(XMLNode node) {
  std::set<std::string> result;
  if (xmlnode_to_kind(node) == NK_Call) {
    for (XMLNode n : node.children("name")) {
      result.insert(n.child_value());
    }
  }
  for (auto name_node : node.select_nodes(".//call/name")) {
    result.insert(name_node.node().child_value());
  }
  return result;
}
std::set<std::string> get_call_ids(XMLNodeList nodes) {
  std::set<std::string> result;
  for (XMLNode n : nodes) {
    std::set<std::string> tmp = get_call_ids(n);
    result.insert(tmp.begin(), tmp.end());
  }
  return result;
}


/**
 * decl_stmt's variables except the declared variables.
 * e.g. int a = b; int a = func(b,c);
 */
std::set<std::string> decl_stmt_get_var_ids(XMLNode node) {
  std::set<std::string> result;
  if (xmlnode_to_kind(node) == NK_Expr) {
    for (XMLNode n : node.children("name")) {
      result.insert(n.child_value());
    }
  }
  pugi::xpath_node_set expr_nodes = node.select_nodes(".//expr");
  for (auto n : expr_nodes) {
    std::set<std::string> tmp = expr_get_var_ids(n.node());
    result.insert(tmp.begin(), tmp.end());
  }
  return result;
}

/**
 * for general node, search for //expr, and resolve /name
 */
std::set<std::string> node_get_var_ids(XMLNode node) {
  std::set<std::string> result;
  if (xmlnode_to_kind(node) == NK_Expr) {
    for (XMLNode n : node.children("name")) {
      result.insert(n.child_value());
    }
  }
  pugi::xpath_node_set expr_nodes = node.select_nodes(".//expr");
  for (auto n : expr_nodes) {
    std::set<std::string> tmp = expr_get_var_ids(n.node());
    result.insert(tmp.begin(), tmp.end());
  }
  return result;
}

/*******************************
 ** Unit
 *******************************/

/**
 * OK, this node is not necessary to be a <unit> node
 */
std::string unit_get_filename(XMLNode node) {
  XMLNode root = node.root();
  XMLNode unit_node = root.select_node("/unit").node();
  assert(unit_node);
  std::string ret = unit_node.attribute("filename").value();
  return ret;
}


// TEST(ASTTestCase, UnitGetFilenameTest) {
//   std::string tmp_dir = utils::create_tmp_dir();
//   std::string file = tmp_dir + "/a.c";
//   utils::write_file(file, "");
//   // 1. the name should be absolute path
//   // XMLDoc *doc = XMLDocReader::CreateDocFromFile("/Users/hebi/tmp/a.c");
//   XMLDoc *doc = XMLDocReader::CreateDocFromFile(file);
//   std::string filename = unit_get_filename(doc->document_element());
//   // std::cout << filename  << "\n";
//   // 2. the leading slash is removed by srcml
//   EXPECT_EQ("/" + filename, file);
//   delete doc;
// }


/*******************************
 ** Function
 *******************************/

/**
 * Call
 */
std::string call_get_name(XMLNode node) {
  return node.child_value("name");
}


/**
 * decl_stmt
 */
XMLNodeList decl_stmt_get_decls(XMLNode node) {
  XMLNodeList nodes;
  for (XMLNode n : node.children("decl")) {
    nodes.push_back(n);
  }
  return nodes;
}

std::string decl_get_name(XMLNode node) {
  // this is an array
  if (node.child("name").child("name")) {
    return node.child("name").child_value("name");
  }
  // has a <decl><name>xxx</name></decl>
  return node.child_value("name");
}


TEST(XMLNodeCase, DeclGetTypeTest) {
  std::string code = "int a; const int b;";
  XMLDoc *doc = XMLDocReader::CreateDocFromString(code);
  XMLNodeList nodelist = find_nodes(*doc, NK_Decl);
  ASSERT_EQ(nodelist.size(), 2);
  XMLNode node;
  node = nodelist[0];
  EXPECT_EQ(decl_get_type(node), "int");
  node = nodelist[1];
  EXPECT_EQ(decl_get_type(node), "const int");
}

std::string decl_get_type(XMLNode node) {
  // if (!node || strcmp(node.name(), "decl") != 0) {
  //   assert(false && "node should be <decl>");
  // }
  if (node.child("type").attribute("ref")) {
    assert(strcmp(node.child("type").attribute("ref").value(), "prev") == 0);
    // go to parent <decl_stmt>
    // get the first <decl>'s type
    // assert it should not be empty
    XMLNode decl_stmt_node = node.parent();
    std::string type = decl_stmt_node.child("decl").child("type").child_value("name");
    // then, count how many <modifier>s direct before, until a <decl>
    // CAUTION: <modifier> may contain &
    for (XMLNode n=node.previous_sibling();xmlnode_to_kind(n)!=NK_Decl;n=n.previous_sibling()) {
      if (xmlnode_to_kind(n) == NK_Modifier && strcmp(n.child_value(), "*")==0) {
        type +='*';
      }
    }
    return type;
  }
  // return node.child("type").child_value("name");
  // return get_text(node.child("type")); // this version contains modifiers
  // this version is even better:
  // 1) will append '*' directly to the type name, without space
  // 2) better control of the content of node, incase there's other unexpected item inside <type>
  // Bug: struct AA yyy;
  // The <type><name> would be <name></name><name></name>
  // so the child_value will be empty!
  // std::string type = node.child("type").child_value("name");
  // FIXME the corresponding type handle will also be affected.
  // Consider the type raw is consisted with a couple of words.
  std::string type = get_text(node.child("type").child("name"));
  if (type.empty()) {
    // std::cout <<get_text(node)  << "\n";
    // node.print(std::cout);
    // FIXME
    // it can be ...
    // void foo(int a, ...) {}
    return "";
  }
  // const
  std::string specifier = get_text(node.child("type").child("specifier"));
  if (!specifier.empty() && specifier != "register") { // FIXME register variable cannot take address
    type = specifier + " " + type;
  }
  for (XMLNode n : node.child("type").children("modifier")) {
    if (strcmp(n.child_value(), "*") == 0) {
      type += '*';
    }
  }
  // array
  // UPDATE: type doesnot contain array dimension information
  // if (node.child("name").child("index")) {
  //   for (XMLNode n : node.child("name").children("index")) {
  //     n.value(); // to suppress unused variable warning
  //     // type += "[6]"; // FIXME I hard coded this array size here!
  //     // Now I use the true value in original code.
  //     // It can be a literal number
  //     // or a variable, a macro
  //     // Or, it may be empty as the parameter of the function.
  //     type += get_text(n);
  //   }
  // }
  return type;
}

std::vector<std::string> decl_get_dimension(XMLNode node) {
  std::vector<std::string> result;
  if (node.child("name").child("index")) {
    for (XMLNode n : node.child("name").children("index")) {
      result.push_back(get_text(n.child("expr")));
    }
  }
  return result;
}

TEST(ast_test_case, decl_get_dimension_test) {
  XMLDoc *doc = nullptr;
  const char* raw = R"prefix(
int aa[5][4];
)prefix";
  doc = XMLDocReader::CreateDocFromString(raw);
  XMLNodeList nodes = find_nodes(*doc, NK_DeclStmt);
  ASSERT_EQ(nodes.size(), 1);
  XMLNode decl = nodes[0].child("decl");
  std::vector<std::string> dims = decl_get_dimension(decl);
  ASSERT_EQ(dims.size(), 2);
  // output
  // for (std::string dim : dims) {
  //   std::cout << dim  << "\n";
  // }
  // another test
  raw = R"prefix(
int aa[];
)prefix";
  

  doc = XMLDocReader::CreateDocFromString(raw);
  nodes = find_nodes(*doc, NK_DeclStmt);
  ASSERT_EQ(nodes.size(), 1);
  decl = nodes[0].child("decl");
  dims = decl_get_dimension(decl);
  // this will still be a dim, but has empty string
  ASSERT_EQ(dims.size(), 1);
}

  
/**
 * block
 */
XMLNodeList block_get_nodes(XMLNode node) {
  XMLNodeList result;
  for (XMLNode n : node.children()) {
    if (n.type() == pugi::node_element) result.push_back(n);
  }
  return result;
}



/**
 * Function
 */
  
std::string function_get_return_type(XMLNode node) {
  // return node.child("type").child_value("name");
  // FIXME This may have *, enum, etc. I.e. not clean
  return get_text(node.child("type"));
}
  
std::string function_get_name(XMLNode node) {
  // FIXME this may fail because ns::foo()
  return node.child_value("name");
}
  
XMLNodeList function_get_params(XMLNode node) {
  XMLNodeList nodes;
  XMLNode parameter_list = node.child("parameter_list");
  for (XMLNode param : parameter_list.children("parameter")) {
    nodes.push_back(param);
  }
  return nodes;
}

/**
 * Take care of legacy code.
 */
XMLNodeList function_get_param_decls(XMLNode node) {
  XMLNodeList result;
  if (node.child("parameter_list").child("parameter")) {
    // have parameter
    if (node.child("parameter_list").child("parameter").child("decl").child("name")) {
      // regular
      for (XMLNode param : node.child("parameter_list").children("parameter")) {
        result.push_back(param.child("decl"));
      }
    } else {
      // legacy
      for (XMLNode decl_stmt : node.children("decl_stmt")) {
        result.push_back(decl_stmt.child("decl"));
      }
    }
  }
  return result;
}

TEST(ASTTestCase, LegacyFuncTest) {
  XMLDoc *doc;
  const char *raw = R"prefix(

enum context
ns_ownercontext(type, transport)
	int type;
	enum transport transport;
{
}

)prefix";

  doc = XMLDocReader::CreateDocFromString(raw);
  XMLNodeList nodes = find_nodes(*doc, NK_Function);
  ASSERT_EQ(nodes.size(), 1);
  
  XMLNodeList decls = function_get_param_decls(nodes[0]);
  ASSERT_EQ(decls.size(), 2);
  // for (XMLNode decl : decls) {
  //   std::cout << get_text(decl)  << "\n";
  // }
  EXPECT_EQ(decl_get_type(decls[0]), "int");
  EXPECT_EQ(decl_get_name(decls[0]), "type");
  EXPECT_EQ(decl_get_type(decls[1]), "enum transport");
  EXPECT_EQ(decl_get_name(decls[1]), "transport");
  std::string ret_ty = function_get_return_type(nodes[0]);
  EXPECT_EQ(ret_ty, "enum context");
}


XMLNode function_get_block(XMLNode node) {
  return node.child("block");
}

std::string param_get_type(XMLNode node) {
  // FIXME should contains modifiers, pointers, etc. Or should do semantic analysis further?
  return node.child("decl").child("type").child_value("name");
}
std::string param_get_name(XMLNode node) {
  return node.child("decl").child_value("name");
}

XMLNode param_get_decl(XMLNode node) {
  return node.child("decl");
}


/**
 *
 FIXME
 local int get_istat(iname, sbuf)
    char *iname;
    struct stat *sbuf;
{
 * FIXME I need to use a parser because the code may contain comments
 */
std::string get_function_decl(std::string code) {

  // std::cout << "======"  << "\n";
  // std::cout << code  << "\n";
  // 1. remove whatever after {,
  // 2. if ; inside, remove (), and add another, move the ; into ",", and remove the last one. add ; after it.
  // assert(code.find("(") != std::string::npos);
  // // This might fail because of srcml bug: the function body is not parsed into the <function> tag.
  // assert(code.find("{") != std::string::npos);
  // assert(code.find(")") != std::string::npos);
  // assert(code.find("(") < code.find(")"));
  // assert(code.find(")") < code.find("{"));
  if (code.find("(") == std::string::npos
      || code.find("{") == std::string::npos
      || code.find(")") == std::string::npos
      || code.find("(") >= code.find(")")
      || code.find(")") >= code.find("{")
      ) {
    std::cerr << "Waring: get_function_decl: not a good function" << "\n";
    return "";
  }
  
  std::string ret = code.substr(0, code.find('{'));
  if (ret.find(';') == std::string::npos) {
    ret += ";";
  } else {
    std::string first, last;
    first = ret.substr(0, ret.find('('));
    last = ret.substr(ret.find(')')+1);
    std::string params;
    while (last.find(';') != std::string::npos) {
      int pos = last.find(';');
      params += last.substr(0, pos) + ",";
      last = last.substr(pos+1);
    }
    params.pop_back();
    ret = first + "(" + params + ");";
  }
  return ret;
}

TEST(ASTTestCase, FunctionDeclTest) {
  const char *code = R"prefix(
 local int get_istat(iname, sbuf)
    char *iname;
    struct stat *sbuf;
{
lalala
}
)prefix";
  std::string s(code);
  std::string decl = get_function_decl(s);
  // std::cout << decl  << "\n";

  code = R"prefix(
local int get_istat(iname, sbuf)
    char *iname;
    struct stat *sbuf;
{
    int ilen;  /* strlen(ifname) */
}
)prefix";
  decl = get_function_decl(code);
  // std::cout << decl  << "\n";
}

/**
 * Get function node the node belongs to.
 */
XMLNode get_function_node(XMLNode node) {
  while (node.parent()) {
    if (xmlnode_to_kind(node) == NK_Function) {
      return node;
    }
    node = node.parent();
  }
  return XMLNode();
}


/**
 * get a map from name to type.
 * e.g. int a,b; char c;
 * will return:
 * {(a,int), (b,int), (c,char)}
 */
// std::map<std::string, std::string> get_decl_detail(std::string code) {
//   std::map<std::string, std::string> result;
//   utils::trim(code);
//   if (code.back() != ';') code += ';';
//   XMLDoc doc;
//   utils::string2xml(code, doc);
//   XMLNodeList decl_stmts = find_nodes(doc, NK_DeclStmt);
//   for (XMLNode decl_stmt : decl_stmts) {
//     std::string type = decl_stmt.child("decl").child("type").child_value("name");
//     for (XMLNode decl : decl_stmt.children("decl")) {
//       for (XMLNode name : decl.children("name")) {
//         std::string s = name.child_value();
//         result[s] = type;
//       }
//     }
//   }
//   return result;
// }

/**
 * This node can be <decl_stmt>, or <init>(for), both of which may contains a list of <decl>s
 * deprecated.
 */
// std::map<std::string, std::string> get_decl_detail(XMLNode node) {
//   std::map<std::string, std::string> result;
//   std::string type = node.child("decl").child("type").child_value("name");
//   if (type.empty()) return result;
//   for (XMLNode decl : node.children("decl")) {
//     for (XMLNode name : decl.children("name")) {
//       std::string s = name.child_value();
//       result[s] = type;
//     }
//   }
//   return result;
// }


  
/**
 * For
 */

/**
 * Get the init stmts (only consider decl and expr here). Return a list of node, either decl or expr.
 * Add necessary comma for text convertion.
 *
 * The init statement of for is not only decls. It may be expr.
 * int ii = 0;
 * for (ii = 0; ii < 8 ; ++ii) {
 * }
 * for (int i=0,c=i;;) {}

 * Thus, this is not correct.
 */
XMLNodeList for_get_init_decls_or_exprs(XMLNode node) {
  XMLNodeList nodes;
  // for (XMLNode n : node.child("control").child("init").children("decl")) {
  //   nodes.push_back(n);
  // }
  for (XMLNode n : node.child("control").child("init").children()) {
    // FIXME I'm omitting everything else here
    if (xmlnode_to_kind(n) == NK_Decl || xmlnode_to_kind(n) == NK_Expr) {
      nodes.push_back(n);
    }
  }
  return nodes;
}
  
// std::map<std::string, std::string> for_get_init_detail(XMLNode node) {
//   return get_decl_detail(node.child("init"));
// }
XMLNode for_get_condition_expr(XMLNode node) {
  return node.child("control").child("condition").child("expr");
}
XMLNode for_get_incr_expr(XMLNode node) {
  return node.child("control").child("incr").child("expr");
}
XMLNode for_get_block(XMLNode node) {
  return node.child("block");
}
XMLNode for_get_control(XMLNode node) {
  return node.child("control");
}



/*******************************
 ** If
 *******************************/
XMLNode if_get_condition_expr(XMLNode node) {
  return node.child("condition").child("expr");
}
XMLNode if_get_then_block(XMLNode node) {
  return node.child("then").child("block");
}

XMLNode if_get_else_block(XMLNode node) {
  return node.child("else").child("block");
}


XMLNode if_get_then(XMLNode node) {
  return node.child("then");
}
XMLNode if_get_else(XMLNode node) {
  return node.child("else");
}
XMLNodeList if_get_elseifs(XMLNode node) {
  XMLNodeList ret;
  for (XMLNode n : node.children("elseif")) {
    ret.push_back(n);
  }
  return ret;
}

XMLNode then_get_block(XMLNode node) {
  return node.child("block");
}
XMLNode else_get_block(XMLNode node) {
  return node.child("block");
}
XMLNode elseif_get_block(XMLNode node) {
  return node.child("if").child("then").child("block");
}
// elseif IF related
XMLNode elseif_get_condition_expr(XMLNode node) {
  return node.child("if").child("condition").child("expr");
}


/*******************************
 ** Switch
 *******************************/

XMLNode switch_get_condition_expr(XMLNode node) {
  return node.child("condition").child("expr");
}
XMLNodeList switch_get_cases(XMLNode node) {
  XMLNodeList result;
  for (XMLNode n : node.child("block").children("case")) {
    result.push_back(n);
  }
  return result;
}
XMLNode switch_get_default(XMLNode node) {
  return node.child("block").child("default");
}

XMLNode case_get_condition_expr(XMLNode node) {
  return node.child("expr");
}



// deprecated
XMLNodeList case_get_nodes(XMLNode node) {
  XMLNodeList result;
  if (node.child("block")) {
    // FIXME this child may be a inner block, not the first one
    XMLNode block = node.child("block");
    result = block_get_nodes(block);
  } else {
    // the first expr and a : are conditino.
    // TODO
  }
  return result;
}
/**
 * Get case blocks. Skip case.
 */
XMLNodeList switch_get_blocks(XMLNode node) {
  XMLNodeList result;
  for (XMLNode b : node.child("block").children("block")) {
    result.push_back(b);
  }
  return result;
}
/**
 * This is the first level block. It is not so special.
 */
XMLNode switch_get_block(XMLNode node) {
  return node.child("block");
}
/**
 * Actually switch just has content. No special blocks.
 */
// XMLNodeList switch_get_content(XMLNode node) {
//   XMLNodeList result;
//   for (XMLNode n : node.child("block").children()) {
//     if (n.type() == pugi::node_element && xmlnode_to_kind(n) != NK_Condition && kind())
//   }
// }

/*******************************
 ** Do
 *******************************/
XMLNode do_get_condition_expr(XMLNode node) {
  return node.child("condition").child("expr");
}
XMLNode do_get_condition(XMLNode node) {
  return node.child("condition");
}
XMLNode do_get_block(XMLNode node) {
  return node.child("block");
}

/*******************************
 ** While
 *******************************/
XMLNode while_get_condition_expr(XMLNode node) {
  return node.child("condition").child("expr");
}
XMLNode while_get_block(XMLNode node) {
  return node.child("block");
}




  

