#ifndef XMLNODE_H
#define XMLNODE_H

#include <pugixml.hpp>

#include <string>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <iostream>


typedef pugi::xml_node XMLNode;
typedef pugi::xml_document XMLDoc;
typedef std::vector<XMLNode> XMLNodeList;

typedef enum _XMLNodeKind {
  // general structure
  NK_DeclStmt,
  NK_Decl,
  NK_Init,
  NK_ExprStmt,
  NK_Expr,
  NK_EmptyStmt,
  // more general
  NK_Type,
  NK_Name,
  NK_Block,
  NK_Stmt,
  NK_Operator,
  NK_Op, // why op:operator?
  NK_Asm,
  NK_Literal,
  NK_Specifier,
  NK_Modifier,
  NK_Extern,
  NK_Range, // bit field
  NK_Sizeof,
  // control flow
  NK_For,
  NK_Control,
  NK_ForIncr,
  NK_If,
  NK_Condition,
  NK_Then,
  NK_Else,
  NK_ElseIf,
  NK_Case,
  NK_Default,
  NK_Switch,
  NK_While,
  NK_Do,
  NK_Break,
  NK_Continue,
  NK_Return,
  NK_Label,
  NK_Goto,
  // function
  NK_Call,
  NK_Function,
  NK_FuncDecl,
  NK_ArgList,
  NK_Arg,
  NK_ParamList,
  NK_Param,
  // structures
  NK_Typedef,
  NK_Struct,
  NK_StructDecl,
  NK_Union,
  NK_UnionDecl,
  NK_Enum,
  NK_Define,
  // conditional compile
  NK_CppIf,
  NK_CppElif,
  NK_IfDef,
  NK_IfnDef,
  NK_DefElse,
  NK_EndIf,
  NK_Include,
  NK_Pragma,
  NK_Directive,
  NK_Undef,
  // trival srcml staff
  NK_Comment,
  NK_Position,
  NK_Unit,
  NK_HeliumInstrument,
  NK_Null,
  NK_Other, // other name
  NK_Invalid // the node is invalid
} XMLNodeKind;

std::string xmlnode_kind_to_name(XMLNodeKind k);  
XMLNodeKind xmlnode_to_kind(XMLNode node);

const std::set<XMLNodeKind> helium_valid_ast {
  NK_Function,
    NK_DeclStmt,
    NK_ExprStmt,
    NK_For,
    // NK_Block,
    NK_Stmt,
    NK_If,
    NK_Switch,
    NK_While,
    NK_Do,
    NK_Break,
    NK_Continue,
    NK_Return,
    NK_Typedef,
    NK_Struct,
    NK_Union,
    NK_Enum,
    // NK_Comment,
    NK_Define
    };


// get only children that is node element intead of content
XMLNodeList element_children(XMLNode node);
XMLNode first_element_child(XMLNode node);
XMLNode next_element_sibling(XMLNode node);

/*******************************
 ** For specific type of node
 *******************************/

// get the filename field of this SrcML document belongs
std::string unit_get_filename(XMLNode node);

// TODO validate node
  
std::string function_get_return_type(XMLNode node);
std::string function_get_name(XMLNode node);
XMLNodeList function_get_params(XMLNode node);
XMLNode function_get_block(XMLNode node);

XMLNode get_function_node(XMLNode node);

XMLNodeList function_get_param_decls(XMLNode node);

std::string get_function_decl(std::string code);


// deprecated
std::string param_get_type(XMLNode node);
std::string param_get_name(XMLNode node);
// use this one
XMLNode param_get_decl(XMLNode node);
XMLNodeList block_get_nodes(XMLNode node);


XMLNodeList decl_stmt_get_decls(XMLNode node);
// FIXME srcml will give one <decl> for multiple variable, int a=0,b=8;
std::string decl_get_name(XMLNode node);
std::string decl_get_type(XMLNode node);

std::vector<std::string> decl_get_dimension(XMLNode node);


XMLNodeList for_get_init_decls_or_exprs(XMLNode node);
// std::map<std::string, std::string> for_get_init_detail(Node node);
XMLNode for_get_condition_expr(XMLNode node);
XMLNode for_get_incr_expr(XMLNode node);
XMLNode for_get_block(XMLNode node);
XMLNode for_get_control(XMLNode node);  
// if
XMLNode if_get_condition_expr(XMLNode node);
XMLNode if_get_then_block(XMLNode node);
XMLNode if_get_else_block(XMLNode node);
XMLNode if_get_then(XMLNode node);
XMLNode if_get_else(XMLNode node);
XMLNodeList if_get_elseifs(XMLNode node);

XMLNode then_get_block(XMLNode node);
XMLNode else_get_block(XMLNode node);
XMLNode elseif_get_block(XMLNode node);
// elseif IF related
XMLNode elseif_get_condition_expr(XMLNode node);
  

// switch
XMLNode switch_get_condition_expr(XMLNode);
XMLNodeList switch_get_cases(XMLNode);
XMLNode switch_get_default(XMLNode node);
XMLNode case_get_condition_expr(XMLNode node);

  
XMLNodeList case_get_nodes(XMLNode);
XMLNodeList switch_get_blocks(XMLNode node);
XMLNode switch_get_block(XMLNode node);
// NodeList switch_get_content(Node node);
// do
XMLNode do_get_condition_expr(XMLNode);
XMLNode do_get_condition(XMLNode node);
XMLNode do_get_block(XMLNode);

// while
XMLNode while_get_condition_expr(XMLNode);
XMLNode while_get_block(XMLNode);

// expr
// std::set<std::string> expr_stmt_get_ids(Node);
// std::set<std::string> expr_get_ids(Node);
std::set<std::string> expr_stmt_get_var_ids(XMLNode);
std::set<std::string> expr_get_var_ids(XMLNode);
std::set<std::string> decl_stmt_get_var_ids(XMLNode node);  
std::set<std::string> node_get_var_ids(XMLNode node);
// the above are all deprecated
std::set<std::string> get_var_ids(XMLNode node);
std::set<std::string> get_var_ids(XMLNodeList nodes);

std::set<std::string> get_type_ids(XMLNode node);
std::set<std::string> get_type_ids(XMLNodeList nodes);

std::set<std::string> get_call_ids(XMLNode node);
std::set<std::string> get_call_ids(XMLNodeList nodes);
  
std::string call_get_name(XMLNode);



std::set<std::string> get_used_names(XMLNode node);



#endif /* XMLNODE_H */
