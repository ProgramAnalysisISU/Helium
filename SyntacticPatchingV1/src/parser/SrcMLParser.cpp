#include "helium/parser/Parser.h"
#include "helium/utils/XMLDocReader.h"
#include "helium/parser/AST.h"

#include "helium/utils/XMLNodeHelper.h"
#include "helium/utils/StringUtils.h"

#include "helium/utils/Exception.h"


using std::vector;


// rename this file to srcmlparser.cc

ASTContext *SrcMLParser::parse(fs::path file) {
  XMLDoc *doc = XMLDocReader::CreateDocFromFile(file.string());
  XMLNode root = doc->document_element();
  ASTContext *ctx = new ASTContext(file.string());
  ctx->setTranslationUnitDecl(ParseTranslationUnitDecl(ctx, root));
  return ctx;
}

TranslationUnitDecl *SrcMLParser::ParseTranslationUnitDecl(ASTContext *ctx, XMLNode unit) {
  if (!unit) {
    throw HeliumException("No <unit> element.");
  }
  match(unit, "unit");
  // std::vector<DeclStmt*> decls;
  // std::vector<FunctionDecl*> funcs;
  vector<ASTNodeBase*> decls;
  for (XMLNode node : unit.children()) {
    if (std::string(node.name()) == "decl_stmt") {
      DeclStmt *decl = ParseDeclStmt(ctx, node);
      decls.push_back(decl);
    } else if (std::string(node.name()) == "function") {
      FunctionDecl *func = ParseFunctionDecl(ctx, node);
      // funcs.push_back(func);
      decls.push_back(func);
    }
  }

  // get the source location of unit
  std::pair<int, int> begin = get_node_begin_position(unit);
  std::pair<int, int> end = get_node_end_position(unit);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  // std::cout << decls.size() << "\n";
  return new TranslationUnitDecl(ctx, decls, BeginLoc, EndLoc);
}

DeclStmt *SrcMLParser::ParseDeclStmt(ASTContext *ctx, XMLNode decl) {
  std::string text = get_text(decl);
  std::pair<int, int> begin = get_node_begin_position(decl);
  std::pair<int, int> end = get_node_end_position(decl);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);

  // get params
  DeclStmt *ret = new DeclStmt(ctx, text, BeginLoc, EndLoc);

  std::set<std::string> params;
  for (XMLNode n : decl.children("decl")) {
    std::string name = decl_get_name(n);
    std::string type = decl_get_type(n);
    params.insert(name);
    ret->addFullVar(name, type);
  }
  ret->addDefinedVar(params);
  // ret->addUsedVars(get_var_ids(decl));
  // ret->addUsedVars(get_used_names(decl));
  // FIXME It is very hard to get the used variables. I need a precise parser
  // Now I'm using everything
  ret->addUsedVar(utils::extract_id_to_resolve(get_text(decl)));
  return ret;
}

/**
 * Make a TokenNode out of an XMLNode
 */
TokenNode *make_token_node(ASTContext *ctx, XMLNode node, std::string text) {
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_begin_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  return new TokenNode(ctx, text, BeginLoc, EndLoc);
}

/**
 * Make a token node from begin_node to end_node, including both
 */
TokenNode *make_token_node(ASTContext *ctx, XMLNode begin_node, XMLNode end_node, std::string text) {
  std::pair<int, int> begin = get_node_begin_position(begin_node);
  std::pair<int, int> end = get_node_begin_position(end_node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  return new TokenNode(ctx, text, BeginLoc, EndLoc);
}

FunctionDecl *SrcMLParser::ParseFunctionDecl(ASTContext *ctx, XMLNode node) {
  // constructnig children
  XMLNode block = node.child("block");
  if (!block) {
    throw HeliumException("Function Does not have body.");
  }
  CompoundStmt *comp = ParseCompoundStmt(ctx, block);
  std::string name = function_get_name(node);
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);

  // TokenNodes
  // FIXME this might have static specifier
  // <function>
  //     <specifier>static</specifier>
  // <type>
  //   <name>_Bool</name>
  // </type>

  TokenNode *ReturnTypeNode = nullptr;
  if (!node.child("type")) {
    std::cerr << "[Helium Fatal Error] Function " << name
              << " does not have a return type. Exception Throwing." << "\n";
    throw HeliumException("Function does not have a return type");
  }
  if (node.child("specifier")) {
    ReturnTypeNode = make_token_node(ctx, node.child("specifier"), node.child("type"),
                                     get_text(node.child("specifier")) + " " + get_text(node.child("type")));
  } else {
    // abhishekkr--n00bRAT main function does not have type!!! WTF
    ReturnTypeNode = make_token_node(ctx, node.child("type"), get_text(node.child("type")));
  }
  
  TokenNode *NameNode = make_token_node(ctx, node.child("name"), get_text(node.child("name")));
  // caution: this contains parenthesis
  TokenNode *ParamNode = make_token_node(ctx, node.child("parameter_list"),
                                         get_text(node.child("parameter_list")));


  FunctionDecl *ret = new FunctionDecl(ctx, name, ReturnTypeNode, NameNode, ParamNode, comp, BeginLoc, EndLoc);
  // set params
  XMLNodeList params = function_get_param_decls(node);
  std::set<std::string> vars;
  for (XMLNode param : params) {
    std::string name = decl_get_name(param);
    vars.insert(name);
  }
  ret->addDefinedVar(vars);
  
  return ret;
}

CompoundStmt *SrcMLParser::ParseCompoundStmt(ASTContext *ctx, XMLNode node) {
  match(node, "block");
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  // COMP_DUMMY
  // TokenNode *CompNode = new TokenNode(ctx, "", BeginLoc, BeginLoc + std::make_pair(0,1));
  TokenNode *lbrace = new TokenNode(ctx, "{", BeginLoc, BeginLoc+std::make_pair(0,1));
  TokenNode *rbrace= new TokenNode(ctx, "}", EndLoc+std::make_pair(0,-1), EndLoc);
  CompoundStmt *ret = new CompoundStmt(ctx, lbrace, rbrace, BeginLoc, EndLoc);
  
  for (XMLNode n : element_children(node)) {
    std::string NodeName = n.name();
    Stmt *stmt = ParseStmt(ctx, n);
    if (stmt) {
      ret->Add(stmt);
    }
  }
  return ret;
}

/**
 * Parse all kinds of statements here
 * FIXME this is very error-prone!
 * I Need to list all the types of statements, otherwise it will not appear in AST
 */
Stmt *SrcMLParser::ParseStmt(ASTContext *ctx, XMLNode node) {
  std::string NodeName = node.name();
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  Stmt *ret = nullptr;
  if (NodeName == "decl_stmt") {
    ret = ParseDeclStmt(ctx, node);
  } else if (NodeName == "expr_stmt") {
    ret = ParseExprStmt(ctx, node);
  } else if (NodeName == "block") {
    ret = ParseCompoundStmt(ctx, node);
  } else if (NodeName == "stmt") {
    // FIXME srcml does not only contain these two
    // e.g. it might directly be a if statement
    // Stmt *stmt = ParseStmt(node);
    // FIXME no such tag in srmcl? will cause recursion
    // error();
    assert(0);
  } else if (NodeName == "if") {
    ret = ParseIfStmt(ctx, node);
  } else if (NodeName == "switch") {
    ret = ParseSwitchStmt(ctx, node);
  } else if (NodeName == "for") {
    ret = ParseForStmt(ctx, node);
  } else if (NodeName == "do") {
    ret = ParseDoStmt(ctx, node);
  } else if (NodeName == "while") {
    ret = ParseWhileStmt(ctx, node);
  }
  else if (NodeName == "break") {
    ret = new BreakStmt(ctx, BeginLoc, EndLoc);
  } else if (NodeName == "continue") {
    ret = new ContinueStmt(ctx, BeginLoc, EndLoc);
  } else if (NodeName == "return") {
    ret = ParseReturnStmt(ctx, node);
  }
  return ret;
}

ReturnStmt *SrcMLParser::ParseReturnStmt(ASTContext *ctx, XMLNode node) {
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  SourceLocation TokenBeginLoc(begin.first, begin.second);
  SourceLocation TokenEndLoc(begin.first, begin.second + strlen("return"));
  TokenNode *ReturnNode = new TokenNode(ctx, "return", TokenBeginLoc, TokenEndLoc);

  Expr *value = nullptr;
  if (node.child("expr")) {
    value = ParseExpr(ctx, node.child("expr"));
  }
  ReturnStmt *ret = new ReturnStmt(ctx, ReturnNode, value, BeginLoc, EndLoc);
  return ret;
}

Stmt *SrcMLParser::ParseExprStmt(ASTContext *ctx, XMLNode node) {
  match(node, "expr_stmt");
  std::string text = get_text(node);
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  Stmt *ret = new ExprStmt(ctx, text, BeginLoc, EndLoc);
  // ret->addUsedVars(get_var_ids(node));
  // ret->addUsedVars(get_used_names(node));
  ret->addUsedVar(utils::extract_id_to_resolve(get_text(node)));
  return ret;
}


IfStmt *SrcMLParser::ParseIfStmt(ASTContext *ctx, XMLNode node) {
  match(node, "if");
  XMLNode cond = if_get_condition_expr(node);
  Expr *condexpr = ParseExpr(ctx, cond);
  XMLNode then_node = if_get_then(node);
  assert(then_node);
  Stmt *thenstmt = nullptr;
  Stmt *elsestmt = nullptr;
  if (then_node) {
    thenstmt = ParseCompoundStmt(ctx, then_node.child("block"));
  } else {
    std::cerr << "Error: no else for if." << "\n";
    exit(1);
  }
  XMLNode elseifnode = node.child("elseif");
  if (elseifnode) {
    elsestmt = ParseElseIfAsIfStmt(ctx, elseifnode);
  }
  XMLNode elsenode = node.child("else");
  if (elsenode) {
    elsestmt = ParseCompoundStmt(ctx, elsenode.child("block"));
  }
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);

  // TokenNode
  std::pair<int, int> if_begin = get_node_begin_position(node);
  SourceLocation IfNodeBegin(if_begin.first, if_begin.second);
  SourceLocation IfNodeEnd(if_begin.first, if_begin.second + strlen("if"));
  TokenNode *IfNode = new TokenNode(ctx, "if", IfNodeBegin, IfNodeEnd);
  
  TokenNode *ElseNode = nullptr;
  if (elsestmt) {
    // this might be <else> or <elseif>
    std::pair<int,int> else_begin;
    if (elsenode) else_begin = get_node_begin_position(elsenode);
    else if (elseifnode) else_begin = get_node_begin_position(elseifnode);
    SourceLocation ElseNodeBegin(else_begin.first, else_begin.second);
    SourceLocation ElseNodeEnd(else_begin.first, else_begin.second + strlen("else"));
    ElseNode = new TokenNode(ctx, "else", ElseNodeBegin, ElseNodeEnd);
  }

  IfStmt *ret = new IfStmt(ctx, condexpr, thenstmt, elsestmt, IfNode, ElseNode, BeginLoc, EndLoc);
  return ret;
}

/**
 * <if><then></then>
 *     <elseif><if></if></elseif>
 *     <elseif></elseif>
 *     <else></else>
 * </if>
 *
 * Into
 *
 * (if (then)
 *     (if (then)
 *         (if (then)
 *         (else))))
 */
IfStmt *SrcMLParser::ParseElseIfAsIfStmt(ASTContext *ctx, XMLNode node) {
  match(node, "elseif");
  IfStmt *ifstmt = ParseIfStmt(ctx, node.child("if"));
  // XMLNode next = node.next_sibling();
  XMLNode next = next_element_sibling(node);
  if (next) {
    Stmt *elsestmt = nullptr;
    TokenNode *ElseNode = nullptr;
    std::pair<int,int> else_begin = get_node_begin_position(next);
    SourceLocation ElseNodeBegin(else_begin.first, else_begin.second);
    SourceLocation ElseNodeEnd(else_begin.first, else_begin.second + strlen("else"));
    ElseNode = new TokenNode(ctx, "else", ElseNodeBegin, ElseNodeEnd);
    if (std::string(next.name()) == "elseif") {
      elsestmt = ParseElseIfAsIfStmt(ctx, next);
    } else if (std::string(next.name()) == "else") {
      elsestmt = ParseCompoundStmt(ctx, next.child("block"));
    } else {
      // error();
      // std::cout << next.name() << "\n";
      assert(0);
    }
    // should also set elsenode
    ifstmt->setElse(ElseNode, elsestmt);
  }
  return ifstmt;
}


SwitchStmt *SrcMLParser::ParseSwitchStmt(ASTContext *ctx, XMLNode node) {
  match(node, "switch");
  Expr *cond = ParseExpr(ctx, node.child("condition").child("expr"));
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);

  // SwitchNode
  SourceLocation SwitchNodeBegin(begin.first, begin.second);
  SourceLocation SwitchNodeEnd(begin.first, begin.second + strlen("switch"));
  TokenNode *SwitchNode = new TokenNode(ctx, "switch", SwitchNodeBegin, SwitchNodeEnd);
  
  SwitchStmt *ret = new SwitchStmt(ctx, cond, NULL, SwitchNode, BeginLoc, EndLoc);
  for(XMLNode c : node.child("block").children("case")) {
    CaseStmt *casestmt = ParseCaseStmt(ctx, c);
    ret->AddCase(casestmt);
  }
  XMLNode defnode = node.child("block").child("default");
  if (defnode) {
    ret->AddCase(ParseDefaultStmt(ctx, defnode));
  }
  return ret;
}

/**
 * <case><expr></expr></caes>
 * <expr_stmt>
 * <break>
 * <case>
 * <default>
 */
CaseStmt *SrcMLParser::ParseCaseStmt(ASTContext *ctx, XMLNode node) {
  Expr *cond = ParseExpr(ctx, node.child("expr"));
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  // CaseNode
  SourceLocation CaseNodeBegin(begin.first, begin.second);
  SourceLocation CaseNodeEnd(begin.first, begin.second = strlen("case"));
  TokenNode *CaseNode = new TokenNode(ctx, "case", CaseNodeBegin, CaseNodeEnd);
  CaseStmt *ret = new CaseStmt(ctx, cond, CaseNode, BeginLoc, EndLoc);
  XMLNode n = node;
  while ((n = next_element_sibling(n))) {
    if (std::string(n.name()) == "case" ||
        std::string(n.name()) == "default") {
      break;
    }
    Stmt *stmt = ParseStmt(ctx, n);
    if (stmt) {
      ret->Add(stmt);
    }
  }
  return ret;
}
DefaultStmt *SrcMLParser::ParseDefaultStmt(ASTContext *ctx, XMLNode node) {
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  // DefaultNode
  SourceLocation DefaultNodeBegin(begin.first, begin.second);
  SourceLocation DefaultNodeEnd(begin.first, begin.second + strlen("default"));
  TokenNode *DefaultNode = new TokenNode(ctx, "default", DefaultNodeBegin, DefaultNodeEnd);
  
  DefaultStmt *ret = new DefaultStmt(ctx, DefaultNode, BeginLoc, EndLoc);
  XMLNode n = node;
  while ((n = next_element_sibling(n))) {
    if (std::string(n.name()) == "case" ||
        std::string(n.name()) == "default") {
      break;
    }
    Stmt *stmt = ParseStmt(ctx, n);
    if (stmt) {
      ret->Add(stmt);
    }
  }
  return ret;
}


ForStmt *SrcMLParser::ParseForStmt(ASTContext *ctx, XMLNode node) {
  match(node, "for");
  Expr *init = ParseExprWithoutSemicolon(ctx, node.child("control").child("init"));
  // decls
  std::set<std::string> vars;
  XMLNodeList inits = for_get_init_decls_or_exprs(node);
  for (XMLNode init : inits) {
    std::string nodename = init.name();
    if (nodename == "decl") {
      std::string name = decl_get_name(init);
      vars.insert(name);
    }
  }
  // I should set the declaration of vars preciesly to for init expr
  // ret->setVars(vars);
  init->addDefinedVar(vars);

  
  Expr *cond = ParseExprWithoutSemicolon(ctx, node.child("control").child("condition"));
  Expr *inc = ParseExpr(ctx, node.child("control").child("incr"));
  Stmt *block = ParseCompoundStmt(ctx, node.child("block"));
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);

  // make up for for token
  SourceLocation TokenBeginLoc(begin.first, begin.second);
  SourceLocation TokenEndLoc(begin.first, begin.second + strlen("for"));
  TokenNode *ForNode = new TokenNode(ctx, "for", TokenBeginLoc, TokenEndLoc);

  ForStmt *ret = new ForStmt(ctx, init, cond, inc, block, ForNode, BeginLoc, EndLoc);
  
  return ret;
}

WhileStmt *SrcMLParser::ParseWhileStmt(ASTContext *ctx, XMLNode node) {
  match(node, "while");
  Expr *cond = ParseExpr(ctx, while_get_condition_expr(node));
  Stmt *body = ParseCompoundStmt(ctx, while_get_block(node));
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  // WhileNode
  SourceLocation TokenBeginLoc(begin.first, begin.second);
  SourceLocation TokenEndLoc(begin.first, begin.second + strlen("while"));
  TokenNode *WhileNode = new TokenNode(ctx, "while", TokenBeginLoc, TokenEndLoc);
  WhileStmt *ret = new WhileStmt(ctx, cond, body, WhileNode, BeginLoc, EndLoc);
  return ret;
}


DoStmt *SrcMLParser::ParseDoStmt(ASTContext *ctx, XMLNode node) {
  match(node, "do");
  // this might be empty??
  Expr *cond = ParseExpr(ctx, node.child("condition").child("expr"));
  Stmt *block = ParseCompoundStmt(ctx, node.child("block"));
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);

  // DoNode
  SourceLocation TokenBeginLoc(begin.first, begin.second);
  SourceLocation TokenEndLoc(begin.first, begin.second + strlen("do"));
  TokenNode *DoNode = new TokenNode(ctx, "do", TokenBeginLoc, TokenEndLoc);

  // WhileNode: this is not precise
  XMLNode blockNode = do_get_block(node);
  XMLNode condNode = do_get_condition(node);
  if (!condNode) {
    std::cerr << "[Helium Fatal Error] Do statement does not have a condition. Throwing Exception." << "\n";
    throw HeliumException("Do stmt does not have condition.");
  }
  std::pair<int, int> block_end = get_node_end_position(blockNode);
  SourceLocation WhileBeginLoc(block_end.first, block_end.second);
  std::pair<int, int> cond_begin = get_node_begin_position(condNode);
  SourceLocation WhileEndLoc(cond_begin.first, cond_begin.second);
  TokenNode *WhileNode = new TokenNode(ctx, "while", WhileBeginLoc, WhileEndLoc);
  
  DoStmt *ret = new DoStmt(ctx, cond, block, DoNode, WhileNode, BeginLoc, EndLoc);
  return ret;
}

Expr *SrcMLParser::ParseExpr(ASTContext *ctx, XMLNode node) {
  // FIXME expr might not with <expr>??
  // match(node, "expr");

  if (!node) return nullptr;

  // this might be many thing
  // I'm going to handle here
  // I'm not going to check the naem
  // I assume the node passed in is going to be a expression
  // so i just get the text.
  // For example, it might be <init> <condition> <inc>, etc
  // It might have semi-colon included
  
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  Expr *ret = new Expr(ctx, get_text(node), BeginLoc, EndLoc);
  // ret->addUsedVars(get_var_ids(node));
  // ret->addUsedVars(get_used_names(node));
  ret->addUsedVar(utils::extract_id_to_resolve(get_text(node)));
  return ret;
}
Expr *SrcMLParser::ParseExprWithoutSemicolon(ASTContext *ctx, XMLNode node) {
  std::pair<int, int> begin = get_node_begin_position(node);
  std::pair<int, int> end = get_node_end_position(node);
  SourceLocation BeginLoc(begin.first, begin.second);
  SourceLocation EndLoc(end.first, end.second);
  // remove semicolon from text
  std::string text = get_text(node);
  utils::trim(text);
  // FIXME error if no semicolon
  if (text.back() == ';') text.pop_back();
  Expr *ret = new Expr(ctx, text, BeginLoc, EndLoc);
  // ret->addUsedVars(get_var_ids(node));
  // ret->addUsedVars(get_used_names(node));
  ret->addUsedVar(utils::extract_id_to_resolve(get_text(node)));
  return ret;
}


void SrcMLParser::match(XMLNode node, std::string tag) {
  // std::cout << "parser cannot match: " << tag << " vs " << node.name() << "\n";
  assert(tag == node.name());
}
