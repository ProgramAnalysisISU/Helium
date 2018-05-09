#include "helium/parser/Visitor.h"
#include "helium/parser/AST.h"
#include "helium/parser/SourceManager.h"
#include "helium/utils/StringUtils.h"
#include <iostream>

using std::vector;
using std::string;
using std::map;
using std::set;




// high level
void TokenVisitor::visit(TokenNode *node) {
  IdMap[node] = id;
  Tokens.push_back(node);
  id++;
  Visitor::visit(node);
}
void TokenVisitor::visit(TranslationUnitDecl *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(FunctionDecl *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(CompoundStmt *node) {
  Visitor::visit(node);
}
// condition
void TokenVisitor::visit(IfStmt *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(SwitchStmt *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(CaseStmt *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(DefaultStmt *node) {
  Visitor::visit(node);
}
// loop
void TokenVisitor::visit(ForStmt *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(WhileStmt *node) {
  Visitor::visit(node);
}
void TokenVisitor::visit(DoStmt *node) {
  Visitor::visit(node);
}
// single
void TokenVisitor::visit(BreakStmt *node) {
  IdMap[node] = id;
  Tokens.push_back(node);
  id++;
  Visitor::visit(node);
}
void TokenVisitor::visit(ContinueStmt *node) {
  IdMap[node] = id;
  Tokens.push_back(node);
  id++;
  Visitor::visit(node);
}
void TokenVisitor::visit(ReturnStmt *node) {
  Visitor::visit(node);
}
// expr stmt
void TokenVisitor::visit(Expr *node) {
  IdMap[node] = id;
  Tokens.push_back(node);
  id++;
  Visitor::visit(node);
}
void TokenVisitor::visit(DeclStmt *node) {
  IdMap[node] = id;
  Tokens.push_back(node);
  id++;
  Visitor::visit(node);
}
void TokenVisitor::visit(ExprStmt *node) {
  IdMap[node] = id;
  Tokens.push_back(node);
  id++;
  Visitor::visit(node);
}
