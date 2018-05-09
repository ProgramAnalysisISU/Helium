#include "helium/parser/Visitor.h"
#include "helium/parser/AST.h"
#include "helium/parser/SourceManager.h"
#include "helium/utils/StringUtils.h"
#include <iostream>

using std::vector;
using std::string;
using std::map;
using std::set;



void ParentIndexer::pre(ASTNodeBase* node) {
  if (!Stack.empty()) {
    ParentMap[node] = Stack.back();
    ChildrenMap[Stack.back()].push_back(node);
  }
  Stack.push_back(node);
}
void ParentIndexer::post() {
  assert(!Stack.empty());
  Stack.pop_back();
}
// high level
void ParentIndexer::visit(TokenNode *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(TranslationUnitDecl *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(FunctionDecl *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(CompoundStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// condition
void ParentIndexer::visit(IfStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(SwitchStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(CaseStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(DefaultStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// loop
void ParentIndexer::visit(ForStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(WhileStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(DoStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// single
void ParentIndexer::visit(BreakStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(ContinueStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(ReturnStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// expr stmt
void ParentIndexer::visit(Expr *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(DeclStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void ParentIndexer::visit(ExprStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
