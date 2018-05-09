#include "helium/parser/Visitor.h"
#include "helium/parser/AST.h"
#include "helium/parser/SourceManager.h"
#include "helium/utils/StringUtils.h"
#include <iostream>

using std::vector;
using std::string;
using std::map;
using std::set;



void Distributor::pre(ASTNodeBase* node) {
  // for all the things in stack
  // push contain map
  for (ASTNodeBase* n : Stack) {
    ContainMap[n].insert(node);
  }
  Stack.push_back(node);
}

void Distributor::post() {
  Stack.pop_back();
}

// high level
void Distributor::visit(TokenNode *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(TranslationUnitDecl *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(FunctionDecl *node){
  func_nodes.insert(node);
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(CompoundStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
// condition
void Distributor::visit(IfStmt *node){
  if_nodes.insert(node);
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(SwitchStmt *node){
  switch_nodes.insert(node);
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(CaseStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(DefaultStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
// loop
void Distributor::visit(ForStmt *node){
  for_nodes.insert(node);
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(WhileStmt *node){
  while_nodes.insert(node);
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(DoStmt *node){
  do_nodes.insert(node);
  pre(node);
  Visitor::visit(node);
  post();
}
// single
void Distributor::visit(BreakStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(ContinueStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(ReturnStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
// expr stmt
void Distributor::visit(Expr *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(DeclStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}
void Distributor::visit(ExprStmt *node){
  pre(node);
  Visitor::visit(node);
  post();
}






void Distributor::dump(std::ostream &os) {
  os << "if nodes: ";
  for (auto *node : if_nodes) {
    node->dump(os);
  }
  os << "\n";
  os << "switch nodes: ";
  for (auto *node : switch_nodes) {
    node->dump(os);
  }
  os << "\n";
  os << "for nodes: ";
  for (auto *node : for_nodes) {
    node->dump(os);
  }
  os << "\n";
  os << "do nodes:";
  for (auto *node : do_nodes) {
    node->dump(os);
  }
  os << "\n";
  for (auto *node : while_nodes) {
    node->dump(os);
  }
  os << "\n";
  os << "func nodes: ";
  for (auto *node : func_nodes) {
    node->dump(os);
  }
  os << "\n";


  // containmap
  // std::map<ASTNodeBase*, std::set<ASTNodeBase*> > ContainMap;
  os << "Contain Map: " << "\n";
  for (auto &m : ContainMap) {
    os << "\t";
    m.first->dump(os);
    os << " ==> ";
    for (auto *node : m.second) {
      node->dump(os);
    }
    os << "\n";
  }
}
