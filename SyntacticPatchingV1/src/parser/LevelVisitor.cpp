#include "helium/parser/Visitor.h"
#include "helium/parser/AST.h"
#include "helium/parser/SourceManager.h"
#include "helium/utils/StringUtils.h"
#include <iostream>

#include <gtest/gtest.h>

using std::vector;
using std::string;
using std::map;
using std::set;






// level visitor
void LevelVisitor::visit(TokenNode *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(TranslationUnitDecl *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(FunctionDecl *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(CompoundStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// condition
void LevelVisitor::visit(IfStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(SwitchStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(CaseStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(DefaultStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// loop
void LevelVisitor::visit(ForStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(WhileStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(DoStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// single
void LevelVisitor::visit(BreakStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(ContinueStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(ReturnStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
// expr stmt
void LevelVisitor::visit(Expr *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(DeclStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void LevelVisitor::visit(ExprStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}


int LevelVisitor::getLevel(ASTNodeBase *node) {
  if (Levels.count(node) == 1) {
    return Levels[node];
  } else {
    return -1;
  }
}


ASTNodeBase *LevelVisitor::getLowestLevelNode(std::set<ASTNodeBase*> nodes) {
  int retlvl=-1;
  ASTNodeBase *ret = nullptr;
  for (auto *node : nodes) {
    int lvl = getLevel(node);
    if (retlvl < lvl) {
      retlvl = lvl;
      ret = node;
    }
  }
  return ret;
}
