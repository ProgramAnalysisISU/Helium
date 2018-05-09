#include "helium/parser/Visitor.h"
#include "helium/parser/AST.h"
#include "helium/parser/SourceManager.h"
#include "helium/utils/StringUtils.h"
#include <iostream>

using std::vector;
using std::string;
using std::map;
using std::set;

/**
 * Printer
 */
void Printer::pre(ASTNodeBase *node) {
  // oss << "(" << node->getBeginLoc().getLine() << ":" << node->getBeginLoc().getColumn() << " "
  //     << node->getNodeName();
  oss << "(";
  // mark
  for (auto &mark : marks) {
    std::set<ASTNodeBase*> nodes = mark.first;
    std::string text = mark.second;
    if (nodes.count(node) == 1) {
      oss << text;
    }
  }
  node->dump(oss);
  // oss << " " << (void*)node;
  // oss << " use: ";
  // for (std::string var : node->getUsedVars()) {
  //   oss << var << " ";
  // }
}
void Printer::post() {oss << ")";}


void Printer::visit(TokenNode *node) {
  pre(node);
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(TranslationUnitDecl *node) {
  pre(node);
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(FunctionDecl *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(IfStmt *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(SwitchStmt *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(CaseStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(DefaultStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(ForStmt *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(WhileStmt *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(DoStmt *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(CompoundStmt *node) {
  oss<<"\n";pre(node);oss<<"\n";
  Visitor::visit(node);
  post();oss<<"\n";
}
void Printer::visit(BreakStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void Printer::visit(ContinueStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void Printer::visit(ReturnStmt *node) {
  pre(node);
  Visitor::visit(node);
  post();
}
void Printer::visit(Expr *node) {
  oss << "\n"; pre(node);
  Visitor::visit(node);
  post(); oss << "\n";
}
void Printer::visit(DeclStmt *node) {
  oss << "\n"; pre(node);
  Visitor::visit(node);
  post(); oss << "\n";
}
void Printer::visit(ExprStmt *node) {
  oss << "\n"; pre(node);
  Visitor::visit(node);
  post(); oss << "\n";
}
