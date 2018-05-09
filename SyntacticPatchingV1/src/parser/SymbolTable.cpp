#include "helium/parser/Visitor.h"
#include "helium/parser/SymbolTable.h"
#include "helium/parser/AST.h"
#include "helium/parser/SourceManager.h"
#include "helium/utils/StringUtils.h"
#include <iostream>

using std::vector;
using std::string;
using std::map;
using std::set;



// high level
void SymbolTableBuilder::visit(TokenNode *node) {
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(TranslationUnitDecl *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
void SymbolTableBuilder::visit(FunctionDecl *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
void SymbolTableBuilder::visit(CompoundStmt *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
// condition
void SymbolTableBuilder::visit(IfStmt *node) {
  // it is the then and else COMPOUND Statement that creates scope
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(SwitchStmt *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
void SymbolTableBuilder::visit(CaseStmt *node) {
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(DefaultStmt *node) {
  process(node);
  Visitor::visit(node);
}
// loop
void SymbolTableBuilder::visit(ForStmt *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
void SymbolTableBuilder::visit(WhileStmt *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
void SymbolTableBuilder::visit(DoStmt *node) {
  tbl->pushScope();
  process(node);
  Visitor::visit(node);
  tbl->popScope();
}
// single
void SymbolTableBuilder::visit(BreakStmt *node) {
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(ContinueStmt *node) {
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(ReturnStmt *node) {
  process(node);
  Visitor::visit(node);
}
// expr stmt
void SymbolTableBuilder::visit(Expr *node) {
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(DeclStmt *node) {
  process(node);
  Visitor::visit(node);
}
void SymbolTableBuilder::visit(ExprStmt *node) {
  process(node);
  Visitor::visit(node);
}



void SymbolTableBuilder::process(ASTNodeBase *node) {
  std::set<std::string> defined_vars = node->getDefinedVars();
  for (std::string var : defined_vars) {
    std::string type = node->getDefinedVarType(var); 
    tbl->add(var, type, node);
  }
  tbl->bindNode(node);
}

void SymbolTableEntry::dump(std::ostream &os) {
  os << "(lambda " << (void*)this << "\n";
  for (auto &mi : m) {
    std::string name = mi.first;
    std::string type = mi.second.first;
    ASTNodeBase *node = mi.second.second;
    os << "(" << name << " " << type << " " << (void*)node << ")\n";
  }
  for (SymbolTableEntry *child : children) {
    child->dump(os);
  }
  os << ")\n";
}

void SymbolTable::dump(std::ostream &os) {
  os << "Dumping SymbolTable .." << "\n";
  std::ostringstream oss;
  root->dump(oss);
  os << utils::lisp_pretty_print(oss.str());
}
