#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include "Visitor.h"

class SymbolTableEntry {
public:
  SymbolTableEntry(SymbolTableEntry *parent) : parent(parent) {}
  ~SymbolTableEntry() {}
  SymbolTableEntry *getParent() {return parent;}
  void add(std::string name, std::string type, ASTNodeBase *node) {
    m[name] = {type, node};
  }
  ASTNodeBase *getNode(std::string name) {
    if (m.count(name) == 1) return m[name].second;
    return nullptr;
  }
  ASTNodeBase *getNodeRecursive(std::string name) {
    if (m.count(name) == 1) return m[name].second;
    if (parent) {
      return parent->getNodeRecursive(name);
    }
    return nullptr;
  }
  std::string getType(std::string name) {
    if (m.count(name) == 1) return m[name].first;
    return "";
  }
  std::string getTypeRecursive(std::string name) {
    if (m.count(name) == 1) return m[name].first;
    if (parent) {
      return parent->getTypeRecursive(name);
    }
    return "";
  }
  std::set<std::string> getAllVars() {
    std::set<std::string> ret;
    for (auto &mi : m) {
      ret.insert(mi.first);
    }
    return ret;
  }
  std::set<std::string> getAllVarsRecursive() {
    std::set<std::string> ret;
    for (auto &mi : m) {
      ret.insert(mi.first);
    }
    if (parent) {
      std::set<std::string> par = parent->getAllVarsRecursive();
      ret.insert(par.begin(), par.end());
    }
    return ret;
  }
  std::vector<SymbolTableEntry*> getChildren() {return children;}
  void addChild(SymbolTableEntry *entry) {
    children.push_back(entry);
  }
  void dump(std::ostream &os);
private:
  SymbolTableEntry *parent = nullptr;
  std::vector<SymbolTableEntry*> children;
  std::map<std::string, std::pair<std::string, ASTNodeBase*>> m;
};

class SymbolTable {
public:
  SymbolTable() {
    root = new SymbolTableEntry(nullptr);
    current = root;
  }
  ~SymbolTable() {}

  void pushScope() {
    SymbolTableEntry *tbl = new SymbolTableEntry(current);
    current->addChild(tbl);
    current = tbl;
  }
  void popScope() {
    current = current->getParent();
  }
  SymbolTableEntry *getCurrent() {return current;}

  void add(std::string name, std::string type, ASTNodeBase *node) {
    current->add(name, type, node);
  }
  void bindNode(ASTNodeBase *node) {
    node2entry[node] = current;
  }
  SymbolTableEntry *getEntry(ASTNodeBase *node) {
    if (node2entry.count(node) == 1) {
      return node2entry[node];
    }
    return nullptr;
  }
  void dump(std::ostream &os);
private:
  std::map<ASTNodeBase*, SymbolTableEntry*> node2entry;
  SymbolTableEntry *current = nullptr;
  SymbolTableEntry *root = nullptr;
};

/**
 * The symbol table should be easy to access for each node.
 * So I might consider to
 * - create the symbol table right after I create the AST
 * - store the symbol table with the AST nodes
 */
class SymbolTableBuilder : public Visitor {
public:
  SymbolTableBuilder() {
    tbl = new SymbolTable();
  }
  virtual ~SymbolTableBuilder() {}
  // high level
  virtual void visit(TokenNode *node);
  virtual void visit(TranslationUnitDecl *node);
  virtual void visit(FunctionDecl *node);
  virtual void visit(CompoundStmt *node);
  // condition
  virtual void visit(IfStmt *node);
  virtual void visit(SwitchStmt *node);
  virtual void visit(CaseStmt *node);
  virtual void visit(DefaultStmt *node);
  // loop
  virtual void visit(ForStmt *node);
  virtual void visit(WhileStmt *node);
  virtual void visit(DoStmt *node);
  // single
  virtual void visit(BreakStmt *node);
  virtual void visit(ContinueStmt *node);
  virtual void visit(ReturnStmt *node);
  // expr stmt
  virtual void visit(Expr *node);
  virtual void visit(DeclStmt *node);
  virtual void visit(ExprStmt *node);
  SymbolTable *getSymbolTable() {return tbl;}
private:
  void process(ASTNodeBase *use);
  // this will be empty after traversal
  SymbolTable *tbl = nullptr;
};


#endif /* SYMBOLTABLE_H */
