#ifndef NEWGENERATOR_H
#define NEWGENERATOR_H

#include "Visitor.h"


class Instrumentor : public Visitor {
public:
  Instrumentor() {}
  virtual ~Instrumentor() {}
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

  void setSelection(std::set<ASTNodeBase*> sel) {
    m_sel = sel;
  }

  void pre(ASTNodeBase *node);

  std::map<ASTNodeBase*, std::string> getSpecPre() {
    return m_spec_pre;
  }
  std::map<ASTNodeBase*, std::string> getSpecPost() {
    return m_spec_post;
  }
  
private:
  std::set<ASTNodeBase*> m_sel;
  std::map<ASTNodeBase*, std::string> m_spec_pre;
  std::map<ASTNodeBase*, std::string> m_spec_post;
  ASTNodeBase *m_last = nullptr;
};

/**
 * Code Generator
 */
class Generator : public Visitor {
public:
  Generator() {}
  virtual ~Generator() {}
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


  void setSelection(std::set<ASTNodeBase*> sel);
  void setSpecPre(std::map<ASTNodeBase*, std::string> pre) {
    m_spec_pre = pre;
  }
  void setSpecPost(std::map<ASTNodeBase*, std::string> post) {
    m_spec_post = post;
  }

  // TODO outer interface
  std::string getProgram(ASTNodeBase *node) {
    if (m_Node2ProgMap.count(node) == 1) {
      return m_Node2ProgMap[node];
    }
    return "";
  }

  // TODO inner interface
  void addInnerProg(ASTNodeBase *node, std::string prog) {
    m_Node2ProgMap[node] = prog;
  }
  std::string getInnerProg(ASTNodeBase *node) {
    if (m_Node2ProgMap.count(node) == 1) {
      return m_Node2ProgMap[node];
    }
    return "";
  }
  std::set<std::string> getEntryFuncs() {return m_entry_funcs;}

private:
  std::set<ASTNodeBase*> m_sel;
  std::map<ASTNodeBase*,std::string> m_Node2ProgMap;
  std::set<std::string> m_entry_funcs;
  std::map<ASTNodeBase*, std::string> m_spec_pre;
  std::map<ASTNodeBase*, std::string> m_spec_post;
};



#endif /* NEWGENERATOR_H */
