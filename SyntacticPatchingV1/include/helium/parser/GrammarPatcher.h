#ifndef GRAMMARPATCHER_H
#define GRAMMARPATCHER_H
#include <vector>
#include <map>
#include <set>
#include <sstream>
#include "helium/parser/Visitor.h"

class ASTContext;
  
class ASTNodeBase;
class TokenNode;
class TranslationUnitDecl;
class FunctionDecl;
class Decl;
class Stmt;
class DeclStmt;
class ExprStmt;
class CompoundStmt;
class ForStmt;
class WhileStmt;
class DoStmt;
class BreakStmt;
class ContinueStmt;
class ReturnStmt;
class IfStmt;
class SwitchStmt;
class SwitchCase;
class CaseStmt;
class DefaultStmt;
class Expr;


class StandAloneGrammarPatcher {
public:
  StandAloneGrammarPatcher(ASTContext *ast, std::set<ASTNodeBase*> selection)
    : AST(ast), Selection(selection) {}
  ~StandAloneGrammarPatcher() {}
  void process();
  void matchMin(ASTNodeBase *parent, std::set<ASTNodeBase*> sel);
  std::set<ASTNodeBase*> getPatch() {return Patch;}

  bool validAlone(ASTNodeBase* node);
  
private:
  ASTContext *AST = nullptr;
  std::set<ASTNodeBase*> Selection;
  std::set<ASTNodeBase*> Worklist;
  std::set<ASTNodeBase*> Patch;
  std::map<ASTNodeBase*,ASTNodeBase*> Skip;
};


typedef struct PatchData {
  // ASTNodeBase *parent = nullptr;
  std::set<ASTNodeBase*> selection;
  // std::set<ASTNodeBase*> patch;
} PatchData;


/**
 * This is actaully matchMin procedure.
 * Do grammar patching for an AST node and its selected children.
 */
class GrammarPatcher : public Visitor {
public:
  // GrammarPatcher(ASTNodeBase *parent, std::set<ASTNodeBase*> selection)
  //   : parent(parent), selection(selection) {}
  GrammarPatcher() {}
  GrammarPatcher(std::set<ASTNodeBase*> sel) : Selection(sel) {}
  virtual ~GrammarPatcher() {}
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

  void setSelection(std::set<ASTNodeBase*> sel) {Selection=sel;}
  
  std::set<ASTNodeBase*> getPatch() {return Patch;}
  void merge(GrammarPatcher *patcher) {
    if (patcher) {
      this->Patch.insert(patcher->Patch.begin(), patcher->Patch.end());
    }
  }
  // std::map<ASTNodeBase*, ASTNodeBase*> getSkip() {return Skip;}
private:
  // std::set<ASTNodeBase*> selection;
  // std::set<ASTNodeBase*> worklist;
  // ASTNodeBase *parent = nullptr;
  // std::set<ASTNodeBase*> selection;
  std::set<ASTNodeBase*> Selection;
  std::set<ASTNodeBase*> Patch;
};

#endif /* GRAMMARPATCHER_H */
