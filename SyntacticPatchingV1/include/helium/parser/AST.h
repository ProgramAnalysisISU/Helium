#ifndef AST_V2_H
#define AST_V2_H

#include <string>
#include <vector>
#include <set>
#include <map>

#include <ostream>

#include "helium/parser/Visitor.h"
// class Visitor;

// #include "helium/parser/source_manager.h"

#include "helium/parser/SourceLocation.h"

class SourceManager;



class TranslationUnitDecl;
class FunctionDecl;
class Expr;
class Stmt;
class Decl;
class DeclStmt;
class ASTNodeBase;
  
/**
 * \addtogroup ast
 * \ingroup parser
 *
 * ast group is a subgroup of parser group
 * @{
 */

class SymbolTable;

/**
 * You can get TranslationUnitDecl from this
 * This also contains the source manager
 * The Loc manager
 */
class ASTContext {
public:
  ASTContext(std::string filename) : Filename(filename) {}
  ~ASTContext() {}
  void setTranslationUnitDecl(TranslationUnitDecl *unit) {
    m_unit = unit;
  }
  TranslationUnitDecl *getTranslationUnitDecl() {return m_unit;}
  int getLevel(ASTNodeBase *node) {return Levels[node];}
  void setSourceManager(SourceManager *manager) {Manager=manager;}
  SourceManager *getSourceManager() {return Manager;}
  std::vector<ASTNodeBase*> getNodes() {return Nodes;}
  std::string getFileName() {return Filename;}

  void createSymbolTable();
  SymbolTable *getSymbolTable() {return m_symtbl;}
private:
  std::string Filename;
  TranslationUnitDecl *m_unit = nullptr;
  std::vector<ASTNodeBase*> Nodes;
  std::map<ASTNodeBase*, int> Levels;
  SourceManager *Manager = nullptr;
  // int level=-1;
  // root will have level 0
  // level will increase
  SymbolTable *m_symtbl = nullptr;
};


// I need to travese the AST
// - so that I can keep the first-order of all nodes into the ASTContext
// - and also the levels information
// Or do i need to keep everything in ASTContext?
// I can just write a visitor to dump nodes

class ASTNodeBase {
public:
  ASTNodeBase(ASTContext *ctx, SourceLocation begin, SourceLocation end)
    : Ctx(ctx), BeginLoc(begin), EndLoc(end)  {}
  ASTNodeBase(ASTContext *ctx, SourceRange range)
    : Ctx(ctx), BeginLoc(range.getBegin()), EndLoc(range.getEnd())  {}
  ~ASTNodeBase() {}
  virtual std::string label() {return "";}
  // virtual void accept(Visitor *visitor) {
  //   visitor->visit(this);
  // }
  virtual void accept(Visitor *visitor) = 0;
  ASTContext *getASTContext() {return Ctx;}
  SourceLocation getBeginLoc() {return BeginLoc;}
  SourceLocation getEndLoc() {return EndLoc;}
  virtual void dump(std::ostream &os) {
    os << getNodeName()
       << " " << BeginLoc.getLine() << ":" << BeginLoc.getColumn()
       << ":" << EndLoc.getLine() << ":" << EndLoc.getColumn();
  }
  virtual std::string getNodeName() = 0;

  void addUsedVar(std::string var) {m_used_vars.insert(var);}
  void addUsedVar(std::set<std::string> vars) {m_used_vars.insert(vars.begin(), vars.end());}
  std::set<std::string> getUsedVars() {return m_used_vars;}
  void addDefinedVar(std::string var, std::string type) {
    m_defined_vars.insert(var);
    name2type[var] = type;
  }
  std::set<std::string> getDefinedVars() {return m_defined_vars;}
  std::string getDefinedVarType(std::string name) {
    if (name2type.count(name) == 1) return name2type[name];
    return "";
  }

  // void addFullVar(std::string name, std::string type) {
  //   fullVars[name] = type;
  // }
  // std::map<std::string, std::string> getFullVars() {return fullVars;}
  virtual std::vector<ASTNodeBase*> getChildren() {
    return {};
  }
  virtual std::vector<ASTNodeBase*> getChildrenRecursive() {
    std::vector<ASTNodeBase*> ret;
    for (ASTNodeBase *node : getChildren()) {
      // assert(node);
      if (node) {
        ret.push_back(node);
        std::vector<ASTNodeBase*> tmp = node->getChildrenRecursive();
        ret.insert(ret.end(), tmp.begin(), tmp.end());
      }
    }
    return ret;
  }
    
  virtual std::set<std::string> getIdToResolve() {return {};}
  virtual bool isLeaf() {return false;}

  std::set<std::string> getCallees() {
    return m_callees;
  }
  void addCallee(std::string name) {
    m_callees.insert(name);
  }
  void addCallee(std::set<std::string> names) {
    m_callees.insert(names.begin(), names.end());
  }
protected:
  ASTContext *Ctx = nullptr;
  SourceLocation BeginLoc;
  SourceLocation EndLoc;
  // name to type map
  std::map<std::string, std::string> fullVars;
  // def-use
  std::set<std::string> m_defined_vars;
  std::set<std::string> m_used_vars;
  std::map<std::string, std::string> name2type;
  std::set<std::string> m_callees;
};


/**
 * Exprs
 */

class Expr : public ASTNodeBase {
public:
  Expr(ASTContext *ctx, std::string text, SourceLocation begin, SourceLocation end)
    : ASTNodeBase(ctx, begin, end), Text(text) {}
  Expr(ASTContext *ctx, std::string text, SourceRange range)
    : ASTNodeBase(ctx, range), Text(text) {}
  ~Expr() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  std::string getText() {return Text;}
  virtual void dump(std::ostream &os) {
    ASTNodeBase::dump(os);
    os << " " << "\"" << getText() << "\"";
  }
  virtual std::string getNodeName() {return "Expr";}
  virtual std::set<std::string> getIdToResolve();
  virtual bool isLeaf() {return true;}
private:
  std::string Text;
};


class Decl : public ASTNodeBase {
public:
  Decl(ASTContext *ctx, SourceLocation begin, SourceLocation end)
    : ASTNodeBase(ctx, begin, end) {}
  Decl(ASTContext *ctx, SourceRange range) : ASTNodeBase(ctx, range) {}
  ~Decl() {}
};

class Stmt : public ASTNodeBase {
public:
  Stmt(ASTContext *ctx, SourceLocation begin, SourceLocation end)
    : ASTNodeBase(ctx, begin, end) {}
  Stmt(ASTContext *ctx, SourceRange range) : ASTNodeBase(ctx, range) {}
  ~Stmt() {}
};
  
class TokenNode : public ASTNodeBase {
public:
  TokenNode(ASTContext *ctx, std::string text, SourceLocation begin, SourceLocation end)
    : ASTNodeBase(ctx, begin, end), Text(text) {}
  TokenNode(ASTContext *ctx, std::string text, SourceRange range)
    : ASTNodeBase(ctx, range), Text(text) {}
  ~TokenNode() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  std::string getText() {return Text;}
  virtual void dump(std::ostream &os) {
    ASTNodeBase::dump(os);
    os << " " << "\"" << getText() << "\"";
  }
  virtual std::string getNodeName() {
    return "TokenNode";
  }
  virtual std::set<std::string> getIdToResolve();
  virtual bool isLeaf() {return true;}
private:
  std::string Text;
};

class TranslationUnitDecl : public Decl {
public:
  TranslationUnitDecl(ASTContext *ctx, std::vector<ASTNodeBase*> decls,
                      SourceLocation begin, SourceLocation end)
    : Decl(ctx, begin, end), decls(decls) {}
  TranslationUnitDecl(ASTContext *ctx, std::vector<ASTNodeBase*> decls, SourceRange range)
    : Decl(ctx, range), decls(decls) {}
  ~TranslationUnitDecl() {}
  std::vector<ASTNodeBase*> getDecls() {return decls;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  virtual std::string getNodeName() {return "TranslationUnitDecl";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    return decls;
  }
private:
  std::vector<ASTNodeBase*> decls;
};



/**
 * Adapter class for mixing declarations with statements
 */
class DeclStmt : public Stmt {
public:
  DeclStmt(ASTContext *ctx, std::string text, SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), Text(text) {}
  DeclStmt(ASTContext *ctx, std::string text, SourceRange range)
    : Stmt(ctx, range), Text(text) {}
  ~DeclStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  std::string getText() {return Text;}
  virtual void dump(std::ostream &os) {
    ASTNodeBase::dump(os);
    os << " " << "\"" << getText() << "\"";
  }
  virtual std::string getNodeName() {return "DeclStmt";}
  // void setVars(std::set<std::string> vars) {this->vars = vars;}
  // std::set<std::string> getVars() {return vars;}
  virtual std::set<std::string> getIdToResolve();
  virtual bool isLeaf() {return true;}
private:
  std::string Text;
  // std::set<std::string> vars;
};

/**
 * There's actually no ExprStmt in C grammar, but srcml made it
 */
class ExprStmt : public Stmt {
public:
  ExprStmt(ASTContext *ctx, std::string text, SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), Text(text) {}
  ExprStmt(ASTContext *ctx, std::string text, SourceRange range)
    : Stmt(ctx, range), Text(text) {}
  ~ExprStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  std::string getText() {return Text;}
  virtual void dump(std::ostream &os) {
    ASTNodeBase::dump(os);
    os << " " << "\"" << getText() << "\"";
  }
  virtual std::string getNodeName() {return "ExprStmt";}
  virtual std::set<std::string> getIdToResolve();
  virtual bool isLeaf() {return true;}
private:
  std::string Text;
};

class CompoundStmt : public Stmt {
public:
  CompoundStmt(ASTContext *ctx, TokenNode *lbrace, TokenNode *rbrace, SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), m_lbrace(lbrace), m_rbrace(rbrace) {}
  CompoundStmt(ASTContext *ctx, TokenNode *lbrace, TokenNode *rbrace, SourceRange range)
    : Stmt(ctx, range), m_lbrace(lbrace), m_rbrace(rbrace) {}
  ~CompoundStmt() {}
  void Add(Stmt *stmt) {
    stmts.push_back(stmt);
  }
  std::vector<Stmt*> getBody() {return stmts;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(m_lbrace);
    ret.insert(ret.end(), stmts.begin(), stmts.end());
    ret.push_back(m_rbrace);
    return ret;
  }

  TokenNode *getLBrace() {return m_lbrace;}
  TokenNode *getRBrace() {return m_rbrace;}
  virtual std::string getNodeName() {return "CompoundStmt";}
private:
  // signature node. Even if compound statement do not have a
  // keyword, I need one to record if I select it or not
  // This is very useful to make the braces correct
  TokenNode *m_lbrace = nullptr;
  TokenNode *m_rbrace = nullptr;
  std::vector<Stmt*> stmts;
};

class FunctionDecl : public Decl {
public:
  FunctionDecl(ASTContext *ctx, std::string name,
               TokenNode *ReturnTypeNode, TokenNode *NameNode, TokenNode *ParamNode,
               Stmt *body, SourceLocation begin, SourceLocation end)
    : Decl(ctx, begin, end), name(name),
      ReturnTypeNode(ReturnTypeNode), NameNode(NameNode), ParamNode(ParamNode),
      body(body) {}
  FunctionDecl(ASTContext *ctx, std::string name,
               TokenNode *ReturnTypeNode, TokenNode *NameNode, TokenNode *ParamNode,
               Stmt *body, SourceRange range)
    : Decl(ctx, range), name(name),
      ReturnTypeNode(ReturnTypeNode), NameNode(NameNode), ParamNode(ParamNode),
      body(body) {}
  ~FunctionDecl() {}
  Stmt *getBody() {return body;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  std::string getName() {return name;}
  TokenNode *getReturnTypeNode() {return ReturnTypeNode;}
  TokenNode *getNameNode() {return NameNode;}
  TokenNode *getParamNode() {return ParamNode;}
  virtual std::string getNodeName() {return "FunctionDecl";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(ReturnTypeNode);
    ret.push_back(NameNode);
    if (ParamNode) ret.push_back(ParamNode);
    ret.push_back(body);
    return ret;
  }

  // void setVars(std::set<std::string> vars) {this->vars = vars;}
  // std::set<std::string> getVars() {return vars;}
private:
  std::string name;
  TokenNode *ReturnTypeNode = nullptr;
  TokenNode *NameNode = nullptr;
  // not sure if param node is useful
  // (HEBI: param, although is omitable, should never be ommited)
  // (HEBI: what if selection is across multiple function?? Which as main??)
  // (HEBI: what if selection is across multiple files? There will be multiple translation unit, aka mulitple ASTs)
  TokenNode *ParamNode = nullptr;
  Stmt *body = nullptr;

  // TODO map to type
  // std::set<std::string> vars;
};

class ForStmt : public Stmt {
public:
  ForStmt(ASTContext *ctx, Expr *Init, Expr *Cond, Expr *Inc, Stmt *Body, TokenNode *ForNode,
          SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), Init(Init), Cond(Cond), Inc(Inc), Body(Body), ForNode(ForNode) {
  }
  ForStmt(ASTContext *ctx, Expr *Init, Expr *Cond, Expr *Inc, Stmt *Body, TokenNode *ForNode,
          SourceRange range)
    : Stmt(ctx, range), Init(Init), Cond(Cond), Inc(Inc), Body(Body), ForNode(ForNode) {
  }
  ~ForStmt() {}
  Expr *getInit() {return Init;}
  Expr *getCond() {return Cond;}
  Expr *getInc() {return Inc;}
  Stmt *getBody() {return Body;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getForNode() {return ForNode;}
  virtual std::string getNodeName() {return "ForStmt";}

  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(ForNode);
    ret.push_back(Init);
    ret.push_back(Cond);
    ret.push_back(Inc);
    ret.push_back(Body);
    return ret;
  }

  // void setVars(std::set<std::string> vars) {this->vars = vars;}
  // std::set<std::string> getVars() {return vars;}
private:
  Expr *Init = nullptr;
  Expr *Cond = nullptr;
  Expr *Inc = nullptr;
  Stmt *Body = nullptr;
  TokenNode *ForNode = nullptr;

  // std::set<std::string> vars;
};

class WhileStmt : public Stmt {
public:
  WhileStmt(ASTContext *ctx, Expr *cond, Stmt *Body, TokenNode *WhileNode,
            SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), Cond(cond), Body(Body), WhileNode(WhileNode) {
  }
  WhileStmt(ASTContext *ctx, Expr *cond, Stmt *Body, TokenNode *WhileNode,
            SourceRange range)
    : Stmt(ctx, range), Cond(cond), Body(Body), WhileNode(WhileNode) {
  }
  ~WhileStmt() {}
  Expr *getCond() {return Cond;}
  Stmt *getBody() {return Body;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getWhileNode() {return WhileNode;}
  virtual std::string getNodeName() {return "WhileStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(Cond);
    ret.push_back(WhileNode);
    ret.push_back(Body);
    return ret;
  }
private:
  Expr *Cond;
  Stmt *Body;
  TokenNode *WhileNode = nullptr;
};

class DoStmt : public Stmt {
public:
  DoStmt(ASTContext *ctx, Expr *cond, Stmt *body, TokenNode *DoNode, TokenNode *WhileNode,
         SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), Cond(cond), Body(body), DoNode(DoNode), WhileNode(WhileNode) {}
  DoStmt(ASTContext *ctx, Expr *cond, Stmt *body, TokenNode *DoNode, TokenNode *WhileNode,
         SourceRange range)
    : Stmt(ctx, range), Cond(cond), Body(body), DoNode(DoNode), WhileNode(WhileNode) {}
  ~DoStmt() {}
  Expr *getCond() {return Cond;}
  Stmt *getBody() {return Body;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getDoNode() {return DoNode;}
  TokenNode *getWhileNode() {return WhileNode;}
  virtual std::string getNodeName() {return "DoStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(DoNode);
    ret.push_back(Cond);
    ret.push_back(Body);
    ret.push_back(WhileNode);
    return ret;
  }
private:
  Expr *Cond;
  Stmt *Body;
  TokenNode *DoNode = nullptr;
  TokenNode *WhileNode = nullptr;
};

class BreakStmt : public Stmt {
public:
  BreakStmt(ASTContext *ctx, SourceLocation begin, SourceLocation end) : Stmt(ctx, begin, end) {}
  BreakStmt(ASTContext *ctx, SourceRange range) : Stmt(ctx, range) {}
  ~BreakStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  virtual std::string getNodeName() {return "BreakStmt";}
  virtual bool isLeaf() {return true;}
};
class ContinueStmt : public Stmt {
public:
  ContinueStmt(ASTContext *ctx, SourceLocation begin, SourceLocation end) : Stmt(ctx, begin, end) {}
  ContinueStmt(ASTContext *ctx, SourceRange range) : Stmt(ctx, range) {}
  ~ContinueStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  virtual std::string getNodeName() {return "ContinueStmt";}
  virtual bool isLeaf() {return true;}
};
class ReturnStmt : public Stmt {
public:
  ReturnStmt(ASTContext *ctx, TokenNode *ReturnNode, Expr *Value, SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), ReturnNode(ReturnNode), Value(Value) {
  }
  ReturnStmt(ASTContext *ctx, TokenNode *ReturnNode, Expr *Value, SourceRange range)
    : Stmt(ctx, range), ReturnNode(ReturnNode), Value(Value) {
  }
  ~ReturnStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  Expr *getValue() {return Value;}
  TokenNode *getReturnNode() {return ReturnNode;}
  virtual std::string getNodeName() {return "ReturnStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(ReturnNode);
    ret.push_back(Value);
    return ret;
  }
private:
  TokenNode *ReturnNode = nullptr;
  Expr *Value = nullptr;
};

class IfStmt : public Stmt {
public:
  IfStmt(ASTContext *ctx, Expr *cond, Stmt *thenstmt, Stmt *elsestmt,
         TokenNode *IfNode, TokenNode *ElseNode,
         SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), cond(cond), thenstmt(thenstmt), elsestmt(elsestmt),
      IfNode(IfNode), ElseNode(ElseNode) {
  }
  IfStmt(ASTContext *ctx, Expr *cond, Stmt *thenstmt, Stmt *elsestmt,
         TokenNode *IfNode, TokenNode *ElseNode,
         SourceRange range)
    : Stmt(ctx, range), cond(cond), thenstmt(thenstmt), elsestmt(elsestmt),
      IfNode(IfNode), ElseNode(ElseNode) {
  }
  ~IfStmt() {}
  void setElse(TokenNode *elsenode, Stmt *stmt) {
    ElseNode = elsenode;
    elsestmt = stmt;
  }
  Expr *getCond() {return cond;}
  Stmt *getThen() {return thenstmt;}
  Stmt *getElse() {return elsestmt;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getIfNode() {return IfNode;}
  TokenNode *getElseNode() {return ElseNode;}
  virtual std::string getNodeName() {return "IfStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(IfNode);
    ret.push_back(cond);
    ret.push_back(thenstmt);
    ret.push_back(ElseNode);
    ret.push_back(elsestmt);
    return ret;
  }
private:
  Expr *cond = nullptr;
  Stmt *thenstmt = nullptr;
  Stmt *elsestmt = nullptr;
  TokenNode *IfNode = nullptr;
  TokenNode *ElseNode = nullptr;
};

/**
 * Base class for CaseStmt and DefaultStmt
 */
class SwitchCase : public Stmt {
public:
  SwitchCase(ASTContext *ctx, SourceLocation begin, SourceLocation end) : Stmt(ctx, begin, end) {}
  SwitchCase(ASTContext *ctx, SourceRange range) : Stmt(ctx, range) {}
  ~SwitchCase() {}
  void Add(Stmt *stmt) {assert(stmt); Body.push_back(stmt);}
  std::vector<Stmt*> getBody() {return Body;}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.insert(ret.end(), Body.begin(), Body.end());
    return ret;
  }
protected:
  std::vector<Stmt*> Body;
};

class SwitchStmt : public Stmt {
public:
  // FIXME body??
  SwitchStmt(ASTContext *ctx, Expr *cond, Stmt *body, TokenNode *SwitchNode,
             SourceLocation begin, SourceLocation end)
    : Stmt(ctx, begin, end), Cond(cond), SwitchNode(SwitchNode) {
  }
  SwitchStmt(ASTContext *ctx, Expr *cond, Stmt *body, TokenNode *SwitchNode,
             SourceRange range)
    : Stmt(ctx, range), Cond(cond), SwitchNode(SwitchNode) {
  }
  ~SwitchStmt() {}
  void AddCase(SwitchCase *casestmt) {Cases.push_back(casestmt);}
  std::vector<SwitchCase*> getCases() {return Cases;}
  Expr *getCond() {return Cond;}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getSwitchNode() {return SwitchNode;}
  virtual std::string getNodeName() {return "SwitchStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret;
    ret.push_back(SwitchNode);
    ret.push_back(Cond);
    ret.insert(ret.end(), Cases.begin(), Cases.end());
    return ret;
  }
private:
  Expr *Cond = nullptr;
  std::vector<SwitchCase*> Cases;
  TokenNode *SwitchNode = nullptr;
};


/**
 * This is tricky. The grammar says:
 * labeled-statement ::= case : statement
 * But this is a single statement.
 * The grammar actually permits 0 or more statements without curly braces to make them compound statement
 * Thus, we need to make some decision here:
 * The easiest way seems to be alter the grammar to be
 * labeled-statement ::= case : [case-body-stmt]_opt
 * case-body-stmt ::= stmt case-body-stmt
 * Or using eBNF
 * labeled-statement ::= case : [stmt]*
 * The implementation is trivial: use a vector of statements
 */
class CaseStmt : public SwitchCase {
public:
  CaseStmt(ASTContext *ctx, Expr *cond, TokenNode *CaseNode, SourceLocation begin, SourceLocation end)
    : SwitchCase(ctx, begin, end), Cond(cond), CaseNode(CaseNode) {
  }
  CaseStmt(ASTContext *ctx, Expr *cond, TokenNode *CaseNode, SourceRange range)
    : SwitchCase(ctx, range), Cond(cond), CaseNode(CaseNode) {
  }
  ~CaseStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getCaseNode() {return CaseNode;}
  Expr *getCond() {return Cond;}
  virtual std::string getNodeName() {return "CaseStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret = SwitchCase::getChildren();
    ret.insert(ret.begin(), Cond);
    ret.insert(ret.begin(), CaseNode);
    return ret;
  }
private:
  Expr *Cond = nullptr;
  TokenNode *CaseNode = nullptr;
};

class DefaultStmt : public SwitchCase {
public:
  DefaultStmt(ASTContext *ctx, TokenNode *DefaultNode, SourceLocation begin, SourceLocation end)
    : SwitchCase(ctx, begin, end), DefaultNode(DefaultNode) {
  }
  DefaultStmt(ASTContext *ctx, TokenNode *DefaultNode, SourceRange range)
    : SwitchCase(ctx, range), DefaultNode(DefaultNode) {
  }
  ~DefaultStmt() {}
  virtual void accept(Visitor *visitor) {
    visitor->visit(this);
  }
  TokenNode *getDefaultNode() {return DefaultNode;}
  virtual std::string getNodeName() {return "DefaultStmt";}
  virtual std::vector<ASTNodeBase*> getChildren() {
    std::vector<ASTNodeBase*> ret = SwitchCase::getChildren();
    ret.insert(ret.begin(), DefaultNode);
    return ret;
  }
private:
  TokenNode *DefaultNode = nullptr;
};

/** @}*/




#endif /* AST_V2_H */
