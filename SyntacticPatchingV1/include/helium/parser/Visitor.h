#ifndef VISITOR_H
#define VISITOR_H

// #include "helium/parser/AST.h"

#include <vector>
#include <map>
#include <set>
#include <sstream>
#include <boost/algorithm/string/join.hpp>

#include "helium/parser/SourceLocation.h"
#include "helium/utils/Graph.h"
#include "helium/utils/StringUtils.h"

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


/**
 * \defgroup visitor
 * \ingroup parser
 * Visitors for AST
 */

/**
 * \ingroup visitor
 *
 * visitor interface
 */
class Visitor {
public:
  Visitor() {}
  virtual ~Visitor() {}
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
};


class SimplePreorderVisitor : public Visitor {
public:
  SimplePreorderVisitor() {}
  virtual ~SimplePreorderVisitor() {}
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
  std::vector<ASTNodeBase*> getNodes() {
    return Nodes;
  }
private:
  std::vector<ASTNodeBase*> Nodes;
};

/**
 * \ingroup visitor
 *  use this to replace the dump method
 */
class Printer : public  Visitor {
public:
  Printer() {}
  ~Printer() {}


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

  void addMark(std::set<ASTNodeBase*> nodes, std::string text) {
    marks.push_back({nodes, text});
  }

  std::string getString() {
    return utils::lisp_pretty_print(oss.str());
  }
private:
  void pre(ASTNodeBase *node);
  void post();
  std::ostringstream oss;
  std::vector<std::pair<std::set<ASTNodeBase*>, std::string> > marks;
};


class LevelVisitor : public Visitor {
public:
  LevelVisitor() {}
  virtual ~LevelVisitor() {}
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

  std::map<ASTNodeBase*, int> getLevels() {return Levels;}
  int getLevel(ASTNodeBase *node);
  ASTNodeBase *getLowestLevelNode(std::set<ASTNodeBase*> nodes);
private:
  void pre(ASTNodeBase* node) {Levels[node]=lvl; lvl++;}
  void post() {lvl--;}
  std::map<ASTNodeBase*, int> Levels;
  int lvl=0;
};

/**
 * Token here means the leaf-node of AST, so it is abstract token.
 * The granularity depends on the parser precision.
 */
class TokenVisitor : public Visitor {
public:
  TokenVisitor() {}
  ~TokenVisitor() {}
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

  std::vector<ASTNodeBase*> getTokens() {return Tokens;}
  std::map<ASTNodeBase*,int> getIdMap() {return IdMap;}
  int getId(ASTNodeBase *node) {
    if (IdMap.count(node) == 1) return IdMap[node];
    return -1;
  }
private:
  int id = 0;
  std::map<ASTNodeBase*,int> IdMap; // ID start from 0
  std::vector<ASTNodeBase*> Tokens; // this is actually ID->Node implemented in vector
};

/**
 * Build a reverse index from children node to its parent
 */
class ParentIndexer : public Visitor {
public:
  ParentIndexer() {}
  ~ParentIndexer() {}

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

  std::map<ASTNodeBase*, ASTNodeBase*> getParentMap() {return ParentMap;}
  std::vector<ASTNodeBase*> getChildren(ASTNodeBase* parent) {
    if (ChildrenMap.count(parent) == 1) {
      return ChildrenMap[parent];
    }
    return {};
  }

  ASTNodeBase *getParent(ASTNodeBase *node) {
    if (ParentMap.count(node) == 1) return ParentMap[node];
    return nullptr;
  }
private:
  void pre(ASTNodeBase* node);
  void post();
  std::map<ASTNodeBase*, ASTNodeBase*> ParentMap;
  std::map<ASTNodeBase*, std::vector<ASTNodeBase*> > ChildrenMap;

  std::vector<ASTNodeBase*> Stack;
};





/**
 * Compute the distribution of tokens, e.g. inside which if, which function
 *
 * This is also going to be bi-direction.
 *
 * map<Node, set> maps a node to all the tokens it contains. Notice that this node should be the representative nodes, like if, switch, loop.
 * set<Node> if_nodes;
 * set<Node> switch_nodes;
 * set<Node> for_nodes;
 * set<Node> do_nodes;
 * set<Node> while_nodes;
 * set<Node> func_nodes;
 */
class Distributor : public Visitor {
public:
  Distributor() {}
  ~Distributor() {}
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

  void pre(ASTNodeBase *node);
  void post();

  /**
   * get how many functions enclosing these nodes
   * Note these nodes may not belong to this AST
   */
  int getDistFunc(std::set<ASTNodeBase*> sel) {
    return getDist(sel, func_nodes);
  }
  std::set<ASTNodeBase*> getDistFuncNodes(std::set<ASTNodeBase*> sel) {
    return getDistNodes(sel, func_nodes);
  }
  int getDistIf(std::set<ASTNodeBase*> sel) {
    return getDist(sel, if_nodes);
  }
  int getDistLoop(std::set<ASTNodeBase*> sel) {
    std::set<ASTNodeBase*> loop_nodes;
    loop_nodes.insert(for_nodes.begin(), for_nodes.end());
    loop_nodes.insert(while_nodes.begin(), while_nodes.end());
    loop_nodes.insert(do_nodes.begin(), do_nodes.end());
    return getDist(sel, loop_nodes);
  }
  int getDistSwitch(std::set<ASTNodeBase*> sel) {
    return getDist(sel, switch_nodes);
  }

  /**
   * Generic function for get distribution
   */
  int getDist(std::set<ASTNodeBase*> sel, std::set<ASTNodeBase*> nodes) {
    int ret=0;
    for (auto *node : nodes) {
      for (auto *s : sel) {
        if (ContainMap[node].count(s) == 1) {
          ret++;
          break;
        }
      }
    }
    return ret;
  }
  std::set<ASTNodeBase*> getDistNodes(std::set<ASTNodeBase*> sel, std::set<ASTNodeBase*> nodes) {
    std::set<ASTNodeBase*> ret;
    for (auto *node : nodes) {
      for (auto *s : sel) {
        if (ContainMap[node].count(s) == 1) {
          ret.insert(node);
          break;
        }
      }
    }
    return ret;
  }

  void dump(std::ostream &os);

  std::set<ASTNodeBase*> getIfNodes() {return if_nodes;}
  std::set<ASTNodeBase*> getSwitchNodes() {return switch_nodes;}
  std::set<ASTNodeBase*> getForNodes() {return for_nodes;}
  std::set<ASTNodeBase*> getDoNodes() {return do_nodes;}
  std::set<ASTNodeBase*> getWhileNodes() {return while_nodes;}
  std::set<ASTNodeBase*> getFuncNodes() {return func_nodes;}
  
  
private:
  std::map<ASTNodeBase*, std::set<ASTNodeBase*> > ContainMap;
  std::set<ASTNodeBase*> if_nodes;
  std::set<ASTNodeBase*> switch_nodes;
  std::set<ASTNodeBase*> for_nodes;
  std::set<ASTNodeBase*> do_nodes;
  std::set<ASTNodeBase*> while_nodes;
  std::set<ASTNodeBase*> func_nodes;

  std::vector<ASTNodeBase*> Stack;
};







/**
 * This is simple AST matcher
 * It is used in testing for locating the node I want.
 * I'm going to visit each node, and get the full path of them
 * Then just simply match.
 *
 * As input,
 * Specify the full path of the node you want
 * E.g. /DeclarationUnitDecl/Function/...
 * 1. n-th: if the path match multiple nodes, return all of them. The order is TODO
 * 2. return nullptr if no such node
 *
 * A second approach to match a node is through location and node name
 * e.g. line 5, column 8, IfStmt
 */
class Matcher : public Visitor {
public:
  Matcher() {}
  virtual ~Matcher() {}
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

  std::vector<ASTNodeBase*> match(std::string name) {
    if (PathMap.count(name) == 1) {
      return PathMap[name];
    }
    return {};
  }
  int size() {return PathMap.size();}
  void dump(std::ostream &os) {
    for (auto m : PathMap) {
      os << m.first << m.second.size() << "\n";
    }
  }

  /**
   * use line, don't care about column
   * Must start on that line
   */
  ASTNodeBase* getNodeByLoc(std::string name, int line);
  /**
   * nth: 0 means first, 1 means second
   */
  ASTNodeBase* getNodeByLoc(std::string name, int line, int nth);
  /**
   * Use both line and column
   */
  ASTNodeBase* getNodeByLoc(std::string name, SourceLocation loc);
  ASTNodeBase* getNodeByLoc(std::string name, SourceLocation loc, int nth);

  /**
   * Dump:
   * - path to node map
   * - all nodes
   */
  // void dump(std::ostream &os);

private:
  void pre(ASTNodeBase* node);
  void post();
  std::string currentName() {
    return boost::algorithm::join(Stack, "/");
  }
  std::vector<std::string> Stack;
  std::map<std::string, std::vector<ASTNodeBase*> > PathMap;

  std::vector<ASTNodeBase*> Nodes;
};

class CFGNode {
public:
  CFGNode(ASTNodeBase*node) {
    astnode = node;
  }
  CFGNode(std::string dummy) {
    this->dummy = dummy;
  }
  ~CFGNode() {}
  std::string getLabel();
  std::set<std::string> getCallees() {
    return callees;
  }
  void addCallee(std::string name) {
    callees.insert(name);
  }
  void addCallee(std::set<std::string> names) {
    callees.insert(names.begin(), names.end());
  }
  ASTNodeBase *getASTNode() {return astnode;}
private:
  ASTNodeBase *astnode=nullptr;
  std::set<std::string> callees;
  std::string dummy;
};
class CFG {
public:
  CFG() {}
  ~CFG() {}

  // add nodes
  void addNode(CFGNode *node) {
    graph.addNode(node);
  }
  void addNodes(std::set<CFGNode*> nodes) {
    for (CFGNode *node : nodes) {
      graph.addNode(node);
    }
  }
  std::set<CFGNode*> getHead() {
    return ins;
  }
  std::set<CFGNode*> getTail() {
    return outs;
  }
  std::set<CFGNode*> getPredecessor(CFGNode *node) {
    return graph.getPredecessor(node);
  }
  std::set<CFGNode*> getSuccessor(CFGNode *node) {
    return graph.getSuccessor(node);
  }
  /**
   * Merge
   */
  void connect(CFG *cfg) {
    this->graph.merge(cfg->graph);
    for (CFGNode *from : this->outs) {
      for (CFGNode *to : cfg->ins) {
        this->graph.addEdge(from, to);
      }
    }
    this->outs = cfg->outs;
  }
  /**
   * add node and connect all exits to it (it will be the out)
   */
  void connect(CFGNode *node) {
    this->graph.addNode(node);
    for (CFGNode *from : this->outs) {
      this->graph.addEdge(from, node);
    }
    this->outs.clear();
    this->outs.insert(node);
  }
  void mergeBranch(CFG *cfg, CFGNode* node, bool b) {
    for (CFGNode *to : cfg->ins) {
      this->graph.addEdge(node, to, (b?"true":"false"));
    }
    this->outs.insert(cfg->outs.begin(), cfg->outs.end());
  }

  void mergeCase(CFG *cfg, CFGNode *node, std::string case_label) {
    for (CFGNode *to : cfg->ins) {
      this->graph.addEdge(node, to, case_label);
    }
    this->outs.insert(cfg->outs.begin(), cfg->outs.end());
  }
    
  void addEdge(CFGNode *from, CFGNode *to) {
    graph.addEdge(from, to);
  }
  /**
   * remove the out edge of node
   * Used to handle break, continue, return
   */
  void removeOutEdge(CFGNode *node) {
    graph.removeOutEdge(node);
  }
  std::string getDotString();
  std::string getGgxString();
  std::string getGrsString();

  /**
   * I still need to set in and out manually because, return xx; a=b;
   * When I remove the return out edge, a=b; will be the input.
   * There're several such bugs
   */
  void addIn(CFGNode *node) {
    ins.insert(node);
  }
  void addOut(CFGNode *node) {
    outs.insert(node);
  }
  void clearIn(CFGNode *node) {
    ins.clear();
  }
  void clearOut(CFGNode *node) {
    outs.clear();
  }
  void addFunc(CFGNode *node, std::string name) {
    funcs[node] = name;
  }
  std::map<CFGNode*, std::string> getFuncs() {
    return funcs;
  }
  std::set<CFGNode*> getAllNodes() {
    return graph.getAllNodes();
  }
  // friend class CFGBuilder;
  // friend is not working, probably due to namespace
  // i'm making the fields public for the time
  hebigraph::Graph<CFGNode*> graph;
  std::set<CFGNode*> ins;
  std::set<CFGNode*> outs;
private:
  std::map<CFGNode*, std::string> funcs;
};

typedef enum _CFGBuilderOption {
  CFG_NoDecl
} CFGBuilderOption;

class CFGBuilder : public Visitor {
public:
  CFGBuilder() {}
  ~CFGBuilder() {}
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

  void pre(ASTNodeBase* node);

  /**
   * CFG for the top level passed in
   */
  CFG* getCFG() {return cur_cfg;}

  CFG *getCFGByNode(ASTNodeBase *node) {
    if (Node2CFG.count(node)) {
      return Node2CFG[node];
    }
    return nullptr;
  }

  /**
   * get CFGs orderred by functions
   */
  std::map<std::string, CFG*> getFuncCFGs() {
    return m_func_cfgs;
  }
  void addFuncCFG(std::string name, CFG *cfg) {
    m_func_cfgs[name] = cfg;
  }

  CFG* getInnerCFG(ASTNodeBase* node) {
    assert(node);
    // assert(Node2CFG.count(node) == 1);
    if (Node2CFG.count(node) == 0) return nullptr;
    return Node2CFG[node];
  }
  void addInnerCFG(ASTNodeBase* node, CFG *cfg) {
    assert(cfg);
    assert(node);
    Node2CFG[node]=cfg;
    // record current, for the top level output
    cur_cfg = cfg;
  }

  void addOption(CFGBuilderOption option) {
    m_options.insert(option);
  }
  void clearOptions(CFGBuilderOption option) {
    m_options.clear();
  }

private:
  std::set<CFGBuilderOption> m_options;
  CFG *cur_cfg = nullptr;
  // CFGNode *cur_cfgnode = nullptr;
  // do not use this alone
  std::map<ASTNodeBase*, CFG*> Node2CFG;
  std::set<CFGNode*> break_nodes;
  std::set<CFGNode*> continue_nodes;
  std::set<CFGNode*> return_nodes;
  std::map<std::string, CFG*> m_func_cfgs;
};

CFG *create_icfg(std::vector<CFG*> cfgs, bool mutate=false);


/**
 * Select which DeclStmt, ForInit, ParamVar is going to be
 * initialized.
 * Criteria:
 * - used without definition
 */
class InputSelector : Visitor {
};

#endif /* VISITOR_H */
