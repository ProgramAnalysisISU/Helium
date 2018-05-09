#ifndef SOURCE_MANAGER_H
#define SOURCE_MANAGER_H

#include <vector>
#include "helium/parser/AST.h"

#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

#include <gtest/gtest.h>

#include "helium/parser/IncludeManager.h"
#include "helium/parser/LibraryManager.h"
#include "helium/type/SnippetManager.h"

namespace fs = boost::filesystem;



/**
 * \ingroup parser
 */
class SourceManager {
public:
  /**
   * Precondition: p should be the cache/XXX/cpp folder
   */
  SourceManager() {}
  ~SourceManager() {}

  void parse(fs::path p);
  
  /**
   * num: how many nodes to be selected.
   */
  std::set<ASTNodeBase*> genRandSel(int num);
  std::set<ASTNodeBase*> genRandSelFunc(int num);
  std::set<ASTNodeBase*> genRandSelSameFunc(int num);
  std::set<ASTNodeBase*> loadSelection(fs::path sel_file);
  void dumpSelection(std::set<ASTNodeBase*> selection, std::ostream &os);
  
  void analyzeDistribution(std::set<ASTNodeBase*> selection,
                           std::set<ASTNodeBase*> patch,
                           std::ostream &os);

  void dump(std::ostream &os);
  std::set<ASTNodeBase*> patchFunctionHeader(std::set<ASTNodeBase*> sel);

  void dumpDist(std::set<ASTNodeBase*> sel, std::ostream &os);
  std::set<ASTNodeBase*> filterLeaf(std::set<ASTNodeBase*> sel);

  /**
   * Perform grammar patch based on this->selection.
   * Thus you need to call select first.
   */
  std::set<ASTNodeBase*> grammarPatch(std::set<ASTNodeBase*> sel);
  /**
   * Def use analysis
   * If some variable is used, include the node containing its declaration.
   */
  std::set<ASTNodeBase*> defUse(std::set<ASTNodeBase*> sel);

  /**
   * Generate program based on selection of nodes.
   * These nodes might be in different AST
   */
  // std::string generateProgram(std::set<ASTNodeBase*> sel);
  
  std::string generateMainC(std::set<ASTNodeBase*> sel);
  std::string generateInputH();

  /**
   * Generate main.h
   */
  std::string generateMainH(std::set<ASTNodeBase*> sel,
                              SnippetManager *snip_manager,
                              IncludeManager *inc_manager,
                              LibraryManager *lib_manager);

  /**
   * Generate main.c, main.h, Makefile, into a folder
   */
  void generate(std::set<ASTNodeBase*> sel, fs::path dir,
                SnippetManager *snip_man,
                IncludeManager *inc_man,
                LibraryManager *lib_man,
                fs::path input_value_dir);

  /**
   * Dump ast to 
   */
  void dumpAST(fs::path outdir, fs::path ext = ".lisp");
  void dumpAST(fs::path outdir, std::set<ASTNodeBase*> sel, fs::path ext = ".lisp");

private:
  /**
   * Match a file in files and return the best match. Empty if no match.
   */
  fs::path matchFile(fs::path file);
  int getDistFile(std::set<ASTNodeBase*> sel);
  int getDistProc(std::set<ASTNodeBase*> sel);
  int getDistIf(std::set<ASTNodeBase*> sel);
  int getDistLoop(std::set<ASTNodeBase*> sel);
  int getDistSwitch(std::set<ASTNodeBase*> sel);
  
  /**
   * ASTs associcated with the nodes
   */
  std::set<ASTContext*> getASTByNodes(std::set<ASTNodeBase*> nodes);
  bool shouldPutIntoMain(std::set<ASTNodeBase*> sel);


  // this class holds all ASTs
  // it also determines the ID of ast nodes
  // each AST node should have an unique ID
  // std::vector<ASTContext*> ASTs;
  fs::path cppfolder;
  // std::vector<fs::path> files;

  std::map<fs::path, ASTContext*> File2ASTMap;
  std::map<ASTContext*, fs::path> AST2FileMap;

  // token visitor, always available because it is useful
  std::map<ASTContext*, TokenVisitor*> AST2TokenVisitorMap;
  std::map<ASTContext*, Distributor*> AST2DistributorMap;

  std::set<ASTNodeBase*> InputVarNodes;
  std::set<ASTNodeBase*> OutputVarNodes;
  
  // std::vector<ASTNodeBase*> Nodes;
  // std::map<ASTNodeBase*,int> IDs;

  // std::set<ASTNodeBase*> selection;
};


#endif /* SOURCE_MANAGER_H */
