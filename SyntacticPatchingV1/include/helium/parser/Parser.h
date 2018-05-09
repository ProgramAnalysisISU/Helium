#ifndef PARSER_H
#define PARSER_H

#include "helium/parser/AST.h"
#include <string>
#include "helium/utils/XMLNode.h"

#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Frontend/FrontendActions.h"
#include "llvm/Support/CommandLine.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Rewrite/Core/Rewriter.h"


namespace fs = boost::filesystem;

/**
 * \defgroup parser
 * Parser converts source code to AST. When consuming srcml output, we convert that output to AST.
 */


class Parser {
public:
  Parser() {}
  virtual ~Parser() {}
  virtual ASTContext* parse(fs::path file) = 0;
};

/**
 * With Clang Pointers
 * - Use: From ASTNodeBase* to set of VarDecl
 * - Def: from ASTNodeBase* to set of VarDecl
 * - Def-rev: From VarDecl* to ASTNodeBase*
 * Without clang:
 * - Def: ASTNodeBase* to set of name
 * - Use: ASTNodeBase* to pairs of <ASTNodeBase*, name>
 * Long Lasting General Purpose Symbol Table:
 * - A tree structure
 * - 
 */
class ClangSymbolTable {
public:
private:
};

class ClangParser : public Parser {
public:
  ClangParser() {}
  virtual ~ClangParser() {}

  virtual ASTContext *parse(fs::path file);


  static TranslationUnitDecl* parseTranslationUnitDecl
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::TranslationUnitDecl *dec,
   ASTContext *myctx);
  static DeclStmt* parseDeclStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::DeclStmt *decl_stmt,
   ASTContext *myctx);
  static FunctionDecl *parseFunctionDecl
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::FunctionDecl *func,
   ASTContext *myctx);
  static CompoundStmt *parseCompoundStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::CompoundStmt *comp,
   ASTContext *myctx);
  static Stmt *parseStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::Stmt *stmt,
   ASTContext *myctx);
  static ReturnStmt *parseReturnStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::ReturnStmt *ret_stmt,
   ASTContext *myctx);
  static IfStmt *parseIfStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::IfStmt *if_stmt,
   ASTContext *myctx);
  static SwitchStmt *parseSwitchStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::SwitchStmt *switch_stmt,
   ASTContext *myctx);
  static CaseStmt *parseCaseStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::CaseStmt *case_stmt,
   ASTContext *myctx);
  static DefaultStmt *parseDefaultStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::DefaultStmt *def_stmt,
   ASTContext *myctx);
  static WhileStmt *parseWhileStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::WhileStmt *while_stmt,
   ASTContext *myctx);
  static ForStmt *parseForStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::ForStmt *for_stmt,
   ASTContext *myctx);
  static DoStmt *parseDoStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::DoStmt *do_stmt,
   ASTContext *myctx);
  static Expr *parseExpr
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::Expr *expr,
   ASTContext *myctx);
  static ExprStmt *parseExprStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::Expr *expr,
   ASTContext *myctx);
  static Expr *parseForInit
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::Stmt *init,
   ASTContext *myctx);
  static BreakStmt *parseBreakStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::BreakStmt *break_stmt,
   ASTContext *myctx);
  static ContinueStmt *parseContinueStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::ContinueStmt *cont_stmt,
   ASTContext *myctx);
  static ExprStmt *parseNullStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::NullStmt *null_stmt,
   ASTContext *myctx);
  static ExprStmt *parseDummyStmt
  (clang::ASTContext *ctx, clang::Rewriter &rewriter,
   clang::Stmt *stmt, ASTContext *myctx);
};


#if 0
/**
 * \ingroup parser
 * \brief Parser for SrcML.
 *
 * This is the only place related to SrcML (hopefully)
 */
class SrcMLParser : public Parser {
public:
  SrcMLParser() {}
  virtual ~SrcMLParser() {}

  virtual ASTContext *parse(fs::path file);

  TranslationUnitDecl *ParseTranslationUnitDecl(ASTContext *ctx, XMLNode unit);
  DeclStmt *ParseDeclStmt(ASTContext *ctx, XMLNode decl);
  FunctionDecl *ParseFunctionDecl(ASTContext *ctx, XMLNode func);
  CompoundStmt *ParseCompoundStmt(ASTContext *ctx, XMLNode node);
  Stmt *ParseStmt(ASTContext *ctx, XMLNode node);
  ReturnStmt *ParseReturnStmt(ASTContext *ctx, XMLNode node);
  IfStmt *ParseIfStmt(ASTContext *ctx, XMLNode node);
  IfStmt *ParseElseIfAsIfStmt(ASTContext *ctx, XMLNode node);
  SwitchStmt *ParseSwitchStmt(ASTContext *ctx, XMLNode node);
  CaseStmt *ParseCaseStmt(ASTContext *ctx, XMLNode node);
  DefaultStmt *ParseDefaultStmt(ASTContext *ctx, XMLNode node);
  WhileStmt *ParseWhileStmt(ASTContext *ctx, XMLNode node);
  ForStmt *ParseForStmt(ASTContext *ctx, XMLNode node);
  DoStmt *ParseDoStmt(ASTContext *ctx, XMLNode node);
  Expr *ParseExpr(ASTContext *ctx, XMLNode node);
  Expr *ParseExprWithoutSemicolon(ASTContext *ctx, XMLNode node);
  Stmt *ParseExprStmt(ASTContext *ctx, XMLNode node);
private:
  void match(XMLNode node, std::string tag);
};

#endif

#endif /* PARSER_H */
