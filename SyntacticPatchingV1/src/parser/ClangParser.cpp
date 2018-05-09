#include "helium/parser/Parser.h"
#include "helium/utils/FSUtils.h"
#include "clang/Lex/Preprocessor.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/CommandLine.h"

static SourceLocation convert_clang_loc(clang::ASTContext *ctx, clang::SourceLocation loc) {
  clang::FullSourceLoc full = ctx->getFullLoc(loc);
  SourceLocation ret(full.getSpellingLineNumber(), full.getSpellingColumnNumber());
  return ret;
}

static SourceRange convert_clang_loc(clang::ASTContext *ctx, clang::SourceLocation loc, std::string keyword) {
  SourceLocation begin = convert_clang_loc(ctx, loc);
  SourceLocation end = begin + std::make_pair(0, keyword.length());
  return SourceRange(begin, end);
}
static SourceRange convert_clang_loc(clang::ASTContext *ctx, clang::SourceRange range) {
  return SourceRange(convert_clang_loc(ctx, range.getBegin()),
                     convert_clang_loc(ctx, range.getEnd()));
}



TranslationUnitDecl* ClangParser::parseTranslationUnitDecl
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::TranslationUnitDecl *unit,
 ASTContext *myctx) {
  std::vector<ASTNodeBase*> decls;
  clang::DeclContext::decl_iterator it;
  for (it=unit->decls_begin(); it!=unit->decls_end(); ++it) {
    clang::Decl *child = *it;
    if (ctx->getSourceManager().isInMainFile(child->getLocStart())) {
      // child->dump();
      // FIXME if i use a strcpy in the test program, but didn't include
      // the header file string.h, then clang will generate a top level
      // functiondecl for strcpy as implicit used function
      if (clang::FunctionDecl *func = dynamic_cast<clang::FunctionDecl*>(child)) {
        FunctionDecl *myfunc = parseFunctionDecl(ctx, rewriter, func, myctx);
        if (myfunc) {
          decls.push_back(myfunc);
        }
      }
    }
  }
  TranslationUnitDecl *myunit
    = new TranslationUnitDecl(myctx, decls,
                              convert_clang_loc(ctx, unit->getSourceRange()));
  return myunit;
}

/**
 * TODO NOW
 */
std::set<clang::VarDecl*> get_referred_var_decl(clang::Expr *expr) {
  std::set<clang::VarDecl*> ret;
  if (clang::DeclRefExpr *ref = clang::dyn_cast<clang::DeclRefExpr>(expr)) {
    // this is final
    clang::ValueDecl *decl = ref->getDecl();
    if (clang::VarDecl *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      ret.insert(var_decl);
    }
  } else if (clang::UnaryOperator *un = clang::dyn_cast<clang::UnaryOperator>(expr)) {
    clang::Expr *sub = un->getSubExpr();
    std::set<clang::VarDecl*> tmp = get_referred_var_decl(sub);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::BinaryOperator *bi = clang::dyn_cast<clang::BinaryOperator>(expr)) {
    clang::Expr *lhs = bi->getLHS();
    std::set<clang::VarDecl*> tmp = get_referred_var_decl(lhs);
    ret.insert(tmp.begin(), tmp.end());
    clang::Expr *rhs = bi->getRHS();
    tmp = get_referred_var_decl(rhs);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::CallExpr *call = clang::dyn_cast<clang::CallExpr>(expr)) {
    for (clang::CallExpr::arg_iterator it=call->arg_begin();it!=call->arg_end();++it) {
      clang::Expr *arg = *it;
      std::set<clang::VarDecl*> tmp = get_referred_var_decl(arg);
      ret.insert(tmp.begin(), tmp.end());
    }
  } else if (clang::CastExpr *cast = clang::dyn_cast<clang::CastExpr>(expr)) {
    clang::Expr *sub = cast->getSubExpr();
    std::set<clang::VarDecl*> tmp = get_referred_var_decl(sub);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::ParenExpr *paren = clang::dyn_cast<clang::ParenExpr>(expr)) {
    clang::Expr *sub = paren->getSubExpr();
    std::set<clang::VarDecl*> tmp = get_referred_var_decl(sub);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::MemberExpr *mem = clang::dyn_cast<clang::MemberExpr>(expr)) {
    clang::Expr *base = mem->getBase();
    std::set<clang::VarDecl*> tmp = get_referred_var_decl(base);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::ConditionalOperator *cond_expr = clang::dyn_cast<clang::ConditionalOperator>(expr)) {
    clang::Expr *cond = cond_expr->getCond();
    clang::Expr *t = cond_expr->getTrueExpr();
    clang::Expr *f = cond_expr->getFalseExpr();
    std::set<clang::VarDecl*> tmp;
    tmp = get_referred_var_decl(cond);
    ret.insert(tmp.begin(), tmp.end());
    tmp = get_referred_var_decl(t);
    ret.insert(tmp.begin(), tmp.end());
    tmp = get_referred_var_decl(f);
    ret.insert(tmp.begin(), tmp.end());
  } else {
    // TODO not supported
  }
  return ret;
}

std::set<std::string> get_used_vars(clang::DeclStmt *decl_stmt) {
  std::set<std::string> ret;
  for (clang::DeclStmt::decl_iterator it=decl_stmt->decl_begin();it!=decl_stmt->decl_end();++it) {
    clang::Decl *decl = *it;
    // I'm interested in VarDecl
    if (clang::VarDecl *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      if (var_decl->hasInit()) {
        clang::Expr *init = var_decl->getInit();
        std::set<clang::VarDecl*> used = get_referred_var_decl(init);
        // store names
        for (clang::VarDecl *var : used) {
          std::string name = var->getNameAsString();
          ret.insert(name);
        }
      }
    }
  }
  return ret;
}

typedef struct {
  std::string name;
  std::string type;
} PlainVar;

std::vector<PlainVar> get_defined_vars(clang::DeclStmt *decl_stmt) {
  std::vector<PlainVar> ret;
  for (clang::DeclStmt::decl_iterator it=decl_stmt->decl_begin();it!=decl_stmt->decl_end();++it) {
    clang::Decl *decl = *it;
    // I'm interested in VarDecl
    if (clang::VarDecl *var_decl = clang::dyn_cast<clang::VarDecl>(decl)) {
      std::string name = var_decl->getNameAsString();
      std::string type = var_decl->getType().getCanonicalType().getAsString();
      // add this var to this node
      PlainVar var;
      var.name = name;
      var.type = type;
      ret.push_back(var);
    }
  }
  return ret;
}

std::set<std::string> get_callees(clang::Expr *expr) {
  std::set<std::string> ret;
  if (clang::DeclRefExpr *ref = clang::dyn_cast<clang::DeclRefExpr>(expr)) {
  } else if (clang::UnaryOperator *un = clang::dyn_cast<clang::UnaryOperator>(expr)) {
    clang::Expr *sub = un->getSubExpr();
    std::set<std::string> tmp = get_callees(sub);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::BinaryOperator *bi = clang::dyn_cast<clang::BinaryOperator>(expr)) {
    clang::Expr *lhs = bi->getLHS();
    std::set<std::string> tmp = get_callees(lhs);
    ret.insert(tmp.begin(), tmp.end());
    clang::Expr *rhs = bi->getRHS();
    tmp = get_callees(rhs);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::CallExpr *call = clang::dyn_cast<clang::CallExpr>(expr)) {
    // add callee
    std::string callee = clang::dyn_cast<clang::NamedDecl>(call->getCalleeDecl())->getNameAsString();
    ret.insert(callee);
    for (clang::CallExpr::arg_iterator it=call->arg_begin();it!=call->arg_end();++it) {
      clang::Expr *arg = *it;
      std::set<std::string> tmp = get_callees(arg);
      ret.insert(tmp.begin(), tmp.end());
    }
  } else if (clang::CastExpr *cast = clang::dyn_cast<clang::CastExpr>(expr)) {
    clang::Expr *sub = cast->getSubExpr();
    std::set<std::string> tmp = get_callees(sub);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::ParenExpr *paren = clang::dyn_cast<clang::ParenExpr>(expr)) {
    clang::Expr *sub = paren->getSubExpr();
    std::set<std::string> tmp = get_callees(sub);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::MemberExpr *mem = clang::dyn_cast<clang::MemberExpr>(expr)) {
    clang::Expr *base = mem->getBase();
    std::set<std::string> tmp = get_callees(base);
    ret.insert(tmp.begin(), tmp.end());
  } else if (clang::ConditionalOperator *cond_expr = clang::dyn_cast<clang::ConditionalOperator>(expr)) {
    clang::Expr *cond = cond_expr->getCond();
    clang::Expr *t = cond_expr->getTrueExpr();
    clang::Expr *f = cond_expr->getFalseExpr();
    std::set<std::string> tmp;
    tmp = get_callees(cond);
    ret.insert(tmp.begin(), tmp.end());
    tmp = get_callees(t);
    ret.insert(tmp.begin(), tmp.end());
    tmp = get_callees(f);
    ret.insert(tmp.begin(), tmp.end());
  } else {
    // TODO not supported
  }
  return ret;
}

std::set<std::string> get_callees_for_declstmt(clang::DeclStmt *decl_stmt) {
  std::set<std::string> ret;
  for (auto it=decl_stmt->decl_begin();it!=decl_stmt->decl_end();++it) {
    clang::Decl *decl = *it;
    if (clang::VarDecl *vardecl = clang::dyn_cast<clang::VarDecl>(decl)) {
      if (vardecl->hasInit()) {
        clang::Expr *expr = vardecl->getInit();
        std::set<std::string> tmp = get_callees(expr);
        ret.insert(tmp.begin(), tmp.end());
      }
    }
  }
  return ret;
}

DeclStmt* ClangParser::parseDeclStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::DeclStmt *decl_stmt,
 ASTContext *myctx) {
  clang::SourceRange range = decl_stmt->getSourceRange();
  std::string mytext = rewriter.getRewrittenText(range);
  DeclStmt *ret = new DeclStmt(myctx, mytext, convert_clang_loc(ctx, decl_stmt->getSourceRange()));
  std::set<std::string> used = get_used_vars(decl_stmt);
  std::vector<PlainVar> defined = get_defined_vars(decl_stmt);
  ret->addUsedVar(used);
  for (PlainVar var : defined) {
    ret->addDefinedVar(var.name, var.type);
  }
  // callee
  ret->addCallee(get_callees_for_declstmt(decl_stmt));
  return ret;
}
FunctionDecl *ClangParser::parseFunctionDecl
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::FunctionDecl *func,
 ASTContext *myctx) {
  if (!func->isThisDeclarationADefinition()) return nullptr;
  clang::Stmt *stmt = func->getBody();
  // this does not have a function body
  if (!stmt) return nullptr;
  // this should be a compound stmt
  assert(clang::dyn_cast<clang::CompoundStmt>(stmt));
  clang::CompoundStmt *comp = clang::dyn_cast<clang::CompoundStmt>(stmt);
  Stmt *mycomp = parseCompoundStmt(ctx, rewriter, comp, myctx);

  TokenNode *ReturnTypeNode
    = new TokenNode(myctx, func->getReturnType().getAsString(),
                    convert_clang_loc(ctx, func->getReturnTypeSourceRange()));
  TokenNode *NameNode
    = new TokenNode(myctx, func->getNameAsString(),
                    convert_clang_loc(ctx, func->getNameInfo().getSourceRange()));
  std::vector<std::string> params;
  // FIXME this should have both () and ,
  TokenNode *ParamNode = nullptr;
  if (func->param_size() != 0) {
    std::vector<PlainVar> defined_vars;
    clang::SourceLocation param_begin = func->getParamDecl(0)->getLocStart();
    clang::SourceLocation param_end = func->getParamDecl(func->param_size()-1)->getLocEnd();
    for (clang::FunctionDecl::param_iterator it=func->param_begin();it!=func->param_end();++it) {
      clang::ParmVarDecl *param = *it;
      // param_text += rewriter.getRewrittenText(param->getSourceRange());
      params.push_back(rewriter.getRewrittenText(param->getSourceRange()));
      PlainVar var;
      var.name = param->getNameAsString();
      var.type = param->getType().getCanonicalType().getAsString();
      defined_vars.push_back(var);
    }
    std::string param_text = boost::algorithm::join(params, ", ");
    ParamNode = new TokenNode(myctx, param_text,
                              convert_clang_loc(ctx, param_begin),
                              convert_clang_loc(ctx, param_end));
    for (PlainVar var : defined_vars) {
      ParamNode->addDefinedVar(var.name, var.type);
    }
  }

  FunctionDecl *ret = new FunctionDecl(myctx, func->getName().str(), ReturnTypeNode, NameNode, ParamNode, mycomp,
                                       convert_clang_loc(ctx, func->getSourceRange()));
  return ret;
}
CompoundStmt *ClangParser::parseCompoundStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::CompoundStmt *comp,
 ASTContext *myctx) {
  clang::SourceLocation lbraceloc = comp->getLBracLoc();
  clang::SourceLocation rbraceloc = comp->getRBracLoc();
  SourceLocation mylloc = convert_clang_loc(ctx, lbraceloc);
  SourceLocation myrloc = convert_clang_loc(ctx, rbraceloc);
  TokenNode *lbrace = new TokenNode(myctx, "{", mylloc, mylloc+std::make_pair(0, 1));
  TokenNode *rbrace = new TokenNode(myctx, "}", myrloc, myrloc+std::make_pair(0,1));
  CompoundStmt *ret = new CompoundStmt(myctx, lbrace, rbrace,
                                       convert_clang_loc(ctx, comp->getLocStart()),
                                       convert_clang_loc(ctx, comp->getLocEnd()));
  for (clang::CompoundStmt::body_iterator it=comp->body_begin(); it!=comp->body_end();++it) {
    clang::Stmt *stmt = *it;
    Stmt *mystmt = parseStmt(ctx, rewriter, stmt, myctx);
    if (mystmt) {
      ret->Add(mystmt);
    }
  }
  return ret;
}
Stmt *ClangParser::parseStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::Stmt *stmt,
 ASTContext *myctx) {
  if (!stmt) return nullptr;
  Stmt *ret = nullptr;
  if (clang::DeclStmt *decl_stmt = clang::dyn_cast<clang::DeclStmt>(stmt)) {
    ret = parseDeclStmt(ctx, rewriter, decl_stmt, myctx);
  } else if (clang::CompoundStmt *comp = clang::dyn_cast<clang::CompoundStmt>(stmt)) {
    ret = parseCompoundStmt(ctx, rewriter, comp, myctx);
  } else if (clang::IfStmt *if_stmt = clang::dyn_cast<clang::IfStmt>(stmt)) {
    ret = parseIfStmt(ctx, rewriter, if_stmt, myctx);
  } else if (clang::SwitchStmt *switch_stmt = clang::dyn_cast<clang::SwitchStmt>(stmt)) {
    ret = parseSwitchStmt(ctx, rewriter, switch_stmt, myctx);
  } else if (clang::ForStmt *for_stmt = clang::dyn_cast<clang::ForStmt>(stmt)) {
    ret = parseForStmt(ctx, rewriter, for_stmt, myctx);
  } else if (clang::DoStmt *do_stmt = clang::dyn_cast<clang::DoStmt>(stmt)) {
    ret = parseDoStmt(ctx, rewriter, do_stmt, myctx);
  } else if (clang::WhileStmt *while_stmt = clang::dyn_cast<clang::WhileStmt>(stmt)) {
    ret = parseWhileStmt(ctx, rewriter, while_stmt, myctx);
  } else if (clang::BreakStmt *break_stmt = clang::dyn_cast<clang::BreakStmt>(stmt)) {
    ret = parseBreakStmt(ctx, rewriter, break_stmt, myctx);
  } else if (clang::ContinueStmt *cont_stmt = clang::dyn_cast<clang::ContinueStmt>(stmt)) {
    ret = parseContinueStmt(ctx, rewriter, cont_stmt, myctx);
  } else if (clang::ReturnStmt *ret_stmt = clang::dyn_cast<clang::ReturnStmt>(stmt)) {
    ret = parseReturnStmt(ctx, rewriter, ret_stmt, myctx);
  } else if (clang::Expr *expr = clang::dyn_cast<clang::Expr>(stmt)){
    ret = parseExprStmt(ctx, rewriter, expr, myctx);
  } else if (clang::NullStmt *null_stmt = clang::dyn_cast<clang::NullStmt>(stmt)) {
    ret = parseNullStmt(ctx, rewriter, null_stmt, myctx);
  } else {
    std::string name = stmt->getStmtClassName();
    // std::cerr << "Stmt of kind " << name << " not supported." << "\n";
    // goto is fine
    // but FIXME label?
    ret = parseDummyStmt(ctx, rewriter, stmt, myctx);
  }
  return ret;
};

ReturnStmt *ClangParser::parseReturnStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::ReturnStmt *ret_stmt,
 ASTContext *myctx) {
  clang::Expr *expr = ret_stmt->getRetValue();
  Expr *myexpr = parseExpr(ctx, rewriter, expr, myctx);
  clang::SourceLocation loc = ret_stmt->getReturnLoc();
  SourceLocation myloc = convert_clang_loc(ctx, loc);
  ReturnStmt *ret = new ReturnStmt(myctx,
                                   new TokenNode(myctx, "return", myloc, myloc +std::make_pair(0, 6)),
                                   myexpr,
                                   convert_clang_loc(ctx, ret_stmt->getLocStart()),
                                   convert_clang_loc(ctx, ret_stmt->getLocEnd()));
  return ret;
}
IfStmt *ClangParser::parseIfStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::IfStmt *if_stmt,
 ASTContext *myctx) {
  clang::Expr *cond = if_stmt->getCond();
  Expr *mycond = parseExpr(ctx, rewriter, cond, myctx);
  // this expr might be empty, when
  // it is a function call
  // the function is inside a header file
  // the #include is needed for clang to parse it
  // otherwise it will be a OpaqueValueExpr, which does not have a valid sloc
  clang::Stmt *then_stmt = if_stmt->getThen();
  clang::Stmt *else_stmt = if_stmt->getElse();
  Stmt *mythen = parseStmt(ctx, rewriter, then_stmt, myctx);
  Stmt *myelse = parseStmt(ctx, rewriter, else_stmt, myctx);

  clang::SourceLocation ifloc = if_stmt->getIfLoc();
  SourceLocation myifloc = convert_clang_loc(ctx, ifloc);
  clang::SourceLocation elseloc = if_stmt->getElseLoc();
  SourceLocation myelseloc = convert_clang_loc(ctx, elseloc);

  IfStmt *ret = new IfStmt(myctx, mycond, mythen, myelse,
                           new TokenNode(myctx, "if", myifloc, myifloc+std::make_pair(0,2)),
                           myelse? new TokenNode(myctx, "else", myelseloc, myelseloc+std::make_pair(0,4)) : nullptr,
                           convert_clang_loc(ctx, if_stmt->getLocStart()),
                           convert_clang_loc(ctx, if_stmt->getLocEnd()));
  return ret;
}
SwitchStmt *ClangParser::parseSwitchStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::SwitchStmt *switch_stmt,
 ASTContext *myctx) {
  clang::Expr *cond = switch_stmt->getCond();
  Expr *mycond = parseExpr(ctx, rewriter, cond, myctx);
  SwitchStmt *ret = new SwitchStmt(myctx, mycond, nullptr,
                                   new TokenNode(myctx, "switch", convert_clang_loc(ctx, switch_stmt->getSwitchLoc(), "switch")),
                                   convert_clang_loc(ctx, switch_stmt->getSourceRange()));
  // case
  clang::SwitchCase *switch_case =  switch_stmt->getSwitchCaseList();
  while (switch_case) {
    if (clang::CaseStmt *case_stmt = clang::dyn_cast<clang::CaseStmt>(switch_case)) {
      CaseStmt *mycase = parseCaseStmt(ctx, rewriter, case_stmt, myctx);
      ret->AddCase(mycase);
    } else if (clang::DefaultStmt *def_stmt = clang::dyn_cast<clang::DefaultStmt>(switch_case)) {
      DefaultStmt *mydef = parseDefaultStmt(ctx, rewriter, def_stmt, myctx);
      ret->AddCase(mydef);
    }
    switch_case = switch_case->getNextSwitchCase();
  }
  return ret;
}


CaseStmt *ClangParser::parseCaseStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::CaseStmt *case_stmt,
 ASTContext *myctx) {
  // FIXME cond?? rhs?
  clang::Expr *expr = case_stmt->getLHS();
  // std::cout << expr << "\n";
  Expr *myexpr = parseExpr(ctx, rewriter, expr, myctx);
  // std::cout << myexpr->getText() << "\n";
  assert(myexpr);
  CaseStmt *ret = new CaseStmt(myctx, myexpr,
                               new TokenNode(myctx, "case", convert_clang_loc(ctx, case_stmt->getCaseLoc(), "case")),
                               convert_clang_loc(ctx, case_stmt->getLocStart()),
                               convert_clang_loc(ctx, case_stmt->getLocEnd()));
  // FIXME
  // get substatement
  clang::Stmt *sub = case_stmt->getSubStmt();
  Stmt *mysub = parseStmt(ctx, rewriter, sub, myctx);
  ret->Add(mysub);
  return ret;
}
DefaultStmt *ClangParser::parseDefaultStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::DefaultStmt *def_stmt,
 ASTContext *myctx) {

  DefaultStmt *ret = new DefaultStmt(myctx,
                                     new TokenNode(myctx, "default", convert_clang_loc(ctx, def_stmt->getDefaultLoc(), "default")),
                                     convert_clang_loc(ctx, def_stmt->getLocStart()),
                                     convert_clang_loc(ctx, def_stmt->getLocEnd()));
  clang::Stmt *sub = def_stmt->getSubStmt();
  Stmt *mysub = parseStmt(ctx, rewriter, sub, myctx);
  ret->Add(mysub);
  return ret;
}
WhileStmt *ClangParser::parseWhileStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::WhileStmt *while_stmt,
 ASTContext *myctx) {
  clang::Expr *cond = while_stmt->getCond();
  Expr *mycond = parseExpr(ctx, rewriter, cond, myctx);
  clang::Stmt *body = while_stmt->getBody();
  Stmt *mybody = parseStmt(ctx, rewriter, body, myctx);
  WhileStmt *ret = new WhileStmt(myctx, mycond, mybody,
                                 new TokenNode(myctx, "while", convert_clang_loc(ctx, while_stmt->getWhileLoc(), "while")),
                                 convert_clang_loc(ctx, while_stmt->getLocStart()),
                                 convert_clang_loc(ctx, while_stmt->getLocEnd()));
  return ret;
}
ForStmt *ClangParser::parseForStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::ForStmt *for_stmt,
 ASTContext *myctx) {
  clang::Stmt *init = for_stmt->getInit();
  clang::Expr *cond = for_stmt->getCond();
  clang::Expr *inc = for_stmt->getInc();
  clang::Stmt *body = for_stmt->getBody();
  // FIXME expr or stmt?
  // Stmt *myinit = parseStmt(ctx, rewriter, init, myctx);
  // Expr *myinit = nullptr;
  Expr *myinit = parseForInit(ctx, rewriter, init, myctx);
  Expr *mycond = parseExpr(ctx, rewriter, cond, myctx);
  Expr *myinc = parseExpr(ctx, rewriter, inc, myctx);
  Stmt *mybody = parseStmt(ctx, rewriter, body, myctx);
  ForStmt *ret = new ForStmt(myctx, myinit, mycond, myinc, mybody,
                             new TokenNode(myctx, "for", convert_clang_loc(ctx, for_stmt->getForLoc(), "for")),
                             convert_clang_loc(ctx, for_stmt->getLocStart()),
                             convert_clang_loc(ctx, for_stmt->getLocEnd()));
  return ret;
}
DoStmt *ClangParser::parseDoStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::DoStmt *do_stmt,
 ASTContext *myctx) {
  clang::Expr *cond = do_stmt->getCond();
  clang::Stmt *body = do_stmt->getBody();
  Expr *mycond = parseExpr(ctx, rewriter, cond, myctx);
  Stmt *mybody = parseStmt(ctx, rewriter, body, myctx);
  DoStmt *ret = new DoStmt(myctx, mycond, mybody,
                           new TokenNode(myctx, "do", convert_clang_loc(ctx, do_stmt->getDoLoc(), "do")),
                           new TokenNode(myctx, "while", convert_clang_loc(ctx, do_stmt->getWhileLoc(), "while")),
                           convert_clang_loc(ctx, do_stmt->getLocStart()),
                           convert_clang_loc(ctx, do_stmt->getLocEnd()));
  return ret;
}
Expr *ClangParser::parseExpr
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::Expr *expr,
 ASTContext *myctx) {
  if (!expr) return nullptr;
  clang::SourceRange range = expr->getSourceRange();
  std::string text = rewriter.getRewrittenText(range);
  // fixing include file error
  // TODO fix the line number inconsistent (should only occur when using macro)
  // TODO consider use printPretty to replace all rewriter

  // expr->dumpPretty(*ctx);
  std::string s;
  llvm::raw_string_ostream rss(s);
  expr->printPretty(rss, nullptr, clang::PrintingPolicy(ctx->getLangOpts()));
  // CAUTAION: need to "flush" to update s
  rss.flush();
  text = s;

  // expr->getLocStart().dump(ctx->getSourceManager());
  // expr->getLocEnd().dump(ctx->getSourceManager());
  // adding includes does solve half of the opaque value
  #if 0
  if (clang::OpaqueValueExpr *poa_expr = clang::dyn_cast<clang::OpaqueValueExpr>(expr)) {
    // std::cout << "opaque: " << text << " "
    //           << convert_clang_loc(ctx, poa_expr->getLocStart()) << " "
    //           << convert_clang_loc(ctx, poa_expr->getLocEnd()) << "\n";
    // clang::Expr *src_expr = poa_expr->getSourceExpr();
    // These does not output anything
    // poa_expr->getExprLoc().dump(ctx->getSourceManager());
    // poa_expr->getLocation().dump(ctx->getSourceManager());
    // poa_expr->getLocStart().dump(ctx->getSourceManager());
    // poa_expr->getLocEnd().dump(ctx->getSourceManager());

    // if (src_expr) {
    //   std::cout << "exists" << "\n";
    //   src_expr->dump();
    //   // src_expr->getLocStart();
    //   src_expr->getLocStart().dump(ctx->getSourceManager());
    //   src_expr->getLocEnd().dump(ctx->getSourceManager());
    // } else {
    //   std::cout << "non exists" << "\n";
    // }
  }
  #endif
  // expr->dump();
  Expr *ret = new Expr(myctx, text,
                       convert_clang_loc(ctx, expr->getLocStart()),
                       convert_clang_loc(ctx, expr->getLocEnd()));
  // used var
  std::set<clang::VarDecl*> decls = get_referred_var_decl(expr);
  for (clang::VarDecl *decl : decls) {
    std::string name = decl->getNameAsString();
    ret->addUsedVar(name);
  }
  // callee
  ret->addCallee(get_callees(expr));
  return ret;
}
ExprStmt *ClangParser::parseExprStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::Expr *expr,
 ASTContext *myctx) {
  clang::SourceRange range = expr->getSourceRange();
  std::string text = rewriter.getRewrittenText(range);
  utils::trim(text);
  assert(text.back() != ';');
  text.push_back(';');
  ExprStmt *ret = new ExprStmt(myctx, text,
                       convert_clang_loc(ctx, expr->getLocStart()),
                       convert_clang_loc(ctx, expr->getLocEnd()));
  // used var
  std::set<clang::VarDecl*> decls = get_referred_var_decl(expr);
  for (clang::VarDecl *decl : decls) {
    std::string name = decl->getNameAsString();
    ret->addUsedVar(name);
  }
  // callee
  ret->addCallee(get_callees(expr));
  return ret;
}

ExprStmt *ClangParser::parseNullStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::NullStmt *null_stmt,
 ASTContext *myctx) {
  ExprStmt *ret = new ExprStmt(myctx, "",
                               convert_clang_loc(ctx, null_stmt->getLocStart()),
                               convert_clang_loc(ctx, null_stmt->getLocEnd()));
  return ret;
}
ExprStmt *ClangParser::parseDummyStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter, clang::Stmt *stmt, ASTContext *myctx) {
  ExprStmt *ret = new ExprStmt(myctx, "",
                               convert_clang_loc(ctx, stmt->getLocStart()),
                               convert_clang_loc(ctx, stmt->getLocEnd()));
  return ret;
}

/**
 * This is for "for init", as it should be expr, but clang parse it as Stmt
 */
Expr *ClangParser::parseForInit
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::Stmt *init,
 ASTContext *myctx) {
  if (!init) return nullptr;
  clang::SourceRange range = init->getSourceRange();
  std::string text = rewriter.getRewrittenText(range);
  utils::trim(text);
  // std::cout << text << "\n";
  // assert(text.back() == ';');
  if (text.back() == ';') {
    text.pop_back();
  }
  Expr *ret = new Expr(myctx, text,
                       convert_clang_loc(ctx, range));
  // get symbol
  if (clang::DeclStmt *decl_stmt = clang::dyn_cast<clang::DeclStmt>(init)) {
    ret->addUsedVar(get_used_vars(decl_stmt));
    for (PlainVar var : get_defined_vars(decl_stmt)) {
      ret->addDefinedVar(var.name, var.type);
    }
  }
  // callee
  // it should be a declstmt, or a expr
  if (clang::DeclStmt *decl_stmt = clang::dyn_cast<clang::DeclStmt>(init)) {
    ret->addCallee(get_callees_for_declstmt(decl_stmt));
  } else if (clang::Expr *expr = clang::dyn_cast<clang::Expr>(init)) {
    ret->addCallee(get_callees(expr));
  }
  return ret;
}

BreakStmt *ClangParser::parseBreakStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::BreakStmt *break_stmt,
 ASTContext *myctx) {
  BreakStmt *ret = new BreakStmt(myctx,
                                 convert_clang_loc(ctx, break_stmt->getLocStart()),
                                 convert_clang_loc(ctx, break_stmt->getLocEnd()));
  return ret;
}
ContinueStmt *ClangParser::parseContinueStmt
(clang::ASTContext *ctx, clang::Rewriter &rewriter,
 clang::ContinueStmt *cont_stmt,
 ASTContext *myctx) {
  ContinueStmt *ret = new ContinueStmt(myctx,
                                       convert_clang_loc(ctx, cont_stmt->getLocStart()),
                                       convert_clang_loc(ctx, cont_stmt->getLocEnd()));
  return ret;
}

class ParserConsumer : public clang::ASTConsumer {
public:
  explicit ParserConsumer(clang::ASTContext *Context, clang::Rewriter &rewriter, ASTContext **retctx, llvm::StringRef InFile)
    : rewriter(rewriter), retctx(retctx), InFile(InFile) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    // Then I don't need source code at all
    clang::TranslationUnitDecl *unit = Context.getTranslationUnitDecl();
    ASTContext *myctx = new ASTContext(InFile.str());
    TranslationUnitDecl *myunit = ClangParser::parseTranslationUnitDecl(&Context, rewriter, unit, myctx);
    myctx->setTranslationUnitDecl(myunit);
    if (!retctx) {
      std::cerr << "retctx is empty" << "\n";
      exit(1);
    }
    if (retctx) {*retctx = myctx;}
  }
private:
  clang::Rewriter &rewriter;
  ASTContext **retctx=nullptr;
  llvm::StringRef InFile;
};

class ParserAction : public clang::ASTFrontendAction {
public:
  virtual std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    // suppress compiler diagnostics
    Compiler.getDiagnostics().setClient(new clang::IgnoringDiagConsumer());
    rewriter.setSourceMgr(Compiler.getSourceManager(), Compiler.getLangOpts());
    // return consumer
    return std::unique_ptr<clang::ASTConsumer>
      (new ParserConsumer(&Compiler.getASTContext(), rewriter, &retctx, InFile));
  }
  ASTContext *getMyASTContext() {
    return retctx;
  }
private:
  clang::Rewriter rewriter;
  ASTContext *retctx = nullptr;
};

ASTContext* create_by_action(fs::path file) {
  // read from file
  std::string code = utils::read_file(file);
  ParserAction *action = new ParserAction();
  clang::tooling::runToolOnCode(action, code, file.string());
  // Now I should be able to get data from action
  ASTContext *ctx = action->getMyASTContext();
  return ctx;
}


ASTContext *ClangParser::parse(fs::path file) {
  // std::cout << "clang parser pasing " << file << "\n";
  // ASTContext *ctx = new ASTContext(file.string());
  ASTContext *ctx = create_by_action(file);
  // ASTContext *ctx = create_by_tool_action(file);
  ctx->createSymbolTable();
  return ctx;
}

#if 0
void create_by_test(fs::path file) {
  CompilerInstance compiler;
  DiagnosticOptions options;
  compiler.createDisagnostics();
  CompilerInvocation *invocation = new CompilerInvocation;
  CompilerInvocation::CreateFromArgs(*invocation, NULL, NULL, compiler.getDiagnostics());
  compiler.setInvocation(invocation);
  std::shared_ptr<clang::TargetOptions> pto = std::make_shared<clang::TargetOptions>();
  pto->Triple = llvm::sys::getDefaultTargetTriple();
  TargetInfo *pti = TargetInfo::CreateTargetInfo(compiler.getDiagnostics(), pto);
  compiler.setTarget(pti);
  compiler.createFileManager();
  compiler.createSourceManager(compiler.getFileManager());
  HeaderSearchOptions &headerSearchOptions = compiler.getHeaderSearchOpts();

  LangOptions langOpts;
  langOpts.GNUMode = 1;
  invocation->setLangDefaults(langOpts);
  compiler.createPreprocessor(clang::TU_Complete);
  compiler.getPreprocessorOpts().UsePredefines = false;
  compiler.createASTContext();
}

static llvm::cl::OptionCategory MyToolCategory("mytool options");

#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"

/**
 * For some reason, I cannot get the compiler find stdbool.h But
 * clang-check can find. That's very wired.  So i'm not going to worry
 * about that because what clang gives me is always the valid parts.
 * (maybe sometime empty, like if() bugs)
 */
ASTContext *create_by_tool_action(fs::path file) {
  llvm::sys::PrintStackTraceOnErrorSignal("dummy");

  // Initialize targets for clang module support.
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  std::vector<std::string> Sources;
  int argc = 2;
  const char *argv[2];
  argv[0] = "tool";
  // argv[1] = "-I/usr/lib/gcc/x86_64-pc-linux-gnu/7.1.1/include/";
  argv[1] = file.string().c_str();
  // argv[2] = "--";
  // argv[2] = "-I/usr/include/linux";
  // argv[3] = "-I/usr/lib/clang/5.0.0/include";
  // argv[3] = "-I/usr/include";
  Sources.push_back(file.string());
  clang::tooling::CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  clang::tooling::ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

  // Clear adjusters because -fsyntax-only is inserted by the default chain.
  Tool.clearArgumentsAdjusters();
  Tool.appendArgumentsAdjuster(clang::tooling::getClangStripOutputAdjuster());

  // Running the analyzer requires --analyze. Other modes can work with the
  // -fsyntax-only option.
  Tool.appendArgumentsAdjuster(getInsertArgumentAdjuster(
      "-fsyntax-only", clang::tooling::ArgumentInsertPosition::BEGIN));
  
  // Tool.setDiagnosticConsumer(new clang::IgnoringDiagConsumer());
  std::vector<std::unique_ptr<clang::ASTUnit> > ASTUnits;

  std::cout << "going to run tool" << "\n";
  Tool.run(clang::tooling::newFrontendActionFactory<ParserAction>().get());
  // should set diagnostic before this
  std::cout << "Going to build AST" << "\n";
  Tool.buildASTs(ASTUnits);
  std::cout << ASTUnits.size() << "\n";
  assert(ASTUnits.size() == 1);

  ASTContext *ret;
  for (std::unique_ptr<clang::ASTUnit> &astunit : ASTUnits) {
    clang::Rewriter rewriter;
    clang::ASTContext &ast = astunit->getASTContext();
    clang::TranslationUnitDecl *unit = ast.getTranslationUnitDecl();
    ASTContext *myctx = new ASTContext("dummy");
    rewriter.setSourceMgr(ast.getSourceManager(), ast.getLangOpts());
    TranslationUnitDecl *myunit = ClangParser::parseTranslationUnitDecl(&ast, rewriter, unit, myctx);
    myctx->setTranslationUnitDecl(myunit);
    ret = myctx;
  }
  return ret;
}

// Unused
void create_by_build(fs::path file) {
  std::string code = utils::read_file(file);
  // std::unique_ptr<clang::ASTUnit> ast =
  //   clang::tooling::buildASTFromCode(code, file.string());
  std::vector<std::string> args = {"-I/usr/include/linux"};
  std::unique_ptr<clang::ASTUnit> ast =
    clang::tooling::buildASTFromCodeWithArgs(code, args, file.string());
  // clang::ASTContext *ctx = &(ast->getASTContext());
  // setting ignore here doesn't work because the error is already thrown
  // ctx->getDiagnostics().setClient(new clang::IgnoringDiagConsumer());
  // clang::TranslationUnitDecl *unit = ctx->getTranslationUnitDecl();
  // parseTranslationUnitDecl(unit);
}

// void create_by_tool(fs::path file) {
//   ToolInvocation invo()
// }


#endif
