#include "helium/type/SnippetAction.h"
#include "helium/utils/FSUtils.h"

#include "helium/parser/SourceLocation.h"

#include "helium/type/Snippet.h"

#include <iostream>

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Rewrite/Core/Rewriter.h"

static std::vector<Snippet*> snippets;


SourceLocation convertLocation(clang::ASTContext *ctx, clang::SourceLocation loc) {
  clang::FullSourceLoc fullBeginLoc = ctx->getFullLoc(loc);
  int line = fullBeginLoc.getSpellingLineNumber();
  int column = fullBeginLoc.getSpellingColumnNumber();
  // int line = fullBeginLoc.getExpansionLineNumber();
  // int column = fullBeginLoc.getExpansionColumnNumber();
  return SourceLocation(line, column);
}

class SnippetVisitor
  : public clang::RecursiveASTVisitor<SnippetVisitor> {
public:
  explicit SnippetVisitor(clang::ASTContext *Context)
    : Context(Context) {
    clang::SourceManager &sourceManager = Context->getSourceManager();
    const clang::FileEntry *entry = sourceManager.getFileEntryForID(sourceManager.getMainFileID());
    Filename = entry->getName();
  }

  bool VisitFunctionDecl(clang::FunctionDecl *func_decl) {
    clang::FunctionDecl *def = func_decl->getDefinition();
    std::string name = func_decl->getNameInfo().getName().getAsString();
    // llvm::errs() << "visiting func " << name << "\n";
    clang::SourceLocation begin = func_decl->getLocStart();
    clang::SourceLocation end = func_decl->getLocEnd();
    if (!Context->getSourceManager().isInMainFile(begin)) return true;
    // I would also extract the declaration
    // TESTME body is empty, then does it have a body?
    // TESTME it includes function prototype or not?
    Snippet *s = nullptr;
    if (def == func_decl) {
      clang::SourceLocation body_begin = func_decl->getBody()->getLocStart();
      s = new FunctionSnippet(name, Filename,
                                  convertLocation(Context, begin),
                                  convertLocation(Context, end),
                                  convertLocation(Context, body_begin));
      snippets.push_back(s);
    } else {
      s = new FunctionDeclSnippet(name, Filename,
                                      convertLocation(Context, begin),
                                      convertLocation(Context, end));
      snippets.push_back(s);
    }
    return true;
  }
  // bool WalkUpFromFunctionDecl(clang::FunctionDecl *func) {
  //   std::string name = func->getNameInfo().getName().getAsString();
  //   llvm::errs() << "work up from func " << name << "\n";
  //   // FIXME check if the function name is the same
  //   clang::FunctionDecl *def = func->getDefinition();
  //   if (def == func) {
  //     infunc = false;
  //   }
  //   VisitFunctionDecl(func);
  //   return true;
  // }
  // BINGO!! This can prevent the traverse to the children
  bool TraverseFunctionDecl(clang::FunctionDecl *func) {
    std::string name = func->getNameInfo().getName().getAsString();
    // llvm::errs() << "traverse func " << name << "\n";
    WalkUpFromFunctionDecl(func);
    // I'm not go into the function
    return true;
  }
  bool VisitVarDecl(clang::VarDecl *var_decl) {
    // FIXME global or file?
    if (!var_decl->isFileVarDecl()) return true;
    if (var_decl->hasExternalStorage() && !var_decl->hasInit()) return true;
    std::string name = var_decl->getName();
    // llvm::errs() << "visiting var " << name << "\n";
    // clang::SourceRange range = var_decl->getSourceRange();
    // clang::SourceLocation begin = range.getBegin();
    // clang::SourceLocation end = range.getEnd();
    clang::SourceLocation begin = var_decl->getLocStart();
    clang::SourceLocation end = var_decl->getLocEnd();
    if (!Context->getSourceManager().isInMainFile(begin)) return true;
    // clang::SourceLocation loc = var_decl->getLocation();
      
    Snippet *s = new VarSnippet(name, Filename,
                                        convertLocation(Context, begin),
                                        convertLocation(Context, end));
    snippets.push_back(s);
    return true;
  }
  bool VisitTypedefDecl (clang::TypedefDecl *decl) {
    std::string name = decl->getName();
    // clang::SourceRange range = decl->getSourceRange();
    // clang::SourceLocation begin = range.getBegin();
    // clang::SourceLocation end = range.getEnd();
    clang::SourceLocation begin = decl->getLocStart();
    clang::SourceLocation end = decl->getLocEnd();
    if (!Context->getSourceManager().isInMainFile(begin)) return true;
    Snippet *s = new TypedefSnippet(name, Filename,
                                            convertLocation(Context, begin),
                                            convertLocation(Context, end));
    snippets.push_back(s);
    return true;
  }
  bool VisitEnumDecl(clang::EnumDecl *decl) {
    clang::EnumDecl *def = decl->getDefinition();
    if (def == decl) {
      std::string name = decl->getName();
      // clang::SourceRange range = decl->getSourceRange();
      // clang::SourceLocation begin = range.getBegin();
      // clang::SourceLocation end = range.getEnd();
      clang::SourceLocation begin = decl->getLocStart();
      clang::SourceLocation end = decl->getLocEnd();
      if (!Context->getSourceManager().isInMainFile(begin)) return true;
      EnumSnippet *s = new EnumSnippet(name, Filename,
                                               convertLocation(Context, begin),
                                               convertLocation(Context, end));
      // all fields
      for (auto it=decl->enumerator_begin(), end=decl->enumerator_end(); it!=end; ++it) {
        std::string name = (*it)->getName();
        // llvm::errs() << "Field: " << name << "\n";
        s->addField(name);
      }
      snippets.push_back(s);
    }
    return true;
  }
  // struct/union/class
  bool VisitRecordDecl (clang::RecordDecl *decl) {
    // if (infunc) return true;
    clang::RecordDecl *def = decl->getDefinition();
    std::string name = decl->getName();
    // llvm::errs() << "visiting record decl " << name << "\n";
    // clang::SourceRange range = decl->getSourceRange();
    // clang::SourceLocation begin = range.getBegin();
    // clang::SourceLocation end = range.getEnd();
    clang::SourceLocation begin = decl->getLocStart();
    clang::SourceLocation end = decl->getLocEnd();
    if (!Context->getSourceManager().isInMainFile(begin)) return true;
    // name can be empty, this is an anonymous record. It must have a typedef or var to enclose it.
    Snippet *s = nullptr;
    if (def == decl) {
      s = new RecordSnippet(name, Filename,
                                convertLocation(Context, begin),
                                convertLocation(Context, end));
    } else {
      s = new RecordDeclSnippet(name, Filename,
                                    convertLocation(Context, begin),
                                    convertLocation(Context, end));
    }
    snippets.push_back(s);
    return true;
  }

  bool VisitCallExpr (clang::CallExpr *call) {
    return true;
  }

private:
  clang::ASTContext *Context;
  std::string Filename;
};


void parse_macro(clang::ASTContext &Context, clang::CompilerInstance &compiler, clang::Rewriter &rewriter) {
  // process macros here
  clang::SourceManager &sourceManager = Context.getSourceManager();
  const clang::FileEntry *entry = sourceManager.getFileEntryForID(sourceManager.getMainFileID());
  std::string filename = entry->getName();
    
  clang::Preprocessor &prep = compiler.getPreprocessor();
  for (auto it=prep.macro_begin(false), itend=prep.macro_end(false);it!=itend;++it) {
    const clang::IdentifierInfo *idinfo = (*it).first;
    clang::MacroInfo *macroinfo = prep.getMacroInfo(idinfo);
    // TESTME #define hello(x) world(x)
    // What is the name? hello?
    if (idinfo && macroinfo) {
      std::string macroname = idinfo->getName();
      clang::SourceLocation start = macroinfo->getDefinitionLoc();
      clang::SourceLocation end = macroinfo->getDefinitionEndLoc();
      clang::SourceRange range(start, end);
      if (Context.getSourceManager().isWrittenInMainFile(start)) {
        std::string text = rewriter.getRewrittenText(range);
        // std::cout << "Got Macro: " << macroname << "\n";
        // std::cout << text << "\n";
        Snippet *s = new MacroSnippet(macroname, filename,
                                      convertLocation(&Context, start),
                                      convertLocation(&Context, end));
        snippets.push_back(s);
      }
    }
  }
}

class SnippetConsumer : public clang::ASTConsumer {
public:
  explicit SnippetConsumer(clang::ASTContext *Context,
                           clang::CompilerInstance &compiler,
                           clang::Rewriter &rewriter)
    : Visitor(Context), compiler(compiler), rewriter(rewriter) {}
  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    // parsing macro here still has problems:
    // 1. If there are multiple ones, i don't know how to choose
    // 2. clang does not always give me the correct end location
    // parse_macro(Context, compiler, rewriter);
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }
private:
  SnippetVisitor Visitor;
  clang::CompilerInstance &compiler;
  clang::Rewriter &rewriter;
};

class SnippetAction : public clang::ASTFrontendAction {
public:
  virtual std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    // suppress compiler diagnostics
    Compiler.getDiagnostics().setClient(new clang::IgnoringDiagConsumer());
    rewriter.setSourceMgr(Compiler.getSourceManager(), Compiler.getLangOpts());
    return std::unique_ptr<clang::ASTConsumer>
      (new SnippetConsumer(&Compiler.getASTContext(), Compiler, rewriter));
  }
private:
  clang::Rewriter rewriter;
};

std::vector<Snippet*> clang_parse_file_for_snippets(fs::path file) {
  snippets.clear();
  clang::tooling::runToolOnCode(new SnippetAction,
                                utils::read_file(file.string()),
                                file.string());
  return snippets;
}
