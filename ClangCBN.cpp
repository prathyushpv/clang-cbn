// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/ADT/StringRef.h"
#include <iostream>
#include <string>
#include <sstream>
#include <list>
#include <algorithm>

using namespace clang::tooling;
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;

SourceLocation initialLocation;
ASTContext *astContext;

std::list <const FunctionDecl *> rewrittenFunctions;

class RewrittenCalls{
    struct call_closures{
        const CallExpr* expr;
        std::string closure_list;
      };
    std::list <call_closures> functionList;

  public:
    void addFunction(const CallExpr* callExpr, std::string closures){
      struct call_closures c;
      c.expr = callExpr;
      c.closure_list = "";
      c.closure_list += closures;
      functionList.push_back(c);
    }

    bool isPresent(const CallExpr * callExpr){
      for (std::list<call_closures>::iterator it = functionList.begin(); it != functionList.end(); ++it){
        if (it->expr == callExpr)
          return true;
      }
      return false;
    }
    std::string getClosures(const CallExpr * callExpr){
      for (std::list<call_closures>::iterator it = functionList.begin(); it != functionList.end(); ++it){
        if (it->expr == callExpr)
          return it->closure_list;
      }
      return std::string("");
    }
} rewrittenCalls;

bool isaParam(const FunctionDecl *functiondecl, const DeclRefExpr* declRef){
  ArrayRef< ParmVarDecl * > params = functiondecl->parameters();
  for(unsigned int paramIndex=0;paramIndex< params.size();paramIndex++){
    if(declRef->getDecl () == (ValueDecl *)params[paramIndex])
    {
      return true;
    }
  }
  return false;
}

class CodeGenerator{
public:
    void VisitExpr(const Expr *expr)
    {
      expr = expr->IgnoreImpCasts ();

      // check if this expression contains a binary operation
      if (expr && isa<clang::BinaryOperator> (expr)) {
        const BinaryOperator *biOp = (const BinaryOperator *) expr;

        // get the lhs and rhs of the operator
        Expr *lhs = biOp->getLHS ();
        Expr *rhs = biOp->getRHS ();
        // recursively visit each one
        VisitExpr (lhs);
        std::cout<<biOp -> getOpcodeStr ().str();
        VisitExpr (rhs);

      }
      if (expr && isa<clang::ArraySubscriptExpr> (expr)) {
        const ArraySubscriptExpr *aSE = (const ArraySubscriptExpr *)expr;
        const Expr *lhs = aSE->getLHS ();
        const Expr *rhs = aSE->getRHS ();
        VisitExpr (lhs);
        std::cout<<"[";
        VisitExpr (rhs);
        std::cout<<"]";
      }
        // check other kinds of expr,
        // since my test code is simple, the recursion stops when it find DeclRefExpr

      else if (expr && isa<clang::DeclRefExpr> (expr)) {
        const DeclRefExpr *declRef = (const DeclRefExpr *) expr;
        // get a var name
        std::string name = (declRef->getNameInfo ()).getName ().getAsString ();
        std::cout << name;
      }
    }

    std::string generateThunkExpression(const Expr *expr, int &closureCount){
      std::string thunkExpression;
      expr = expr->IgnoreImpCasts ();

      // check if this expression contains a binary operation
      if (expr && isa<clang::BinaryOperator> (expr)) {
        const BinaryOperator *biOp = (const BinaryOperator *) expr;

        // get the lhs and rhs of the operator
        Expr *lhs = biOp->getLHS ();
        Expr *rhs = biOp->getRHS ();
        // recursively visit each one
        thunkExpression += generateThunkExpression (lhs, closureCount);
        //std::cout<<biOp -> getOpcodeStr ().str();
        thunkExpression += biOp->getOpcodeStr().str();
        thunkExpression += generateThunkExpression (rhs, closureCount);

      }
      if (expr && isa<clang::ConditionalOperator> (expr)) {
        const ConditionalOperator *condOp = (const ConditionalOperator *) expr;
        Expr *condExp = condOp->getCond ();
        Expr *trueExp = condOp->getTrueExpr ();
        Expr *falseExp = condOp->getFalseExpr ();
        // recursively visit each one
        thunkExpression += generateThunkExpression (condExp, closureCount);
        thunkExpression += "?";
        thunkExpression += generateThunkExpression (trueExp, closureCount);
        thunkExpression += ":";
        thunkExpression += generateThunkExpression (falseExp, closureCount);
      }
      if (expr && isa<clang::ArraySubscriptExpr> (expr)) {
        const ArraySubscriptExpr *aSE = (const ArraySubscriptExpr *)expr;
        const Expr *lhs = aSE->getLHS ();
        const Expr *rhs = aSE->getRHS ();
        thunkExpression += generateThunkExpression (lhs, closureCount);
        thunkExpression += "[";
        thunkExpression += generateThunkExpression (rhs, closureCount);
        thunkExpression += "]";
      }
        // check other kinds of expr,
        // since my test code is simple, the recursion stops when it find DeclRefExpr

      else if (expr && isa<clang::DeclRefExpr> (expr)) {
        const DeclRefExpr *declRef = (const DeclRefExpr *) expr;
        // get a var name
        std::string name = (declRef->getNameInfo ()).getName ().getAsString ();
        if (isa<clang::ArrayType> (declRef->getType ()))
          thunkExpression += "env->" + name;
        else
          thunkExpression += "*env->" + name;
      }

      else if (expr && isa<clang::IntegerLiteral> (expr)) {
        const IntegerLiteral *integerLiteral = (const IntegerLiteral *) expr;
        std::string TypeS;
        llvm::raw_string_ostream s (TypeS);
        clang::LangOptions LangOpts;
        LangOpts.CPlusPlus = true;
        clang::PrintingPolicy Policy(LangOpts);
        integerLiteral->printPretty (s, 0, Policy);
        thunkExpression += s.str();
      }

      else if (expr && isa<clang::FloatingLiteral> (expr)) {
        const FloatingLiteral *floatingLiteral = (const FloatingLiteral *) expr;
        std::string TypeS;
        llvm::raw_string_ostream s (TypeS);
        clang::LangOptions LangOpts;
        LangOpts.CPlusPlus = true;
        clang::PrintingPolicy Policy(LangOpts);
        floatingLiteral->printPretty (s, 0, Policy);
        thunkExpression += s.str();
      }
      else if (expr && isa<clang::CallExpr> (expr)) {
        const CallExpr *callExpr = (const CallExpr *) expr;
        const FunctionDecl *functionDecl = callExpr->getDirectCallee ();
        std::string funcName = functionDecl->getNameInfo ().getName ().getAsString ();
        std::string argList = "";
        unsigned int argNum = callExpr->getNumArgs ();
        for (unsigned i = 0; i < argNum; i++) {
          argList += "env->c_" + std::to_string(closureCount++) + ", ";
        }
        argList = argList.substr(0, argList.size()-2);
        thunkExpression += "env->" + funcName + "(" + argList + ")";
      }
      return thunkExpression;
    }

    std::string generateEnvMembers(const Expr *expr, std::set<std::string> &members, int &closureCount){
      std::string envMembers;
      expr = expr->IgnoreImpCasts ();

      // check if this expression contains a binary operation
      if (expr && isa<clang::BinaryOperator> (expr)) {
        const BinaryOperator *biOp = (const BinaryOperator *) expr;

        // get the lhs and rhs of the operator
        Expr *lhs = biOp->getLHS ();
        Expr *rhs = biOp->getRHS ();
        // recursively visit each one
        envMembers += generateEnvMembers (lhs, members, closureCount);
        envMembers += generateEnvMembers (rhs, members, closureCount);

      }
      if (expr && isa<clang::ConditionalOperator> (expr)) {
        const ConditionalOperator *condOp = (const ConditionalOperator *) expr;
        Expr *condExp = condOp->getCond ();
        Expr *trueExp = condOp->getTrueExpr ();
        Expr *falseExp = condOp->getFalseExpr ();
        // recursively visit each one
        envMembers += generateEnvMembers (condExp, members, closureCount);
        envMembers += generateEnvMembers (trueExp, members, closureCount);
        envMembers += generateEnvMembers (falseExp, members, closureCount);
      }
      if (expr && isa<clang::ArraySubscriptExpr> (expr)) {
        const ArraySubscriptExpr *aSE = (const ArraySubscriptExpr *)expr;
        const Expr *lhs = aSE->getLHS ();
        const Expr *rhs = aSE->getRHS ();
        envMembers += generateEnvMembers (lhs, members, closureCount);
        envMembers += generateEnvMembers (rhs, members, closureCount);
      }
        // check other kinds of expr,
        // since my test code is simple, the recursion stops when it find DeclRefExpr

      else if (expr && isa<clang::DeclRefExpr> (expr)) {
        const DeclRefExpr *declRef = (const DeclRefExpr *) expr;
        // get a var name
        std::string name = (declRef->getNameInfo ()).getName ().getAsString ();
        if(members.find(name) != members.end())
          return envMembers;
        members.insert(name);
        if (isa<clang::ArrayType> (declRef->getType ())) {
          std::string star = "*";
          const clang::ArrayType *arrayType = (const clang::ArrayType*)declRef->getType ().getTypePtr();
          while (arrayType && isa<clang::ArrayType> (arrayType->getElementType ()))
          {
            arrayType = (const clang::ArrayType*)arrayType->getElementType ().getTypePtr ();
            star += "*";
          }
          std::string type = arrayType->getElementType ().getAsString ();
          envMembers += "\t" +type+" *"+ name+";\n";
        } else {
          std::string type = declRef->getType ().getAsString ();
          envMembers += "\t" + type + " *" + name + ";\n";
        }
      }

      else if (expr && isa<clang::CallExpr> (expr)){
        const CallExpr *callExpr = (const CallExpr *) expr;
        const FunctionDecl *functionDecl = 	callExpr->getDirectCallee ();
        std::string funcName = functionDecl->getNameInfo().getName().getAsString();
        std::string funcType = functionDecl->getReturnType().getAsString();
        bool alreadyPresent = (members.find(funcName) != members.end());
        members.insert(funcName);
        std::string argList = "";
        std::string closureList = "";
        unsigned int argNum = callExpr->getNumArgs ();
        for (unsigned i = 0; i < argNum; i++) {
          argList += "struct closure, ";
          closureList += "c_" + std::to_string(closureCount++) + ", ";
        }
        argList = argList.substr(0, argList.size()-2);
        closureList = closureList.substr(0, closureList.size()-2);
        if(alreadyPresent)
          envMembers += "\tstruct closure " + closureList + ";\n";
        else
          envMembers += "\t"+funcType+" (*"+ funcName +")("+argList+")"+";\n"
                      + "\tstruct closure " + closureList + ";\n";
        
      }
      return envMembers;
    }

    std::string generateEnvInitialization(const Expr *expr, std::set<std::string> &members, int &closureCount){
      std::string EnvInitialization;
      expr = expr->IgnoreImpCasts ();

      // check if this expression contains a binary operation
      if (expr && isa<clang::BinaryOperator> (expr)) {
        const BinaryOperator *biOp = (const BinaryOperator *) expr;

        // get the lhs and rhs of the operator
        const Expr *lhs = biOp->getLHS ();
        const Expr *rhs = biOp->getRHS ();
        // recursively visit each one
        EnvInitialization += generateEnvInitialization (lhs, members, closureCount);
        EnvInitialization += generateEnvInitialization (rhs, members, closureCount);

      }
      if (expr && isa<clang::ConditionalOperator> (expr)) {
        const ConditionalOperator *condOp = (const ConditionalOperator *) expr;
        Expr *condExp = condOp->getCond ();
        Expr *trueExp = condOp->getTrueExpr ();
        Expr *falseExp = condOp->getFalseExpr ();
        // recursively visit each one
        EnvInitialization += generateEnvInitialization (condExp, members, closureCount);
        EnvInitialization += generateEnvInitialization (trueExp, members, closureCount);
        EnvInitialization += generateEnvInitialization (falseExp, members, closureCount);
      }
      if (expr && isa<clang::ArraySubscriptExpr> (expr)) {
        const ArraySubscriptExpr *aSE = (const ArraySubscriptExpr *)expr;
        const Expr *lhs = aSE->getLHS ();
        const Expr *rhs = aSE->getRHS ();
        EnvInitialization += generateEnvInitialization (lhs, members, closureCount);
        EnvInitialization += generateEnvInitialization (rhs, members, closureCount);
      }
        // check other kinds of expr,
        // since my test code is simple, the recursion stops when it find DeclRefExpr

      else if (expr && isa<clang::DeclRefExpr> (expr)) {
        const DeclRefExpr *declRef = (const DeclRefExpr *) expr;
        // get a var name
        std::string name = (declRef->getNameInfo ()).getName ().getAsString ();
        if (members.find(name) != members.end())
          return EnvInitialization;
        members.insert(name);
        // VarDecl *decl = dynamic_cast<const VarDecl*>((declRef)->getDecl());
        const DeclContext *context = declRef->getDecl()->getParentFunctionOrMethod ();
        const FunctionDecl *functionDecl = 0;
        if (context && isa<clang::FunctionDecl> (context)){
          functionDecl = (const  FunctionDecl *)context;
        }
        if (isa<clang::ArrayType> (declRef->getType ())) {
          const clang::ArrayType *arrayType = (const clang::ArrayType*)declRef->getType ().getTypePtr();
          std::string type = arrayType->getElementType ().getAsString ();
          // if(decl->isLocalVarDeclOrParm () && !decl->isLocalVarDecl())
          if(functionDecl && isaParam(functionDecl, declRef))
            EnvInitialization += "ref_ptr("+name+","+ type + "), ";
          else
            EnvInitialization += name+",";
        } else {
          std::string type = declRef->getType ().getAsString ();
          if(functionDecl && isaParam(functionDecl, declRef))
            EnvInitialization += "ref_ptr("+name+","+ type + "), ";
          else
            EnvInitialization += "&" + name + ", ";
        }
      }

      else if (expr && isa<clang::CallExpr> (expr)){
        const CallExpr *callExpr = (const CallExpr *) expr;
        const FunctionDecl *functionDecl = 	callExpr->getDirectCallee ();
        std::string funcName = functionDecl->getNameInfo().getName().getAsString();
        if (members.find(funcName) == members.end())
          EnvInitialization +=  funcName + ",";
        members.insert(funcName);
        EnvInitialization += rewrittenCalls.getClosures(callExpr);
        // unsigned int argNum = callExpr->getNumArgs ();
        // for (unsigned i = 0; i < argNum; i++) {
        //   EnvInitialization += "c_" + std::to_string(closureCount++) + ", ";
        // }
      }
      return EnvInitialization;
    }

    std::string generateThunk(const Expr *expr, int thunk_no){
      int closureCount = 0;
      std::string thunkExpression = generateThunkExpression (expr, closureCount);
      std::stringstream thunkStr;
      std::string exp_type = expr->getType ().getAsString ();
      bool LValue = expr->IgnoreImpCasts ()->isLValue();
      if(LValue)
        thunkStr <<  "void *thunk"<<thunk_no<<"(void* e)\n"
                 <<  "{\n"
                 <<  "\tstruct env"<<thunk_no<<" *env = (struct env"<<thunk_no<<"*)e;\n"
                 <<  "\t\n"
                 <<  "\treturn &("<<thunkExpression<<");\n"
                 <<  "}\n";
      else
        thunkStr <<  "void *thunk"<<thunk_no<<"(void* e)\n"
                 <<  "{\n"
                 <<  "\tstruct env"<<thunk_no<<" *env = (struct env"<<thunk_no<<"*)e;\n"
                 <<  "\tenv->__cbn_result = " << thunkExpression << ";\n"
                 <<  "\t\n"
                 <<  "\treturn &(env->__cbn_result);\n"
                 <<  "}\n";

      std::string thunk = thunkStr.str();
      return thunk;
    }

    std::string generateEnv(const Expr *expr, int thunk_no){
      std::set<std::string> members;
      int closureCount = 0;
      std::string envMembers = generateEnvMembers (expr, members, closureCount);
      std::stringstream envStr;
      envStr << "struct env"<<thunk_no<<"\n"
                 <<"{\n"
                 <<""<<envMembers<<"\n"
                 <<(!expr->IgnoreImpCasts ()->isLValue()?"\t"+expr->getType ().getAsString ()+" __cbn_result;\n":"")
                 <<"};\n";
      std::string env = envStr.str();
      return env;
    }

    std::string generateInstrumentation(const Expr *expr, int thunk_no){
      std::set<std::string> members;
      int closureCount = 0;
      std::string envInitialization = generateEnvInitialization(expr, members, closureCount);
      // envInitialization = envInitialization.substr(0, envInitialization.size()-2);
      std::stringstream envInitStr;
      envInitStr << "struct env"<<thunk_no<<" e"<<thunk_no<<" = {"<<envInitialization<<"};\n";
      envInitStr << "struct closure c_"<<thunk_no<<" = {thunk"<<thunk_no<<", &e"<<thunk_no<<"};\n";
      std::string envInit = envInitStr.str();
      return envInit;
    }
  };

static llvm::cl::OptionCategory MatcherSampleCategory("Matcher Sample");

class IfStmtHandler : public MatchFinder::MatchCallback {
  public:
    IfStmtHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

    virtual void run(const MatchFinder::MatchResult &Result) {
      // The matched 'if' statement was bound to 'ifStmt'.
      if (const IfStmt *IfS = Result.Nodes.getNodeAs<clang::IfStmt>("ifStmt")) {
        const Stmt *Then = IfS->getThen();
        Rewrite.InsertText(Then->getBeginLoc(), "// the 'if' part\n", true, true);

        if (const Stmt *Else = IfS->getElse()) {
          Rewrite.InsertText(Else->getBeginLoc(), "// the 'else' part\n", true,
                             true);
        }
      }
    }

  private:
    Rewriter &Rewrite;
  };

class CallExprHandler : public MatchFinder::MatchCallback {
  public:
    CallExprHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {thunk_count = 0;}

    virtual void run(const MatchFinder::MatchResult &Result) {
      const Stmt *stmt = Result.Nodes.getNodeAs<clang::Stmt>("callExpr");
      SourceLocation beginLocation = stmt->getBeginLoc ();
      
      if(stmt && isa<clang::Expr> (stmt))
      {
        const Expr *expr = Result.Nodes.getNodeAs<clang::Expr>("callExpr");
        rewriteFunctionCall (expr, beginLocation);
      }
      else if(stmt && isa<clang::ReturnStmt> (stmt))
      {
        const ReturnStmt *returnStmt = (const ReturnStmt *)stmt;
        const Expr *expr = returnStmt->getRetValue ();
        rewriteFunctionCall (expr, beginLocation);
      }
      else if(stmt && isa<clang::ForStmt> (stmt))
      {
        const ForStmt *forStmt = (const ForStmt *)stmt;
        const Stmt *initStmt = forStmt->getInit ();
        const Expr *cond = forStmt->getCond ();
        const Expr *inc = forStmt->getInc ();  
        // rewriteFunctionCall (init, beginLocation);
        rewriteFunctionCall (cond, beginLocation);
        rewriteFunctionCall (inc, beginLocation);
        if(initStmt && isa<clang::Expr> (initStmt))
        {
          const Expr *initExpr = (const Expr *)initStmt;
          rewriteFunctionCall (initExpr, beginLocation);
        }
        else if(initStmt && isa<clang::DeclStmt> (initStmt))
        {
          const DeclStmt *declStmt = (const DeclStmt *)initStmt;
          const DeclGroupRef declGroup = declStmt->getDeclGroup ();
          for (DeclGroupRef::const_iterator i = declGroup.begin(), e = declGroup.end(); i != e; ++i) {
              Decl *decl = *i;
              if(decl && isa<clang::VarDecl> (decl))
              {
                VarDecl *varDecl = (VarDecl *)decl;
                const Expr *expr = varDecl->getInit ();
                rewriteFunctionCall (expr, beginLocation);
              }

          }
        }
      }

      else if(stmt && isa<clang::IfStmt> (stmt))
      {
        const IfStmt *ifStmt = (const IfStmt *)stmt;
        const Expr *expr = ifStmt->getCond ();
        rewriteFunctionCall (expr, beginLocation);
      }
      else if(stmt && isa<clang::SwitchStmt> (stmt))
      {
        const SwitchStmt *switchStmt = (const SwitchStmt *)stmt;
        const Expr *expr = switchStmt->getCond ();
        rewriteFunctionCall (expr, beginLocation);
      }
      else if(stmt && isa<clang::WhileStmt> (stmt))
      {
        const WhileStmt *whileStmt = (const WhileStmt *)stmt;
        const Expr *expr = whileStmt->getCond ();
        rewriteFunctionCall (expr, beginLocation);
      }
      else if(stmt && isa<clang::DeclStmt> (stmt))
      {
        const DeclStmt *declStmt = (const DeclStmt *)stmt;
        const DeclGroupRef declGroup = declStmt->getDeclGroup ();
        for (DeclGroupRef::const_iterator i = declGroup.begin(), e = declGroup.end(); i != e; ++i) {
            Decl *decl = *i;
            if(decl && isa<clang::VarDecl> (decl))
            {
              VarDecl *varDecl = (VarDecl *)decl;
              if(!varDecl)
                continue;
              const Expr *expr = varDecl->getInit ();
              rewriteFunctionCall (expr, beginLocation);
            }
        }

      }
    }

    void rewriteFunctionCall(const Expr *expr, SourceLocation beginLocation){
      if (expr && isa<clang::BinaryOperator> (expr)) {
        const BinaryOperator *biOp = (const BinaryOperator *) expr;

        // get the lhs and rhs of the operator
        const Expr *lhs = biOp->getLHS ();
        const Expr *rhs = biOp->getRHS ();
        // recursively visit each one
        rewriteFunctionCall(lhs, beginLocation);
        rewriteFunctionCall(rhs, beginLocation);

      }
      if (expr && isa<clang::ConditionalOperator> (expr)) {
        const ConditionalOperator *condOp = (const ConditionalOperator *) expr;
        Expr *condExp = condOp->getCond ();
        Expr *trueExp = condOp->getTrueExpr ();
        Expr *falseExp = condOp->getFalseExpr ();
        // recursively visit each one
        rewriteFunctionCall (condExp, beginLocation);
        rewriteFunctionCall (trueExp, beginLocation);
        rewriteFunctionCall (falseExp, beginLocation);
      }
      if (expr && isa<clang::CallExpr> (expr)) {
        const CallExpr *callExpr = (const CallExpr*) expr;
        bool found1 = (std::find(rewrittenFunctions.begin(), rewrittenFunctions.end(), callExpr->getDirectCallee()) != rewrittenFunctions.end());
        if (!found1)
          return;
        std::string closure_list = "";
        bool found2 = rewrittenCalls.isPresent (callExpr);
        if (found2)
          return;
        clang::LangOptions LangOpts;
        LangOpts.CPlusPlus = true;
        clang::PrintingPolicy Policy (LangOpts);
        unsigned int argNum = callExpr->getNumArgs ();
        // Checking if there is any non rewritten calls in argument list
        for (unsigned i = 0; i < argNum; i++) {
          if (isa<clang::CallExpr> (callExpr->getArg (i))) {
            const CallExpr *arg = (const CallExpr *) callExpr->getArg (i);
            bool found = rewrittenCalls.isPresent (arg);
            if (!found) {
              rewriteFunctionCall (arg, beginLocation);
            }
          }
          if (isa<clang::BinaryOperator> (callExpr->getArg (i))) {
            const BinaryOperator *biOp = (const BinaryOperator *) callExpr->getArg (i);

            // get the lhs and rhs of the operator
            const Expr *lhs = biOp->getLHS ();
            const Expr *rhs = biOp->getRHS ();
            // recursively visit each one
            rewriteFunctionCall (lhs, beginLocation);
            rewriteFunctionCall (rhs, beginLocation);
          }
          if (isa<clang::ConditionalOperator> (callExpr->getArg (i))) {
            const ConditionalOperator *condOp = (const ConditionalOperator *) callExpr->getArg (i);
            Expr *condExp = condOp->getCond ();
            Expr *trueExp = condOp->getTrueExpr ();
            Expr *falseExp = condOp->getFalseExpr ();
            // recursively visit each one
            rewriteFunctionCall (condExp, beginLocation);
            rewriteFunctionCall (trueExp, beginLocation);
            rewriteFunctionCall (falseExp, beginLocation);
          }
        }
        if (/*callExpr &&*/ !callExpr->getCalleeDecl ()->isExported ()) {
          for (unsigned i = 0; i < argNum; i++) {
            std::string TypeS;
            llvm::raw_string_ostream s (TypeS);
            callExpr->getArg (i)->printPretty (s, 0, Policy);
            Rewrite.ReplaceText (callExpr->getArg (i)->getSourceRange (), "c_" + std::to_string (thunk_count));
            //            VisitExpr(callExpr->getArg (i));
            std::string env = codeGen.generateEnv (callExpr->getArg (i), thunk_count);
            Rewrite.InsertText (initialLocation, env, true, true);
            std::string thunk = codeGen.generateThunk (callExpr->getArg (i), thunk_count);
            Rewrite.InsertText (initialLocation, thunk, true, true);
            std::string instrumentation = codeGen.generateInstrumentation (callExpr->getArg (i), thunk_count);
            Rewrite.InsertText (beginLocation, instrumentation, true, true);
            closure_list += "c_" + std::to_string (thunk_count) + ", ";
            thunk_count++;
          }

        }
        closure_list = closure_list.substr (0, closure_list.size () - 2);
        rewrittenCalls.addFunction (callExpr, closure_list);
      }
      if (expr && isa<clang::BinaryOperator> (expr)) {
        const BinaryOperator *biOp = (const BinaryOperator *) expr;

        // get the lhs and rhs of the operator
        const Expr *lhs = biOp->getLHS ();
        const Expr *rhs = biOp->getRHS ();
        // recursively visit each one
        rewriteFunctionCall (lhs, beginLocation);
        rewriteFunctionCall (rhs, beginLocation);
        }

    }

  private:
    Rewriter &Rewrite;
    CodeGenerator codeGen;
    int thunk_count;
  };

class FunctionDeclStmtHandler : public MatchFinder::MatchCallback {
  public:
    FunctionDeclStmtHandler(Rewriter &Rewrite) : Rewrite(Rewrite) { count = 0;}

    bool recursiveVisit (Stmt *stmt) {
      if(!stmt)
          return true;
      for (Stmt::child_iterator i = stmt->child_begin(), e = stmt->child_end(); i != e; ++i) {
        Stmt *currStmt = *i;
        if(!currStmt)
          continue;
        if (isa<clang::DeclRefExpr> (currStmt)) {
          DeclRefExpr *declRef = (DeclRefExpr *) currStmt;
          bool found = false;
          for(unsigned int paramIndex=0;paramIndex< params.size();paramIndex++){
            if(declRef->getDecl () == (ValueDecl *)params[paramIndex])
            {
              found = true;
            }
          }
          if(found) {
            Rewrite.ReplaceText(SourceRange(declRef->getBeginLoc(), declRef->getEndLoc()),
                "ref("+declRef->getNameInfo().getName().getAsString ()+", "+declRef->getType().getAsString()+")");
          }

        }
        
        if(isa<clang::CallExpr> (currStmt))
        {
          const CallExpr *callExpr = (const CallExpr *)currStmt;
          bool found = (std::find(rewrittenFunctions.begin(), rewrittenFunctions.end(), callExpr->getDirectCallee()) != rewrittenFunctions.end());
          if(!found)
            recursiveVisit(currStmt);
        }
        else{
          recursiveVisit(currStmt);
        }
          
      }
      return true;
    }
    virtual void run(const MatchFinder::MatchResult &Result) {
      // The matched 'if' statement was bound to 'ifStmt'.
      const FunctionDecl *S = Result.Nodes.getNodeAs<clang::FunctionDecl>("FunctionDecl");
//      S->dump();
      if (count++ == 0) {
        if (S) {
          Rewrite.InsertText(S->getBeginLoc(), "#define ref(x, type) (*(type*)x.call(x.env))\n"
                                               "#define ref_ptr(x, type) ((type*)x.call(x.env))\n\n"
                                               "struct closure\n"
                                               "{\n"
                                               "\tvoid* (* call)(void *);\n"
                                               "\tvoid* env;\n"
                                               "};\n", true,
                             true);
          initialLocation = S->getBeginLoc();
        }
      }
      if(S){

        params = S->parameters();
        for(unsigned int i=0;i< params.size();i++)
        {
          Rewrite.ReplaceText(params[i]->getSourceRange(), "struct closure "+params[i]->getNameAsString());
        }
        if(!S->isMain()) {
          Stmt *body = S->getBody ();
          rewrittenFunctions.push_back(S);
          recursiveVisit (body);
        }
      }
    }

  private:
    Rewriter &Rewrite;
    int count;
    ArrayRef<ParmVarDecl *> params;
  };


class IncrementForLoopHandler : public MatchFinder::MatchCallback {
public:
  IncrementForLoopHandler(Rewriter &Rewrite) : Rewrite(Rewrite) {}

  virtual void run(const MatchFinder::MatchResult &Result) {
    const VarDecl *IncVar = Result.Nodes.getNodeAs<VarDecl>("incVarName");
    Rewrite.InsertText(IncVar->getBeginLoc(), "/* increment */", true, true);
  }

private:
  Rewriter &Rewrite;
};

// Implementation of the ASTConsumer interface for reading an AST produced
// by the Clang parser. It registers a couple of matchers and runs them on
// the AST.
class MyASTConsumer : public ASTConsumer {
public:
  MyASTConsumer(Rewriter &R) : HandlerForFuncionDecl(R), HandlerForCallExpr(R) {
    Matcher.addMatcher(functionDecl(
        isDefinition(),
        unless(isExpansionInSystemHeader())
    ).bind("FunctionDecl"), &HandlerForFuncionDecl);

    // Matcher for instrumenting call expression
    // Matcher.addMatcher(callExpr(callee(functionDecl(unless(isExpansionInSystemHeader())))).bind("callExpr"), &HandlerForCallExpr);
    // Matcher.addMatcher(callExpr().bind("callExpr"), &HandlerForCallExpr);
    Matcher.addMatcher(stmt().bind("callExpr"), &HandlerForCallExpr);

  }

  void HandleTranslationUnit(ASTContext &Context) override {
    astContext = &Context;
    Matcher.matchAST(Context);
  }

private:
  FunctionDeclStmtHandler HandlerForFuncionDecl;
  CallExprHandler HandlerForCallExpr;
  MatchFinder Matcher;
};

StatementMatcher LoopMatcher =
  forStmt(hasLoopInit(declStmt(hasSingleDecl(varDecl(
    hasInitializer(integerLiteral(equals(0)))))))).bind("forLoop");

class LoopPrinter : public MatchFinder::MatchCallback {
public :
  virtual void run(const MatchFinder::MatchResult &Result) {
    if (const ForStmt *FS = Result.Nodes.getNodeAs<clang::ForStmt>("forLoop"))
      FS->dump();
  }
};

class MyFrontendAction : public ASTFrontendAction {
public:
  MyFrontendAction() {}
  void EndSourceFileAction() override {
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID())
        .write(llvm::outs());
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter);
  }

private:
  Rewriter TheRewriter;
};

int main(int argc, const char **argv) {
  CommonOptionsParser op(argc, argv, MatcherSampleCategory);
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());

  return Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
}

