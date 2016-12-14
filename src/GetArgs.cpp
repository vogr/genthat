#include "genthat.h"
using namespace Rcpp;
using namespace std;

SEXP forceEval(SEXP expr, SEXP env)
{
  if (expr != R_UnboundValue && TYPEOF(expr) == PROMSXP) {
    SEXP prcode = PRCODE(expr);
    SEXP evalEnv = !Rf_isNull(PRENV(expr)) ? PRENV(expr) : env;
    int err = 0;
    SEXP res = R_tryEvalSilent(expr, evalEnv, &err);
    SEXP evaluatedArg = err ? prcode : res;
    return evaluatedArg;
  } else {
    cerr << "Unexpected branch taken!" << endl;
    return R_NilValue;
  }
}

SEXP GetArgs(SEXP dotsE)
{
  Environment dotsEnv(dotsE);
  List args(0);

  // named arguments
  CharacterVector envNames = dotsEnv.ls(false);
  for (int i = 0; i < envNames.length(); i++)
  {
    string name = as<string>(envNames[i]);
    if (!missing(Rf_install(name.c_str()), dotsE)) {
        SEXP unevaluatedArg = dotsEnv.get(name);
        args[name] = unevaluatedArg; 
    }
  }

  // dot arguments
  if (dotsEnv.exists("...") && dotsEnv.get("...") != R_MissingArg) {
    for (SEXP dots = dotsEnv.get("..."); dots != R_NilValue; dots = CDR(dots))
    {
      SEXP arg_val = forceEval(CAR(dots), dotsE);
      if (R_NilValue == TAG(dots)) {
          args.push_back(arg_val); 
      } else {
          string name = as<string>(PRINTNAME(TAG(dots)));
          args[name] = arg_val; 
      }
    }
  }

  return args;
}

