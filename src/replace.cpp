#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

// [[Rcpp::export]]
SEXP reassign_function(SEXP name, SEXP env, SEXP fn, SEXP new_fn)
{
  if (TYPEOF(name) != SYMSXP) error("name must be a symbol");
  if (TYPEOF(env) != ENVSXP) error("env must be an environment");
  if (TYPEOF(fn) != CLOSXP) error("fn must be a function");
  if (TYPEOF(new_fn) != CLOSXP) error("new_fn must be a function");

  //  SET_FORMALS(fn, FORMALS(new_fn));
  SET_BODY(fn, BODY(new_fn));
  //  SET_CLOENV(fn, CLOENV(new_fn));
  DUPLICATE_ATTRIB(fn, new_fn);

  return R_NilValue;
}


// [[Rcpp::export]]
SEXP create_duplicate(SEXP x) {
  return duplicate(x);
}
