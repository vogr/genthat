#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

// [[Rcpp::export]]
SEXP reassign_function(SEXP target_fun, SEXP new_fun)
{
  if (TYPEOF(target_fun) != CLOSXP) error("target_fun must be a function");
  if (TYPEOF(new_fun) != CLOSXP) error("new_fun must be a function");

  //  TODO: check if the formals are the same
  SET_BODY(target_fun, BODY(new_fun));
  DUPLICATE_ATTRIB(target_fun, new_fun);

  return R_NilValue;
}

// [[Rcpp::export]]
SEXP create_duplicate(SEXP target) {
  if (isNull(target)) error("target must not be null");
                      
  return duplicate(target);
}
