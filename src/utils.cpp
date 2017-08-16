#include <string>

#include "utils.h"

extern "C" {
    // from inp into ans (from attrib.c)
    void copyMostAttrib(SEXP inp, SEXP ans);
}

// [[Rcpp::export]]
SEXP reassign_function(SEXP target_fun, SEXP new_fun, bool keep_only_new_attributes=false) {
  if (TYPEOF(target_fun) != CLOSXP) error("target_fun must be a function");
  if (TYPEOF(new_fun) != CLOSXP) error("new_fun must be a function");

  //  TODO: check if the formals are the same
  SET_BODY(target_fun, BODY(new_fun));
  if (keep_only_new_attributes) {
      DUPLICATE_ATTRIB(target_fun, new_fun);
  } else {
      copyMostAttrib(new_fun, target_fun);
  }

  return R_NilValue;
}

// [[Rcpp::export]]
SEXP create_duplicate(SEXP target) {
  if (isNull(target)) error("target must not be null");

  return duplicate(target);
}

// [[Rcpp::export]]
std::string environment_name(SEXP env) {
    if (R_IsPackageEnv(env) == TRUE) {
        // cf. builtin.c:432 do_envirName
        return CHAR(STRING_ELT(R_PackageEnvName(env), 0));
    } else if (R_IsNamespaceEnv(env) == TRUE) {
        // cf. builtin.c:434 do_envirName
        return CHAR(STRING_ELT(R_NamespaceEnvSpec(env), 0));
    } else {
        return "";
    }
}

// [[Rcpp::export]]
std::string environment_name_as_code(SEXP env) {
    if (env == R_EmptyEnv) {
        return "emptyenv()";
    } else if (env == R_GlobalEnv) {
        return ".GlobalEnv";
    } else if (env == R_BaseEnv || env == R_BaseNamespace) {
        return ".BaseNamespaceEnv";
    } else {
        std::string name = environment_name(env);
        if (!name.empty()) {
            return "as.environment(\"" + name + "\")";
        } else {
            return "";
        }
    }
}
