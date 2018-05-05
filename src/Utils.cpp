#include <string>

#include "Utils.h"

extern "C" {
    // from inp into ans (from attrib.c)
    void copyMostAttrib(SEXP inp, SEXP ans);
}

// [[Rcpp::export]]
SEXP get_dd_val(int i, SEXP rho, SEXP default_value, bool force=false) {
    // TODO: check args
    SEXP dots = findVar(R_DotsSymbol, rho);

    if (TYPEOF(dots) == DOTSXP && dots != R_UnboundValue) {
        if (length(dots) >= i) {
            dots = nthcdr(dots, i - 1);
            SEXP val = CAR(dots);

            if (TYPEOF(val) == PROMSXP) {
                if (force) {
                    return Rf_eval(val, rho);
                } else if (PRVALUE(val) == R_UnboundValue) {
                    return default_value;
                } else {
                    return PRVALUE(val);
                }
            } else {
                return val;
            }
        } else {
            Rf_error("Unable to find ..%d - the ... does not contain %d elements", i, i);
        }
    } else {
        Rf_error("Unable to find ..%d - used in an incorrect context, no ... to look in", i);
    }

    return default_value;
}

// [[Rcpp::export]]
SEXP reassign_function(SEXP target_fun, SEXP new_fun) {
  if (TYPEOF(target_fun) != CLOSXP) error("target_fun must be a function");
  if (TYPEOF(new_fun) != CLOSXP) error("new_fun must be a function");

  SET_BODY(target_fun, BODY(new_fun));

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
            return "getNamespace(\"" + name + "\")";
        } else {
            return "";
        }
    }
}
