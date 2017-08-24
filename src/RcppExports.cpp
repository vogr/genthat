// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// sequence_tracer_create
SEXP sequence_tracer_create();
RcppExport SEXP _genthat_sequence_tracer_create() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(sequence_tracer_create());
    return rcpp_result_gen;
END_RCPP
}
// sequence_tracer_reset_traces
SEXP sequence_tracer_reset_traces(SEXP tracer_xp);
RcppExport SEXP _genthat_sequence_tracer_reset_traces(SEXP tracer_xpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type tracer_xp(tracer_xpSEXP);
    rcpp_result_gen = Rcpp::wrap(sequence_tracer_reset_traces(tracer_xp));
    return rcpp_result_gen;
END_RCPP
}
// sequence_tracer_store_trace
SEXP sequence_tracer_store_trace(SEXP tracer_xp, SEXP trace);
RcppExport SEXP _genthat_sequence_tracer_store_trace(SEXP tracer_xpSEXP, SEXP traceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type tracer_xp(tracer_xpSEXP);
    Rcpp::traits::input_parameter< SEXP >::type trace(traceSEXP);
    rcpp_result_gen = Rcpp::wrap(sequence_tracer_store_trace(tracer_xp, trace));
    return rcpp_result_gen;
END_RCPP
}
// sequence_tracer_copy_traces
SEXP sequence_tracer_copy_traces(SEXP tracer_xp);
RcppExport SEXP _genthat_sequence_tracer_copy_traces(SEXP tracer_xpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type tracer_xp(tracer_xpSEXP);
    rcpp_result_gen = Rcpp::wrap(sequence_tracer_copy_traces(tracer_xp));
    return rcpp_result_gen;
END_RCPP
}
// serialize_value
std::string serialize_value(SEXP s);
RcppExport SEXP _genthat_serialize_value(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(serialize_value(s));
    return rcpp_result_gen;
END_RCPP
}
// is_infix_fun_no_space
bool is_infix_fun_no_space(std::string const& fun);
RcppExport SEXP _genthat_is_infix_fun_no_space(SEXP funSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string const& >::type fun(funSEXP);
    rcpp_result_gen = Rcpp::wrap(is_infix_fun_no_space(fun));
    return rcpp_result_gen;
END_RCPP
}
// is_infix_fun
bool is_infix_fun(std::string const& fun);
RcppExport SEXP _genthat_is_infix_fun(SEXP funSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string const& >::type fun(funSEXP);
    rcpp_result_gen = Rcpp::wrap(is_infix_fun(fun));
    return rcpp_result_gen;
END_RCPP
}
// escape_name
std::string escape_name(std::string const& name);
RcppExport SEXP _genthat_escape_name(SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string const& >::type name(nameSEXP);
    rcpp_result_gen = Rcpp::wrap(escape_name(name));
    return rcpp_result_gen;
END_RCPP
}
// get_dd_val
SEXP get_dd_val(int i, SEXP rho, SEXP default_value, bool force);
RcppExport SEXP _genthat_get_dd_val(SEXP iSEXP, SEXP rhoSEXP, SEXP default_valueSEXP, SEXP forceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< SEXP >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< SEXP >::type default_value(default_valueSEXP);
    Rcpp::traits::input_parameter< bool >::type force(forceSEXP);
    rcpp_result_gen = Rcpp::wrap(get_dd_val(i, rho, default_value, force));
    return rcpp_result_gen;
END_RCPP
}
// reassign_function
SEXP reassign_function(SEXP target_fun, SEXP new_fun, bool keep_only_new_attributes);
RcppExport SEXP _genthat_reassign_function(SEXP target_funSEXP, SEXP new_funSEXP, SEXP keep_only_new_attributesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type target_fun(target_funSEXP);
    Rcpp::traits::input_parameter< SEXP >::type new_fun(new_funSEXP);
    Rcpp::traits::input_parameter< bool >::type keep_only_new_attributes(keep_only_new_attributesSEXP);
    rcpp_result_gen = Rcpp::wrap(reassign_function(target_fun, new_fun, keep_only_new_attributes));
    return rcpp_result_gen;
END_RCPP
}
// create_duplicate
SEXP create_duplicate(SEXP target);
RcppExport SEXP _genthat_create_duplicate(SEXP targetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type target(targetSEXP);
    rcpp_result_gen = Rcpp::wrap(create_duplicate(target));
    return rcpp_result_gen;
END_RCPP
}
// environment_name
std::string environment_name(SEXP env);
RcppExport SEXP _genthat_environment_name(SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(environment_name(env));
    return rcpp_result_gen;
END_RCPP
}
// environment_name_as_code
std::string environment_name_as_code(SEXP env);
RcppExport SEXP _genthat_environment_name_as_code(SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(environment_name_as_code(env));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_genthat_sequence_tracer_create", (DL_FUNC) &_genthat_sequence_tracer_create, 0},
    {"_genthat_sequence_tracer_reset_traces", (DL_FUNC) &_genthat_sequence_tracer_reset_traces, 1},
    {"_genthat_sequence_tracer_store_trace", (DL_FUNC) &_genthat_sequence_tracer_store_trace, 2},
    {"_genthat_sequence_tracer_copy_traces", (DL_FUNC) &_genthat_sequence_tracer_copy_traces, 1},
    {"_genthat_serialize_value", (DL_FUNC) &_genthat_serialize_value, 1},
    {"_genthat_is_infix_fun_no_space", (DL_FUNC) &_genthat_is_infix_fun_no_space, 1},
    {"_genthat_is_infix_fun", (DL_FUNC) &_genthat_is_infix_fun, 1},
    {"_genthat_escape_name", (DL_FUNC) &_genthat_escape_name, 1},
    {"_genthat_get_dd_val", (DL_FUNC) &_genthat_get_dd_val, 4},
    {"_genthat_reassign_function", (DL_FUNC) &_genthat_reassign_function, 3},
    {"_genthat_create_duplicate", (DL_FUNC) &_genthat_create_duplicate, 1},
    {"_genthat_environment_name", (DL_FUNC) &_genthat_environment_name, 1},
    {"_genthat_environment_name_as_code", (DL_FUNC) &_genthat_environment_name_as_code, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_genthat(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
