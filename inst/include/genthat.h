#include <Rcpp.h>

#include <string>
#include <regex>
#include <chrono>

using namespace std;

#ifdef HAHA


typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

/* Information for Deparsing Expressions */
typedef enum {
    PP_INVALID  =  0,
    PP_ASSIGN   =  1,
    PP_ASSIGN2  =  2,
    PP_BINARY   =  3,
    PP_BINARY2  =  4,
    PP_BREAK    =  5,
    PP_CURLY    =  6,
    PP_FOR      =  7,
    PP_FUNCALL  =  8,
    PP_FUNCTION =  9,
    PP_IF       = 10,
    PP_NEXT     = 11,
    PP_PAREN    = 12,
    PP_RETURN   = 13,
    PP_SUBASS   = 14,
    PP_SUBSET   = 15,
    PP_WHILE    = 16,
    PP_UNARY    = 17,
    PP_DOLLAR 	= 18,
    PP_FOREIGN 	= 19,
    PP_REPEAT 	= 20
} PPkind;

typedef enum {
    PREC_FN	 = 0,
    PREC_LEFT    = 1,
    PREC_EQ	 = 2,
    PREC_RIGHT	 = 3,
    PREC_TILDE	 = 4,
    PREC_OR	 = 5,
    PREC_AND	 = 6,
    PREC_NOT	 = 7,
    PREC_COMPARE = 8,
    PREC_SUM	 = 9,
    PREC_PROD	 = 10,
    PREC_PERCENT = 11,
    PREC_COLON	 = 12,
    PREC_SIGN	 = 13,
    PREC_POWER	 = 14,
    PREC_DOLLAR  = 15,
    PREC_NS	 = 16,
    PREC_SUBSET	 = 17
} PPprec;

typedef struct {
	PPkind kind; 	 /* deparse kind */
	PPprec precedence; /* operator precedence */
	unsigned int rightassoc;  /* right associative? */
} PPinfo;

typedef struct {
    char   *name;    /* print name */
    CCODE  cfun;     /* c-code address */
    int	   code;     /* offset within c-code */
    int	   eval;     /* evaluate args? */
    int	   arity;    /* function arity */
    PPinfo gram;     /* pretty-print info */
} FUNTAB;

extern FUNTAB	R_FunTab[];	    /* Built in functions */

CCODE get_internal(std::string);

#endif


SEXP search();
bool contains(Rcpp::CharacterVector, std::string);
std::string getFunctionEnvironmentName(std::string &functionName);
SEXP deparse(SEXP);
SEXP pop_args();
bool missing(SEXP, SEXP);
SEXP GetArgs(SEXP);

class serialization_error : public std::exception {
public:
    const string msg;
    serialization_error(string msg) : msg(msg) {}
    string to_string() { return "<SERIALIZATION_ERROR: " + msg + ">"; }
};

class sexp_not_implemented : public serialization_error {
public:
    const string sexp_type;
    sexp_not_implemented(string sexp_type) : sexp_type(sexp_type), serialization_error("SEXP type " + sexp_type + " not implemented!") {}
    string to_string() { return "serialization for SEXP type " + sexp_type + " not implemented!"; }
};

class cycle_in_structure : public serialization_error {
public:
    cycle_in_structure() : serialization_error("Serialized data structure contains cycle!") {}
    string to_string() { return "Serialized data structure contains cycle!"; }
};

string StringFromReal(double x);
string serialize_cpp(SEXP s);
