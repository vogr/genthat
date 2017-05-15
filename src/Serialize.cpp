#include <Rcpp.h>
#include <R.h>

#include <string>
#include <regex>

using namespace Rcpp;
using namespace std;

class serialization_error : public runtime_error {
 public:
 serialization_error(string details) : runtime_error("Serialization error: " + details) {}
};

class sexp_not_supported_error : public serialization_error {
 public:
 sexp_not_supported_error(string sexp_type) : serialization_error("SEXP type " + sexp_type + " not supported!") {}
};

class cycle_error : public serialization_error {
 public:
 cycle_error() : serialization_error("Serialized data structure contains cycle!") {}
};

static const set<string> BASE_INFIX_FUNS = {
    "<-", "=", "<<-", "+", "-", "*", "/", "^", "==", "!=", "<", "<=", ">=", ">", "&", "|", "!", "&&", "||", "~"
};

static const set<string> BASE_INFIX_FUNS_NO_SPACE = {
   ":", "::", ":::", "$", "@"
};

// "keywords" that need to be escaped
static const set<string> KEYWORDS = {
   "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
   "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
   "NA_complex_", "NA_character_"
};

static const regex VAR_NAME = regex("^([a-zA-Z][a-zA-Z0-9._]*|[.]([a-zA-Z._][a-zA-Z0-9._]*)?)$");

class Serializer {
private:
    static bool is_infix_fun_no_space(string const &fun) {
        return BASE_INFIX_FUNS_NO_SPACE.find(fun) != BASE_INFIX_FUNS_NO_SPACE.end();
    }

    static bool is_infix_fun(string const &fun) {
        if (BASE_INFIX_FUNS.find(fun) != BASE_INFIX_FUNS.end()) {
            return true;
        } else if (BASE_INFIX_FUNS_NO_SPACE.find(fun) != BASE_INFIX_FUNS_NO_SPACE.end()) {
            return true;
        } else if (fun[0] == '%' && fun[fun.size() - 1] == '%') {
            return true;
        } else {
            return false;
        }
    }

/* check for attributes other than function source */
// TODO: bool
    static Rboolean hasAttributes(SEXP s)
        {
            for (SEXP a = ATTRIB(s); !Rf_isNull(a); a = CDR(a))
            {
                if (TAG(a) != Rf_install("srcref")) return TRUE;
            }
            return FALSE;
        }

    // TODO: fix
    static string escape_name(string const &name) {
        if (name.empty()) {
            return name;
        } else if (KEYWORDS.find(name) != KEYWORDS.end() || !regex_match(name, VAR_NAME)) {
            return "`" + name + "`";
        } else {
            return name;
        }
    }

    // TODO: fix
    static string symbol_to_attrib_key(SEXP s) {
        if(s == R_DimSymbol) {
            return ".Dim";
        } else if (s == R_DimNamesSymbol) {
            return ".Dimnames";
        } else if (s == R_NamesSymbol) {
            return ".Names";
        } else if (s == R_TspSymbol) {
            return ".Tsp";
        } else if (s == R_LevelsSymbol) {
            return ".Label";
        } else {
            string tag = CHAR(PRINTNAME(s));
            return escape_name(tag);
        }
    }

    // TODO: fix
    string wrap_in_attributes(SEXP s, string s_str, bool wrap_just_names) {
        RObject protected_s(s);
        if (hasAttributes(s)) {
            string elems = "";
            bool non_name_attr = false;
            for (SEXP a = ATTRIB(s); !Rf_isNull(a); a = CDR(a))
            {
                if (TAG(a) != Rf_install("srcref")) {
                    string attr_name = symbol_to_attrib_key(TAG(a));
                    elems += ", " + attr_name + "=" + serialize(CAR(a), true);
                    if (attr_name != ".Names") {
                        non_name_attr = true;
                    }
                }
            }
            return (non_name_attr || wrap_just_names) ?
                "structure(" + s_str + elems + ")" :
                s_str;
        } else {
            return s_str;
        }
    }

    // TODO: fix
    inline char hex_digit_to_char(int x) {
        return x < 10 ? x + '0' : x + 'a' - 9;
    }

    inline string to_string_literal(const char *x) {
        string ret = "";
        for (int i = 0; i < strlen(x); i++)
        {
            unsigned char c = x[i];
            switch (c) {
                /* ANSI Escapes */
            case '\a': ret += "\\a"; break;
            case '\b': ret += "\\b"; break;
            case '\f': ret += "\\f"; break;
            case '\n': ret += "\\n"; break;
            case '\r': ret += "\\r"; break;
            case '\t': ret += "\\t"; break;
            case '\v': ret += "\\v"; break;
            case '\0': ret += "\\0"; break;
            default: {
                if(c < 0x80 && c > 0x1F) {
                    // printable chars
                    switch(c) {
                    case '\\': ret += "\\\\"; break;
                    case '"': ret += "\\\""; break;
                    default: ret += string((const char *) &c, 1);
                    }
                } else {
                    // non-printable chars
                    char digits[] = { '4', '2', '\0' };
                    digits[0] = hex_digit_to_char((c >> 4) & 0x0f);
                    digits[1] = hex_digit_to_char(c & 0x0f);
                    ret += string("\\x") + digits;
                }
            }
            }
        }
        return "\"" + ret + "\"";
    }

    static string char_to_hex(unsigned char c) {
        if (c == 0) {
            return "0";
        } else if (c == 1) {
            return "1";
        } else {
            char buff[33];
            sprintf(buff, "%x", c);
            return "0x" + string(buff);
        }
    }

    string format_argument(SEXP const arg) {
        SEXP arg_name = TAG(arg);
        SEXP arg_value = CAR(arg);

        string name;

        switch (TYPEOF(arg_name)) {
        case NILSXP:
            name = "";
            break;
        case SYMSXP:
            name = escape_name(serialize(arg_name, false));
            break;
        default:
            throw serialization_error("Unexpected SEXPTYPE in function arguments: " + to_string(TYPEOF(arg_name)));
        }

        string value = serialize(arg_value, false);
        string res;

        if (!name.empty() && !value.empty()) {
            res = name + "=" + value;
        } else if (name.empty()) {
            res = value;
        } else if (value.empty()) {
            res = name;
        } else {
            res = "";
        }

        return res;
    }

    string format_arguments(SEXP const args, string const &sep=", ") {
        string res = "";

        for (SEXP arg = args; !Rf_isNull(arg); arg = CDR(arg)) {
            res += format_argument(arg);
            res += Rf_isNull(CDR(arg)) ? "" : sep;
        }

        return res;
    }

    string get_element_name(SEXP const names, int i) {
        string name;

        if (!Rf_isNull(names)) {
            name = CHAR(STRING_ELT(names, i));
        }

        return name;
    }

    // contains the list of visited environments so far
    // it is used for the ENVSXP serialization
    set<SEXP> visited_environments;
public:
    string serialize(SEXP s, bool quote) {
        switch (TYPEOF(s)) {
        case NILSXP:
            return "NULL";
        case VECSXP: { /* lists */
            RObject protected_s(s);

            SEXP names = Rf_getAttrib(s, R_NamesSymbol);
            int size = XLENGTH(s);
            string args;

            for (int i = 0 ; i < size ; i++) {
                string value = serialize(VECTOR_ELT(s, i), true);
                string name = escape_name(get_element_name(names, i));

                args += name.empty() ? name : name + "=";
                args += value;
                args += i + 1 < size ? ", " : "";
            }

            return wrap_in_attributes(s, "list(" + args + ")", false);
        }
        case LGLSXP: {
            RObject protected_s(s);
            int n = XLENGTH(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                int val = LOGICAL(s)[i];
                string str_val = (val == NA_LOGICAL) ? "NA" : (val == 0) ? "FALSE" : "TRUE";
                elems += (i == 0 ? "" : ",") + str_val;
            }
            return wrap_in_attributes(s, n == 0 ? "logical(0)" : n == 1 ? elems : "c(" + elems + ")", true); }
        case INTSXP: {
            RObject protected_s(s);
            int n = XLENGTH(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                int val = INTEGER(s)[i];
                string str_val = val == NA_INTEGER ? "NA_integer_" : (to_string(val) + "L");
                elems += (i == 0 ? "" : ",") + str_val;
            }
            return wrap_in_attributes(s, n == 0 ? "integer(0)" : n == 1 ? elems : "c(" + elems + ")", true); }
        case REALSXP: {
            RObject protected_s(s);
            int n = XLENGTH(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                double val = REAL(s)[i];
                for (int j = 0; j < sizeof(double); j++)
                {
                    string str_val = char_to_hex(((unsigned char *) &val)[j]);
                    elems += ((i == 0 && j == 0) ? "" : ",") + str_val;
                }
            }
            string decoded = "readBin(as.raw(c(" + elems + ")), n=" + to_string(n) + ", \"double\")";
            return wrap_in_attributes(s, n == 0 ? "double(0)" : decoded, true); }
        case CPLXSXP: {
            RObject protected_s(s);
            int n = XLENGTH(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                Rcomplex val = COMPLEX(s)[i];

                double real = val.r;
                for (int j = 0; j < sizeof(double); j++)
                {
                    string str_val = char_to_hex(((unsigned char *) &real)[j]);
                    elems += ((i == 0 && j == 0) ? "" : ",") + str_val;
                }

                double imag = val.i;
                for (int j = 0; j < sizeof(double); j++)
                {
                    string str_val = char_to_hex(((unsigned char *) &imag)[j]);
                    elems += "," + str_val;
                }
            }
            string decoded = "readBin(as.raw(c(" + elems + ")), n=" + to_string(n) + ", \"complex\")";
            return wrap_in_attributes(s, n == 0 ? "complex(0)" : decoded, true); }
        case STRSXP: {
            RObject protected_s(s);
            int n = XLENGTH(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                SEXP val = STRING_ELT(s, i);
                string str_val = "NA_character_";
                if (val != NA_STRING) {
                    str_val = to_string_literal(CHAR(val));
                }
                elems += (i == 0 ? "" : ",") + str_val;
            }
            return wrap_in_attributes(s, n == 0 ? "character(0)" : n == 1 ? elems : "c(" + elems + ")", true); }
        case SYMSXP: {
            RObject protected_s(s);
            string symbol = string(CHAR(PRINTNAME(s)));

            if (symbol.empty()) {
                return quote ? "quote(expr=)" : "";
            } else {
                return quote ? "quote(" + symbol  + ")" : symbol;
            }
        }
        case ENVSXP: {
            RObject protected_s(s);

            if (visited_environments.find(s) != visited_environments.end()) {
                throw cycle_error();
            }

            visited_environments.insert(s);

            SEXP parent = ENCLOS(s);
            SEXP names = R_lsInternal3(s, TRUE, FALSE);
            int n = XLENGTH(names);

            string elems;
            for (int i = 0; i < n; i++) {
                const char *key = CHAR(STRING_ELT(names, i));
                SEXP value = Rf_findVarInFrame(s, Rf_install(key));

                elems += escape_name(key) + "=" + serialize(value, true);
                elems += i + 1 < n ? ", " : "";
            }

            string parent_env_arg;
            if (!Rf_isNull(parent)) {
                string parent_env;

                if (parent == R_EmptyEnv) {
                    parent_env = "emptyenv()";
                } else if (parent == R_GlobalEnv) {
                    parent_env = "globalenv()";
                } else if (parent == R_BaseEnv || parent == R_BaseNamespace) {
                    parent_env = "baseenv()";
                } else if (R_IsPackageEnv(parent) == TRUE) {
                    // cf. builtin.c:432 do_envirName
                    string parent_name = CHAR(STRING_ELT(R_PackageEnvName(parent), 0));

                    parent_env = "as.environment(\"" + parent_name + "\")";
                } else if (R_IsNamespaceEnv(parent) == TRUE) {
                    // cf. builtin.c:434 do_envirName
                    string parent_name = CHAR(STRING_ELT(R_NamespaceEnvSpec(parent), 0));

                    parent_env = "getNamespace(\"" + parent_name + "\")";
                } else {
                    parent_env = serialize(parent, false);
                }

                parent_env_arg = ", parent=" + parent_env;
            }

            visited_environments.erase(s);

            return "list2env(list(" + elems + ")" + parent_env_arg + ")";
        }
        case LISTSXP: {/* pairlists */
            // TODO: when will this happen?
            RObject protected_s(s);
            stringstream outStr;
            outStr << "\"alist(";
            SEXP names = Rf_getAttrib(s, R_NamesSymbol);
            int i = 0;

            for (SEXP con = s; con != R_NilValue; con = CDR(con))
            {
                if (i != 0) outStr << ", ";

                outStr << escape_name(CHAR(STRING_ELT(names, i++))) << " = ";
                auto val = serialize(CAR(con), false);
                if (val != "")
                    outStr << val;
            }
            outStr << ")\"";
            return outStr.str(); }
        case LANGSXP: {
            RObject protected_s(s);
            string fun = serialize(CAR(s), false);
            string res;
            s = CDR(s);

            if (is_infix_fun(fun)) {
                SEXP lhs = CAR(s);
                SEXP rhs = CADR(s);

                string space = is_infix_fun_no_space(fun) ? "" : " ";

                res =
                    serialize(lhs, false) +
                    space + fun + space +
                    serialize(rhs, false);

            } else if (fun[0] == '[') {
                string collection = serialize(CAR(s), false);
                string subset = format_arguments(CDR(s));
                string close;

                if (fun == "[") {
                    close = "]";
                } else if (fun == "[[") {
                    close = "]]";
                } else {
                    throw serialization_error("Unknown sub-setting operator: " + fun);
                }

                res = collection + fun + subset + close;
            } else if (fun == "function") {
                string args = format_arguments(CAR(s));
                string body = serialize(CADR(s), false);

                res = fun + "(" + args + ") " + body;
            } else if (fun == "{") {
                string args = format_arguments(s, "\n\t");
                res = "{\n\t" + args + "\n}";
            } else {
                string args = format_arguments(s);
                res = fun + "(" + args + ")";
            }

            return res;
        }
            // the following is annoying, but the sexptype2char from memory.c is not public
        case SPECIALSXP:
            throw sexp_not_supported_error("SPECIALSXP");
        case BUILTINSXP:
            throw sexp_not_supported_error("BUILTINSXP");
        case EXTPTRSXP:
            throw sexp_not_supported_error("EXTPTRSXP");
        case BCODESXP:
            throw sexp_not_supported_error("BCODESXP");
        case WEAKREFSXP:
            throw sexp_not_supported_error("WEAKREFSXP");
        case CLOSXP:
            throw sexp_not_supported_error("CLOSXP");
        case DOTSXP:
            throw sexp_not_supported_error("DOTSXP");
        case CHARSXP:
            throw sexp_not_supported_error("CHARSXP");
        case EXPRSXP:
            throw sexp_not_supported_error("EXPRSXP");
        case RAWSXP:
            throw sexp_not_supported_error("RAWSXP");
        case PROMSXP:
            throw sexp_not_supported_error("PROMSXP");
        case S4SXP:
            throw sexp_not_supported_error("S4SXP");
        default:
            throw sexp_not_supported_error("unknown");
        }
    }
};

// [[Rcpp::export]]
std::string serialize_value(SEXP s) {
    Serializer serializer;

    return serializer.serialize(s, false);
}
