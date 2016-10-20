
#include "testr.h"

using namespace Rcpp;
using namespace std;

string serialize_cpp(SEXP s);
string serialize_cpp0(SEXP s);
string to_string_literal(const char *x);

static string char_to_hex(unsigned char c)
{
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

/* check for attributes other than function source */
static Rboolean hasAttributes(SEXP s)
{
    for (SEXP a = ATTRIB(s); !Rf_isNull(a); a = CDR(a))
    {
        if (TAG(a) != Rf_install("srcref")) return TRUE;
    }
    return FALSE;
}

static string serialize_env(SEXP s)
{
    SEXP names = R_lsInternal3(s, TRUE, FALSE);
    int n = XLENGTH(names);
    string elems = "";
    for (int i = 0; i < n; i++)
    {
        SEXP key = STRING_ELT(names, i);
        SEXP value = Rf_findVarInFrame(s, Rf_install(CHAR(key)));
        elems += (i == 0 ? "" : ", ") + string(CHAR(key)) + "=" + serialize_cpp0(value);
    }
    return "as.environment(list(" + elems + "))";
}

static string simple_real_to_string(double val)
{
    static char buff[1000];
    sprintf(buff, "%g", val);
    return string(buff);
}

static string real_to_string(double val)
{
    return R_IsNA(val)  ? "NA" :
           R_IsNaN(val) ? "NaN" :
           val == R_PosInf ? "Inf" :
           val == R_NegInf ? "-Inf" : StringFromReal(val);
}

static string complex_scalar_to_string(Rcomplex val)
{
    double real = val.r;
    double imag = val.i;
    bool isNA = (R_IsNA(real) || R_IsNA(imag));
    if (isNA) {
        return "NA";
    } else {
        string real_str = real_to_string(real);
        string imag_str = real_to_string(imag) + "i";
        string separator = imag_str[0] == '-' ? "" : "+";
        return real_str + separator + imag_str;
    }
}

static bool is_syntactic_name(const char *name)
{
    const char *error_msg = NULL;
    int error_n;
    {
        const char *keyword_regex = "^(if"
                     "|else"
                     "|repeat"
                     "|while"
                     "|function"
                     "|for"
                     "|in"
                     "|next"
                     "|break"
                     "|TRUE"
                     "|FALSE"
                     "|NULL"
                     "|Inf"
                     "|NaN"
                     "|NA"
                     "|NA_integer_"
                     "|NA_real_"
                     "|NA_complex_"
                     "|NA_character_)$";
        pcre *keywrd = pcre_compile(keyword_regex, PCRE_ANCHORED, &error_msg, &error_n, NULL);;
        if (keywrd == NULL) {
            throw serialization_error("couldn't compile regex!");
        }
        int ovector[2];
        int match = pcre_exec(keywrd, NULL, name, strlen(name), 0, 0, (int *) &ovector, 2);
        if (match < -1) {
            throw serialization_error("pcre_exec failed!");
        } else if (match != -1) { /* PCRE_ERROR_NOMATCH */
            return false; // is keyword
        }
    }
    { 
        const char *syntactic_name_regex = "^([a-zA-Z][a-zA-Z0-9._]*|[.]([a-zA-Z._][a-zA-Z0-9._]*)?)$";
        pcre *syntct = pcre_compile(syntactic_name_regex, PCRE_ANCHORED, &error_msg, &error_n, NULL);;
        if (syntct == NULL) {
            throw serialization_error("couldn't compile regex!");
        }
        int ovector[2];
        int match = pcre_exec(syntct, NULL, name, strlen(name), 0, 0, (int *) &ovector, 2);
        if (match < -1) {
            throw serialization_error("pcre_exec failed!");
        } else if (match == -1) { /* PCRE_ERROR_NOMATCH */
            return false;
        } else {
            return true;
        }
    }
}

string escape_attrib_name(string str)
{
    int str_size = str.size();
    const char* in_buff = str.c_str();
    char *out_buff = (char *) malloc(2 * str_size + 1);
    int j = 0;
    for (int i = 0; i < str_size; i++, j++)
    {
        if (in_buff[i] == '`') {
            out_buff[j++] = '\\';
            out_buff[j] = in_buff[i];
        } else {
            out_buff[j] = in_buff[i];
        }
    }
    out_buff[j] = '\0';
    string ret(out_buff);
    free(out_buff);
    return ret;
}

string symbol_to_attrib_key(SEXP a)
{
    if(TAG(a) == R_DimSymbol) {
        return ".Dim";
    }
    else if(TAG(a) == R_DimNamesSymbol) {
        return ".Dimnames";
    }
    else if(TAG(a) == R_NamesSymbol) {
        return ".Names";
    }
    else if(TAG(a) == R_TspSymbol) {
        return ".Tsp";
    }
    else if(TAG(a) == R_LevelsSymbol) {
        return ".Label";
    }
    else {
        const char *tag = CHAR(PRINTNAME(TAG(a)));
        if (is_syntactic_name(tag)) {
            return string(tag);
        } else {
            return "`" + escape_attrib_name(tag) + "`";
        }
    }
}

/**
 * @param s is the expression we are serializing
 * @param s_str is the serialized reprezentation of s but without the attributes
 */
string wrap_in_attributes(SEXP s, string s_str)
{
    PROTECT(s);
    if (hasAttributes(s)) {
        string elems = "";
		for (SEXP a = ATTRIB(s); !Rf_isNull(a); a = CDR(a))
        {
			if (TAG(a) != Rf_install("srcref")) {
                elems += ", " + symbol_to_attrib_key(a) + "=" + serialize_cpp0(CAR(a));
            }
		}
        UNPROTECT(1);
        return "structure(" + s_str + elems + ")";
    } else {
        UNPROTECT(1);
        return s_str;
    }
}

inline char hex_digit_to_char(int x)
{
    return x < 10 ? x + '0' : x + 'a' - 9; 
}

string to_string_literal(const char *x)
{
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

set<SEXP> visited_environments;

string serialize_cpp(SEXP s)
{
    visited_environments.clear();

    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

    string res = serialize_cpp0(s);

    std::chrono::steady_clock::time_point end= std::chrono::steady_clock::now();
    auto diffTime = std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count();
    cout << "serialize duration: " << diffTime << "ms" << endl;

    visited_environments.clear();
    return res;
}

string serialize_cpp0(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
        return "NULL";
    case VECSXP: { /* lists */
        PROTECT(s);
        try {
            int n = XLENGTH(s);
            string ret = "list(";
            SEXP names = Rf_getAttrib(s, R_NamesSymbol);
            for (int i = 0 ; i < n ; i++)
            {
                SEXP val = VECTOR_ELT(s, i);
                string label = (names == R_NilValue || string(CHAR(STRING_ELT(names, i))) == "") ?
                                    "" :
                                    (string(CHAR(STRING_ELT(names, i))) + "=");
                ret += string(i == 0 ? "" : ",") + label + serialize_cpp0(val);
            }
            ret += ")";
            UNPROTECT(1);
            return wrap_in_attributes(s, ret);
        } catch (std::exception e) {
            UNPROTECT(1);
            throw;
        } }
    case LGLSXP:
        throw sexp_not_implemented("EXPRSXP");
    case INTSXP: {
        PROTECT(s);
        try {
            int n = XLENGTH(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                int val = INTEGER(s)[i];
                string str_val = val == NA_INTEGER ? "NA" : (to_string(val) + "L");
                elems += (i == 0 ? "" : ",") + str_val;
            }
            UNPROTECT(1);
            return wrap_in_attributes(s, n == 1 ? elems : "c(" + elems + ")");
        } catch (std::exception e) {
            UNPROTECT(1);
            throw;
        } }
    case REALSXP: {
        PROTECT(s);
        try {
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
            UNPROTECT(1);
            string decoded = "readBin(as.raw(c(" + elems + ")), n=" + to_string(n) + ", \"double\")";
            return wrap_in_attributes(s, decoded);
        } catch (std::exception e) {
            UNPROTECT(1);
            throw;
        } }
    case CPLXSXP: {
        PROTECT(s);
        try {
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
            UNPROTECT(1);
            string decoded = "readBin(as.raw(c(" + elems + ")), n=" + to_string(n) + ", \"complex\")";
            return wrap_in_attributes(s, decoded);
        } catch (std::exception e) {
            UNPROTECT(1);
            throw;
        } }
    case STRSXP: {
        PROTECT(s);
        try {
            int n = XLENGTH(s);
            SEXP attrs = ATTRIB(s);
            string elems = "";
            for (int i = 0; i < n; i++)
            {
                SEXP val = STRING_ELT(s, i);
                string str_val = "NA";
                if (val != NA_STRING) {
                    str_val = to_string_literal(CHAR(val));
                }
                elems += (i == 0 ? "" : ",") + str_val;
            }
            UNPROTECT(1);
            return wrap_in_attributes(s, n == 1 ? elems : "c(" + elems + ")");
        } catch (std::exception e) {
            UNPROTECT(1);
            throw;
        } }
    case SYMSXP:
        throw sexp_not_implemented("SYMSXP");
    case ENVSXP:
        if (visited_environments.find(s) != visited_environments.end()) {
            throw cycle_in_structure();
        }
        visited_environments.emplace(s);
        return serialize_env(s);
    case SPECIALSXP:
        throw sexp_not_implemented("SPECIALSXP");
    case BUILTINSXP:
        throw sexp_not_implemented("BUILTINSXP");
    case EXTPTRSXP:
        throw sexp_not_implemented("EXTPTRSXP");
    case BCODESXP:
        throw sexp_not_implemented("BCODESXP");
    case WEAKREFSXP:
        throw sexp_not_implemented("WEAKREFSXP");
    case CLOSXP:
        throw sexp_not_implemented("CLOSXP");
    case LISTSXP: /* pairlists */
        throw sexp_not_implemented("LISTSXP");
    case LANGSXP:
        throw sexp_not_implemented("LANGSXP");
    case DOTSXP:
        throw sexp_not_implemented("DOTSXP");
    case CHARSXP:
        throw sexp_not_implemented("CHARSXP");
    case EXPRSXP:
        throw sexp_not_implemented("EXPRSXP");
    case RAWSXP:
        throw sexp_not_implemented("RAWSXP");
    case PROMSXP:
        throw sexp_not_implemented("PROMSXP");
    case S4SXP:
        throw sexp_not_implemented("S4SXP");
    default:
        throw sexp_not_implemented("unknown");
    }
}

// [[Rcpp::export]]
SEXP serialize_r(SEXP s)
{
    try {
        string str_rep = serialize_cpp(s);
        return Rf_mkString(str_rep.c_str());
    } catch(sexp_not_implemented e) {
        Rf_error(e.to_string().c_str());
    }
}

