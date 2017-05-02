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


string serialize_value(SEXP s);
string do_serialize_value(SEXP s);
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

static bool is_syntactic_name(const char *name)
{
	auto keywords = string("^(if"
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
		"|NA_character_)$");

	try 
	{
          auto keyword_regex = regex(keywords);
          if (regex_match(name, keyword_regex))
            return false;
	}
	catch (regex_error& e)
	{
          throw serialization_error("keyword regex failed with code: " + std::to_string(e.code()));
	}

	try
	{
		auto name_regex = regex("^([a-zA-Z][a-zA-Z0-9._]*|[.]([a-zA-Z._][a-zA-Z0-9._]*)?)$");
		return regex_match(name, name_regex);
	}
	catch (regex_error& e)
	{
          throw serialization_error("syntactic name regex failed with code: " + std::to_string(e.code()));
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
    if (hasAttributes(s)) {
        string elems = "";
		for (SEXP a = ATTRIB(s); !Rf_isNull(a); a = CDR(a))
        {
			if (TAG(a) != Rf_install("srcref")) {
                elems += ", " + symbol_to_attrib_key(a) + "=" + do_serialize_value(CAR(a));
            }
		}
        return "structure(" + s_str + elems + ")";
    } else {
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

// [[Rcpp::export]]
std::string serialize_value(SEXP s)
{
    visited_environments.clear();

    string res = do_serialize_value(s);

    visited_environments.clear();

    return res;
}

string escape_list_key(string name)
{
    if (is_syntactic_name(name.c_str())) {
        return name;
    } else {
        return "`" + escape_attrib_name(name.c_str()) + "`";
    }
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
        elems += (i == 0 ? "" : ", ") + escape_list_key(string(CHAR(key))) + "=" + do_serialize_value(value);
    }
    return "as.environment(list(" + elems + "))";
}


string do_serialize_value(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
        return "NULL";
    case VECSXP: { /* lists */
        RObject protected_s(s);
        int n = XLENGTH(s);
        string ret = "list(";
        SEXP names = Rf_getAttrib(s, R_NamesSymbol);
        for (int i = 0 ; i < n ; i++)
        {
            SEXP val = VECTOR_ELT(s, i);
            string label = (names == R_NilValue || string(CHAR(STRING_ELT(names, i))) == "") ?
                                "" :
                                (escape_list_key(string(CHAR(STRING_ELT(names, i)))) + "=");
            ret += string(i == 0 ? "" : ",") + label + do_serialize_value(val);
        }
        ret += ")";

        // do not wrap if the only attribute is names
        vector<string> attrs = protected_s.attributeNames();
        if (attrs.size() == 1 && attrs.at(0) == as<string>(PRINTNAME(R_NamesSymbol))) {
          return ret;
        } else {
          return wrap_in_attributes(s, ret);
        }
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
        return wrap_in_attributes(s, n == 0 ? "logical(0)" : n == 1 ? elems : "c(" + elems + ")"); }
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
        return wrap_in_attributes(s, n == 0 ? "integer(0)" : n == 1 ? elems : "c(" + elems + ")"); }
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
        return wrap_in_attributes(s, n == 0 ? "double(0)" : decoded); }
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
        return wrap_in_attributes(s, n == 0 ? "complex(0)" : decoded); }
    case STRSXP: {
        RObject protected_s(s);
        int n = XLENGTH(s);
        SEXP attrs = ATTRIB(s);
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
        return wrap_in_attributes(s, n == 0 ? "character(0)" : n == 1 ? elems : "c(" + elems + ")"); }
    case SYMSXP:
        throw sexp_not_supported_error("SYMSXP");
    case ENVSXP:
        if (visited_environments.find(s) != visited_environments.end()) {
            throw cycle_error();
        }
        visited_environments.insert(s);
        return serialize_env(s);
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
    case LISTSXP: /* pairlists */
        throw sexp_not_supported_error("LISTSXP");
    case LANGSXP:
        throw sexp_not_supported_error("LANGSXP");
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
