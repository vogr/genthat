#include <iostream>
#include <fstream>
#include <exception>
#include "testr.h"
#include <sys/stat.h>
#define MAX_FILE_SIZE 50 * 1000000
using namespace Rcpp;
using namespace std;

class serialization_error : public std::exception {
public:
    const string msg;
    serialization_error(string msg) : msg(msg) {}
};

std::string kFuncPrefix = "func: ";
std::string kArgsPrefix = "argv: ";
std::string kRetvPrefix = "retv: ";

std::ofstream tracefile;

void printCapture(CharacterVector x, std::string prefix) {
    if (x[0] != "NULL"){
        if (x.length() < 1000) {
            for (int i = 0; i < x.length(); i++) {
                tracefile << prefix << x[i] << std::endl;
            }
        } else {
            tracefile << prefix << "<too long>" << std::endl;
        }
    }
}

string serialize(SEXP value)
{
    Environment testr = Environment::namespace_env("testr");
    Function Rfn(testr.find("serialize"));
    SEXP res = Rfn(value);
    cout << "computed value!" << endl;
    SEXP resType = Rf_getAttrib(res, Rf_install("class"));
    cout << "got res type:" << endl;
    print(resType);
    if (Rf_isNull(resType)) {
        cout << "is null!" << endl;
        print(res);
        cout << "is list: " << Rf_isList(res) << endl;
        string sres = as<string>(res);
        cout << "converted to string!" << endl;
        return sres;
    } else {
        if (as<string>(resType) == "GENTHAT_SERIALIZE_ERROR") {
            cout << "is error" << endl;
            SEXP errMsg = Rf_getAttrib(res, Rf_install("message"));
            throw serialization_error(as<string>(errMsg));
        } else {
            cout << "is unknown" << endl;
            throw serialization_error("unexpected return value from serialize()");
        }
    }
}

int captureFileNumber = 0;

inline int get_file_size(std::string path)
{
    struct stat stat_buf;
    stat(path.c_str(), &stat_buf);
    return stat_buf.st_size;
}

// [[Rcpp::export]]
void WriteCapInfo_cpp (CharacterVector fname, SEXP args_env, SEXP retv_env) {
    Environment testr = Environment::namespace_env("testr");
    Function options("testr_options");
    Environment cache = testr.get("cache");
    if (as<bool>(options("IO"))) {

        cout << ("TRACE START!") << endl;
        string traceFile = as<string>(cache.get("trace_path"));
        traceFile += "/";
        traceFile += as<string>(options("capture.file"));
        traceFile += ".";
        traceFile += to_string(captureFileNumber);
        tracefile.open(traceFile.c_str(), std::ios::app);

        string args;
        try {
            args = serialize(GetArgs(args_env));
        } catch (serialization_error e) {
            args = "<unserializable " + e.msg + ">";
        }

        string retv;
        try {
            retv = serialize(retv_env);
        } catch (serialization_error e) {
            retv = "<unserializable " + e.msg + ">";
        }

        printCapture(fname, kFuncPrefix);
        printCapture(args, kArgsPrefix);
        printCapture(retv, kRetvPrefix);

        tracefile << std::endl;
        tracefile.close();

        cout << ("TRACE END!") << endl;

        if (get_file_size(traceFile) > MAX_FILE_SIZE)
            captureFileNumber++;
    } else {
        GetArgs(args_env);
    }
}

