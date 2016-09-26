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

std::string kPkgPrefix  = "pkg: ";
std::string kFuncPrefix = "func: ";
std::string kArgsPrefix = "argv: ";
std::string kRetvPrefix = "retv: ";

std::ofstream tracefile;

void printCapture(CharacterVector x, std::string prefix) {
    if (x.length() < 1000) {
        for (int i = 0; i < x.length(); i++) {
            tracefile << prefix << x[i] << std::endl;
        }
    } else {
        tracefile << prefix << "<too long>" << std::endl;
    }
}

SEXP serialize_error = nullptr;

// [[Rcpp::export]]
SEXP serialize_error_handler(SEXP e)
{
    serialize_error = e;
    return e;
}

string serialize(SEXP value)
{
    Environment testr = Environment::namespace_env("testr");
    Function Rfn(testr.find("serialize"));
    serialize_error = nullptr;
    SEXP res = Rfn(value);
    if (serialize_error != nullptr) {
        throw serialization_error(as<string>(serialize_error));
    } else {
        return as<string>(res);
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
    SEXP originalErrMsg = Function("geterrmessage")();
    if (as<bool>(options("IO"))) {

        string traceFile = as<string>(cache.get("trace_path"));
        traceFile += "/";
        traceFile += as<string>(options("capture.file"));
        traceFile += ".";
        traceFile += to_string(captureFileNumber);
        tracefile.open(traceFile.c_str(), std::ios::app);

        if (get_file_size(traceFile) == 0) {
            string pkgName = as<string>(cache.get("pkg.name"));
            tracefile << kPkgPrefix << pkgName << std::endl << std::endl;
        }

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

        if (get_file_size(traceFile) > MAX_FILE_SIZE)
            captureFileNumber++;
    } else {
        GetArgs(args_env);
    }

    Function replaceError(testr.find("replaceError"));
    replaceError(originalErrMsg);
}

