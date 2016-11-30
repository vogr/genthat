#include <iostream>
#include <fstream>
#include <map>
#include <exception>
#include "testr.h"
#include <sys/stat.h>
#define MAX_FILE_SIZE 50 * 1000000
using namespace Rcpp;
using namespace std;

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

map<int,pair<string,string>> cached_args;

// [[Rcpp::export]]
void enterFunction_cpp (CharacterVector fname, SEXP args_list, SEXP call_id) {
    Environment testr = Environment::namespace_env("testr");
    Function options("testr_options");
    Environment cache = testr.get("cache");
    SEXP originalErrMsg = Function("geterrmessage")();
    if (as<bool>(options("IO"))) {

        //print(args_list);
        string args;
        try {
            cout << "args_list:" << endl;
            print(args_list);
            args = serialize_cpp(args_list);
        } catch (serialization_error e) {
            args = "<unserializable " + e.msg + ">";
        }

        //cout << "called :" << fname << "()" << endl;
        //cout << "with args :" << args << endl;
        cout << "args map size: " << cached_args.size() << endl;
        cached_args.emplace(as<int>(call_id), make_pair(as<string>(fname), args));
    }
    Function replaceError(testr.find("replaceError"));
    replaceError(originalErrMsg);
}

// [[Rcpp::export]]
void exitFunction_cpp (SEXP call_id, SEXP retv_env) {
    SEXP originalErrMsg = PROTECT(Function("geterrmessage")());
    Environment testr = Environment::namespace_env("testr");
    Function options("testr_options");
    Environment cache = testr.get("cache");

    auto cached_call = cached_args.find(as<int>(call_id));
    if (cached_call == cached_args.end()) {
        cerr << "Terminated non-initialized call!" <<endl;
        return;
    }
    auto call = cached_call->second;
    string fname = call.first;
    string args = call.second;
    cached_args.erase(cached_call);

    string retv;
    try {
        retv = serialize_cpp(retv_env);
    } catch (serialization_error e) {
        retv = "<unserializable " + e.msg + ">";
    }

    cout << "serialized retv!!! " << endl;
    cout << "args: " << endl;
    cout << args << endl;
    cout << "retv: " << endl;
    cout << retv << endl;

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
        printCapture(fname, kFuncPrefix);
        printCapture(args, kArgsPrefix);
        printCapture(retv, kRetvPrefix);
        tracefile << std::endl;
        tracefile.close();
        if (get_file_size(traceFile) > MAX_FILE_SIZE)
            captureFileNumber++;
    }

    Function replaceError(testr.find("replaceError"));
    replaceError(originalErrMsg);
    UNPROTECT_PTR(originalErrMsg);
}

