#include <iostream>
#include <fstream>
#include <map>
#include <exception>
#include "genthat.h"
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
    Environment genthat = Environment::namespace_env("genthat");
    Function Rfn(genthat.find("serialize"));
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
    Environment genthat = Environment::namespace_env("genthat");
    Environment cache = genthat.get("cache");

    //print(args_list);
    string args;
    try {
        args = serialize_cpp(args_list);
    } catch (serialization_error e) {
        args = "<unserializable " + e.msg + ">";
    }

    //cout << "called :" << fname << "()" << endl;
    //cout << "with args :" << args << endl;
    cached_args.emplace(as<int>(call_id), make_pair(as<string>(fname), args));
}

// [[Rcpp::export]]
void exitFunction_cpp (SEXP call_id, SEXP retv_env) {
    Environment genthat = Environment::namespace_env("genthat");
    Environment cache = genthat.get("cache");

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

    string traceFile = as<string>(cache.get("capture.dir"));
    traceFile += "/capture.";
    traceFile += to_string(captureFileNumber);
    tracefile.open(traceFile.c_str(), std::ios::app);
    //if (get_file_size(traceFile) == 0) {
    //    string pkgName = as<string>(cache.get("package.name"));
    //    tracefile << kPkgPrefix << pkgName << std::endl << std::endl;
    //}
    printCapture(fname, kFuncPrefix);
    printCapture(args, kArgsPrefix);
    printCapture(retv, kRetvPrefix);
    tracefile << std::endl;
    tracefile.close();
    if (get_file_size(traceFile) > MAX_FILE_SIZE)
        captureFileNumber++;
}

