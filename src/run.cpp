#include <Rcpp.h>
#include <R.h>

#include <iostream>
#include <fstream>

using namespace std;

Rcpp::List as_data_frame(Rcpp::List &list) {
    std::vector<int> seq(list.size());
    std::iota(seq.begin(), seq.end(), 1);

    list.attr("class") = "data.frame";
    list.attr("row.names") = seq;

    return list;
}

// [[Rcpp::export]]
Rcpp::List run_generated_tests_cpp(Rcpp::DataFrame const &tests, bool show_progress) {

    Rcpp::Environment base("package:base");
    Rcpp::Environment genthat("package:genthat");

    Rcpp::Function tempfile_r = base["tempfile"];
    Rcpp::Function test_file_r = genthat["test_file"];

    int size = tests.nrows();

    Rcpp::StringVector codes = tests["code"];
    Rcpp::StringVector traces = tests["trace"];

    Rcpp::String test_file_name = tempfile_r();

    // column-wise vectors that will be used to create the final data frames
    auto c_trace = vector<string>();
    auto c_code = vector<string>();
    auto c_test = vector<string>();
    auto c_result = vector<int>();
    auto c_error = vector<string>();
    auto c_out = vector<string>();
    auto c_err = vector<string>();
    auto c_time = vector<int>();

    for (int i=0; i<size; i++) {
        if (Rcpp::StringVector::is_na(codes[i])) {
            continue;
        }

        auto code = Rcpp::as<string>(codes[i]);
        auto trace = Rcpp::as<string>(traces[i]);

        ofstream test_file;

        test_file.open(test_file_name.get_cstring());
        test_file << code;
        test_file.close();

        Rcpp::List capture = test_file_r(test_file_name);

        if (capture.containsElementNamed("result")) {
            Rcpp::List run = capture["result"];

            int nb = run["nb"];
            int failed = run["failed"];
            bool error = run["error"];

            int result = nb;
            if (failed) {
                result = 2;
            } else if (error) {
                result = 3;
            }

            c_test.push_back(Rcpp::as<string>(run["test"]));
            c_result.push_back(result);
            c_error.push_back("");
            c_time.push_back(Rcpp::as<int>(capture["time"]));
        } else {
            Rcpp::List error = capture["error"];
            std::string msg = Rcpp::as<string>(error["message"]);

            c_test.push_back("");
            c_result.push_back(NA_INTEGER);
            c_error.push_back(msg);
            c_time.push_back(NA_INTEGER);
        }

        c_trace.push_back(trace);
        c_code.push_back(code);
        c_out.push_back(capture["out"]);
        c_err.push_back(capture["err"]);
    }

    auto res = Rcpp::List::create(
        Rcpp::_["trace"]=Rcpp::wrap(c_trace),
        Rcpp::_["code"]=Rcpp::wrap(c_code),
        Rcpp::_["test"]=Rcpp::wrap(c_test),
        Rcpp::_["result"]=Rcpp::wrap(c_result),
        Rcpp::_["error"]=Rcpp::wrap(c_error),
        Rcpp::_["out"]=Rcpp::wrap(c_out),
        Rcpp::_["err"]=Rcpp::wrap(c_err),
        Rcpp::_["time"]=Rcpp::wrap(c_time),
        Rcpp::_["stringsAsFactors"]=false
        );

    return as_data_frame(res);
}
