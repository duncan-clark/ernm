// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

Rcpp::List ernm_hello_world();

static bool validateExported(const std::string& sig) {
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("Rcpp::List(*ernm_hello_world)()");
    }
    return signatures.find(sig) != signatures.end();
}

RCPP_MODULE(ErnmExtension_RcppExports) {
    Rcpp::function("ernm_hello_world", &ernm_hello_world, Rcpp::List::create());
    Rcpp::function("RcppExports_validateExported", &validateExported);
}
