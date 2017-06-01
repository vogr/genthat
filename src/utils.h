#ifndef __UTILS_H
#define __UTILS_H

#include <string>
#include <Rdefines.h>

std::string environment_name(SEXP env);
std::string environment_name_as_code(SEXP env);

#endif // __UTILS_H
