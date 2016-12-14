
#include "genthat.h"

extern "C" {

void Rf_formatReal(double *, R_xlen_t, int *, int *, int *, int);
const char *Rf_EncodeReal0(double, int, int, int, const char *);

}

string StringFromReal(double x)
{
    //int w, d, e;
    //Rf_PrintDefaults(); /* from global options() */
    //auto savedigits = R_print.digits;
    //R_print.digits = DBL_DIG;/* MAX precision */
    //Rf_formatReal(&x, 1, &w, &d, &e, 0);
    //R_print.digits = savedigits;/* MAX precision */
    //return string(Rf_EncodeReal0(x, w, d, e, "."));
    static char buff[60];
    sprintf(buff, "%.15f", x);
    return string(buff);
}
