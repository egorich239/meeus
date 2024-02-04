#ifndef MEEUS_DATE_H__
#define MEEUS_DATE_H__

#ifdef SWIG
%module pybindings
%{
#include "date.h"
%}
#endif

struct jd {
  double rep;
};

extern int make_jd_gregorian(int y, int m, float d, struct jd *out);
extern int make_jd_julian(int y, int m, float d, struct jd *out);

#endif