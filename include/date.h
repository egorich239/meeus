#ifndef MEEUS_DATE_H__
#define MEEUS_DATE_H__

struct jd;

extern int make_jd_gregorian(struct jd *out, int y, int m, float d);
extern int make_jd_julian(struct jd *out, int y, int m, float d);

#endif