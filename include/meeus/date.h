#ifndef MEEUS_DATE_H__
#define MEEUS_DATE_H__

struct jd;

extern int jm_jd_set_gregorian(struct jd *out, int y, int m, float d);
extern int jm_jd_set_julian(struct jd *out, int y, int m, float d);

#endif