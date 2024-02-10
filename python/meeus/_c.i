%module _c

%{
#include "include/meeus/meeus.h"
%}

%include "include/meeus/date.h"

struct jd* jm_create_jd();
void jm_free(void*);

%{

struct jd* jm_create_jd() {
    return calloc(1, sizeof(struct jd));
}

void jm_free(void *arg) {
    free(arg);
}

%}