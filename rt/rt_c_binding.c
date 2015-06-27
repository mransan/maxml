
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include "caml/fail.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>


/* *********************************************** */
/* ******                                    ***** */
/* ******          C LIBRARY                 ***** */
/* ******                                    ***** */
/* *********************************************** */
    
typedef struct  { 
    int m_i;
    double m_d;
} t1;

t1 get_t1_value(int i) { 

    t1 v; 
    v.m_i = i; 
    v.m_d = i * 2;

    return v;
}

char* string_of_t1(t1*v) { 
    static char buffer[128];
    snprintf(&buffer[0], sizeof(buffer), "{i:%d , d:%f }", v->m_i, v->m_d);

    return &buffer[0];
}

t1* t1_create(int i) { 
    t1* v = malloc(sizeof(t1));
    v->m_i = i; 
    v->m_d = i * 2;

    return v;
}

void t1_destroy(t1* v) { 
    free(v);
}

/* MANUAL Bindings with explicit create destroy */

// ----- OCaml Utilities ---- 

static struct custom_operations t3_ops = {
    "rt.t3",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

#define T3_val(v) (*((t1**)Data_custom_val((v))))

value ml_t3_create(value ml_i) {

    CAMLparam1(ml_i);

    int i = Int_val(ml_i);

    value ml_p = caml_alloc_custom(&t3_ops, 
                                   sizeof(t1*), 
                                   0, 
                                   1);

    T3_val(ml_p) = t1_create(i);

    CAMLreturn(ml_p);
} 

value ml_t3_destroy(value ml_p) {

    CAMLparam1(ml_p); 

    t1_destroy(T3_val(ml_p));

    CAMLreturn(Val_unit);
}

value ml_string_of_t3(value ml_p) {

    CAMLparam1(ml_p); 

    CAMLlocal1(ml_s);

    char* s = string_of_t1(T3_val(ml_p));
    ml_s = caml_copy_string(s);

    CAMLreturn(ml_s);
}


/* BINDING WITH FINALIZE */

#define T4_val(v) (*((t1**)Data_custom_val((v))))

void t4_finalize(value v) {
    t1_destroy(T4_val(v));
}

static struct custom_operations t4_ops = {
    "rt.t4",
    &t4_finalize, 
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

value ml_t4_create(value ml_i) {

    CAMLparam1(ml_i);

    int i = Int_val(ml_i);

    value ml_p = caml_alloc_custom(&t4_ops, 
                                   sizeof(t1*), 
                                   0, 
                                   1);

    T4_val(ml_p) = t1_create(i);

    CAMLreturn(ml_p);
} 

value ml_string_of_t4(value ml_p) {

    CAMLparam1(ml_p); 

    CAMLlocal1(ml_s);

    char* s = string_of_t1(T4_val(ml_p));
    ml_s = caml_copy_string(s);

    CAMLreturn(ml_s);
}

#ifdef __cplusplus
}
#endif
