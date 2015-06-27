
#include "caml/memory.h"
#include "caml/mlvalues.h"


#ifdef __cplusplus
extern "C" {
#endif

int add_int (int x, int y) {
    return x + y;
}

value add_int_ml(value ml_x, value ml_y) {
    CAMLparam2(ml_x,ml_y);
    CAMLlocal1(ml_sum);

    int sum = add_int(Int_val(ml_x), Int_val(ml_y));

    ml_sum = Val_int(sum);
    CAMLreturn(ml_sum);
}

#ifdef __cplusplus
}
#endif
