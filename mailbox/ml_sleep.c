
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/threads.h"


#include <unistd.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

value ml_c_sleep(value ml_second) {
    CAMLparam1(ml_second);

    int s = Int_val(ml_second);
    caml_release_runtime_system();
    for (size_t i = 0; i < s;++i) {
      printf("sleep[%3d])\n", (int)i);
      sleep(1);
    }
    caml_acquire_runtime_system();

    CAMLreturn(Val_unit);
}

#ifdef __cplusplus
}
#endif
