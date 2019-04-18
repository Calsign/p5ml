
#include <string.h>
#include <limits.h>
#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define CAML_NAME_SPACE

// Jane Street: https://github.com/janestreet/core/blob/master/src/unix_stubs.c
value caml_realpath(value v_path)
{
    const char *path = String_val(v_path);
    char *res = realpath(path, NULL);
    if (res == NULL) {
        caml_failwith("realpath");
    } else {
        value v_res = caml_copy_string(res);
        free(res);
        return v_res;
    }
}
