#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>

typedef void fork_fn(int errors, value v_args);

Caml_inline value Val_fork_fn(fork_fn *fn) {
  return caml_copy_nativeint((intnat) fn);
}

static void action_dup2(int errors, value v_config) {
  int oldfd = Int_val(Field(v_config, 1));
  int newfd = Int_val(Field(v_config, 2));
  if (dup2(oldfd, newfd) == -1) _exit(oldfd);
}

CAMLprim value eio_unix_fork_dup2(value v_unit) {
  return Val_fork_fn(action_dup2);
}
