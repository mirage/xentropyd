/*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008     Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <xenctrl.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

/* Can't use the standard library function because the file has size '0' which
   causes the stdlib to attempt to extend it. */
CAMLprim value stub_mmap(value fd, value len)
{
  CAMLparam2(fd, len);
  CAMLlocal1(result);

  void *buf = mmap(NULL, Int_val(len), PROT_READ | PROT_WRITE, MAP_SHARED, Int_val(fd), 0);
  if (buf == MAP_FAILED) {
       syslog(LOG_ERR, "mmap(NULL, %d, PROT_READ | PROT_WRITE, MAP_SHARED, %d, 0) = %d:%s", Int_val(len), Int_val(fd), errno, strerror(errno));
       /* If we can't mmap (dom0's ring) then we should quit */
       abort();
  }
  result = alloc_bigarray_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, buf, Int_val(len));
  CAMLreturn(result);
}

static xc_interface *get_xc_interface() {
  static xc_interface *xch = NULL;
  if (!xch) {
    xch = xc_interface_open(NULL, NULL, 0);
    if (!xch) {
      syslog(LOG_ERR, "xc_interface_open: %d:%s", errno, strerror(errno));
      caml_failwith("xc_domain_getinfolist failed");
    }
  }
  return xch;
}

CAMLprim value
stub_domain_getinfolist(value lowest_domid, value number_requested, value result)
{
  CAMLparam3(lowest_domid, number_requested, result);
  CAMLlocal2(v_ba, v_ofs);

  const xc_error *error;
  int ret = 0;
  int lowest_domid_ = Int_val(lowest_domid);
  int number_requested_ = Int_val(number_requested);
  void *result_;
  xc_interface *xch = get_xc_interface();

  v_ba = Field(result, 0);
  v_ofs = Field(result, 1);
  result_ = Caml_ba_data_val(v_ba) + Int_val(v_ofs);

  ret = xc_domain_getinfolist(xch, lowest_domid_, number_requested_, result_);
  if (ret < 0) {
    error = xc_get_last_error(xch);
    syslog(LOG_ERR, "xc_domain_getinfolist(%p, %d, %d, %p) = %d:%s", (void*) xch, lowest_domid_, number_requested_, result_, error->code, xc_error_code_to_desc(error->code));
    caml_failwith("xc_domain_getinfolist failed");
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value stub_sizeof_domaininfo_t(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_int(sizeof(xc_domaininfo_t)));
}

CAMLprim value stub_domaininfo_t_parse(value cstruct)
{
  CAMLparam1(cstruct);
  CAMLlocal3(result, v_ba, v_ofs);
  void *addr;
  v_ba = Field(cstruct, 0);
  v_ofs = Field(cstruct, 1);
  addr = Caml_ba_data_val(v_ba) + Int_val(v_ofs);

  xc_domaininfo_t *di = addr;
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(di->domain));
  Store_field(result, 1, Val_bool(di->flags & XEN_DOMINF_dying));
  Store_field(result, 2, Val_bool(di->flags & XEN_DOMINF_shutdown));

  CAMLreturn(result);
}

CAMLprim value
stub_map_foreign(value domid, value mfn)
{
  CAMLparam2(domid, mfn);
  CAMLlocal1(result);
  xc_interface *xch = get_xc_interface();
  uint32_t domid_ = Int_val(domid);
  int mfn_ = Nativeint_val(mfn);
  void *buf = xc_map_foreign_range(xch, domid_, getpagesize(), PROT_READ|PROT_WRITE, mfn_);
  if (!buf){
    syslog(LOG_ERR, "xc_map_foreign_range(%d, %d, %x) failed", domid_, getpagesize(), mfn_);
    caml_failwith("xc_map_foreign_range");
  }
  result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, buf, getpagesize());
  CAMLreturn(result);
}

CAMLprim value
stub_unmap_foreign(value ba)
{
  CAMLparam1(ba);
  int ret = munmap(Data_bigarray_val(ba), getpagesize());
  if (ret != 0)
    syslog(LOG_ERR, "munmap %p = %d:%s", (void*) (Data_bigarray_val(ba)), errno, strerror(errno));
  CAMLreturn(Val_unit);
}
