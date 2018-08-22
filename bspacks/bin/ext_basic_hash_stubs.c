#include "caml/hash.h"
#include "caml/mlvalues.h"
#include <string.h>
#include <stdint.h>
typedef uint32_t uint32;

#define FINAL_MIX(h) \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d) \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

CAMLprim value caml_bs_hash_string (value obj){

  uint32 h = 0;
  h = caml_hash_mix_string(h,obj);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_int  ( value d){
  uint32 h = 0; 
  h = caml_hash_mix_intnat(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_string_and_int  (value obj, value d){
  uint32 h = 0; 
  h = caml_hash_mix_string(h,obj);
  h = caml_hash_mix_intnat(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_string_and_small_int(value obj, value d){
  uint32 h = 0;
  h = caml_hash_mix_string(h,obj);
  MIX(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_bs_hash_small_int(value d){
  uint32 h = 0; 
  // intnat stamp = Long_val(d); 
  // FIXME: unused value
  MIX(h,d);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_int_array_blit(
  value a1, value ofs1, 
  value a2, value ofs2,
  value n)
  {
     memmove(&Field(a2, Long_val(ofs2)),
            &Field(a1, Long_val(ofs1)),
            Long_val(n) * sizeof(value));
    return Val_unit;
  }
/*
 * http://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
 * https://en.wikipedia.org/wiki/MurmurHash
 * http://zimbry.blogspot.it/2011/09/better-bit-mixing-improving-on.html
 * http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx
 * We gave up the idea to  hash Ident.t (take only one argument)
 * customized hash function for Ident.t, first 
 * argument is stamp, second argument is string 
 * It's not just introducing c stubs, we need make a clear line
 * which part of our libraries depends on Ident.t
 */
CAMLprim value caml_bs_hash_stamp_and_name(value d, value obj ){
  uint32 h = 0;
  intnat stamp = Long_val(d); 
  if (stamp){
    MIX(h,d);
  } else {
    h = caml_hash_mix_string(h,obj);
  }
  
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}





// https://github.com/ocaml/ocaml/pull/255/files
#define Val_long_clang(x)     ((intnat) (((uintnat)(x) << 1)) + 1)

CAMLprim value caml_string_length_based_compare(value s1, value s2)
{
  mlsize_t len1, len2;
  mlsize_t temp;
  int res;
  if (s1 == s2) return Val_int(0);
  
  len1 = Wosize_val(s1);
  temp = Bsize_wsize(len1) - 1 ;
  len1 = temp - Byte(s1,temp);

  len2 = Wosize_val(s2);
  temp = Bsize_wsize(len2) - 1 ; 
  len2 = temp - Byte(s2,temp);

  if (len1 != len2) 
  { 
    if (len1 < len2 ) {
      return Val_long_clang(-1);
    } else {
      return Val_long_clang(1);
    }
  }
  else {
    
    res = memcmp(String_val(s1), String_val(s2), len1);
    if(res < 0) return Val_long_clang(-1); 
    if(res > 0) return Val_long_clang(1);
    return Val_long_clang(0);
    
  }
}





/* local variables: */
/* compile-command: "ocamlopt.opt -c ext_basic_hash_stubs.c" */
/* end: */


