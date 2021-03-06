(***********************************************************************)
(*                                                                     *)
(*             OCamlJava: Objective Caml / Java interface              *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: jni.mlp,v 1.4 2005-10-21 08:19:11 xleroy Exp $ *)

(* Low-level Java interface (JNI level) *)

external set_debug: bool -> unit = "camljava_set_debug"

external set_string_auto_conv: bool -> unit = "camljava_set_strconv"

external init: string -> unit = "camljava_Init"
external shutdown: unit -> unit = "camljava_Shutdown"

type obj

external get_null: unit -> obj = "camljava_GetNull"

let null = get_null()

exception Null_pointer

let _ = Callback.register_exception "camljava_null_pointer" Null_pointer

exception Exception of obj

let _ = Callback.register "camljava_raise_exception"
          (fun obj -> raise (Exception obj))

external register_natives: unit -> unit = "camljava_RegisterNatives"

let _ = register_natives()

let _ = Callback.register "Oo.new_method" Oo.new_method

(* String operations *)

external string_to_java: string -> obj = "camljava_MakeJavaString"
external string_from_java: obj -> string = "camljava_ExtractJavaString"

let null_string = ""

let is_null_string s = s == null_string

external register_null_string: string -> unit = "camljava_RegisterNullString"

let _ = register_null_string null_string

(* Class operations *)

type clazz
        (* The type of class identifiers *)

external find_class: string -> clazz
        = "camljava_FindClass"
external get_superclass: clazz -> clazz
        = "camljava_GetSuperclass"
external is_assignable_from: clazz -> clazz -> bool
        = "camljava_IsAssignableFrom"

(* Field and method identifiers *)

type fieldID
type methodID

external get_fieldID: clazz -> string -> string -> fieldID
        = "camljava_GetFieldID"
external get_static_fieldID: clazz -> string -> string -> fieldID
        = "camljava_GetStaticFieldID"
external get_methodID: clazz -> string -> string -> methodID
        = "camljava_GetMethodID"
external get_static_methodID: clazz -> string -> string -> methodID
        = "camljava_GetStaticMethodID"

(* Field access *)

external get_object_field: obj -> fieldID -> obj
        = "camljava_GetObjectField"
external get_boolean_field: obj -> fieldID -> bool
        = "camljava_GetBooleanField"
external get_byte_field: obj -> fieldID -> int
        = "camljava_GetByteField"
external get_char_field: obj -> fieldID -> int
        = "camljava_GetCharField"
external get_short_field: obj -> fieldID -> int
        = "camljava_GetShortField"
external get_int_field: obj -> fieldID -> int32
        = "camljava_GetIntField"
external get_camlint_field: obj -> fieldID -> int
        = "camljava_GetCamlintField"
external get_long_field: obj -> fieldID -> int64
        = "camljava_GetLongField"
external get_float_field: obj -> fieldID -> float
        = "camljava_GetFloatField"
external get_double_field: obj -> fieldID -> float
        = "camljava_GetDoubleField"

external set_object_field: obj -> fieldID -> obj -> unit
        = "camljava_SetObjectField"
external set_boolean_field: obj -> fieldID -> bool -> unit
        = "camljava_SetBooleanField"
external set_byte_field: obj -> fieldID -> int -> unit
        = "camljava_SetByteField"
external set_char_field: obj -> fieldID -> int -> unit
        = "camljava_SetCharField"
external set_short_field: obj -> fieldID -> int -> unit
        = "camljava_SetShortField"
external set_int_field: obj -> fieldID -> int32 -> unit
        = "camljava_SetIntField"
external set_camlint_field: obj -> fieldID -> int -> unit
        = "camljava_SetCamlintField"
external set_long_field: obj -> fieldID -> int64 -> unit
        = "camljava_SetLongField"
external set_float_field: obj -> fieldID -> float -> unit
        = "camljava_SetFloatField"
external set_double_field: obj -> fieldID -> float -> unit
        = "camljava_SetDoubleField"

external get_static_object_field: clazz -> fieldID -> obj
        = "camljava_GetStaticObjectField"
external get_static_boolean_field: clazz -> fieldID -> bool
        = "camljava_GetStaticBooleanField"
external get_static_byte_field: clazz -> fieldID -> int
        = "camljava_GetStaticByteField"
external get_static_char_field: clazz -> fieldID -> int
        = "camljava_GetStaticCharField"
external get_static_short_field: clazz -> fieldID -> int
        = "camljava_GetStaticShortField"
external get_static_int_field: clazz -> fieldID -> int32
        = "camljava_GetStaticIntField"
external get_static_camlint_field: clazz -> fieldID -> int
        = "camljava_GetStaticCamlintField"
external get_static_long_field: clazz -> fieldID -> int64
        = "camljava_GetStaticLongField"
external get_static_float_field: clazz -> fieldID -> float
        = "camljava_GetStaticFloatField"
external get_static_double_field: clazz -> fieldID -> float
        = "camljava_GetStaticDoubleField"

external set_static_obj_field: clazz -> fieldID -> obj -> unit
        = "camljava_SetStaticObjectField"
external set_static_boolean_field: clazz -> fieldID -> bool -> unit
        = "camljava_SetStaticBooleanField"
external set_static_byte_field: clazz -> fieldID -> int -> unit
        = "camljava_SetStaticByteField"
external set_static_char_field: clazz -> fieldID -> int -> unit
        = "camljava_SetStaticCharField"
external set_static_short_field: clazz -> fieldID -> int -> unit
        = "camljava_SetStaticShortField"
external set_static_int_field: clazz -> fieldID -> int32 -> unit
        = "camljava_SetStaticIntField"
external set_static_camlint_field: clazz -> fieldID -> int -> unit
        = "camljava_SetStaticCamlintField"
external set_static_long_field: clazz -> fieldID -> int64 -> unit
        = "camljava_SetStaticLongField"
external set_static_float_field: clazz -> fieldID -> float -> unit
        = "camljava_SetStaticFloatField"
external set_static_double_field: clazz -> fieldID -> float -> unit
        = "camljava_SetStaticDoubleField"

type argument =
    Boolean of bool
  | Byte of int
  | Char of int
  | Short of int
  | Camlint of int
  | Int of int32
  | Long of int64
  | Float of float
  | Double of float
  | Obj of obj

external call_object_method: obj -> methodID -> argument array -> obj
        = "camljava_CallObjectMethod"
external call_boolean_method: obj -> methodID -> argument array -> bool
        = "camljava_CallBooleanMethod"
external call_byte_method: obj -> methodID -> argument array -> int
        = "camljava_CallByteMethod"
external call_char_method: obj -> methodID -> argument array -> int
        = "camljava_CallCharMethod"
external call_short_method: obj -> methodID -> argument array -> int
        = "camljava_CallShortMethod"
external call_int_method: obj -> methodID -> argument array -> int32
        = "camljava_CallIntMethod"
external call_camlint_method: obj -> methodID -> argument array -> int
        = "camljava_CallCamlintMethod"
external call_long_method: obj -> methodID -> argument array -> int64
        = "camljava_CallLongMethod"
external call_float_method: obj -> methodID -> argument array -> float
        = "camljava_CallFloatMethod"
external call_double_method: obj -> methodID -> argument array -> float
        = "camljava_CallDoubleMethod"
external call_void_method: obj -> methodID -> argument array -> unit
        = "camljava_CallVoidMethod"

external call_static_object_method:
                 clazz -> methodID -> argument array -> obj
        = "camljava_CallStaticObjectMethod"
external call_static_boolean_method:
                 clazz -> methodID -> argument array -> bool
        = "camljava_CallStaticBooleanMethod"
external call_static_byte_method:
                 clazz -> methodID -> argument array -> int
        = "camljava_CallStaticByteMethod"
external call_static_char_method:
                 clazz -> methodID -> argument array -> int
        = "camljava_CallStaticCharMethod"
external call_static_short_method:
                 clazz -> methodID -> argument array -> int
        = "camljava_CallStaticShortMethod"
external call_static_int_method:
                 clazz -> methodID -> argument array -> int32
        = "camljava_CallStaticIntMethod"
external call_static_camlint_method:
                 clazz -> methodID -> argument array -> int
        = "camljava_CallStaticCamlintMethod"
external call_static_long_method:
                 clazz -> methodID -> argument array -> int64
        = "camljava_CallStaticLongMethod"
external call_static_float_method:
                 clazz -> methodID -> argument array -> float
        = "camljava_CallStaticFloatMethod"
external call_static_double_method:
                 clazz -> methodID -> argument array -> float
        = "camljava_CallStaticDoubleMethod"
external call_static_void_method:
                 clazz -> methodID -> argument array -> unit
        = "camljava_CallStaticVoidMethod"

external call_nonvirtual_object_method:
                 obj -> clazz -> methodID -> argument array -> obj
        = "camljava_CallNonvirtualObjectMethod"
external call_nonvirtual_boolean_method:
                 obj -> clazz -> methodID -> argument array -> bool
        = "camljava_CallNonvirtualBooleanMethod"
external call_nonvirtual_byte_method: 
                 obj -> clazz -> methodID -> argument array -> int
        = "camljava_CallNonvirtualByteMethod"
external call_nonvirtual_char_method:
                 obj -> clazz -> methodID -> argument array -> int
        = "camljava_CallNonvirtualCharMethod"
external call_nonvirtual_short_method:
                 obj -> clazz -> methodID -> argument array -> int
        = "camljava_CallNonvirtualShortMethod"
external call_nonvirtual_int_method:
                 obj -> clazz -> methodID -> argument array -> int32
        = "camljava_CallNonvirtualIntMethod"
external call_nonvirtual_camlint_method:
                 obj -> clazz -> methodID -> argument array -> int
        = "camljava_CallNonvirtualCamlintMethod"
external call_nonvirtual_long_method:
                 obj -> clazz -> methodID -> argument array -> int64
        = "camljava_CallNonvirtualLongMethod"
external call_nonvirtual_float_method:
                 obj -> clazz -> methodID -> argument array -> float
        = "camljava_CallNonvirtualFloatMethod"
external call_nonvirtual_double_method:
                 obj -> clazz -> methodID -> argument array -> float
        = "camljava_CallNonvirtualDoubleMethod"
external call_nonvirtual_void_method:
                 obj -> clazz -> methodID -> argument array -> unit
        = "camljava_CallNonvirtualVoidMethod"

(* Arrays *)

external get_array_length: obj -> int = "camljava_GetArrayLength"

external new_object_array: int -> clazz -> obj
        = "camljava_NewObjectArray"
external get_object_array_element: obj -> int -> obj
        = "camljava_GetObjectArrayElement"
external set_object_array_element: obj -> int -> obj -> unit
        = "camljava_SetObjectArrayElement"
external new_boolean_array: int -> obj
        = "camljava_NewBooleanArray"
external get_boolean_array_element: obj -> int -> bool
        = "camljava_GetBooleanArrayElement"
external set_boolean_array_element: obj -> int -> bool -> unit
        = "camljava_SetBooleanArrayElement"
external new_byte_array: int -> obj
        = "camljava_NewByteArray"
external get_byte_array_element: obj -> int -> int
        = "camljava_GetByteArrayElement"
external set_byte_array_element: obj -> int -> int -> unit
        = "camljava_SetByteArrayElement"
external get_byte_array_region: obj -> int -> string -> int -> int -> unit
        = "camljava_GetByteArrayRegion"
external set_byte_array_region: string -> int -> obj -> int -> int -> unit
        = "camljava_SetByteArrayRegion"
external new_char_array: int -> obj
        = "camljava_NewCharArray"
external get_char_array_element: obj -> int -> int
        = "camljava_GetCharArrayElement"
external set_char_array_element: obj -> int -> int -> unit
        = "camljava_SetCharArrayElement"
external new_short_array: int -> obj
        = "camljava_NewShortArray"
external get_short_array_element: obj -> int -> int
        = "camljava_GetShortArrayElement"
external set_short_array_element: obj -> int -> int -> unit
        = "camljava_SetShortArrayElement"
external new_int_array: int -> obj
        = "camljava_NewIntArray"
external get_int_array_element: obj -> int -> int32
        = "camljava_GetIntArrayElement"
external set_int_array_element: obj -> int -> int32 -> unit
        = "camljava_SetIntArrayElement"
external get_camlint_array_element: obj -> int -> int
        = "camljava_GetCamlintArrayElement"
external set_camlint_array_element: obj -> int -> int -> unit
        = "camljava_SetCamlintArrayElement"
external new_long_array: int -> obj
        = "camljava_NewLongArray"
external get_long_array_element: obj -> int -> int64
        = "camljava_GetLongArrayElement"
external set_long_array_element: obj -> int -> int64 -> unit
        = "camljava_SetLongArrayElement"
external new_float_array: int -> obj
        = "camljava_NewFloatArray"
external get_float_array_element: obj -> int -> float
        = "camljava_GetFloatArrayElement"
external set_float_array_element: obj -> int -> float -> unit
        = "camljava_SetFloatArrayElement"
external new_double_array: int -> obj
        = "camljava_NewDoubleArray"
external get_double_array_element: obj -> int -> float
        = "camljava_GetDoubleArrayElement"
external set_double_array_element: obj -> int -> float -> unit
        = "camljava_SetDoubleArrayElement"

(* Object operations *)

external is_null: obj -> bool = "camljava_IsNull"
external alloc_object: clazz -> obj = "camljava_AllocObject"
external get_object_class: obj -> clazz = "camljava_GetObjectClass"
external is_instance_of: obj -> clazz -> bool = "camljava_IsInstanceOf"
external is_same_object: obj -> obj -> bool = "camljava_IsSameObject"

(* Auxiliaries for Java->OCaml callbacks *)

external wrap_caml_object : < .. > -> int64 = "camljava_WrapCamlObject"

let callback_class =
  find_class "fr/inria/caml/camljava/Callback"
let callback_init =
  get_methodID callback_class "<init>" "(J)V"
let wrap_object camlobj =
  let javaobj = alloc_object callback_class in
  call_nonvirtual_void_method javaobj callback_class callback_init
                              [|Long (wrap_caml_object camlobj)|];
  javaobj

let callback_to_java: (obj list -> obj) -> obj =
  fun f -> wrap_object (object method closure objs = f objs end)

let callback_void_to_java: (obj list -> unit) -> obj =
  fun f -> wrap_object (object method closure objs = f objs end)

let start_vm libpath =
  let sep =
    match Sys.os_type with
      "Unix" -> ":"
    | "Win32" -> ";"
    | _ -> assert false in
  let path =
    try
      Sys.getenv "CLASSPATH" ^ sep ^ libpath
    with Not_found ->
      libpath in
  init path;
  at_exit shutdown