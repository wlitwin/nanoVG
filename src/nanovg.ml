open Ctypes
open Unsigned

include Nanovg_bindings.C(Nanovg_generated)

module Create_flags = struct
  let antialias       = 1 lsl 0
  let stencil_strokes = 1 lsl 1
  let debug           = 1 lsl 2
end

module Winding = struct
    let ccw = 1
    let cw = 2
end

module Solidity = struct
    let solid = 1
    let hole = 2
end

module LineCap = struct
    let butt = 0
    let round = 1
    let square = 2
    let bevel = 3
    let miter = 4
end

module Align = struct
    let left = 1 lsl 0
    let center = 1 lsl 1
    let right = 1 lsl 2

    let top = 1 lsl 3
    let middle = 1 lsl 4
    let bottom = 1 lsl 5
    let baseline = 1 lsl 6
end

module CompositeOperation = struct
    let source_over = 0
    let source_in  = 1
    let source_out = 2
    let atop = 3
    let destination_over = 4
    let destination_in = 5
    let destination_out = 6
    let destination_atop = 7
    let lighter = 8
    let copy = 9
    let xor = 10
end

exception Memory_error

let rgb r g b =
  rgb (UChar.of_int r) (UChar.of_int g) (UChar.of_int b)

let rgba r g b a =
  rgba (UChar.of_int r) (UChar.of_int g) (UChar.of_int b) (UChar.of_int a)

let trans_rgba c a =
  trans_rgba c (UChar.of_int a)

let hsla h s l a =
  hsla h s l (UChar.of_int a)

let create_gles2 flags =
  match create_gles2 flags with
  | Some x -> x
  | None -> raise Memory_error

let null_char : char ptr = Ctypes.(from_voidp char null)
let null_float : float ptr = Ctypes.(from_voidp float null)

exception Out_of_bounds
let char_ptr_from_string str index =
    if index < 0 || index >= String.length str then (
        raise Out_of_bounds
    );
    let ptr = Ctypes.ocaml_string_start str in
    let ptr_cast : char ptr = Obj.magic ptr in
    Ctypes.(ptr_cast +@ index)
;;

