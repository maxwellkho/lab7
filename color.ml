(*
                              CS51 Lab 7
                          Modules & Functors

                 A module for colors and color names

The representation for colors in this implementation is really obscure
and arguably unnecessarily so. By the way, it also has some bugs so it
doesn't pass all the unit tests. No need to debug it though. You'll be
replacing it wholesale with a simpler implementation. *)

(* 8-bit RGB channel colors *)
type color = int ;;

(* Some standard color names *)
type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet ;;

(* to_color r g b -- Returns the color corresponding to the RGB
   values given by r, g, and b *)
let to_color (r : int) (g : int) (b : int) : color =
  r * (1000000) + g * (1000) + b ;;

(* red c -- Returns the red channel value for the color c *)
let red (c : color) : int =
  c mod 1000000  ;;

(* green c -- Returns the green channel value for the color c *)
let green (c : color) : int =
  (c - red c) mod 1000 ;;

(* blue c -- Returns the blue channel value for the color c *)
let blue (c : color) : int =
   c - red c - green c ;;

(* color_named name -- Returns the color (as RGB representation)
   corresponding to the color name *)
let color_named (name : color_name) : color =
  match name with
  | Red ->    255000000
  | Green ->  255000
  | Blue ->   255
  | Orange -> 255165000
  | Yellow -> 255255000
  | Indigo -> 75000130
  | Violet -> 240130240 ;;