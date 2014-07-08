 (* {{{ LICENSE                                                              *
  * vi: set fdm=marker fdl=0:                                                *
  *                                                                          *
  * Copyright (c) 2012 Raphaël Proust <raphlalou@gmail.com>                  *
  * Copyright (c) 2012 INRIA - Raphaël Proust <raphlalou@gmail.com>          *
  * Copyright (c) 2012 ENS - Raphaël Proust <raphlalou@gmail.com>            *
  *                                                                          *
  * Permission to use, copy, modify, and distribute this software for any    *
  * purpose with or without fee is hereby granted, provided that the above   *
  * copyright notice and this permission notice appear in all copies.        *
  *                                                                          *
  * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *
  * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *
  * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *
  * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *
  * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *
  * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *
  * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *
  * }}}                                                                      *)

module E = struct

  type ('a, 'b) either = Left of 'a | Right of 'b
  let left l = Left l
  let right r = Right r

end

module I = struct

  let fold_inc f acc i =
    let rec aux acc j =
      if j >= i then
        acc
      else
        aux (f acc j) (succ j)
    in
    aux acc 0

end

module L = struct

  let concat_map f l = List.concat (List.map f l)

  let exists_one predicate l =
    let rec aux flag = function
      | [] -> flag
      | h::t ->
        let f = predicate h in
        (not (flag && f)) || (aux (flag || f) t)
    in
    aux false l

  let unique extract l =
    let hshtbl = Hashtbl.create 32 in
    let rec aux = function
      | [] -> true
      | h::t ->
        let x = extract h in
        match x with
        | Some x -> begin
          let h = Hashtbl.hash x in
          try
            if Hashtbl.find hshtbl h = x then
              false
            else
              raise Not_found (*hackish way to DRY*)
          with
            | Not_found ->
              Hashtbl.add hshtbl h x;
              aux t
          end
        | None -> aux t
    in
    aux l

  let pick_one_such_as f l =
    let rec aux accu = function
      | [] -> raise Not_found
      | h::t ->
        if f h then
          (h, List.rev_append accu t)
        else
          aux (h::accu) t
    in
    aux [] l

  let rec map_option f l = match l with
    | [] -> []
    | h::t -> match f h with
      | None -> map_option f t
      | Some h -> h :: map_option f t

  let inter l1 l2 = (* quite inefficient, prefer *real* sets *)
    List.filter (fun x -> List.mem x l2) l1

  let minus l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1

  let includes l1 l2 =
    List.for_all (fun x -> List.mem x l1) l2

  let disjoint l1 l2 =
    List.for_all (fun x -> not (List.mem x l1)) l2

  let cat_uniq l1 l2 =
    List.iter (fun x -> assert (not (List.mem x l2))) l1;
    l1 @ l2

  let rec take l n =
    if n <= 0 then
      []
    else
      List.hd l :: (take (List.tl l) (pred n))

  let n f i = I.fold_inc (fun acc i -> f i :: acc) [] i

  let nconst v i = n (fun _ -> v) i

  let classes l =
    let l = List.sort Pervasives.compare l in
    List.fold_left
      (fun acc (c, v) -> match acc with
        | [] -> [c, [v]]
        | (cc, cv) :: cs when c = cc -> (cc, v::cv) :: cs
        | cs -> (c, [v]) :: cs
      )
      []
      l

end

module T2 = struct

  let map1 f (l, r) = (f l, f r)

end

module O = struct

  let opt x = Some x

  let none = None

  let unopt_soft f = function
    | None -> f ()
    | Some v -> v

  let unopt d = function
    | None -> d
    | Some v -> v

  let unopt_hard = function
    | None -> assert false
    | Some v -> v


end

module P = struct

  let pos_char pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  let pos_line pos = pos.Lexing.pos_lnum

  let print_pos ch pos =
    Printf.fprintf ch "line %d, character %d" (pos_line pos) (pos_char pos)

  exception Lex_error_unterminated_string of Lexing.position

end

module PP = struct

  open Pprint.Operators

  let with_paren d = Pprint.lparen ^^ d ^^ Pprint.rparen
  let with_paren_br d = with_paren (d ^^ Pprint.break1)
  let comma_space = Pprint.comma ^^ Pprint.space
  let list ?(empty=Pprint.empty) ?(sep=Pprint.break1) pp = function
    | [] -> empty
    | l  ->  Pprint.sepmap sep pp l
  let either pl pr = function
    | E.Left l  -> pl l
    | E.Right r -> pr r
  let level d = Pprint.nest 2 (d)
  let unit = !^ "()"
  let op pp_v v1 op v2 = pp_v v1 ^^Pprint.space^^ op ^^Pprint.space^^ pp_v v2
  let fn pp_v fn vs =
    fn ^^ Pprint.space ^^ with_paren (list ~sep:comma_space pp_v vs)

end
