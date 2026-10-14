(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let u8 s at = Char.code s.[at]
let u16 s at = u8 s at lor (u8 s (at + 1) lsl 8)
let u32 s at = u16 s at lor (u16 s (at + 2) lsl 16)
let u64 s at = u32 s at lor (u32 s (at + 4) lsl 32)
let ehdr_size = 64
let shdr_size = 64
let sym_size = 24

type section = {
  name_at : int;
  kind : int;
  flags : int;
  offset : int;
  size : int;
  link : int;
  info : int;
  align : int;
  entsize : int;
}

let section obj at =
  {
    name_at = u32 obj at;
    kind = u32 obj (at + 4);
    flags = u64 obj (at + 8);
    offset = u64 obj (at + 24);
    size = u64 obj (at + 32);
    link = u32 obj (at + 40);
    info = u32 obj (at + 44);
    align = u64 obj (at + 48);
    entsize = u64 obj (at + 56);
  }

let sections obj =
  let shoff = u64 obj 40 in
  let shnum = u16 obj 60 in
  List.init shnum (fun i -> section obj (shoff + (i * shdr_size)))

let data obj s = String.sub obj s.offset s.size

let name_in table at =
  match String.index_from_opt table at '\000' with
  | Some stop -> String.sub table at (stop - at)
  | None -> String.sub table at (String.length table - at)

let named obj =
  let all = sections obj in
  let names = data obj (List.nth all (u16 obj 62)) in
  List.map (fun s -> (name_in names s.name_at, s)) all

let find obj name = List.assoc name (named obj)

let symbols obj =
  let symtab = find obj ".symtab" in
  let names = data obj (find obj ".strtab") in
  let entries = data obj symtab in
  List.init (symtab.size / sym_size) (fun i ->
      let at = i * sym_size in
      ( name_in names (u32 entries at),
        u8 entries (at + 4),
        u16 entries (at + 6),
        u64 entries (at + 8) ))

let dump_header obj =
  Printf.printf "magic %02x %s class %d data %d version %d abi %d\n" (u8 obj 0)
    (String.sub obj 1 3) (u8 obj 4) (u8 obj 5) (u8 obj 6) (u8 obj 7);
  Printf.printf "type %d machine %d version %d\n" (u16 obj 16) (u16 obj 18)
    (u32 obj 20);
  Printf.printf "entry %d phoff %d phnum %d phentsize %d\n" (u64 obj 24)
    (u64 obj 32) (u16 obj 56) (u16 obj 54);
  Printf.printf "ehsize %d shentsize %d shnum %d shstrndx %d\n" (u16 obj 52)
    (u16 obj 58) (u16 obj 60) (u16 obj 62)

let dump_sections obj =
  let show (name, s) =
    Printf.printf
      "%-10s kind %d flags %d size %d link %d info %d align %d entsize %d\n"
      (if String.is_empty name then "<null>" else name)
      s.kind s.flags s.size s.link s.info s.align s.entsize
  in
  List.iter show (named obj)

let dump_symbols obj =
  let show (name, info, shndx, value) =
    Printf.printf "%-8s info %d shndx %d value %d\n"
      (if String.is_empty name then "<null>" else name)
      info shndx value
  in
  List.iter show (symbols obj)

let%expect_test "elf64: the header describes a relocatable x86-64 object" =
  dump_header (Elf64.object_file ~text:"\x90" ~globals:[ ("main", 0) ]);
  [%expect
    {|
    magic 7f ELF class 2 data 1 version 1 abi 0
    type 1 machine 62 version 1
    entry 0 phoff 0 phnum 0 phentsize 0
    ehsize 64 shentsize 64 shnum 5 shstrndx 4
    |}]

let%expect_test "elf64: an object carries five sections" =
  dump_sections (Elf64.object_file ~text:"\x90\x90" ~globals:[ ("main", 0) ]);
  [%expect
    {|
    <null>     kind 0 flags 0 size 0 link 0 info 0 align 0 entsize 0
    .text      kind 1 flags 6 size 2 link 0 info 0 align 0 entsize 0
    .symtab    kind 2 flags 0 size 48 link 3 info 1 align 8 entsize 24
    .strtab    kind 3 flags 0 size 6 link 0 info 0 align 1 entsize 0
    .shstrtab  kind 3 flags 0 size 33 link 0 info 0 align 1 entsize 0
    |}]

let%expect_test "elf64: the null section header stays all zero" =
  let obj = Elf64.object_file ~text:"\x90" ~globals:[ ("main", 0) ] in
  let shoff = u64 obj 40 in
  let zero = String.for_all (fun c -> c = '\000') (String.sub obj shoff 64) in
  Printf.printf "%b\n" zero;
  [%expect {| true |}]

let%expect_test "elf64: the text lands where its header says" =
  let obj = Elf64.object_file ~text:"\x48\x31\xc0\xc3" ~globals:[ ("f", 0) ] in
  let text = find obj ".text" in
  print_endline (Fake.hex (data obj text));
  [%expect {| 48 31 c0 c3 |}]

let%expect_test "elf64: a blank symbol leads the table" =
  dump_symbols (Elf64.object_file ~text:"\x90" ~globals:[]);
  [%expect {| <null>   info 0 shndx 0 value 0 |}]

let%expect_test "elf64: every global becomes a func symbol in the text" =
  dump_symbols
    (Elf64.object_file ~text:"\x90\x90\x90\x90"
       ~globals:[ ("main", 0); ("helper", 2); ("last", 3) ]);
  [%expect
    {|
    <null>   info 0 shndx 0 value 0
    main     info 18 shndx 1 value 0
    helper   info 18 shndx 1 value 2
    last     info 18 shndx 1 value 3
    |}]

let%expect_test "elf64: the symtab points at the string table it uses" =
  let obj = Elf64.object_file ~text:"\x90" ~globals:[ ("main", 0) ] in
  let symtab = find obj ".symtab" in
  let all = named obj in
  let index name = Option.get (List.find_index (fun (n, _) -> n = name) all) in
  Printf.printf "link %d strtab %d locals %d\n" symtab.link (index ".strtab")
    symtab.info;
  [%expect {| link 3 strtab 3 locals 1 |}]

let%expect_test "elf64: a repeated name is pooled once" =
  let obj =
    Elf64.object_file ~text:"\x90" ~globals:[ ("same", 0); ("same", 1) ]
  in
  let names = data obj (find obj ".strtab") in
  Printf.printf "%S\n" names;
  dump_symbols obj;
  [%expect
    {|
    "\000same\000"
    <null>   info 0 shndx 0 value 0
    same     info 18 shndx 1 value 0
    same     info 18 shndx 1 value 1
    |}]

let%expect_test "elf64: the symbol table stays eight byte aligned" =
  let show n =
    let obj = Elf64.object_file ~text:(String.make n '\x90') ~globals:[] in
    let symtab = find obj ".symtab" in
    Printf.printf "text %d symtab at %d aligned %b\n" n symtab.offset
      (symtab.offset mod 8 = 0)
  in
  List.iter show [ 0; 1; 7; 8; 9 ];
  [%expect
    {|
    text 0 symtab at 64 aligned true
    text 1 symtab at 72 aligned true
    text 7 symtab at 72 aligned true
    text 8 symtab at 72 aligned true
    text 9 symtab at 80 aligned true
    |}]

let%expect_test "elf64: an empty text still makes a valid object" =
  let obj = Elf64.object_file ~text:"" ~globals:[] in
  dump_sections obj;
  dump_symbols obj;
  [%expect
    {|
    <null>     kind 0 flags 0 size 0 link 0 info 0 align 0 entsize 0
    .text      kind 1 flags 6 size 0 link 0 info 0 align 0 entsize 0
    .symtab    kind 2 flags 0 size 24 link 3 info 1 align 8 entsize 24
    .strtab    kind 3 flags 0 size 1 link 0 info 0 align 1 entsize 0
    .shstrtab  kind 3 flags 0 size 33 link 0 info 0 align 1 entsize 0
    <null>   info 0 shndx 0 value 0
    |}]

let%expect_test "elf64: the section headers sit past every section body" =
  let obj =
    Elf64.object_file ~text:"\x90\x90\x90" ~globals:[ ("a", 0); ("b", 1) ]
  in
  let shoff = u64 obj 40 in
  let ends (_, s) = if s.kind = 0 then ehdr_size else s.offset + s.size in
  let last = List.fold_left (fun m s -> max m (ends s)) 0 (named obj) in
  Printf.printf "shoff %d past every body %b size %d\n" shoff (shoff >= last)
    (String.length obj);
  [%expect {| shoff 182 past every body true size 502 |}]
