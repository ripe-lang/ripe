(* SPDX-License-Identifier: Apache-2.0 *)

(* In the future it would be interesting to try Mach O or PE/COFF *)

(* ELF layout and machine bytes follow the gABI and psABI *)

let elf_header_size = 64
let section_header_size = 64
let sym_entry_size = 24
let et_rel = 1
let em_x86_64 = 62
let sht_progbits = 1
let sht_symtab = 2
let sht_strtab = 3
let shf_alloc = 2
let shf_execinstr = 4
let stb_global = 1
let stt_func = 2

(* Section indexes match the order emitted below *)
let text_index = 1
let strtab_index = 3
let shstrtab_index = 4
let u8 buffer value = Buffer.add_char buffer (Char.chr (value land 0xff))

let u16 buffer value =
  u8 buffer value;
  u8 buffer (value lsr 8)

let u32 buffer value =
  u16 buffer value;
  u16 buffer (value lsr 16)

let u64 buffer value =
  u32 buffer value;
  u32 buffer (value lsr 32)

(* An overrun means the computed layout is wrong before linking *)
let pad_to buffer offset =
  if Buffer.length buffer > offset then
    Diagnostic.ice
      (Printf.sprintf "already %d bytes past offset %d"
         (Buffer.length buffer - offset)
         offset);
  while Buffer.length buffer < offset do
    u8 buffer 0
  done

(* ELF names use offsets into one shared string table *)
module Strtab = struct
  type t = { buffer : Buffer.t; offsets : (string, int) Hashtbl.t }

  let create () =
    let buffer = Buffer.create 32 in
    (* The empty name is the required zero offset entry *)
    Buffer.add_char buffer '\000';
    { buffer; offsets = Hashtbl.create 8 }

  let add table name =
    if String.is_empty name then 0
    else
      match Hashtbl.find_opt table.offsets name with
      | Some at -> at
      | None ->
          let at = Buffer.length table.buffer in
          Hashtbl.add table.offsets name at;
          Buffer.add_string table.buffer name;
          Buffer.add_char table.buffer '\000';
          at

  (* Lookup stays read only after the table has been copied out *)
  let find table name =
    if String.is_empty name then 0
    else
      match Hashtbl.find_opt table.offsets name with
      | Some at -> at
      | None -> Diagnostic.ice ("no pooled name for " ^ name)

  let contents table = Buffer.contents table.buffer
end

type section = {
  name : string;
  kind : int;
  flags : int;
  data : string;
  link : int;
  info : int;
  align : int;
  entsize : int;
}

let empty_section =
  {
    name = "";
    kind = 0;
    flags = 0;
    data = "";
    link = 0;
    info = 0;
    align = 0;
    entsize = 0;
  }

let ehdr buffer ~kind ~shoff ~shnum ~shstrndx =
  u8 buffer 0x7f;
  Buffer.add_string buffer "ELF";
  u8 buffer 2 (* ELFCLASS64 *);
  u8 buffer 1 (* ELFDATA2LSB *);
  u8 buffer 1 (* EV_CURRENT *);
  u8 buffer 0 (* ELFOSABI_SYSV *);
  for _ = 1 to 8 do
    u8 buffer 0
  done;
  u16 buffer kind;
  u16 buffer em_x86_64;
  u32 buffer 1 (* e_version *);
  u64 buffer 0 (* e_entry is unused in a relocatable object *);
  u64 buffer 0 (* e_phoff is unused without segments *);
  u64 buffer shoff;
  u32 buffer 0 (* e_flags *);
  u16 buffer elf_header_size;
  u16 buffer 0 (* e_phentsize *);
  u16 buffer 0 (* e_phnum *);
  u16 buffer section_header_size;
  u16 buffer shnum;
  u16 buffer shstrndx

let shdr buffer section ~name_at ~offset =
  u32 buffer name_at;
  u32 buffer section.kind;
  u64 buffer section.flags;
  u64 buffer 0 (* The linker assigns section addresses later *);
  (* The null section header must remain all zero *)
  u64 buffer (if section.kind = 0 then 0 else offset);
  u64 buffer (String.length section.data);
  u32 buffer section.link;
  u32 buffer section.info;
  u64 buffer section.align;
  u64 buffer section.entsize

let symbol buffer ~name_at ~info ~shndx ~value =
  u32 buffer name_at;
  u8 buffer info;
  u8 buffer 0 (* st_other is reserved and must be zero *);
  u16 buffer shndx;
  u64 buffer value;
  u64 buffer 0 (* st_size is zero because this emitter has no size *)

(* The header stores where local symbols end so return that count *)
let symbol_table strtab globals =
  let buffer = Buffer.create (sym_entry_size * (List.length globals + 1)) in
  symbol buffer ~name_at:0 ~info:0 ~shndx:0 ~value:0;

  (* The blank symbol is local because its info byte is zero *)
  let locals = 1 in

  let emit (name, at) =
    symbol buffer ~name_at:(Strtab.add strtab name)
      ~info:((stb_global lsl 4) lor stt_func)
      ~shndx:text_index ~value:at
  in
  List.iter emit globals;
  (Buffer.contents buffer, locals)

(* Section types and flags follow the special section table *)
let object_sections ~text ~symbols ~locals ~names ~section_names =
  [
    empty_section;
    {
      empty_section with
      name = ".text";
      kind = sht_progbits;
      flags = shf_alloc lor shf_execinstr;
      data = text;
    };
    {
      empty_section with
      name = ".symtab";
      kind = sht_symtab;
      data = symbols;
      link = strtab_index;
      info = locals;
      align = 8;
      entsize = sym_entry_size;
    };
    {
      empty_section with
      name = ".strtab";
      kind = sht_strtab;
      data = names;
      align = 1;
    };
    {
      empty_section with
      name = ".shstrtab";
      kind = sht_strtab;
      data = section_names;
      align = 1;
    };
  ]

(* Sections land before the header table here so headers can record each section's offset *)
let place sections =
  let step (placed, at) section =
    let at =
      (* Alignment zero or one needs no padding *)
      if section.align > 1 then Layout.align_to at section.align else at
    in
    ((section, at) :: placed, at + String.length section.data)
  in
  let placed, ending = List.fold_left step ([], elf_header_size) sections in
  (List.rev placed, ending)

let object_file ~text ~globals =
  let strtab = Strtab.create () in
  let symbols, locals = symbol_table strtab globals in

  (* The section name table needs one pass to collect names and one to emit them *)
  let shstrtab = Strtab.create () in
  let build section_names =
    object_sections ~text ~symbols ~locals ~names:(Strtab.contents strtab)
      ~section_names
  in
  List.iter
    (fun section -> ignore (Strtab.add shstrtab section.name))
    (build "");
  let sections = build (Strtab.contents shstrtab) in

  let placed, ending = place sections in

  let shoff = ending in

  let buffer =
    Buffer.create (shoff + (section_header_size * List.length sections))
  in
  ehdr buffer ~kind:et_rel ~shoff ~shnum:(List.length sections)
    ~shstrndx:shstrtab_index;
  List.iter
    (fun (section, at) ->
      pad_to buffer at;
      Buffer.add_string buffer section.data)
    placed;

  pad_to buffer shoff;
  List.iter
    (fun (section, at) ->
      shdr buffer section
        ~name_at:(Strtab.find shstrtab section.name)
        ~offset:at)
    placed;
  Buffer.contents buffer
