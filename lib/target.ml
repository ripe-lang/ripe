(* SPDX-License-Identifier: GPL-2.0-only *)

type t = Linux_x86_64

let host () : t =
  let input = Unix.open_process_args_in "uname" [| "uname"; "-sm" |] in
  let value = In_channel.input_all input |> String.trim in
  match Unix.close_process_in input with
  | Unix.WEXITED 0 when value = "Linux x86_64" -> Linux_x86_64
  | _ -> failwith "no toolchain for this host"

let command_env (_target : t) : string =
  match Config.lld_library_path with
  | Some path ->
      let paths =
        match Sys.getenv_opt "LD_LIBRARY_PATH" with
        | Some existing when existing <> "" -> path ^ ":" ^ existing
        | _ -> path
      in
      "LD_LIBRARY_PATH=" ^ Filename.quote paths ^ " "
  | None -> ""

let assembler_args (_target : t) ~output ~input : string list =
  [ Config.assembler; "--64"; "-o"; output; input ]

let find_file_in_directory directory name =
  let path = Filename.concat directory name in
  if Sys.file_exists path then Some path else None

let rec find_file_under_directory directory depth name =
  match find_file_in_directory directory name with
  | Some path -> Some path
  | None when depth = 0 -> None
  | None -> (
      try
        Sys.readdir directory |> Array.to_list
        |> List.find_map (fun entry ->
            let path = Filename.concat directory entry in
            if Sys.file_exists path && Sys.is_directory path then
              find_file_under_directory path (depth - 1) name
            else None)
      with Sys_error _ -> None)

let required_file = function
  | Some path -> path
  | None -> failwith "cannot find a Linux x86 64 startup file"

let dynamic_linker () =
  match Sys.getenv_opt "RIPE_DYNAMIC_LINKER" with
  | Some path when String.trim path <> "" -> path
  | _ ->
      [
        "/lib64/ld-linux-x86-64.so.2";
        "/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2";
      ]
      |> List.find_opt Sys.file_exists
      |> Option.value ~default:"/lib64/ld-linux-x86-64.so.2"

let linker_args (_target : t) ~output ~object_file ~runtime ~libraries :
    string list =
  let startup_dirs =
    [
      "/usr/lib64";
      "/lib64";
      "/usr/lib/x86_64-linux-gnu";
      "/lib/x86_64-linux-gnu";
    ]
  in
  let gcc_dirs = [ "/usr/lib/gcc"; "/usr/lib64/gcc" ] in
  let crt1 =
    required_file
      (List.find_map
         (fun dir -> find_file_in_directory dir "crt1.o")
         startup_dirs)
  in
  let startup_dir = Filename.dirname crt1 in
  let crti = required_file (find_file_in_directory startup_dir "crti.o") in
  let crtn = required_file (find_file_in_directory startup_dir "crtn.o") in
  let crtbegin =
    required_file
      (List.find_map
         (fun dir -> find_file_under_directory dir 2 "crtbegin.o")
         gcc_dirs)
  in
  let gcc_dir = Filename.dirname crtbegin in
  let crtend = required_file (find_file_in_directory gcc_dir "crtend.o") in
  [
    Config.lld;
    "--hash-style=gnu";
    "--build-id";
    "--eh-frame-hdr";
    "-m";
    "elf_x86_64";
    "-dynamic-linker";
    dynamic_linker ();
    "-o";
    output;
    crt1;
    crti;
    crtbegin;
    "-L";
    gcc_dir;
    "-L";
    startup_dir;
    "-L/usr/lib64";
    "-L/lib64";
    "-L/usr/lib";
    "-L/lib";
    object_file;
    "-lgcc";
    "--as-needed";
    "-lgcc_s";
    "--no-as-needed";
    "-lc";
    "-lgcc";
    "--as-needed";
    "-lgcc_s";
    "--no-as-needed";
    crtend;
    crtn;
    runtime;
  ]
  @ List.map (fun library -> "-l" ^ library) libraries
