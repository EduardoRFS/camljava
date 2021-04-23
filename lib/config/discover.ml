open Configurator.V1

let ( / ) a b = a ^ Filename.dir_sep ^ b

type os = Linux | Mac | Windows

module OS = struct
  let detect_system_header =
    {|
      #if __APPLE__
        #define PLATFORM_NAME "mac"
      #elif __linux__
        #define PLATFORM_NAME "linux"
      #elif WIN32
        #define PLATFORM_NAME "windows"
      #endif
    |}

  let get_os t =
    let header =
      let file = Filename.temp_file "discover" "os.h" in
      let fd = open_out file in
      output_string fd detect_system_header;
      close_out fd;
      file
    in
    let platform =
      C_define.import t ~includes:[ header ] [ ("PLATFORM_NAME", String) ]
    in
    match platform with
    | [ (_, String "linux") ] -> Linux
    | [ (_, String "mac") ] -> Mac
    | [ (_, String "windows") ] -> Windows
    | _ -> failwith "Unknown operating system"
end

module Jdk_home = struct
  let rec find_map f = function
    | [] -> None
    | x :: l -> (
        match f x with
        | Some _ as result -> result
        | None -> find_map f l)

  let find_java_home t =
    (* find JAVA_HOME using the java command *)
    let Process.{ stderr; _ } =
      Process.run t "java" [ "-XshowSettings:properties"; "-version" ]
    in
    stderr
    |> String.split_on_char '\n'
    |> find_map (fun line ->
           match String.split_on_char '=' line with
           | [ key; value ] when String.trim key = "java.home" ->
               Some (String.trim value)
           | _ -> None)

  let ends_with ~pat str =
    let pat_length = String.length pat in
    let length = String.length str in
    pat = String.sub str (length - pat_length) pat_length

  let get_jdk_home t =
    match Sys.getenv_opt "JDK_HOME" with
    | Some jdk_home -> jdk_home
    | None -> (
        match find_java_home t with
        | Some java_home ->
            let jre_suffix = Filename.dir_sep ^ "jre" in
            if Filename.check_suffix java_home jre_suffix then
              Filename.chop_suffix java_home jre_suffix
            else java_home
        | None -> failwith "JDK_HOME not found")
end

let jni_includes os jdk_home =
  let os =
    match os with
    | Linux -> "linux"
    | Mac -> "darwin"
    | Windows -> "win32"
  in
  [ "-I" ^ (jdk_home / "include"); "-I" ^ (jdk_home / "include" / os) ]

let jni_lib_opts jdk_home =
  let mk_flags lib =
    [
      "-L" ^ lib;
      "-L" ^ (lib / "server");
      "-Wl,-rpath," ^ lib;
      "-Wl,-rpath," ^ (lib / "server");
      "-Wl,-rpath," ^ (lib / "native_threads");
    ]
  in
  let arch = Config.architecture in
  let lib = jdk_home / "jre" / "lib" in
  (* some distros like archlinux uses also lib/amd64 *)
  mk_flags lib @ mk_flags (lib / arch)

let c_flags = jni_includes

let library_flags jdk_home =
  let ccopt flag = [ "-ccopt"; flag ] in
  let jni_lib_opts = List.map ccopt (jni_lib_opts jdk_home) |> List.flatten in
  jni_lib_opts @ [ "-cclib"; "-ljvm" ]

let generate_flags t =
  let os = OS.get_os t in
  let jdk_home = Jdk_home.get_jdk_home t in
  Flags.write_sexp "c_flags.sexp" (c_flags os jdk_home);
  Flags.write_sexp "library_flags.sexp" (library_flags jdk_home)

let () = Configurator.V1.main ~name:"camljava_discover" generate_flags
