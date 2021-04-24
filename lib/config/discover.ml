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

  let find_jdk_home t =
    match Sys.getenv_opt "JDK_HOME" with
    | Some jdk_home -> Some jdk_home
    | None -> (
        match find_java_home t with
        | Some java_home ->
            (* up to java 8, all versions had /jre for the JAVA_HOME*)
            let jre_suffix = Filename.dir_sep ^ "jre" in
            if Filename.check_suffix java_home jre_suffix then
              Some (Filename.chop_suffix java_home jre_suffix)
            else Some java_home
        | None -> None)

  let get_jdk_home t =
    match find_jdk_home t with
    | Some jdk_home -> jdk_home
    | None -> failwith "JDK_HOME not found"
end

module Jdk_version = struct
  let find_jdk_version t =
    let Process.{ stdout; stderr; _ } = Process.run t "javac" [ "-version" ] in
    let stdout_and_stderr = String.trim stdout ^ String.trim stderr in
    match String.split_on_char ' ' stdout_and_stderr with
    | [ "javac"; version ] -> (
        match String.split_on_char '.' version with
        | "1" :: n :: _ | n :: _ -> int_of_string_opt n
        | _ -> None)
    | _ -> None

  let get_jdk_version t =
    match find_jdk_version t with
    | Some version -> version
    | None -> failwith "JDK version not found"
end

let jni_includes os jdk_home =
  let os =
    match os with
    | Linux -> "linux"
    | Mac -> "darwin"
    | Windows -> "win32"
  in
  [ "-I" ^ (jdk_home / "include"); "-I" ^ (jdk_home / "include" / os) ]

let ccopt flag = [ "-ccopt"; flag ]

let cclib flag = [ "-cclib"; flag ]

let jni_lib_opts os jdk_version jdk_home =
  let lib =
    let adds_arch_if_needed path =
      let arch = Config.architecture in
      match Unix.stat (path / arch) with
      | _ -> path / arch
      | exception _ -> path
    in
    let lib =
      match os with
      | Linux | Mac -> "lib"
      | Windows -> "bin"
    in
    match jdk_version with
    | n when n <= 8 -> adds_arch_if_needed (jdk_home / "jre" / lib)
    | 9 -> adds_arch_if_needed (jdk_home / lib)
    (* OpenJDK 10 and above *)
    | _ -> jdk_home / lib
  in
  let rpath path =
    let arg = "-Wl,-rpath," ^ path in
    match os with
    | Linux | Mac -> [ arg ]
    | Windows -> [ "-link"; arg ]
  in
  [ "-L" ^ lib; "-L" ^ (lib / "server") ]
  @ rpath lib
  @ rpath (lib / "server")
  @ rpath (lib / "native_threads")

let c_flags = jni_includes

let library_flags os jdk_version jdk_home =
  let jni_lib_opts =
    jni_lib_opts os jdk_version jdk_home
    |> List.map Filename.quote
    |> List.map ccopt
    |> List.flatten
  in
  jni_lib_opts @ cclib "-ljvm"

let generate_flags t =
  let os = OS.get_os t in
  let jdk_version = Jdk_version.get_jdk_version t in
  let jdk_home = Jdk_home.get_jdk_home t in
  Flags.write_sexp "c_flags.sexp" (c_flags os jdk_home);
  Flags.write_sexp "library_flags.sexp" (library_flags os jdk_version jdk_home)

let () = Configurator.V1.main ~name:"camljava_discover" generate_flags
