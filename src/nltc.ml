(************************************************************************)
(*                              NLTC                                    *)
(* - A natural language text comparator written in OCaml                *)
(* Copyright (C) 2016  Claes Worm                                       *)
(*                                                                      *)
(* This program is free software: you can redistribute it and/or modify *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 3 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* This program is distributed in the hope that it will be useful,      *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.*)
(************************************************************************)


open Batteries

(*>module Sqlexpr need to be here for syntax extension*)
module Sqlexpr = Sqlexpr_sqlite_lwt
module Sqex = Sqlexpr 
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type jobs =
  | Cli_txtmatch_lowbound of float
  | Cli_pomp_dbloc of string
  | Cli_pomp_dbversion of string
  | Cli_pomp_datasets of string
  | Cli_pomp_documents of string
  | Cli_pomp_sections of string
  | Cli_pomp_run of string
  | Cli_local_clean 
  | Cli_local_insert of string
  | Cli_local_run of string
  | DB_pomp_analysis of int
  [@@deriving ord]

let jobs = ref []
let add_job job = jobs := job :: !jobs

let _ = 
  Sys.chdir (Filename.dirname Sys.argv.(0));
  (*< needed for the case where program is run from another location*)
  Parallel.init ();
  let db_local =
    let db_local = Sqex.open_db "local.db" in
    at_exit (fun () -> Sqex.close_db db_local);
    DB.Local.init_tables db_local
    >> Lwt.return db_local
  in
  let program_name = 
    let n = Filename.basename Sys.argv.(0) in
    let n = 
      try Filename.chop_extension n 
      with Invalid_argument _ -> n in
    n 
  in
  Arg.parse 
    [ 
      ("--db-local-clean", 
       Arg.Unit (fun () -> add_job Cli_local_clean), 
       ": Deletes entries from the 'documents' table of [db-local].\n");

      ("--db-local-run", 
       Arg.String (fun arg -> add_job @@ Cli_local_run arg), 
       String.concat ""
         [ ": Tests a part of the program with the current content \
            of the local database.\n";
           "      Options are:\n";
           "         token            -> Runs the tokenizer on contents \
                                         and excludes tokentypes.\n";
           "         token_types      -> Runs the tokenizer on contents \
                                         and prints token-types with \
                                         tokens.\n";
           "         content          -> Print raw content from [local \
                                         db].\n"; 
           "         compare          -> Output a sorted list of \
                                         matching text's.\n"]);

      ("--txtmatch-lowbound", 
       Arg.Float (fun arg -> add_job @@ Cli_txtmatch_lowbound arg),
       ": Sets the lower bound for text-matches to be included in\
        comparison results.\n");      

      ("--db-pomp-datasets", 
       Arg.String (fun arg -> add_job @@ Cli_pomp_datasets arg),
       ": Controls which datasets to include in comparison of texts \
          of [db-pomp].\n      Supply a comma-separated string of \
          numbers or the string \"all\".\n");      

      ("--db-pomp-documents", 
       Arg.String (fun arg -> add_job @@ Cli_pomp_documents arg),
       ": Like '--db-pomp-datasets', but filters documents.\n");
      
      ("--db-pomp-sections", 
       Arg.String (fun arg -> add_job @@ Cli_pomp_sections arg),
       ": Like '--db-pomp-datasets', but filters sections/template-\
        title-id's.\n");

      ("--db-pomp-loc", 
       Arg.String (fun arg -> add_job @@ Cli_pomp_dbloc arg),
       ": Controls which location of [db-pomp] to use. \
          \n      Supply an absolute path to the db to use, or one \
          \n      relative to where '"^program_name^"' lies .\n");

      ("--db-pomp-version", 
       Arg.String (fun arg -> add_job @@ Cli_pomp_dbversion arg),
       ": Controls which version of [db-pomp] to use. \
          \n      Currently you can choose 'v1' or 'v2'.\n");

      ("--db-pomp-run", Arg.String (fun arg -> add_job @@ Cli_pomp_run arg), 
       ": Like argument [--db-local] but tests on the sections given as \
          argument to\n      '--db-pomp-sections'.\n");

      ("--run-id", Arg.Int (fun id -> add_job @@ DB_pomp_analysis id),
       ": Starts an analysis with the argument 'analysis-ID', \
        which's settings are listed in\n      the Pomp database.\
        This means that none of the settings given via the CLI\n\
        \      counts.\n")
    ] 

    (fun file -> add_job @@ Cli_local_insert file
    )

    (String.concat "\n" 
       [ "\n"^program_name^" usage:"; 
         "  The last arguments given on cmd-line are loaded as text-files \
          into [db-local].\n" ]);

  let options = ref Arg_aux.({ txtmatch_lowbound = None })
  and pomp_db_datasets = ref `All         
  and pomp_db_docs = ref `All       
  and pomp_db_sects = ref `All
  and pomp_db_version = ref `V2 
  and pomp_db_loc = ref "../db/POMP_new_data.sqlite"
  in
  let job_thunks = 
    List.sort compare_jobs !jobs
    |> List.map 
      (function
        | Cli_txtmatch_lowbound lowbound ->
          (fun () ->
             options :=
               Arg_aux.({ !options with txtmatch_lowbound = Some lowbound });
             Lwt.return ()
          )
        | Cli_local_clean -> 
          (fun () -> db_local >>= DB.Local.del_docs)
        | Cli_local_insert file -> 
          (fun () -> 
             match Sys.file_exists file with 
             | true -> 
               db_local >>= fun db -> 
               Sqex.execute db
                 DB.Local.Ins.replace_filetext file (input_file file)
             | false -> 
               failwith 
                 (Printf.sprintf "[%s]: %s%s%s\n" 
                    program_name
                    "The file you tried to insert in local-db'" 
                    file 
                    "' does not exist."))
        | Cli_local_run arg -> 
          (fun () -> 
             db_local >>= fun db -> 
             Arg_aux.run_db_cli arg ~db:(`Local db) ~options:!options)
        | Cli_pomp_datasets arg -> 
          (fun () ->
             match Arg_aux.parse_filter arg with
             | Some filter -> 
               pomp_db_datasets := filter;
               Lwt.return ()
             | None -> Lwt.return () )
        | Cli_pomp_documents arg -> 
          (fun () ->
             match Arg_aux.parse_filter arg with
             | Some filter -> 
               pomp_db_docs := filter;
               Lwt.return ()
             | None -> Lwt.return () )
        | Cli_pomp_sections arg -> 
          (fun () ->
             match Arg_aux.parse_filter arg with
             | Some filter -> 
               pomp_db_sects := filter;
               Lwt.return ()
             | None -> Lwt.return () )
        | Cli_pomp_dbloc arg -> 
          (fun () ->
             pomp_db_loc := arg;
             Lwt.return ()
          )
        | Cli_pomp_dbversion arg -> 
          (fun () -> 
             match arg with 
             | "v1" | "V1" -> 
               begin
                 pomp_db_version := `V1;
                 Lwt.return ()
               end
             | "v2" | "V2" -> 
               begin
                 pomp_db_version := `V2;
                 Lwt.return ()
               end 
             | _ -> 
               prerr_endline 
                 (Printf.sprintf "[%s]: %s\n" program_name 
                    "Failure: Pomp-db version argument wrong.");
               exit 1
          )
        | Cli_pomp_run arg -> 
          (fun () ->
             let db_pomp =
               let db_pomp = Sqex.open_db !pomp_db_loc in
               at_exit (fun () -> Sqex.close_db db_pomp);
               db_pomp
             in
             match !pomp_db_version with
             | `V1 -> 
               Arg_aux.run_db_cli arg 
                 ~db:(`Pomp_v1 (db_pomp, !pomp_db_sects))
                 ~options:!options
             | `V2 ->
               let string_of_filter = function
                   `All -> "All"
                 | `List l -> List.map Int.to_string l |> String.concat ","
               in
               Printf.printf "Sections-filter : %s\nDocuments-filter : %s\n\
                              Datasets-filter : %s\n"
                 (string_of_filter !pomp_db_sects)
                 (string_of_filter !pomp_db_docs)
                 (string_of_filter !pomp_db_datasets);
               flush_all ();
               let filters =
                 DB.PompV2.T.(
                   { sects = !pomp_db_sects
                   ; docs = !pomp_db_docs
                   ; datasets = !pomp_db_datasets }
                 )
               in
               Arg_aux.run_db_cli arg 
                 ~db:(`Pomp_v2 (db_pomp, filters))
                 ~options:!options
          )
        | DB_pomp_analysis anal_id -> 
          (fun () ->
             (*let db_pomp = 
                 let db_pomp = Sqex.open_db !pomp_db_loc in
                 at_exit (fun () -> Sqex.close_db db_pomp);
                 db_pomp
               in *)
             let version = match !pomp_db_version 
               with `V1 -> "V1" | `V2 -> "V2"
             in 
             Printf.printf
               "Hello Mr. analysis-runner. \n\
                \  We ran a succesful non-analysis with \n\
                \    analysis-id: %d\n\
                \    pomp-db location: %s\n\
                \    pomp-db version: %s\n\
                \  Congratz.\n"
               anal_id
               !pomp_db_loc
               version;
                Lwt.return ()
          )
(*
       (fun () -> 
         Arg_aux.run_anal_pomp 
           ~anal_id
           ~db:(db_pomp, !pomp_db_version)
*)
      )
  in 
  Lwt_main.run @@ Lwt_list.iter_s (fun f -> f ()) job_thunks




