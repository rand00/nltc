open Batteries
module Sql = Sqlexpr_sqlite_lwt

module type S = sig 

  val put_mtokens : 
    tokens : Token.t list -> 
    tid1 : 'a -> 
    tid2 : 'a -> 
    unit Lwt.t

end

module NoAction : S = struct 

  let put_mtokens ~tokens ~tid1 ~tid2 = Lwt.return ()

end

module Print : S = struct

  open Lwt
  
  let put_mtokens ~tokens ~tid1 ~tid2 = 
    Lwt_list.iter_s (fun t -> 
        Lwt_io.printf "   %s\n" (Token.to_tstring t))
      tokens

end

