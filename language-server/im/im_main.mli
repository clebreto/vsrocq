(**************************************************************************)
(*                                                                        *)
(*                                 VSRocq                                 *)
(*                                                                        *)
(*                   Copyright INRIA and contributors                     *)
(*       (see version control and README file for authors & dates)        *)
(*                                                                        *)
(**************************************************************************)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License.         *)
(*   See LICENSE file.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Common.Types
open Protocol

(** The event manager is in charge of the actual event of tasks (as
    defined by the scheduler), caching event states and invalidating
    them. It can delegate to worker processes via DelegationManager *)

type options = {
  delegation_mode : Im_coq.delegation_mode;
  completion_options : Settings.Completion.t;
  enableDiagnostics : bool;
}

type errored_sentence = (sentence_id * Im_coq.loc option) option

val is_diagnostics_enabled: unit -> bool

(** Execution state, includes the cache *)
type state
type event
type events = event Sel.Event.t list

val pr_event : event -> Pp.t
val init : Vernacstate.t -> state * event Sel.Event.t
val destroy : state -> unit

val get_options : unit -> options
val set_options : options -> unit
val set_default_options : unit -> unit
val invalidate : Dm.Document.document -> Common.Scheduler.schedule -> sentence_id -> state -> state

val error : state -> sentence_id -> (Loc.t option * Pp.t) option
val feedback :  state -> sentence_id -> Im_coq.feedback_message list
val all_errors : state -> (sentence_id * (Loc.t option * Pp.t * Quickfix.t list option)) list
val all_feedback : state -> (sentence_id * Im_coq.feedback_message) list

val reset_overview : state -> Dm.Document.document -> state
val shift_overview : state -> before:Dm.RawDocument.t -> after:Dm.RawDocument.t -> start:int -> offset:int -> state
val shift_diagnostics_locs : state -> start:int -> offset:int -> state
val executed_ids : state -> sentence_id list

(** we know if it worked and we have the state in this process *)
val is_locally_executed : state -> sentence_id -> bool

(** we know if it worked but we do not have the state in this process *)
val is_remotely_executed : state -> sentence_id -> bool

val get_context : state -> sentence_id -> (Evd.evar_map * Environ.env) option
val get_initial_context : state -> Evd.evar_map * Environ.env

(** Returns the vernac state after the sentence *)
val get_vernac_state : state -> sentence_id -> Vernacstate.t option

(** Events for the main loop *)
val handle_event : event -> state -> (sentence_id option * state option * events)

(** Execution happens in two steps. In particular the event one takes only
    one task at a time to ease checking for interruption *)

val build_tasks_for : Dm.Document.document -> Common.Scheduler.schedule -> state -> sentence_id -> bool -> Vernacstate.t * state * Im_types.prepared_task option * errored_sentence
val execute : state -> Dm.Document.document -> Vernacstate.t * events * bool -> Im_types.prepared_task -> bool -> (Im_types.prepared_task option * state * Vernacstate.t * events * errored_sentence)

(* val update_overview : Im_types.prepared_task -> Im_types.prepared_task list -> state -> Document.document -> state
val cut_overview : Im_types.prepared_task -> state -> Document.document -> state *)
val update_processed : sentence_id -> state -> Dm.Document.document -> state
val prepare_overview : state -> LspWrapper.Range.t list -> state
val overview : state -> exec_overview
val overview_until_range : state -> LspWrapper.Range.t -> exec_overview
val print_exec_overview_of_state : state -> unit

(** Rocq toplevels for delegation without fork *)
module ProofWorkerProcess : sig
  type options
[%%if rocq = "8.18" || rocq = "8.19" || rocq = "8.20"]
   val parse_options : string list -> options * string list
[%%else]
   val parse_options : Coqargs.t -> string list -> options * string list
[%%endif]
  val main : st:Vernacstate.t -> options -> unit
  val log : ?force:bool -> (unit -> string) -> unit
end
