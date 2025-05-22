open Common.Types
open Common.Scheduler

let (Log log) = Common.Log.mk_log "executionManager"

type delegated_task = { 
  terminator_id: sentence_id;
  opener_id: sentence_id;
  proof_using: Vernacexpr.section_subset_expr;
  last_step_id: sentence_id option; (* only for setting a proof remotely *)
  tasks: executable_sentence list;
}

type prepared_task =
  | PSkip of { id: sentence_id; error: Pp.t option }
  | PBlock of { id: sentence_id; error: Pp.t Loc.located }
  | PExec of executable_sentence
  | PQuery of executable_sentence
  | PDelegate of delegated_task

let get_id_of_executed_task task =
  match task with
  | PSkip {id} -> id
  | PBlock {id} -> id
  | PExec {id} -> id
  | PQuery {id} -> id
  | PDelegate {terminator_id} -> terminator_id

let id_of_prepared_task = function
  | PSkip { id } -> id
  | PBlock { id } -> id
  | PExec ex -> ex.id
  | PQuery ex -> ex.id
  | PDelegate { terminator_id } -> terminator_id
