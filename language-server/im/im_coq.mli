open Common.Scheduler

type feedback_level = Feedback.level
type feedback_message = feedback_level * Loc.t option * Quickfix.t list * Pp.t
type success = Vernacstate.t
type failure = Pp.t Loc.located * Quickfix.t list option * success option

val error : Loc.t option -> Quickfix.t list option -> Pp.t -> success -> failure

type sentence_id = Stateid.t
type loc = Loc.t

module Stateid = Stateid

type delegation_mode

val default_delegation_mode : delegation_mode

val do_opaque_proof :
  delegation_mode ->
  executable_sentence ->
  sentence_id ->
  executable_sentence list ->
  Vernacexpr.section_subset_expr ->
  Im_types.prepared_task list

val translate_delegation_mode :
  Protocol.Settings.DelegationMode.t -> int -> delegation_mode

type initial = Vernacstate.t
type feedback_route_id = Feedback.route_id