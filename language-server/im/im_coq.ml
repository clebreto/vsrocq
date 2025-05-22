open Common.Scheduler

type feedback_level = Feedback.level
type feedback_message = feedback_level * Loc.t option * Quickfix.t list * Pp.t
type success = Vernacstate.t
type failure = Pp.t Loc.located * Quickfix.t list option * Vernacstate.t option

let error loc qf msg vernac_st : failure = ((loc, msg), qf, Some vernac_st)

type sentence_id = Stateid.t
type loc = Loc.t

let log = Im_types.log

module Stateid = Stateid

type delegation_mode =
  | CheckProofsInMaster
  | SkipProofs
  | DelegateProofsToWorkers of { number_of_workers : int }

let default_delegation_mode = CheckProofsInMaster

let do_opaque_proof delegation_mode terminator opener_id tasks proof_using =
  let last_opt l = try Some (CList.last l).id with Failure _ -> None in
  match delegation_mode with
  | DelegateProofsToWorkers _ ->
      log (fun () -> "delegating proofs to workers");
      let last_step_id = last_opt tasks in
      [
        Im_types.PDelegate
          {
            terminator_id = terminator.id;
            opener_id;
            last_step_id;
            tasks;
            proof_using;
          };
      ]
  | CheckProofsInMaster ->
      log (fun () -> "running the proof in master as per config");
      List.map (fun x -> Im_types.PExec x) tasks @ [ PExec terminator ]
  | SkipProofs ->
      log (fun () ->
          Printf.sprintf "skipping proof made of %d tasks" (List.length tasks));
      [ PExec terminator ]

let translate_delegation_mode delegation number_of_workers =
  let open Protocol.Settings.DelegationMode in
  match delegation with
  | None -> CheckProofsInMaster
  | Skip -> SkipProofs
  | Delegate -> DelegateProofsToWorkers { number_of_workers }

type initial = Vernacstate.t
type feedback_route_id = Feedback.route_id