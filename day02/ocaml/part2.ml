open Base
open Stdio

module Command = struct
  type t = { direction : string; amount : int } [@@deriving fields]
end

module State = struct
  type t = { horizontal : int; depth : int; aim : int } [@@deriving fields]
end

let line_to_move line =
  match String.rsplit2 line ~on:' ' with
  | None -> None
  | Some (dir, amt) ->
      Some { Command.direction = dir; amount = Int.of_string amt }

let update state move =
  match move.Command.direction with
  | "down" -> { state with State.aim = state.State.aim + move.amount }
  | "up" -> { state with State.aim = state.State.aim - move.amount }
  | "forward" ->
     {
       state with
       horizontal = state.State.horizontal + move.Command.amount;
       depth = state.depth + (state.State.aim * move.Command.amount);
     }
  | _ -> state

let rec process_commands state =
  match In_channel.input_line In_channel.stdin with
  | None -> state
  | Some line -> (
      match line_to_move line with
      | None -> state
      | Some move -> process_commands (update state move))

let final_state = process_commands { horizontal = 0; depth = 0; aim = 0 }

let () = printf "Final state: %d %d\n" final_state.horizontal final_state.depth

let () = printf "Product: %d\n" (final_state.horizontal * final_state.depth)
