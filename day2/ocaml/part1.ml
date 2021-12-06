open Base
open Stdio

type move = { direction : string; amount : int }

type state = { horizontal : int; depth : int }

let line_to_move line =
  match String.rsplit2 line ~on:' ' with
  | None -> None
  | Some (dir, amt) -> Some { direction = dir; amount = Int.of_string amt }

let update state move =
  match move.direction with
  | "up" -> { horizontal = state.horizontal; depth = state.depth - move.amount }
  | "down" ->
      { horizontal = state.horizontal; depth = state.depth + move.amount }
  | "forward" ->
      { horizontal = state.horizontal + move.amount; depth = state.depth }
  | _ -> state

let rec process_commands state =
  match In_channel.input_line In_channel.stdin with
  | None -> state
  | Some line -> (
      match line_to_move line with
      | None -> state
      | Some move -> process_commands (update state move))

let final_state = process_commands { horizontal = 0; depth = 0 }

let () = printf "Final state: %d %d\n" final_state.horizontal final_state.depth

let () = printf "Product: %d\n" (final_state.horizontal * final_state.depth)
