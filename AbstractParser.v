Require Import Ascii.
Require Import String.
Import List.ListNotations.

(* Context関係 *)

Class ParsingContext := {
  input : string;
  position : nat;
  stack : list nat;
}.

Instance InitContext (input : string) : ParsingContext := {
  input := input;
  position := 0;
  stack := nil;
}.

Instance save (context : ParsingContext) : ParsingContext := {
  input := input;
  position := position;
  stack := position :: stack;
}.

Instance restore (context : ParsingContext) : ParsingContext := {
  input := input;
  position := List.hd 0 stack;
  stack := List.tl stack;
}.

Instance consume (context : ParsingContext) : ParsingContext := {
  input := input;
  position := S (@position context);
  stack := stack
}.


(* bind for option *)

Definition bind {A B : Type} (f : A -> option B) (a : option A) : option B :=
  match a with
  | Some a' => f a'
  | None => None
  end.


(* Parsing関係の関数 *)

Definition accept (context : ParsingContext) (ch : ascii) : option ParsingContext :=
  bind
   (fun c => if ascii_dec ch c then Some (consume context) else None)
   (get position input).


(* Parserの型 *)

Definition Parser (T : Type) := ParsingContext -> option (T * ParsingContext).


(* 終了処理 *)

Definition EOF (context : ParsingContext) : bool :=
  Nat.eqb (length input) position.

Definition result {T : Type} (parser : Parser T) (input : string) :=
  bind (fun con => if EOF (snd con) then Some (fst con) else None) (parser (InitContext input)).

