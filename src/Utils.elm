module Utils exposing (..)

import Html exposing (..)
import Html.Attributes   exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import ASM exposing (..)

showInstruction : Instruction -> String
showInstruction instr =
  let showAddr : Address -> String
      showAddr a = "[" ++ (String.fromInt a) ++ "]"

      showFrom: From -> String
      showFrom f =
        case f of
          Immediate i -> String.fromInt i
          Address   a -> showAddr a

  in case instr of
       Halt            -> "Halt"
       Nop             -> "Nop"
       Copy   f0 a1    -> "Copy " ++ (showFrom f0) ++ " " ++ (showAddr a1)
       PC     a0       -> "PC " ++ (showAddr a0)
       Jump   f0       -> "Jump " ++ (showFrom f0)
       JumpIf a0 f1    -> "JumpIf " ++ (showAddr a0) ++  " " ++ (showFrom f1)
       Add    a0 f1 a2 -> "Add " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       Mul    a0 f1 a2 -> "Mul " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       Sub    a0 f1 a2 -> "Sub " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       Div    a0 f1 a2 -> "Div " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       Eq     a0 f1 a2 -> "Eq " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       NE     a0 f1 a2 -> "NE " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       LE     a0 f1 a2 -> "LE " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       ASM.LT     a0 f1 a2 -> "LT " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       GE     a0 f1 a2 -> "GE " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       ASM.GT     a0 f1 a2 -> "GT " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       And    a0 f1 a2 -> "And " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       Or     a0 f1 a2 -> "Or " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       Not    a0 a1    -> "Not " ++ (showAddr a0) ++  " " ++ (showAddr a1)

showMaybe: (a -> String) -> Maybe a -> String
showMaybe f m =
  case m of
    Just a  -> "Just (" ++ f a ++ ")"
    Nothing -> "Nothing"

viewMachine: Machine -> Html a
viewMachine machine =
  let mth: String -> Html a
      mth s = th [] [text s]

      renderIndex: Int -> Int -> Html a
      renderIndex idx val =
        tr [] [
          td [] [text ("[" ++ String.fromInt idx ++ "] =")],
          td [] [text (String.fromInt val)]
        ]

  in div [style "width" "50%"] [
       h2 [] [text "Machine"],
       ul [] [
         li [] [text ("Status: " ++ (Debug.toString machine.state))],
         li [] [text ("Program Counter: " ++ (String.fromInt machine.pc))],
         li [] [text ("Current Instructiion: " ++ (showMaybe showInstruction (memory2instruction machine.memory machine.pc)))],
         li [] [
           table [] (
             (tr [] [mth "Index", mth "Valeur"]) ::
             (Array.indexedMap renderIndex machine.memory |> Array.toList)
           )
         ]
       ]
     ]

unfold: (b -> Maybe (a,b)) -> b -> List a
unfold f b0 =
  case f b0 of
    Just (a,b1) -> a :: unfold f b1
    Nothing -> []

decompile: Memory -> Array (Maybe Instruction)
decompile memory =
  let size    = Array.length memory

      next n  =
        if n < size
        then Just (n, n+4)
        else Nothing

  in unfold next 0
      |> Array.fromList
      |> Array.map (memory2instruction memory)

viewProgram: Machine -> Html a
viewProgram machine =
  let mth: String -> Html a
      mth s = th [] [text s]

      renderInstruction: Int -> Maybe Instruction -> Html a
      renderInstruction idx val =
        tr [] [
          td [] [text (String.fromInt (4*idx) )],
          td [] [text (showMaybe showInstruction val)]
        ]

  in div [style "width" "50%"] [
       h2 [] [text "Program"],
       table [] (
         (tr [] [mth "Index", mth "Instruction"]) ::
         let program = decompile machine.memory
         in Array.indexedMap renderInstruction program |> Array.toList
       )
     ]

type Msg = Step

view : Machine -> Html Msg
view machine =
  div [] [
    button [onClick Step] [text "Step"],
    div [style "display" "flex"] [
      viewMachine machine,
      viewProgram machine
    ]
  ]
