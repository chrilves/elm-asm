module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes   exposing (..)
import Html.Events exposing (..)
import Browser exposing (..)

type alias Address = Int
type alias Memory  = Array Int

type Instruction = Halt
                 | Nop
                 | Load   Int     Address
                 | Jump   Address
                 | JumpIf Address Address
                 | Add    Address Address Address
                 | Mul    Address Address Address
                 | Sub    Address Address Address
                 | Div    Address Address Address
                 | Eq     Address Address Address
                 | LE     Address Address Address
                 | LT     Address Address Address
                 | GE     Address Address Address
                 | GT     Address Address Address
                 | And    Address Address Address
                 | Or     Address Address Address
                 | Not    Address Address
                 
type alias Program = List Instruction

intruction2memory: Instruction -> List Int
intruction2memory instr =
  case instr of
    Halt            -> [ 0,  0,  0,  0]
    Nop             -> [ 1,  0,  0,  0]
    Load   i a      -> [ 2,  i,  a,  0]
    Jump   a        -> [ 3,  a,  0,  0]
    JumpIf a0 a1    -> [ 4, a0, a1,  0]
    Add    a0 a1 a2 -> [ 5, a0, a1, a2]
    Mul    a0 a1 a2 -> [ 6, a0, a1, a2]
    Sub    a0 a1 a2 -> [ 7, a0, a1, a2]
    Div    a0 a1 a2 -> [ 8, a0, a1, a2]
    Eq     a0 a1 a2 -> [ 9, a0, a1, a2]
    LE     a0 a1 a2 -> [10, a0, a1, a2]
    LT     a0 a1 a2 -> [11, a0, a1, a2]
    GE     a0 a1 a2 -> [12, a0, a1, a2]
    GT     a0 a1 a2 -> [13, a0, a1, a2]
    And    a0 a1 a2 -> [14, a0, a1, a2]
    Or     a0 a1 a2 -> [15, a0, a1, a2]
    Not    a0 a1    -> [16, a0, a1,  0]

type State = Running | Finished | Error String

type alias Machine = {
    memory : Array Int,
    pc: Int,
    state: State
  }

memory2instruction : Memory -> Int -> Maybe (Instruction)
memory2instruction memory pc =
  let get : Int -> Maybe Int
      get i = Array.get i memory

  in get pc |> Maybe.andThen (\opcode ->
       case opcode of
         0 -> Just Halt
         1 -> Just Nop
         2 -> Maybe.map2 Load   (get (pc + 1)) (get (pc + 2))
         3 -> Maybe.map  Jump   (get (pc + 1))
         4 -> Maybe.map2 JumpIf (get (pc + 1)) (get (pc + 2))
         5 -> Maybe.map3 Add    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         6 -> Maybe.map3 Mul    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         7 -> Maybe.map3 Sub    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         8 -> Maybe.map3 Div    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         9 -> Maybe.map3 Eq     (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         10 -> Maybe.map3 LE    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         11 -> Maybe.map3 LT    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         12 -> Maybe.map3 GE    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         13 -> Maybe.map3 GT    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         14 -> Maybe.map3 And   (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         15 -> Maybe.map3 Or    (get (pc + 1)) (get (pc + 2)) (get (pc + 3))
         16 -> Maybe.map2 Not   (get (pc + 1)) (get (pc + 2))
         _  -> Nothing
     )

halt: Machine -> Machine
halt machine = { machine | state = Finished }

nop: Machine -> Machine
nop machine = { machine | pc = machine.pc + 4 }

load: Int -> Address -> Machine -> Machine
load i a machine =
  { machine | memory = Array.set a i machine.memory, pc = machine.pc + 4 }

jump: Address -> Machine -> Machine
jump a machine = 
  case Array.get a machine.memory of
    Nothing -> {machine | state = Error ("Jump to the bad address: " ++ String.fromInt a) }
    Just i  -> {machine | pc = i }

jumpIf: Address -> Address -> Machine -> Machine
jumpIf a0 a1 machine =
  let get : Int -> Maybe Int
      get i = Array.get i machine.memory

      doIt: Int -> Int -> Machine
      doIt i0 i1 =
        if i0 /= 0
        then {machine | pc = i1 }
        else {machine | pc = machine.pc + 4 }

      res : Maybe Machine 
      res = Maybe.map2 doIt (get a0) (get a1)

  in Maybe.withDefault {machine | state = Error "Bad address"} res
            
binOp : (Int -> Int -> Int) -> Int -> Int -> Int -> Machine -> Machine
binOp f a0 a1 a2 machine =
  let get : Int -> Maybe Int
      get a = Array.get a machine.memory
  in case Maybe.map2 f (get a0) (get a1) |> Maybe.map (\x -> Array.set a2 x machine.memory) of
       Just mem -> {machine | memory = mem, pc = machine.pc + 4 }
       Nothing  -> {machine | state = Error "Mauvaise adresse"}

non: Address -> Address -> Machine -> Machine
non a0 a1 machine =
  case Array.get a0 machine.memory of
    Nothing -> {machine | state = Error ("Bad a0 address for not: " ++ String.fromInt a0) }
    Just i  -> let r   = if i/=0 then 0 else 1
                   mem = Array.set a1 r machine.memory
               in {machine | memory = mem, pc = machine.pc + 4}

step: Machine -> Machine
step machine =
  case machine.state of
    Running ->
      case memory2instruction machine.memory machine.pc of
          Nothing    -> { machine | state = Error "Bad instruction"}
          Just instr ->
            case instr of
              Halt            -> halt machine
              Nop             -> nop  machine
              Load   i a      -> load i a machine
              Jump   a        -> jump a machine
              JumpIf a0 a1    -> jumpIf a0 a1 machine
              Add    a0 a1 a2 -> binOp (+)  a0 a1 a2 machine
              Mul    a0 a1 a2 -> binOp (*)  a0 a1 a2 machine
              Sub    a0 a1 a2 -> binOp (-)  a0 a1 a2 machine
              Div    a0 a1 a2 -> binOp (//) a0 a1 a2 machine
              Eq     a0 a1 a2 -> binOp (\x y -> if x==y then 1 else 0) a0 a1 a2 machine
              LE     a0 a1 a2 -> binOp (\x y -> if x<=y then 1 else 0) a0 a1 a2 machine
              LT     a0 a1 a2 -> binOp (\x y -> if x<y  then 1 else 0) a0 a1 a2 machine
              GE     a0 a1 a2 -> binOp (\x y -> if x>=y then 1 else 0) a0 a1 a2 machine
              GT     a0 a1 a2 -> binOp (\x y -> if x>y  then 1 else 0) a0 a1 a2 machine
              And    a0 a1 a2 -> binOp (\x y -> if x/=0 && y/=0 then 1 else 0) a0 a1 a2 machine
              Or     a0 a1 a2 -> binOp (\x y -> if x/=0 && y/=0 then 1 else 0) a0 a1 a2 machine
              Not    a0 a1    -> non a0 a1 machine
    _ -> machine

loadProgram: Program -> Int -> Machine
loadProgram pgm i =
  { memory = pgm |> List.concatMap intruction2memory |> (++) (List.repeat (4*i) 0) |> Array.fromList,
    pc = 0,
    state = Running
  }
  

viewMachine: Machine -> Html a
viewMachine machine =
  let mth: String -> Html a
      mth s = th [] [text s]

      renderIndex: Int -> Int -> Html a
      renderIndex idx val = tr [] [td [] [text (String.fromInt idx)], td [] [text (String.fromInt val)]]
  in div [style "width" "50%"] [
       h2 [] [text "Machine"],
       ul [] [
         li [] [text ("Status: " ++ (Debug.toString machine.state))],
         li [] [text ("Program Counter: " ++ (String.fromInt machine.pc))],
         li [] [text ("Current Instructiion: " ++ (Debug.toString (memory2instruction machine.memory machine.pc)))],
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
          td [] [text (Debug.toString val     )]
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


fact5 : Program 
fact5 = [
  Halt
  ]

main : Platform.Program () Machine Msg
main = Browser.sandbox {
    init = loadProgram fact5 0,
    view = view,
    update = \_ -> step
  }