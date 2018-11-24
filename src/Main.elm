module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes   exposing (..)
import Html.Events exposing (..)
import Browser exposing (..)

type alias Address = Int
type alias Memory  = Array Int

type From = Immediate Int
          | Address   Int

type Instruction = Halt
                 | Nop
                 | Copy   From Address
                 | PC     Address
                 | Jump   From
                 | JumpIf Address From
                 | Add    Address From Address
                 | Mul    Address From Address
                 | Sub    Address From Address
                 | Div    Address From Address
                 | Eq     Address From Address
                 | NE     Address From Address
                 | LE     Address From Address
                 | LT     Address From Address
                 | GE     Address From Address
                 | GT     Address From Address
                 | And    Address From Address
                 | Or     Address From Address
                 | Not    Address Address
                 
type alias Program = List Instruction

intruction2memory: Instruction -> List Int
intruction2memory instr =
  case instr of
    Halt                        -> [ 0,  0,  0,  0]
    Nop                         -> [ 1,  0,  0,  0]
    Copy   (Immediate i0) a1    -> [ 2, i0, a1,  0]
    Copy   (Address   a0) a1    -> [ 3, a0, a1,  0]
    PC     a0                   -> [ 4, a0,  0,  0] 
    Jump   (Immediate i0)       -> [ 6, i0,  0,  0]
    Jump   (Address   a0)       -> [ 7, a0,  0,  0]
    JumpIf a0 (Immediate i1)    -> [ 8, a0, i1,  0]
    JumpIf a0 (Address   a1)    -> [ 9, a0, a1,  0]
    Add    a0 (Immediate i1) a2 -> [10, a0, i1, a2]
    Add    a0 (Address   a1) a2 -> [11, a0, a1, a2]
    Mul    a0 (Immediate i1) a2 -> [12, a0, i1, a2]
    Mul    a0 (Address   a1) a2 -> [13, a0, a1, a2]
    Sub    a0 (Immediate i1) a2 -> [14, a0, i1, a2]
    Sub    a0 (Address   a1) a2 -> [15, a0, a1, a2]
    Div    a0 (Immediate i1) a2 -> [16, a0, i1, a2]
    Div    a0 (Address   a1) a2 -> [17, a0, a1, a2]
    Eq     a0 (Immediate i1) a2 -> [18, a0, i1, a2]
    Eq     a0 (Address   a1) a2 -> [19, a0, a1, a2]
    NE     a0 (Immediate i1) a2 -> [20, a0, i1, a2]
    NE     a0 (Address   a1) a2 -> [21, a0, a1, a2]
    LE     a0 (Immediate i1) a2 -> [22, a0, i1, a2]
    LE     a0 (Address   a1) a2 -> [23, a0, a1, a2]
    LT     a0 (Immediate i1) a2 -> [24, a0, i1, a2]
    LT     a0 (Address   a1) a2 -> [25, a0, a1, a2]
    GE     a0 (Immediate i1) a2 -> [26, a0, i1, a2]
    GE     a0 (Address   a1) a2 -> [27, a0, a1, a2]
    GT     a0 (Immediate i1) a2 -> [28, a0, i1, a2]
    GT     a0 (Address   a1) a2 -> [29, a0, a1, a2]
    And    a0 (Immediate i1) a2 -> [30, a0, i1, a2]
    And    a0 (Address   a1) a2 -> [31, a0, a1, a2]
    Or     a0 (Immediate i1) a2 -> [32, a0, i1, a2]
    Or     a0 (Address   a1) a2 -> [33, a0, a1, a2]
    Not    a0 a1                -> [34, a0, a1,  0]

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
         2 -> Maybe.map2 Copy   (get (pc + 1) |> Maybe.map Immediate) (get (pc + 2))
         3 -> Maybe.map2 Copy   (get (pc + 1) |> Maybe.map Address  ) (get (pc + 2))
         4 -> Maybe.map  PC     (get (pc + 1))
         6 -> Maybe.map  Jump   (get (pc + 1) |> Maybe.map Immediate)
         7 -> Maybe.map  Jump   (get (pc + 1) |> Maybe.map Address  )
         8 -> Maybe.map2 JumpIf (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate)
         9 -> Maybe.map2 JumpIf (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  )
         10 -> Maybe.map3 Add   (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         11 -> Maybe.map3 Add   (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         12 -> Maybe.map3 Mul   (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         13 -> Maybe.map3 Mul   (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         14 -> Maybe.map3 Sub   (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         15 -> Maybe.map3 Sub   (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         16 -> Maybe.map3 Div   (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         17 -> Maybe.map3 Div   (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         18 -> Maybe.map3 Eq    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         19 -> Maybe.map3 Eq    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         20 -> Maybe.map3 NE    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         21 -> Maybe.map3 NE    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         22 -> Maybe.map3 LE    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         23 -> Maybe.map3 LE    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         24 -> Maybe.map3 LT    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         25 -> Maybe.map3 LT    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         26 -> Maybe.map3 GE    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         27 -> Maybe.map3 GE    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         28 -> Maybe.map3 GT    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         29 -> Maybe.map3 GT    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         30 -> Maybe.map3 And   (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         31 -> Maybe.map3 And   (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         32 -> Maybe.map3 Or    (get (pc + 1)) (get (pc + 2) |> Maybe.map Immediate) (get (pc + 3))
         33 -> Maybe.map3 Or    (get (pc + 1)) (get (pc + 2) |> Maybe.map Address  ) (get (pc + 3))
         34 -> Maybe.map2 Not   (get (pc + 1)) (get (pc + 2))
         _  -> Nothing
     )

halt: Machine -> Machine
halt machine = { machine | state = Finished }

nop: Machine -> Machine
nop machine = { machine | pc = machine.pc + 4 }

valueAt: Machine -> From -> Maybe Int
valueAt machine from =
  case from of
    Immediate i -> Just i
    Address   a -> Array.get a machine.memory

copy: From -> Address -> Machine -> Machine
copy from a machine =
  case valueAt machine from of
    Nothing -> {machine | state = Error "Bad address" }
    Just i  -> { machine | memory = Array.set a i machine.memory, pc = machine.pc + 4 }

jump: From -> Machine -> Machine
jump from machine = 
  case valueAt machine from of
    Nothing -> {machine | state = Error "Jump to a bad address" }
    Just i  -> {machine | pc = i }

jumpIf: Address -> From -> Machine -> Machine
jumpIf a from machine =
  let doIt: Int -> Int -> Machine
      doIt i0 i1 =
        if i0 /= 0
        then {machine | pc = i1 }
        else {machine | pc = machine.pc + 4 }

      res : Maybe Machine 
      res = Maybe.map2 doIt (Array.get a machine.memory) (valueAt machine from)

  in Maybe.withDefault {machine | state = Error "Bad address"} res
            
binOp : (Int -> Int -> Int) -> Address -> From -> Address -> Machine -> Machine
binOp op a0 f1 a2 machine =
  let i0 = Array.get a0 machine.memory
      i1 = valueAt machine f1
      
      op2 x y = Array.set a2 (op x y) machine.memory

  in case Maybe.map2 op2 i0 i1 of
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
              Copy   i a      -> copy i a machine
              PC     a        -> {machine |
                                    memory = Array.set a (machine.pc + 4) machine.memory,
                                    pc = machine.pc + 4
                                  }
              Jump   a        -> jump a machine
              JumpIf a0 a1    -> jumpIf a0 a1 machine
              Add    a0 a1 a2 -> binOp (+)  a0 a1 a2 machine
              Mul    a0 a1 a2 -> binOp (*)  a0 a1 a2 machine
              Sub    a0 a1 a2 -> binOp (-)  a0 a1 a2 machine
              Div    a0 a1 a2 -> binOp (//) a0 a1 a2 machine
              Eq     a0 a1 a2 -> binOp (\x y -> if x==y then 1 else 0) a0 a1 a2 machine
              NE     a0 a1 a2 -> binOp (\x y -> if x/=y then 1 else 0) a0 a1 a2 machine
              LE     a0 a1 a2 -> binOp (\x y -> if x<=y then 1 else 0) a0 a1 a2 machine
              LT     a0 a1 a2 -> binOp (\x y -> if x<y  then 1 else 0) a0 a1 a2 machine
              GE     a0 a1 a2 -> binOp (\x y -> if x>=y then 1 else 0) a0 a1 a2 machine
              GT     a0 a1 a2 -> binOp (\x y -> if x>y  then 1 else 0) a0 a1 a2 machine
              And    a0 a1 a2 -> binOp (\x y -> if x/=0 && y/=0 then 1 else 0) a0 a1 a2 machine
              Or     a0 a1 a2 -> binOp (\x y -> if x/=0 || y/=0 then 1 else 0) a0 a1 a2 machine
              Not    a0 a1    -> non a0 a1 machine
    _ -> machine

loadProgram: Program -> Int -> Machine
loadProgram pgm i =
  { memory = pgm |> List.concatMap intruction2memory |> (\x -> x ++ (List.repeat (4*i) 0)) |> Array.fromList,
    pc = 0,
    state = Running
  }
  

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
       LT     a0 f1 a2 -> "LT " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       GE     a0 f1 a2 -> "GE " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
       GT     a0 f1 a2 -> "GT " ++ (showAddr a0) ++ " " ++ (showFrom f1) ++ " " ++ (showAddr a2)
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


fact5 : Program 
fact5 = [
  Nop,
  Copy (Immediate 5) 0, -- [0] n
  Copy (Immediate 1) 1, -- [1] x
  Copy (Immediate 2) 2, -- [2] i
  
  LE 2 (Address 0) 3, -- [3] i!=n
  Not 3 3,
  JumpIf 3 (Immediate 40),
  Mul 2 (Address   1) 1, -- x = i * x
  Add 2 (Immediate 1) 2, -- i++
  Jump (Immediate 16),
  
  Halt
  ]

main : Platform.Program () Machine Msg
main = Browser.sandbox {
    init = loadProgram fact5 0,
    view = view,
    update = \_ -> step
  }