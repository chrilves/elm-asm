module Main exposing (..)

import ASM exposing (..)
import Browser exposing (..)
import Utils exposing (..)

fact5 : ASM.Program
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

main : Platform.Program () Machine Utils.Msg
main = Browser.sandbox {
    init = loadProgram fact5 0,
    view = view,
    update = \_ -> step
  }