module TestRound exposing (roundTest)

import Test exposing (..)
import Expect exposing (equal)

import TestFunction exposing (testFunction)
import Round

data =
  [(0,"0","0","0","0.0","0.00")
  ,(0,"0","0","0","0.0","0.00")
  ,(0,"0","0","0","0.0","0.00")
  ,(0,"0","0","0","0.0","0.00")
  ,(0,"0","0","0","0.0","0.00")
  ,(99,"100","100","99","99.0","99.00")
  ,(9.9,"0","10","10","9.9","9.90")
  ,(0.99,"0","0","1","1.0","0.99")
  ,(0.099,"0","0","0","0.1","0.10")
  ,(0.0099,"0","0","0","0.0","0.01")
  ,(-99,"-100","-100","-99","-99.0","-99.00")
  ,(-9.9,"0","-10","-10","-9.9","-9.90")
  ,(-0.99,"0","0","-1","-1.0","-0.99")
  ,(-0.099,"0","0","0","-0.1","-0.10")
  ,(-0.0099,"0","0","0","0.0","-0.01")
  ,(1,"0","0","1","1.0","1.00")
  ,(1.1,"0","0","1","1.1","1.10")
  ,(1.01,"0","0","1","1.0","1.01")
  ,(1.001,"0","0","1","1.0","1.00")
  ,(-1,"0","0","-1","-1.0","-1.00")
  ,(-1.1,"0","0","-1","-1.1","-1.10")
  ,(-1.01,"0","0","-1","-1.0","-1.01")
  ,(-1.001,"0","0","-1","-1.0","-1.00")
  ,(213,"200","210","213","213.0","213.00")
  ,(213.1,"200","210","213","213.1","213.10")
  ,(213.01,"200","210","213","213.0","213.01")
  ,(213.001,"200","210","213","213.0","213.00")
  ,(-213,"-200","-210","-213","-213.0","-213.00")
  ,(-213.1,"-200","-210","-213","-213.1","-213.10")
  ,(-213.01,"-200","-210","-213","-213.0","-213.01")
  ,(-213.001,"-200","-210","-213","-213.0","-213.00")
  ,(5.5,"0","10","6","5.5","5.50")
  ,(5.55,"0","10","6","5.6","5.55")
  ,(5.555,"0","10","6","5.6","5.56")
  ,(5.5555,"0","10","6","5.6","5.56")
  ,(-5.5,"0","-10","-5","-5.5","-5.50")
  ,(-5.55,"0","-10","-6","-5.5","-5.55")
  ,(-5.555,"0","-10","-6","-5.6","-5.55")
  ,(-5.5555,"0","-10","-6","-5.6","-5.56")
  ,(5.5,"0","10","6","5.5","5.50")
  ,(5.51,"0","10","6","5.5","5.51")
  ,(5.501,"0","10","6","5.5","5.50")
  ,(5.5001,"0","10","6","5.5","5.50")
  ,(-5.5,"0","-10","-5","-5.5","-5.50")
  ,(-5.51,"0","-10","-6","-5.5","-5.51")
  ,(-5.501,"0","-10","-6","-5.5","-5.50")
  ,(-5.5001,"0","-10","-6","-5.5","-5.50")
  ,(4.9,"0","0","5","4.9","4.90")
  ,(4.99,"0","0","5","5.0","4.99")
  ,(4.999,"0","0","5","5.0","5.00")
  ,(4.9999,"0","0","5","5.0","5.00")
  ,(-4.9,"0","0","-5","-4.9","-4.90")
  ,(-4.99,"0","0","-5","-5.0","-4.99")
  ,(-4.999,"0","0","-5","-5.0","-5.00")
  ,(-4.9999,"0","0","-5","-5.0","-5.00")
  ]

roundTest : Test
roundTest =
  testFunction "round" Round.round data
