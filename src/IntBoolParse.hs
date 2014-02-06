{-# OPTIONS_GHC -w #-}
module IntBoolParse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Value
import IntBool
import Lexer

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (11) = happyShift action_10
action_0 (14) = happyShift action_11
action_0 (15) = happyShift action_12
action_0 (16) = happyShift action_2
action_0 (18) = happyShift action_13
action_0 (19) = happyShift action_14
action_0 (22) = happyShift action_15
action_0 (31) = happyShift action_16
action_0 (33) = happyShift action_17
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 _ = happyFail

action_1 (16) = happyShift action_2
action_1 _ = happyFail

action_2 (18) = happyShift action_32
action_2 _ = happyFail

action_3 (35) = happyAccept
action_3 _ = happyFail

action_4 (32) = happyShift action_31
action_4 _ = happyReduce_3

action_5 (30) = happyShift action_30
action_5 _ = happyReduce_5

action_6 (25) = happyShift action_26
action_6 (26) = happyShift action_27
action_6 (27) = happyShift action_28
action_6 (29) = happyShift action_29
action_6 _ = happyReduce_7

action_7 (21) = happyShift action_24
action_7 (22) = happyShift action_25
action_7 _ = happyReduce_13

action_8 (23) = happyShift action_22
action_8 (24) = happyShift action_23
action_8 _ = happyReduce_16

action_9 _ = happyReduce_19

action_10 (33) = happyShift action_21
action_10 _ = happyFail

action_11 _ = happyReduce_21

action_12 _ = happyReduce_22

action_13 _ = happyReduce_25

action_14 _ = happyReduce_20

action_15 (14) = happyShift action_11
action_15 (15) = happyShift action_12
action_15 (18) = happyShift action_13
action_15 (19) = happyShift action_14
action_15 (22) = happyShift action_15
action_15 (31) = happyShift action_16
action_15 (33) = happyShift action_17
action_15 (10) = happyGoto action_20
action_15 _ = happyFail

action_16 (14) = happyShift action_11
action_16 (15) = happyShift action_12
action_16 (18) = happyShift action_13
action_16 (19) = happyShift action_14
action_16 (22) = happyShift action_15
action_16 (31) = happyShift action_16
action_16 (33) = happyShift action_17
action_16 (10) = happyGoto action_19
action_16 _ = happyFail

action_17 (11) = happyShift action_10
action_17 (14) = happyShift action_11
action_17 (15) = happyShift action_12
action_17 (16) = happyShift action_2
action_17 (18) = happyShift action_13
action_17 (19) = happyShift action_14
action_17 (22) = happyShift action_15
action_17 (31) = happyShift action_16
action_17 (33) = happyShift action_17
action_17 (4) = happyGoto action_18
action_17 (5) = happyGoto action_4
action_17 (6) = happyGoto action_5
action_17 (7) = happyGoto action_6
action_17 (8) = happyGoto action_7
action_17 (9) = happyGoto action_8
action_17 (10) = happyGoto action_9
action_17 _ = happyFail

action_18 (34) = happyShift action_45
action_18 _ = happyFail

action_19 _ = happyReduce_24

action_20 _ = happyReduce_23

action_21 (11) = happyShift action_10
action_21 (14) = happyShift action_11
action_21 (15) = happyShift action_12
action_21 (16) = happyShift action_2
action_21 (18) = happyShift action_13
action_21 (19) = happyShift action_14
action_21 (22) = happyShift action_15
action_21 (31) = happyShift action_16
action_21 (33) = happyShift action_17
action_21 (4) = happyGoto action_44
action_21 (5) = happyGoto action_4
action_21 (6) = happyGoto action_5
action_21 (7) = happyGoto action_6
action_21 (8) = happyGoto action_7
action_21 (9) = happyGoto action_8
action_21 (10) = happyGoto action_9
action_21 _ = happyFail

action_22 (14) = happyShift action_11
action_22 (15) = happyShift action_12
action_22 (18) = happyShift action_13
action_22 (19) = happyShift action_14
action_22 (22) = happyShift action_15
action_22 (31) = happyShift action_16
action_22 (33) = happyShift action_17
action_22 (10) = happyGoto action_43
action_22 _ = happyFail

action_23 (14) = happyShift action_11
action_23 (15) = happyShift action_12
action_23 (18) = happyShift action_13
action_23 (19) = happyShift action_14
action_23 (22) = happyShift action_15
action_23 (31) = happyShift action_16
action_23 (33) = happyShift action_17
action_23 (10) = happyGoto action_42
action_23 _ = happyFail

action_24 (14) = happyShift action_11
action_24 (15) = happyShift action_12
action_24 (18) = happyShift action_13
action_24 (19) = happyShift action_14
action_24 (22) = happyShift action_15
action_24 (31) = happyShift action_16
action_24 (33) = happyShift action_17
action_24 (9) = happyGoto action_41
action_24 (10) = happyGoto action_9
action_24 _ = happyFail

action_25 (14) = happyShift action_11
action_25 (15) = happyShift action_12
action_25 (18) = happyShift action_13
action_25 (19) = happyShift action_14
action_25 (22) = happyShift action_15
action_25 (31) = happyShift action_16
action_25 (33) = happyShift action_17
action_25 (9) = happyGoto action_40
action_25 (10) = happyGoto action_9
action_25 _ = happyFail

action_26 (14) = happyShift action_11
action_26 (15) = happyShift action_12
action_26 (18) = happyShift action_13
action_26 (19) = happyShift action_14
action_26 (22) = happyShift action_15
action_26 (31) = happyShift action_16
action_26 (33) = happyShift action_17
action_26 (8) = happyGoto action_39
action_26 (9) = happyGoto action_8
action_26 (10) = happyGoto action_9
action_26 _ = happyFail

action_27 (14) = happyShift action_11
action_27 (15) = happyShift action_12
action_27 (18) = happyShift action_13
action_27 (19) = happyShift action_14
action_27 (22) = happyShift action_15
action_27 (31) = happyShift action_16
action_27 (33) = happyShift action_17
action_27 (8) = happyGoto action_38
action_27 (9) = happyGoto action_8
action_27 (10) = happyGoto action_9
action_27 _ = happyFail

action_28 (14) = happyShift action_11
action_28 (15) = happyShift action_12
action_28 (18) = happyShift action_13
action_28 (19) = happyShift action_14
action_28 (22) = happyShift action_15
action_28 (31) = happyShift action_16
action_28 (33) = happyShift action_17
action_28 (8) = happyGoto action_37
action_28 (9) = happyGoto action_8
action_28 (10) = happyGoto action_9
action_28 _ = happyFail

action_29 (14) = happyShift action_11
action_29 (15) = happyShift action_12
action_29 (18) = happyShift action_13
action_29 (19) = happyShift action_14
action_29 (22) = happyShift action_15
action_29 (31) = happyShift action_16
action_29 (33) = happyShift action_17
action_29 (8) = happyGoto action_36
action_29 (9) = happyGoto action_8
action_29 (10) = happyGoto action_9
action_29 _ = happyFail

action_30 (14) = happyShift action_11
action_30 (15) = happyShift action_12
action_30 (18) = happyShift action_13
action_30 (19) = happyShift action_14
action_30 (22) = happyShift action_15
action_30 (31) = happyShift action_16
action_30 (33) = happyShift action_17
action_30 (7) = happyGoto action_35
action_30 (8) = happyGoto action_7
action_30 (9) = happyGoto action_8
action_30 (10) = happyGoto action_9
action_30 _ = happyFail

action_31 (14) = happyShift action_11
action_31 (15) = happyShift action_12
action_31 (18) = happyShift action_13
action_31 (19) = happyShift action_14
action_31 (22) = happyShift action_15
action_31 (31) = happyShift action_16
action_31 (33) = happyShift action_17
action_31 (6) = happyGoto action_34
action_31 (7) = happyGoto action_6
action_31 (8) = happyGoto action_7
action_31 (9) = happyGoto action_8
action_31 (10) = happyGoto action_9
action_31 _ = happyFail

action_32 (20) = happyShift action_33
action_32 _ = happyFail

action_33 (11) = happyShift action_10
action_33 (14) = happyShift action_11
action_33 (15) = happyShift action_12
action_33 (16) = happyShift action_2
action_33 (18) = happyShift action_13
action_33 (19) = happyShift action_14
action_33 (22) = happyShift action_15
action_33 (31) = happyShift action_16
action_33 (33) = happyShift action_17
action_33 (4) = happyGoto action_47
action_33 (5) = happyGoto action_4
action_33 (6) = happyGoto action_5
action_33 (7) = happyGoto action_6
action_33 (8) = happyGoto action_7
action_33 (9) = happyGoto action_8
action_33 (10) = happyGoto action_9
action_33 _ = happyFail

action_34 (30) = happyShift action_30
action_34 _ = happyReduce_4

action_35 (25) = happyShift action_26
action_35 (26) = happyShift action_27
action_35 (27) = happyShift action_28
action_35 (29) = happyShift action_29
action_35 _ = happyReduce_6

action_36 (21) = happyShift action_24
action_36 (22) = happyShift action_25
action_36 _ = happyReduce_8

action_37 (17) = happyReduce_12
action_37 (21) = happyShift action_24
action_37 (22) = happyShift action_25
action_37 (25) = happyReduce_12
action_37 (26) = happyReduce_12
action_37 (27) = happyReduce_12
action_37 (29) = happyReduce_12
action_37 (30) = happyReduce_12
action_37 (32) = happyReduce_12
action_37 (34) = happyReduce_12
action_37 (35) = happyReduce_12
action_37 _ = happyReduce_12

action_38 (21) = happyShift action_24
action_38 (22) = happyShift action_25
action_38 _ = happyReduce_10

action_39 (21) = happyShift action_24
action_39 (22) = happyShift action_25
action_39 _ = happyReduce_9

action_40 (23) = happyShift action_22
action_40 (24) = happyShift action_23
action_40 _ = happyReduce_15

action_41 (23) = happyShift action_22
action_41 (24) = happyShift action_23
action_41 _ = happyReduce_14

action_42 _ = happyReduce_18

action_43 _ = happyReduce_17

action_44 (34) = happyShift action_46
action_44 _ = happyFail

action_45 _ = happyReduce_26

action_46 (11) = happyShift action_10
action_46 (14) = happyShift action_11
action_46 (15) = happyShift action_12
action_46 (16) = happyShift action_2
action_46 (18) = happyShift action_13
action_46 (19) = happyShift action_14
action_46 (22) = happyShift action_15
action_46 (31) = happyShift action_16
action_46 (33) = happyShift action_17
action_46 (4) = happyGoto action_49
action_46 (5) = happyGoto action_4
action_46 (6) = happyGoto action_5
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 (9) = happyGoto action_8
action_46 (10) = happyGoto action_9
action_46 _ = happyFail

action_47 (17) = happyShift action_48
action_47 _ = happyFail

action_48 (11) = happyShift action_10
action_48 (14) = happyShift action_11
action_48 (15) = happyShift action_12
action_48 (16) = happyShift action_2
action_48 (18) = happyShift action_13
action_48 (19) = happyShift action_14
action_48 (22) = happyShift action_15
action_48 (31) = happyShift action_16
action_48 (33) = happyShift action_17
action_48 (4) = happyGoto action_51
action_48 (5) = happyGoto action_4
action_48 (6) = happyGoto action_5
action_48 (7) = happyGoto action_6
action_48 (8) = happyGoto action_7
action_48 (9) = happyGoto action_8
action_48 (10) = happyGoto action_9
action_48 _ = happyFail

action_49 (17) = happyShift action_50
action_49 _ = happyFail

action_50 (13) = happyShift action_52
action_50 _ = happyFail

action_51 _ = happyReduce_1

action_52 (11) = happyShift action_10
action_52 (14) = happyShift action_11
action_52 (15) = happyShift action_12
action_52 (16) = happyShift action_2
action_52 (18) = happyShift action_13
action_52 (19) = happyShift action_14
action_52 (22) = happyShift action_15
action_52 (31) = happyShift action_16
action_52 (33) = happyShift action_17
action_52 (4) = happyGoto action_53
action_52 (5) = happyGoto action_4
action_52 (6) = happyGoto action_5
action_52 (7) = happyGoto action_6
action_52 (8) = happyGoto action_7
action_52 (9) = happyGoto action_8
action_52 (10) = happyGoto action_9
action_52 _ = happyFail

action_53 _ = happyReduce_2

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Declare happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 8 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Binary Or happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary And happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary EQ happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary LT happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary GT happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary LE happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary GE happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary Mul happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 (HappyTerminal (Digits happy_var_1))
	 =  HappyAbsSyn10
		 (Literal (IntV happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn10
		 (Literal (BoolV True)
	)

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn10
		 (Literal (BoolV False)
	)

happyReduce_23 = happySpecReduce_2  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Unary Neg happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Unary Not happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn10
		 (Variable happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  10 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "if" -> cont 11;
	TokenKeyword "then" -> cont 12;
	TokenKeyword "else" -> cont 13;
	TokenKeyword "true" -> cont 14;
	TokenKeyword "false" -> cont 15;
	TokenKeyword "var" -> cont 16;
	Symbol ";" -> cont 17;
	TokenIdent happy_dollar_dollar -> cont 18;
	Digits happy_dollar_dollar -> cont 19;
	Symbol "=" -> cont 20;
	Symbol "+" -> cont 21;
	Symbol "-" -> cont 22;
	Symbol "*" -> cont 23;
	Symbol "/" -> cont 24;
	Symbol "<" -> cont 25;
	Symbol ">" -> cont 26;
	Symbol "<=" -> cont 27;
	Symbol ">=" -> cont 28;
	Symbol "==" -> cont 29;
	Symbol "&&" -> cont 30;
	Symbol "!" -> cont 31;
	Symbol "||" -> cont 32;
	Symbol "(" -> cont 33;
	Symbol ")" -> cont 34;
	_ -> happyError' (tk:tks)
	}

happyError_ 35 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


symbols = ["+", "-", "*", "/", "(", ")", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!"]
keywords = ["var", "if", "else", "true", "false"]
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
