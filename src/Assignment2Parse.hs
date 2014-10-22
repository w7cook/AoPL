{-# OPTIONS_GHC -w #-}
module Assignment2Parse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Assignment2
import Lexer

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

action_0 (14) = happyShift action_2
action_0 (15) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (19) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (23) = happyShift action_15
action_0 (26) = happyShift action_16
action_0 (35) = happyShift action_17
action_0 (37) = happyShift action_18
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 _ = happyFail

action_1 (14) = happyShift action_2
action_1 _ = happyFail

action_2 (37) = happyShift action_39
action_2 _ = happyFail

action_3 (41) = happyAccept
action_3 _ = happyFail

action_4 (36) = happyShift action_38
action_4 _ = happyReduce_4

action_5 (34) = happyShift action_37
action_5 _ = happyReduce_11

action_6 (29) = happyShift action_32
action_6 (30) = happyShift action_33
action_6 (31) = happyShift action_34
action_6 (32) = happyShift action_35
action_6 (33) = happyShift action_36
action_6 _ = happyReduce_13

action_7 (25) = happyShift action_30
action_7 (26) = happyShift action_31
action_7 _ = happyReduce_19

action_8 (27) = happyShift action_28
action_8 (28) = happyShift action_29
action_8 _ = happyReduce_22

action_9 (37) = happyShift action_27
action_9 _ = happyReduce_25

action_10 (37) = happyShift action_26
action_10 _ = happyFail

action_11 _ = happyReduce_28

action_12 _ = happyReduce_29

action_13 (22) = happyShift action_24
action_13 (37) = happyShift action_25
action_13 (5) = happyGoto action_23
action_13 _ = happyFail

action_14 _ = happyReduce_32

action_15 _ = happyReduce_27

action_16 (17) = happyShift action_11
action_16 (18) = happyShift action_12
action_16 (22) = happyShift action_14
action_16 (23) = happyShift action_15
action_16 (26) = happyShift action_16
action_16 (35) = happyShift action_17
action_16 (37) = happyShift action_18
action_16 (12) = happyGoto action_22
action_16 _ = happyFail

action_17 (17) = happyShift action_11
action_17 (18) = happyShift action_12
action_17 (22) = happyShift action_14
action_17 (23) = happyShift action_15
action_17 (26) = happyShift action_16
action_17 (35) = happyShift action_17
action_17 (37) = happyShift action_18
action_17 (12) = happyGoto action_21
action_17 _ = happyFail

action_18 (14) = happyShift action_2
action_18 (15) = happyShift action_10
action_18 (17) = happyShift action_11
action_18 (18) = happyShift action_12
action_18 (19) = happyShift action_13
action_18 (22) = happyShift action_14
action_18 (23) = happyShift action_15
action_18 (26) = happyShift action_16
action_18 (35) = happyShift action_17
action_18 (37) = happyShift action_18
action_18 (4) = happyGoto action_19
action_18 (7) = happyGoto action_4
action_18 (8) = happyGoto action_5
action_18 (9) = happyGoto action_6
action_18 (10) = happyGoto action_7
action_18 (11) = happyGoto action_8
action_18 (12) = happyGoto action_9
action_18 (13) = happyGoto action_20
action_18 _ = happyFail

action_19 (38) = happyShift action_61
action_19 _ = happyReduce_35

action_20 (21) = happyShift action_59
action_20 (38) = happyShift action_60
action_20 _ = happyFail

action_21 (37) = happyShift action_27
action_21 _ = happyReduce_31

action_22 (37) = happyShift action_27
action_22 _ = happyReduce_30

action_23 (24) = happyShift action_58
action_23 _ = happyFail

action_24 _ = happyReduce_5

action_25 (22) = happyShift action_24
action_25 (37) = happyShift action_25
action_25 (5) = happyGoto action_56
action_25 (6) = happyGoto action_57
action_25 _ = happyFail

action_26 (14) = happyShift action_2
action_26 (15) = happyShift action_10
action_26 (17) = happyShift action_11
action_26 (18) = happyShift action_12
action_26 (19) = happyShift action_13
action_26 (22) = happyShift action_14
action_26 (23) = happyShift action_15
action_26 (26) = happyShift action_16
action_26 (35) = happyShift action_17
action_26 (37) = happyShift action_18
action_26 (4) = happyGoto action_55
action_26 (7) = happyGoto action_4
action_26 (8) = happyGoto action_5
action_26 (9) = happyGoto action_6
action_26 (10) = happyGoto action_7
action_26 (11) = happyGoto action_8
action_26 (12) = happyGoto action_9
action_26 _ = happyFail

action_27 (14) = happyShift action_2
action_27 (15) = happyShift action_10
action_27 (17) = happyShift action_11
action_27 (18) = happyShift action_12
action_27 (19) = happyShift action_13
action_27 (22) = happyShift action_14
action_27 (23) = happyShift action_15
action_27 (26) = happyShift action_16
action_27 (35) = happyShift action_17
action_27 (37) = happyShift action_18
action_27 (4) = happyGoto action_53
action_27 (7) = happyGoto action_4
action_27 (8) = happyGoto action_5
action_27 (9) = happyGoto action_6
action_27 (10) = happyGoto action_7
action_27 (11) = happyGoto action_8
action_27 (12) = happyGoto action_9
action_27 (13) = happyGoto action_54
action_27 _ = happyFail

action_28 (17) = happyShift action_11
action_28 (18) = happyShift action_12
action_28 (22) = happyShift action_14
action_28 (23) = happyShift action_15
action_28 (26) = happyShift action_16
action_28 (35) = happyShift action_17
action_28 (37) = happyShift action_18
action_28 (12) = happyGoto action_52
action_28 _ = happyFail

action_29 (17) = happyShift action_11
action_29 (18) = happyShift action_12
action_29 (22) = happyShift action_14
action_29 (23) = happyShift action_15
action_29 (26) = happyShift action_16
action_29 (35) = happyShift action_17
action_29 (37) = happyShift action_18
action_29 (12) = happyGoto action_51
action_29 _ = happyFail

action_30 (17) = happyShift action_11
action_30 (18) = happyShift action_12
action_30 (22) = happyShift action_14
action_30 (23) = happyShift action_15
action_30 (26) = happyShift action_16
action_30 (35) = happyShift action_17
action_30 (37) = happyShift action_18
action_30 (11) = happyGoto action_50
action_30 (12) = happyGoto action_9
action_30 _ = happyFail

action_31 (17) = happyShift action_11
action_31 (18) = happyShift action_12
action_31 (22) = happyShift action_14
action_31 (23) = happyShift action_15
action_31 (26) = happyShift action_16
action_31 (35) = happyShift action_17
action_31 (37) = happyShift action_18
action_31 (11) = happyGoto action_49
action_31 (12) = happyGoto action_9
action_31 _ = happyFail

action_32 (17) = happyShift action_11
action_32 (18) = happyShift action_12
action_32 (22) = happyShift action_14
action_32 (23) = happyShift action_15
action_32 (26) = happyShift action_16
action_32 (35) = happyShift action_17
action_32 (37) = happyShift action_18
action_32 (10) = happyGoto action_48
action_32 (11) = happyGoto action_8
action_32 (12) = happyGoto action_9
action_32 _ = happyFail

action_33 (17) = happyShift action_11
action_33 (18) = happyShift action_12
action_33 (22) = happyShift action_14
action_33 (23) = happyShift action_15
action_33 (26) = happyShift action_16
action_33 (35) = happyShift action_17
action_33 (37) = happyShift action_18
action_33 (10) = happyGoto action_47
action_33 (11) = happyGoto action_8
action_33 (12) = happyGoto action_9
action_33 _ = happyFail

action_34 (17) = happyShift action_11
action_34 (18) = happyShift action_12
action_34 (22) = happyShift action_14
action_34 (23) = happyShift action_15
action_34 (26) = happyShift action_16
action_34 (35) = happyShift action_17
action_34 (37) = happyShift action_18
action_34 (10) = happyGoto action_46
action_34 (11) = happyGoto action_8
action_34 (12) = happyGoto action_9
action_34 _ = happyFail

action_35 (17) = happyShift action_11
action_35 (18) = happyShift action_12
action_35 (22) = happyShift action_14
action_35 (23) = happyShift action_15
action_35 (26) = happyShift action_16
action_35 (35) = happyShift action_17
action_35 (37) = happyShift action_18
action_35 (10) = happyGoto action_45
action_35 (11) = happyGoto action_8
action_35 (12) = happyGoto action_9
action_35 _ = happyFail

action_36 (17) = happyShift action_11
action_36 (18) = happyShift action_12
action_36 (22) = happyShift action_14
action_36 (23) = happyShift action_15
action_36 (26) = happyShift action_16
action_36 (35) = happyShift action_17
action_36 (37) = happyShift action_18
action_36 (10) = happyGoto action_44
action_36 (11) = happyGoto action_8
action_36 (12) = happyGoto action_9
action_36 _ = happyFail

action_37 (17) = happyShift action_11
action_37 (18) = happyShift action_12
action_37 (22) = happyShift action_14
action_37 (23) = happyShift action_15
action_37 (26) = happyShift action_16
action_37 (35) = happyShift action_17
action_37 (37) = happyShift action_18
action_37 (9) = happyGoto action_43
action_37 (10) = happyGoto action_7
action_37 (11) = happyGoto action_8
action_37 (12) = happyGoto action_9
action_37 _ = happyFail

action_38 (17) = happyShift action_11
action_38 (18) = happyShift action_12
action_38 (22) = happyShift action_14
action_38 (23) = happyShift action_15
action_38 (26) = happyShift action_16
action_38 (35) = happyShift action_17
action_38 (37) = happyShift action_18
action_38 (8) = happyGoto action_42
action_38 (9) = happyGoto action_6
action_38 (10) = happyGoto action_7
action_38 (11) = happyGoto action_8
action_38 (12) = happyGoto action_9
action_38 _ = happyFail

action_39 (22) = happyShift action_24
action_39 (37) = happyShift action_25
action_39 (5) = happyGoto action_40
action_39 (6) = happyGoto action_41
action_39 _ = happyFail

action_40 _ = happyReduce_8

action_41 (21) = happyShift action_64
action_41 (38) = happyShift action_69
action_41 _ = happyFail

action_42 (34) = happyShift action_37
action_42 _ = happyReduce_10

action_43 (29) = happyShift action_32
action_43 (30) = happyShift action_33
action_43 (31) = happyShift action_34
action_43 (32) = happyShift action_35
action_43 (33) = happyShift action_36
action_43 _ = happyReduce_12

action_44 (25) = happyShift action_30
action_44 (26) = happyShift action_31
action_44 _ = happyReduce_14

action_45 (25) = happyShift action_30
action_45 (26) = happyShift action_31
action_45 _ = happyReduce_18

action_46 (25) = happyShift action_30
action_46 (26) = happyShift action_31
action_46 _ = happyReduce_17

action_47 (25) = happyShift action_30
action_47 (26) = happyShift action_31
action_47 _ = happyReduce_16

action_48 (25) = happyShift action_30
action_48 (26) = happyShift action_31
action_48 _ = happyReduce_15

action_49 (27) = happyShift action_28
action_49 (28) = happyShift action_29
action_49 _ = happyReduce_21

action_50 (27) = happyShift action_28
action_50 (28) = happyShift action_29
action_50 _ = happyReduce_20

action_51 (37) = happyShift action_27
action_51 _ = happyReduce_24

action_52 (37) = happyShift action_27
action_52 _ = happyReduce_23

action_53 _ = happyReduce_35

action_54 (21) = happyShift action_59
action_54 (38) = happyShift action_68
action_54 _ = happyFail

action_55 (38) = happyShift action_67
action_55 _ = happyFail

action_56 (38) = happyShift action_66
action_56 _ = happyReduce_8

action_57 (21) = happyShift action_64
action_57 (38) = happyShift action_65
action_57 _ = happyFail

action_58 (14) = happyShift action_2
action_58 (15) = happyShift action_10
action_58 (17) = happyShift action_11
action_58 (18) = happyShift action_12
action_58 (19) = happyShift action_13
action_58 (22) = happyShift action_14
action_58 (23) = happyShift action_15
action_58 (26) = happyShift action_16
action_58 (35) = happyShift action_17
action_58 (37) = happyShift action_18
action_58 (4) = happyGoto action_63
action_58 (7) = happyGoto action_4
action_58 (8) = happyGoto action_5
action_58 (9) = happyGoto action_6
action_58 (10) = happyGoto action_7
action_58 (11) = happyGoto action_8
action_58 (12) = happyGoto action_9
action_58 _ = happyFail

action_59 (14) = happyShift action_2
action_59 (15) = happyShift action_10
action_59 (17) = happyShift action_11
action_59 (18) = happyShift action_12
action_59 (19) = happyShift action_13
action_59 (22) = happyShift action_14
action_59 (23) = happyShift action_15
action_59 (26) = happyShift action_16
action_59 (35) = happyShift action_17
action_59 (37) = happyShift action_18
action_59 (4) = happyGoto action_62
action_59 (7) = happyGoto action_4
action_59 (8) = happyGoto action_5
action_59 (9) = happyGoto action_6
action_59 (10) = happyGoto action_7
action_59 (11) = happyGoto action_8
action_59 (12) = happyGoto action_9
action_59 _ = happyFail

action_60 _ = happyReduce_34

action_61 _ = happyReduce_33

action_62 _ = happyReduce_36

action_63 (20) = happyShift action_73
action_63 _ = happyFail

action_64 (22) = happyShift action_24
action_64 (37) = happyShift action_25
action_64 (5) = happyGoto action_72
action_64 _ = happyFail

action_65 _ = happyReduce_7

action_66 _ = happyReduce_6

action_67 (14) = happyShift action_2
action_67 (15) = happyShift action_10
action_67 (17) = happyShift action_11
action_67 (18) = happyShift action_12
action_67 (19) = happyShift action_13
action_67 (22) = happyShift action_14
action_67 (23) = happyShift action_15
action_67 (26) = happyShift action_16
action_67 (35) = happyShift action_17
action_67 (37) = happyShift action_18
action_67 (4) = happyGoto action_71
action_67 (7) = happyGoto action_4
action_67 (8) = happyGoto action_5
action_67 (9) = happyGoto action_6
action_67 (10) = happyGoto action_7
action_67 (11) = happyGoto action_8
action_67 (12) = happyGoto action_9
action_67 _ = happyFail

action_68 _ = happyReduce_26

action_69 (39) = happyShift action_70
action_69 _ = happyFail

action_70 (14) = happyShift action_2
action_70 (15) = happyShift action_10
action_70 (17) = happyShift action_11
action_70 (18) = happyShift action_12
action_70 (19) = happyShift action_13
action_70 (22) = happyShift action_14
action_70 (23) = happyShift action_15
action_70 (26) = happyShift action_16
action_70 (35) = happyShift action_17
action_70 (37) = happyShift action_18
action_70 (4) = happyGoto action_76
action_70 (7) = happyGoto action_4
action_70 (8) = happyGoto action_5
action_70 (9) = happyGoto action_6
action_70 (10) = happyGoto action_7
action_70 (11) = happyGoto action_8
action_70 (12) = happyGoto action_9
action_70 _ = happyFail

action_71 (20) = happyShift action_75
action_71 _ = happyFail

action_72 _ = happyReduce_9

action_73 (14) = happyShift action_2
action_73 (15) = happyShift action_10
action_73 (17) = happyShift action_11
action_73 (18) = happyShift action_12
action_73 (19) = happyShift action_13
action_73 (22) = happyShift action_14
action_73 (23) = happyShift action_15
action_73 (26) = happyShift action_16
action_73 (35) = happyShift action_17
action_73 (37) = happyShift action_18
action_73 (4) = happyGoto action_74
action_73 (7) = happyGoto action_4
action_73 (8) = happyGoto action_5
action_73 (9) = happyGoto action_6
action_73 (10) = happyGoto action_7
action_73 (11) = happyGoto action_8
action_73 (12) = happyGoto action_9
action_73 _ = happyFail

action_74 _ = happyReduce_2

action_75 (16) = happyShift action_78
action_75 _ = happyFail

action_76 (40) = happyShift action_77
action_76 _ = happyFail

action_77 _ = happyReduce_1

action_78 (14) = happyShift action_2
action_78 (15) = happyShift action_10
action_78 (17) = happyShift action_11
action_78 (18) = happyShift action_12
action_78 (19) = happyShift action_13
action_78 (22) = happyShift action_14
action_78 (23) = happyShift action_15
action_78 (26) = happyShift action_16
action_78 (35) = happyShift action_17
action_78 (37) = happyShift action_18
action_78 (4) = happyGoto action_79
action_78 (7) = happyGoto action_4
action_78 (8) = happyGoto action_5
action_78 (9) = happyGoto action_6
action_78 (10) = happyGoto action_7
action_78 (11) = happyGoto action_8
action_78 (12) = happyGoto action_9
action_78 _ = happyFail

action_79 _ = happyReduce_3

happyReduce_1 = happyReduce 7 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Function (TupleP happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Declare happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 8 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_8) `HappyStk`
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

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn5
		 (VarP happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (TupleP happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Binary Or happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Binary And happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary EQ happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary LT happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary GT happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary LE happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary GE happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary Mul happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  11 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 12 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Call happy_var_1 (Tuple happy_var_3)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 (HappyTerminal (Digits happy_var_1))
	 =  HappyAbsSyn12
		 (Literal (IntV happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn12
		 (Literal (BoolV True)
	)

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn12
		 (Literal (BoolV False)
	)

happyReduce_30 = happySpecReduce_2  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Unary Neg happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  12 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Unary Not happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  12 happyReduction_32
happyReduction_32 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn12
		 (Variable happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  12 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Tuple happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  13 happyReduction_35
happyReduction_35 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  13 happyReduction_36
happyReduction_36 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "function" -> cont 14;
	TokenKeyword "if" -> cont 15;
	TokenKeyword "else" -> cont 16;
	TokenKeyword "true" -> cont 17;
	TokenKeyword "false" -> cont 18;
	TokenKeyword "var" -> cont 19;
	Symbol ";" -> cont 20;
	Symbol "," -> cont 21;
	TokenIdent happy_dollar_dollar -> cont 22;
	Digits happy_dollar_dollar -> cont 23;
	Symbol "=" -> cont 24;
	Symbol "+" -> cont 25;
	Symbol "-" -> cont 26;
	Symbol "*" -> cont 27;
	Symbol "/" -> cont 28;
	Symbol "<" -> cont 29;
	Symbol ">" -> cont 30;
	Symbol "<=" -> cont 31;
	Symbol ">=" -> cont 32;
	Symbol "==" -> cont 33;
	Symbol "&&" -> cont 34;
	Symbol "!" -> cont 35;
	Symbol "||" -> cont 36;
	Symbol "(" -> cont 37;
	Symbol ")" -> cont 38;
	Symbol "{" -> cont 39;
	Symbol "}" -> cont 40;
	_ -> happyError' (tk:tks)
	}

happyError_ 41 tk tks = happyError' tks
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


symbols = ["+", "-", "*", "/", "(", ")", "{", "}", ";", ",", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!"]
keywords = ["function", "var", "if", "else", "true", "false"]
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
