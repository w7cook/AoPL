{-# OPTIONS_GHC -w #-}
module TopLevelFunctionsParse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Value
import Operators
import TopLevelFunctions
import Lexer
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
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
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (16) = happyShift action_12
action_2 (20) = happyShift action_13
action_2 (22) = happyShift action_14
action_2 (23) = happyShift action_15
action_2 (24) = happyShift action_16
action_2 (26) = happyShift action_17
action_2 (27) = happyShift action_18
action_2 (30) = happyShift action_19
action_2 (39) = happyShift action_20
action_2 (41) = happyShift action_21
action_2 (6) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (9) = happyGoto action_6
action_2 (10) = happyGoto action_7
action_2 (11) = happyGoto action_8
action_2 (12) = happyGoto action_9
action_2 (13) = happyGoto action_10
action_2 (14) = happyGoto action_11
action_2 _ = happyFail

action_3 (43) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_2

action_5 _ = happyReduce_1

action_6 (40) = happyShift action_39
action_6 _ = happyReduce_10

action_7 (38) = happyShift action_38
action_7 _ = happyReduce_12

action_8 (33) = happyShift action_33
action_8 (34) = happyShift action_34
action_8 (35) = happyShift action_35
action_8 (36) = happyShift action_36
action_8 (37) = happyShift action_37
action_8 _ = happyReduce_14

action_9 (29) = happyShift action_31
action_9 (30) = happyShift action_32
action_9 _ = happyReduce_20

action_10 (31) = happyShift action_29
action_10 (32) = happyShift action_30
action_10 _ = happyReduce_23

action_11 _ = happyReduce_26

action_12 (26) = happyShift action_28
action_12 _ = happyFail

action_13 (41) = happyShift action_27
action_13 _ = happyFail

action_14 _ = happyReduce_28

action_15 _ = happyReduce_29

action_16 (26) = happyShift action_26
action_16 _ = happyFail

action_17 (41) = happyShift action_25
action_17 _ = happyReduce_32

action_18 _ = happyReduce_27

action_19 (22) = happyShift action_14
action_19 (23) = happyShift action_15
action_19 (26) = happyShift action_17
action_19 (27) = happyShift action_18
action_19 (30) = happyShift action_19
action_19 (39) = happyShift action_20
action_19 (41) = happyShift action_21
action_19 (14) = happyGoto action_24
action_19 _ = happyFail

action_20 (22) = happyShift action_14
action_20 (23) = happyShift action_15
action_20 (26) = happyShift action_17
action_20 (27) = happyShift action_18
action_20 (30) = happyShift action_19
action_20 (39) = happyShift action_20
action_20 (41) = happyShift action_21
action_20 (14) = happyGoto action_23
action_20 _ = happyFail

action_21 (20) = happyShift action_13
action_21 (22) = happyShift action_14
action_21 (23) = happyShift action_15
action_21 (24) = happyShift action_16
action_21 (26) = happyShift action_17
action_21 (27) = happyShift action_18
action_21 (30) = happyShift action_19
action_21 (39) = happyShift action_20
action_21 (41) = happyShift action_21
action_21 (8) = happyGoto action_22
action_21 (9) = happyGoto action_6
action_21 (10) = happyGoto action_7
action_21 (11) = happyGoto action_8
action_21 (12) = happyGoto action_9
action_21 (13) = happyGoto action_10
action_21 (14) = happyGoto action_11
action_21 _ = happyFail

action_22 (42) = happyShift action_56
action_22 _ = happyFail

action_23 _ = happyReduce_31

action_24 _ = happyReduce_30

action_25 (20) = happyShift action_13
action_25 (22) = happyShift action_14
action_25 (23) = happyShift action_15
action_25 (24) = happyShift action_16
action_25 (26) = happyShift action_17
action_25 (27) = happyShift action_18
action_25 (30) = happyShift action_19
action_25 (39) = happyShift action_20
action_25 (41) = happyShift action_21
action_25 (8) = happyGoto action_54
action_25 (9) = happyGoto action_6
action_25 (10) = happyGoto action_7
action_25 (11) = happyGoto action_8
action_25 (12) = happyGoto action_9
action_25 (13) = happyGoto action_10
action_25 (14) = happyGoto action_11
action_25 (15) = happyGoto action_55
action_25 _ = happyReduce_37

action_26 (28) = happyShift action_53
action_26 _ = happyFail

action_27 (20) = happyShift action_13
action_27 (22) = happyShift action_14
action_27 (23) = happyShift action_15
action_27 (24) = happyShift action_16
action_27 (26) = happyShift action_17
action_27 (27) = happyShift action_18
action_27 (30) = happyShift action_19
action_27 (39) = happyShift action_20
action_27 (41) = happyShift action_21
action_27 (8) = happyGoto action_52
action_27 (9) = happyGoto action_6
action_27 (10) = happyGoto action_7
action_27 (11) = happyGoto action_8
action_27 (12) = happyGoto action_9
action_27 (13) = happyGoto action_10
action_27 (14) = happyGoto action_11
action_27 _ = happyFail

action_28 (41) = happyShift action_51
action_28 _ = happyFail

action_29 (22) = happyShift action_14
action_29 (23) = happyShift action_15
action_29 (26) = happyShift action_17
action_29 (27) = happyShift action_18
action_29 (30) = happyShift action_19
action_29 (39) = happyShift action_20
action_29 (41) = happyShift action_21
action_29 (14) = happyGoto action_50
action_29 _ = happyFail

action_30 (22) = happyShift action_14
action_30 (23) = happyShift action_15
action_30 (26) = happyShift action_17
action_30 (27) = happyShift action_18
action_30 (30) = happyShift action_19
action_30 (39) = happyShift action_20
action_30 (41) = happyShift action_21
action_30 (14) = happyGoto action_49
action_30 _ = happyFail

action_31 (22) = happyShift action_14
action_31 (23) = happyShift action_15
action_31 (26) = happyShift action_17
action_31 (27) = happyShift action_18
action_31 (30) = happyShift action_19
action_31 (39) = happyShift action_20
action_31 (41) = happyShift action_21
action_31 (13) = happyGoto action_48
action_31 (14) = happyGoto action_11
action_31 _ = happyFail

action_32 (22) = happyShift action_14
action_32 (23) = happyShift action_15
action_32 (26) = happyShift action_17
action_32 (27) = happyShift action_18
action_32 (30) = happyShift action_19
action_32 (39) = happyShift action_20
action_32 (41) = happyShift action_21
action_32 (13) = happyGoto action_47
action_32 (14) = happyGoto action_11
action_32 _ = happyFail

action_33 (22) = happyShift action_14
action_33 (23) = happyShift action_15
action_33 (26) = happyShift action_17
action_33 (27) = happyShift action_18
action_33 (30) = happyShift action_19
action_33 (39) = happyShift action_20
action_33 (41) = happyShift action_21
action_33 (12) = happyGoto action_46
action_33 (13) = happyGoto action_10
action_33 (14) = happyGoto action_11
action_33 _ = happyFail

action_34 (22) = happyShift action_14
action_34 (23) = happyShift action_15
action_34 (26) = happyShift action_17
action_34 (27) = happyShift action_18
action_34 (30) = happyShift action_19
action_34 (39) = happyShift action_20
action_34 (41) = happyShift action_21
action_34 (12) = happyGoto action_45
action_34 (13) = happyGoto action_10
action_34 (14) = happyGoto action_11
action_34 _ = happyFail

action_35 (22) = happyShift action_14
action_35 (23) = happyShift action_15
action_35 (26) = happyShift action_17
action_35 (27) = happyShift action_18
action_35 (30) = happyShift action_19
action_35 (39) = happyShift action_20
action_35 (41) = happyShift action_21
action_35 (12) = happyGoto action_44
action_35 (13) = happyGoto action_10
action_35 (14) = happyGoto action_11
action_35 _ = happyFail

action_36 (22) = happyShift action_14
action_36 (23) = happyShift action_15
action_36 (26) = happyShift action_17
action_36 (27) = happyShift action_18
action_36 (30) = happyShift action_19
action_36 (39) = happyShift action_20
action_36 (41) = happyShift action_21
action_36 (12) = happyGoto action_43
action_36 (13) = happyGoto action_10
action_36 (14) = happyGoto action_11
action_36 _ = happyFail

action_37 (22) = happyShift action_14
action_37 (23) = happyShift action_15
action_37 (26) = happyShift action_17
action_37 (27) = happyShift action_18
action_37 (30) = happyShift action_19
action_37 (39) = happyShift action_20
action_37 (41) = happyShift action_21
action_37 (12) = happyGoto action_42
action_37 (13) = happyGoto action_10
action_37 (14) = happyGoto action_11
action_37 _ = happyFail

action_38 (22) = happyShift action_14
action_38 (23) = happyShift action_15
action_38 (26) = happyShift action_17
action_38 (27) = happyShift action_18
action_38 (30) = happyShift action_19
action_38 (39) = happyShift action_20
action_38 (41) = happyShift action_21
action_38 (11) = happyGoto action_41
action_38 (12) = happyGoto action_9
action_38 (13) = happyGoto action_10
action_38 (14) = happyGoto action_11
action_38 _ = happyFail

action_39 (22) = happyShift action_14
action_39 (23) = happyShift action_15
action_39 (26) = happyShift action_17
action_39 (27) = happyShift action_18
action_39 (30) = happyShift action_19
action_39 (39) = happyShift action_20
action_39 (41) = happyShift action_21
action_39 (10) = happyGoto action_40
action_39 (11) = happyGoto action_8
action_39 (12) = happyGoto action_9
action_39 (13) = happyGoto action_10
action_39 (14) = happyGoto action_11
action_39 _ = happyFail

action_40 (38) = happyShift action_38
action_40 _ = happyReduce_11

action_41 (33) = happyShift action_33
action_41 (34) = happyShift action_34
action_41 (35) = happyShift action_35
action_41 (36) = happyShift action_36
action_41 (37) = happyShift action_37
action_41 _ = happyReduce_13

action_42 (29) = happyShift action_31
action_42 (30) = happyShift action_32
action_42 _ = happyReduce_15

action_43 (29) = happyShift action_31
action_43 (30) = happyShift action_32
action_43 _ = happyReduce_19

action_44 (29) = happyShift action_31
action_44 (30) = happyShift action_32
action_44 _ = happyReduce_18

action_45 (29) = happyShift action_31
action_45 (30) = happyShift action_32
action_45 _ = happyReduce_17

action_46 (29) = happyShift action_31
action_46 (30) = happyShift action_32
action_46 _ = happyReduce_16

action_47 (31) = happyShift action_29
action_47 (32) = happyShift action_30
action_47 _ = happyReduce_22

action_48 (31) = happyShift action_29
action_48 (32) = happyShift action_30
action_48 _ = happyReduce_21

action_49 _ = happyReduce_25

action_50 _ = happyReduce_24

action_51 (26) = happyShift action_62
action_51 (7) = happyGoto action_61
action_51 _ = happyReduce_7

action_52 (42) = happyShift action_60
action_52 _ = happyFail

action_53 (20) = happyShift action_13
action_53 (22) = happyShift action_14
action_53 (23) = happyShift action_15
action_53 (24) = happyShift action_16
action_53 (26) = happyShift action_17
action_53 (27) = happyShift action_18
action_53 (30) = happyShift action_19
action_53 (39) = happyShift action_20
action_53 (41) = happyShift action_21
action_53 (8) = happyGoto action_59
action_53 (9) = happyGoto action_6
action_53 (10) = happyGoto action_7
action_53 (11) = happyGoto action_8
action_53 (12) = happyGoto action_9
action_53 (13) = happyGoto action_10
action_53 (14) = happyGoto action_11
action_53 _ = happyFail

action_54 _ = happyReduce_36

action_55 (17) = happyShift action_57
action_55 (42) = happyShift action_58
action_55 _ = happyFail

action_56 _ = happyReduce_34

action_57 (20) = happyShift action_13
action_57 (22) = happyShift action_14
action_57 (23) = happyShift action_15
action_57 (24) = happyShift action_16
action_57 (26) = happyShift action_17
action_57 (27) = happyShift action_18
action_57 (30) = happyShift action_19
action_57 (39) = happyShift action_20
action_57 (41) = happyShift action_21
action_57 (8) = happyGoto action_67
action_57 (9) = happyGoto action_6
action_57 (10) = happyGoto action_7
action_57 (11) = happyGoto action_8
action_57 (12) = happyGoto action_9
action_57 (13) = happyGoto action_10
action_57 (14) = happyGoto action_11
action_57 _ = happyFail

action_58 _ = happyReduce_33

action_59 (25) = happyShift action_66
action_59 _ = happyFail

action_60 (20) = happyShift action_13
action_60 (22) = happyShift action_14
action_60 (23) = happyShift action_15
action_60 (24) = happyShift action_16
action_60 (26) = happyShift action_17
action_60 (27) = happyShift action_18
action_60 (30) = happyShift action_19
action_60 (39) = happyShift action_20
action_60 (41) = happyShift action_21
action_60 (8) = happyGoto action_65
action_60 (9) = happyGoto action_6
action_60 (10) = happyGoto action_7
action_60 (11) = happyGoto action_8
action_60 (12) = happyGoto action_9
action_60 (13) = happyGoto action_10
action_60 (14) = happyGoto action_11
action_60 _ = happyFail

action_61 (17) = happyShift action_63
action_61 (42) = happyShift action_64
action_61 _ = happyFail

action_62 _ = happyReduce_6

action_63 (26) = happyShift action_71
action_63 _ = happyFail

action_64 (18) = happyShift action_70
action_64 _ = happyFail

action_65 (21) = happyShift action_69
action_65 _ = happyFail

action_66 (20) = happyShift action_13
action_66 (22) = happyShift action_14
action_66 (23) = happyShift action_15
action_66 (24) = happyShift action_16
action_66 (26) = happyShift action_17
action_66 (27) = happyShift action_18
action_66 (30) = happyShift action_19
action_66 (39) = happyShift action_20
action_66 (41) = happyShift action_21
action_66 (8) = happyGoto action_68
action_66 (9) = happyGoto action_6
action_66 (10) = happyGoto action_7
action_66 (11) = happyGoto action_8
action_66 (12) = happyGoto action_9
action_66 (13) = happyGoto action_10
action_66 (14) = happyGoto action_11
action_66 _ = happyFail

action_67 _ = happyReduce_35

action_68 _ = happyReduce_8

action_69 (20) = happyShift action_13
action_69 (22) = happyShift action_14
action_69 (23) = happyShift action_15
action_69 (24) = happyShift action_16
action_69 (26) = happyShift action_17
action_69 (27) = happyShift action_18
action_69 (30) = happyShift action_19
action_69 (39) = happyShift action_20
action_69 (41) = happyShift action_21
action_69 (8) = happyGoto action_73
action_69 (9) = happyGoto action_6
action_69 (10) = happyGoto action_7
action_69 (11) = happyGoto action_8
action_69 (12) = happyGoto action_9
action_69 (13) = happyGoto action_10
action_69 (14) = happyGoto action_11
action_69 _ = happyFail

action_70 (20) = happyShift action_13
action_70 (22) = happyShift action_14
action_70 (23) = happyShift action_15
action_70 (24) = happyShift action_16
action_70 (26) = happyShift action_17
action_70 (27) = happyShift action_18
action_70 (30) = happyShift action_19
action_70 (39) = happyShift action_20
action_70 (41) = happyShift action_21
action_70 (8) = happyGoto action_72
action_70 (9) = happyGoto action_6
action_70 (10) = happyGoto action_7
action_70 (11) = happyGoto action_8
action_70 (12) = happyGoto action_9
action_70 (13) = happyGoto action_10
action_70 (14) = happyGoto action_11
action_70 _ = happyFail

action_71 _ = happyReduce_5

action_72 (19) = happyShift action_74
action_72 _ = happyFail

action_73 _ = happyReduce_9

action_74 _ = happyReduce_4

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happyReduce 8 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, Function happy_var_4 happy_var_7)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyTerminal (TokenIdent happy_var_3))
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happyReduce 6 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Declare happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 7 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (If happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Binary Or happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Binary And happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary EQ happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary LT happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary GT happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary LE happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Binary GE happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Binary Mul happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyTerminal (Digits happy_var_1))
	 =  HappyAbsSyn14
		 (Literal (IntV happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn14
		 (Literal (BoolV True)
	)

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn14
		 (Literal (BoolV False)
	)

happyReduce_30 = happySpecReduce_2  14 happyReduction_30
happyReduction_30 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (Unary Neg happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  14 happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (Unary Not happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  14 happyReduction_32
happyReduction_32 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn14
		 (Variable happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 14 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_3  14 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  15 happyReduction_35
happyReduction_35 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  15 happyReduction_37
happyReduction_37  =  HappyAbsSyn15
		 ([]
	)

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "function" -> cont 16;
	Symbol "," -> cont 17;
	Symbol "{" -> cont 18;
	Symbol "}" -> cont 19;
	TokenKeyword "if" -> cont 20;
	TokenKeyword "else" -> cont 21;
	TokenKeyword "true" -> cont 22;
	TokenKeyword "false" -> cont 23;
	TokenKeyword "var" -> cont 24;
	Symbol ";" -> cont 25;
	TokenIdent happy_dollar_dollar -> cont 26;
	Digits happy_dollar_dollar -> cont 27;
	Symbol "=" -> cont 28;
	Symbol "+" -> cont 29;
	Symbol "-" -> cont 30;
	Symbol "*" -> cont 31;
	Symbol "/" -> cont 32;
	Symbol "<" -> cont 33;
	Symbol ">" -> cont 34;
	Symbol "<=" -> cont 35;
	Symbol ">=" -> cont 36;
	Symbol "==" -> cont 37;
	Symbol "&&" -> cont 38;
	Symbol "!" -> cont 39;
	Symbol "||" -> cont 40;
	Symbol "(" -> cont 41;
	Symbol ")" -> cont 42;
	_ -> happyError' (tk:tks)
	}

happyError_ 43 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
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


symbols = ["{", "}", ",", "+", "-", "*", "/", "(", ")", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!"]
keywords = ["function", "var", "if", "else", "true", "false"]
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/Library/Frameworks/GHC.framework/Versions/7.10.3-x86_64/usr/lib/ghc-7.10.3/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










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


{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
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
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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

