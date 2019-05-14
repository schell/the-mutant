-- | A collection of scan codes your convenience.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Mutant.KeyboardCodes.Scancode where

import           Data.Word (Word32)


-- | Scancodes are codes that correspond to the physical position of a key,
-- independent of the current keyboard layout.
-- Scancodes are meant to be layout-independent. Think of this as \"the user
-- pressed the Q key as it would be on a US QWERTY keyboard\" regardless of
-- whether this is actually a European keyboard or a Dvorak keyboard or
-- whatever. The scancode is always the same key position.
-- 'Scancode' makes sense when you want key presses based on their location on
-- a keyboard - for example, the traditional \"WASD\" layout used in
-- first-person shooters.
newtype Scancode
  = Scancode { unScancode :: Word32 }
  deriving newtype (Eq, Ord, Num, Show)


pattern ScancodeUnknown :: Scancode
pattern ScancodeUnknown = (0) :: Scancode

pattern ScancodeA :: Scancode
pattern ScancodeA = (4) :: Scancode

pattern ScancodeB :: Scancode
pattern ScancodeB = (5) :: Scancode

pattern ScancodeC :: Scancode
pattern ScancodeC = (6) :: Scancode

pattern ScancodeD :: Scancode
pattern ScancodeD = (7) :: Scancode

pattern ScancodeE :: Scancode
pattern ScancodeE = (8) :: Scancode

pattern ScancodeF :: Scancode
pattern ScancodeF = (9) :: Scancode

pattern ScancodeG :: Scancode
pattern ScancodeG = (10) :: Scancode

pattern ScancodeH :: Scancode
pattern ScancodeH = (11) :: Scancode

pattern ScancodeI :: Scancode
pattern ScancodeI = (12) :: Scancode

pattern ScancodeJ :: Scancode
pattern ScancodeJ = (13) :: Scancode

pattern ScancodeK :: Scancode
pattern ScancodeK = (14) :: Scancode

pattern ScancodeL :: Scancode
pattern ScancodeL = (15) :: Scancode

pattern ScancodeM :: Scancode
pattern ScancodeM = (16) :: Scancode

pattern ScancodeN :: Scancode
pattern ScancodeN = (17) :: Scancode

pattern ScancodeO :: Scancode
pattern ScancodeO = (18) :: Scancode

pattern ScancodeP :: Scancode
pattern ScancodeP = (19) :: Scancode

pattern ScancodeQ :: Scancode
pattern ScancodeQ = (20) :: Scancode

pattern ScancodeR :: Scancode
pattern ScancodeR = (21) :: Scancode

pattern ScancodeS :: Scancode
pattern ScancodeS = (22) :: Scancode

pattern ScancodeT :: Scancode
pattern ScancodeT = (23) :: Scancode

pattern ScancodeU :: Scancode
pattern ScancodeU = (24) :: Scancode

pattern ScancodeV :: Scancode
pattern ScancodeV = (25) :: Scancode

pattern ScancodeW :: Scancode
pattern ScancodeW = (26) :: Scancode

pattern ScancodeX :: Scancode
pattern ScancodeX = (27) :: Scancode

pattern ScancodeY :: Scancode
pattern ScancodeY = (28) :: Scancode

pattern ScancodeZ :: Scancode
pattern ScancodeZ = (29) :: Scancode

pattern Scancode1 :: Scancode
pattern Scancode1 = (30) :: Scancode

pattern Scancode2 :: Scancode
pattern Scancode2 = (31) :: Scancode

pattern Scancode3 :: Scancode
pattern Scancode3 = (32) :: Scancode

pattern Scancode4 :: Scancode
pattern Scancode4 = (33) :: Scancode

pattern Scancode5 :: Scancode
pattern Scancode5 = (34) :: Scancode

pattern Scancode6 :: Scancode
pattern Scancode6 = (35) :: Scancode

pattern Scancode7 :: Scancode
pattern Scancode7 = (36) :: Scancode

pattern Scancode8 :: Scancode
pattern Scancode8 = (37) :: Scancode

pattern Scancode9 :: Scancode
pattern Scancode9 = (38) :: Scancode

pattern Scancode0 :: Scancode
pattern Scancode0 = (39) :: Scancode

pattern ScancodeReturn :: Scancode
pattern ScancodeReturn = (40) :: Scancode

pattern ScancodeEscape :: Scancode
pattern ScancodeEscape = (41) :: Scancode

pattern ScancodeBackspace :: Scancode
pattern ScancodeBackspace = (42) :: Scancode

pattern ScancodeTab :: Scancode
pattern ScancodeTab = (43) :: Scancode

pattern ScancodeSpace :: Scancode
pattern ScancodeSpace = (44) :: Scancode

pattern ScancodeMinus :: Scancode
pattern ScancodeMinus = (45) :: Scancode

pattern ScancodeEquals :: Scancode
pattern ScancodeEquals = (46) :: Scancode

pattern ScancodeLeftBracket :: Scancode
pattern ScancodeLeftBracket = (47) :: Scancode

pattern ScancodeRightBracket :: Scancode
pattern ScancodeRightBracket = (48) :: Scancode

pattern ScancodeBackslash :: Scancode
pattern ScancodeBackslash = (49) :: Scancode

pattern ScancodeNonusHash :: Scancode
pattern ScancodeNonusHash = (50) :: Scancode

pattern ScancodeSemicolon :: Scancode
pattern ScancodeSemicolon = (51) :: Scancode

pattern ScancodeApostrophe :: Scancode
pattern ScancodeApostrophe = (52) :: Scancode

pattern ScancodeGrave :: Scancode
pattern ScancodeGrave = (53) :: Scancode

pattern ScancodeComma :: Scancode
pattern ScancodeComma = (54) :: Scancode

pattern ScancodePeriod :: Scancode
pattern ScancodePeriod = (55) :: Scancode

pattern ScancodeSlash :: Scancode
pattern ScancodeSlash = (56) :: Scancode

pattern ScancodeCapsLock :: Scancode
pattern ScancodeCapsLock = (57) :: Scancode

pattern ScancodeF1 :: Scancode
pattern ScancodeF1 = (58) :: Scancode

pattern ScancodeF2 :: Scancode
pattern ScancodeF2 = (59) :: Scancode

pattern ScancodeF3 :: Scancode
pattern ScancodeF3 = (60) :: Scancode

pattern ScancodeF4 :: Scancode
pattern ScancodeF4 = (61) :: Scancode

pattern ScancodeF5 :: Scancode
pattern ScancodeF5 = (62) :: Scancode

pattern ScancodeF6 :: Scancode
pattern ScancodeF6 = (63) :: Scancode

pattern ScancodeF7 :: Scancode
pattern ScancodeF7 = (64) :: Scancode

pattern ScancodeF8 :: Scancode
pattern ScancodeF8 = (65) :: Scancode

pattern ScancodeF9 :: Scancode
pattern ScancodeF9 = (66) :: Scancode

pattern ScancodeF10 :: Scancode
pattern ScancodeF10 = (67) :: Scancode

pattern ScancodeF11 :: Scancode
pattern ScancodeF11 = (68) :: Scancode

pattern ScancodeF12 :: Scancode
pattern ScancodeF12 = (69) :: Scancode

pattern ScancodePrintScreen :: Scancode
pattern ScancodePrintScreen = (70) :: Scancode

pattern ScancodeScrollLock :: Scancode
pattern ScancodeScrollLock = (71) :: Scancode

pattern ScancodePause :: Scancode
pattern ScancodePause = (72) :: Scancode

pattern ScancodeInsert :: Scancode
pattern ScancodeInsert = (73) :: Scancode

pattern ScancodeHome :: Scancode
pattern ScancodeHome = (74) :: Scancode

pattern ScancodePageup :: Scancode
pattern ScancodePageup = (75) :: Scancode

pattern ScancodeDelete :: Scancode
pattern ScancodeDelete = (76) :: Scancode

pattern ScancodeEnd :: Scancode
pattern ScancodeEnd = (77) :: Scancode

pattern ScancodePagedown :: Scancode
pattern ScancodePagedown = (78) :: Scancode

pattern ScancodeRight :: Scancode
pattern ScancodeRight = (79) :: Scancode

pattern ScancodeLeft :: Scancode
pattern ScancodeLeft = (80) :: Scancode

pattern ScancodeDown :: Scancode
pattern ScancodeDown = (81) :: Scancode

pattern ScancodeUp :: Scancode
pattern ScancodeUp = (82) :: Scancode

pattern ScancodeNumlockClear :: Scancode
pattern ScancodeNumlockClear = (83) :: Scancode

pattern ScancodeKpDivide :: Scancode
pattern ScancodeKpDivide = (84) :: Scancode

pattern ScancodeKpMultiply :: Scancode
pattern ScancodeKpMultiply = (85) :: Scancode

pattern ScancodeKpMinus :: Scancode
pattern ScancodeKpMinus = (86) :: Scancode

pattern ScancodeKpPlus :: Scancode
pattern ScancodeKpPlus = (87) :: Scancode

pattern ScancodeKpEnter :: Scancode
pattern ScancodeKpEnter = (88) :: Scancode

pattern ScancodeKp1 :: Scancode
pattern ScancodeKp1 = (89) :: Scancode

pattern ScancodeKp2 :: Scancode
pattern ScancodeKp2 = (90) :: Scancode

pattern ScancodeKp3 :: Scancode
pattern ScancodeKp3 = (91) :: Scancode

pattern ScancodeKp4 :: Scancode
pattern ScancodeKp4 = (92) :: Scancode

pattern ScancodeKp5 :: Scancode
pattern ScancodeKp5 = (93) :: Scancode

pattern ScancodeKp6 :: Scancode
pattern ScancodeKp6 = (94) :: Scancode

pattern ScancodeKp7 :: Scancode
pattern ScancodeKp7 = (95) :: Scancode

pattern ScancodeKp8 :: Scancode
pattern ScancodeKp8 = (96) :: Scancode

pattern ScancodeKp9 :: Scancode
pattern ScancodeKp9 = (97) :: Scancode

pattern ScancodeKp0 :: Scancode
pattern ScancodeKp0 = (98) :: Scancode

pattern ScancodeKpPeriod :: Scancode
pattern ScancodeKpPeriod = (99) :: Scancode

pattern ScancodeNonusBackslash :: Scancode
pattern ScancodeNonusBackslash = (100) :: Scancode

pattern ScancodeApplication :: Scancode
pattern ScancodeApplication = (101) :: Scancode

pattern ScancodePower :: Scancode
pattern ScancodePower = (102) :: Scancode

pattern ScancodeKpEquals :: Scancode
pattern ScancodeKpEquals = (103) :: Scancode

pattern ScancodeF13 :: Scancode
pattern ScancodeF13 = (104) :: Scancode

pattern ScancodeF14 :: Scancode
pattern ScancodeF14 = (105) :: Scancode

pattern ScancodeF15 :: Scancode
pattern ScancodeF15 = (106) :: Scancode

pattern ScancodeF16 :: Scancode
pattern ScancodeF16 = (107) :: Scancode

pattern ScancodeF17 :: Scancode
pattern ScancodeF17 = (108) :: Scancode

pattern ScancodeF18 :: Scancode
pattern ScancodeF18 = (109) :: Scancode

pattern ScancodeF19 :: Scancode
pattern ScancodeF19 = (110) :: Scancode

pattern ScancodeF20 :: Scancode
pattern ScancodeF20 = (111) :: Scancode

pattern ScancodeF21 :: Scancode
pattern ScancodeF21 = (112) :: Scancode

pattern ScancodeF22 :: Scancode
pattern ScancodeF22 = (113) :: Scancode

pattern ScancodeF23 :: Scancode
pattern ScancodeF23 = (114) :: Scancode

pattern ScancodeF24 :: Scancode
pattern ScancodeF24 = (115) :: Scancode

pattern ScancodeExecute :: Scancode
pattern ScancodeExecute = (116) :: Scancode

pattern ScancodeHelp :: Scancode
pattern ScancodeHelp = (117) :: Scancode

pattern ScancodeMenu :: Scancode
pattern ScancodeMenu = (118) :: Scancode

pattern ScancodeSelect :: Scancode
pattern ScancodeSelect = (119) :: Scancode

pattern ScancodeStop :: Scancode
pattern ScancodeStop = (120) :: Scancode

pattern ScancodeAgain :: Scancode
pattern ScancodeAgain = (121) :: Scancode

pattern ScancodeUndo :: Scancode
pattern ScancodeUndo = (122) :: Scancode

pattern ScancodeCut :: Scancode
pattern ScancodeCut = (123) :: Scancode

pattern ScancodeCopy :: Scancode
pattern ScancodeCopy = (124) :: Scancode

pattern ScancodePaste :: Scancode
pattern ScancodePaste = (125) :: Scancode

pattern ScancodeFind :: Scancode
pattern ScancodeFind = (126) :: Scancode

pattern ScancodeMute :: Scancode
pattern ScancodeMute = (127) :: Scancode

pattern ScancodeVolumeUp :: Scancode
pattern ScancodeVolumeUp = (128) :: Scancode

pattern ScancodeVolumeDown :: Scancode
pattern ScancodeVolumeDown = (129) :: Scancode

pattern ScancodeKpComma :: Scancode
pattern ScancodeKpComma = (133) :: Scancode

pattern ScancodeKpEqualsAs400 :: Scancode
pattern ScancodeKpEqualsAs400 = (134) :: Scancode

pattern ScancodeInternational1 :: Scancode
pattern ScancodeInternational1 = (135) :: Scancode

pattern ScancodeInternational2 :: Scancode
pattern ScancodeInternational2 = (136) :: Scancode

pattern ScancodeInternational3 :: Scancode
pattern ScancodeInternational3 = (137) :: Scancode

pattern ScancodeInternational4 :: Scancode
pattern ScancodeInternational4 = (138) :: Scancode

pattern ScancodeInternational5 :: Scancode
pattern ScancodeInternational5 = (139) :: Scancode

pattern ScancodeInternational6 :: Scancode
pattern ScancodeInternational6 = (140) :: Scancode

pattern ScancodeInternational7 :: Scancode
pattern ScancodeInternational7 = (141) :: Scancode

pattern ScancodeInternational8 :: Scancode
pattern ScancodeInternational8 = (142) :: Scancode

pattern ScancodeInternational9 :: Scancode
pattern ScancodeInternational9 = (143) :: Scancode

pattern ScancodeLang1 :: Scancode
pattern ScancodeLang1 = (144) :: Scancode

pattern ScancodeLang2 :: Scancode
pattern ScancodeLang2 = (145) :: Scancode

pattern ScancodeLang3 :: Scancode
pattern ScancodeLang3 = (146) :: Scancode

pattern ScancodeLang4 :: Scancode
pattern ScancodeLang4 = (147) :: Scancode

pattern ScancodeLang5 :: Scancode
pattern ScancodeLang5 = (148) :: Scancode

pattern ScancodeLang6 :: Scancode
pattern ScancodeLang6 = (149) :: Scancode

pattern ScancodeLang7 :: Scancode
pattern ScancodeLang7 = (150) :: Scancode

pattern ScancodeLang8 :: Scancode
pattern ScancodeLang8 = (151) :: Scancode

pattern ScancodeLang9 :: Scancode
pattern ScancodeLang9 = (152) :: Scancode

pattern ScancodeAlterase :: Scancode
pattern ScancodeAlterase = (153) :: Scancode

pattern ScancodeSysreq :: Scancode
pattern ScancodeSysreq = (154) :: Scancode

pattern ScancodeCancel :: Scancode
pattern ScancodeCancel = (155) :: Scancode

pattern ScancodeClear :: Scancode
pattern ScancodeClear = (156) :: Scancode

pattern ScancodePrior :: Scancode
pattern ScancodePrior = (157) :: Scancode

pattern ScancodeReturn2 :: Scancode
pattern ScancodeReturn2 = (158) :: Scancode

pattern ScancodeSeparator :: Scancode
pattern ScancodeSeparator = (159) :: Scancode

pattern ScancodeOut :: Scancode
pattern ScancodeOut = (160) :: Scancode

pattern ScancodeOper :: Scancode
pattern ScancodeOper = (161) :: Scancode

pattern ScancodeClearagain :: Scancode
pattern ScancodeClearagain = (162) :: Scancode

pattern ScancodeCrsel :: Scancode
pattern ScancodeCrsel = (163) :: Scancode

pattern ScancodeExsel :: Scancode
pattern ScancodeExsel = (164) :: Scancode

pattern ScancodeKp00 :: Scancode
pattern ScancodeKp00 = (176) :: Scancode

pattern ScancodeKp000 :: Scancode
pattern ScancodeKp000 = (177) :: Scancode

pattern ScancodeThousandsSeparator :: Scancode
pattern ScancodeThousandsSeparator = (178) :: Scancode

pattern ScancodeDecimalSeparator :: Scancode
pattern ScancodeDecimalSeparator = (179) :: Scancode

pattern ScancodeCurrencyUnit :: Scancode
pattern ScancodeCurrencyUnit = (180) :: Scancode

pattern ScancodeCurrencySubunit :: Scancode
pattern ScancodeCurrencySubunit = (181) :: Scancode

pattern ScancodeKpLeftParen :: Scancode
pattern ScancodeKpLeftParen = (182) :: Scancode

pattern ScancodeKpRightParen :: Scancode
pattern ScancodeKpRightParen = (183) :: Scancode

pattern ScancodeKpLeftBrace :: Scancode
pattern ScancodeKpLeftBrace = (184) :: Scancode

pattern ScancodeKpRightBrace :: Scancode
pattern ScancodeKpRightBrace = (185) :: Scancode

pattern ScancodeKpTab :: Scancode
pattern ScancodeKpTab = (186) :: Scancode

pattern ScancodeKpBackspace :: Scancode
pattern ScancodeKpBackspace = (187) :: Scancode

pattern ScancodeKpA :: Scancode
pattern ScancodeKpA = (188) :: Scancode

pattern ScancodeKpB :: Scancode
pattern ScancodeKpB = (189) :: Scancode

pattern ScancodeKpC :: Scancode
pattern ScancodeKpC = (190) :: Scancode

pattern ScancodeKpD :: Scancode
pattern ScancodeKpD = (191) :: Scancode

pattern ScancodeKpE :: Scancode
pattern ScancodeKpE = (192) :: Scancode

pattern ScancodeKpF :: Scancode
pattern ScancodeKpF = (193) :: Scancode

pattern ScancodeKpXor :: Scancode
pattern ScancodeKpXor = (194) :: Scancode

pattern ScancodeKpPower :: Scancode
pattern ScancodeKpPower = (195) :: Scancode

pattern ScancodeKpPercent :: Scancode
pattern ScancodeKpPercent = (196) :: Scancode

pattern ScancodeKpLess :: Scancode
pattern ScancodeKpLess = (197) :: Scancode

pattern ScancodeKpGreater :: Scancode
pattern ScancodeKpGreater = (198) :: Scancode

pattern ScancodeKpAmpersand :: Scancode
pattern ScancodeKpAmpersand = (199) :: Scancode

pattern ScancodeKpDblAmpersand :: Scancode
pattern ScancodeKpDblAmpersand = (200) :: Scancode

pattern ScancodeKpVerticalBar :: Scancode
pattern ScancodeKpVerticalBar = (201) :: Scancode

pattern ScancodeKpDblVerticalBar :: Scancode
pattern ScancodeKpDblVerticalBar = (202) :: Scancode

pattern ScancodeKpColon :: Scancode
pattern ScancodeKpColon = (203) :: Scancode

pattern ScancodeKpHash :: Scancode
pattern ScancodeKpHash = (204) :: Scancode

pattern ScancodeKpSpace :: Scancode
pattern ScancodeKpSpace = (205) :: Scancode

pattern ScancodeKpAt :: Scancode
pattern ScancodeKpAt = (206) :: Scancode

pattern ScancodeKpExclam :: Scancode
pattern ScancodeKpExclam = (207) :: Scancode

pattern ScancodeKpMemStore :: Scancode
pattern ScancodeKpMemStore = (208) :: Scancode

pattern ScancodeKpMemRecall :: Scancode
pattern ScancodeKpMemRecall = (209) :: Scancode

pattern ScancodeKpMemClear :: Scancode
pattern ScancodeKpMemClear = (210) :: Scancode

pattern ScancodeKpMemAdd :: Scancode
pattern ScancodeKpMemAdd = (211) :: Scancode

pattern ScancodeKpMemSubtract :: Scancode
pattern ScancodeKpMemSubtract = (212) :: Scancode

pattern ScancodeKpMemMultiply :: Scancode
pattern ScancodeKpMemMultiply = (213) :: Scancode

pattern ScancodeKpMemDivide :: Scancode
pattern ScancodeKpMemDivide = (214) :: Scancode

pattern ScancodeKpPlusMinus :: Scancode
pattern ScancodeKpPlusMinus = (215) :: Scancode

pattern ScancodeKpClear :: Scancode
pattern ScancodeKpClear = (216) :: Scancode

pattern ScancodeKpClearEntry :: Scancode
pattern ScancodeKpClearEntry = (217) :: Scancode

pattern ScancodeKpBinary :: Scancode
pattern ScancodeKpBinary = (218) :: Scancode

pattern ScancodeKpOctal :: Scancode
pattern ScancodeKpOctal = (219) :: Scancode

pattern ScancodeKpDecimal :: Scancode
pattern ScancodeKpDecimal = (220) :: Scancode

pattern ScancodeKpHexadecimal :: Scancode
pattern ScancodeKpHexadecimal = (221) :: Scancode

pattern ScancodeLCtrl :: Scancode
pattern ScancodeLCtrl = (224) :: Scancode

pattern ScancodeLShift :: Scancode
pattern ScancodeLShift = (225) :: Scancode

pattern ScancodeLAlt :: Scancode
pattern ScancodeLAlt = (226) :: Scancode

pattern ScancodeLGUI :: Scancode
pattern ScancodeLGUI = (227) :: Scancode

pattern ScancodeRCtrl :: Scancode
pattern ScancodeRCtrl = (228) :: Scancode

pattern ScancodeRShift :: Scancode
pattern ScancodeRShift = (229) :: Scancode

pattern ScancodeRAlt :: Scancode
pattern ScancodeRAlt = (230) :: Scancode

pattern ScancodeRGUI :: Scancode
pattern ScancodeRGUI = (231) :: Scancode

pattern ScancodeMode :: Scancode
pattern ScancodeMode = (257) :: Scancode

pattern ScancodeAudioNext :: Scancode
pattern ScancodeAudioNext = (258) :: Scancode

pattern ScancodeAudioPrev :: Scancode
pattern ScancodeAudioPrev = (259) :: Scancode

pattern ScancodeAudioStop :: Scancode
pattern ScancodeAudioStop = (260) :: Scancode

pattern ScancodeAudioPlay :: Scancode
pattern ScancodeAudioPlay = (261) :: Scancode

pattern ScancodeAudioMute :: Scancode
pattern ScancodeAudioMute = (262) :: Scancode

pattern ScancodeMediaSelect :: Scancode
pattern ScancodeMediaSelect = (263) :: Scancode

pattern ScancodeWWW :: Scancode
pattern ScancodeWWW = (264) :: Scancode

pattern ScancodeMail :: Scancode
pattern ScancodeMail = (265) :: Scancode

pattern ScancodeCalculator :: Scancode
pattern ScancodeCalculator = (266) :: Scancode

pattern ScancodeComputer :: Scancode
pattern ScancodeComputer = (267) :: Scancode

pattern ScancodeAcSearch :: Scancode
pattern ScancodeAcSearch = (268) :: Scancode

pattern ScancodeAcHome :: Scancode
pattern ScancodeAcHome = (269) :: Scancode

pattern ScancodeAcBack :: Scancode
pattern ScancodeAcBack = (270) :: Scancode

pattern ScancodeAcForward :: Scancode
pattern ScancodeAcForward = (271) :: Scancode

pattern ScancodeAcStop :: Scancode
pattern ScancodeAcStop = (272) :: Scancode

pattern ScancodeAcRefresh :: Scancode
pattern ScancodeAcRefresh = (273) :: Scancode

pattern ScancodeAcBookmarks :: Scancode
pattern ScancodeAcBookmarks = (274) :: Scancode

pattern ScancodeBrightnessDown :: Scancode
pattern ScancodeBrightnessDown = (275) :: Scancode

pattern ScancodeBrightnessUp :: Scancode
pattern ScancodeBrightnessUp = (276) :: Scancode

pattern ScancodeDisplaySwitch :: Scancode
pattern ScancodeDisplaySwitch = (277) :: Scancode

pattern ScancodeKbdIllumToggle :: Scancode
pattern ScancodeKbdIllumToggle = (278) :: Scancode

pattern ScancodeKbdIllumDown :: Scancode
pattern ScancodeKbdIllumDown = (279) :: Scancode

pattern ScancodeKbdIllumUp :: Scancode
pattern ScancodeKbdIllumUp = (280) :: Scancode

pattern ScancodeEject :: Scancode
pattern ScancodeEject = (281) :: Scancode

pattern ScancodeSleep :: Scancode
pattern ScancodeSleep = (282) :: Scancode

pattern ScancodeApp1 :: Scancode
pattern ScancodeApp1 = (283) :: Scancode

pattern ScancodeApp2 :: Scancode
pattern ScancodeApp2 = (284) :: Scancode

pattern NumScancodes :: Scancode
pattern NumScancodes = (512) :: Scancode
