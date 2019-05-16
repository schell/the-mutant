-- | A collection of key codes for your convenience.
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Mutant.KeyboardCodes.Keycode where

import           Data.Int      (Int32)


-- | Keycodes are layout-dependent codes, in that the code you receive can change
-- for the same physical key depending on the users keyboard layout.
-- Think of this as "the user pressed the key that is labelled 'Q' on a specific
-- keyboard."
-- 'Keycode' makes sense when you are anticipating the user to press either keys
-- that match some mneumonic, or they are entering text.
newtype Keycode
  = Keycode { unKeycode :: Int32 }
  deriving newtype (Eq, Ord, Num, Show)


pattern KeycodeUnknown :: Keycode
pattern KeycodeUnknown = (0) :: Keycode

pattern KeycodeReturn :: Keycode
pattern KeycodeReturn = (13) :: Keycode

pattern KeycodeEscape :: Keycode
pattern KeycodeEscape = (27) :: Keycode

pattern KeycodeBackspace :: Keycode
pattern KeycodeBackspace = (8) :: Keycode

pattern KeycodeTab :: Keycode
pattern KeycodeTab = (9) :: Keycode

pattern KeycodeSpace :: Keycode
pattern KeycodeSpace = (32) :: Keycode

pattern KeycodeExclaim :: Keycode
pattern KeycodeExclaim = (33) :: Keycode

pattern KeycodeQuotedbl :: Keycode
pattern KeycodeQuotedbl = (34) :: Keycode

pattern KeycodeHash :: Keycode
pattern KeycodeHash = (35) :: Keycode

pattern KeycodePercent :: Keycode
pattern KeycodePercent = (37) :: Keycode

pattern KeycodeDollar :: Keycode
pattern KeycodeDollar = (36) :: Keycode

pattern KeycodeAmpersand :: Keycode
pattern KeycodeAmpersand = (38) :: Keycode

pattern KeycodeQuote :: Keycode
pattern KeycodeQuote = (39) :: Keycode

pattern KeycodeLeftParen :: Keycode
pattern KeycodeLeftParen = (40) :: Keycode

pattern KeycodeRightParen :: Keycode
pattern KeycodeRightParen = (41) :: Keycode

pattern KeycodeAsterisk :: Keycode
pattern KeycodeAsterisk = (42) :: Keycode

pattern KeycodePlus :: Keycode
pattern KeycodePlus = (43) :: Keycode

pattern KeycodeComma :: Keycode
pattern KeycodeComma = (44) :: Keycode

pattern KeycodeMinus :: Keycode
pattern KeycodeMinus = (45) :: Keycode

pattern KeycodePeriod :: Keycode
pattern KeycodePeriod = (46) :: Keycode

pattern KeycodeSlash :: Keycode
pattern KeycodeSlash = (47) :: Keycode

pattern Keycode0 :: Keycode
pattern Keycode0 = (48) :: Keycode

pattern Keycode1 :: Keycode
pattern Keycode1 = (49) :: Keycode

pattern Keycode2 :: Keycode
pattern Keycode2 = (50) :: Keycode

pattern Keycode3 :: Keycode
pattern Keycode3 = (51) :: Keycode

pattern Keycode4 :: Keycode
pattern Keycode4 = (52) :: Keycode

pattern Keycode5 :: Keycode
pattern Keycode5 = (53) :: Keycode

pattern Keycode6 :: Keycode
pattern Keycode6 = (54) :: Keycode

pattern Keycode7 :: Keycode
pattern Keycode7 = (55) :: Keycode

pattern Keycode8 :: Keycode
pattern Keycode8 = (56) :: Keycode

pattern Keycode9 :: Keycode
pattern Keycode9 = (57) :: Keycode

pattern KeycodeColon :: Keycode
pattern KeycodeColon = (58) :: Keycode

pattern KeycodeSemicolon :: Keycode
pattern KeycodeSemicolon = (59) :: Keycode

pattern KeycodeLess :: Keycode
pattern KeycodeLess = (60) :: Keycode

pattern KeycodeEquals :: Keycode
pattern KeycodeEquals = (61) :: Keycode

pattern KeycodeGreater :: Keycode
pattern KeycodeGreater = (62) :: Keycode

pattern KeycodeQuestion :: Keycode
pattern KeycodeQuestion = (63) :: Keycode

pattern KeycodeAt :: Keycode
pattern KeycodeAt = (64) :: Keycode

pattern KeycodeLeftbracket :: Keycode
pattern KeycodeLeftbracket = (91) :: Keycode

pattern KeycodeBackslash :: Keycode
pattern KeycodeBackslash = (92) :: Keycode

pattern KeycodeRightbracket :: Keycode
pattern KeycodeRightbracket = (93) :: Keycode

pattern KeycodeCaret :: Keycode
pattern KeycodeCaret = (94) :: Keycode

pattern KeycodeUnderscore :: Keycode
pattern KeycodeUnderscore = (95) :: Keycode

pattern KeycodeBackquote :: Keycode
pattern KeycodeBackquote = (96) :: Keycode

pattern Keycodea :: Keycode
pattern Keycodea = (97) :: Keycode

pattern Keycodeb :: Keycode
pattern Keycodeb = (98) :: Keycode

pattern Keycodec :: Keycode
pattern Keycodec = (99) :: Keycode

pattern Keycoded :: Keycode
pattern Keycoded = (100) :: Keycode

pattern Keycodee :: Keycode
pattern Keycodee = (101) :: Keycode

pattern Keycodef :: Keycode
pattern Keycodef = (102) :: Keycode

pattern Keycodeg :: Keycode
pattern Keycodeg = (103) :: Keycode

pattern Keycodeh :: Keycode
pattern Keycodeh = (104) :: Keycode

pattern Keycodei :: Keycode
pattern Keycodei = (105) :: Keycode

pattern Keycodej :: Keycode
pattern Keycodej = (106) :: Keycode

pattern Keycodek :: Keycode
pattern Keycodek = (107) :: Keycode

pattern Keycodel :: Keycode
pattern Keycodel = (108) :: Keycode

pattern Keycodem :: Keycode
pattern Keycodem = (109) :: Keycode

pattern Keycoden :: Keycode
pattern Keycoden = (110) :: Keycode

pattern Keycodeo :: Keycode
pattern Keycodeo = (111) :: Keycode

pattern Keycodep :: Keycode
pattern Keycodep = (112) :: Keycode

pattern Keycodeq :: Keycode
pattern Keycodeq = (113) :: Keycode

pattern Keycoder :: Keycode
pattern Keycoder = (114) :: Keycode

pattern Keycodes :: Keycode
pattern Keycodes = (115) :: Keycode

pattern Keycodet :: Keycode
pattern Keycodet = (116) :: Keycode

pattern Keycodeu :: Keycode
pattern Keycodeu = (117) :: Keycode

pattern Keycodev :: Keycode
pattern Keycodev = (118) :: Keycode

pattern Keycodew :: Keycode
pattern Keycodew = (119) :: Keycode

pattern Keycodex :: Keycode
pattern Keycodex = (120) :: Keycode

pattern Keycodey :: Keycode
pattern Keycodey = (121) :: Keycode

pattern Keycodez :: Keycode
pattern Keycodez = (122) :: Keycode

pattern KeycodeCapslock :: Keycode
pattern KeycodeCapslock = (1073741881) :: Keycode

pattern KeycodeF1 :: Keycode
pattern KeycodeF1 = (1073741882) :: Keycode

pattern KeycodeF2 :: Keycode
pattern KeycodeF2 = (1073741883) :: Keycode

pattern KeycodeF3 :: Keycode
pattern KeycodeF3 = (1073741884) :: Keycode

pattern KeycodeF4 :: Keycode
pattern KeycodeF4 = (1073741885) :: Keycode

pattern KeycodeF5 :: Keycode
pattern KeycodeF5 = (1073741886) :: Keycode

pattern KeycodeF6 :: Keycode
pattern KeycodeF6 = (1073741887) :: Keycode

pattern KeycodeF7 :: Keycode
pattern KeycodeF7 = (1073741888) :: Keycode

pattern KeycodeF8 :: Keycode
pattern KeycodeF8 = (1073741889) :: Keycode

pattern KeycodeF9 :: Keycode
pattern KeycodeF9 = (1073741890) :: Keycode

pattern KeycodeF10 :: Keycode
pattern KeycodeF10 = (1073741891) :: Keycode

pattern KeycodeF11 :: Keycode
pattern KeycodeF11 = (1073741892) :: Keycode

pattern KeycodeF12 :: Keycode
pattern KeycodeF12 = (1073741893) :: Keycode

pattern KeycodePrintscreen :: Keycode
pattern KeycodePrintscreen = (1073741894) :: Keycode

pattern KeycodeScrolllock :: Keycode
pattern KeycodeScrolllock = (1073741895) :: Keycode

pattern KeycodePause :: Keycode
pattern KeycodePause = (1073741896) :: Keycode

pattern KeycodeInsert :: Keycode
pattern KeycodeInsert = (1073741897) :: Keycode

pattern KeycodeHome :: Keycode
pattern KeycodeHome = (1073741898) :: Keycode

pattern KeycodePageup :: Keycode
pattern KeycodePageup = (1073741899) :: Keycode

pattern KeycodeDelete :: Keycode
pattern KeycodeDelete = (127) :: Keycode

pattern KeycodeEnd :: Keycode
pattern KeycodeEnd = (1073741901) :: Keycode

pattern KeycodePagedown :: Keycode
pattern KeycodePagedown = (1073741902) :: Keycode

pattern KeycodeRight :: Keycode
pattern KeycodeRight = (1073741903) :: Keycode

pattern KeycodeLeft :: Keycode
pattern KeycodeLeft = (1073741904) :: Keycode

pattern KeycodeDown :: Keycode
pattern KeycodeDown = (1073741905) :: Keycode

pattern KeycodeUp :: Keycode
pattern KeycodeUp = (1073741906) :: Keycode

pattern KeycodeNumlockClear :: Keycode
pattern KeycodeNumlockClear = (1073741907) :: Keycode

pattern KeycodeKpDivide :: Keycode
pattern KeycodeKpDivide = (1073741908) :: Keycode

pattern KeycodeKpMultiply :: Keycode
pattern KeycodeKpMultiply = (1073741909) :: Keycode

pattern KeycodeKpMinus :: Keycode
pattern KeycodeKpMinus = (1073741910) :: Keycode

pattern KeycodeKpPlus :: Keycode
pattern KeycodeKpPlus = (1073741911) :: Keycode

pattern KeycodeKpEnter :: Keycode
pattern KeycodeKpEnter = (1073741912) :: Keycode

pattern KeycodeKp1 :: Keycode
pattern KeycodeKp1 = (1073741913) :: Keycode

pattern KeycodeKp2 :: Keycode
pattern KeycodeKp2 = (1073741914) :: Keycode

pattern KeycodeKp3 :: Keycode
pattern KeycodeKp3 = (1073741915) :: Keycode

pattern KeycodeKp4 :: Keycode
pattern KeycodeKp4 = (1073741916) :: Keycode

pattern KeycodeKp5 :: Keycode
pattern KeycodeKp5 = (1073741917) :: Keycode

pattern KeycodeKp6 :: Keycode
pattern KeycodeKp6 = (1073741918) :: Keycode

pattern KeycodeKp7 :: Keycode
pattern KeycodeKp7 = (1073741919) :: Keycode

pattern KeycodeKp8 :: Keycode
pattern KeycodeKp8 = (1073741920) :: Keycode

pattern KeycodeKp9 :: Keycode
pattern KeycodeKp9 = (1073741921) :: Keycode

pattern KeycodeKp0 :: Keycode
pattern KeycodeKp0 = (1073741922) :: Keycode

pattern KeycodeKpPeriod :: Keycode
pattern KeycodeKpPeriod = (1073741923) :: Keycode

pattern KeycodeApplication :: Keycode
pattern KeycodeApplication = (1073741925) :: Keycode

pattern KeycodePower :: Keycode
pattern KeycodePower = (1073741926) :: Keycode

pattern KeycodeKpEquals :: Keycode
pattern KeycodeKpEquals = (1073741927) :: Keycode

pattern KeycodeF13 :: Keycode
pattern KeycodeF13 = (1073741928) :: Keycode

pattern KeycodeF14 :: Keycode
pattern KeycodeF14 = (1073741929) :: Keycode

pattern KeycodeF15 :: Keycode
pattern KeycodeF15 = (1073741930) :: Keycode

pattern KeycodeF16 :: Keycode
pattern KeycodeF16 = (1073741931) :: Keycode

pattern KeycodeF17 :: Keycode
pattern KeycodeF17 = (1073741932) :: Keycode

pattern KeycodeF18 :: Keycode
pattern KeycodeF18 = (1073741933) :: Keycode

pattern KeycodeF19 :: Keycode
pattern KeycodeF19 = (1073741934) :: Keycode

pattern KeycodeF20 :: Keycode
pattern KeycodeF20 = (1073741935) :: Keycode

pattern KeycodeF21 :: Keycode
pattern KeycodeF21 = (1073741936) :: Keycode

pattern KeycodeF22 :: Keycode
pattern KeycodeF22 = (1073741937) :: Keycode

pattern KeycodeF23 :: Keycode
pattern KeycodeF23 = (1073741938) :: Keycode

pattern KeycodeF24 :: Keycode
pattern KeycodeF24 = (1073741939) :: Keycode

pattern KeycodeExecute :: Keycode
pattern KeycodeExecute = (1073741940) :: Keycode

pattern KeycodeHelp :: Keycode
pattern KeycodeHelp = (1073741941) :: Keycode

pattern KeycodeMenu :: Keycode
pattern KeycodeMenu = (1073741942) :: Keycode

pattern KeycodeSelect :: Keycode
pattern KeycodeSelect = (1073741943) :: Keycode

pattern KeycodeStop :: Keycode
pattern KeycodeStop = (1073741944) :: Keycode

pattern KeycodeAgain :: Keycode
pattern KeycodeAgain = (1073741945) :: Keycode

pattern KeycodeUndo :: Keycode
pattern KeycodeUndo = (1073741946) :: Keycode

pattern KeycodeCut :: Keycode
pattern KeycodeCut = (1073741947) :: Keycode

pattern KeycodeCopy :: Keycode
pattern KeycodeCopy = (1073741948) :: Keycode

pattern KeycodePaste :: Keycode
pattern KeycodePaste = (1073741949) :: Keycode

pattern KeycodeFind :: Keycode
pattern KeycodeFind = (1073741950) :: Keycode

pattern KeycodeMute :: Keycode
pattern KeycodeMute = (1073741951) :: Keycode

pattern KeycodeVolumeUp :: Keycode
pattern KeycodeVolumeUp = (1073741952) :: Keycode

pattern KeycodeVolumeDown :: Keycode
pattern KeycodeVolumeDown = (1073741953) :: Keycode

pattern KeycodeKpComma :: Keycode
pattern KeycodeKpComma = (1073741957) :: Keycode

pattern KeycodeKpEqualsAs400 :: Keycode
pattern KeycodeKpEqualsAs400 = (1073741958) :: Keycode

pattern KeycodeAltErase :: Keycode
pattern KeycodeAltErase = (1073741977) :: Keycode

pattern KeycodeSysReq :: Keycode
pattern KeycodeSysReq = (1073741978) :: Keycode

pattern KeycodeCancel :: Keycode
pattern KeycodeCancel = (1073741979) :: Keycode

pattern KeycodeClear :: Keycode
pattern KeycodeClear = (1073741980) :: Keycode

pattern KeycodePrior :: Keycode
pattern KeycodePrior = (1073741981) :: Keycode

pattern KeycodeReturn2 :: Keycode
pattern KeycodeReturn2 = (1073741982) :: Keycode

pattern KeycodeSeparator :: Keycode
pattern KeycodeSeparator = (1073741983) :: Keycode

pattern KeycodeOut :: Keycode
pattern KeycodeOut = (1073741984) :: Keycode

pattern KeycodeOper :: Keycode
pattern KeycodeOper = (1073741985) :: Keycode

pattern KeycodeClearAgain :: Keycode
pattern KeycodeClearAgain = (1073741986) :: Keycode

pattern KeycodeCrSel :: Keycode
pattern KeycodeCrSel = (1073741987) :: Keycode

pattern KeycodeExSel :: Keycode
pattern KeycodeExSel = (1073741988) :: Keycode

pattern KeycodeKp00 :: Keycode
pattern KeycodeKp00 = (1073742000) :: Keycode

pattern KeycodeKp000 :: Keycode
pattern KeycodeKp000 = (1073742001) :: Keycode

pattern KeycodeThousandsSeparator :: Keycode
pattern KeycodeThousandsSeparator = (1073742002) :: Keycode

pattern KeycodeDecimalSeparator :: Keycode
pattern KeycodeDecimalSeparator = (1073742003) :: Keycode

pattern KeycodeCurrencyUnit :: Keycode
pattern KeycodeCurrencyUnit = (1073742004) :: Keycode

pattern KeycodeCurrencySubunit :: Keycode
pattern KeycodeCurrencySubunit = (1073742005) :: Keycode

pattern KeycodeKpLeftParen :: Keycode
pattern KeycodeKpLeftParen = (1073742006) :: Keycode

pattern KeycodeKpRightParen :: Keycode
pattern KeycodeKpRightParen = (1073742007) :: Keycode

pattern KeycodeKpLeftBrace :: Keycode
pattern KeycodeKpLeftBrace = (1073742008) :: Keycode

pattern KeycodeKpRightBrace :: Keycode
pattern KeycodeKpRightBrace = (1073742009) :: Keycode

pattern KeycodeKpTab :: Keycode
pattern KeycodeKpTab = (1073742010) :: Keycode

pattern KeycodeKpBackspace :: Keycode
pattern KeycodeKpBackspace = (1073742011) :: Keycode

pattern KeycodeKpA :: Keycode
pattern KeycodeKpA = (1073742012) :: Keycode

pattern KeycodeKpB :: Keycode
pattern KeycodeKpB = (1073742013) :: Keycode

pattern KeycodeKpC :: Keycode
pattern KeycodeKpC = (1073742014) :: Keycode

pattern KeycodeKpD :: Keycode
pattern KeycodeKpD = (1073742015) :: Keycode

pattern KeycodeKpE :: Keycode
pattern KeycodeKpE = (1073742016) :: Keycode

pattern KeycodeKpF :: Keycode
pattern KeycodeKpF = (1073742017) :: Keycode

pattern KeycodeKpXor :: Keycode
pattern KeycodeKpXor = (1073742018) :: Keycode

pattern KeycodeKpPower :: Keycode
pattern KeycodeKpPower = (1073742019) :: Keycode

pattern KeycodeKpPercent :: Keycode
pattern KeycodeKpPercent = (1073742020) :: Keycode

pattern KeycodeKpLess :: Keycode
pattern KeycodeKpLess = (1073742021) :: Keycode

pattern KeycodeKpGreater :: Keycode
pattern KeycodeKpGreater = (1073742022) :: Keycode

pattern KeycodeKpAmpersand :: Keycode
pattern KeycodeKpAmpersand = (1073742023) :: Keycode

pattern KeycodeKpDblAmpersand :: Keycode
pattern KeycodeKpDblAmpersand = (1073742024) :: Keycode

pattern KeycodeKpVerticalBar :: Keycode
pattern KeycodeKpVerticalBar = (1073742025) :: Keycode

pattern KeycodeKpDblVerticalBar :: Keycode
pattern KeycodeKpDblVerticalBar = (1073742026) :: Keycode

pattern KeycodeKpColon :: Keycode
pattern KeycodeKpColon = (1073742027) :: Keycode

pattern KeycodeKpHash :: Keycode
pattern KeycodeKpHash = (1073742028) :: Keycode

pattern KeycodeKpSpace :: Keycode
pattern KeycodeKpSpace = (1073742029) :: Keycode

pattern KeycodeKpAt :: Keycode
pattern KeycodeKpAt = (1073742030) :: Keycode

pattern KeycodeKpExclam :: Keycode
pattern KeycodeKpExclam = (1073742031) :: Keycode

pattern KeycodeKpMemStore :: Keycode
pattern KeycodeKpMemStore = (1073742032) :: Keycode

pattern KeycodeKpMemRecall :: Keycode
pattern KeycodeKpMemRecall = (1073742033) :: Keycode

pattern KeycodeKpMemClear :: Keycode
pattern KeycodeKpMemClear = (1073742034) :: Keycode

pattern KeycodeKpMemAdd :: Keycode
pattern KeycodeKpMemAdd = (1073742035) :: Keycode

pattern KeycodeKpMemSubtract :: Keycode
pattern KeycodeKpMemSubtract = (1073742036) :: Keycode

pattern KeycodeKpMemMultiply :: Keycode
pattern KeycodeKpMemMultiply = (1073742037) :: Keycode

pattern KeycodeKpMemDivide :: Keycode
pattern KeycodeKpMemDivide = (1073742038) :: Keycode

pattern KeycodeKpPlusMinus :: Keycode
pattern KeycodeKpPlusMinus = (1073742039) :: Keycode

pattern KeycodeKpClear :: Keycode
pattern KeycodeKpClear = (1073742040) :: Keycode

pattern KeycodeKpClearEntry :: Keycode
pattern KeycodeKpClearEntry = (1073742041) :: Keycode

pattern KeycodeKpBinary :: Keycode
pattern KeycodeKpBinary = (1073742042) :: Keycode

pattern KeycodeKpOctal :: Keycode
pattern KeycodeKpOctal = (1073742043) :: Keycode

pattern KeycodeKpDecimal :: Keycode
pattern KeycodeKpDecimal = (1073742044) :: Keycode

pattern KeycodeKpHexadecimal :: Keycode
pattern KeycodeKpHexadecimal = (1073742045) :: Keycode

pattern KeycodeLCtrl :: Keycode
pattern KeycodeLCtrl = (1073742048) :: Keycode

pattern KeycodeLShift :: Keycode
pattern KeycodeLShift = (1073742049) :: Keycode

pattern KeycodeLAlt :: Keycode
pattern KeycodeLAlt = (1073742050) :: Keycode

pattern KeycodeLGUI :: Keycode
pattern KeycodeLGUI = (1073742051) :: Keycode

pattern KeycodeRCtrl :: Keycode
pattern KeycodeRCtrl = (1073742052) :: Keycode

pattern KeycodeRShift :: Keycode
pattern KeycodeRShift = (1073742053) :: Keycode

pattern KeycodeRAlt :: Keycode
pattern KeycodeRAlt = (1073742054) :: Keycode

pattern KeycodeRGUI :: Keycode
pattern KeycodeRGUI = (1073742055) :: Keycode

pattern KeycodeMode :: Keycode
pattern KeycodeMode = (1073742081) :: Keycode

pattern KeycodeAudioNext :: Keycode
pattern KeycodeAudioNext = (1073742082) :: Keycode

pattern KeycodeAudioPrev :: Keycode
pattern KeycodeAudioPrev = (1073742083) :: Keycode

pattern KeycodeAudioStop :: Keycode
pattern KeycodeAudioStop = (1073742084) :: Keycode

pattern KeycodeAudioPlay :: Keycode
pattern KeycodeAudioPlay = (1073742085) :: Keycode

pattern KeycodeAudioMute :: Keycode
pattern KeycodeAudioMute = (1073742086) :: Keycode

pattern KeycodeMediaSelect :: Keycode
pattern KeycodeMediaSelect = (1073742087) :: Keycode

pattern KeycodeWWW :: Keycode
pattern KeycodeWWW = (1073742088) :: Keycode

pattern KeycodeMail :: Keycode
pattern KeycodeMail = (1073742089) :: Keycode

pattern KeycodeCalculator :: Keycode
pattern KeycodeCalculator = (1073742090) :: Keycode

pattern KeycodeComputer :: Keycode
pattern KeycodeComputer = (1073742091) :: Keycode

pattern KeycodeAcSearch :: Keycode
pattern KeycodeAcSearch = (1073742092) :: Keycode

pattern KeycodeAcHome :: Keycode
pattern KeycodeAcHome = (1073742093) :: Keycode

pattern KeycodeAcBack :: Keycode
pattern KeycodeAcBack = (1073742094) :: Keycode

pattern KeycodeAcForward :: Keycode
pattern KeycodeAcForward = (1073742095) :: Keycode

pattern KeycodeAcStop :: Keycode
pattern KeycodeAcStop = (1073742096) :: Keycode

pattern KeycodeAcRefresh :: Keycode
pattern KeycodeAcRefresh = (1073742097) :: Keycode

pattern KeycodeAcBookmarks :: Keycode
pattern KeycodeAcBookmarks = (1073742098) :: Keycode

pattern KeycodeBrightnessDown :: Keycode
pattern KeycodeBrightnessDown = (1073742099) :: Keycode

pattern KeycodeBrightnessUp :: Keycode
pattern KeycodeBrightnessUp = (1073742100) :: Keycode

pattern KeycodeDisplaySwitch :: Keycode
pattern KeycodeDisplaySwitch = (1073742101) :: Keycode

pattern KeycodeKbdIllumToggle :: Keycode
pattern KeycodeKbdIllumToggle = (1073742102) :: Keycode

pattern KeycodeKbdIllumDown :: Keycode
pattern KeycodeKbdIllumDown = (1073742103) :: Keycode

pattern KeycodeKbdIllumUp :: Keycode
pattern KeycodeKbdIllumUp = (1073742104) :: Keycode

pattern KeycodeEject :: Keycode
pattern KeycodeEject = (1073742105) :: Keycode

pattern KeycodeSleep :: Keycode
pattern KeycodeSleep = (1073742106) :: Keycode
