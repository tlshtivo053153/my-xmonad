module XMonad.Vim.Parse.Key
    ( singleCharKey
    , multiCharkeys
    , multimediaKeys
    , parse
    , parseKeymap
    , keyToString
    ) where

import Graphics.X11.Types

import Control.Arrow ( first )
import Data.Bits ( (.|.), (.&.) )
import Data.List ( find )

import Text.Appar.String hiding (parse)

--"!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
singleCharKey :: [(Char, KeySym)]
singleCharKey =
    [ ('!'    , xK_exclam)
    , ('\"'   , xK_quotedbl)
    , ('#'    , xK_numbersign)
    , ('$'    , xK_dollar)
    , ('%'    , xK_percent)
    , ('&'    , xK_ampersand)
    , ('\''   , xK_apostrophe)
    , ('('    , xK_parenleft)
    , (')'    , xK_parenright)
    , ('*'    , xK_asterisk)
    , ('+'    , xK_plus)
    , (','    , xK_comma)
    , ('-'    , xK_minus)
    , ('.'    , xK_period)
    , ('/'    , xK_slash)
    , ('0'    , xK_0)
    , ('1'    , xK_1)
    , ('2'    , xK_2)
    , ('3'    , xK_3)
    , ('4'    , xK_4)
    , ('5'    , xK_5)
    , ('6'    , xK_6)
    , ('7'    , xK_7)
    , ('8'    , xK_8)
    , ('9'    , xK_9)
    , (':'    , xK_colon)
    , (';'    , xK_semicolon)
    , ('<'    , xK_less)
    , ('='    , xK_equal)
    , ('>'    , xK_greater)
    , ('?'    , xK_question)
    , ('@'    , xK_at)
    , ('A'    , xK_A)
    , ('B'    , xK_B)
    , ('C'    , xK_C)
    , ('D'    , xK_D)
    , ('E'    , xK_E)
    , ('F'    , xK_F)
    , ('G'    , xK_G)
    , ('H'    , xK_H)
    , ('I'    , xK_I)
    , ('J'    , xK_J)
    , ('K'    , xK_K)
    , ('L'    , xK_L)
    , ('M'    , xK_M)
    , ('N'    , xK_N)
    , ('O'    , xK_O)
    , ('P'    , xK_P)
    , ('Q'    , xK_Q)
    , ('R'    , xK_R)
    , ('S'    , xK_S)
    , ('T'    , xK_T)
    , ('U'    , xK_U)
    , ('V'    , xK_V)
    , ('W'    , xK_W)
    , ('X'    , xK_X)
    , ('Y'    , xK_Y)
    , ('Z'    , xK_Z)
    , ('['    , xK_bracketleft)
    , ('\\'   , xK_backslash)
    , (']'    , xK_bracketright)
    , ('^'    , xK_asciicircum)
    , ('_'    , xK_underscore)
    , ('`'    , xK_grave)
    , ('a'    , xK_a)
    , ('b'    , xK_b)
    , ('c'    , xK_c)
    , ('d'    , xK_d)
    , ('e'    , xK_e)
    , ('f'    , xK_f)
    , ('g'    , xK_g)
    , ('h'    , xK_h)
    , ('i'    , xK_i)
    , ('j'    , xK_j)
    , ('k'    , xK_k)
    , ('l'    , xK_l)
    , ('m'    , xK_m)
    , ('n'    , xK_n)
    , ('o'    , xK_o)
    , ('p'    , xK_p)
    , ('q'    , xK_q)
    , ('r'    , xK_r)
    , ('s'    , xK_s)
    , ('t'    , xK_t)
    , ('u'    , xK_u)
    , ('v'    , xK_v)
    , ('w'    , xK_w)
    , ('x'    , xK_x)
    , ('y'    , xK_y)
    , ('z'    , xK_z)
    , ('{'    , xK_braceleft)
    , ('|'    , xK_bar)
    , ('}'    , xK_braceright)
    , ('~'    , xK_asciitilde)
    , ('\xa0' , xK_nobreakspace) --160
    , ('\xa1' , xK_exclamdown)
    , ('\xa2' , xK_cent)
    , ('\xa3' , xK_sterling)
    , ('\xa4' , xK_currency)
    , ('\xa5' , xK_yen)
    , ('\xa6' , xK_brokenbar)
    , ('\xa7' , xK_section)
    , ('\xa8' , xK_diaeresis)
    , ('\xa9' , xK_copyright)
    , ('\xaa' , xK_ordfeminine)
    , ('\xab' , xK_guillemotleft)
    , ('\xac' , xK_notsign)
    , ('\xad' , xK_hyphen)
    , ('\xae' , xK_registered)
    , ('\xaf' , xK_macron)
    , ('\xb0' , xK_degree)
    , ('\xb1' , xK_plusminus)
    , ('\xb2' , xK_twosuperior)
    , ('\xb3' , xK_threesuperior)
    , ('\xb4' , xK_acute)
    , ('\xb5' , xK_mu)
    , ('\xb6' , xK_paragraph)
    , ('\xb7' , xK_periodcentered)
    , ('\xb8' , xK_cedilla)
    , ('\xb9' , xK_onesuperior)
    , ('\xba' , xK_masculine)
    , ('\xbb' , xK_guillemotright)
    , ('\xbc' , xK_onequarter)
    , ('\xbd' , xK_onehalf)
    , ('\xbe' , xK_threequarters)
    , ('\xbf' , xK_questiondown)
    , ('\xc0' , xK_Agrave)
    , ('\xc1' , xK_Aacute)
    , ('\xc2' , xK_Acircumflex)
    , ('\xc3' , xK_Atilde)
    , ('\xc4' , xK_Adiaeresis)
    , ('\xc5' , xK_Aring)
    , ('\xc6' , xK_AE)
    , ('\xc7' , xK_Ccedilla)
    , ('\xc8' , xK_Egrave)
    , ('\xc9' , xK_Eacute)
    , ('\xca' , xK_Ecircumflex)
    , ('\xcb' , xK_Ediaeresis)
    , ('\xcc' , xK_Igrave)
    , ('\xcd' , xK_Iacute)
    , ('\xce' , xK_Icircumflex)
    , ('\xcf' , xK_Idiaeresis)
    , ('\xd0' , xK_ETH) -- 208
    , ('\xd1' , xK_Ntilde)
    , ('\xd2' , xK_Ograve)
    , ('\xd3' , xK_Oacute)
    , ('\xd4' , xK_Ocircumflex)
    , ('\xd5' , xK_Otilde)
    , ('\xd6' , xK_Odiaeresis)
    , ('\xd7' , xK_multiply)
    , ('\xd8' , xK_Ooblique)
    , ('\xd9' , xK_Ugrave)
    , ('\xda' , xK_Uacute)
    , ('\xdb' , xK_Ucircumflex)
    , ('\xdc' , xK_Udiaeresis)
    , ('\xdd' , xK_Yacute)
    , ('\xde' , xK_THORN)
    , ('\xdf' , xK_ssharp)
    , ('\xe0' , xK_agrave)
    , ('\xe1' , xK_aacute)
    , ('\xe2' , xK_acircumflex)
    , ('\xe3' , xK_atilde)
    , ('\xe4' , xK_adiaeresis)
    , ('\xe5' , xK_aring)
    , ('\xe6' , xK_ae)
    , ('\xe7' , xK_ccedilla)
    , ('\xe8' , xK_egrave)
    , ('\xe9' , xK_eacute)
    , ('\xea' , xK_ecircumflex)
    , ('\xeb' , xK_ediaeresis)
    , ('\xec' , xK_igrave)
    , ('\xed' , xK_iacute)
    , ('\xee' , xK_icircumflex)
    , ('\xef' , xK_idiaeresis)
    , ('\xf0' , xK_eth)
    , ('\xf1' , xK_ntilde)
    , ('\xf2' , xK_ograve)
    , ('\xf3' , xK_oacute)
    , ('\xf4' , xK_ocircumflex)
    , ('\xf5' , xK_otilde)
    , ('\xf6' , xK_odiaeresis)
    , ('\xf7' , xK_division)
    , ('\xf8' , xK_oslash)
    , ('\xf9' , xK_ugrave)
    , ('\xfa' , xK_uacute)
    , ('\xfb' , xK_ucircumflex)
    , ('\xfc' , xK_udiaeresis)
    , ('\xfd' , xK_yacute)
    , ('\xfe' , xK_thorn)
    , ('\xff' , xK_ydiaeresis)
    --, ('\''        , xK_quoteright) --duplicate
    --, ('`'         , xK_quoteleft) --duplicate
    --, ('\xd0'               , xK_Eth) -- duplicate
    --, ('\xde'             , xK_Thorn) --duplicate
    ]

-- Graphics.Graphics.X11.stringToKeysym string == keySym
multiCharkeys :: [(String, KeySym)]
multiCharkeys =
    [ ("VoidSymbol"        , xK_VoidSymbol)
    , ("BackSpace"         , xK_BackSpace)
    , ("Tab"               , xK_Tab)
    , ("Linefeed"          , xK_Linefeed)
    , ("Clear"             , xK_Clear)
    , ("Return"            , xK_Return)
    , ("Pause"             , xK_Pause)
    , ("Scroll_Lock"       , xK_Scroll_Lock)
    , ("Sys_Req"           , xK_Sys_Req)
    , ("Escape"            , xK_Escape)
    , ("Delete"            , xK_Delete)
    , ("Multi_key"         , xK_Multi_key)
    , ("Codeinput"         , xK_Codeinput)
    , ("SingleCandidate"   , xK_SingleCandidate)
    , ("MultipleCandidate" , xK_MultipleCandidate)
    , ("PreviousCandidate" , xK_PreviousCandidate)
    , ("Home"              , xK_Home)
    , ("Left"              , xK_Left)
    , ("Up"                , xK_Up)
    , ("Right"             , xK_Right)
    , ("Down"              , xK_Down)
    --, ("Prior"             , xK_Prior)
    , ("Page_Up"           , xK_Page_Up)
    --, ("Next"              , xK_Next)
    , ("Page_Down"         , xK_Page_Down)
    , ("End"               , xK_End)
    , ("Begin"             , xK_Begin)
    , ("Select"            , xK_Select)
    , ("Print"             , xK_Print)
    , ("Execute"           , xK_Execute)
    , ("Insert"            , xK_Insert)
    , ("Undo"              , xK_Undo)
    , ("Redo"              , xK_Redo)
    , ("Menu"              , xK_Menu)
    , ("Find"              , xK_Find)
    , ("Cancel"            , xK_Cancel)
    , ("Help"              , xK_Help)
    , ("Break"             , xK_Break)
    --, ("Mode_switch"       , xK_Mode_switch)
    , ("script_switch"     , xK_script_switch)
    , ("Num_Lock"          , xK_Num_Lock)
    , ("KP_Space"          , xK_KP_Space)
    , ("KP_Tab"            , xK_KP_Tab)
    , ("KP_Enter"          , xK_KP_Enter)
    , ("KP_F1"             , xK_KP_F1)
    , ("KP_F2"             , xK_KP_F2)
    , ("KP_F3"             , xK_KP_F3)
    , ("KP_F4"             , xK_KP_F4)
    , ("KP_Home"           , xK_KP_Home)
    , ("KP_Left"           , xK_KP_Left)
    , ("KP_Up"             , xK_KP_Up)
    , ("KP_Right"          , xK_KP_Right)
    , ("KP_Down"           , xK_KP_Down)
    --, ("KP_Prior"          , xK_KP_Prior)
    , ("KP_Page_Up"        , xK_KP_Page_Up)
    --, ("KP_Next"           , xK_KP_Next)
    , ("KP_Page_Down"      , xK_KP_Page_Down)
    , ("KP_End"            , xK_KP_End)
    , ("KP_Begin"          , xK_KP_Begin)
    , ("KP_Insert"         , xK_KP_Insert)
    , ("KP_Delete"         , xK_KP_Delete)
    , ("KP_Equal"          , xK_KP_Equal)
    , ("KP_Multiply"       , xK_KP_Multiply)
    , ("KP_Add"            , xK_KP_Add)
    , ("KP_Separator"      , xK_KP_Separator)
    , ("KP_Subtract"       , xK_KP_Subtract)
    , ("KP_Decimal"        , xK_KP_Decimal)
    , ("KP_Divide"         , xK_KP_Divide)
    , ("KP_0"              , xK_KP_0)
    , ("KP_1"              , xK_KP_1)
    , ("KP_2"              , xK_KP_2)
    , ("KP_3"              , xK_KP_3)
    , ("KP_4"              , xK_KP_4)
    , ("KP_5"              , xK_KP_5)
    , ("KP_6"              , xK_KP_6)
    , ("KP_7"              , xK_KP_7)
    , ("KP_8"              , xK_KP_8)
    , ("KP_9"              , xK_KP_9)
    , ("F1"                , xK_F1)
    , ("F2"                , xK_F2)
    , ("F3"                , xK_F3)
    , ("F4"                , xK_F4)
    , ("F5"                , xK_F5)
    , ("F6"                , xK_F6)
    , ("F7"                , xK_F7)
    , ("F8"                , xK_F8)
    , ("F9"                , xK_F9)
    , ("F10"               , xK_F10)
    , ("F11"               , xK_F11)
    --, ("L1"                , xK_L1)
    , ("F12"               , xK_F12)
    --, ("L2"                , xK_L2)
    , ("F13"               , xK_F13)
    --, ("L3"                , xK_L3)
    , ("F14"               , xK_F14)
    --, ("L4"                , xK_L4)
    , ("F15"               , xK_F15)
    --, ("L5"                , xK_L5)
    , ("F16"               , xK_F16)
    --, ("L6"                , xK_L6)
    , ("F17"               , xK_F17)
    --, ("L7"                , xK_L7)
    , ("F18"               , xK_F18)
    --, ("L8"                , xK_L8)
    , ("F19"               , xK_F19)
    --, ("L9"                , xK_L9)
    , ("F20"               , xK_F20)
    --, ("L10"               , xK_L10)
    , ("F21"               , xK_F21)
    --, ("R1"                , xK_R1)
    , ("F22"               , xK_F22)
    --, ("R2"                , xK_R2)
    , ("F23"               , xK_F23)
    --, ("R3"                , xK_R3)
    , ("F24"               , xK_F24)
    --, ("R4"                , xK_R4)
    , ("F25"               , xK_F25)
    --, ("R5"                , xK_R5)
    , ("F26"               , xK_F26)
    --, ("R6"                , xK_R6)
    , ("F27"               , xK_F27)
    --, ("R7"                , xK_R7)
    , ("F28"               , xK_F28)
    --, ("R8"                , xK_R8)
    , ("F29"               , xK_F29)
    --, ("R9"                , xK_R9)
    , ("F30"               , xK_F30)
    --, ("R10"               , xK_R10)
    , ("F31"               , xK_F31)
    --, ("R11"               , xK_R11)
    , ("F32"               , xK_F32)
    --, ("R12"               , xK_R12)
    , ("F33"               , xK_F33)
    --, ("R13"               , xK_R13)
    , ("F34"               , xK_F34)
    --, ("R14"               , xK_R14)
    , ("F35"               , xK_F35)
    --, ("R15"               , xK_R15)
    , ("Shift_L"           , xK_Shift_L)
    , ("Shift_R"           , xK_Shift_R)
    , ("Control_L"         , xK_Control_L)
    , ("Control_R"         , xK_Control_R)
    , ("Caps_Lock"         , xK_Caps_Lock)
    , ("Shift_Lock"        , xK_Shift_Lock)
    , ("Meta_L"            , xK_Meta_L)
    , ("Meta_R"            , xK_Meta_R)
    , ("Alt_L"             , xK_Alt_L)
    , ("Alt_R"             , xK_Alt_R)
    , ("Super_L"           , xK_Super_L)
    , ("Super_R"           , xK_Super_R)
    , ("Hyper_L"           , xK_Hyper_L)
    , ("Hyper_R"           , xK_Hyper_R)
    , ("space"             , xK_space)
    ]

modKeys :: KeyMask -> [(String, KeyMask)]
modKeys mod =
    [ ("M-"  , mod)
    , ("S-"  , shiftMask)
    , ("C-"  , controlMask)
    , ("M1-" , mod1Mask)
    , ("M2-" , mod2Mask)
    , ("M3-" , mod3Mask)
    , ("M4-" , mod4Mask)
    , ("M5-" , mod5Mask)
    ]

-- from /usr/include/X11/XF86_keysym.h
multimediaKeys :: [(String, KeySym)]
multimediaKeys = [ ("XF86_ModeLock"          , 0x1008FF01) -- Mode Switch Lock
                 , ("XF86_MonBrightnessUp"   , 0x1008FF02)  -- Monitor/panel brightness
                 , ("XF86_MonBrightnessDown" , 0x1008FF03)  -- Monitor/panel brightness
                 , ("XF86_KbdLightOnOff"     , 0x1008FF04)  -- Keyboards may be lit
                 , ("XF86_KbdBrightnessUp"   , 0x1008FF05)  -- Keyboards may be lit
                 , ("XF86_KbdBrightnessDown" , 0x1008FF06)  -- Keyboards may be lit
                 , ("XF86_Standby"           , 0x1008FF10)   -- System into standby mode
                 , ("XF86_AudioLowerVolume"  , 0x1008FF11)   -- Volume control down
                 , ("XF86_AudioMute"         , 0x1008FF12)   -- Mute sound from the system
                 , ("XF86_AudioRaiseVolume"  , 0x1008FF13)   -- Volume control up
                 , ("XF86_AudioPlay"         , 0x1008FF14)   -- Start playing of audio >
                 , ("XF86_AudioStop"         , 0x1008FF15)   -- Stop playing audio
                 , ("XF86_AudioPrev"         , 0x1008FF16)   -- Previous track
                 , ("XF86_AudioNext"         , 0x1008FF17)   -- Next track
                 , ("XF86_HomePage"          , 0x1008FF18)   -- Display user's home page
                 , ("XF86_Mail"              , 0x1008FF19)   -- Invoke user's mail program
                 , ("XF86_Start"             , 0x1008FF1A)   -- Start application
                 , ("XF86_Search"            , 0x1008FF1B)   -- Search
                 , ("XF86_AudioRecord"       , 0x1008FF1C)   -- Record audio application
                 , ("XF86_Calculator"        , 0x1008FF1D)   -- Invoke calculator program
                 , ("XF86_Memo"              , 0x1008FF1E)   -- Invoke Memo taking program
                 , ("XF86_ToDoList"          , 0x1008FF1F)   -- Invoke To Do List program
                 , ("XF86_Calendar"          , 0x1008FF20)   -- Invoke Calendar program
                 , ("XF86_PowerDown"         , 0x1008FF21)   -- Deep sleep the system
                 , ("XF86_ContrastAdjust"    , 0x1008FF22)   -- Adjust screen contrast
                 , ("XF86_RockerUp"          , 0x1008FF23)   -- Rocker switches exist up
                 , ("XF86_RockerDown"        , 0x1008FF24)   -- and down
                 , ("XF86_RockerEnter"       , 0x1008FF25)   -- and let you press them
                 , ("XF86_Back"              , 0x1008FF26)   -- Like back on a browser
                 , ("XF86_Forward"           , 0x1008FF27)   -- Like forward on a browser
                 , ("XF86_Stop"              , 0x1008FF28)   -- Stop current operation
                 , ("XF86_Refresh"           , 0x1008FF29)   -- Refresh the page
                 , ("XF86_PowerOff"          , 0x1008FF2A)   -- Power off system entirely
                 , ("XF86_WakeUp"            , 0x1008FF2B)   -- Wake up system from sleep
                 , ("XF86_Eject"             , 0x1008FF2C)   -- Eject device (e.g. DVD)
                 , ("XF86_ScreenSaver"       , 0x1008FF2D)   -- Invoke screensaver
                 , ("XF86_WWW"               , 0x1008FF2E)   -- Invoke web browser
                 , ("XF86_Sleep"             , 0x1008FF2F)   -- Put system to sleep
                 , ("XF86_Favorites"         , 0x1008FF30)   -- Show favorite locations
                 , ("XF86_AudioPause"        , 0x1008FF31)   -- Pause audio playing
                 , ("XF86_AudioMedia"        , 0x1008FF32)   -- Launch media collection app
                 , ("XF86_MyComputer"        , 0x1008FF33)   -- Display "My Computer" window
                 , ("XF86_VendorHome"        , 0x1008FF34)   -- Display vendor home web site
                 , ("XF86_LightBulb"         , 0x1008FF35)   -- Light bulb keys exist
                 , ("XF86_Shop"              , 0x1008FF36)   -- Display shopping web site
                 , ("XF86_History"           , 0x1008FF37)   -- Show history of web surfing
                 , ("XF86_OpenURL"           , 0x1008FF38)   -- Open selected URL
                 , ("XF86_AddFavorite"       , 0x1008FF39)   -- Add URL to favorites list
                 , ("XF86_HotLinks"          , 0x1008FF3A)   -- Show "hot" links
                 , ("XF86_BrightnessAdjust"  , 0x1008FF3B)   -- Invoke brightness adj. UI
                 , ("XF86_Finance"           , 0x1008FF3C)   -- Display financial site
                 , ("XF86_Community"         , 0x1008FF3D)   -- Display user's community
                 , ("XF86_AudioRewind"       , 0x1008FF3E)   -- "rewind" audio track
                 , ("XF86_BackForward"       , 0x1008FF3F)   -- ???
                 , ("XF86_Launch0"           , 0x1008FF40)   -- Launch Application
                 , ("XF86_Launch1"           , 0x1008FF41)   -- Launch Application
                 , ("XF86_Launch2"           , 0x1008FF42)   -- Launch Application
                 , ("XF86_Launch3"           , 0x1008FF43)   -- Launch Application
                 , ("XF86_Launch4"           , 0x1008FF44)   -- Launch Application
                 , ("XF86_Launch5"           , 0x1008FF45)   -- Launch Application
                 , ("XF86_Launch6"           , 0x1008FF46)   -- Launch Application
                 , ("XF86_Launch7"           , 0x1008FF47)   -- Launch Application
                 , ("XF86_Launch8"           , 0x1008FF48)   -- Launch Application
                 , ("XF86_Launch9"           , 0x1008FF49)   -- Launch Application
                 , ("XF86_LaunchA"           , 0x1008FF4A)   -- Launch Application
                 , ("XF86_LaunchB"           , 0x1008FF4B)   -- Launch Application
                 , ("XF86_LaunchC"           , 0x1008FF4C)   -- Launch Application
                 , ("XF86_LaunchD"           , 0x1008FF4D)   -- Launch Application
                 , ("XF86_LaunchE"           , 0x1008FF4E)   -- Launch Application
                 , ("XF86_LaunchF"           , 0x1008FF4F)   -- Launch Application
                 , ("XF86_ApplicationLeft"   , 0x1008FF50)   -- switch to application, left
                 , ("XF86_ApplicationRight"  , 0x1008FF51)   -- switch to application, right
                 , ("XF86_Book"              , 0x1008FF52)   -- Launch bookreader
                 , ("XF86_CD"                , 0x1008FF53)   -- Launch CD/DVD player
                 , ("XF86_Calculater"        , 0x1008FF54)   -- Launch Calculater
                 , ("XF86_Clear"             , 0x1008FF55)   -- Clear window, screen
                 , ("XF86_Close"             , 0x1008FF56)   -- Close window
                 , ("XF86_Copy"              , 0x1008FF57)   -- Copy selection
                 , ("XF86_Cut"               , 0x1008FF58)   -- Cut selection
                 , ("XF86_Display"           , 0x1008FF59)   -- Output switch key
                 , ("XF86_DOS"               , 0x1008FF5A)   -- Launch DOS (emulation)
                 , ("XF86_Documents"         , 0x1008FF5B)   -- Open documents window
                 , ("XF86_Excel"             , 0x1008FF5C)   -- Launch spread sheet
                 , ("XF86_Explorer"          , 0x1008FF5D)   -- Launch file explorer
                 , ("XF86_Game"              , 0x1008FF5E)   -- Launch game
                 , ("XF86_Go"                , 0x1008FF5F)   -- Go to URL
                 , ("XF86_iTouch"            , 0x1008FF60)   -- Logitch iTouch- don't use
                 , ("XF86_LogOff"            , 0x1008FF61)   -- Log off system
                 , ("XF86_Market"            , 0x1008FF62)   -- ??
                 , ("XF86_Meeting"           , 0x1008FF63)   -- enter meeting in calendar
                 , ("XF86_MenuKB"            , 0x1008FF65)   -- distingush keyboard from PB
                 , ("XF86_MenuPB"            , 0x1008FF66)   -- distinuish PB from keyboard
                 , ("XF86_MySites"           , 0x1008FF67)   -- Favourites
                 , ("XF86_New"               , 0x1008FF68)   -- New (folder, document...
                 , ("XF86_News"              , 0x1008FF69)   -- News
                 , ("XF86_OfficeHome"        , 0x1008FF6A)   -- Office home (old Staroffice)
                 , ("XF86_Open"              , 0x1008FF6B)   -- Open
                 , ("XF86_Option"            , 0x1008FF6C)   -- ??
                 , ("XF86_Paste"             , 0x1008FF6D)   -- Paste
                 , ("XF86_Phone"             , 0x1008FF6E)   -- Launch phone; dial number
                 , ("XF86_Q"                 , 0x1008FF70)   -- Compaq's Q - don't use
                 , ("XF86_Reply"             , 0x1008FF72)   -- Reply e.g., mail
                 , ("XF86_Reload"            , 0x1008FF73)   -- Reload web page, file, etc.
                 , ("XF86_RotateWindows"     , 0x1008FF74)   -- Rotate windows e.g. xrandr
                 , ("XF86_RotationPB"        , 0x1008FF75)   -- don't use
                 , ("XF86_RotationKB"        , 0x1008FF76)   -- don't use
                 , ("XF86_Save"              , 0x1008FF77)   -- Save (file, document , state
                 , ("XF86_ScrollUp"          , 0x1008FF78)   -- Scroll window/contents up
                 , ("XF86_ScrollDown"        , 0x1008FF79)   -- Scrool window/contentd down
                 , ("XF86_ScrollClick"       , 0x1008FF7A)   -- Use XKB mousekeys instead
                 , ("XF86_Send"              , 0x1008FF7B)   -- Send mail, file, object
                 , ("XF86_Spell"             , 0x1008FF7C)   -- Spell checker
                 , ("XF86_SplitScreen"       , 0x1008FF7D)   -- Split window or screen
                 , ("XF86_Support"           , 0x1008FF7E)   -- Get support (??)
                 , ("XF86_TaskPane"          , 0x1008FF7F)   -- Show tasks
                 , ("XF86_Terminal"          , 0x1008FF80)   -- Launch terminal emulator
                 , ("XF86_Tools"             , 0x1008FF81)   -- toolbox of desktop/app.
                 , ("XF86_Travel"            , 0x1008FF82)   -- ??
                 , ("XF86_UserPB"            , 0x1008FF84)   -- ??
                 , ("XF86_User1KB"           , 0x1008FF85)   -- ??
                 , ("XF86_User2KB"           , 0x1008FF86)   -- ??
                 , ("XF86_Video"             , 0x1008FF87)   -- Launch video player
                 , ("XF86_WheelButton"       , 0x1008FF88)   -- button from a mouse wheel
                 , ("XF86_Word"              , 0x1008FF89)   -- Launch word processor
                 , ("XF86_Xfer"              , 0x1008FF8A)
                 , ("XF86_ZoomIn"            , 0x1008FF8B)   -- zoom in view, map, etc.
                 , ("XF86_ZoomOut"           , 0x1008FF8C)   -- zoom out view, map, etc.
                 , ("XF86_Away"              , 0x1008FF8D)   -- mark yourself as away
                 , ("XF86_Messenger"         , 0x1008FF8E)   -- as in instant messaging
                 , ("XF86_WebCam"            , 0x1008FF8F)   -- Launch web camera app.
                 , ("XF86_MailForward"       , 0x1008FF90)   -- Forward in mail
                 , ("XF86_Pictures"          , 0x1008FF91)   -- Show pictures
                 , ("XF86_Music"             , 0x1008FF92)   -- Launch music application
                 , ("XF86_Battery"           , 0x1008FF93)   -- Display battery information
                 , ("XF86_Bluetooth"         , 0x1008FF94)   -- Enable/disable Bluetooth
                 , ("XF86_WLAN"              , 0x1008FF95)   -- Enable/disable WLAN
                 , ("XF86_UWB"               , 0x1008FF96)   -- Enable/disable UWB
                 , ("XF86_AudioForward"      , 0x1008FF97)   -- fast-forward audio track
                 , ("XF86_AudioRepeat"       , 0x1008FF98)   -- toggle repeat mode
                 , ("XF86_AudioRandomPlay"   , 0x1008FF99)   -- toggle shuffle mode
                 , ("XF86_Subtitle"          , 0x1008FF9A)   -- cycle through subtitle
                 , ("XF86_AudioCycleTrack"   , 0x1008FF9B)   -- cycle through audio tracks
                 , ("XF86_CycleAngle"        , 0x1008FF9C)   -- cycle through angles
                 , ("XF86_FrameBack"         , 0x1008FF9D)   -- video: go one frame back
                 , ("XF86_FrameForward"      , 0x1008FF9E)   -- video: go one frame forward
                 , ("XF86_Time"              , 0x1008FF9F)   -- display, or shows an entry for time seeking
                 , ("XF86_Select"            , 0x1008FFA0)   -- Select button on joypads and remotes
                 , ("XF86_View"              , 0x1008FFA1)   -- Show a view options/properties
                 , ("XF86_TopMenu"           , 0x1008FFA2)   -- Go to a top-level menu in a video
                 , ("XF86_Red"               , 0x1008FFA3)   -- Red button
                 , ("XF86_Green"             , 0x1008FFA4)   -- Green button
                 , ("XF86_Yellow"            , 0x1008FFA5)   -- Yellow button
                 , ("XF86_Blue"              , 0x1008FFA6)   -- Blue button
                 , ("XF86_Suspend"           , 0x1008FFA7)   -- Sleep to RAM
                 , ("XF86_Hibernate"         , 0x1008FFA8)   -- Sleep to disk
                 , ("XF86_TouchpadToggle"    , 0x1008FFA9)   -- Toggle between touchpad/trackstick
                 , ("XF86_TouchpadOn"        , 0x1008FFB0)   -- The touchpad got switched on
                 , ("XF86_TouchpadOff"       , 0x1008FFB1)   -- The touchpad got switched off
                 , ("XF86_AudioMicMute"      , 0x1008FFB2)   -- Mute the Mic from the system
                 , ("XF86_Keyboard"          , 0x1008FFB3)   -- User defined keyboard related action
                 , ("XF86_WWAN"              , 0x1008FFB4)   -- Toggle WWAN (LTE, UMTS, etc.) radio
                 , ("XF86_RFKill"            , 0x1008FFB5)   -- Toggle radios on/off
                 , ("XF86_AudioPreset"       , 0x1008FFB6)   -- Select equalizer preset, e.g. theatre-mode
                 , ("XF86_Switch_VT_1"       , 0x1008FE01)
                 , ("XF86_Switch_VT_2"       , 0x1008FE02)
                 , ("XF86_Switch_VT_3"       , 0x1008FE03)
                 , ("XF86_Switch_VT_4"       , 0x1008FE04)
                 , ("XF86_Switch_VT_5"       , 0x1008FE05)
                 , ("XF86_Switch_VT_6"       , 0x1008FE06)
                 , ("XF86_Switch_VT_7"       , 0x1008FE07)
                 , ("XF86_Switch_VT_8"       , 0x1008FE08)
                 , ("XF86_Switch_VT_9"       , 0x1008FE09)
                 , ("XF86_Switch_VT_10"      , 0x1008FE0A)
                 , ("XF86_Switch_VT_11"      , 0x1008FE0B)
                 , ("XF86_Switch_VT_12"      , 0x1008FE0C)
                 , ("XF86_Ungrab"            , 0x1008FE20)   -- force ungrab
                 , ("XF86_ClearGrab"         , 0x1008FE21)   -- kill application with grab
                 , ("XF86_Next_VMode"        , 0x1008FE22)   -- next video mode available
                 , ("XF86_Prev_VMode"        , 0x1008FE23)   -- prev. video mode available
                 , ("XF86_LogWindowTree"     , 0x1008FE24)   -- print window tree to log
                 , ("XF86_LogGrabInfo"       , 0x1008FE25)   -- print all active grabs to log
                 ]

unMasks :: [KeyMask] -> KeyMask
unMasks = foldr (.|.) noModMask

toMask :: KeyMask -> [KeyMask]
toMask mask = filter (/= noModMask) $ map (.&. mask) mods
    where
      mods = [ shiftMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask, mod5Mask ]
keyZipToParser :: (a -> Parser a) -> [(a, b)] -> Parser b
keyZipToParser p = choice . map (\(x, y) -> p x >> return y)

parseKeys :: KeyMask -> Parser [(KeyMask, KeySym)]
parseKeys mod = sepBy1 (parseKey mod) (some space)

parseKey :: KeyMask -> Parser (KeyMask, KeySym)
parseKey mod = do
    mod' <- parseModMany mod
    keySym <- try (char '<' *> parseString <* char '>') <|> parseChar
    return (mod', keySym)

parseModMany :: KeyMask -> Parser KeyMask
parseModMany mod = unMasks <$> many (parseMod mod)

parseMod :: KeyMask -> Parser KeyMask
parseMod mod = keyZipToParser (try . string) (modKeys mod)

parseChar :: Parser KeySym
parseChar = keyZipToParser char singleCharKey

parseString :: Parser KeySym
parseString = parseMultiChar <|> parseMultimedia

parseMultiChar :: Parser KeySym
parseMultiChar = keyZipToParser (try . string) multiCharkeys

parseMultimedia :: Parser KeySym
parseMultimedia = keyZipToParser (try . string) multimediaKeys

parse :: KeyMask -> String -> [(KeyMask, KeySym)]
parse mod str = case runParser (parseKeys mod) str of
    (Just a, "") -> a
    _            -> []

parseKeymap :: KeyMask -> [(String, a)] -> [( [(KeyMask, KeySym)], a )]
parseKeymap mod = filter (not . null . fst) . map (first $ parse mod)

keyToString :: (KeyMask, KeySym) -> String
keyToString (mask, key) = case key' of
      (Just k) -> mask' ++ fst k
      Nothing -> ""
    where
        mask' = concatMap f $ toMask mask
        f m = maybe "" fst $ find (\(_,y) -> y == m) (modKeys noModMask)
        key' = find (\(_,y) -> y == key) keyList
        keyList = map (\(x,y) -> ([x],y) ) singleCharKey ++ multiCharkeys ++ multimediaKeys

