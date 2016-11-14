{  $Id: lcltype.pp 51570 2016-02-10 15:35:03Z ondrej $  }
{
 /***************************************************************************
                                LCLType.pp
                                ----------
                             Component Library Windows Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
@author(Curtis White <cwhite@aracnet.com>)
@created(17-Oct-1999)
@lastmod(17-Oct-1999)

This unit is being created specifically for compatibility with Delphi. It
should only be used for constants and type definitions that are included in
the Delphi Windows unit. This is only done for compatibiltiy.

}

unit LCLType;

{$mode objfpc}{$H+}{$macro on}

interface


{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
{$IFDEF USE_UTF8BIDI_LCL}
  UTF8BIDI,
{$ENDIF USE_UTF8BIDI_LCL}
{$ifdef WINDOWS}
  windows,
{$endif WINDOWS}
  Classes, SysUtils;

{$ifdef WinCE}
  {$define extdecl := cdecl}
{$else}
  {$define extdecl := stdcall}
{$endif}

type
{$IFDEF USE_UTF8BIDI_LCL}
  TUTF8Char = UTF8BIDI.TUTF8Char;
{$ELSE USE_UTF8BIDI_LCL}
  TUTF8Char = String[7]; // UTF-8 character is at most 6 bytes plus a #0
{$ENDIF USE_UTF8BIDI_LCL}
  UINT = LongWord;
  UINT_PTR = PtrUInt;

  TTranslateString = type String;

  TCriticalSection = PtrUInt;
  PCriticalSection = ^TCriticalSection;

  TDockImageOperation = (disShow, disMove, disHide);

  // Types for native Handle integration

  TNativeHandleType = (nhtWindowsHWND, nhtX11TWindow, nhtCocoaNSWindow,
    nhtQtQWidget);
  TNativeHandleTypes = set of TNativeHandleType;

  TNativeCanvasType = (nctWindowsDC, nctLazCanvas);
  TNativeCanvasTypes = set of TNativeCanvasType;

  // Callback types for new LCLIntf dialogs

  // -1 indicates that the dialog was canceled and no item selected
  TOnShowSelectItemDialogResult = procedure (ASelectedItem: Integer) of object;

  {$ifndef WINDOWS}
  PInt = ^integer;
  THandle = type PtrUInt; // define our own, because the SysUtils.THandle = System.THandle is a longint
  HANDLE = THandle;
  PHandle = ^THandle;

  { Provided for compatibility with Windows registry ONLY }
  HKEY  = Integer;
  HDC   = type THandle;
  HHOOK = type THandle;
  HFONT = type THandle;
  HGDIOBJ = type THandle;
  HPEN  = type THandle;
  HRGN  = type THandle;
  HINST = type THandle;
  HICON = type THandle;
  HIMAGELIST = type THandle;
  HCURSOR = HICON;
  HGLOBAL = type THandle;
  HWND    = type THandle;
  HMENU   = type THandle;
  HBITMAP = type THandle;
  HPALETTE = type THandle;
  HBRUSH = type THandle;
  HMONITOR = type THandle;

  Bool    = LongBool;
  Short   = SmallInt;
  PRect   = ^TRect;

  WPARAM = type PtrInt; //LongInt or Int64 on CPU64;
  LPARAM = type PtrInt; //LongInt or Int64 on CPU64;
  LRESULT = type PtrInt; //LongInt or Int64 on CPU64;

  ULONG_PTR = type PtrUInt;

{$else}
  HKEY  = Windows.HKEY;
  HDC   = Windows.HDC;
  HHOOK = Windows.HHOOK;
  HFONT = Windows.HFont;
  HGDIOBJ = Windows.HGDIOBJ;
  HPEN    = Windows.HPEN;
  HRGN    = Windows.HRGN;
  HINST   = Windows.HINST;
  HICON   = Windows.HICON;
  HIMAGELIST = Windows.HIMAGELIST;
  HCURSOR = HICON;
  BOOL    = Windows.BOOL;
  HGLOBAL = Windows.HGLOBAL;
  Short   = Windows.Short;
  HWND    = Windows.HWND;
  HMENU   = Windows.HMENU;
  HBITMAP = Windows.HBITMAP;
  HPALETTE = Windows.HPALETTE;
  HBRUSH = Windows.HBRUSH;
  HMONITOR = HANDLE;

  WPARAM = Windows.WPARAM;
  LPARAM = Windows.LPARAM;
  LRESULT = Windows.LRESULT;

{$endif}

  TLCLIntfHandle = type THandle;

  PHKEY = ^HKEY;

const
  INVALID_HANDLE_VALUE  = HANDLE(-1); // prior to 1.1 it was 0, see for example FileOpen

  MAXBYTE  = Byte($FF);
  MAXWORD  = Word($FFFF);
  MAXDWORD = DWord($FFFFFFFF);

  MINCHAR  = $80;
  MAXCHAR  = $7F;
  MINSHORT = $8000;
  MAXSHORT = $7FFF;
  MINLONG  = DWord($80000000);
  MAXLONG  = $7FFFFFFF;


const
//==============================================
// Binary raster operations
//==============================================
  R2_BLACK       =  1;  {  0   }
  R2_NOTMERGEPEN =  2;  { DPon }
  R2_MASKNOTPEN  =  3;  { DPna }
  R2_NOTCOPYPEN  =  4;  { PN   }
  R2_MASKPENNOT  =  5;  { PDna }
  R2_NOT         =  6;  { Dn   }
  R2_XORPEN      =  7;  { DPx  }
  R2_NOTMASKPEN  =  8;  { DPan }
  R2_MASKPEN     =  9;  { DPa  }
  R2_NOTXORPEN   =  10; { DPxn }
  R2_NOP         =  11; { D    }
  R2_MERGENOTPEN =  12; { DPno }
  R2_COPYPEN     =  13; { P    }
  R2_MERGEPENNOT =  14; { PDno }
  R2_MERGEPEN    =  15; { DPo  }
  R2_WHITE       =  16; { 1    }
  R2_LAST        =  16;

//==============================================
// Ternary raster operations
//==============================================
  SRCCOPY     = $00CC0020;     { dest = source                    }
  SRCPAINT    = $00EE0086;     { dest = source OR dest            }
  SRCAND      = $008800C6;     { dest = source AND dest           }
  SRCINVERT   = $00660046;     { dest = source XOR dest           }
  SRCERASE    = $00440328;     { dest = source AND (NOT dest )    }
  NOTSRCCOPY  = $00330008;     { dest = (NOT source)              }
  NOTSRCERASE = $001100A6;     { dest = (NOT src) AND (NOT dest)  }
  MERGECOPY   = $00C000CA;     { dest = (source AND pattern)      }
  MERGEPAINT  = $00BB0226;     { dest = (NOT source) OR dest      }
  PATCOPY     = $00F00021;     { dest = pattern                   }
  PATPAINT    = $00FB0A09;     { dest = DPSnoo                    }
  PATINVERT   = $005A0049;     { dest = pattern XOR dest          }
  DSTINVERT   = $00550009;     { dest = (NOT dest)                }
  BLACKNESS   = $00000042;     { dest = BLACK                     }
  WHITENESS   = $00FF0062;     { dest = WHITE                     }

type
  TKeyBoardState = array[0..255] of byte;

  PABC = ^TABC;

  _ABC = record
    abcA: Integer;
    abcB: UINT;
    abcC: Integer;
  end;
  TABC = _ABC;


const
  ETO_OPAQUE = 2;
  ETO_CLIPPED = 4;

  CS_VREDRAW = dword(1);
  CS_HREDRAW = dword(2);

//------------
// CombineRgn Mode flags
//------------
  RGN_AND = 1;
  RGN_OR = 2;
  RGN_XOR = 3;
  RGN_DIFF = 4;
  RGN_COPY = 5;

//------------
// DrawText flags
//------------
  DT_TOP = $0;
  DT_LEFT = $0;
  DT_CENTER = $1;
  DT_RIGHT = $2;
  DT_VCENTER = $4;
  DT_BOTTOM = $8;
  DT_WORDBREAK = $10;
  DT_SINGLELINE = $20;
  DT_EXPANDTABS = $40;
  DT_NOCLIP = $100;
  DT_CALCRECT = $400;
  DT_NOPREFIX = $800;
  DT_INTERNAL = $1000;
  DT_EDITCONTROL = $2000;
  DT_END_ELLIPSIS = $8000;
  DT_MODIFYSTRING = $10000;
  DT_RTLREADING =  $20000;

//==============================================
// Draw frame constants
//==============================================

//------------
// Draw frame control flags
//------------
  DFC_CAPTION = $01;
  DFC_MENU = $02;
  DFC_SCROLL = $03;
  DFC_BUTTON =  $04;

//------------
// Draw frame control Styles
//------------
  DFCS_BUTTONCHECK = 0;
  DFCS_BUTTONRADIOIMAGE = 1;
  DFCS_BUTTONRADIOMASK = 2;
  DFCS_BUTTONRADIO = 4;
  DFCS_BUTTON3STATE = 8;
  DFCS_BUTTONPUSH = 16;

  DFCS_CAPTIONCLOSE = 0;
  DFCS_CAPTIONMIN = 1;
  DFCS_CAPTIONMAX = 2;
  DFCS_CAPTIONRESTORE = 3;
  DFCS_CAPTIONHELP = 4;

  DFCS_MENUARROW = 0;
  DFCS_MENUCHECK = 1;
  DFCS_MENUBULLET = 2;

  DFCS_SCROLLDOWN = 1;
  DFCS_SCROLLLEFT = 2;
  DFCS_SCROLLRIGHT = 3;
  DFCS_SCROLLCOMBOBOX = 5;
  DFCS_SCROLLSIZEGRIP = 8;
  DFCS_SCROLLUP = 0;

  DFCS_INACTIVE = 256;
  DFCS_PUSHED = 512;
  DFCS_CHECKED = 1024;
  DFCS_TRANSPARENT = 2048;
  DFCS_HOT = 4096;
  DFCS_ADJUSTRECT = 8192;
  DFCS_FLAT = 16384;
  DFCS_MONO = 32768;

  HTERROR = -2;
  HTTRANSPARENT = -1;
  HTNOWHERE = 0;
  HTCLIENT = 1;
  HTCAPTION = 2;

  MSGF_DIALOGBOX = 0;
  MSGF_MESSAGEBOX = 1;
  MSGF_MENU = 2;
  MSGF_MOVE = 3;
  MSGF_SIZE = 4;
  MSGF_SCROLLBAR = 5;
  MSGF_NEXTWINDOW = 6;


// PEEKMESSAGE stuff
  PM_Noremove = 0;
  PM_Remove = 1;

//==============================================
// Menu constants
//==============================================

  MF_BYCOMMAND       = 0;
  MF_BYPOSITION      = $400;
  MF_BYHANDLE        = $800; // introduced by the LCL

  MF_SEPARATOR       = $800;

  MF_ENABLED         = 0;
  MF_GRAYED          = 1;
  MF_DISABLED        = 2;

  MF_UNCHECKED       = 0;
  MF_CHECKED         = 8;
  MF_USECHECKBITMAPS = $200;

  MF_STRING          = 0;
  MF_BITMAP          = 4;
  MF_OWNERDRAW       = $100;

  MF_POPUP           = $10;
  MF_MENUBARBREAK    = $20;
  MF_MENUBREAK       = $40;

  MF_UNHILITE        = 0;
  MF_HILITE          = $80;

  MF_DEFAULT         = $1000;
  MF_SYSMENU         = $2000;
  MF_HELP            = $4000;
  MF_RIGHTJUSTIFY    = $4000;

  MF_MOUSESELECT     = $8000;

  MF_UNSUPPORTED     = $FFFFFFFF;

//==============================================
// Keyboard constants
//==============================================

//------------
// KeyFlags (High word part !!!)
//------------
  KF_EXTENDED = $100;
  KF_DLGMODE = $800;
  KF_MENUMODE = $1000;
  KF_ALTDOWN = $2000;
  KF_REPEAT = $4000;
  KF_UP = $8000;

// TShortCut additions:
  scMeta = $1000;

//-------------
// Virtual keys
//-------------
//
// Basic keys up to $FF have values and meaning compatible with the Windows API as described here:
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/WinUI/WindowsUserInterface/UserInput/VirtualKeyCodes.asp
//
// Starting with $100 and upwards the key constants are LCL additions
//
  VK_UNKNOWN    = 0; // defined by LCL
  VK_LBUTTON    = 1;
  VK_RBUTTON    = 2;
  VK_CANCEL     = 3;
  VK_MBUTTON    = 4;
  VK_XBUTTON1   = 5;
  VK_XBUTTON2   = 6;
  VK_BACK       = 8;  // The "Backspace" key, dont confuse with the
                      // Android BACK key which is mapped to VK_ESCAPE
  VK_TAB        = 9;
  VK_CLEAR      = 12;
  VK_RETURN     = 13; // The "Enter" key, also used for a keypad center press
  VK_SHIFT      = 16; // See also VK_LSHIFT, VK_RSHIFT
  VK_CONTROL    = 17; // See also VK_LCONTROL, VK_RCONTROL
  VK_MENU       = 18; // The ALT key. Also called "Option" in Mac OS X. See also VK_LMENU, VK_RMENU
  VK_PAUSE      = 19; // Pause/Break key
  VK_CAPITAL    = 20; // CapsLock key
  VK_KANA       = 21;
  VK_HANGUL     = 21;
  VK_JUNJA      = 23;
  VK_FINAL      = 24;
  VK_HANJA      = 25;
  VK_KANJI      = 25;
  VK_ESCAPE     = 27; // Also used for the hardware Back key in Android
  VK_CONVERT    = 28;
  VK_NONCONVERT = 29;
  VK_ACCEPT     = 30;
  VK_MODECHANGE = 31;
  VK_SPACE      = 32;
  VK_PRIOR      = 33; // Page Up
  VK_NEXT       = 34; // Page Down
  VK_END        = 35;
  VK_HOME       = 36;
  VK_LEFT       = 37;
  VK_UP         = 38;
  VK_RIGHT      = 39;
  VK_DOWN       = 40;
  VK_SELECT     = 41;
  VK_PRINT      = 42; // PrintScreen key
  VK_EXECUTE    = 43;
  VK_SNAPSHOT   = 44;
  VK_INSERT     = 45;
  VK_DELETE     = 46;
  VK_HELP       = 47;
  VK_0          = $30;
  VK_1          = $31;
  VK_2          = $32;
  VK_3          = $33;
  VK_4          = $34;
  VK_5          = $35;
  VK_6          = $36;
  VK_7          = $37;
  VK_8          = $38;
  VK_9          = $39;
  //3A-40 Undefined
  VK_A          = $41;
  VK_B          = $42;
  VK_C          = $43;
  VK_D          = $44;
  VK_E          = $45;
  VK_F          = $46;
  VK_G          = $47;
  VK_H          = $48;
  VK_I          = $49;
  VK_J          = $4A;
  VK_K          = $4B;
  VK_L          = $4C;
  VK_M          = $4D;
  VK_N          = $4E;
  VK_O          = $4F;
  VK_P          = $50;
  VK_Q          = $51;
  VK_R          = $52;
  VK_S          = $53;
  VK_T          = $54;
  VK_U          = $55;
  VK_V          = $56;
  VK_W          = $57;
  VK_X          = $58;
  VK_Y          = $59;
  VK_Z          = $5A;

  VK_LWIN       = $5B; // In Mac OS X this is the Apple, or Command key. Windows Key in PC keyboards
  VK_RWIN       = $5C; // In Mac OS X this is the Apple, or Command key. Windows Key in PC keyboards
  VK_APPS       = $5D; // The PopUp key in PC keyboards
  // $5E reserved
  VK_SLEEP      = $5F;

  VK_NUMPAD0    = 96; // $60
  VK_NUMPAD1    = 97;
  VK_NUMPAD2    = 98;
  VK_NUMPAD3    = 99;
  VK_NUMPAD4    = 100;
  VK_NUMPAD5    = 101;
  VK_NUMPAD6    = 102;
  VK_NUMPAD7    = 103;
  VK_NUMPAD8    = 104;
  VK_NUMPAD9    = 105;
  VK_MULTIPLY   = 106; // VK_MULTIPLY up to VK_DIVIDE are usually in the numeric keypad in PC keyboards
  VK_ADD        = 107;
  VK_SEPARATOR  = 108;
  VK_SUBTRACT   = 109;
  VK_DECIMAL    = 110;
  VK_DIVIDE     = 111;
  VK_F1         = 112;
  VK_F2         = 113;
  VK_F3         = 114;
  VK_F4         = 115;
  VK_F5         = 116;
  VK_F6         = 117;
  VK_F7         = 118;
  VK_F8         = 119;
  VK_F9         = 120;
  VK_F10        = 121;
  VK_F11        = 122;
  VK_F12        = 123;
  VK_F13        = 124;
  VK_F14        = 125;
  VK_F15        = 126;
  VK_F16        = 127;
  VK_F17        = 128;
  VK_F18        = 129;
  VK_F19        = 130;
  VK_F20        = 131;
  VK_F21        = 132;
  VK_F22        = 133;
  VK_F23        = 134;
  VK_F24        = 135; // $87

  // $88-$8F unassigned

  VK_NUMLOCK    = $90;
  VK_SCROLL     = $91;

  // $92-$96  OEM specific
  // $97-$9F  Unassigned

  // not in VCL defined:
  //MWE: And should not be used.
  //     The keys they are on map to another VK

//  VK_SEMICOLON  = 186;
//  VK_EQUAL      = 187; // $BB
//  VK_COMMA      = 188;
//  VK_POINT      = 190;
//  VK_SLASH      = 191;
//  VK_AT         = 192;

  // VK_L & VK_R - left and right Alt, Ctrl and Shift virtual keys.
  // When Application.ExtendedKeysSupport is false, these keys are
  // used only as parameters to GetAsyncKeyState() and GetKeyState().
  // No other API or message will distinguish left and right keys in this way
  //
  // When Application.ExtendedKeysSupport is true, these keys will be sent
  // on KeyDown / KeyUp instead of the generic VK_SHIFT, VK_CONTROL, etc.
  VK_LSHIFT     = $A0;
  VK_RSHIFT     = $A1;
  VK_LCONTROL   = $A2;
  VK_RCONTROL   = $A3;
  VK_LMENU      = $A4; // Left ALT key (also named Option in Mac OS X)
  VK_RMENU      = $A5; // Right ALT key (also named Option in Mac OS X)

  VK_BROWSER_BACK        = $A6;
  VK_BROWSER_FORWARD     = $A7;
  VK_BROWSER_REFRESH     = $A8;
  VK_BROWSER_STOP        = $A9;
  VK_BROWSER_SEARCH      = $AA;
  VK_BROWSER_FAVORITES   = $AB;
  VK_BROWSER_HOME        = $AC;
  VK_VOLUME_MUTE         = $AD;
  VK_VOLUME_DOWN         = $AE;
  VK_VOLUME_UP           = $AF;
  VK_MEDIA_NEXT_TRACK    = $B0;
  VK_MEDIA_PREV_TRACK    = $B1;
  VK_MEDIA_STOP          = $B2;
  VK_MEDIA_PLAY_PAUSE    = $B3;
  VK_LAUNCH_MAIL         = $B4;
  VK_LAUNCH_MEDIA_SELECT = $B5;
  VK_LAUNCH_APP1         = $B6;
  VK_LAUNCH_APP2         = $B7;

  // VK_OEM keys are utilized only when Application.ExtendedKeysSupport is false

  // $B8-$B9 Reserved
  VK_OEM_1               = $BA; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the ';:' key
  VK_OEM_PLUS            = $BB; // For any country/region, the '+' key
  VK_OEM_COMMA           = $BC; // For any country/region, the ',' key
  VK_OEM_MINUS           = $BD; // For any country/region, the '-' key
  VK_OEM_PERIOD          = $BE; // For any country/region, the '.' key
  VK_OEM_2               = $BF; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '/?' key
  VK_OEM_3               = $C0; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '`~' key
  // $C1-$D7 Reserved
  // $D8-$DA Unassigned
  VK_OEM_4               = $DB; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '[{' key
  VK_OEM_5               = $DC; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '\|' key
  VK_OEM_6               = $DD; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the ']}' key
  VK_OEM_7               = $DE; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the 'single-quote/double-quote' key
  VK_OEM_8               = $DF; // Used for miscellaneous characters; it can vary by keyboard.

  // $E0 Reserved
  // $E1 OEM specific
  VK_OEM_102             = $E2; // Either the angle bracket key or the backslash key on the RT 102-key keyboard

  // $E3-$E4 OEM specific

  VK_PROCESSKEY          = $E7; // IME Process key

  // $E8 Unassigned
  // $E9-$F5 OEM specific

  VK_ATTN       = $F6;
  VK_CRSEL      = $F7;
  VK_EXSEL      = $F8;
  VK_EREOF      = $F9;
  VK_PLAY       = $FA;
  VK_ZOOM       = $FB;
  VK_NONAME     = $FC;
  VK_PA1        = $FD;
  VK_OEM_CLEAR  = $FE;

  VK_HIGHESTVALUE = $FFFF;
  VK_UNDEFINED  = $FF; // defined by LCL

//==============================================
// LCL aliases for more clear naming of keys
//==============================================

  VK_LCL_EQUAL       = VK_OEM_PLUS;  // The "=+" Key
  VK_LCL_COMMA       = VK_OEM_COMMA; // The ",<" Key
  VK_LCL_POINT       = VK_OEM_PERIOD;// The ".>" Key
  VK_LCL_SLASH       = VK_OEM_2;     // The "/?" Key
  VK_LCL_SEMI_COMMA  = VK_OEM_1;     // The ";:" Key
  VK_LCL_MINUS       = VK_OEM_MINUS; // The "-_" Key
  VK_LCL_OPEN_BRAKET = VK_OEM_4;     // The "[{" Key
  VK_LCL_CLOSE_BRAKET= VK_OEM_6;     // The "]}" Key
  VK_LCL_BACKSLASH   = VK_OEM_5;     // The "\|" Key
  VK_LCL_TILDE       = VK_OEM_3;     // The "`~" Key
  VK_LCL_QUOTE       = VK_OEM_7;     // The "'"" Key

  VK_LCL_ALT        = VK_MENU;
  VK_LCL_LALT       = VK_LMENU;
  VK_LCL_RALT       = VK_RMENU;

  VK_LCL_CAPSLOCK   = VK_CAPITAL;

//==============================================
// New LCL defined keys
//==============================================

  VK_LCL_POWER      = $100;
  VK_LCL_CALL       = $101;
  VK_LCL_ENDCALL    = $102;
  VK_LCL_AT     = $103; // Not equivalent to anything < $FF, will only be sent by a primary "@" key
                        // but not for a @ key as secondary action of a "2" key for example

//==============================================
//
//==============================================

const

  { 3D border styles }
  BDR_RAISEDOUTER = 1;
  BDR_SUNKENOUTER = 2;
  BDR_RAISEDINNER = 4;
  BDR_SUNKENINNER = 8;

  BDR_OUTER = 3;
  BDR_INNER = 12;
  BDR_RAISED = 5;
  BDR_SUNKEN = 10;

  EDGE_BUMP = (BDR_RAISEDOUTER or BDR_SUNKENINNER);
  EDGE_ETCHED = (BDR_SUNKENOUTER or BDR_RAISEDINNER);
  EDGE_RAISED = (BDR_RAISEDOUTER or BDR_RAISEDINNER);
  EDGE_SUNKEN = (BDR_SUNKENOUTER or BDR_SUNKENINNER);

  { Border flags }
  BF_LEFT = 1;
  BF_TOP = 2;
  BF_RIGHT = 4;
  BF_BOTTOM = 8;

  BF_TOPLEFT = (BF_TOP or BF_LEFT);
  BF_TOPRIGHT = (BF_TOP or BF_RIGHT);
  BF_BOTTOMLEFT = (BF_BOTTOM or BF_LEFT);
  BF_BOTTOMRIGHT = (BF_BOTTOM or BF_RIGHT);
  BF_RECT = (BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM);
  BF_DIAGONAL = $10;

  BF_DIAGONAL_ENDTOPRIGHT = (BF_DIAGONAL or BF_TOP or BF_RIGHT);
  BF_DIAGONAL_ENDTOPLEFT = (BF_DIAGONAL or BF_TOP or BF_LEFT);
  BF_DIAGONAL_ENDBOTTOMLEFT = (BF_DIAGONAL or BF_BOTTOM or BF_LEFT);
  BF_DIAGONAL_ENDBOTTOMRIGHT = (BF_DIAGONAL or BF_BOTTOM or BF_RIGHT);

  BF_MIDDLE = $800;   { Fill in the middle }
  BF_SOFT = $1000;    { For softer buttons.  Not sure what the use is for this }
  BF_ADJUST = $2000;  { Calculate the space left over }
  BF_FLAT = $4000;    { For flat rather than 3D borders }
  BF_MONO = $8000;    { For monochrome borders }

{Dialog codes}
  DLGC_WANTARROWS = 1;
  DLGC_WANTTAB = 2;
  DLGC_WANTALLKEYS = 4;

  DLGC_WANTCHARS = $80;

{ScrollWindowEx}
  SW_SMOOTHSCROLL = 16;
  SW_ERASE = 4;
  SW_INVALIDATE = 2;
  SW_SCROLLCHILDREN = 1;

//==============================================
// owner drawn constants
//==============================================
  //CtlType
  //winuser.h
  ODT_MENU     = 1;
  ODT_LISTBOX  = 2;
  ODT_COMBOBOX = 3;
  ODT_BUTTON   = 4;
  ODT_STATIC   = 5;
  //commctrl.h
  ODT_HEADER   = 100;
  ODT_TAB      = 101;
  ODT_LISTVIEW = 102;

  //itemAction
  ODA_DRAWENTIRE = 1;
  ODA_SELECT     = 2;
  ODA_FOCUS      = 4;

  //itemState
  ODS_SELECTED     = $0001;
  ODS_GRAYED       = $0002;
  ODS_DISABLED     = $0004;
  ODS_CHECKED      = $0008;
  ODS_FOCUS        = $0010;
  ODS_DEFAULT      = $0020;
  ODS_HOTLIGHT     = $0040;
  ODS_INACTIVE     = $0080;
  ODS_NOACCEL      = $0100;
  ODS_NOFOCUSRECT  = $0200;
  ODS_COMBOBOXEDIT = $1000;



//==============================================
// GetWindowLong() constants
//==============================================
  GWL_WNDPROC = -4;
  GWL_HINSTANCE = -6;
  GWL_HWNDPARENT = -8;
  GWL_STYLE = -16;
  GWL_EXSTYLE = -20;
  GWL_USERDATA = -21;
  GWL_ID = -12;

  MB_OK = $00000000;
  MB_OKCANCEL = $00000001;
  MB_ABORTRETRYIGNORE = $00000002;
  MB_YESNOCANCEL = $00000003;
  MB_YESNO = $00000004;
  MB_RETRYCANCEL = $00000005;
  MB_CANCELTRYCONTINUE = $00000006; // not yet supported by MessageDlg

  MB_HELP = $00004000; // not yet supported by MessageDlg

  MB_ICONHAND = $00000010;
  MB_ICONQUESTION = $00000020;
  MB_ICONEXCLAMATION = $00000030;
  MB_ICONASTERISK = $00000040;
  MB_ICONWARNING = MB_ICONEXCLAMATION;
  MB_ICONERROR = MB_ICONHAND;
  MB_ICONSTOP = MB_ICONHAND;
  MB_ICONINFORMATION = MB_ICONASTERISK;

  IDOK = 1;     ID_OK = IDOK;
  IDCANCEL = 2; ID_CANCEL = IDCANCEL;
  IDABORT = 3;  ID_ABORT = IDABORT;
  IDRETRY = 4;  ID_RETRY = IDRETRY;
  IDIGNORE = 5; ID_IGNORE = IDIGNORE;
  IDYES = 6;    ID_YES = IDYES;
  IDNO = 7;     ID_NO = IDNO;
  IDCLOSE = 8;  ID_CLOSE = IDCLOSE;
  IDHELP = 9;   ID_HELP = IDHELP;

  MB_DEFBUTTON1 = $00000000;
  MB_DEFBUTTON2 = $00000100;
  MB_DEFBUTTON3 = $00000200;
  MB_DEFBUTTON4 = $00000300;

{Region Flags}
  Error = 0;
  NullRegion = 1;
  SimpleRegion = 2;
  ComplexRegion = 3;
  Region_Error = Error;


{Scroll bar stuff}
  SB_Horz = 0;
  SB_Vert = 1;
  SB_CTL = 2;
  SB_BOTH = 3;  //What would this do ??? it is not a used winapi constant

{Scroll Bar Commands}
  SB_LINEUP = 0;
  SB_LINELEFT = 0;
  SB_LINEDOWN = 1;
  SB_LINERIGHT = 1;
  SB_PAGEUP = 2;
  SB_PAGELEFT = 2;
  SB_PAGEDOWN = 3;
  SB_PAGERIGHT = 3;
  SB_THUMBPOSITION = 4;
  SB_THUMBTRACK = 5;
  SB_TOP = 6;
  SB_LEFT = 6;
  SB_BOTTOM = 7;
  SB_RIGHT = 7;
  SB_ENDSCROLL = 8;
  SB_POLICY_CONTINUOUS = 10;
  SB_POLICY_DISCONTINUOUS = 11;
  SB_POLICY_DELAYED=12;

  SBS_HORZ = 0;
  SBS_VERT = 1;
  SBS_TOPALIGN = 2;
  SBS_LEFTALIGN = 2;
  SBS_BOTTOMALIGN = 4;
  SBS_RIGHTALIGN = 4;
  SBS_SIZEBOXTOPLEFTALIGN = 2;
  SBS_SIZEBOXBOTTOMRIGHTALIGN = 4;
  SBS_SIZEBOX = 8;
  SBS_SIZEGRIP = $10;

  SIF_RANGE = 1;
  SIF_PAGE = 2;
  SIF_POS = 4;
  SIF_DISABLENOSCROLL = 8;
  SIF_TRACKPOS = $10;
  SIF_ALL = (SIF_RANGE or SIF_PAGE or SIF_POS or SIF_TRACKPOS);
  SIF_UPDATEPOLICY = $100;


{ WMWindowPosChanged message constants}
  SWP_NOSIZE            = $00001;
  SWP_NOMOVE            = $00002;
  SWP_NOZORDER          = $00004;
  SWP_NOREDRAW          = $00008;
  SWP_NOACTIVATE        = $00010;
  SWP_DRAWFRAME         = $00020;
  SWP_FRAMECHANGED      = $00020;
  SWP_SHOWWINDOW        = $00040;
  SWP_HIDEWINDOW        = $00080;
  SWP_NOCOPYBITS        = $00100;
  SWP_NOOWNERZORDER     = $00200;
  SWP_NOREPOSITION      = $00200;
  SWP_NOSENDCHANGING    = $00400;
  SWP_DEFERERASE        = $02000;
  SWP_ASYNCWINDOWPOS    = $04000;
  SWP_STATECHANGED      = $08000; // used by windows but not documented (used even in wine)
  SWP_SourceIsInterface = $10000; // this flag can be combined with the above

{ WMSIZE message constants}
  SIZE_RESTORED   = 0; // the default
  SIZE_MINIMIZED  = 1;
  SIZE_MAXIMIZED  = 2;
  SIZE_MAXSHOW    = 3;
  SIZE_MAXHIDE    = 4;
  SIZE_FULLSCREEN = 16; // non-winapi value to support wsFullScreen state
  Size_SourceIsInterface = 128; // this flag can be combined with the above

  SIZENORMAL = SIZE_RESTORED;
  SIZEICONIC = SIZE_MINIMIZED;
  SIZEFULLSCREEN = SIZE_MAXIMIZED;
  SIZEZOOMSHOW = SIZE_MAXSHOW;
  SIZEZOOMHIDE = SIZE_MAXHIDE;

{ WMMove Message Constants }
  Move_Default = 0;
  Move_SourceIsInterface = 128; // this flag can be combined with the above

//==============================================
// Window Styles
//==============================================
  WS_OVERLAPPED = 0;
  WS_POPUP = DWORD($80000000);
  WS_CHILD = DWORD($40000000);
  WS_MINIMIZE = DWORD($20000000);
  WS_VISIBLE = DWORD($10000000);
  WS_DISABLED = DWORD($8000000);
  WS_CLIPSIBLINGS = DWORD($4000000);
  WS_CLIPCHILDREN = DWORD($2000000);
  WS_MAXIMIZE = DWORD($1000000);
  WS_BORDER = DWORD($800000);
  WS_DLGFRAME = DWORD($400000);
  WS_CAPTION = DWORD(WS_BORDER or WS_DLGFRAME);
  WS_VSCROLL = DWORD($200000);
  WS_HSCROLL = DWORD($100000);
  WS_SYSMENU = DWORD($80000);
  WS_THICKFRAME = DWORD($40000);
  WS_GROUP = DWORD($20000);
  WS_TABSTOP = DWORD($10000);

  WS_MINIMIZEBOX = DWORD($20000);
  WS_MAXIMIZEBOX = DWORD($10000);

  WS_TILED = WS_OVERLAPPED;

  WS_ICONIC = WS_MINIMIZE;

  WS_SIZEBOX = WS_THICKFRAME;

  { Common Window Styles }
  WS_OVERLAPPEDWINDOW = (WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU
                         or WS_THICKFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW;
  WS_POPUPWINDOW = (WS_POPUP or WS_BORDER or WS_SYSMENU);
  WS_CHILDWINDOW = (WS_CHILD);

  { Extended Window Styles }
  WS_EX_DLGMODALFRAME = 1;
  WS_EX_NOPARENTNOTIFY = 4;
  WS_EX_TOPMOST = 8;
  WS_EX_ACCEPTFILES = $10;
  WS_EX_TRANSPARENT = $20;
  WS_EX_MDICHILD = $40;
  WS_EX_TOOLWINDOW = $80;
  WS_EX_WINDOWEDGE = $100;
  WS_EX_CLIENTEDGE = $200;
  WS_EX_CONTEXTHELP = $400;

  WS_EX_RIGHT = $1000;
  WS_EX_LEFT = 0;
  WS_EX_RTLREADING = $2000;
  WS_EX_LTRREADING = 0;
  WS_EX_LEFTSCROLLBAR = $4000;
  WS_EX_RIGHTSCROLLBAR = 0;

  WS_EX_CONTROLPARENT = $10000;
  WS_EX_STATICEDGE = $20000;
  WS_EX_APPWINDOW = $40000;
  WS_EX_OVERLAPPEDWINDOW = (WS_EX_WINDOWEDGE or WS_EX_CLIENTEDGE);
  WS_EX_PALETTEWINDOW = (WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST);

  { Button styles }
  BS_PUSHBUTTON    = $00000000;
  BS_DEFPUSHBUTTON = $00000001;
  BS_AUTOCHECKBOX  = $00000003;
  BS_RADIOBUTTON   = $00000004;
  BS_3STATE        = $00000005;
  BS_GROUPBOX      = $00000007;
  BS_OWNERDRAW     = $0000000B;
  BS_RIGHTBUTTON   = $00000020;
  BS_PUSHLIKE      = $00001000;

  { Edit styles }
  ES_LEFT          = $0000;
  ES_CENTER        = $0001;
  ES_RIGHT         = $0002;
  ES_MULTILINE     = $0004;
  ES_AUTOVSCROLL   = $0040;
  ES_AUTOHSCROLL   = $0080;
  ES_NOHIDESEL     = $0100;
  ES_NUMBER        = $2000;
  ES_READONLY      = $0800;
  ES_WANTRETURN    = $1000;

  { Combobox style }
  CBS_SIMPLE            = $0001;
  CBS_DROPDOWN          = $0002;
  CBS_DROPDOWNLIST      = $0003;
  CBS_OWNERDRAWFIXED    = $0010;
  CBS_OWNERDRAWVARIABLE = $0020;
  CBS_AUTOHSCROLL       = $0040;
  CBS_SORT              = $0100;
  CBS_HASSTRINGS        = $0200;

  { Listbox style }
  LBS_NOTIFY            = $0001;
  LBS_SORT              = $0002;
  LBS_NOREDRAW          = $0004;
  LBS_MULTIPLESEL       = $0008;
  LBS_OWNERDRAWFIXED    = $0010;
  LBS_OWNERDRAWVARIABLE = $0020;
  LBS_HASSTRINGS        = $0040;
  LBS_USETABSTOPS       = $0080;
  LBS_NOINTEGRALHEIGHT  = $0100;
  LBS_MULTICOLUMN       = $0200;
  LBS_WANTKEYBOARDINPUT = $0400;
  LBS_EXTENDEDSEL       = $0800;
  LBS_DISABLENOSCROLL   = $1000;
  LBS_NODATA            = $2000;
  LBS_NOSEL             = $4000;
  LBS_STANDARD          = $A00003;

const
//==============================================
// SetWindowPos Flags
//==============================================
  HWND_TOP = 0;
  HWND_BOTTOM = 1;
  HWND_TOPMOST = HWND(-1);
  HWND_NOTOPMOST = HWND(-2);

//==============================================
// ShowWindow() Commands
//==============================================
  SW_HIDE           = 0;
  SW_SHOWNORMAL     = 1;
  SW_NORMAL         = 1;
  SW_SHOWMINIMIZED  = 2;
  SW_SHOWMAXIMIZED  = 3;
  SW_MAXIMIZE       = 3;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOW           = 5;
  SW_MINIMIZE       = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA         = 8;
  SW_RESTORE        = 9;
  SW_SHOWDEFAULT    = 10;
  SW_MAX            = 10;
  SW_SHOWFULLSCREEN = 11; // LCL Addition

//==============================================
// Redrawindow() flags
//==============================================
  RDW_INVALIDATE      = $0001;
  RDW_INTERNALPAINT   = $0002;
  RDW_ERASE           = $0004;
  RDW_VALIDATE        = $0008;
  RDW_NOINTERNALPAINT = $0010;
  RDW_NOERASE         = $0020;
  RDW_NOCHILDREN      = $0040;
  RDW_ALLCHILDREN     = $0080;
  RDW_UPDATENOW       = $0100;
  RDW_ERASENOW        = $0200;
  RDW_FRAME           = $0400;
  RDW_NOFRAME         = $0800;


const
  { DIB color table identifiers }

  DIB_RGB_COLORS = 0;     { color table in RGBs  }
  DIB_PAL_COLORS = 1;     { color table in palette indices  }

const
  { Gradient Fill Modes }
  GRADIENT_FILL_RECT_H = 0;
  GRADIENT_FILL_RECT_V = 1;
  GRADIENT_FILL_TRIANGLE = 2;


type
  PNMHdr = ^TNMHdr;
  tagNMHDR = record
    hwndFrom: HWND;
    idFrom: PtrUInt;
    code: Integer;
  end;
  TNMHdr = tagNMHDR;
  NMHDR = tagNMHDR;

  PScreenInfo = ^TScreenInfo;
  TScreenInfo = record
    PixelsPerInchX: Integer;
    PixelsPerInchY: Integer;
    ColorDepth: Integer;
    Initialized: boolean;
  end;

{ monitor support }
const
  MONITOR_UNIMPL = HMONITOR(-1);

  MONITOR_DEFAULTTONULL    = $00000000;
  MONITOR_DEFAULTTOPRIMARY = $00000001;
  MONITOR_DEFAULTTONEAREST = $00000002;

  MONITORINFOF_PRIMARY = $00000001;
  CCHDEVICENAME = 32;

type
  tagMonitorInfo = record
    cbSize: DWord;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWord;
  end;
  PMonitorInfo = ^TMonitorInfo;
  TMonitorInfo = tagMonitorInfo;

  tagMonitorInfoEx = record
    cbSize: DWord;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWord;
    szDevice: array[0..CCHDEVICENAME - 1] of Char;
  end;
  PMonitorInfoEx = ^TMonitorInfoEx;
  TMonitorInfoEx = tagMonitorInfoEx;

  tagMonitorInfoExW = record
    cbSize: DWord;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: DWord;
    szDevice: array[0..CCHDEVICENAME - 1] of WideChar;
  end;
  PMonitorInfoExW = ^TMonitorInfoExW;
  TMonitorInfoExW = tagMonitorInfoExW;

{painting stuff}

  PDrawItemStruct = ^TDrawItemStruct;
  tagDrawItemStruct = record
    ctlType: UINT;
    ctlID : UINT;
    itemID : UINT;
    itemAction : UINT;
    itemState : UINT;
    hwndItem: HWND;
    _hDC: HDC;
    rcItem: TRect;
    itemData : ULONG_PTR;
  end;
  TDrawItemStruct = tagDrawItemStruct;
  DrawItemStruct = tagDrawItemStruct;

  // ToDo: move this to StdCtrls
  TOwnerDrawStateType = (
    odSelected, odGrayed, odDisabled, odChecked,
    odFocused, odDefault, odHotLight, odInactive, odNoAccel,
    odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit,
    odPainted  // item already painted
    );
  TOwnerDrawState = set of TOwnerDrawStateType;

  PDrawListItemStruct = ^TDrawListItemStruct;
  TDrawListItemStruct = record
    ItemID: UINT;
    Area: TRect;
    DC: HDC;
    ItemState: TOwnerDrawState;
  end;

  PMeasureItemStruct = ^TMeasureItemStruct;
  TMeasureItemStruct = record
    CtlType : UINT;
    CtlID : UINT;
    itemID : UINT;
    itemWidth : UINT;
    itemHeight : UINT;
    itemData : ULONG_PTR;
  end;

  PPaintStruct = ^TPaintStruct;
  tagPAINTSTRUCT = record
    hdc: HDC;
    fErase: BOOL;
    rcPaint: TRect;
    fRestore: BOOL;
    fIncUpdate: BOOL;
    rgbReserved: array[0..31] of Byte;
  end;
  TPaintStruct = tagPAINTSTRUCT;

  PAINTSTRUCT = tagPAINTSTRUCT;


  PWindowPos = ^TWIndowPos;
  tagWindowPos = record
    _hwnd : HWND;
    hwndInsertAfter: HWND;
    x : Integer;
    y : Integer;
    cx : Integer;
    cy : Integer;
    flags: UINT;
    end;
  TWindowPos = tagWindowPos;
  WindowPos = tagWindowPos;

  tagScrollInfo = record
    cbSize : UINT;
    fMask : UINT;
    nMin: Integer;
    nMax: Integer;
    nPage: UInt;
    nPos: Integer;
    nTrackPos: Integer;
  end;
  PScrollInfo = ^TScrollInfo;
  TScrollInfo = tagScrollinfo;

{ Palette Structures }
  PPaletteEntry = ^tagPaletteEntry;
  tagPALETTEENTRY = record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;
  TPaletteEntry = tagPALETTEENTRY;
  PALETTEENTRY = tagPALETTEENTRY;

  PLogPalette = ^tagLogPalette;
  tagLOGPALETTE = record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[0..0] of tagPaletteEntry;
  end;
  LOGPALETTE = tagLOGPALETTE;
  TLOGPALETTE = tagLOGPALETTE;

{ GradientFill Structures }
  PTriVertex = ^tagTriVertex;
  tagTRIVERTEX = record
    x: Longint;
    y: Longint;
    Red: Word;
    Green: Word;
    Blue: Word;
    Alpha: Word;
  end;
  TRIVERTEX = tagTRIVERTEX;
  TTriVertex = TRIVERTEX;

  PGradientTriangle = ^tagGradientTriangle;
  tagGRADIENTTRIANGLE = record
    Vertex1: Cardinal;
    Vertex2: Cardinal;
    Vertex3: Cardinal;
  end;
  GRADIENTTRIANGLE = tagGRADIENTTRIANGLE;
  TGradientTriangle = GRADIENTTRIANGLE;

  PGradientRect = ^tagGradientRect;
  tagGRADIENTRECT = record
    UpperLeft: Cardinal;
    LowerRight: Cardinal;
  end;
  GRADIENTRECT = tagGRADIENTRECT;
  TGradientRect = GRADIENTRECT;

{ ********************************** }
{        B I T M A P    S T U F F    }

  { TBitmap is an encapsulation of a matrix of pixels. }
  PBitmap = ^TagBitmap;
  tagBITMAP = record
    bmType: Longint;
    bmWidth: Longint;
    bmHeight: Longint;
    bmWidthBytes: Longint;
    bmPlanes: Word;
    bmBitsPixel: Word;
    bmBits: Pointer;
  end;
  BITMAP = tagBITMAP;


  PBitmapCoreHeader = ^TBitmapCoreHeader;
  tagBITMAPCOREHEADER = packed record // use packed, this is the .bmp file format
    bcSize: DWORD;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;
  TBitmapCoreHeader = tagBITMAPCOREHEADER;
  BITMAPCOREHEADER = tagBITMAPCOREHEADER;


  PBitmapInfoHeader = ^TBitmapInfoHeader;
  tagBITMAPINFOHEADER = packed record // use packed, this is the .bmp file format
    biSize : DWORD;
    biWidth : Longint;
    biHeight : Longint;
    biPlanes : WORD;
    biBitCount : WORD;
    biCompression : DWORD;
    biSizeImage : DWORD;
    biXPelsPerMeter : Longint;
    biYPelsPerMeter : Longint;
    biClrUsed : DWORD;
    biClrImportant : DWORD;
  end;
  TBitmapInfoHeader = tagBITMAPINFOHEADER;
  BITMAPINFOHEADER = tagBITMAPINFOHEADER;


  PRGBTriple = ^TRGBTriple;
  tagRGBTRIPLE = record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  TRGBTriple = tagRGBTRIPLE;
  RGBTRIPLE = tagRGBTRIPLE;

  PRGBQUAD = ^TRGBQUAD;
  tagRGBQUAD = record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
          rgbReserved : BYTE;
       end;
  TRGBQuad = tagRGBQUAD;
  RGBQUAD = tagRGBQUAD;
  
  TRGBAQuad = record
    Blue:  Byte;
    Green: Byte;
    Red:   Byte;
    Alpha: Byte;
  end;
  PRGBAQuad = ^TRGBAQuad;


  PBitmapInfo = ^TBitmapInfo;
  tagBITMAPINFO = record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..0] of TRGBQuad;
  end;
  TBitmapInfo = tagBITMAPINFO;
  BITMAPINFO = tagBITMAPINFO;

  PBitmapCoreInfo = ^TBitmapCoreInfo;
  tagBITMAPCOREINFO = record
    bmciHeader: TBitmapCoreHeader;
    bmciColors: array[0..0] of TRGBTriple;
    Reserved: array[0..0] of Char;
  end;
  TBitmapCoreInfo = tagBITMAPCOREINFO;
  BITMAPCOREINFO = tagBITMAPCOREINFO;


  PBitmapFileHeader = ^TBitmapFileHeader;
  tagBITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;
  TBitmapFileHeader = tagBITMAPFILEHEADER;
  BITMAPFILEHEADER = tagBITMAPFILEHEADER;


  PDIBSection = ^TDIBSection;
  tagDIBSECTION = record
    dsBm: TagBitmap;
    dsBmih: tagBITMAPINFOHEADER;
    dsBitfields: array[0..2] of DWORD;
    dshSection: THandle;
    dsOffset: DWORD;
  end;
  TDIBSection = tagDIBSECTION;
  DIBSECTION = tagDIBSECTION;

  tagHELPINFO = record
    cbSize: DWORD;
    iContextType: Integer;
    iCtrlId: Integer;
    hItemHandle: THandle;
    dwContextId: DWORD;
    MousePos: TPOINT;
  end;
  THelpInfo = tagHelpInfo;
  PHelpInfo = ^THelpInfo;

const
  RASTER_FONTTYPE = 1;
  DEVICE_FONTTYPE = 2;
  TRUETYPE_FONTTYPE = 4;

  GCP_DBCS = 1;

// context type
  HELPINFO_WINDOW   = $0001;
  HELPINFO_MENUITEM = $0002;

// DoOnHelp commands
  HELP_CONTEXT = 1;
  HELP_COMMAND = 258;

//==============================================
// Background Modes
//==============================================
  TRANSPARENT = 1;
  OPAQUE = 2;
  BKMODE_LAST = 2;

//==============================================
// Font constants
//==============================================

//------------
// Font Styles
//------------
  LF_FULLFACESIZE = 64;
  LF_FACESIZE = 32;

  OUT_DEFAULT_PRECIS = 0;
  OUT_STRING_PRECIS = 1;
  OUT_CHARACTER_PRECIS = 2;
  OUT_STROKE_PRECIS = 3;
  OUT_TT_PRECIS = 4;
  OUT_DEVICE_PRECIS = 5;
  OUT_RASTER_PRECIS = 6;
  OUT_TT_ONLY_PRECIS = 7;
  OUT_OUTLINE_PRECIS = 8;
  OUT_SCREEN_OUTLINE_PRECIS = 9;

  CLIP_DEFAULT_PRECIS = 0;
  CLIP_CHARACTER_PRECIS = 1;
  CLIP_STROKE_PRECIS = 2;
  CLIP_MASK = 15;
  CLIP_LH_ANGLES = (1 shl 4);
  CLIP_TT_ALWAYS = (2 shl 4);
  CLIP_EMBEDDED  = (8 shl 4);

  DEFAULT_QUALITY           = 0;
  DRAFT_QUALITY             = 1;
  PROOF_QUALITY             = 2;
  NONANTIALIASED_QUALITY    = 3;
  ANTIALIASED_QUALITY       = 4;
  CLEARTYPE_QUALITY         = 5;
  CLEARTYPE_NATURAL_QUALITY = 6;

  DEFAULT_PITCH  = 0;
  FIXED_PITCH    = 1;
  VARIABLE_PITCH = 2;
  MONO_FONT      = 8;

  // font character sets
  ANSI_CHARSET        = 0;
  DEFAULT_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  // added for ISO_8859_2 under gtk
  FCS_ISO_10646_1     = 4;  // Unicode;
  FCS_ISO_8859_1      = 5;  //  ISO Latin-1 (Western Europe);
  FCS_ISO_8859_2      = 6;  //  ISO Latin-2 (Eastern Europe);
  FCS_ISO_8859_3      = 7;  //  ISO Latin-3 (Southern Europe);
  FCS_ISO_8859_4      = 8;  //  ISO Latin-4 (Northern Europe);
  FCS_ISO_8859_5      = 9;  //  ISO Cyrillic;
  FCS_ISO_8859_6      = 10; //  ISO Arabic;
  FCS_ISO_8859_7      = 11; //  ISO Greek;
  FCS_ISO_8859_8      = 12; //  ISO Hebrew;
  FCS_ISO_8859_9      = 13; //  ISO Latin-5 (Turkish);
  FCS_ISO_8859_10     = 14; //  ISO Latin-6 (Nordic);
  FCS_ISO_8859_15     = 15; //  ISO Latin-9, or Latin-0 (Revised Western-European);
  //FCS_koi8_r          = 16; //  KOI8 Russian;
  //FCS_koi8_u          = 17; //  KOI8 Ukrainian (see RFC 2319);
  //FCS_koi8_ru         = 18; //  KOI8 Russian/Ukrainian
  //FCS_koi8_uni        = 19; //  KOI8 ``Unified'' (Russian, Ukrainian, and Byelorussian);
  //FCS_koi8_e          = 20; //  KOI8 ``European,'' ISO-IR-111, or ECMA-Cyrillic;
  // end of our own additions
  MAC_CHARSET         = 77;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  JOHAB_CHARSET       = 130;
  GB2312_CHARSET      = 134;
  CHINESEBIG5_CHARSET = 136;
  GREEK_CHARSET       = 161;
  TURKISH_CHARSET     = 162;
  VIETNAMESE_CHARSET  = 163;
  HEBREW_CHARSET      = 177;
  ARABIC_CHARSET      = 178;
  BALTIC_CHARSET      = 186;
  RUSSIAN_CHARSET     = 204;
  THAI_CHARSET        = 222;
  EASTEUROPE_CHARSET  = 238;
  OEM_CHARSET         = 255;
  // additional charsets

//-----------
// Font Sets
//-----------
  FS_LATIN1 = 1;
  FS_LATIN2 = 2;
  FS_CYRILLIC = 4;
  FS_GREEK = 8;
  FS_TURKISH = $10;
  FS_HEBREW = $20;
  FS_ARABIC = $40;
  FS_BALTIC = $80;
  FS_VIETNAMESE = $00000100;
  FS_THAI = $10000;
  FS_JISJAPAN = $20000;
  FS_CHINESESIMP = $40000;
  FS_WANSUNG = $80000;
  FS_CHINESETRAD = $100000;
  FS_JOHAB = $200000;
  FS_SYMBOL = DWORD($80000000);

//---------------
// Font Families
//---------------
  FF_DONTCARE   = (0 shl 4);
  FF_ROMAN      = (1 shl 4);
  FF_SWISS      = (2 shl 4);
  FF_MODERN     = (3 shl 4);
  FF_SCRIPT     = (4 shl 4);
  FF_DECORATIVE = (5 shl 4);

//--------------
// Font Weights
//--------------
  FW_DONTCARE   = 0;
  FW_THIN       = 100;
  FW_EXTRALIGHT = 200;
  FW_LIGHT      = 300;
  FW_NORMAL     = 400;
  FW_MEDIUM     = 500;
  FW_SEMIBOLD   = 600;
  FW_BOLD       = 700;
  FW_EXTRABOLD  = 800;
  FW_HEAVY      = 900;
  FW_ULTRALIGHT = FW_EXTRALIGHT;
  FW_REGULAR    = FW_NORMAL;
  FW_DEMIBOLD   = FW_SEMIBOLD;
  FW_ULTRABOLD  = FW_EXTRABOLD;
  FW_BLACK      = FW_HEAVY;

  FOUNDRYCHAR_OPEN  = '[';  // added for support foundry encoded in family name
  FOUNDRYCHAR_CLOSE = ']';  // also needed to drop foundry when creating font in windows

//--------------
// XFLD constans
//--------------
  XLFD_FONTNAME_REG = 0;
  XLFD_FOUNDRY      = 1;
  XLFD_FAMILY       = 2;
  XLFD_WEIGHTNAME   = 3;
  XLFD_SLANT        = 4;
  XLFD_WIDTHNAME    = 5;
  XLFD_STYLENAME    = 6;
  XLFD_PIXELSIZE    = 7;
  XLFD_POINTSIZE    = 8;
  XLFD_RESX         = 9;
  XLFD_RESY         = 10;
  XLFD_SPACING      = 11;
  XLFD_AVG_WIDTH    = 12;
  XLFD_CHARSET_REG  = 13;
  XLFD_CHARSET_COD  = 14;



//==============================================
// Brush constants
//==============================================

//--------------
// Brush Styles
//--------------

  BS_SOLID                = 0;
  BS_NULL                 = 1;
  BS_HOLLOW               = BS_NULL;
  BS_HATCHED              = 2;
  BS_PATTERN              = 3;
  BS_INDEXED              = 4;
  BS_DIBPATTERN           = 5;
  BS_DIBPATTERNPT         = 6;
  BS_PATTERN8X8           = 7;
  BS_DIBPATTERN8X8        = 8;
  BS_MONOPATTERN          = 9;

//--------------
// Hatch Styles
//--------------

  HS_HORIZONTAL = 0;       { ----- }
  HS_VERTICAL   = 1;       { ||||| }
  HS_FDIAGONAL  = 2;       { ///// }
  HS_BDIAGONAL  = 3;       { \\\\\ }
  HS_CROSS      = 4;       { +++++ }
  HS_DIAGCROSS  = 5;       { xxxxx }

//==============================================
// Pen constants
//==============================================

//------------
// Pen Styles
//------------

  PS_SOLID       = 0;
  PS_DASH        = 1;      { ------- }
  PS_DOT         = 2;      { ....... }
  PS_DASHDOT     = 3;      { _._._._ }
  PS_DASHDOTDOT  = 4;      { _.._.._ }
  PS_NULL = 5;
  PS_INSIDEFRAME = 6;
  PS_USERSTYLE = 7;
  PS_ALTERNATE = 8;
  PS_STYLE_MASK = 15;

  PS_ENDCAP_ROUND = 0;
  PS_ENDCAP_SQUARE = $100;
  PS_ENDCAP_FLAT = $200;
  PS_ENDCAP_MASK = 3840;

  PS_JOIN_ROUND = 0;
  PS_JOIN_BEVEL = $1000;
  PS_JOIN_MITER = $2000;
  PS_JOIN_MASK = 61440;

  PS_COSMETIC = 0;
  PS_GEOMETRIC = $10000;
  PS_TYPE_MASK = $F0000;


//==============================================
// Mapping modes for SetMapMode/GetMapMode
//==============================================

  MM_TEXT        = 1;
  MM_LOMETRIC    = 2;
  MM_HIMETRIC    = 3;
  MM_LOENGLISH   = 4;
  MM_HIENGLISH   = 5;
  MM_TWIPS       = 6;
  MM_ISOTROPIC   = 7;
  MM_ANISOTROPIC = 8;


//==============================================
// API system Color constants  pbd
// note these are usually shown ORed with
// $80000000 as these would have interfered with
// other MS color enumerations
// GetSysColor and SetSysColor expects the values
// below
//==============================================

type
  COLORREF = Cardinal;
  TColorRef = COLORREF;

const
  CLR_NONE = TColorRef($FFFFFFFF);
  CLR_DEFAULT = TColorRef($FF000000);
  CLR_INVALID = TColorRef($FFFFFFFF);

  COLOR_SCROLLBAR = 0;
  COLOR_BACKGROUND = 1;
  COLOR_ACTIVECAPTION = 2;
  COLOR_INACTIVECAPTION = 3;
  COLOR_MENU = 4;
  COLOR_WINDOW = 5;
  COLOR_WINDOWFRAME = 6;
  COLOR_MENUTEXT = 7;
  COLOR_WINDOWTEXT = 8;
  COLOR_CAPTIONTEXT = 9;
  COLOR_ACTIVEBORDER = 10;
  COLOR_INACTIVEBORDER = 11;
  COLOR_APPWORKSPACE = 12;
  COLOR_HIGHLIGHT = 13;
  COLOR_HIGHLIGHTTEXT = 14;
  COLOR_BTNFACE = 15;
  COLOR_BTNSHADOW = 16;
  COLOR_GRAYTEXT = 17;
  COLOR_BTNTEXT = 18;
  COLOR_INACTIVECAPTIONTEXT = 19;
  COLOR_BTNHIGHLIGHT = 20;
  COLOR_3DDKSHADOW = 21;
  COLOR_3DLIGHT = 22;
  COLOR_INFOTEXT = 23;
  COLOR_INFOBK = 24;
  // PBD: 25 is unassigned in all the docs I can find
  //      if someone finds what this is supposed to be then fill it in
  //      note defaults below, and cl[ColorConst] in graphics
  COLOR_HOTLIGHT = 26;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
  COLOR_MENUHILIGHT = 29;
  COLOR_MENUBAR = 30;

  COLOR_FORM = 31;

  COLOR_ENDCOLORS = COLOR_FORM;

  COLOR_DESKTOP = COLOR_BACKGROUND;
  COLOR_3DFACE = COLOR_BTNFACE;
  COLOR_3DSHADOW = COLOR_BTNSHADOW;
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_3DHILIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_BTNHILIGHT = COLOR_BTNHIGHLIGHT;

  MAX_SYS_COLORS = COLOR_ENDCOLORS;
  SYS_COLOR_BASE = TColorRef($80000000);

  // !! deprecated colors !!

  // CLX base, mapped, pseudo, rgb values
  COLOR_clForeground =  32;
  COLOR_clButton =  COLOR_clForeground+1;
  COLOR_clLight =  COLOR_clForeground+2;
  COLOR_clMidlight =  COLOR_clForeground+3;
  COLOR_clDark =  COLOR_clForeground+4;
  COLOR_clMid =  COLOR_clForeground+5;
  COLOR_clText =  COLOR_clForeground+6;
  COLOR_clBrightText =  COLOR_clForeground+7;
  COLOR_clButtonText =  COLOR_clForeground+8;
  COLOR_clBase =  COLOR_clForeground+9;
  //clBackground
  COLOR_clShadow =  COLOR_clForeground+10;
  //clHighlight
  COLOR_clHighlightedText =  COLOR_clForeground+11;

  // CLX normal, mapped, pseudo, rgb values
  COLOR_clNormalForeground =  44;
  COLOR_clNormalButton =  COLOR_clNormalForeground+1;
  COLOR_clNormalLight =  COLOR_clNormalForeground+2;
  COLOR_clNormalMidlight =  COLOR_clNormalForeground+3;
  COLOR_clNormalDark =  COLOR_clNormalForeground+4;
  COLOR_clNormalMid =  COLOR_clNormalForeground+5;
  COLOR_clNormalText =  COLOR_clNormalForeground+6;
  COLOR_clNormalBrightText =  COLOR_clNormalForeground+7;
  COLOR_clNormalButtonText =  COLOR_clNormalForeground+8;
  COLOR_clNormalBase =  COLOR_clNormalForeground+9;
  COLOR_clNormalBackground =  COLOR_clNormalForeground+10;
  COLOR_clNormalShadow =  COLOR_clNormalForeground+11;
  COLOR_clNormalHighlight =  COLOR_clNormalForeground+12;
  COLOR_clNormalHighlightedText =  COLOR_clNormalForeground+13;

  // CLX disabled, mapped, pseudo, rgb values
  COLOR_clDisabledForeground =  58;
  COLOR_clDisabledButton =  COLOR_clDisabledForeground+1;
  COLOR_clDisabledLight =  COLOR_clDisabledForeground+2;
  COLOR_clDisabledMidlight =  COLOR_clDisabledForeground+3;
  COLOR_clDisabledDark =  COLOR_clDisabledForeground+4;
  COLOR_clDisabledMid =  COLOR_clDisabledForeground+5;
  COLOR_clDisabledText =  COLOR_clDisabledForeground+6;
  COLOR_clDisabledBrightText =  COLOR_clDisabledForeground+7;
  COLOR_clDisabledButtonText =  COLOR_clDisabledForeground+8;
  COLOR_clDisabledBase =  COLOR_clDisabledForeground+9;
  COLOR_clDisabledBackground =  COLOR_clDisabledForeground+10;
  COLOR_clDisabledShadow =  COLOR_clDisabledForeground+11;
  COLOR_clDisabledHighlight =  COLOR_clDisabledForeground+12;
  COLOR_clDisabledHighlightedText =  COLOR_clDisabledForeground+13;

  // CLX active, mapped, pseudo, rgb values
  COLOR_clActiveForeground =  72;
  COLOR_clActiveButton =  COLOR_clActiveForeground+1;
  COLOR_clActiveLight =  COLOR_clActiveForeground+2;
  COLOR_clActiveMidlight =  COLOR_clActiveForeground+3;
  COLOR_clActiveDark =  COLOR_clActiveForeground+4;
  COLOR_clActiveMid =  COLOR_clActiveForeground+5;
  COLOR_clActiveText =  COLOR_clActiveForeground+6;
  COLOR_clActiveBrightText =  COLOR_clActiveForeground+7;
  COLOR_clActiveButtonText =  COLOR_clActiveForeground+8;
  COLOR_clActiveBase =  COLOR_clActiveForeground+9;
  COLOR_clActiveBackground =  COLOR_clActiveForeground+10;
  COLOR_clActiveShadow =  COLOR_clActiveForeground+11;
  COLOR_clActiveHighlight =  COLOR_clActiveForeground+12;
  COLOR_clActiveHighlightedText =  COLOR_clActiveForeground+13;

//==============================================
// Stock Objects
//==============================================

  WHITE_BRUSH = 0;
  LTGRAY_BRUSH = 1;
  GRAY_BRUSH = 2;
  DKGRAY_BRUSH = 3;
  BLACK_BRUSH = 4;
  NULL_BRUSH = 5;
  HOLLOW_BRUSH = NULL_BRUSH;
  WHITE_PEN = 6;
  BLACK_PEN = 7;
  NULL_PEN = 8;
  OEM_FIXED_FONT = 10;
  ANSI_FIXED_FONT = 11;
  ANSI_VAR_FONT = 12;
  SYSTEM_FONT = 13;
  DEVICE_DEFAULT_FONT = 14;
  DEFAULT_PALETTE = 15;
  SYSTEM_FIXED_FONT = 16;
  DEFAULT_GUI_FONT = 17;
  DC_BRUSH = 18;
  DC_PEN = 19;
  STOCK_LAST = 19;

//==============================================
// Stock Pixmap Types
//==============================================
  idButtonBase = 0;
  idButtonOk = idButtonBase + 1;
  idButtonCancel = idButtonBase + 2;
  idButtonHelp = idButtonBase + 3;
  idButtonYes = idButtonBase + 4;
  idButtonNo = idButtonBase + 5;
  idButtonClose = idButtonBase + 6;
  idButtonAbort = idButtonBase + 7;
  idButtonRetry = idButtonBase + 8;
  idButtonIgnore = idButtonBase + 9;
  idButtonAll = idButtonBase + 10;
  idButtonYesToAll = idButtonBase + 11;
  idButtonNoToAll = idButtonBase + 12;
  idButtonOpen = idButtonBase + 13;
  idButtonSave = idButtonBase + 14;
  idButtonShield = idButtonBase + 15;

  idDialogBase = $FF;
  idDialogWarning = idDialogBase + 1;
  idDialogError = idDialogBase + 2;
  idDialogInfo = idDialogBase + 3;
  idDialogConfirm = idDialogBase + 4;
  idDialogShield = idDialogBase + 5;


//==============================================
// Devicecontext object types
//==============================================
  OBJ_PEN         = 1;
  OBJ_BRUSH       = 2;
  OBJ_DC          = 3;
  OBJ_METADC      = 4;
  OBJ_PAL         = 5;
  OBJ_FONT        = 6;
  OBJ_BITMAP      = 7;
  OBJ_REGION      = 8;
  OBJ_METAFILE    = 9;
  OBJ_MEMDC       = 10;
  OBJ_EXTPEN      = 11;
  OBJ_ENHMETADC   = 12;
  OBJ_ENHMETAFILE = 13;
  OBJ_COLORSPACE  = 14;


//==============================================
// SystemMetrics constants
//==============================================

  SM_CXSCREEN = 0;
  SM_CYSCREEN = 1;
  SM_CXVSCROLL = 2;
  SM_CYHSCROLL = 3;
  SM_CYCAPTION = 4;
  SM_CXBORDER = 5;
  SM_CYBORDER = 6;
  SM_CXDLGFRAME = 7;
  SM_CYDLGFRAME = 8;
  SM_CYVTHUMB = 9;
  SM_CXHTHUMB = 10;
  SM_CXICON = 11;
  SM_CYICON = 12;
  SM_CXCURSOR = 13;
  SM_CYCURSOR = 14;
  SM_CYMENU = 15;
  SM_CXFULLSCREEN = 16;
  SM_CYFULLSCREEN = 17;
  SM_CYKANJIWINDOW = 18;
  SM_MOUSEPRESENT = 19;
  SM_CYVSCROLL = 20;
  SM_CXHSCROLL = 21;
  SM_DEBUG = 22;
  SM_SWAPBUTTON = 23;
  SM_RESERVED1 = 24;
  SM_RESERVED2 = 25;
  SM_RESERVED3 = 26;
  SM_RESERVED4 = 27;
  SM_CXMIN = 28;
  SM_CYMIN = 29;
  SM_CXSIZE = 30;
  SM_CYSIZE = 31;
  SM_CXFRAME = 32;
  SM_CYFRAME = 33;
  SM_CXMINTRACK = 34;
  SM_CYMINTRACK = 35;
  SM_CXDOUBLECLK = 36;
  SM_CYDOUBLECLK = 37;
  SM_CXICONSPACING = 38;
  SM_CYICONSPACING = 39;
  SM_MENUDROPALIGNMENT = 40;
  SM_PENWINDOWS = 41;
  SM_DBCSENABLED = 42;
  SM_CMOUSEBUTTONS = 43;

  SM_CXFIXEDFRAME = SM_CXDLGFRAME;
  SM_CYFIXEDFRAME = SM_CYDLGFRAME;
  SM_CXSIZEFRAME = SM_CXFRAME;
  SM_CYSIZEFRAME = SM_CYFRAME;

  SM_SECURE = 44;
  SM_CXEDGE = 45;
  SM_CYEDGE = 46;
  SM_CXMINSPACING = 47;
  SM_CYMINSPACING = 48;
  SM_CXSMICON = 49;
  SM_CYSMICON = 50;
  SM_CYSMCAPTION = 51;
  SM_CXSMSIZE = 52;
  SM_CYSMSIZE = 53;
  SM_CXMENUSIZE = 54;
  SM_CYMENUSIZE = 55;
  SM_ARRANGE = 56;
  SM_CXMINIMIZED = 57;
  SM_CYMINIMIZED = 58;
  SM_CXMAXTRACK = 59;
  SM_CYMAXTRACK = 60;
  SM_CXMAXIMIZED = 61;
  SM_CYMAXIMIZED = 62;
  SM_NETWORK = 63;
  SM_CLEANBOOT = 67;
  SM_CXDRAG = 68;
  SM_CYDRAG = 69;
  SM_SHOWSOUNDS = 70;
  SM_CXMENUCHECK = 71;
  SM_CYMENUCHECK = 72;
  SM_SLOWMACHINE = 73;
  SM_MIDEASTENABLED = 74;
  SM_MOUSEWHEELPRESENT = 75;
  SM_CMETRICS = 76;
  
  SM_XVIRTUALSCREEN     = 76;
  SM_YVIRTUALSCREEN     = 77;
  SM_CXVIRTUALSCREEN    = 78;
  SM_CYVIRTUALSCREEN    = 79;
  SM_CMONITORS          = 80;
  SM_SAMEDISPLAYFORMAT  = 81;

  SM_SWSCROLLBARSPACING = 120;

  {needed for accurate maximized window size, since under X11 we cannot get it until
   window is decorated by wm. see issue #21119.}
  SM_LCLMAXIMIZEDWIDTH = 121;
  SM_LCLMAXIMIZEDHEIGHT = 122;

  SM_LCLHasFormAlphaBlend = 123;

//==============================================
// SystemParametersInfo constants
//==============================================
  SPI_GETBEEP = 1;
  SPI_SETBEEP = 2;
  SPI_GETMOUSE = 3;
  SPI_SETMOUSE = 4;
  SPI_GETBORDER = 5;
  SPI_SETBORDER = 6;
  SPI_GETKEYBOARDSPEED = 10;
  SPI_SETKEYBOARDSPEED = 11;
  SPI_LANGDRIVER = 12;
  SPI_ICONHORIZONTALSPACING = 13;
  SPI_GETSCREENSAVETIMEOUT = 14;
  SPI_SETSCREENSAVETIMEOUT = 15;
  SPI_GETSCREENSAVEACTIVE = 16;
  SPI_SETSCREENSAVEACTIVE = 17;
  SPI_GETGRIDGRANULARITY = 18;
  SPI_SETGRIDGRANULARITY = 19;
  SPI_SETDESKWALLPAPER = 20;
  SPI_SETDESKPATTERN = 21;
  SPI_GETKEYBOARDDELAY = 22;
  SPI_SETKEYBOARDDELAY = 23;
  SPI_ICONVERTICALSPACING = 24;
  SPI_GETICONTITLEWRAP = 25;
  SPI_SETICONTITLEWRAP = 26;
  SPI_GETMENUDROPALIGNMENT = 27;
  SPI_SETMENUDROPALIGNMENT = 28;
  SPI_SETDOUBLECLKWIDTH = 29;
  SPI_SETDOUBLECLKHEIGHT = 30;
  SPI_GETICONTITLELOGFONT = 31;
  SPI_SETDOUBLECLICKTIME = 32;
  SPI_SETMOUSEBUTTONSWAP = 33;
  SPI_SETICONTITLELOGFONT = 34;
  SPI_GETFASTTASKSWITCH = 35;
  SPI_SETFASTTASKSWITCH = 36;
  SPI_SETDRAGFULLWINDOWS = 37;
  SPI_GETDRAGFULLWINDOWS = 38;
  SPI_GETNONCLIENTMETRICS = 41;
  SPI_SETNONCLIENTMETRICS = 42;
  SPI_GETMINIMIZEDMETRICS = 43;
  SPI_SETMINIMIZEDMETRICS = 44;
  SPI_GETICONMETRICS = 45;
  SPI_SETICONMETRICS = 46;
  SPI_SETWORKAREA = 47;
  SPI_GETWORKAREA = 48;
  SPI_SETPENWINDOWS = 49;
  SPI_GETFILTERKEYS = 50;
  SPI_SETFILTERKEYS = 51;
  SPI_GETTOGGLEKEYS = 52;
  SPI_SETTOGGLEKEYS = 53;
  SPI_GETMOUSEKEYS = 54;
  SPI_SETMOUSEKEYS = 55;
  SPI_GETSHOWSOUNDS = 56;
  SPI_SETSHOWSOUNDS = 57;
  SPI_GETSTICKYKEYS = 58;
  SPI_SETSTICKYKEYS = 59;
  SPI_GETACCESSTIMEOUT = 60;
  SPI_SETACCESSTIMEOUT = 61;
  SPI_GETSERIALKEYS = 62;
  SPI_SETSERIALKEYS = 63;
  SPI_GETSOUNDSENTRY = 64;
  SPI_SETSOUNDSENTRY = 65;
  SPI_GETHIGHCONTRAST = 66;
  SPI_SETHIGHCONTRAST = 67;
  SPI_GETKEYBOARDPREF = 68;
  SPI_SETKEYBOARDPREF = 69;
  SPI_GETSCREENREADER = 70;
  SPI_SETSCREENREADER = 71;
  SPI_GETANIMATION = 72;
  SPI_SETANIMATION = 73;
  SPI_GETFONTSMOOTHING = 74;
  SPI_SETFONTSMOOTHING = 75;
  SPI_SETDRAGWIDTH = 76;
  SPI_SETDRAGHEIGHT = 77;
  SPI_SETHANDHELD = 78;
  SPI_GETLOWPOWERTIMEOUT = 79;
  SPI_GETPOWEROFFTIMEOUT = 80;
  SPI_SETLOWPOWERTIMEOUT = 81;
  SPI_SETPOWEROFFTIMEOUT = 82;
  SPI_GETLOWPOWERACTIVE = 83;
  SPI_GETPOWEROFFACTIVE = 84;
  SPI_SETLOWPOWERACTIVE = 85;
  SPI_SETPOWEROFFACTIVE = 86;
  SPI_SETCURSORS = 87;
  SPI_SETICONS = 88;
  SPI_GETDEFAULTINPUTLANG = 89;
  SPI_SETDEFAULTINPUTLANG = 90;
  SPI_SETLANGTOGGLE = 91;
  SPI_GETWINDOWSEXTENSION = 92;
  SPI_SETMOUSETRAILS = 93;
  SPI_GETMOUSETRAILS = 94;
  SPI_GETSNAPTODEFBUTTON = 95;
  SPI_SETSNAPTODEFBUTTON = 96;
  SPI_SCREENSAVERRUNNING = 97;
  SPI_SETSCREENSAVERRUNNING = 97;
  SPI_GETMOUSEHOVERWIDTH = 98;
  SPI_SETMOUSEHOVERWIDTH = 99;
  SPI_GETMOUSEHOVERHEIGHT = 100;
  SPI_SETMOUSEHOVERHEIGHT = 101;
  SPI_GETMOUSEHOVERTIME = 102;
  SPI_SETMOUSEHOVERTIME = 103;
  SPI_GETWHEELSCROLLLINES = 104;
  SPI_SETWHEELSCROLLLINES = 105;
  SPI_GETMENUSHOWDELAY = 106;
  SPI_SETMENUSHOWDELAY = 107;
  SPI_GETSHOWIMEUI = 110;
  SPI_SETSHOWIMEUI = 111;
  SPI_GETMOUSESPEED = 112;
  SPI_SETMOUSESPEED = 113;
  SPI_GETSCREENSAVERRUNNING = 114;
  SPI_GETDESKWALLPAPER = 115;
  SPI_GETACTIVEWINDOWTRACKING = 4096;
  SPI_SETACTIVEWINDOWTRACKING = 4097;
  SPI_GETMENUANIMATION = 4098;
  SPI_SETMENUANIMATION = 4099;
  SPI_GETCOMBOBOXANIMATION = 4100;
  SPI_SETCOMBOBOXANIMATION = 4101;
  SPI_GETLISTBOXSMOOTHSCROLLING = 4102;
  SPI_SETLISTBOXSMOOTHSCROLLING = 4103;
  SPI_GETGRADIENTCAPTIONS = 4104;
  SPI_SETGRADIENTCAPTIONS = 4105;
  SPI_GETKEYBOARDCUES = 4106;
  SPI_SETKEYBOARDCUES = 4107;
  SPI_GETMENUUNDERLINES = 4106;
  SPI_SETMENUUNDERLINES = 4107;
  SPI_GETACTIVEWNDTRKZORDER = 4108;
  SPI_SETACTIVEWNDTRKZORDER = 4109;
  SPI_GETHOTTRACKING = 4110;
  SPI_SETHOTTRACKING = 4111;
  SPI_GETMENUFADE = 4114;
  SPI_SETMENUFADE = 4115;
  SPI_GETSELECTIONFADE = 4116;
  SPI_SETSELECTIONFADE = 4117;
  SPI_GETTOOLTIPANIMATION = 4118;
  SPI_SETTOOLTIPANIMATION = 4119;
  SPI_GETTOOLTIPFADE = 4120;
  SPI_SETTOOLTIPFADE = 4121;
  SPI_GETCURSORSHADOW = 4122;
  SPI_SETCURSORSHADOW = 4123;
  SPI_GETUIEFFECTS = 4158;
  SPI_SETUIEFFECTS = 4159;
  SPI_GETFOREGROUNDLOCKTIMEOUT = 8192;
  SPI_SETFOREGROUNDLOCKTIMEOUT = 8193;
  SPI_GETACTIVEWNDTRKTIMEOUT = 8194;
  SPI_SETACTIVEWNDTRKTIMEOUT = 8195;
  SPI_GETFOREGROUNDFLASHCOUNT = 8196;
  SPI_SETFOREGROUNDFLASHCOUNT = 8197;
  SPI_GETCARETWIDTH = 8198;
  SPI_SETCARETWIDTH = 8199;
  SPI_GETMOUSESONAR = 4124;
  SPI_SETMOUSESONAR = 4125;
  SPI_GETMOUSECLICKLOCK = 4126;
  SPI_SETMOUSECLICKLOCK = 4127;
  SPI_GETMOUSEVANISH = 4128;
  SPI_SETMOUSEVANISH = 4129;
  SPI_GETFLATMENU = 4130;
  SPI_SETFLATMENU = 4131;
  SPI_GETDROPSHADOW = 4132;
  SPI_SETDROPSHADOW = 4133;
  SPI_GETBLOCKSENDINPUTRESETS = 4134;
  SPI_SETBLOCKSENDINPUTRESETS = 4135;
  SPI_GETMOUSECLICKLOCKTIME = 8200;
  SPI_SETMOUSECLICKLOCKTIME = 8201;
  SPI_GETFONTSMOOTHINGTYPE = 8202;
  SPI_SETFONTSMOOTHINGTYPE = 8203;
  SPI_GETFONTSMOOTHINGCONTRAST = 8204;
  SPI_SETFONTSMOOTHINGCONTRAST = 8205;
  SPI_GETFOCUSBORDERWIDTH = 8206;
  SPI_SETFOCUSBORDERWIDTH = 8207;
  SPI_GETFOCUSBORDERHEIGHT = 8208;
  SPI_SETFOCUSBORDERHEIGHT = 8209;
  SPI_GETFONTSMOOTHINGORIENTATION = 8210;
  SPI_SETFONTSMOOTHINGORIENTATION = 8211;

//==============================================
// GetDeviceCaps constants
//==============================================
  BI_RGB        = 0;
  BI_RLE8       = 1;
  BI_RLE4       = 2;
  BI_BITFIELDS  = 3;


  HORZSIZE      = 4;   { Horizontal size in millimeters           }
  VERTSIZE      = 6;   { Vertical size in millimeters             }
  HORZRES       = 8;   { Horizontal width in pixels               }
  VERTRES       = 10;  { Vertical height in pixels                }
  BITSPIXEL     = 12;  { Number of bits per pixel                 }
  PLANES        = 14;  { Number of planes                         }
  NUMCOLORS     = 24;  { Number of colors                         }
  LOGPIXELSX    = 88;  { Logical pixelsinch in X                  }
  LOGPIXELSY    = 90;  { Logical pixelsinch in Y                  }
  SIZEPALETTE   = 104; { Number of entries in physical palette    }
  NUMRESERVED   = 106; { Number of reserved entries in palette    }


//==============================================
// Text Alignment Options
//==============================================
  TA_NOUPDATECP = 0;
  TA_UPDATECP   = 1;
  TA_LEFT       = 0;
  TA_RIGHT      = 2;
  TA_CENTER     = 6;
  TA_TOP        = 0;
  TA_BOTTOM     = 8;
  TA_BASELINE   = $18;
  TA_RTLREADING = $100;
  TA_MASK       = (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING);

//==============================================
// PolyFill() Modes
//==============================================
  ALTERNATE     = 1;
  WINDING       = 2;
  POLYFILL_LAST = 2;

//==============================================
// StretchBlt() Modes
//==============================================

  BLACKONWHITE      = 1;
  WHITEONBLACK      = 2;
  COLORONCOLOR      = 3;
  HALFTONE          = 4;
  MAXSTRETCHBLTMODE = 4;

  { constants for CreateDIBitmap }

  CBM_INIT = 4;     { initialize bitmap  }
  
//==============================================
// Predefined Resource Types
//==============================================
type
  {$IF DEFINED(UNICODE) or DEFINED(FPC_OS_UNICODE)}   //=== ct9999 CodeTyphon Studio for FPC SVN 24420 ============
  TResourceType = PWideChar;
  {$else}
  TResourceType = PChar;
  {$endif}
const
{$ifdef windows}
  RT_CURSOR = Windows.RT_CURSOR;
  RT_BITMAP = Windows.RT_BITMAP;
  RT_ICON = Windows.RT_ICON;
  RT_MENU = Windows.RT_MENU;
  RT_DIALOG = Windows.RT_DIALOG;
  RT_STRING = Windows.RT_STRING;
  RT_FONTDIR = Windows.RT_FONTDIR;
  RT_FONT = Windows.RT_FONT;
  RT_ACCELERATOR = Windows.RT_ACCELERATOR;
  RT_RCDATA = Windows.RT_RCDATA;
  RT_MESSAGETABLE = Windows.RT_MESSAGETABLE;
  RT_GROUP_CURSOR = Windows.RT_GROUP_CURSOR;
  RT_GROUP_ICON = Windows.RT_GROUP_ICON;
  RT_VERSION = Windows.RT_VERSION;
{$else}
  RT_CURSOR = TResourceType(1);
  RT_BITMAP = TResourceType(2);
  RT_ICON = TResourceType(3);
  RT_MENU = TResourceType(4);
  RT_DIALOG = TResourceType(5);
  RT_STRING = TResourceType(6);
  RT_FONTDIR = TResourceType(7);
  RT_FONT = TResourceType(8);
  RT_ACCELERATOR = TResourceType(9);
  RT_RCDATA = TResourceType(10);
  RT_MESSAGETABLE = TResourceType(11);
  RT_GROUP_CURSOR = TResourceType(12);
  RT_GROUP_ICON = TResourceType(14);
  RT_VERSION = TResourceType(16);
{$endif}

  // The following resource types are not available in Windows CE
{$ifdef mswindows}
  RT_ANICURSOR = Windows.RT_ANICURSOR;
  RT_ANIICON = Windows.RT_ANIICON;
  RT_HTML = Windows.RT_HTML;
  RT_MANIFEST = Windows.RT_MANIFEST;
{$else}
  RT_ANICURSOR = TResourceType(21);
  RT_ANIICON = TResourceType(22);
  RT_HTML = TResourceType(23);
  RT_MANIFEST = TResourceType(24);
{$endif}

//==============================================
// Load/CopyImage constants
//==============================================
const
  // types
  IMAGE_BITMAP      = 0;
  IMAGE_ICON        = 1;
  IMAGE_CURSOR      = 2;
  IMAGE_ENHMETAFILE = 3;

  // loadflags
  LR_DEFAULTCOLOR     = $0000;
  LR_MONOCHROME       = $0001;
  LR_COLOR            = $0002;
  LR_COPYRETURNORG    = $0004;
  LR_COPYDELETEORG    = $0008;
  LR_LOADFROMFILE     = $0010;
  LR_LOADTRANSPARENT  = $0020;
  LR_DEFAULTSIZE      = $0040;
  LR_VGACOLOR         = $0080;
  LR_LOADMAP3DCOLORS  = $1000;
  LR_CREATEDIBSECTION = $2000;
  LR_COPYFROMRESOURCE = $4000;
  LR_SHARED           = $8000;



type
  TFarProc = Pointer;

  TFNWndProc = TFarProc;

  PLogFontA = ^TLogFontA;
  PLogFontW = ^TLogFontW;
  PLogFont = PLogFontA;

  //win32, win64, but not wince because that is lfFaceName has WideChar on wince
  {$ifdef MSWINDOWS}
  LOGFONTA = Windows.LogFont;
  LOGFONTW = Windows.LogFontW;
  {$else}

  LOGFONTA = record
    lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint; // angle, in tenths of degrees of each line of text
    lfOrientation: Longint;// angle, in tenths of degrees of each character's base line
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;

  LOGFONTW = record
    lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint;
    lfOrientation: Longint;
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of WideChar;
  end;
  {$endif}
  tagLOGFONTA = LOGFONTA;
  tagLOGFONTW = LOGFONTW;
  tagLOGFONT = tagLOGFONTA;

  TLogFontA = tagLOGFONTA;
  TLogFontW = tagLOGFONTW;
  TLogFont = TLogFontA;


  LOGFONT = LOGFONTA;
  LPLOGFONT = ^LOGFONT;

  PLogBrush = ^TLogBrush;
  tagLOGBRUSH = record
    lbStyle: LongWord;
    lbColor: TColorRef;
    lbHatch: PtrUInt;
  end;
  TLogBrush = tagLOGBRUSH;
  LOGBRUSH = tagLOGBRUSH;

  // non-winapi radial gradient log info
  TLogGradientStop = record
    radColorR, radColorG, radColorB, radColorA: Word;
    radPosition: Double; // must be in 0..1
  end;
  TLogRadialGradient = record
    radCenterX, radCenterY, radRadius, radFocalX, radFocalY: Integer;
    radStops: array of TLogGradientStop;
  end;

  PMaxLogPalette = ^TMaxLogPalette; // not in Windows Headers
  TMaxLogPalette = record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array [Byte] of TPaletteEntry;
  end;

  PEnumLogFontA = ^TEnumLogFontA;
  PEnumLogFontW = ^TEnumLogFontW;
  PEnumLogFont = PEnumLogFontA;

  tagENUMLOGFONTA = record
    elfLogFont: TLogFontA;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of AnsiChar;
    elfStyle: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;

  tagENUMLOGFONTW = record
    elfLogFont: TLogFontW;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of WideChar;
    elfStyle: array[0..LF_FACESIZE - 1] of WideChar;
  end;

  tagENUMLOGFONT = tagENUMLOGFONTA;
  TEnumLogFontA = tagENUMLOGFONTA;
  TEnumLogFontW = tagENUMLOGFONTW;
  TEnumLogFont = TEnumLogFontA;

  ENUMLOGFONTA = tagENUMLOGFONTA;

  ENUMLOGFONTW = tagENUMLOGFONTW;

  ENUMLOGFONT = ENUMLOGFONTA;

  PEnumLogFontExA = ^TEnumLogFontExA;
  PEnumLogFontExW = ^TEnumLogFontExW;
  PEnumLogFontEx = PEnumLogFontExA;

  tagENUMLOGFONTEXA = record
    elfLogFont: TLogFontA;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of AnsiChar;
    elfStyle: array[0..LF_FACESIZE - 1] of AnsiChar;
    elfScript: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;

  tagENUMLOGFONTEXW = record
    elfLogFont: TLogFontW;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of WideChar;
    elfStyle: array[0..LF_FACESIZE - 1] of WideChar;
    elfScript: array[0..LF_FACESIZE - 1] of WideChar;
  end;

  tagENUMLOGFONTEX = tagENUMLOGFONTEXA;
  TEnumLogFontExA = tagENUMLOGFONTEXA;
  TEnumLogFontExW = tagENUMLOGFONTEXW;
  TEnumLogFontEx = TEnumLogFontExA;

  ENUMLOGFONTEXA = tagENUMLOGFONTEXA;

  ENUMLOGFONTEXW = tagENUMLOGFONTEXW;

  ENUMLOGFONTEX = ENUMLOGFONTEXA;

  PLogPen = ^TLogPen;
  tagLOGPEN = record
    lopnStyle: LongWord;
    lopnWidth: TPoint;
    lopnColor: TColorRef;
  end;
  TLogPen = tagLOGPEN;
  LOGPEN = tagLOGPEN;

  PExtLogPen = ^TExtLogPen;
  tagEXTLOGPEN = record
    elpPenStyle: LongWord;
    elpWidth: LongWord;
    elpBrushStyle: LongWord;
    elpColor: TColorRef;
    elpHatch: PtrInt;
    elpNumEntries: DWORD;
    elpStyleEntry: array[0..0] of DWORD;
  end;
  TExtLogPen = tagEXTLOGPEN;
  EXTLOGPEN = tagEXTLOGPEN;


type

  PTextMetricA = ^TTextMetricA;
  PTextMetricW = ^TTextMetricW;
  PTextMetric = PTextMetricA;

  tagTextMetricA = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: AnsiChar;
    tmLastChar: AnsiChar;
    tmDefaultChar: AnsiChar;
    tmBreakChar: AnsiChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;


  tagTEXTMETRICW = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: WideChar;
    tmLastChar: WideChar;
    tmDefaultChar: WideChar;
    tmBreakChar: WideChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;
  tagTEXTMETRIC = tagTEXTMETRICA;
  TTextMetricA = tagTEXTMETRICA;
  TTextMetricW = tagTEXTMETRICW;
  TTextMetric = TTextMetricA;
  TEXTMETRICA = tagTEXTMETRICA;
  TEXTMETRICW = tagTEXTMETRICW;
  TEXTMETRIC = TEXTMETRICA;


  TNewTextMetric = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: AnsiChar;
    tmLastChar: AnsiChar;
    tmDefaultChar: AnsiChar;
    tmBreakChar: AnsiChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
    ntmFlags: DWORD;
    ntmSizeEM: UINT;
    ntmCellHeight: UINT;
    ntmAvgWidth: UINT;
  end;

  TFontSignature = record
    fsUsb : array[0..3] of DWORD;
    fsCsb : array[0..1] of DWORD;
  end;

  TNewTextMetricEx = record
    ntmentm : TNewTextMetric;
    ntmeFontSignature : TFontSignature;
  end;

  FontEnumProc = function (var ELogFont:TEnumLogFont; var Metric:TNewTextMetric;
    FontType:longint; Data:LParam):longint; extdecl;

  FontEnumExProc = function (var ELogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx;
    FontType: Longint; Data:LParam):Longint; extdecl;

  MonitorEnumProc = function(hMonitor: HMONITOR; hdcMonitor: HDC; lprcMonitor: PRect;
    dwData: LPARAM): LongBool; extdecl;

  PWndClassExA = ^TWndClassExA;
  PWndClassExW = ^TWndClassExW;
  PWndClassEx = PWndClassExA;

  tagWNDCLASSEXA = record
    cbSize: UINT;
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    _hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PAnsiChar;
    lpszClassName: PAnsiChar;
    hIconSm: HICON;
  end;

  tagWNDCLASSEXW = record
    cbSize: UINT;
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    _hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PWideChar;
    lpszClassName: PWideChar;
    hIconSm: HICON;
  end;

  tagWNDCLASSEX = tagWNDCLASSEXA;
  TWndClassExA = tagWNDCLASSEXA;
  TWndClassExW = tagWNDCLASSEXW;
  TWndClassEx = TWndClassExA;
  WNDCLASSEXA = tagWNDCLASSEXA;
  WNDCLASSEXW = tagWNDCLASSEXW;
  WNDCLASSEX = WNDCLASSEXA;

  PWndClassA = ^TWndClassA;
  PWndClassW = ^TWndClassW;
  PWndClass = PWndClassA;

  tagWNDCLASSA = record
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PAnsiChar;
    lpszClassName: PAnsiChar;
  end;

  tagWNDCLASSW = record
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PWideChar;
    lpszClassName: PWideChar;
  end;

  tagWNDCLASS = tagWNDCLASSA;
  TWndClassA = tagWNDCLASSA;
  TWndClassW = tagWNDCLASSW;
  TWndClass = TWndClassA;
  WNDCLASSA = tagWNDCLASSA;
  WNDCLASSW = tagWNDCLASSW;
  WNDCLASS = WNDCLASSA;

const
  DefaultLogFont: TLogFont = (lfHeight:0;
    lfWidth:0;
    lfEscapement:0;
    lfOrientation:0;
    lfWeight:0;
    lfItalic:0;
    lfUnderline:0;
    lfStrikeOut:0;
    lfCharSet:0;
    lfOutPrecision:0;
    lfClipPrecision:0;
    lfQuality:0;
    lfPitchAndFamily:0;
    lfFaceName:'default';
  );

type
  PMsg = ^TMsg;
  tagMSG = record
    hwnd: HWND;
    message: LongWord;
    wParam: WPARAM;
    lParam: LPARAM;
    time: DWORD;
    pt: TPoint;
  end;
  TMsg = tagMSG;
  _MSG = tagMSG;


type
  // Moved from Controls to avoid circles
  // Since it is part of the interface now
  TCreateParams = record
    Caption: PChar;
    Style: Cardinal;
    ExStyle: Cardinal;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..63] of Char;
  end;
  
type
{$ifdef windows}
  TIconInfo = Windows.TICONINFO;
  PIconInfo = Windows.PICONINFO;
{$else windows}
  TIconInfo = record
    fIcon: BOOL;
    xHotspot: DWORD;
    yHotspot: DWORD;
    hbmMask: HBITMAP;
    hbmColor: HBITMAP;
  end;
  PIconInfo = ^TIconInfo;
{$endif windows}


//------------------------------------------------------------------------------
//timer
type
  TTimerProc = procedure(hWnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;

  TLMTimer = record
    Msg: Cardinal;
    TimerID: PtrUInt;
    TimerProc: TFarProc;
    Result: LRESULT;
  end;

  // delphi
  TFNTimerProc = TFarProc;


//------------------------------------------------------------------------------
// clipboard
type
  TClipboardFormat = PtrUInt;
  PClipboardFormat = ^TClipboardFormat;

  TClipboardRequestEvent = procedure(const RequestedFormatID: TClipboardFormat;
    Data: TStream) of object;

  TClipboardType = (ctPrimarySelection, ctSecondarySelection, ctClipboard);

const
  ClipboardTypeName : array[TClipboardType] of string = (
      'primary selection', 'secondary selection', 'clipboard'
    );

type
  TPredefinedClipboardFormat = (
      pcfText,
      pcfBitmap,
      pcfPixmap,
      pcfIcon,
      pcfPicture,
      pcfObject,
      pcfComponent,
      pcfCustomData,

      // Delphi definitions (only for compatibility)
      pcfDelphiText,
      pcfDelphiBitmap,
      pcfDelphiPicture,
      pcfDelphiMetaFilePict,
      pcfDelphiObject,
      pcfDelphiComponent,

      // Kylix definitions (only for compatibility)
      pcfKylixPicture,
      pcfKylixBitmap,
      pcfKylixDrawing,
      pcfKylixComponent
    );

const
  PredefinedClipboardMimeTypes : array[TPredefinedClipboardFormat] of string = (
     'text/plain',
     'image/bmp',
     'image/xpm',
     'image/lcl.icon',
     'image/lcl.picture',
     'application/lcl.object',
     'application/lcl.component',
     'application/lcl.customdata',

     // Delphi definitions (only for compatibility)
     'text/plain',
     'image/delphi.bitmap',
     'Delphi Picture',
     'image/delphi.metafilepict',
     'application/delphi.object',
     'Delphi Component',

     // Kylix definitons (only for compatibility)
     'image/delphi.picture',
     'image/delphi.bitmap',
     'image/delphi.drawing',
     'application/delphi.component'
  );


type
  TListChangeEvent = procedure(Ptr: Pointer; AnAction: TListNotification) of object;

  { TListWithEvent }

  TListWithEvent = class(TList)
  private
    FOnChange: TListChangeEvent;
  protected
    procedure Notify(Ptr: Pointer; AnAction: TListNotification); override;
  public
    property OnChange: TListChangeEvent read FOnChange write FOnChange;
  end;

const
  csNone = 0;
//  csAlignment = 1;
//  csBox = 2;
  csButton = 3;
  csComboBox = 4;
  csCheckbox = 5;
  csEdit = 6;
  csForm= 7;
  csStaticText = 8;
//  csgtkTable = 9;
  csScrollBar = 10;
  csListView = 11;
//  csMainForm = 12;
  csMemo = 13;
  csMainMenu = 14;
  csMenuBar = 15;
  csMenuItem = 16;
  csNotebook = 17;
  csFileDialog = 18;
  csRadioButton = 19;
  csScrolledWindow= 20;
  csSpinedit = 21;
  csStatusBar = 22;
//  csTable = 23;
  csToggleBox = 24;
  //csVScrollBar = 25;
//  csFrame = 26;
//  csButtonBox = 27;
//  csCanvas = 28;
  csGroupBox = 29;

//  csFont = 30;
//  csPen = 31;
//  csBrush = 32;
  //csTimer = 33;
  csPage = 34;

  csColorDialog = 35;
  csListBox = 36;
  csFontDialog = 37;
  csProgressBar = 38;
  csTrackBar = 39;
  csWinControl = 40;
  csFixed = csWinControl; //TODO remove
//  csImage = 41;
//  csToolbar = 42;
//  csToolButton = 43;
  csBitBtn = 44;
  csCListBox = 45;
//  csSpeedButton = 46;
  csPopupMenu = 47;
  csHintWindow = 48;

  csCalendar = 49;

  csArrow = 50;
  csPanel = 51;
  csScrollBox = 52;

  csCheckListBox = 53;
  csPairSplitter = 54;
  csPairSplitterSide = 55;

  csOpenFileDialog = 56;
  csSaveFileDialog = 57;
  csSelectDirectoryDialog = 58;
  csPreviewFileControl = 59;
  csPreviewFileDialog = 60;

  csNonLCL = 61; // for non LCL controls, that create their own handles


const
  // Mouse message key states
  MK_LBUTTON  = 1;
  MK_RBUTTON = 2;
  MK_SHIFT = 4;
  MK_CONTROL = 8;
  MK_MBUTTON = $10;
  MK_XBUTTON1 = $20;
  MK_XBUTTON2 = $40;
  // following are "virtual" key states
  MK_DOUBLECLICK = $80;
  MK_TRIPLECLICK = $100;
  MK_QUADCLICK = $200;

//==============================================
// Constants from commctrl
//==============================================

//-------------
// Common
//-------------

const
  //all controls
  NM_FIRST           = 0;      
  NM_OUTOFMEMORY     = NM_FIRST - 1;
  NM_CLICK           = NM_FIRST - 2;
  NM_DBLCLK          = NM_FIRST - 3;
  NM_RETURN          = NM_FIRST - 4;
  NM_RCLICK          = NM_FIRST - 5;
  NM_RDBLCLK         = NM_FIRST - 6;
  NM_SETFOCUS        = NM_FIRST - 7;
  NM_KILLFOCUS       = NM_FIRST - 8;
  NM_CUSTOMDRAW      = NM_FIRST - 12;
  NM_HOVER           = NM_FIRST - 13;
  NM_NCHITTEST       = NM_FIRST - 14;   
  NM_KEYDOWN         = NM_FIRST - 15;   
  NM_RELEASEDCAPTURE = NM_FIRST - 16;
  NM_SETCURSOR       = NM_FIRST - 17;   
  NM_CHAR            = NM_FIRST - 18;   

  NM_LAST            = NM_FIRST - 99;
    
  //listview
  LVN_FIRST    = -100;   
  LVN_LAST     = -199;
  
  //header
  HDN_FIRST    = -300;   
  HDN_LAST     = -399;
  
  //treeview
  TVN_FIRST    = -400;   
  TVN_LAST     = -499;
  
  //tooltips
  TTN_FIRST    = -520;   
  TTN_LAST     = -549;
  
  //tab control
  TCN_FIRST    = -550;   
  TCN_LAST     = -580;
  
  // toolbar
  TBN_First    = -700;
  TBN_Last     = -720;

const
  CCS_TOP                 = $00000001;
  CCS_NOMOVEY             = $00000002;
  CCS_BOTTOM              = $00000003;
  CCS_NORESIZE            = $00000004;
  CCS_NOPARENTALIGN       = $00000008;
  CCS_ADJUSTABLE          = $00000020;
  CCS_NODIVIDER           = $00000040;
  CCS_VERT                = $00000080;
  CCS_LEFT                = (CCS_VERT or CCS_TOP);
  CCS_RIGHT               = (CCS_VERT or CCS_BOTTOM);
  CCS_NOMOVEX             = (CCS_VERT or CCS_NOMOVEY);

//-------------
// Listview
//-------------
const
  LVN_ITEMCHANGING    = LVN_FIRST-0;
  LVN_ITEMCHANGED     = LVN_FIRST-1;
  LVN_INSERTITEM      = LVN_FIRST-2;
  LVN_DELETEITEM      = LVN_FIRST-3;
  LVN_DELETEALLITEMS  = LVN_FIRST-4;
  LVN_COLUMNCLICK     = LVN_FIRST-8;
  LVN_BEGINDRAG       = LVN_FIRST-9;
  LVN_BEGINRDRAG      = LVN_FIRST-11;
  LVN_ODCACHEHINT     = LVN_FIRST-13;
  LVN_ODSTATECHANGED  =  LVN_FIRST-15;
  LVN_ODFINDITEM      = LVN_FIRST-79;

const
  LVIF_TEXT           = $0001;
  LVIF_IMAGE          = $0002;
  LVIF_PARAM          = $0004;
  LVIF_STATE          = $0008;
  LVIF_INDENT         = $0010;
  LVIF_NORECOMPUTE    = $0800;

  LVIS_FOCUSED        = $0001;
  LVIS_SELECTED       = $0002;
  LVIS_CUT            = $0004;
  LVIS_DROPHILITED    = $0008;
  LVIS_ACTIVATING     = $0020;

  LVIS_OVERLAYMASK    = $0F00;
  LVIS_STATEIMAGEMASK = $F000;

type
  PNMListView = ^TNMListView;
  TNMListView = packed record
    hdr: TNMHDR;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
  end;
  _NM_LISTVIEW = TNMListView;
  NM_LISTVIEW = TNMListView;
  tagNMLISTVIEW = TNMListView;

  // enum to use with InitStockFont
  TStockFont = (
    sfSystem,  // stock system font
    sfHint,    // stock hint font
    sfIcon,    // stock icon font
    sfMenu     // stock menu font
  );

function CS_To_String(CompStyle: Integer): String;
// key mapping

function HiWord(i: integer): word;
function LoWord(i: integer): word;
function Char2VK(C : Char) : Word;
function VK2Char(AVK: Word): Char;
function MathRound(AValue: ValReal): Int64;
function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
function KeyToShortCut(const Key: Word; const Shift: TShiftState): TShortCut;
function CharSetToString(const Charset: Integer): String;
function StringToCharset(Charset: string): byte;


implementation


function HiWord(i: integer): word;
begin
  Result:=Hi(i);
end;

function LoWord(i: integer): word;
begin
  Result:=Lo(i);
end;

function Char2VK(C : Char) : Word;
begin
  Case C of
    '0'..'9' :Result := VK_0 + Ord(C) - Ord('0');
    'a'..'z' :Result := VK_A + Ord(C) - Ord('a');
    'A'..'Z' :Result := VK_A + Ord(C) - Ord('A');
  else
    Result:=0;
  end;
end;

function VK2Char(AVK: Word): Char;
begin
  case AVK of
    VK_0..VK_9: Result := chr(ord('0')+AVK-VK_0);
    VK_A..VK_Z: Result := chr(ord('a')+AVK-VK_A);
  else
    Result:='?';
  end;
end;

function MathRound(AValue: ValReal): Int64; inline;
begin
  if AValue >= 0 then
    Result := Trunc(AValue + 0.5)
  else
    Result := Trunc(AValue - 0.5);
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(int64(nNumber) * int64(nNumerator) / nDenominator);
end;

function KeyToShortCut(const Key: Word; const Shift: TShiftState): TShortCut;
begin
  Result := Key;
  if (Result and $FF00) <> 0 then begin
    Result:=0;
    exit;
  end;

  if ssShift in Shift then Inc(Result,scShift);
  if ssCtrl in Shift then Inc(Result,scCtrl);
  if ssAlt in Shift then Inc(Result,scAlt);
  if ssMeta in Shift then Inc(Result,scMeta);
end;

{------------------------------------------------------------------------------
  Function: CS_To_String
  Params: CompStyle - Component Style
  Returns: The component style name

  Converts a component style identIfier into the correct component style name
 ------------------------------------------------------------------------------}
function CS_To_String(CompStyle: Integer): String;
begin
  case CompStyle of
    csNone:
      Result := 'csNone';
{    csAlignment:
      Result := 'csAlignment';
    csBox:
      Result := 'csBox';
}
    csButton:
      Result := 'csButton';
    csComboBox:
      Result := 'csComboBox';
    csCheckbox:
      Result := 'csCheckbox';
    csEdit:
      Result := 'csEdit';
    csForm:
      Result := 'csForm';
    csStaticText:
      Result := 'csStaticText';
//    csGTKTable:
//      Result := 'csGTKTable';
    csScrollBar:
      Result := 'csScrollBar';
    csListView:
      Result := 'csListView';
//    csMainForm:
//      Result := 'csMainForm';
    csMemo:
      Result := 'csMemo';
    csMainMenu:
      Result := 'csMainMenu';
    csMenuBar:
      Result := 'csMenuBar';
    csMenuItem:
      Result := 'csMenuItem';
    csNotebook:
      Result := 'csNotebook';
    csFileDialog:
      Result := 'csFileDialog';
    csOpenFileDialog:
      Result := 'csOpenFileDialog';
    csSaveFileDialog:
      Result := 'csSaveFileDialog';
    csSelectDirectoryDialog:
      Result := 'csSelectDirectoryDialog';
    csRadioButton:
      Result := 'csRadioButton';
    csScrolledWinDow:
      Result := 'csScrolledWinDow';
    csSpinEdit:
      Result := 'csSpinEdit';
    csStatusBar:
      Result := 'csStatusBar';
//    csTable:
//      Result := 'csTable';
    csToggleBox:
      Result := 'csToggleBox';
//    25: //csVScrollBar
//      Result := 'csVScrollBar';
//    csFrame:
//      Result := 'csFrame';
//    csButtonBox:
//      Result := 'csButtonBox';
//    csCanvas:
//      Result := 'csCanvas';
    csGroupBox:
      Result := 'csGroupBox';
//    csFont:
//      Result := 'csFont';
//    csPen:
//      Result := 'csPen';
//    csBrush:
//      Result := 'csBrush';
//    33: //csTimer
//      Result := 'csTimer';
    csPage:
      Result := 'csPage';
    csColorDialog:
      Result := 'csColorDialog';
    csListBox:
      Result := 'csListBox';
    csFontDialog:
      Result := 'csFontDialog';
    csProgressBar:
      Result := 'csProgressBar';
    csTrackBar:
      Result := 'csTrackBar';
    csFixed:
      Result := 'csFixed';
{    csImage:
      Result := 'csImage';
    csToolbar:
      Result := 'csToolbar';
    csToolButton:
      Result := 'csToolButton';}
    csBitBtn:
      Result := 'csBitBtn';
    csCListBox:
      Result := 'csCListBox';
{    csSpeedButton:
      Result := 'csSpeedButton';}
    csPopupMenu:
      Result := 'csPopupMenu';
    csHintWinDow:
      Result := 'csHintWinDow';
    csCalendar:
      Result := 'csCalendar';
    csArrow:
      Result := 'csArrow';
    csPanel:
      Result := 'csPanel';
    csScrollBox:
      Result := 'csScrollBox';
    csCheckListBox:
      Result := 'csCheckListBox';
    csPairSplitter:
      Result := 'csPairSplitter';
    csPairSplitterSide:
      Result := 'csPairSplitterSide';
    csPreviewFileControl:
      Result := 'csPreviewFileControl';
    csPreviewFileDialog:
      Result := 'csPreviewFileDialog';
    csNonLCL:
      Result := 'csNonLCL';
    else
      Result := Format('Unknown component style %D', [CompStyle]);
  end; {case}
End;

function CharSetToString(const Charset: Integer): String;
begin
  case Charset of
    ANSI_CHARSET:         result := 'ANSI_CHARSET';
    DEFAULT_CHARSET:      result := 'DEFAULT_CHARSET';
    SYMBOL_CHARSET:       result := 'SYMBOL_CHARSET';
    MAC_CHARSET:          result := 'MAC_CHARSET';
    SHIFTJIS_CHARSET:     result := 'SHIFTJIS_CHARSET';
    HANGEUL_CHARSET:      result := 'HANGEUL_CHARSET';
    JOHAB_CHARSET:        result := 'JOHAB_CHARSET';
    GB2312_CHARSET:       result := 'GB2312_CHARSET';
    CHINESEBIG5_CHARSET:  result := 'CHINESEBIG5_CHARSET';
    GREEK_CHARSET:        result := 'GREEK_CHARSET';
    TURKISH_CHARSET:      result := 'TURKISH_CHARSET';
    VIETNAMESE_CHARSET:   result := 'VIETNAMESE_CHARSET';
    HEBREW_CHARSET:       result := 'HEBREW_CHARSET';
    ARABIC_CHARSET:       result := 'ARABIC_CHARSET';
    BALTIC_CHARSET:       result := 'BALTIC_CHARSET';
    RUSSIAN_CHARSET:      result := 'RUSSIAN_CHARSET';
    THAI_CHARSET:         result := 'THAI_CHARSET';
    EASTEUROPE_CHARSET:   result := 'EASTEUROPE_CHARSET';
    OEM_CHARSET:          result := 'OEM_CHARSET';
    FCS_ISO_10646_1:      result := 'UNICODE';
    FCS_ISO_8859_1:       result := 'FCS_ISO_8859_1';
    FCS_ISO_8859_2:       result := 'FCS_ISO_8859_2';
    FCS_ISO_8859_3:       result := 'FCS_ISO_8859_3';
    FCS_ISO_8859_4:       result := 'FCS_ISO_8859_4';
    FCS_ISO_8859_5:       result := 'FCS_ISO_8859_5';
    FCS_ISO_8859_6:       result := 'FCS_ISO_8859_6';
    FCS_ISO_8859_7:       result := 'FCS_ISO_8859_7';
    FCS_ISO_8859_8:       result := 'FCS_ISO_8859_8';
    FCS_ISO_8859_9:       result := 'FCS_ISO_8859_9';
    FCS_ISO_8859_10:      result := 'FCS_ISO_8859_10';
    FCS_ISO_8859_15:      result := 'FCS_ISO_8859_15';
    else result := '';
  end;
end;

function StringToCharset(Charset: string): Byte;
begin
  Charset := uppercase(charset);
  if Charset = 'ANSI_CHARSET' then result := ANSI_CHARSET else
  if Charset = 'DEFAULT_CHARSET' then result := DEFAULT_CHARSET else
  if Charset = 'SYMBOL_CHARSET' then result := SYMBOL_CHARSET else
  if Charset = 'MAC_CHARSET' then result := MAC_CHARSET else
  if Charset = 'SHIFTJIS_CHARSET' then result := SHIFTJIS_CHARSET else
  if Charset = 'HANGEUL_CHARSET' then result := SHIFTJIS_CHARSET else
  if Charset = 'JOHAB_CHARSET' then result := JOHAB_CHARSET else
  if Charset = 'GB2312_CHARSET' then result := GB2312_CHARSET else
  if Charset = 'CHINESEBIG5_CHARSET' then result := CHINESEBIG5_CHARSET else
  if Charset = 'GREEK_CHARSET' then result := GREEK_CHARSET else
  if Charset = 'TURKISH_CHARSET' then result := TURKISH_CHARSET else
  if Charset = 'VIETNAMESE_CHARSET' then result := VIETNAMESE_CHARSET else
  if Charset = 'HEBREW_CHARSET' then result := HEBREW_CHARSET else
  if Charset = 'ARABIC_CHARSET' then result := ARABIC_CHARSET else
  if Charset = 'BALTIC_CHARSET' then result := BALTIC_CHARSET else
  if Charset = 'RUSSIAN_CHARSET' then result := RUSSIAN_CHARSET else
  if Charset = 'THAI_CHARSET' then result := THAI_CHARSET else
  if Charset = 'EASTEUROPE_CHARSET' then result := EASTEUROPE_CHARSET else
  if Charset = 'OEM_CHARSET' then result := OEM_CHARSET else
  if Charset = 'UNICODE' then result := FCS_ISO_10646_1 else
  if Charset = 'FCS_ISO_8859_1' then result := FCS_ISO_8859_1 else
  if Charset = 'FCS_ISO_8859_2' then result := FCS_ISO_8859_2 else
  if Charset = 'FCS_ISO_8859_3' then result := FCS_ISO_8859_3 else
  if Charset = 'FCS_ISO_8859_4' then result := FCS_ISO_8859_4 else
  if Charset = 'FCS_ISO_8859_5' then result := FCS_ISO_8859_5 else
  if Charset = 'FCS_ISO_8859_6' then result := FCS_ISO_8859_6 else
  if Charset = 'FCS_ISO_8859_7' then result := FCS_ISO_8859_7 else
  if Charset = 'FCS_ISO_8859_8' then result := FCS_ISO_8859_8 else
  if Charset = 'FCS_ISO_8859_9' then result := FCS_ISO_8859_9 else
  if Charset = 'FCS_ISO_8859_10' then result := FCS_ISO_8859_10 else
  if Charset = 'FCS_ISO_8859_15' then result := FCS_ISO_8859_15
  else
    result := DEFAULT_CHARSET;

end;

{ TListWithEvent }

procedure TListWithEvent.Notify(Ptr: Pointer; AnAction: TListNotification);
begin
  inherited Notify(Ptr, AnAction);
  if Assigned(OnChange) then OnChange(Ptr,AnAction);
end;

end.
