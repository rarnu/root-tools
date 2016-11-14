{
 /***************************************************************************
                                  lclproc.pas
                                  -----------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Useful lower level helper functions and classes.
}
unit LCLProc;

{$MODE ObjFPC}{$H+}
{$I lcl_defines.inc}
{$inline on}

interface

uses
  {$IFDEF Darwin}MacOSAll, {$ENDIF}
  {$IFDEF win32}
  Win9xWsManager, // Support for Lower/UpperWideStringProc on Win9x, also used by some Utf8 string handling functions
  {$ENDIF}
  Classes, SysUtils, Math, TypInfo, Types, FPCAdds, AvgLvlTree, LazFileUtils,
  LCLStrConsts, LCLType, WSReferences, LazMethodList, LazUTF8, LazUTF8Classes;

type
  TMethodList = LazMethodList.TMethodList;

  TStackTracePointers = array of Pointer;

  TLineInfoCacheItem = record
    Addr: Pointer;
    Info: string;
  end;
  PLineInfoCacheItem = ^TLineInfoCacheItem;

{$IFDEF DebugLCLComponents}
var
  DebugLCLComponents: TDebugLCLItems = nil;
{$ENDIF}


function CompareLineInfoCacheItems(Data1, Data2: Pointer): integer;
function CompareAddrWithLineInfoCacheItem(Addr, Item: Pointer): integer;


type
  TStringsSortCompare = function(const Item1, Item2: string): Integer;

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare); overload;// sort so that for each i is OnCompare(List[i],List[i+1])<=0
procedure MergeSort(List: TFPList; StartIndex, EndIndex: integer; const OnCompare: TListSortCompare); overload;// sort so that for each i is OnCompare(List[i],List[i+1])<=0
procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare); overload;// sort so that for each i is OnCompare(List[i],List[i+1])<=0

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
                         const DefaultValue: Integer): Integer;

function KeyAndShiftStateToKeyString(Key: word; ShiftState: TShiftState): String;
function KeyStringIsIrregular(const s: string): boolean;
function ShortCutToText(ShortCut: TShortCut): string;// untranslated
function TextToShortCut(const ShortCutText: string): TShortCut;// untranslated

function GetCompleteText(const sText: string; iSelStart: Integer;
  bCaseSensitive, bSearchAscending: Boolean; slTextList: TStrings): string;
function IsEditableTextKey(Key: Word): Boolean;

// Hooks used to prevent unit circles
type
  TSendApplicationMessageFunction =
    function(Msg: Cardinal; WParam: WParam; LParam: LParam):Longint;
  TOwnerFormDesignerModifiedProc =
    procedure(AComponent: TComponent);


var
  SendApplicationMessageFunction: TSendApplicationMessageFunction=nil;
  OwnerFormDesignerModifiedProc: TOwnerFormDesignerModifiedProc=nil;

function SendApplicationMessage(Msg: Cardinal; WParam: WParam; LParam: LParam):Longint;
procedure OwnerFormDesignerModified(AComponent: TComponent);
procedure FreeThenNil(var obj);

{ the LCL interfaces finalization sections are called before the finalization
  sections of the LCL. Those parts, that should be finalized after the LCL, can
  be registered here. }
procedure RegisterInterfaceInitializationHandler(p: TProcedure);
procedure CallInterfaceInitializationHandlers;
procedure RegisterInterfaceFinalizationHandler(p: TProcedure);
procedure CallInterfaceFinalizationHandlers;

function OffsetRect(var ARect: TRect; dx, dy: Integer): Boolean;
procedure MoveRect(var ARect: TRect; x, y: Integer);
procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
procedure MakeMinMax(var i1, i2: integer);
procedure CalculateLeftTopWidthHeight(X1,Y1,X2,Y2: integer;
  out Left,Top,Width,Height: integer);

function DeleteAmpersands(var Str : String) : Longint;
function BreakString(const s: string; MaxLineLength, Indent: integer): string;

function ComparePointers(p1, p2: Pointer): integer;
function CompareHandles(h1, h2: THandle): integer;
function CompareLCLHandles(h1, h2: TLCLHandle): integer;
function CompareRect(R1, R2: PRect): Boolean;
function ComparePoints(const p1, p2: TPoint): integer;
function CompareMethods(const m1, m2: TMethod): boolean;

function RoundToInt(const e: Extended): integer;
function RoundToCardinal(const e: Extended): cardinal;
function TruncToInt(const e: Extended): integer;
function TruncToCardinal(const e: Extended): cardinal;
function StrToDouble(const s: string): double;


// debugging
procedure RaiseAndCatchException;
function GetStackTrace(UseCache: boolean): string;
procedure GetStackTracePointers(var AStack: TStackTracePointers);
function StackTraceAsString(const AStack: TStackTracePointers;
                            UseCache: boolean): string;
function GetLineInfo(Addr: Pointer; UseCache: boolean): string;

{$IFnDEF WithOldDebugln}
{$ELSE}
procedure DebugLn(Args: array of const); overload;
procedure DebugLn(const S: String; Args: array of const); overload;// similar to Format(s,Args)
procedure DebugLn; overload;
procedure DebugLn(const s: string); overload;
procedure DebugLn(const s1,s2: string); overload;
procedure DebugLn(const s1,s2,s3: string); overload;
procedure DebugLn(const s1,s2,s3,s4: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16: string); overload;

procedure DebugLnEnter(const s: string = ''); overload;
procedure DebugLnEnter(Args: array of const); overload;
procedure DebugLnEnter(s: string; Args: array of const); overload;
procedure DebugLnEnter(const s1, s2: string; const s3: string = '';
                     const s4: string = ''; const s5: string = ''; const s6: string = '';
                     const s7: string = ''; const s8: string = ''; const s9: string = '';
                     const s10: string = ''; const s11: string = ''; const s12: string = '';
                     const s13: string = ''; const s14: string = ''; const s15: string = '';
                     const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;
procedure DebugLnExit(const s: string = ''); overload;
procedure DebugLnExit(Args: array of const); overload;
procedure DebugLnExit(s: string; Args: array of const); overload;
procedure DebugLnExit (const s1, s2: string; const s3: string = '';
                     const s4: string = ''; const s5: string = ''; const s6: string = '';
                     const s7: string = ''; const s8: string = ''; const s9: string = '';
                     const s10: string = ''; const s11: string = ''; const s12: string = '';
                     const s13: string = ''; const s14: string = ''; const s15: string = '';
                     const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

procedure DbgOut(const S: String; Args: array of const); overload;
procedure DbgOut(const s: string); overload;
procedure DbgOut(const s1,s2: string); overload;
procedure DbgOut(const s1,s2,s3: string); overload;
procedure DbgOut(const s1,s2,s3,s4: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string); overload;

procedure CloseDebugOutput;
{$ENDIF}

function DbgSWindowPosFlags(Flags: UInt): String;
function DbgsVKCode(c: word): string;
function DbgS(const ATM: TTextMetric): string; overload;
function DbgS(const AScrollInfo: TScrollInfo): string; overload;
function DbgS(const AVariant: Variant): string; overload;

procedure DbgOutThreadLog(const Msg: string); overload;
procedure DebuglnThreadLog(const Msg: string); overload;
procedure DebuglnThreadLog; overload;
procedure DbgSaveData(FileName: String; AData: PChar; ADataSize: PtrUInt);
procedure DbgAppendToFile(FileName, S: String);
procedure DbgAppendToFileWithoutLn(FileName, S: String);

// some string manipulation functions
function StripLN(const ALine: String): String;
function GetPart(const ASkipTo, AnEnd: String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String; overload;
function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String; overload;
function TextToSingleLine(const AText: string): string;
function SwapCase(Const S: String): String;

// case..of utility functions
function StringCase(const AString: String; const ACase: array of String {; const AIgnoreCase = False, APartial = false: Boolean}): Integer; overload;
function StringCase(const AString: String; const ACase: array of String; const AIgnoreCase, APartial: Boolean): Integer; overload;
function ClassCase(const AClass: TClass; const ACase: array of TClass {; const ADescendant: Boolean = True}): Integer; overload;
function ClassCase(const AClass: TClass; const ACase: array of TClass; const ADecendant: Boolean): Integer; overload;

// MWE: define (missing) UTF16string similar to UTF8
//      strictly spoken, a widestring <> utf16string
// todo: use it in existing functions
type
  UTF16String = type UnicodeString;
  PUTF16String = ^UTF16String;

// Felipe: Don't substitute with calls to lazutf16 because lazutf16 includes
// some initialization code and tables, which are not necessary for the LCL
function UTF16CharacterLength(p: PWideChar): integer;
function UTF16Length(const s: UTF16String): PtrInt;
function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
function UnicodeToUTF16(u: cardinal): UTF16String;

{$IFnDEF DisableWrapperFunctions}
function UTF8CharacterLength(p: PChar): integer; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8Length(const s: string): PtrInt; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal; inline; deprecated 'Use the function in LazUTF8 unit';
function UnicodeToUTF8(u: cardinal; Buf: PChar): integer; inline; deprecated 'Use the function in LazUTF8 unit';
function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer; inline; deprecated 'Use the function in LazUTF8 unit';
function UnicodeToUTF8(u: cardinal): shortstring; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8ToDoubleByteString(const s: string): string; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
                                  BytePos: integer): integer; inline; deprecated 'Use the function in LazUTF8 unit';
// find the n-th UTF8 character, ignoring BIDI
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar; inline; deprecated 'Use the function in LazUTF8 unit';
// find the byte index of the n-th UTF8 character, ignoring BIDI (byte len of substr)
function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt; inline; deprecated 'Use the function in LazUTF8 unit';
procedure UTF8FixBroken(P: PChar); inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8CharacterStrictLength(P: PChar): integer; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8Pos(const SearchForText, SearchInText: string): PtrInt; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string; inline; deprecated 'Use the function in LazUTF8 unit';
procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt); inline; deprecated 'Use the function in LazUTF8 unit';
procedure UTF8Insert(const source: String; var s: string; StartCharIndex: PtrInt); inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8LowerCase(const s: String): String; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8UpperCase(const s: String): String; inline; deprecated 'Use the function in LazUTF8 unit';
function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
                                  StopOnNonASCII: Boolean = true): PtrInt; inline; deprecated 'Use the function in LazUTF8 unit';
function ValidUTF8String(const s: String): String; inline; deprecated 'Use the function in LazUTF8 unit';

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings); inline; deprecated 'Use the function in LazUTF8 unit';

//compare functions

function UTF8CompareStr(const S1, S2: String): Integer; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8CompareText(const S1, S2: String): Integer; inline; deprecated 'Use the function in LazUTF8 unit';

type
  TConvertResult = LazUTF8.TConvertResult;
  TConvertOption = LazUTF8.TConvertOption;
  TConvertOptions = LazUTF8.TConvertOptions;

function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult; inline; deprecated 'Use the function in LazUTF8 unit';

function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult; inline; deprecated 'Use the function in LazUTF8 unit';

function UTF8ToUTF16(const S: AnsiString): UTF16String; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF16ToUTF8(const S: UTF16String): AnsiString; inline; deprecated 'Use the function in LazUTF8 unit';

// locale
procedure LCLGetLanguageIDs(var Lang, FallbackLang: String); inline; deprecated 'Use function LazGetLanguageIDs in LazUTF8 unit';
{$ENDIF DisableWrapperFunctions}

// identifier
function CreateFirstIdentifier(const Identifier: string): string;
function CreateNextIdentifier(const Identifier: string): string;

{$IFDEF WithOldDebugln}
type
  TDebugLnProc = procedure (s: string) of object;

var
  DebugLnMaxNestPrefixLen: Integer = 15;
  DebugLnNestLvlIndent: Integer = 2;
  DebugText: ^Text;

  DebugLnProc: TDebugLnProc = nil;
  DebugOutProc: TDebugLnProc = nil;
{$ENDIF}

implementation

uses gettext;

const
  {$IFDEF WithOldDebugln}
  Str_LCL_Debug_File = 'lcldebug.log';
  {$ENDIF}
  UNKNOWN_VK_PREFIX = 'Word(''';
  UNKNOWN_VK_POSTFIX = ''')';

var
  InterfaceInitializationHandlers: TFPList = nil;
  InterfaceFinalizationHandlers: TFPList = nil;
  {$IFDEF WithOldDebugln}
  DebugTextAllocated: boolean;
  DebugNestLvl: Integer = 0;
  DebugNestPrefix: PChar = nil;
  DebugNestAtBOL: Boolean;
  {$ENDIF}
  LineInfoCache: TAvgLvlTree = nil;

function DeleteAmpersands(var Str : String) : Longint;
// Replace all &x with x
// and return the position of the first ampersand letter in the resulting Str.
// double ampersands && are converted to a single & and are ignored.
var
  SrcPos, DestPos, SrcLen: Integer;
begin
  Result:=-1;
  SrcLen:=length(Str);
  SrcPos:=1;
  DestPos:=1;
  while SrcPos<=SrcLen do begin
    if (Str[SrcPos]='&') and (SrcPos<SrcLen) then begin
      // & found
      inc(SrcPos); // skip &
      if (Str[SrcPos]<>'&') and (Result<1) then
        Result:=DestPos;
    end;
    if DestPos<SrcPos then
      Str[DestPos]:=Str[SrcPos];
    inc(SrcPos);
    inc(DestPos);
  end;
  if DestPos<SrcPos then
    SetLength(Str,DestPos-1);
end;

//-----------------------------------------------------------------------------
// Keys and shortcuts

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt, mkcMeta);

var
  MenuKeyCaps: array[TMenuKeyCap] of string;
  MenuKeyCapsInited: boolean = false;

procedure InitializeMenuKeyCaps;
begin
  if MenuKeyCapsInited=false then
  begin
    MenuKeyCaps[mkcBkSp]:=SmkcBkSp;
    MenuKeyCaps[mkcTab]:=SmkcTab;
    MenuKeyCaps[mkcEsc]:=SmkcEsc;
    MenuKeyCaps[mkcEnter]:=SmkcEnter;
    MenuKeyCaps[mkcSpace]:=SmkcSpace;
    MenuKeyCaps[mkcPgUp]:=SmkcPgUp;
    MenuKeyCaps[mkcPgDn]:=SmkcPgDn;
    MenuKeyCaps[mkcEnd]:=SmkcEnd;
    MenuKeyCaps[mkcHome]:=SmkcHome;
    MenuKeyCaps[mkcLeft]:=SmkcLeft;
    MenuKeyCaps[mkcUp]:=SmkcUp;
    MenuKeyCaps[mkcRight]:=SmkcRight;
    MenuKeyCaps[mkcDown]:=SmkcDown;
    MenuKeyCaps[mkcIns]:=SmkcIns;
    MenuKeyCaps[mkcDel]:=SmkcDel;
    MenuKeyCaps[mkcShift]:=SmkcShift;
    MenuKeyCaps[mkcCtrl]:=SmkcCtrl;
    MenuKeyCaps[mkcAlt]:=SmkcAlt;
    MenuKeyCaps[mkcMeta]:=SmkcMeta;
    MenuKeyCapsInited:=true;
  end;
end;

function GetSpecialShortCutName(Key: integer): string;
begin
  Result := '';
  case Key of
    3: Result := 'Cancel'; //generated by Ctrl+Break
    12: Result := 'NumClear'; //generated by Num5 (NumLock off)
    19: Result := 'Break';
    20: Result := 'CapsLock';
    93: Result := 'PopUp'; //PC, near right Ctrl
    106: Result := 'NumMul';
    107: Result := 'NumPlus';
    109: Result := 'NumMinus';
    110: Result := 'NumDot';
    111: Result := 'NumDiv';
    144: Result := 'NumLock';
    145: Result := 'ScrollLock';
    186: Result := ';';
    187: Result := '=';
    188: Result := ',';
    189: Result := '-';
    190: Result := '.';
    191: Result := '/';
    192: Result := '`'; //near tilde
    219: Result := '[';
    220: Result := '\'; //right \
    221: Result := ']';
    222: Result := '''';
    226: Result := '\'; //left \

    //Windows keyboard special:
    $A6: Result := 'BrowserBack';
    $A7: Result := 'BrowserForward';
    $A8: Result := 'BrowserRefresh';
    $A9: Result := 'BrowserStop';
    $AA: Result := 'BrowserSearch';
    $AB: Result := 'BrowserFav';
    $AC: Result := 'BrowserHome';
    $AD: Result := 'VolumeMute';
    $AE: Result := 'VolumeDown';
    $AF: Result := 'VolumeUp';
    $B0: Result := 'MediaNext';
    $B1: Result := 'MediaPrev';
    $B2: Result := 'MediaStop';
    $B3: Result := 'MediaPlay';
    $B4: Result := 'LaunchMail';
    $B5: Result := 'LaunchMedia';
    $B6: Result := 'LaunchApp1';
    $B7: Result := 'LaunchApp2';
  end;
end;


function CompareLineInfoCacheItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointers(PLineInfoCacheItem(Data1)^.Addr,
                          PLineInfoCacheItem(Data2)^.Addr);
end;

function CompareAddrWithLineInfoCacheItem(Addr, Item: Pointer): integer;
begin
  Result:=ComparePointers(Addr,PLineInfoCacheItem(Item)^.Addr);
end;

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
  const DefaultValue: Integer): Integer;
begin
  Result:=GetEnumValue(TypeInfo,Name);
  if Result<0 then
    Result:=DefaultValue;
end;

// Used also by TWidgetSet.GetAcceleratorString
function KeyAndShiftStateToKeyString(Key: word; ShiftState: TShiftState): String;

  procedure AddPart(const APart: string);
  begin
    if Result <> '' then
      Result := Result + '+';
    Result := Result + APart;
  end;

  // Tricky routine. This only works for western languages
  procedure AddKey;
  begin
    case Key of
      VK_UNKNOWN    :AddPart(ifsVK_UNKNOWN);
      VK_LBUTTON    :AddPart(ifsVK_LBUTTON);
      VK_RBUTTON    :AddPart(ifsVK_RBUTTON);
      VK_CANCEL     :AddPart(ifsVK_CANCEL);
      VK_MBUTTON    :AddPart(ifsVK_MBUTTON);
      VK_BACK       :AddPart(ifsVK_BACK);
      VK_TAB        :AddPart(ifsVK_TAB);
      VK_CLEAR      :AddPart(ifsVK_CLEAR);
      VK_RETURN     :AddPart(ifsVK_RETURN);
      VK_SHIFT      :AddPart(ifsVK_SHIFT);
      VK_CONTROL    :AddPart(ifsVK_CONTROL);
      VK_MENU       :AddPart(ifsVK_MENU);
      VK_PAUSE      :AddPart(ifsVK_PAUSE);
      VK_CAPITAL    :AddPart(ifsVK_CAPITAL);
      VK_KANA       :AddPart(ifsVK_KANA);
    //  VK_HANGUL     :AddPart('Hangul');
      VK_JUNJA      :AddPart(ifsVK_JUNJA);
      VK_FINAL      :AddPart(ifsVK_FINAL);
      VK_HANJA      :AddPart(ifsVK_HANJA );
    //  VK_KANJI      :AddPart('Kanji');
      VK_ESCAPE     :AddPart(ifsVK_ESCAPE);
      VK_CONVERT    :AddPart(ifsVK_CONVERT);
      VK_NONCONVERT :AddPart(ifsVK_NONCONVERT);
      VK_ACCEPT     :AddPart(ifsVK_ACCEPT);
      VK_MODECHANGE :AddPart(ifsVK_MODECHANGE);
      VK_SPACE      :AddPart(ifsVK_SPACE);
      VK_PRIOR      :AddPart(ifsVK_PRIOR);
      VK_NEXT       :AddPart(ifsVK_NEXT);
      VK_END        :AddPart(ifsVK_END);
      VK_HOME       :AddPart(ifsVK_HOME);
      VK_LEFT       :AddPart(ifsVK_LEFT);
      VK_UP         :AddPart(ifsVK_UP);
      VK_RIGHT      :AddPart(ifsVK_RIGHT);
      VK_DOWN       :AddPart(ifsVK_DOWN);
      VK_SELECT     :AddPart(ifsVK_SELECT);
      VK_PRINT      :AddPart(ifsVK_PRINT);
      VK_EXECUTE    :AddPart(ifsVK_EXECUTE);
      VK_SNAPSHOT   :AddPart(ifsVK_SNAPSHOT);
      VK_INSERT     :AddPart(ifsVK_INSERT);
      VK_DELETE     :AddPart(ifsVK_DELETE);
      VK_HELP       :AddPart(ifsVK_HELP);
      VK_0..VK_9    :AddPart(chr(ord('0')+Key-VK_0));
      VK_A..VK_Z    :AddPart(chr(ord('A')+Key-VK_A));
      VK_LWIN       :AddPart(ifsVK_LWIN);
      VK_RWIN       :AddPart(ifsVK_RWIN);
      VK_APPS       :AddPart(ifsVK_APPS);
      VK_NUMPAD0..VK_NUMPAD9:  AddPart(Format(ifsVK_NUMPAD,[Key-VK_NUMPAD0]));
      VK_MULTIPLY   :AddPart('*');
      VK_ADD        :AddPart('+');
      VK_OEM_PLUS   :AddPart('+');
      VK_SEPARATOR  :AddPart('|');
      VK_SUBTRACT   :AddPart('-');
      VK_OEM_MINUS  :AddPart('-');
      VK_DECIMAL    :AddPart('.');
      VK_OEM_PERIOD :AddPart('.');
      VK_OEM_COMMA  :AddPart(',');
      VK_DIVIDE     :AddPart('/');
      VK_F1..VK_F24: AddPart('F'+IntToStr(Key-VK_F1+1));
      VK_NUMLOCK    :AddPart(ifsVK_NUMLOCK);
      VK_SCROLL     :AddPart(ifsVK_SCROLL);
      VK_OEM_2      :AddPart('OEM2');
      VK_OEM_3      :AddPart('OEM3');
//    VK_EQUAL      :AddPart('=');
//    VK_AT         :AddPart('@');
    else
      AddPart(UNKNOWN_VK_PREFIX + IntToStr(Key) + UNKNOWN_VK_POSTFIX);
    end;
  end;

begin
  Result := '';
  if ssCtrl in ShiftState then AddPart(ifsCtrl);
  if ssAlt in ShiftState then AddPart(ifsAlt);
  if ssShift in ShiftState then AddPart(ifsVK_SHIFT);
  if ssMeta in ShiftState then
    {$IFDEF LCLcarbon}
    AddPart(ifsVK_CMD);
    {$ELSE}
    AddPart(ifsVK_META);
    {$ENDIF}
  if ssSuper in ShiftState then AddPart(ifsVK_SUPER);
  AddKey;
end;

function KeyStringIsIrregular(const s: string): boolean;
begin
  Result:=(length(UNKNOWN_VK_PREFIX)<length(s)) and
    (AnsiStrLComp(PChar(s),PChar(UNKNOWN_VK_PREFIX),length(UNKNOWN_VK_PREFIX))=0);
end;

function ShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
  Key: Byte;
begin
  InitializeMenuKeyCaps;
  Key := ShortCut and $FF;
  case Key of
    $08: Name := MenuKeyCaps[mkcBkSp]; //made code little nicer
    $09: Name := MenuKeyCaps[mkcTab]; //made code little nicer
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + Key - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + Key - $2D)];
    $30..$39: Name := Chr(Key - $30 + Ord('0'));
    $41..$5A: Name := Chr(Key - $41 + Ord('A'));
    $60..$69: Name := 'Num' + Chr(Key - $60 + Ord('0')); //Delphi differs it from 0..9
    $70..$87: Name := 'F' + IntToStr(Key - $6F);
  else
    Name := GetSpecialShortCutName(Key); //need Key, not shortcut
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scMeta <> 0 then Result := Result + MenuKeyCaps[mkcMeta];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

function TextToShortCut(const ShortCutText: string): TShortCut;

  function CompareFront(var StartPos: integer; const Front: string): Boolean;
  begin
    if (Front<>'') and (StartPos+length(Front)-1<=length(ShortCutText))
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Front), Length(Front))= 0)
    then begin
      Result:=true;
      inc(StartPos,length(Front));
    end else
      Result:=false;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
  StartPos: integer;
  Name: string;
begin
  Result := 0;
  Shift := 0;
  StartPos:=1;
  InitializeMenuKeyCaps;
  while True do
  begin
    if CompareFront(StartPos, MenuKeyCaps[mkcShift]) then
      Shift := Shift or scShift
    else if CompareFront(StartPos, '^') then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcCtrl]) then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcAlt]) then
      Shift := Shift or scAlt
    else if CompareFront(StartPos, MenuKeyCaps[mkcMeta]) then
      Shift := Shift or scMeta
    else
      Break;
  end;
  if ShortCutText = '' then Exit;
  for Key := $08 to $FF do begin { Copy range from table in ShortCutToText }
    Name:=ShortCutToText(Key);
    if (Name<>'') and (length(Name)=length(ShortCutText)-StartPos+1)
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0)
    then begin
      Result := Key or Shift;
      Exit;
    end;
  end;
end;

function GetCompleteText(const sText: string; iSelStart: Integer;
  bCaseSensitive, bSearchAscending: Boolean; slTextList: TStrings): string;

  function IsSamePrefix(const sCompareText, sPrefix: string; iStart: Integer;
    var ResultText: string): Boolean;
  var
    sTempText: string;
  begin
    Result := False;
    sTempText := LazUTF8.UTF8Copy(sCompareText, 1, iStart);
    if not bCaseSensitive then
      sTempText := LazUTF8.UTF8UpperCase(sTempText);
    if (sTempText = sPrefix) then
    begin
      ResultText := sCompareText;
      Result := True;
    end;
  end;

var
  i: Integer;
  sPrefixText: string;
begin
  //DebugLn(['GetCompleteText sText=',sText,' iSelStart=',iSelStart,' bCaseSensitive=',bCaseSensitive,' bSearchAscending=',bSearchAscending,' slTextList.Count=',slTextList.Count]);
  Result := sText;//Default to return original text if no identical text are found
  if (sText = '') then Exit;//Everything is compatible with nothing, Exit.
  if (iSelStart = 0) then Exit;//Cursor at beginning
  if (slTextList.Count = 0) then Exit;//No text list to search for idtenticals, Exit.
  sPrefixText := LazUTF8.UTF8Copy(sText, 1, iSelStart);//Get text from beginning to cursor position.
  if not bCaseSensitive then
    sPrefixText := LazUTF8.UTF8UpperCase(sPrefixText);
  if bSearchAscending then
  begin
    for i := 0 to slTextList.Count - 1 do
      if IsSamePrefix(slTextList[i], sPrefixText, iSelStart, Result) then
        break;
  end else
  begin
    for i := slTextList.Count - 1 downto 0 do
      if IsSamePrefix(slTextList[i], sPrefixText, iSelStart, Result) then
        break;
  end;
end;

function IsEditableTextKey(Key: Word): Boolean;
begin
 Result := (((Key >= VK_A) and (Key <= VK_Z)) or
            ((Key >= VK_NUMPAD0) and (Key <= VK_DIVIDE)) or
            ((Key >= VK_0) and (Key <= VK_9)) or
            ((Key >= 186) and (Key <= 188)) or
            ((Key >= 190) and (Key <= 192)) or
            ((Key >= 219) and (Key <= 222)));
end;

function SendApplicationMessage(Msg: Cardinal; WParam: WParam; LParam: LParam
  ): Longint;
begin
  if SendApplicationMessageFunction<>nil then
    Result:=SendApplicationMessageFunction(Msg, WParam, LParam)
  else
    Result:=0;
end;

procedure OwnerFormDesignerModified(AComponent: TComponent);
begin
  if ([csDesigning,csLoading,csDestroying]*AComponent.ComponentState
    =[csDesigning])
  then begin
    if OwnerFormDesignerModifiedProc<>nil then
      OwnerFormDesignerModifiedProc(AComponent);
  end;
end;

function OffSetRect(var ARect: TRect; dx,dy: Integer): Boolean;
Begin
  with ARect do
  begin
    Left := Left + dx;
    Right := Right + dx;
    Top := Top + dy;
    Bottom := Bottom + dy;
  end;
  Result := (ARect.Left >= 0) and (ARect.Top >= 0);
end;

procedure FreeThenNil(var obj);
begin
  if Pointer(obj) <> nil then 
  begin
    TObject(obj).Free;
    Pointer(obj) := nil;
  end;
end;

procedure RegisterInterfaceInitializationHandler(p: TProcedure);
begin
  InterfaceInitializationHandlers.Add(p);
end;

procedure CallInterfaceInitializationHandlers;
var
  i: Integer;
begin
  for i:=0 to InterfaceInitializationHandlers.Count-1 do
    TProcedure(InterfaceInitializationHandlers[i])();
end;

procedure RegisterInterfaceFinalizationHandler(p: TProcedure);
begin
  InterfaceFinalizationHandlers.Add(p);
end;

procedure CallInterfaceFinalizationHandlers;
var
  i: Integer;
begin
  for i:=InterfaceFinalizationHandlers.Count-1 downto 0 do
    TProcedure(InterfaceFinalizationHandlers[i])();
end;

procedure RaiseAndCatchException;
begin
  try
    if (length(rsERRORInLCL) div (length(rsERRORInLCL) div 10000))=0 then ;
  except
  end;
end;

function GetStackTrace(UseCache: boolean): string;
var
  bp: Pointer;
  addr: Pointer;
  oldbp: Pointer;
  CurAddress: Shortstring;
begin
  Result:='';
  { retrieve backtrace info }
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    addr:=get_caller_addr(bp);
    CurAddress:=GetLineInfo(addr,UseCache);
    //DebugLn('GetStackTrace ',CurAddress);
    Result:=Result+CurAddress+LineEnding;
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
end;

procedure GetStackTracePointers(var AStack: TStackTracePointers);
var
  Depth: Integer;
  bp: Pointer;
  oldbp: Pointer;
begin
  // get stack depth
  Depth:=0;
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    inc(Depth);
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
  SetLength(AStack,Depth);
  if Depth>0 then begin
    Depth:=0;
    bp:=get_caller_frame(get_frame);
    while bp<>nil do begin
      AStack[Depth]:=get_caller_addr(bp);
      inc(Depth);
      oldbp:=bp;
      bp:=get_caller_frame(bp);
      if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
        bp:=nil;
    end;
  end;
end;

function StackTraceAsString(const AStack: TStackTracePointers;
  UseCache: boolean): string;
var
  i: Integer;
  CurAddress: String;
begin
  Result:='';
  for i:=0 to length(AStack)-1 do begin
    CurAddress:=GetLineInfo(AStack[i],UseCache);
    Result:=Result+CurAddress+LineEnding;
  end;
end;

function GetLineInfo(Addr: Pointer; UseCache: boolean): string;
var
  ANode: TAvgLvlTreeNode;
  Item: PLineInfoCacheItem;
begin
  if UseCache then begin
    if LineInfoCache=nil then
      LineInfoCache:=TAvgLvlTree.Create(@CompareLineInfoCacheItems);
    ANode:=LineInfoCache.FindKey(Addr,@CompareAddrWithLineInfoCacheItem);
    if ANode=nil then begin
      Result:=BackTraceStrFunc(Addr);
      New(Item);
      Item^.Addr:=Addr;
      Item^.Info:=Result;
      LineInfoCache.Add(Item);
    end else begin
      Result:=PLineInfoCacheItem(ANode.Data)^.Info;
    end;
  end else
    Result:=BackTraceStrFunc(Addr);
end;

procedure MoveRect(var ARect: TRect; x, y: Integer);
begin
  inc(ARect.Right,x-ARect.Left);
  inc(ARect.Bottom,y-ARect.Top);
  ARect.Left:=x;
  ARect.Top:=y;
end;

procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
// move ARect, so it fits into MaxRect
// if MaxRect is too small, ARect is resized.
begin
  if ARect.Left<MaxRect.Left then begin
    // move rectangle right
    ARect.Right:=Min(ARect.Right+MaxRect.Left-ARect.Left,MaxRect.Right);
    ARect.Left:=MaxRect.Left;
  end;
  if ARect.Top<MaxRect.Top then begin
    // move rectangle down
    ARect.Bottom:=Min(ARect.Bottom+MaxRect.Top-ARect.Top,MaxRect.Bottom);
    ARect.Top:=MaxRect.Top;
  end;
  if ARect.Right>MaxRect.Right then begin
    // move rectangle left
    ARect.Left:=Max(ARect.Left-ARect.Right+MaxRect.Right,MaxRect.Left);
    ARect.Right:=MaxRect.Right;
  end;
  if ARect.Bottom>MaxRect.Bottom then begin
    // move rectangle left
    ARect.Top:=Max(ARect.Top-ARect.Bottom+MaxRect.Bottom,MaxRect.Top);
    ARect.Bottom:=MaxRect.Bottom;
  end;
end;

procedure MakeMinMax(var i1, i2: integer);
var
  h: Integer;
begin
  if i1>i2 then begin
    h:=i1;
    i1:=i2;
    i2:=h;
  end;
end;

procedure CalculateLeftTopWidthHeight(X1, Y1, X2, Y2: integer;
  out Left, Top, Width, Height: integer);
begin
  if X1 <= X2 then 
   begin
    Left := X1;
    Width := X2 - X1;
  end 
  else 
  begin
    Left := X2;
    Width := X1 - X2;
  end;
  if Y1 <= Y2 then 
  begin
    Top := Y1;
    Height := Y2 - Y1;
  end 
  else 
  begin
    Top := Y2;
    Height := Y1 - Y2;
  end;
end;

function BreakString(const s: string; MaxLineLength, Indent: integer): string;
var
  SrcLen: Integer;
  APos: Integer;
  Src: String;
  SplitPos: Integer;
  CurMaxLineLength: Integer;
begin
  Result:='';
  Src:=s;
  CurMaxLineLength:=MaxLineLength;
  if Indent>MaxLineLength-2 then Indent:=MaxLineLength-2;
  if Indent<0 then MaxLineLength:=0;
  repeat
    SrcLen:=length(Src);
    if SrcLen<=CurMaxLineLength then begin
      Result:=Result+Src;
      break;
    end;
    // split line
    SplitPos:=0;
    // search new line chars
    APos:=1;
    while (APos<=CurMaxLineLength) do begin
      if Src[APos] in [#13,#10] then begin
        SplitPos:=APos;
        break;
      end;
      inc(APos);
    end;
    // search a space boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos-1] in [' ',#9])
        and (not (Src[APos] in [' ',#9])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    // search a word boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos] in ['A'..'Z','a'..'z'])
        and (not (Src[APos-1] in ['A'..'Z','a'..'z'])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    if SplitPos=0 then begin
      // no word boundary found -> split chars
      SplitPos:=CurMaxLineLength;
    end;
    // append part and newline
    if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13]) then begin
      // there is already a new line char at position
      inc(SplitPos);
      if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13])
      and (Src[SplitPos]<>Src[SplitPos-1]) then
        inc(SplitPos);
      Result:=Result+copy(Src,1,SplitPos-1);
    end else begin
      Result:=Result+copy(Src,1,SplitPos-1)+LineEnding;
    end;
    // append indent
    if Indent>0 then
      Result:=Result+StringOfChar(' ',Indent);
    // calculate new LineLength
    CurMaxLineLength:=MaxLineLength-Indent;
    // cut string
    Src:=copy(Src,SplitPos,length(Src)-SplitPos+1);
  until false;
end;

function ComparePointers(p1, p2: Pointer): integer;
begin
  if p1>p2 then
    Result:=1
  else if p1<p2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareHandles(h1, h2: THandle): integer;
begin
  if h1>h2 then
    Result:=1
  else if h1<h2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareLCLHandles(h1, h2: TLCLHandle): integer;
begin
  if h1>h2 then
    Result:=1
  else if h1<h2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareRect(R1, R2: PRect): Boolean;
begin
  Result:=(R1^.Left=R2^.Left) and (R1^.Top=R2^.Top) and
          (R1^.Bottom=R2^.Bottom) and (R1^.Right=R2^.Right);
  {if not Result then begin
    DebugLn(' DIFFER: ',R1^.Left,',',R1^.Top,',',R1^.Right,',',R1^.Bottom
      ,' <> ',R2^.Left,',',R2^.Top,',',R2^.Right,',',R2^.Bottom);
  end;}
end;

function ComparePoints(const p1, p2: TPoint): integer;
begin
  if p1.Y>p2.Y then
    Result:=1
  else if p1.Y<p2.Y then
    Result:=-1
  else if p1.X>p2.X then
    Result:=1
  else if p1.X<p2.X then
    Result:=-1
  else
    Result:=0;
end;

function CompareMethods(const m1, m2: TMethod): boolean;
begin
  Result:=(m1.Code=m2.Code) and (m1.Data=m2.Data);
end;

function RoundToInt(const e: Extended): integer;
begin
  Result:=integer(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToInt ',e,' ',Result);
  {$ENDIF}
end;

function RoundToCardinal(const e: Extended): cardinal;
begin
  Result:=cardinal(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function TruncToInt(const e: Extended): integer;
begin
  Result:=integer(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToInt ',e,' ',Result);
  {$ENDIF}
end;

function TruncToCardinal(const e: Extended): cardinal;
begin
  Result:=cardinal(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function StrToDouble(const s: string): double;
begin
  {$IFDEF VerboseRound}
  DebugLn('StrToDouble "',s,'"');
  {$ENDIF}
  Result:=Double(StrToFloat(s));
end;

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);
begin
  if List=nil then exit;
  MergeSort(List,0,List.Count-1,OnCompare);
end;

procedure MergeSort(List: TFPList; StartIndex, EndIndex: integer;
  const OnCompare: TListSortCompare);
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PPointer;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: Pointer;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:PtrInt;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  Cnt: Integer;
begin
  if (List=nil) then exit;
  Cnt:=List.Count;
  if StartIndex<0 then StartIndex:=0;
  if EndIndex>=Cnt then EndIndex:=Cnt-1;
  if StartIndex>=EndIndex then exit;
  MergeList:=GetMem(List.Count*SizeOf(Pointer));
  Sort(StartIndex,EndIndex);
  Freemem(MergeList);
end;

procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare);
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PAnsiString;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: string;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:integer;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  CurSize: PtrInt;
  i: PtrInt;
begin
  if (List=nil) or (List.Count<=1) then exit;
  CurSize:=PtrInt(List.Count)*SizeOf(Pointer);
  MergeList:=GetMem(CurSize);
  FillChar(MergeList^,CurSize,0);
  Sort(0,List.Count-1);
  for i:=0 to List.Count-1 do MergeList[i]:='';
  Freemem(MergeList);
end;


// Debug funcs :

{$IFnDEF WithOldDebugln}

{$ELSE}

procedure InitializeDebugOutput;
var
  DebugFileName: string;

  function GetDebugFileName: string;
  const
    DebugLogStart = '--debug-log=';
    DebugLogStartLength = length(DebugLogStart);
  var
    i: integer;
    EnvVarName: string;
  begin
    Result := '';
    // first try to find the log file name in the command line parameters
    for i:= 1 to Paramcount do begin
      if copy(ParamStrUTF8(i),1, DebugLogStartLength)=DebugLogStart then begin
        Result := copy(ParamStrUTF8(i), DebugLogStartLength+1,
                   Length(ParamStrUTF8(i))-DebugLogStartLength);
      end;
    end;
    // if not found yet, then try to find in the environment variables
    if (length(result)=0) then begin
      EnvVarName:= ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'') + '_debuglog';
      Result := GetEnvironmentVariableUTF8(EnvVarName);
    end;
    if (length(result)>0) then
      Result := ExpandFileNameUTF8(Result);
  end;

var
  fm: Byte;
begin
  DebugText := nil;
  DebugFileName := GetDebugFileName;
  if (length(DebugFileName)>0) and
    (DirPathExists(ExtractFileDir(DebugFileName))) then
  begin
    fm:=Filemode;
    new(DebugText);
    try
      Filemode:=fmShareDenyNone;
      Assign(DebugText^, DebugFileName);
      if FileExistsUTF8(DebugFileName) then
        Append(DebugText^)
      else
        Rewrite(DebugText^);
    except
      Freemem(DebugText);
      DebugText := nil;
      // Add extra line ending: a dialog will be shown in windows gui application
      writeln(StdOut, 'Cannot open file: ', DebugFileName+LineEnding);
    end;
    Filemode:=fm;
  end;
  if DebugText=nil then
  begin
    if TextRec(Output).Mode=fmClosed then
      DebugText := nil
    else
      DebugText := @Output;
    DebugTextAllocated := false;
  end else
    DebugTextAllocated := true;
end;

procedure CloseDebugOutput;
begin
  if DebugTextAllocated then begin
    Close(DebugText^);
    Dispose(DebugText);
    DebugTextAllocated := false;
  end;
  DebugText := nil;
end;

procedure FinalizeDebugOutput;
begin
  CloseDebugOutput;
end;

procedure DebugLnNestCreatePrefix;
const
  CurrentLen: Integer = 0;
var
  s: String;
  NewLen: Integer;
begin
  NewLen := DebugNestLvl * DebugLnNestLvlIndent;
  if NewLen < 0 then NewLen := 0;
  if (NewLen >= DebugLnMaxNestPrefixLen) then begin
    NewLen := DebugLnMaxNestPrefixLen;
    s := IntToStr(DebugNestLvl);
    if length(s)+1 > NewLen then
      NewLen := length(s)+1;
  end else
    s := '';

  if NewLen > CurrentLen then
    ReAllocMem(DebugNestPrefix, NewLen+21);
  CurrentLen := NewLen+20;

  FillChar(DebugNestPrefix^, NewLen, ' ');
  if s <> '' then
    System.Move(s[1], DebugNestPrefix[0], length(s));

  if (NewLen >= DebugLnMaxNestPrefixLen) then
    DebugNestPrefix[DebugLnMaxNestPrefixLen] := #0
  else
    DebugNestPrefix[NewLen] := #0;
end;

procedure DebugLnNestFreePrefix;
begin
  if DebugNestPrefix <> nil then
    ReAllocMem(DebugNestPrefix, 0);
end;

procedure DumpStack;
begin
  if Assigned(DebugText) then
    Dump_Stack(DebugText^, get_frame);
end;

procedure DebugLn(Args: array of const);
var
  i: Integer;
begin
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
    vtInteger: DbgOut(dbgs(Args[i].vinteger));
    vtInt64: DbgOut(dbgs(Args[i].VInt64^));
    vtQWord: DbgOut(dbgs(Args[i].VQWord^));
    vtBoolean: DbgOut(dbgs(Args[i].vboolean));
    vtExtended: DbgOut(dbgs(Args[i].VExtended^));
{$ifdef FPC_CURRENCY_IS_INT64}
    // MWE:
    // fpc 2.x has troubles in choosing the right dbgs()
    // so we convert here
    vtCurrency: DbgOut(dbgs(int64(Args[i].vCurrency^)/10000, 4));
{$else}
    vtCurrency: DbgOut(dbgs(Args[i].vCurrency^));
{$endif}
    vtString: DbgOut(Args[i].VString^);
    vtAnsiString: DbgOut(AnsiString(Args[i].VAnsiString));
    vtChar: DbgOut(Args[i].VChar);
    vtPChar: DbgOut(Args[i].VPChar);
    vtPWideChar: DbgOut(Args[i].VPWideChar);
    vtWideChar: DbgOut(AnsiString(Args[i].VWideChar));
    vtWidestring: DbgOut(AnsiString(WideString(Args[i].VWideString)));
    vtUnicodeString: DbgOut(AnsiString(UnicodeString(Args[i].VUnicodeString)));
    vtObject: DbgOut(DbgSName(Args[i].VObject));
    vtClass: DbgOut(DbgSName(Args[i].VClass));
    vtPointer: DbgOut(Dbgs(Args[i].VPointer));
    else
      DbgOut('?unknown variant?');
    end;
  end;
  DebugLn;
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  DebugLn(Format(S, Args));
end;

procedure DebugLn;
begin
  DebugLn('');
end;

procedure DebugLn(const s: string);
begin
  {$ifdef WinCE}
  if DebugNestAtBOL and (s <> '') then
    DbgAppendToFile(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, DebugNestPrefix+s)
  else
    DbgAppendToFile(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, s);
  {$else}
  // First of all verify if a widgetset has override DebugLn
  if DebugLnProc <> nil then
  begin
    DebugLnProc(s);
    Exit;
  end;

  // Now the default code
  if not Assigned(DebugText) then exit;
  if DebugNestAtBOL and (s <> '') then
    write(DebugText^, DebugNestPrefix);
  writeln(DebugText^, ConvertLineEndings(s));
  {$endif}
  DebugNestAtBOL := True;
end;

procedure DebugLn(const s1, s2: string);
begin
  DebugLn(s1+s2);
end;

procedure DebugLn(const s1, s2, s3: string);
begin
  DebugLn(s1+s2+s3);
end;

procedure DebugLn(const s1, s2, s3, s4: string);
begin
  DebugLn(s1+s2+s3+s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: string);
begin
  DebugLn(s1+s2+s3+s4+s5);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
  s12: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
  s13: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14, s15: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14, s15, s16: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16);
end;

procedure DebugLnEnter(const s: string);
begin
  if not DebugNestAtBOL then
    DebugLn;
  if s <> '' then
    DebugLn(s);
  inc(DebugNestLvl);
  DebugLnNestCreatePrefix;
end;

procedure DebugLnEnter(Args: array of const);
begin
  if not DebugNestAtBOL then
    DebugLn;
  DebugLn(Args);
  inc(DebugNestLvl);
  DebugLnNestCreatePrefix;
end;

procedure DebugLnEnter(s: string; Args: array of const);
begin
  DebugLnEnter(Format(s, Args));
end;

procedure DebugLnEnter(const s1: string; const s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string;
  const s12: string; const s13: string; const s14: string; const s15: string;
  const s16: string; const s17: string; const s18: string);
begin
  DebugLnEnter(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure DebugLnExit(const s: string);
begin
  dec(DebugNestLvl);
  if DebugNestLvl < 0 then DebugNestLvl := 0;
  DebugLnNestCreatePrefix;
  if not DebugNestAtBOL then
    DebugLn;
  if s <> '' then
    DebugLn(s);
end;

procedure DebugLnExit(Args: array of const);
begin
  dec(DebugNestLvl);
  if DebugNestLvl < 0 then DebugNestLvl := 0;
  DebugLnNestCreatePrefix;
  if not DebugNestAtBOL then
    DebugLn;
  DebugLn(Args);
end;

procedure DebugLnExit(s: string; Args: array of const);
begin
  DebugLnExit(Format(s, Args));
end;

procedure DebugLnExit(const s1: string; const s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string;
  const s12: string; const s13: string; const s14: string; const s15: string;
  const s16: string; const s17: string; const s18: string);
begin
  DebugLnExit(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure DbgOut(const S: String; Args: array of const);
begin
  DbgOut(Format(S, Args));
end;

procedure DBGOut(const s: string);
begin
  {$ifdef WinCE}
  if DebugNestAtBOL and (s <> '') then
    DbgAppendToFileWithoutLn(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, DebugNestPrefix);
  DbgAppendToFileWithoutLn(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, s);
  {$else}
  if DebugOutProc <> nil then
  begin
    DebugOutProc(s);
    Exit;
  end;

  if Assigned(DebugText) then begin
    if DebugNestAtBOL and (s <> '') then
      write(DebugText^, DebugNestPrefix);
    write(DebugText^, s);
  end;
  {$endif}
  DebugNestAtBOL := (s = '') or (s[length(s)] in [#10,#13]);
end;

procedure DBGOut(const s1, s2: string);
begin
  DbgOut(s1+s2);
end;

procedure DbgOut(const s1, s2, s3: string);
begin
  DbgOut(s1+s2+s3);
end;

procedure DbgOut(const s1, s2, s3, s4: string);
begin
  DbgOut(s1+s2+s3+s4);
end;

procedure DbgOut(const s1, s2, s3, s4, s5: string);
begin
  DbgOut(s1+s2+s3+s4+s5);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;
{$ENDIF}


function DbgSWindowPosFlags(Flags: UInt): String;
begin
  Result := '';
  if (SWP_NOSIZE and Flags) <> 0 then
    Result := Result + 'SWP_NOSIZE, ';
  if (SWP_NOMOVE and Flags) <> 0 then
    Result := Result + 'SWP_NOMOVE, ';
  if (SWP_NOZORDER and Flags) <> 0 then
    Result := Result + 'SWP_NOZORDER, ';
  if (SWP_NOREDRAW and Flags) <> 0 then
    Result := Result + 'SWP_NOREDRAW, ';
  if (SWP_NOACTIVATE and Flags) <> 0 then
    Result := Result + 'SWP_NOACTIVATE, ';
  if (SWP_DRAWFRAME and Flags) <> 0 then
    Result := Result + 'SWP_DRAWFRAME, ';
  if (SWP_SHOWWINDOW and Flags) <> 0 then
    Result := Result + 'SWP_SHOWWINDOW, ';
  if (SWP_HIDEWINDOW and Flags) <> 0 then
    Result := Result + 'SWP_HIDEWINDOW, ';
  if (SWP_NOCOPYBITS and Flags) <> 0 then
    Result := Result + 'SWP_NOCOPYBITS, ';
  if (SWP_NOOWNERZORDER and Flags) <> 0 then
    Result := Result + 'SWP_NOOWNERZORDER, ';
  if (SWP_NOSENDCHANGING and Flags) <> 0 then
    Result := Result + 'SWP_NOSENDCHANGING, ';
  if (SWP_DEFERERASE and Flags) <> 0 then
    Result := Result + 'SWP_DEFERERASE, ';
  if (SWP_ASYNCWINDOWPOS and Flags) <> 0 then
    Result := Result + 'SWP_ASYNCWINDOWPOS, ';
  if (SWP_STATECHANGED and Flags) <> 0 then
    Result := Result + 'SWP_STATECHANGED, ';
  if (SWP_SourceIsInterface and Flags) <> 0 then
    Result := Result + 'SWP_SourceIsInterface, ';
  if Result <> '' then
    Delete(Result, Length(Result) - 1, 2);
end;

function DbgsVKCode(c: word): string;
begin
  case c of
  VK_UNKNOWN: Result:='VK_UNKNOWN';
  VK_LBUTTON: Result:='VK_LBUTTON';
  VK_RBUTTON: Result:='VK_RBUTTON';
  VK_CANCEL: Result:='VK_CANCEL';
  VK_MBUTTON: Result:='VK_MBUTTON';
  VK_BACK: Result:='VK_BACK';
  VK_TAB: Result:='VK_TAB';
  VK_CLEAR: Result:='VK_CLEAR';
  VK_RETURN: Result:='VK_RETURN';
  VK_SHIFT: Result:='VK_SHIFT';
  VK_CONTROL: Result:='VK_CONTROL';
  VK_MENU: Result:='VK_MENU';
  VK_PAUSE: Result:='VK_PAUSE';
  VK_CAPITAL: Result:='VK_CAPITAL';
  VK_KANA: Result:='VK_KANA';
  VK_JUNJA: Result:='VK_JUNJA';
  VK_FINAL: Result:='VK_FINAL';
  VK_HANJA: Result:='VK_HANJA';
  VK_ESCAPE: Result:='VK_ESCAPE';
  VK_CONVERT: Result:='VK_CONVERT';
  VK_NONCONVERT: Result:='VK_NONCONVERT';
  VK_ACCEPT: Result:='VK_ACCEPT';
  VK_MODECHANGE: Result:='VK_MODECHANGE';
  VK_SPACE: Result:='VK_SPACE';
  VK_PRIOR: Result:='VK_PRIOR';
  VK_NEXT: Result:='VK_NEXT';
  VK_END: Result:='VK_END';
  VK_HOME: Result:='VK_HOME';
  VK_LEFT: Result:='VK_LEFT';
  VK_UP: Result:='VK_UP';
  VK_RIGHT: Result:='VK_RIGHT';
  VK_DOWN: Result:='VK_DOWN';
  VK_SELECT: Result:='VK_SELECT';
  VK_PRINT: Result:='VK_PRINT';
  VK_EXECUTE: Result:='VK_EXECUTE';
  VK_SNAPSHOT: Result:='VK_SNAPSHOT';
  VK_INSERT: Result:='VK_INSERT';
  VK_DELETE: Result:='VK_DELETE';
  VK_HELP: Result:='VK_HELP';

  VK_0: Result:='VK_0';
  VK_1: Result:='VK_1';
  VK_2: Result:='VK_2';
  VK_3: Result:='VK_3';
  VK_4: Result:='VK_4';
  VK_5: Result:='VK_5';
  VK_6: Result:='VK_6';
  VK_7: Result:='VK_7';
  VK_8: Result:='VK_8';
  VK_9: Result:='VK_9';

  VK_A: Result:='VK_A';
  VK_B: Result:='VK_B';
  VK_C: Result:='VK_C';
  VK_D: Result:='VK_D';
  VK_E: Result:='VK_E';
  VK_F: Result:='VK_F';
  VK_G: Result:='VK_G';
  VK_H: Result:='VK_H';
  VK_I: Result:='VK_I';
  VK_J: Result:='VK_J';
  VK_K: Result:='VK_K';
  VK_L: Result:='VK_L';
  VK_M: Result:='VK_M';
  VK_N: Result:='VK_N';
  VK_O: Result:='VK_O';
  VK_P: Result:='VK_P';
  VK_Q: Result:='VK_Q';
  VK_R: Result:='VK_R';
  VK_S: Result:='VK_S';
  VK_T: Result:='VK_T';
  VK_U: Result:='VK_U';
  VK_V: Result:='VK_V';
  VK_W: Result:='VK_W';
  VK_X: Result:='VK_X';
  VK_Y: Result:='VK_Y';
  VK_Z: Result:='VK_Z';

  VK_LWIN: Result:='VK_LWIN';
  VK_RWIN: Result:='VK_RWIN';
  VK_APPS: Result:='VK_APPS';
  VK_SLEEP: Result:='VK_SLEEP';

  VK_NUMPAD0: Result:='VK_NUMPAD0';
  VK_NUMPAD1: Result:='VK_NUMPAD1';
  VK_NUMPAD2: Result:='VK_NUMPAD2';
  VK_NUMPAD3: Result:='VK_NUMPAD3';
  VK_NUMPAD4: Result:='VK_NUMPAD4';
  VK_NUMPAD5: Result:='VK_NUMPAD5';
  VK_NUMPAD6: Result:='VK_NUMPAD6';
  VK_NUMPAD7: Result:='VK_NUMPAD7';
  VK_NUMPAD8: Result:='VK_NUMPAD8';
  VK_NUMPAD9: Result:='VK_NUMPAD9';
  VK_MULTIPLY: Result:='VK_MULTIPLY';
  VK_ADD: Result:='VK_ADD';
  VK_SEPARATOR: Result:='VK_SEPARATOR';
  VK_SUBTRACT: Result:='VK_SUBTRACT';
  VK_DECIMAL: Result:='VK_DECIMAL';
  VK_DIVIDE: Result:='VK_DIVIDE';
  VK_F1: Result:='VK_F1';
  VK_F2: Result:='VK_F2';
  VK_F3: Result:='VK_F3';
  VK_F4: Result:='VK_F4';
  VK_F5: Result:='VK_F5';
  VK_F6: Result:='VK_F6';
  VK_F7: Result:='VK_F7';
  VK_F8: Result:='VK_F8';
  VK_F9: Result:='VK_F9';
  VK_F10: Result:='VK_F10';
  VK_F11: Result:='VK_F11';
  VK_F12: Result:='VK_F12';
  VK_F13: Result:='VK_F13';
  VK_F14: Result:='VK_F14';
  VK_F15: Result:='VK_F15';
  VK_F16: Result:='VK_F16';
  VK_F17: Result:='VK_F17';
  VK_F18: Result:='VK_F18';
  VK_F19: Result:='VK_F19';
  VK_F20: Result:='VK_F20';
  VK_F21: Result:='VK_F21';
  VK_F22: Result:='VK_F22';
  VK_F23: Result:='VK_F23';
  VK_F24: Result:='VK_F24';

  VK_NUMLOCK: Result:='VK_NUMLOCK';
  VK_SCROLL: Result:='VK_SCROLL';

  VK_LSHIFT: Result:='VK_LSHIFT';
  VK_RSHIFT: Result:='VK_RSHIFT';
  VK_LCONTROL: Result:='VK_LCONTROL';
  VK_RCONTROL: Result:='VK_RCONTROL';
  VK_LMENU: Result:='VK_LMENU';
  VK_RMENU: Result:='VK_RMENU';

  VK_BROWSER_BACK: Result:='VK_BROWSER_BACK';
  VK_BROWSER_FORWARD: Result:='VK_BROWSER_FORWARD';
  VK_BROWSER_REFRESH: Result:='VK_BROWSER_REFRESH';
  VK_BROWSER_STOP: Result:='VK_BROWSER_STOP';
  VK_BROWSER_SEARCH: Result:='VK_BROWSER_SEARCH';
  VK_BROWSER_FAVORITES: Result:='VK_BROWSER_FAVORITES';
  VK_BROWSER_HOME: Result:='VK_BROWSER_HOME';
  VK_VOLUME_MUTE: Result:='VK_VOLUME_MUTE';
  VK_VOLUME_DOWN: Result:='VK_VOLUME_DOWN';
  VK_VOLUME_UP: Result:='VK_VOLUME_UP';
  VK_MEDIA_NEXT_TRACK: Result:='VK_MEDIA_NEXT_TRACK';
  VK_MEDIA_PREV_TRACK: Result:='VK_MEDIA_PREV_TRACK';
  VK_MEDIA_STOP: Result:='VK_MEDIA_STOP';
  VK_MEDIA_PLAY_PAUSE: Result:='VK_MEDIA_PLAY_PAUSE';
  VK_LAUNCH_MAIL: Result:='VK_LAUNCH_MAIL';
  VK_LAUNCH_MEDIA_SELECT: Result:='VK_LAUNCH_MEDIA_SELECT';
  VK_LAUNCH_APP1: Result:='VK_LAUNCH_APP1';
  VK_LAUNCH_APP2: Result:='VK_LAUNCH_APP2';
  // New keys in 0.9.31+
  VK_LCL_EQUAL: Result:='VK_LCL_EQUAL';
  VK_LCL_COMMA: Result:='VK_LCL_COMMA';
  VK_LCL_POINT: Result:='VK_LCL_POINT';
  VK_LCL_SLASH: Result:='VK_LCL_SLASH';
  VK_LCL_SEMI_COMMA:Result:='VK_LCL_SEMI_COMMA';
  VK_LCL_MINUS     :Result:='VK_LCL_MINUS';
  VK_LCL_OPEN_BRAKET:Result:='VK_LCL_OPEN_BRAKET';
  VK_LCL_CLOSE_BRAKET:Result:='VK_LCL_CLOSE_BRAKET';
  VK_LCL_BACKSLASH :Result:='VK_LCL_BACKSLASH';
  VK_LCL_TILDE     :Result:='VK_LCL_TILDE';
  VK_LCL_QUOTE     :Result:='VK_LCL_QUOTE';
  //
  VK_LCL_POWER: Result:='VK_LCL_POWER';
  VK_LCL_CALL: Result:='VK_LCL_CALL';
  VK_LCL_ENDCALL: Result:='VK_LCL_ENDCALL';
  VK_LCL_AT: Result:='VK_LCL_AT';
  else
    Result:='VK_('+dbgs(c)+')';
  end;
end;

function DbgS(const ATM: TTextMetric): string;
begin
  with ATM do
    Result :=
      'tmHeight: ' + DbgS(tmHeight) +
      ' tmAscent: ' + DbgS(tmAscent) +
      ' tmDescent: ' + DbgS(tmDescent) +
      ' tmInternalLeading: ' + DbgS(tmInternalLeading) +
      ' tmExternalLeading: ' + DbgS(tmExternalLeading) +
      ' tmAveCharWidth: ' + DbgS(tmAveCharWidth) +
      ' tmMaxCharWidth: ' + DbgS(tmMaxCharWidth) +
      ' tmWeight: ' + DbgS(tmWeight) +
      ' tmOverhang: ' + DbgS(tmOverhang) +
      ' tmDigitizedAspectX: ' + DbgS(tmDigitizedAspectX) +
      ' tmDigitizedAspectY: ' + DbgS(tmDigitizedAspectY) +
      ' tmFirstChar: ' + tmFirstChar +
      ' tmLastChar: ' + tmLastChar +
      ' tmDefaultChar: ' + tmDefaultChar +
      ' tmBreakChar: ' + tmBreakChar +
      ' tmItalic: ' + DbgS(tmItalic) +
      ' tmUnderlined: ' + DbgS(tmUnderlined) +
      ' tmStruckOut: ' + DbgS(tmStruckOut) +
      ' tmPitchAndFamily: ' + DbgS(tmPitchAndFamily) +
      ' tmCharSet: ' + DbgS(tmCharSet);
end;

function DbgS(const AScrollInfo: TScrollInfo): string;
begin
  Result := '';

  if (SIF_POS and AScrollInfo.fMask) > 0 then
    Result := 'Pos: ' + DbgS(AScrollInfo.nPos);
  if (SIF_RANGE and AScrollInfo.fMask) > 0 then
    Result := Result + ' Min: ' + DbgS(AScrollInfo.nMin) + ' Max: ' +
      DbgS(AScrollInfo.nMax);
  if (SIF_PAGE and AScrollInfo.fMask) > 0 then
    Result := Result + ' Page: ' + DbgS(AScrollInfo.nPage);
  if (SIF_TRACKPOS and AScrollInfo.fMask) > 0 then
    Result := Result + ' TrackPos: ' + DbgS(AScrollInfo.nTrackPos);

  if Result = '' then Result := '(no scrollinfo)';
end;

function DbgS(const AVariant: Variant): string;
begin
  if TVarData(AVariant).VType = varEmpty then
    result := '<empty>'
  else
  if TVarData(AVariant).vtype = varNull then
    result := '<null>'
  else
    result := AVariant;
end;

procedure DbgOutThreadLog(const Msg: string);
var
  PID: PtrInt;
  fs: TFileStreamUTF8;
  Filename: string;
begin
  PID:=PtrInt(GetThreadID);
  Filename:='Log'+IntToStr(PID);
  if FileExistsUTF8(Filename) then
    fs:=TFileStreamUTF8.Create(Filename,fmOpenWrite or fmShareDenyNone)
  else
    fs:=TFileStreamUTF8.Create(Filename,fmCreate);
  fs.Position:=fs.Size;
  fs.Write(Msg[1], length(Msg));
  fs.Free;
end;

procedure DebuglnThreadLog(const Msg: string);
var
  PID: PtrInt;
begin
  PID:=PtrInt(GetThreadID);
  DbgOutThreadLog(IntToStr(PtrInt(PID))+' : '+Msg+LineEnding);
end;

procedure DebuglnThreadLog;
begin
  DebuglnThreadLog('');
end;

procedure DbgSaveData(FileName: String; AData: PChar; ADataSize: PtrUInt);
var
  S: TStream;
begin
  S := TFileStreamUTF8.Create(FileName, fmCreate);
  S.Write(AData^, ADataSize);
  S.Free;
end;

procedure DbgAppendToFile(FileName, S: String);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  {$I-}
  Append(F);
  if IOResult <> 0 then
    Rewrite(F);
  {$I+}
  WriteLn(F, S);
  CloseFile(F);
end;

procedure DbgAppendToFileWithoutLn(FileName, S: String);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  {$I-}
  Append(F);
  if IOResult <> 0 then
    Rewrite(F);
  {$I+}
  Write(F, S);
  CloseFile(F);
end;

function StripLN(const ALine: String): String;
var
  idx: Integer;
begin
  Result := ALine;
  idx := Pos(#10, Result);
  if idx = 0
  then begin
    idx := Pos(#13, Result);
    if idx = 0 then Exit;
  end
  else begin
    if (idx > 1)
    and (Result[idx - 1] = #13)
    then Dec(idx);
  end;
  SetLength(Result, idx - 1);
end;

function GetPart(const ASkipTo, AnEnd: String; var ASource: String;
  const AnIgnoreCase, AnUpdateSource: Boolean): String;
begin
  Result := GetPart([ASkipTo], [AnEnd], ASource, AnIgnoreCase, AnUpdateSource);
end;

function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String;
var
  n, i, idx: Integer;
  S, Source, Match: String;
  HasEscape: Boolean;
begin
  Source := ASource;

  if High(ASkipTo) >= 0
  then begin
    idx := 0;
    Match := '';
    HasEscape := False;
    if AnIgnoreCase
    then S := UpperCase(Source)
    else S := Source;
    for n := Low(ASkipTo) to High(ASkipTo) do
    begin
      if ASkipTo[n] = ''
      then begin
        HasEscape := True;
        Continue;
      end;
      if AnIgnoreCase
      then i := Pos(UpperCase(ASkipTo[n]), S)
      else i := Pos(ASkipTo[n], S);
      if i > idx
      then begin
        idx := i;
        Match := ASkipTo[n];
      end;
    end;
    if (idx = 0) and not HasEscape
    then begin
      Result := '';
      Exit;
    end;
    if idx > 0
    then Delete(Source, 1, idx + Length(Match) - 1);
  end;

  if AnIgnoreCase
  then S := UpperCase(Source)
  else S := Source;
  idx := MaxInt;
  for n := Low(AnEnd) to High(AnEnd) do
  begin
    if AnEnd[n] = '' then Continue;
    if AnIgnoreCase
    then i := Pos(UpperCase(AnEnd[n]), S)
    else i := Pos(AnEnd[n], S);
    if (i > 0) and (i < idx) then idx := i;
  end;

  if idx = MaxInt
  then begin
    Result := Source;
    Source := '';
  end
  else begin
    Result := Copy(Source, 1, idx - 1);
    Delete(Source, 1, idx - 1);
  end;

  if AnUpdateSource
  then ASource := Source;
end;

{
  Ensures the covenient look of multiline string
  when displaying it in the single line
  * Replaces CR and LF with spaces
  * Removes duplicate spaces
}
function TextToSingleLine(const AText: string): string;
var
  str: string;
  i, wstart, wlen: Integer;
begin
  str := Trim(AText);
  wstart := 0;
  wlen := 0;
  i := 1;
  while i < Length(str) - 1 do
  begin
    if (str[i] in [' ', #13, #10]) then
    begin
      if (wstart = 0) then
      begin
        wstart := i;
        wlen := 1;
      end else
        Inc(wlen);
    end else
    begin
      if wstart > 0 then
      begin
        str[wstart] := ' ';
        Delete(str, wstart+1, wlen-1);
        Dec(i, wlen-1);
        wstart := 0;
      end;
    end;
    Inc(i);
  end;
  Result := str;
end;

function SwapCase(Const S: String): String;
// Inverts the character case. Like LowerCase and UpperCase combined.
var
  i : Integer;
  P : PChar;
begin
  Result := S;
  if not assigned(pointer(result)) then exit;
  UniqueString(Result);
  P:=Pchar(pointer(Result));
  for i := 1 to Length(Result) do begin
    if (P^ in ['a'..'z']) then
      P^ := char(byte(p^) - 32)
    else if (P^ in ['A'..'Z']) then
      P^ := char(byte(p^) + 32);
    Inc(P);
  end;
end;

function StringCase(const AString: String; const ACase: array of String {; const AIgnoreCase = False, APartial = false: Boolean}): Integer;
begin
  Result := StringCase(AString, ACase, False, False);
end;

function StringCase(const AString: String; const ACase: array of String; const AIgnoreCase, APartial: Boolean): Integer;
var
  Search, S: String;
begin
  if High(ACase) = -1
  then begin
    Result := -1;
    Exit;
  end;

  if AIgnoreCase
  then Search := UpperCase(AString)
  else Search := AString;

  for Result := Low(ACase) to High(ACase) do
  begin
    if AIgnoreCase
    then S := UpperCase(ACase[Result])
    else S := ACase[Result];

    if Search = S then Exit;
    if not APartial then Continue;
    if Length(Search) >= Length(S) then Continue;
    if StrLComp(PChar(Search), PChar(S), Length(Search)) = 0 then Exit;
  end;

  Result := -1;
end;

function ClassCase(const AClass: TClass; const ACase: array of TClass {; const ADecendant: Boolean = True}): Integer;
begin
  Result := ClassCase(AClass, ACase, True);
end;

function ClassCase(const AClass: TClass; const ACase: array of TClass; const ADecendant: Boolean): Integer;
begin
  for Result := Low(ACase) to High(ACase) do
  begin
    if AClass = ACase[Result] then Exit;
    if not ADecendant then Continue;
    if AClass.InheritsFrom(ACase[Result]) then Exit;
  end;

  Result := -1;
end;

function UTF16CharacterLength(p: PWideChar): integer;
// returns length of UTF16 character in number of words
// The endianess of the machine will be taken.
begin
  if p<>nil then begin
    if (ord(p[0]) < $D800) or (ord(p[0]) > $DFFF) then
      Result:=1
    else
      Result:=2;
  end else begin
    Result:=0;
  end;
end;

function UTF16Length(const s: UTF16String): PtrInt;
begin
  Result:=UTF16Length(PWideChar(s),length(s));
end;

function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (WordCount>0) do begin
    inc(Result);
    CharLen:=UTF16CharacterLength(p);
    inc(p,CharLen);
    dec(WordCount,CharLen);
  end;
end;

function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
var
  w1: cardinal;
  w2: Cardinal;
begin
  if p<>nil then begin
    w1:=ord(p[0]);
    if (w1 < $D800) or (w1 > $DFFF) then begin
      // is 1 word character
      Result:=w1;
      CharLen:=1;
    end else begin
      // could be 2 word character
      w2:=ord(p[1]);
      if (w2>=$DC00) then begin
        // is 2 word character
        Result:=(w1-$D800) shl 10 + (w2-$DC00) + $10000;
        CharLen:=2;
      end else begin
        // invalid character
        Result:=w1;
        CharLen:=1;
      end;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF16(u: cardinal): UTF16String;
begin
  // u should be <= $10FFFF to fit into UTF-16

  if u < $10000 then
    // Note: codepoints $D800 - $DFFF are reserved
    Result:=system.widechar(u)
  else
    Result:=system.widechar($D800+((u - $10000) shr 10))+system.widechar($DC00+((u - $10000) and $3ff));
end;

{$IFnDEF DisableWrapperFunctions}
function UTF8CharacterLength(p: PChar): integer;
begin
  Result := LazUTF8.UTF8CharacterLength(p);
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=LazUTF8.UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
begin
  Result := LazUTF8.UTF8Length(p, ByteCount);
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
begin
  Result := LazUTF8.UTF8CharacterToUnicode(p, CharLen);
end;

function UnicodeToUTF8(u: cardinal; Buf: PChar): integer;
begin
  Result := LazUTF8.UnicodeToUTF8(u, Buf);
end;

function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer;
begin
  Result := LazUTF8.UnicodeToUTF8SkipErrors(u, Buf);
end;

function UnicodeToUTF8(u: cardinal): shortstring;
begin
  Result[0]:=chr(LazUTF8.UnicodeToUTF8(u,@Result[1]));
end;

function UTF8ToDoubleByteString(const s: string): string;
begin
  Result := LazUTF8.UTF8ToDoubleByteString(s);
end;

{ returns number of double bytes }
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
begin
  Result := LazUTF8.UTF8ToDoubleByte(UTF8Str, Len, DBStr);
end;

{ Find the start of the UTF8 character which contains BytePos,
  Len is length in byte, BytePos starts at 0 }
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
begin
  Result := LazUTF8.UTF8FindNearestCharStart(UTF8Str, Len, BytePos);
end;

{ Len is the length in bytes of UTF8Str
  CharIndex is the position of the desired char (starting at 0), in chars

  This function is similar to UTF8FindNearestCharStart
}
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
begin
  Result := LazUTF8.UTF8CharStart(UTF8Str, Len, CharIndex);
end;

function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
begin
  Result := LazUTF8.UTF8CharToByteIndex(UTF8Str, Len, CharIndex);
end;

{ fix any broken UTF8 sequences with spaces }
procedure UTF8FixBroken(P: PChar);
begin
  LazUTF8.UTF8FixBroken(P);
end;

function UTF8CharacterStrictLength(P: PChar): integer;
begin
  Result := LazUTF8.UTF8CharacterStrictLength(P);
end;

function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
begin
  Result := LazUTF8.UTF8CStringToUTF8String(SourceStart, SourceLen);
end;

function UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
begin
  Result := LazUTF8.UTF8Pos(SearchForText, SearchInText);
end;

function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
begin
  Result := LazUTF8.UTF8Copy(s, StartCharIndex, CharCount);
end;

procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
begin
  LazUTF8.UTF8Delete(s, StartCharIndex, CharCount);
end;

procedure UTF8Insert(const source: String; var s: string; StartCharIndex: PtrInt);
begin
  LazUTF8.UTF8Insert(source, s, StartCharIndex);
end;

function UTF8LowerCase(const s: String): String;
begin
  Result := LazUTF8.UTF8LowerCase(S);
end;

function UTF8UpperCase(const s: String): String;
begin
  Result := LazUTF8.UTF8UpperCase(s);
end;

function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
  StopOnNonASCII: Boolean): PtrInt;
// return -1 if ok
begin
  Result := LazUTF8.FindInvalidUTF8Character(p, Count, StopOnNonASCII);
end;

function ValidUTF8String(const s: String): String;
begin
  Result := LazUTF8.ValidUTF8String(s);
end;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);
begin
  LazUTF8.AssignUTF8ListToAnsi(UTF8List, AnsiList);
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareStr
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S2 > S1.
  Compare 2 UTF8 encoded strings, case sensitive.
 ------------------------------------------------------------------------------}
function UTF8CompareStr(const S1, S2: String): Integer;
begin
  Result := LazUTF8.UTF8CompareStr(S1,S2);
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareText
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S2 > S1.
  Compare 2 UTF8 encoded strings, case insensitive.
 ------------------------------------------------------------------------------}
function UTF8CompareText(const S1, S2: String): Integer;
begin
  Result := LazUTF8.UTF8CompareText(S1,S2);
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF8ToUTF16
  Params:  Dest                - Pointer to destination string
           DestWideCharCount   - Wide char count allocated in destination string
           Src                 - Pointer to source string
           SrcCharCount        - Char count allocated in source string
           Options             - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped

             toInvalidCharError       - Stop on invalid source char and report
                                      error
             toInvalidCharToSymbol    - Replace invalid source chars with '?'
             toUnfinishedCharError    - Stop on unfinished source char and
                                      report error
             toUnfinishedCharToSymbol - Replace unfinished source char with '?'

           ActualWideCharCount - Actual wide char count converted from source
                               string to destination string
  Returns:
    trNoError        - The string was successfully converted without
                     any error
    trNullSrc        - Pointer to source string is nil
    trNullDest       - Pointer to destination string is nil
    trDestExhausted  - Destination buffer size is not big enough to hold
                     converted string
    trInvalidChar    - Invalid source char has occured
    trUnfinishedChar - Unfinished source char has occured

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
 ------------------------------------------------------------------------------}
function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;
begin
  Result := LazUTF8.ConvertUTF8ToUTF16(Dest, DestWideCharCount,
    Src, SrcCharCount, Options, ActualWideCharCount);
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF16ToUTF8
  Params:  Dest             - Pointer to destination string
           DestCharCount    - Char count allocated in destination string
           Src              - Pointer to source string
           SrcWideCharCount - Wide char count allocated in source string
           Options          - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped.
             See ConvertUTF8ToUTF16 for details.

           ActualCharCount  - Actual char count converted from source
                            string to destination string
  Returns: See ConvertUTF8ToUTF16

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;
begin
  Result := LazUTF8.ConvertUTF16ToUTF8(Dest, DestCharCount,
    Src, SrcWideCharCount, Options, ActualCharCount);
end;

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
  Avoid copying the result string since on windows a widestring requires a full 
  copy
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: AnsiString): UTF16String;
begin
  Result := LazUTF8.UTF8ToUTF16(S);
end;

{------------------------------------------------------------------------------
  Name:    UTF16ToUTF8
  Params:  S - Source UTF-16 string (system endian)
  Returns: UTF-8 encoded string

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function UTF16ToUTF8(const S: UTF16String): AnsiString;
begin
  Result := LazUTF8.UTF16ToUTF8(S);
end;

procedure LCLGetLanguageIDs(var Lang, FallbackLang: String);
begin
  LazUTF8.LazGetLanguageIDs(Lang, FallbackLang);
end;
{$ENDIF DisableWrapperFunctions}

function CreateFirstIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident1
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)+'1';
end;

function CreateNextIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident60
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)
          +IntToStr(1+StrToIntDef(copy(Identifier,p+1,length(Identifier)-p),0));
end;

procedure FreeLineInfoCache;
var
  ANode: TAvgLvlTreeNode;
  Item: PLineInfoCacheItem;
begin
  if LineInfoCache=nil then exit;
  ANode:=LineInfoCache.FindLowest;
  while ANode<>nil do begin
    Item:=PLineInfoCacheItem(ANode.Data);
    Dispose(Item);
    ANode:=LineInfoCache.FindSuccessor(ANode);
  end;
  LineInfoCache.Free;
  LineInfoCache:=nil;
end;


initialization
  {$IFDEF WithOldDebugln} InitializeDebugOutput; {$ENDIF}
  {$ifdef WinCE}
  // The stabs based back trace function crashes on wince,
  // see http://bugs.freepascal.org/view.php?id=14330
  // To prevent crashes, replace it with the default system back trace function
  // that just outputs addresses and not source and line number
  BackTraceStrFunc := @SysBackTraceStr;
  {$endif}
  InterfaceInitializationHandlers := TFPList.Create;
  InterfaceFinalizationHandlers := TFPList.Create;
  {$IFDEF DebugLCLComponents}
  DebugLCLComponents:=TDebugLCLItems.Create('LCLComponents');
  {$ENDIF}
finalization
  InterfaceInitializationHandlers.Free;
  InterfaceInitializationHandlers:=nil;
  InterfaceFinalizationHandlers.Free;
  InterfaceFinalizationHandlers:=nil;
  {$IFDEF DebugLCLComponents}
  DebugLCLComponents.Free;
  DebugLCLComponents:=nil;
  {$ENDIF}
  FreeLineInfoCache;
  {$IFDEF WithOldDebugln}
  FinalizeDebugOutput;
  DebugLnNestFreePrefix;
  {$ENDIF}

end.
