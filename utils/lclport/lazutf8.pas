{
 /***************************************************************************
                                  lazutf8.pas
 ***************************************************************************/

 *****************************************************************************
  This file is part of LazUtils

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Useful routines for managing UTF-8 strings

  - all functions are thread safe unless explicitely stated
}
unit LazUTF8;

{$mode objfpc}{$H+}{$inline on}

{$i lazutils_defines.inc}

interface

uses
  {$IFDEF UTF8_RTL}
  {$ifdef unix}
  cwstring, // UTF8 RTL on Unix requires this. Must be used although it pulls in clib.
  {$endif}
  FPCAdds,
  {$ENDIF}
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils;

// AnsiToUTF8 and UTF8ToAnsi need a widestring manager under Linux, BSD, MacOSX
// but normally these OS use UTF-8 as system encoding so the widestringmanager
// is not needed.
function NeedRTLAnsi: boolean;// true if system encoding is not UTF-8
procedure SetNeedRTLAnsi(NewValue: boolean);

// UTF8ToSys works like UTF8ToAnsi but more independent of widestringmanager
function UTF8ToSys(const s: string): string; overload; {$IFDEF UTF8_RTL}inline;{$ENDIF}
function UTF8ToSys(const AFormatSettings: TFormatSettings): TFormatSettings; overload; {$IFDEF UTF8_RTL}inline;{$ENDIF}

// SysToUTF8 works like AnsiToUTF8 but more independent of widestringmanager
function SysToUTF8(const s: string): string; overload;
function SysToUTF8(const AFormatSettings: TFormatSettings): TFormatSettings; overload;

// converts OEM encoded string to UTF8 (used with some Windows specific functions)
function ConsoleToUTF8(const s: string): string;
// converts UTF8 string to console encoding (used by Write, WriteLn)
function UTF8ToConsole(const s: string): string;

// for all Windows supporting 8bit codepages (e.g. not WinCE)
// converts string in Windows code page to UTF8 (used with some Windows specific functions)
function WinCPToUTF8(const s: string): string; {$ifdef WinCe}inline;{$endif}
// converts UTF8 string to Windows code page encoding (used by Write, WriteLn)
function UTF8ToWinCP(const s: string): string; {$ifdef WinCe}inline;{$endif}

function ParamStrUTF8(Param: Integer): string;

function GetEnvironmentStringUTF8(Index: Integer): string;
function GetEnvironmentVariableUTF8(const EnvVar: string): String;

function SysErrorMessageUTF8(ErrorCode: Integer): String;

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
function UnicodeToUTF8(CodePoint: cardinal): string;
function UnicodeToUTF8(CodePoint: cardinal; Buf: PChar): integer;
function UnicodeToUTF8SkipErrors(CodePoint: cardinal; Buf: PChar): integer;
function UnicodeToUTF8Inline(CodePoint: cardinal; Buf: PChar): integer; inline;
function UTF8ToDoubleByteString(const s: string): string;
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: SizeInt;
                                  BytePos: SizeInt): SizeInt;
// find the n-th UTF8 character, ignoring BIDI
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
// find the byte index of the n-th UTF8 character, ignoring BIDI (byte len of substr)
function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
procedure UTF8FixBroken(P: PChar); overload;
procedure UTF8FixBroken(var S: string); overload;
function UTF8CharacterStrictLength(P: PChar): integer;
function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;

function UTF8Pos(const SearchForText, SearchInText: string; StartPos: SizeInt = 1): PtrInt;
function UTF8PosP(SearchForText: PChar; SearchForTextLen: SizeInt;
  SearchInText: PChar; SearchInTextLen: SizeInt): PChar;
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
{$IFnDEF NO_CP_RTL}
procedure UTF8Delete(var s: Utf8String; StartCharIndex, CharCount: PtrInt);
{$ENDIF}
procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
{$IFnDEF NO_CP_RTL}
procedure UTF8Insert(const source: Utf8String; var s: Utf8String; StartCharIndex: PtrInt);
{$ENDIF}
procedure UTF8Insert(const source: String; var s: String; StartCharIndex: PtrInt);
function UTF8StringReplace(const S, OldPattern, NewPattern: String;
  Flags: TReplaceFlags; ALanguage: string=''): String;

function UTF8LowerCase(const AInStr: string; ALanguage: string=''): string;
function UTF8LowerString(const s: string): string;
function UTF8UpperCase(const AInStr: string; ALanguage: string=''): string;
function UTF8UpperString(const s: string): string;
function UTF8SwapCase(const AInStr: string; ALanguage: string=''): string;
function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
                                  StopOnNonUTF8: Boolean = true): PtrInt;
function ValidUTF8String(const s: String): String;
function UTF8StringOfChar(AUtf8Char: String; N: Integer): String;
function UTF8AddChar(AUtf8Char: String; const S: String; N: Integer): String;
function UTF8AddCharR(AUtf8Char: String; const S: String; N: Integer): String;
function UTF8PadLeft(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
function UTF8PadRight(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
function UTF8PadCenter(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
function UTF8LeftStr(const AText: String; const ACount: Integer): String;
function UTF8RightStr(const AText: String; const ACount: Integer): String;
function UTF8QuotedStr(const S, Quote: string): string;
//Utf8 version of MidStr is just Utf8Copy with same parameters, so it is not implemented here
function Utf8StartsText(const ASubText, AText: string): Boolean;
function Utf8EndsText(const ASubText, AText: string): Boolean;

function UTF8WrapText(S, BreakStr :string; BreakChars :TSysCharSet; MaxCol: integer): string; overload;
function UTF8WrapText(S :string; MaxCol :integer) :string; overload;

type
  TUTF8TrimFlag = (
    u8tKeepStart,
    u8tKeepEnd,
    u8tKeepTabs,
    u8tKeepLineBreaks,
    u8tKeepNoBreakSpaces,
    u8tKeepControlCodes // excluding tabs and line breaks
    );
  TUTF8TrimFlags = set of TUTF8TrimFlag;
function UTF8Trim(const s: string; Flags: TUTF8TrimFlags = []): string;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);

//compare functions

function UTF8CompareStr(const S1, S2: string): PtrInt; inline;
function UTF8CompareStrP(S1, S2: PChar): PtrInt;
function UTF8CompareStr(S1: PChar; Count1: SizeInt; S2: PChar; Count2: SizeInt): PtrInt;
function UTF8CompareText(const S1, S2: string): PtrInt;
function UTF8CompareStrCollated(const S1, S2: string): PtrInt;
function CompareStrListUTF8LowerCase(List: TStringList; Index1, Index2: Integer): Integer;

type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;

function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;

function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;

function UTF8ToUTF16(const S: AnsiString): UnicodeString; overload;
function UTF8ToUTF16(const P: PChar; ByteCnt: SizeUInt): UnicodeString; overload;
function UTF16ToUTF8(const S: UnicodeString): AnsiString; overload;
function UTF16ToUTF8(const P: PWideChar; WideCnt: SizeUInt): AnsiString; overload;

// locale
procedure LazGetLanguageIDs(var Lang, FallbackLang: String);
procedure LazGetShortLanguageID(var Lang: String);

var
  FPUpChars: array[char] of char;

procedure ReplaceSubstring(var s: string; StartPos, Count: SizeInt;
                           const Insertion: string);

implementation

uses
  gettext
{$IFDEF Darwin}, MacOSAll{$ENDIF}
  ;

{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}

function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;

{$ifdef windows}
  {$i winlazutf8.inc}
{$else}
  {$i unixlazutf8.inc}
{$endif}

var
  FNeedRTLAnsi: boolean = false;
  FNeedRTLAnsiValid: boolean = false;

function NeedRTLAnsi: boolean;
{$IFNDEF Windows}
var
  Lang: String;
  i: LongInt;
  Encoding: String;
{$ENDIF}
begin
  if FNeedRTLAnsiValid then
    exit(FNeedRTLAnsi);
  {$IFDEF Windows}
    {$IF FPC_FULLVERSION>=20701}
    FNeedRTLAnsi:=DefaultSystemCodePage<>CP_UTF8;
    {$ELSE}
    FNeedRTLAnsi:=GetACP<>CP_UTF8;
    {$ENDIF}
  {$ELSE}
  FNeedRTLAnsi:=false;
  Lang := SysUtils.GetEnvironmentVariable('LC_ALL');
  if lang = '' then
  begin
    Lang := SysUtils.GetEnvironmentVariable('LC_MESSAGES');
    if Lang = '' then
    begin
      Lang := SysUtils.GetEnvironmentVariable('LANG');
    end;
  end;
  i:=System.Pos('.',Lang);
  if (i>0) then begin
    Encoding:=copy(Lang,i+1,length(Lang)-i);
    FNeedRTLAnsi:=(SysUtils.CompareText(Encoding,'UTF-8')<>0)
              and (SysUtils.CompareText(Encoding,'UTF8')<>0);
  end;
  {$ENDIF}
  FNeedRTLAnsiValid:=true;
  Result:=FNeedRTLAnsi;
end;

procedure SetNeedRTLAnsi(NewValue: boolean);
begin
  FNeedRTLAnsi:=NewValue;
  FNeedRTLAnsiValid:=true;
end;

function UTF8ToSys(const s: string): string;
begin
  {$IFDEF UTF8_RTL}
  Result:=s;
  {$ELSE}
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=UTF8ToAnsi(s)
  else
    Result:=s;
  {$ENDIF}
end;

function SysToUTF8(const s: string): string;
begin
  {$IFDEF UTF8_RTL}
  Result:=s;
  {$ELSE}
  if NeedRTLAnsi and (not IsASCII(s)) then
  begin
    Result:=AnsiToUTF8(s);
    {$ifdef FPC_HAS_CPSTRING}
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic in LCL code
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
    {$endif}
  end
  else
    Result:=s;
  {$ENDIF}
end;

function SysToUTF8(const AFormatSettings: TFormatSettings): TFormatSettings;
var
  i: Integer;
begin
  Result := AFormatSettings;
  Result.CurrencyString := SysToUTF8(AFormatSettings.CurrencyString);
  for i:=1 to 12 do begin
    Result.LongMonthNames[i] := SysToUTF8(AFormatSettings.LongMonthNames[i]);
    Result.ShortMonthNames[i] := SysToUTF8(AFormatSettings.ShortMonthNames[i]);
  end;
  for i:=1 to 7 do begin
    Result.LongDayNames[i] := SysToUTF8(AFormatSettings.LongDayNames[i]);
    Result.ShortDayNames[i] := SysToUTF8(AFormatSettings.ShortDayNames[i]);
  end;
end;

function UTF8ToSys(const AFormatSettings: TFormatSettings): TFormatSettings;
{$IFnDEF UTF8_RTL}
var
  i: Integer;
{$ENDIF}
begin
  Result := AFormatSettings;
  {$IFnDEF UTF8_RTL}
  Result.CurrencyString := UTF8ToSys(AFormatSettings.CurrencyString);
  for i:=1 to 12 do begin
    Result.LongMonthNames[i] := UTF8ToSys(AFormatSettings.LongMonthNames[i]);
    Result.ShortMonthNames[i] := UTF8ToSys(AFormatSettings.ShortMonthNames[i]);
  end;
  for i:=1 to 7 do begin
    Result.LongDayNames[i] := UTF8ToSys(AFormatSettings.LongDayNames[i]);
    Result.ShortDayNames[i] := UTF8ToSys(AFormatSettings.ShortDayNames[i]);
  end;
  {$ENDIF}
end;

function GetEnvironmentStringUTF8(Index: Integer): string;
begin
  {$IFDEF FPC_RTL_UNICODE}
  Result:=UTF16ToUTF8(SysUtils.GetEnvironmentString(Index));
  {$ELSE}
  // on Windows SysUtils.GetEnvironmentString returns OEM encoded string
  // so ConsoleToUTF8 function should be used!
  // RTL issue: http://bugs.freepascal.org/view.php?id=15233
  Result:=ConsoleToUTF8(SysUtils.GetEnvironmentString(Index));
  {$ENDIF}
end;

function GetEnvironmentVariableUTF8(const EnvVar: string): String;
begin
  {$IFDEF FPC_RTL_UNICODE}
  Result:=UTF16ToUTF8(SysUtils.GetEnvironmentVariable(UTF8ToUTF16(EnvVar)));
  {$ELSE}
  // on Windows SysUtils.GetEnvironmentString returns OEM encoded string
  // so ConsoleToUTF8 function should be used!
  // RTL issue: http://bugs.freepascal.org/view.php?id=15233
  Result:=ConsoleToUTF8(SysUtils.GetEnvironmentVariable(UTF8ToSys(EnvVar)));
  {$ENDIF}
end;

function SysErrorMessageUTF8(ErrorCode: Integer): String;
begin
  Result := SysToUTF8(SysUtils.SysErrorMessage(ErrorCode));
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else begin
      // multi byte
      if ((ord(p^) and %11100000) = %11000000) then begin
        // could be 2 byte character
        if (ord(p[1]) and %11000000) = %10000000 then
          Result:=2
        else
          Result:=1;
      end
      else if ((ord(p^) and %11110000) = %11100000) then begin
        // could be 3 byte character
        if ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then
          Result:=3
        else
          Result:=1;
      end
      else if ((ord(p^) and %11111000) = %11110000) then begin
        // could be 4 byte character
        if ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000)
        and ((ord(p[3]) and %11000000) = %10000000) then
          Result:=4
        else
          Result:=1;
      end
      else
        Result:=1;
    end;
  end else
    Result:=0;
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
{ if p=nil then CharLen=0 otherwise CharLen>0
  If there is an encoding error the Result is 0 and CharLen=1.
  Use UTF8FixBroken to fix UTF-8 encoding.
  It does not check if the codepoint is defined in the Unicode tables.
}
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // starts with %110 => could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        CharLen:=2;
        Result:=((ord(p^) and %00011111) shl 6)
                or (ord(p[1]) and %00111111);
        if Result<(1 shl 7) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // starts with %1110 => could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        CharLen:=3;
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        if Result<(1 shl 11) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // starts with %11110 => could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        CharLen:=4;
        Result:=((ord(p^) and %00001111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
        if Result<(1 shl 16) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else begin
      // invalid character
      Result:=ord(p^);
      CharLen:=1;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF8(CodePoint: cardinal; Buf: PChar): integer;

  procedure RaiseInvalidUnicode;
  begin
    raise Exception.Create('UnicodeToUTF8: invalid unicode: '+IntToStr(CodePoint));
  end;

begin
  Result:=UnicodeToUTF8Inline(CodePoint,Buf);
  if Result=0 then
    RaiseInvalidUnicode;
end;

function UnicodeToUTF8SkipErrors(CodePoint: cardinal; Buf: PChar): integer;
begin
  Result:=UnicodeToUTF8Inline(CodePoint,Buf);
end;

function UnicodeToUTF8(CodePoint: cardinal): string;
var
  Buf: array[0..6] of Char;
  Len: Integer;
begin
  Len:=UnicodeToUTF8Inline(CodePoint, @Buf[0]);
  Buf[Len]:=#0;
  Result := StrPas(@Buf[0]);
end;

function UnicodeToUTF8Inline(CodePoint: cardinal; Buf: PChar): integer;
begin
  case CodePoint of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(CodePoint));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (CodePoint shr 6)));
        Buf[1]:=char(byte($80 or (CodePoint and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (CodePoint shr 12)));
        Buf[1]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[2]:=char(byte(CodePoint and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (CodePoint shr 18)));
        Buf[1]:=char(byte((CodePoint shr 12) and $3f) or $80);
        Buf[2]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[3]:=char(byte(CodePoint and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

function UTF8ToDoubleByteString(const s: string): string;
var
  Len: Integer;
begin
  Len:=UTF8Length(s);
  SetLength(Result,Len*2);
  if Len=0 then exit;
  UTF8ToDoubleByte(PChar(s),length(s),PByte(Result));
end;

{ returns number of double bytes }
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
var
  SrcPos: PChar;
  CharLen: LongInt;
  DestPos: PByte;
  u: Cardinal;
begin
  SrcPos:=UTF8Str;
  DestPos:=DBStr;
  Result:=0;
  while Len>0 do begin
    u:=UTF8CharacterToUnicode(SrcPos,CharLen);
    DestPos^:=byte((u shr 8) and $ff);
    inc(DestPos);
    DestPos^:=byte(u and $ff);
    inc(DestPos);
    inc(SrcPos,CharLen);
    dec(Len,CharLen);
    inc(Result);
  end;
end;

{ Find the start of the UTF8 character which contains BytePos,
  Len is length in byte, BytePos starts at 0 }
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: SizeInt; BytePos: SizeInt
  ): SizeInt;
begin
  Result:=0;
  if (UTF8Str<>nil) and (Len>0) and (BytePos>=0) then begin
    Result:=BytePos;
    if Result>Len then Result:=Len-1;
    if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
      dec(Result);
      if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
        dec(Result);
        if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
          dec(Result);
          // should be four byte character
          if (ord(UTF8Str[Result]) and %11111000<>%11110000) then begin
            // broken UTF8 character
            inc(Result,3);
          end else begin
            // is four byte character
          end;
        end else if (ord(UTF8Str[Result]) and %11110000<>%11100000) then begin
          // broken UTF8 character, should be three byte
          inc(Result,2);
        end else
        begin
          // is three byte character
        end;
      end else if (ord(UTF8Str[Result]) and %11100000<>%11000000) then begin
        // broken UTF8 character, should be two byte
        inc(Result);
      end else
      begin
        // is two byte character
      end;
    end;
  end;
end;

{ Len is the length in bytes of UTF8Str
  CharIndex is the position of the desired char (starting at 0), in chars
}
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
var
  CharLen: LongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CharIndex>0) and (Len>0) do begin
      CharLen:=UTF8CharacterLength(Result);
      dec(Len,CharLen);
      dec(CharIndex);
      inc(Result,CharLen);
    end;
    if (CharIndex<>0) or (Len<0) then
      Result:=nil;
  end;
end;

function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
var
  p: PChar;
begin
  p := UTF8CharStart(UTF8Str, Len, CharIndex);
  if p = nil
  then Result := -1
  else Result := p - UTF8Str;
end;

{ fix any broken UTF8 sequences with spaces }
procedure UTF8FixBroken(P: PChar);
var
  c: cardinal;
begin
  if p=nil then exit;
  while p^<>#0 do begin
    if ord(p^)<%10000000 then begin
      // regular single byte character
      inc(p);
    end
    else if ord(p^)<%11000000 then begin
      // invalid
      p^:=' ';
      inc(p);
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // starts with %110 => should be 2 byte character
      if ((ord(p[1]) and %11000000) = %10000000) then begin
        c:=((ord(p^) and %00011111) shl 6);
           //or (ord(p[1]) and %00111111);
        if c<(1 shl 7) then
          p^:=' '  // fix XSS attack
        else
          inc(p,2)
      end
      else if p[1]<>#0 then
        p^:=' ';
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // starts with %1110 => should be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        c:=((ord(p^) and %00011111) shl 12)
           or ((ord(p[1]) and %00111111) shl 6);
           //or (ord(p[2]) and %00111111);
        if c<(1 shl 11) then
          p^:=' '  // fix XSS attack
        else
          inc(p,3);
      end else
        p^:=' ';
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // starts with %11110 => should be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        c:=((ord(p^) and %00001111) shl 18)
           or ((ord(p[1]) and %00111111) shl 12)
           or ((ord(p[2]) and %00111111) shl 6);
           //or (ord(p[3]) and %00111111);
        if c<(1 shl 16) then
          p^:=' ' // fix XSS attack
        else
          inc(p,4)
      end else
        p^:=' ';
    end
    else begin
      p^:=' ';
      inc(p);
    end;
  end;
end;

procedure UTF8FixBroken(var S: string);
begin
  if S='' then exit;
  if FindInvalidUTF8Character(PChar(S),length(S))<0 then exit;
  UniqueString(S);
  UTF8FixBroken(PChar(S));
end;

function UTF8CharacterStrictLength(P: PChar): integer;
begin
  if p=nil then exit(0);
  if ord(p^)<%10000000 then begin
    // regular single byte character
    exit(1);
  end
  else if ord(p^)<%11000000 then begin
    // invalid single byte character
    exit(0);
  end
  else if ((ord(p^) and %11100000) = %11000000) then begin
    // should be 2 byte character
    if (ord(p[1]) and %11000000) = %10000000 then
      exit(2)
    else
      exit(0);
  end
  else if ((ord(p^) and %11110000) = %11100000) then begin
    // should be 3 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then
      exit(3)
    else
      exit(0);
  end
  else if ((ord(p^) and %11111000) = %11110000) then begin
    // should be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then
      exit(4)
    else
      exit(0);
  end else
    exit(0);
end;

function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
var
  Source: PChar;
  Dest: PChar;
  SourceEnd: PChar;
  SourceCopied: PChar;

  // Copies from SourceStart till Source to Dest and updates Dest
  procedure CopyPart; inline;
  var
    CopyLength: SizeInt;
  begin
    CopyLength := Source - SourceCopied;
    if CopyLength=0 then exit;
    System.move(SourceCopied^ , Dest^, CopyLength);
    SourceCopied:=Source;
    inc(Dest, CopyLength);
  end;

begin
  SetLength(Result, SourceLen);
  if SourceLen=0 then exit;
  SourceCopied:=SourceStart;
  Source:=SourceStart;
  Dest:=PChar(Result);
  SourceEnd := Source + SourceLen;
  while Source<SourceEnd do begin
    if (Source^='\') then begin
      CopyPart;
      inc(Source);
      if Source^ in ['t', 'n', '"', '\'] then begin
        case Source^ of
         't' : Dest^ := #9;
         '"' : Dest^ := '"';
         '\' : Dest^ := '\';
         'n' :
         // fpc 2.1.1 stores string constants as array of char so maybe this
         // will work for without ifdef (once available in 2.0.x too):
         // move(lineending, dest^, sizeof(LineEnding));
{$IFDEF WINDOWS}
               begin
                 move(lineending[1], dest^, length(LineEnding));
                 inc(dest, length(LineEnding)-1);
               end;
{$ELSE}
               Dest^ := LineEnding;
{$ENDIF}
        end;
        inc(Source);
        inc(Dest);
      end;
      SourceCopied := Source;
    end
    else
      Inc(Source); // no need for checking for UTF8, the / is never part of an UTF8 multibyte codepoint
  end;
  CopyPart;
  SetLength(Result, Dest - PChar(Result));
end;

function UTF8Pos(const SearchForText, SearchInText: string;
  StartPos: SizeInt = 1): PtrInt;
// returns the character index, where the SearchForText starts in SearchInText
// an optional StartPos can be given (in UTF-8 codepoints, not in byte)
// returns 0 if not found
var
  i: SizeInt;
  p: PChar;
  StartPosP: PChar;
begin
  Result:=0;
  if StartPos=1 then
  begin
    i:=System.Pos(SearchForText,SearchInText);
    if i>0 then
      Result:=UTF8Length(PChar(SearchInText),i-1)+1;
  end
  else if StartPos>1 then
  begin
    // skip
    StartPosP:=UTF8CharStart(PChar(SearchInText),Length(SearchInText),StartPos-1);
    if StartPosP=nil then exit;
    // search
    p:=UTF8PosP(PChar(SearchForText),length(SearchForText),
                StartPosP,length(SearchInText)+PChar(SearchInText)-StartPosP);
    // get UTF-8 position
    if p=nil then exit;
    Result:=StartPos+UTF8Length(StartPosP,p-StartPosP);
  end;
end;

function UTF8PosP(SearchForText: PChar; SearchForTextLen: SizeInt;
  SearchInText: PChar; SearchInTextLen: SizeInt): PChar;
// returns the position where SearchInText starts in SearchForText
// returns nil if not found
var
  p: SizeInt;
begin
  Result:=nil;
  if (SearchForText=nil) or (SearchForTextLen=0) or (SearchInText=nil) then
    exit;
  while SearchInTextLen>0 do begin
    p:=IndexByte(SearchInText^,SearchInTextLen,PByte(SearchForText)^);
    if p<0 then exit;
    inc(SearchInText,p);
    dec(SearchInTextLen,p);
    if SearchInTextLen<SearchForTextLen then exit;
    if CompareMem(SearchInText,SearchForText,SearchForTextLen) then
      exit(SearchInText);
    inc(SearchInText);
    dec(SearchInTextLen);
  end;
end;

function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
// returns substring
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos=nil then
    Result:=''
  else begin
    MaxBytes:=PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos:=UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos=nil then
      Result:=copy(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Result:=copy(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

{$IFnDEF NO_CP_RTL}
procedure UTF8Delete(var s: Utf8String; StartCharIndex, CharCount: PtrInt);
var
  tmp: String;
begin
  tmp := RawByteString(s);
  {.$IFDEF ACP_RTL}
  { change code page without converting the data }
  SetCodePage(RawByteString(tmp), CP_UTF8, False);
  {.$ENDIF}
  { keep refcount to 1 if it was 1, to avoid unnecessary copies }
  s := '';
  UTF8Delete(tmp,StartCharIndex,CharCount);
  { same as above }
  s := RawByteString(tmp);
  tmp := '';
  SetCodePage(RawByteString(s), CP_UTF8, False);
end;
{$ENDIF NO_ACP_RTL}

procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
  begin
    MaxBytes:=PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos:=UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos=nil then
      Delete(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Delete(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

{$IFnDEF NO_CP_RTL}
{It's simper to copy the code from the variant with String parameters than writing a wrapper}
procedure UTF8Insert(const source: UTF8String; var s: UTF8string;
  StartCharIndex: PtrInt);
var
  StartBytePos: PChar;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
    Insert(source, s, StartBytePos-PChar(s)+1);
end;
{$ENDIF NO_CP_RTL}

procedure UTF8Insert(const source: String; var s: String; StartCharIndex: PtrInt);
var
  StartBytePos: PChar;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
    Insert(source, s, StartBytePos-PChar(s)+1);
end;

function UTF8StringReplace(const S, OldPattern, NewPattern: String;
  Flags: TReplaceFlags; ALanguage: string): String;
// same algorithm as StringReplace, but using UTF8LowerCase
// for case insensitive search
var
  Srch, OldP, RemS: string;
  P: Integer;
begin
  Srch := S;
  OldP := OldPattern;
  if rfIgnoreCase in Flags then
  begin
    Srch := UTF8LowerCase(Srch,ALanguage);
    OldP := UTF8LowerCase(OldP,ALanguage);
  end;
  RemS := S;
  Result := '';
  while Length(Srch) <> 0 do
  begin
    P := Pos(OldP, Srch);
    if P = 0 then
    begin
      Result := Result + RemS;
      Srch := '';
    end
    else
    begin
      Result := Result + Copy(RemS,1,P-1) + NewPattern;
      P := P + Length(OldP);
      RemS := Copy(RemS, P, Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
      begin
        Result := Result + RemS;
        Srch := '';
      end
      else
        Srch := Copy(Srch, P, Length(Srch)-P+1);
    end;
  end;
end;

{
  UTF8SwapCase - a "naive" implementation that uses UTF8UpperCase and UTF8LowerCase.
    It serves its purpose and performs OK for short and resonably long strings
    but it should be rewritten in the future if better performance and lower
    memory consumption is needed.

  AInStr - The input string.
  ALanguage - The language. Use '' for maximum speed if one desires to ignore the language
    (See UTF8LowerCase comment for more details on ALanguage parameter.)
}
function UTF8SwapCase(const AInStr: string; ALanguage: string=''): string;
var
  xUpperCase: string;
  xLowerCase: string;
  I: Integer;
begin
  if AInStr = '' then
    Exit('');

  xUpperCase := UTF8UpperCase(AInStr, ALanguage);
  xLowerCase := UTF8LowerCase(AInStr, ALanguage);
  if (Length(xUpperCase) <> Length(AInStr)) or (Length(xLowerCase) <> Length(AInStr)) then
    Exit(AInStr);//something went wrong -> the lengths of utf8 strings changed

  SetLength(Result, Length(AInStr));
  for I := 1 to Length(AInStr) do
    if AInStr[I] <> xUpperCase[I] then
      Result[I] := xUpperCase[I]
    else
      Result[I] := xLowerCase[I];
end;

{
  AInStr - The input string
  ALanguage - The language. Use '' for maximum speed if one desires to ignore the language
              The language should be specified in the format from ISO 639-1,
              which uses 2 characters to represent each language.
              If the language has no code in ISO 639-1, then the 3-chars code
              from ISO 639-2 should be used.
              Example: "tr" - Turkish language locale

  Data from here: ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt

  The columns in the file UnicodeData.txt are explained here:
  http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#Case Mappings
}
function UTF8LowerCase(const AInStr: string; ALanguage: string=''): string;
var
  CounterDiff: PtrInt;
  InStr, InStrEnd, OutStr: PChar;
  // Language identification
  IsTurkish: Boolean;
  c1, c2, c3, new_c1, new_c2, new_c3: Char;
  p: SizeInt;
begin
  Result:=AInStr;
  InStr := PChar(AInStr);
  InStrEnd := InStr + length(AInStr); // points behind last char

  // Do a fast initial parsing of the string to maybe avoid doing
  // UniqueString if the resulting string will be identical
  while (InStr < InStrEnd) do
  begin
    c1 := InStr^;
    case c1 of
    'A'..'Z': Break;
    #$C3..#$FF:
      case c1 of
      #$C3..#$C9, #$CE, #$CF, #$D0..#$D5, #$E1..#$E2,#$E5:
        begin
          c2 := InStr[1];
          case c1 of
          #$C3: if c2 in [#$80..#$9E] then Break;
          #$C4:
          begin
            case c2 of
            #$80..#$AF, #$B2..#$B6: if ord(c2) mod 2 = 0 then Break;
            #$B8..#$FF: if ord(c2) mod 2 = 1 then Break;
            #$B0: Break;
            end;
          end;
          #$C5:
          begin
            case c2 of
              #$8A..#$B7: if ord(c2) mod 2 = 0 then Break;
              #$00..#$88, #$B9..#$FF: if ord(c2) mod 2 = 1 then Break;
              #$B8: Break;
            end;
          end;
          // Process E5 to avoid stopping on chinese chars
          #$E5: if (c2 = #$BC) and (InStr[2] in [#$A1..#$BA]) then Break;
          // Others are too complex, better not to pre-inspect them
          else
            Break;
          end;
          // already lower, or otherwhise not affected
        end;
      end;
    end;
    inc(InStr);
  end;

  if InStr >= InStrEnd then Exit;

  // Language identification
  IsTurkish := (ALanguage = 'tr') or (ALanguage = 'az'); // Turkish and Azeri have a special handling

  UniqueString(Result);
  OutStr := PChar(Result) + (InStr - PChar(AInStr));
  CounterDiff := 0;

  while InStr < InStrEnd do
  begin
    c1 := InStr^;
    case c1 of
      // codepoints      UTF-8 range           Description                Case change
      // $0041..$005A    $41..$5A              Capital ASCII              X+$20
      'A'..'Z':
      begin
        { First ASCII chars }
        // Special turkish handling
        // capital undotted I to small undotted i
        if IsTurkish and (c1 = 'I') then
        begin
          p:=OutStr - PChar(Result);
          SetLength(Result,Length(Result)+1);// Increase the buffer
          OutStr := PChar(Result)+p;
          OutStr^ := #$C4;
          inc(OutStr);
          OutStr^ := #$B1;
          dec(CounterDiff);
        end
        else
        begin
          OutStr^ := chr(ord(c1)+32);
        end;
        inc(InStr);
        inc(OutStr);
      end;

      // Chars with 2-bytes which might be modified
      #$C3..#$D5:
      begin
        c2 := InStr[1];
        new_c1 := c1;
        new_c2 := c2;
        case c1 of
        // Latin Characters 0000–0FFF http://en.wikibooks.org/wiki/Unicode/Character_reference/0000-0FFF
        // codepoints      UTF-8 range           Description                Case change
        // $00C0..$00D6    C3 80..C3 96          Capital Latin with accents X+$20
        // $D7             C3 97                 Multiplication Sign        N/A
        // $00D8..$00DE    C3 98..C3 9E          Capital Latin with accents X+$20
        // $DF             C3 9F                 German beta ß              already lowercase
        #$C3:
        begin
          case c2 of
          #$80..#$96, #$98..#$9E: new_c2 := chr(ord(c2) + $20)
          end;
        end;
        // $0100..$012F    C4 80..C4 AF        Capital/Small Latin accents  if mod 2 = 0 then X+1
        // $0130..$0131    C4 B0..C4 B1        Turkish
        //  C4 B0 turkish uppercase dotted i -> 'i'
        //  C4 B1 turkish lowercase undotted ı
        // $0132..$0137    C4 B2..C4 B7        Capital/Small Latin accents  if mod 2 = 0 then X+1
        // $0138           C4 B8               ĸ                            N/A
        // $0139..$024F    C4 B9..C5 88        Capital/Small Latin accents  if mod 2 = 1 then X+1
        #$C4:
        begin
          case c2 of
            #$80..#$AF, #$B2..#$B7: if ord(c2) mod 2 = 0 then new_c2 := chr(ord(c2) + 1);
            #$B0: // Turkish
            begin
              OutStr^ := 'i';
              inc(InStr, 2);
              inc(OutStr);
              inc(CounterDiff, 1);
              Continue;
            end;
            #$B9..#$BE: if ord(c2) mod 2 = 1 then new_c2 := chr(ord(c2) + 1);
            #$BF: // This crosses the borders between the first byte of the UTF-8 char
            begin
              new_c1 := #$C5;
              new_c2 := #$80;
            end;
          end;
        end;
        // $C589 ŉ
        // $C58A..$C5B7: if OldChar mod 2 = 0 then NewChar := OldChar + 1;
        // $C5B8:        NewChar := $C3BF; // Ÿ
        // $C5B9..$C8B3: if OldChar mod 2 = 1 then NewChar := OldChar + 1;
        #$C5:
        begin
          case c2 of
            #$8A..#$B7: //0
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$00..#$88, #$B9..#$BE: //1
            begin
              if ord(c2) mod 2 = 1 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$B8:  // Ÿ
            begin
              new_c1 := #$C3;
              new_c2 := #$BF;
            end;
          end;
        end;
        {A convoluted part: C6 80..C6 8F

        0180;LATIN SMALL LETTER B WITH STROKE;Ll;0;L;;;;;N;LATIN SMALL LETTER B BAR;;0243;;0243
        0181;LATIN CAPITAL LETTER B WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER B HOOK;;;0253; => C6 81=>C9 93
        0182;LATIN CAPITAL LETTER B WITH TOPBAR;Lu;0;L;;;;;N;LATIN CAPITAL LETTER B TOPBAR;;;0183;
        0183;LATIN SMALL LETTER B WITH TOPBAR;Ll;0;L;;;;;N;LATIN SMALL LETTER B TOPBAR;;0182;;0182
        0184;LATIN CAPITAL LETTER TONE SIX;Lu;0;L;;;;;N;;;;0185;
        0185;LATIN SMALL LETTER TONE SIX;Ll;0;L;;;;;N;;;0184;;0184
        0186;LATIN CAPITAL LETTER OPEN O;Lu;0;L;;;;;N;;;;0254; ==> C9 94
        0187;LATIN CAPITAL LETTER C WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER C HOOK;;;0188;
        0188;LATIN SMALL LETTER C WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER C HOOK;;0187;;0187
        0189;LATIN CAPITAL LETTER AFRICAN D;Lu;0;L;;;;;N;;;;0256; => C9 96
        018A;LATIN CAPITAL LETTER D WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER D HOOK;;;0257; => C9 97
        018B;LATIN CAPITAL LETTER D WITH TOPBAR;Lu;0;L;;;;;N;LATIN CAPITAL LETTER D TOPBAR;;;018C;
        018C;LATIN SMALL LETTER D WITH TOPBAR;Ll;0;L;;;;;N;LATIN SMALL LETTER D TOPBAR;;018B;;018B
        018D;LATIN SMALL LETTER TURNED DELTA;Ll;0;L;;;;;N;;;;;
        018E;LATIN CAPITAL LETTER REVERSED E;Lu;0;L;;;;;N;LATIN CAPITAL LETTER TURNED E;;;01DD; => C7 9D
        018F;LATIN CAPITAL LETTER SCHWA;Lu;0;L;;;;;N;;;;0259; => C9 99
        }
        #$C6:
        begin
          case c2 of
            #$81:
            begin
              new_c1 := #$C9;
              new_c2 := #$93;
            end;
            #$82..#$85:
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$87..#$88,#$8B..#$8C:
            begin
              if ord(c2) mod 2 = 1 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$86:
            begin
              new_c1 := #$C9;
              new_c2 := #$94;
            end;
            #$89:
            begin
              new_c1 := #$C9;
              new_c2 := #$96;
            end;
            #$8A:
            begin
              new_c1 := #$C9;
              new_c2 := #$97;
            end;
            #$8E:
            begin
              new_c1 := #$C7;
              new_c2 := #$9D;
            end;
            #$8F:
            begin
              new_c1 := #$C9;
              new_c2 := #$99;
            end;
          {
          And also C6 90..C6 9F

          0190;LATIN CAPITAL LETTER OPEN E;Lu;0;L;;;;;N;LATIN CAPITAL LETTER EPSILON;;;025B; => C9 9B
          0191;LATIN CAPITAL LETTER F WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER F HOOK;;;0192; => +1
          0192;LATIN SMALL LETTER F WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER SCRIPT F;;0191;;0191 <=
          0193;LATIN CAPITAL LETTER G WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER G HOOK;;;0260; => C9 A0
          0194;LATIN CAPITAL LETTER GAMMA;Lu;0;L;;;;;N;;;;0263; => C9 A3
          0195;LATIN SMALL LETTER HV;Ll;0;L;;;;;N;LATIN SMALL LETTER H V;;01F6;;01F6 <=
          0196;LATIN CAPITAL LETTER IOTA;Lu;0;L;;;;;N;;;;0269; => C9 A9
          0197;LATIN CAPITAL LETTER I WITH STROKE;Lu;0;L;;;;;N;LATIN CAPITAL LETTER BARRED I;;;0268; => C9 A8
          0198;LATIN CAPITAL LETTER K WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER K HOOK;;;0199; => +1
          0199;LATIN SMALL LETTER K WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER K HOOK;;0198;;0198 <=
          019A;LATIN SMALL LETTER L WITH BAR;Ll;0;L;;;;;N;LATIN SMALL LETTER BARRED L;;023D;;023D <=
          019B;LATIN SMALL LETTER LAMBDA WITH STROKE;Ll;0;L;;;;;N;LATIN SMALL LETTER BARRED LAMBDA;;;; <=
          019C;LATIN CAPITAL LETTER TURNED M;Lu;0;L;;;;;N;;;;026F; => C9 AF
          019D;LATIN CAPITAL LETTER N WITH LEFT HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER N HOOK;;;0272; => C9 B2
          019E;LATIN SMALL LETTER N WITH LONG RIGHT LEG;Ll;0;L;;;;;N;;;0220;;0220 <=
          019F;LATIN CAPITAL LETTER O WITH MIDDLE TILDE;Lu;0;L;;;;;N;LATIN CAPITAL LETTER BARRED O;;;0275; => C9 B5
          }
          #$90:
          begin
            new_c1 := #$C9;
            new_c2 := #$9B;
          end;
          #$91, #$98: new_c2 := chr(ord(c2)+1);
          #$93:
          begin
            new_c1 := #$C9;
            new_c2 := #$A0;
          end;
          #$94:
          begin
            new_c1 := #$C9;
            new_c2 := #$A3;
          end;
          #$96:
          begin
            new_c1 := #$C9;
            new_c2 := #$A9;
          end;
          #$97:
          begin
            new_c1 := #$C9;
            new_c2 := #$A8;
          end;
          #$9C:
          begin
            new_c1 := #$C9;
            new_c2 := #$AF;
          end;
          #$9D:
          begin
            new_c1 := #$C9;
            new_c2 := #$B2;
          end;
          #$9F:
          begin
            new_c1 := #$C9;
            new_c2 := #$B5;
          end;
          {
          And also C6 A0..C6 AF

          01A0;LATIN CAPITAL LETTER O WITH HORN;Lu;0;L;004F 031B;;;;N;LATIN CAPITAL LETTER O HORN;;;01A1; => +1
          01A1;LATIN SMALL LETTER O WITH HORN;Ll;0;L;006F 031B;;;;N;LATIN SMALL LETTER O HORN;;01A0;;01A0 <=
          01A2;LATIN CAPITAL LETTER OI;Lu;0;L;;;;;N;LATIN CAPITAL LETTER O I;;;01A3; => +1
          01A3;LATIN SMALL LETTER OI;Ll;0;L;;;;;N;LATIN SMALL LETTER O I;;01A2;;01A2 <=
          01A4;LATIN CAPITAL LETTER P WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER P HOOK;;;01A5; => +1
          01A5;LATIN SMALL LETTER P WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER P HOOK;;01A4;;01A4 <=
          01A6;LATIN LETTER YR;Lu;0;L;;;;;N;LATIN LETTER Y R;;;0280; => CA 80
          01A7;LATIN CAPITAL LETTER TONE TWO;Lu;0;L;;;;;N;;;;01A8; => +1
          01A8;LATIN SMALL LETTER TONE TWO;Ll;0;L;;;;;N;;;01A7;;01A7 <=
          01A9;LATIN CAPITAL LETTER ESH;Lu;0;L;;;;;N;;;;0283; => CA 83
          01AA;LATIN LETTER REVERSED ESH LOOP;Ll;0;L;;;;;N;;;;;
          01AB;LATIN SMALL LETTER T WITH PALATAL HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER T PALATAL HOOK;;;; <=
          01AC;LATIN CAPITAL LETTER T WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER T HOOK;;;01AD; => +1
          01AD;LATIN SMALL LETTER T WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER T HOOK;;01AC;;01AC <=
          01AE;LATIN CAPITAL LETTER T WITH RETROFLEX HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER T RETROFLEX HOOK;;;0288; => CA 88
          01AF;LATIN CAPITAL LETTER U WITH HORN;Lu;0;L;0055 031B;;;;N;LATIN CAPITAL LETTER U HORN;;;01B0; => +1
          }
          #$A0..#$A5,#$AC:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$A7,#$AF:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$A6:
          begin
            new_c1 := #$CA;
            new_c2 := #$80;
          end;
          #$A9:
          begin
            new_c1 := #$CA;
            new_c2 := #$83;
          end;
          #$AE:
          begin
            new_c1 := #$CA;
            new_c2 := #$88;
          end;
          {
          And also C6 B0..C6 BF

          01B0;LATIN SMALL LETTER U WITH HORN;Ll;0;L;0075 031B;;;;N;LATIN SMALL LETTER U HORN;;01AF;;01AF <= -1
          01B1;LATIN CAPITAL LETTER UPSILON;Lu;0;L;;;;;N;;;;028A; => CA 8A
          01B2;LATIN CAPITAL LETTER V WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER SCRIPT V;;;028B; => CA 8B
          01B3;LATIN CAPITAL LETTER Y WITH HOOK;Lu;0;L;;;;;N;LATIN CAPITAL LETTER Y HOOK;;;01B4; => +1
          01B4;LATIN SMALL LETTER Y WITH HOOK;Ll;0;L;;;;;N;LATIN SMALL LETTER Y HOOK;;01B3;;01B3 <=
          01B5;LATIN CAPITAL LETTER Z WITH STROKE;Lu;0;L;;;;;N;LATIN CAPITAL LETTER Z BAR;;;01B6; => +1
          01B6;LATIN SMALL LETTER Z WITH STROKE;Ll;0;L;;;;;N;LATIN SMALL LETTER Z BAR;;01B5;;01B5 <=
          01B7;LATIN CAPITAL LETTER EZH;Lu;0;L;;;;;N;LATIN CAPITAL LETTER YOGH;;;0292; => CA 92
          01B8;LATIN CAPITAL LETTER EZH REVERSED;Lu;0;L;;;;;N;LATIN CAPITAL LETTER REVERSED YOGH;;;01B9; => +1
          01B9;LATIN SMALL LETTER EZH REVERSED;Ll;0;L;;;;;N;LATIN SMALL LETTER REVERSED YOGH;;01B8;;01B8 <=
          01BA;LATIN SMALL LETTER EZH WITH TAIL;Ll;0;L;;;;;N;LATIN SMALL LETTER YOGH WITH TAIL;;;; <=
          01BB;LATIN LETTER TWO WITH STROKE;Lo;0;L;;;;;N;LATIN LETTER TWO BAR;;;; X
          01BC;LATIN CAPITAL LETTER TONE FIVE;Lu;0;L;;;;;N;;;;01BD; => +1
          01BD;LATIN SMALL LETTER TONE FIVE;Ll;0;L;;;;;N;;;01BC;;01BC <=
          01BE;LATIN LETTER INVERTED GLOTTAL STOP WITH STROKE;Ll;0;L;;;;;N;LATIN LETTER INVERTED GLOTTAL STOP BAR;;;; X
          01BF;LATIN LETTER WYNN;Ll;0;L;;;;;N;;;01F7;;01F7  <=
          }
          #$B8,#$BC:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$B3..#$B6:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$B1:
          begin
            new_c1 := #$CA;
            new_c2 := #$8A;
          end;
          #$B2:
          begin
            new_c1 := #$CA;
            new_c2 := #$8B;
          end;
          #$B7:
          begin
            new_c1 := #$CA;
            new_c2 := #$92;
          end;
          end;
        end;
        #$C7:
        begin
          case c2 of
          #$84..#$8C,#$B1..#$B3:
          begin
            if (ord(c2) and $F) mod 3 = 1 then new_c2 := chr(ord(c2) + 2)
            else if (ord(c2) and $F) mod 3 = 2 then new_c2 := chr(ord(c2) + 1);
          end;
          #$8D..#$9C:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$9E..#$AF,#$B4..#$B5,#$B8..#$BF:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          {
          01F6;LATIN CAPITAL LETTER HWAIR;Lu;0;L;;;;;N;;;;0195;
          01F7;LATIN CAPITAL LETTER WYNN;Lu;0;L;;;;;N;;;;01BF;
          }
          #$B6:
          begin
            new_c1 := #$C6;
            new_c2 := #$95;
          end;
          #$B7:
          begin
            new_c1 := #$C6;
            new_c2 := #$BF;
          end;
          end;
        end;
        {
        Codepoints 0200 to 023F
        }
        #$C8:
        begin
          // For this one we can simply start with a default and override for some specifics
          if (c2 in [#$80..#$B3]) and (ord(c2) mod 2 = 0) then new_c2 := chr(ord(c2) + 1);

          case c2 of
          #$A0:
          begin
            new_c1 := #$C6;
            new_c2 := #$9E;
          end;
          #$A1: new_c2 := c2;
          {
          023A;LATIN CAPITAL LETTER A WITH STROKE;Lu;0;L;;;;;N;;;;2C65; => E2 B1 A5
          023B;LATIN CAPITAL LETTER C WITH STROKE;Lu;0;L;;;;;N;;;;023C; => +1
          023C;LATIN SMALL LETTER C WITH STROKE;Ll;0;L;;;;;N;;;023B;;023B <=
          023D;LATIN CAPITAL LETTER L WITH BAR;Lu;0;L;;;;;N;;;;019A; => C6 9A
          023E;LATIN CAPITAL LETTER T WITH DIAGONAL STROKE;Lu;0;L;;;;;N;;;;2C66; => E2 B1 A6
          023F;LATIN SMALL LETTER S WITH SWASH TAIL;Ll;0;L;;;;;N;;;2C7E;;2C7E <=
          0240;LATIN SMALL LETTER Z WITH SWASH TAIL;Ll;0;L;;;;;N;;;2C7F;;2C7F <=
          }
          #$BA,#$BE:
          begin
            p:= OutStr - PChar(Result);
            SetLength(Result,Length(Result)+1);// Increase the buffer
            OutStr := PChar(Result)+p;
            OutStr^ := #$E2;
            inc(OutStr);
            OutStr^ := #$B1;
            inc(OutStr);
            if c2 = #$BA then OutStr^ := #$A5
            else OutStr^ := #$A6;
            dec(CounterDiff);
            inc(OutStr);
            inc(InStr, 2);
            Continue;
          end;
          #$BD:
          begin
            new_c1 := #$C6;
            new_c2 := #$9A;
          end;
          #$BB: new_c2 := chr(ord(c2) + 1);
          end;
        end;
        {
        Codepoints 0240 to 027F

        Here only 0240..024F needs lowercase
        }
        #$C9:
        begin
          case c2 of
          #$81..#$82:
          begin
            if ord(c2) mod 2 = 1 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$86..#$8F:
          begin
            if ord(c2) mod 2 = 0 then
              new_c2 := chr(ord(c2) + 1);
          end;
          #$83:
          begin
            new_c1 := #$C6;
            new_c2 := #$80;
          end;
          #$84:
          begin
            new_c1 := #$CA;
            new_c2 := #$89;
          end;
          #$85:
          begin
            new_c1 := #$CA;
            new_c2 := #$8C;
          end;
          end;
        end;
        // $CE91..$CE9F: NewChar := OldChar + $20; // Greek Characters
        // $CEA0..$CEA9: NewChar := OldChar + $E0; // Greek Characters
        #$CE:
        begin
          case c2 of
            // 0380 = CE 80
            #$86: new_c2 := #$AC;
            #$88: new_c2 := #$AD;
            #$89: new_c2 := #$AE;
            #$8A: new_c2 := #$AF;
            #$8C: new_c1 := #$CF; // By coincidence new_c2 remains the same
            #$8E:
            begin
              new_c1 := #$CF;
              new_c2 := #$8D;
            end;
            #$8F:
            begin
              new_c1 := #$CF;
              new_c2 := #$8E;
            end;
            // 0390 = CE 90
            #$91..#$9F:
            begin
              new_c2 := chr(ord(c2) + $20);
            end;
            // 03A0 = CE A0
            #$A0..#$AB:
            begin
              new_c1 := #$CF;
              new_c2 := chr(ord(c2) - $20);
            end;
          end;
        end;
        // 03C0 = CF 80
        // 03D0 = CF 90
        // 03E0 = CF A0
        // 03F0 = CF B0
        #$CF:
        begin
          case c2 of
            // 03CF;GREEK CAPITAL KAI SYMBOL;Lu;0;L;;;;;N;;;;03D7; CF 8F => CF 97
            #$8F: new_c2 := #$97;
            // 03D8;GREEK LETTER ARCHAIC KOPPA;Lu;0;L;;;;;N;;;;03D9;
            #$98: new_c2 := #$99;
            // 03DA;GREEK LETTER STIGMA;Lu;0;L;;;;;N;GREEK CAPITAL LETTER STIGMA;;;03DB;
            #$9A: new_c2 := #$9B;
            // 03DC;GREEK LETTER DIGAMMA;Lu;0;L;;;;;N;GREEK CAPITAL LETTER DIGAMMA;;;03DD;
            #$9C: new_c2 := #$9D;
            // 03DE;GREEK LETTER KOPPA;Lu;0;L;;;;;N;GREEK CAPITAL LETTER KOPPA;;;03DF;
            #$9E: new_c2 := #$9F;
            {
            03E0;GREEK LETTER SAMPI;Lu;0;L;;;;;N;GREEK CAPITAL LETTER SAMPI;;;03E1;
            03E1;GREEK SMALL LETTER SAMPI;Ll;0;L;;;;;N;;;03E0;;03E0
            03E2;COPTIC CAPITAL LETTER SHEI;Lu;0;L;;;;;N;GREEK CAPITAL LETTER SHEI;;;03E3;
            03E3;COPTIC SMALL LETTER SHEI;Ll;0;L;;;;;N;GREEK SMALL LETTER SHEI;;03E2;;03E2
            ...
            03EE;COPTIC CAPITAL LETTER DEI;Lu;0;L;;;;;N;GREEK CAPITAL LETTER DEI;;;03EF;
            03EF;COPTIC SMALL LETTER DEI;Ll;0;L;;;;;N;GREEK SMALL LETTER DEI;;03EE;;03EE
            }
            #$A0..#$AF: if ord(c2) mod 2 = 0 then
                          new_c2 := chr(ord(c2) + 1);
            // 03F4;GREEK CAPITAL THETA SYMBOL;Lu;0;L;<compat> 0398;;;;N;;;;03B8;
            #$B4:
            begin
              new_c1 := #$CE;
              new_c2 := #$B8;
            end;
            // 03F7;GREEK CAPITAL LETTER SHO;Lu;0;L;;;;;N;;;;03F8;
            #$B7: new_c2 := #$B8;
            // 03F9;GREEK CAPITAL LUNATE SIGMA SYMBOL;Lu;0;L;<compat> 03A3;;;;N;;;;03F2;
            #$B9: new_c2 := #$B2;
            // 03FA;GREEK CAPITAL LETTER SAN;Lu;0;L;;;;;N;;;;03FB;
            #$BA: new_c2 := #$BB;
            // 03FD;GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL;Lu;0;L;;;;;N;;;;037B;
            #$BD:
            begin
              new_c1 := #$CD;
              new_c2 := #$BB;
            end;
            // 03FE;GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL;Lu;0;L;;;;;N;;;;037C;
            #$BE:
            begin
              new_c1 := #$CD;
              new_c2 := #$BC;
            end;
            // 03FF;GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL;Lu;0;L;;;;;N;;;;037D;
            #$BF:
            begin
              new_c1 := #$CD;
              new_c2 := #$BD;
            end;
          end;
        end;
        // $D080..$D08F: NewChar := OldChar + $110; // Cyrillic alphabet
        // $D090..$D09F: NewChar := OldChar + $20; // Cyrillic alphabet
        // $D0A0..$D0AF: NewChar := OldChar + $E0; // Cyrillic alphabet
        #$D0:
        begin
          c2 := InStr[1];
          case c2 of
            #$80..#$8F:
            begin
              new_c1 := chr(ord(c1)+1);
              new_c2  := chr(ord(c2) + $10);
            end;
            #$90..#$9F:
            begin
              new_c2 := chr(ord(c2) + $20);
            end;
            #$A0..#$AF:
            begin
              new_c1 := chr(ord(c1)+1);
              new_c2 := chr(ord(c2) - $20);
            end;
          end;
        end;
        // Archaic and non-slavic cyrillic 460-47F = D1A0-D1BF
        // These require just adding 1 to get the lowercase
        #$D1:
        begin
          if (c2 in [#$A0..#$BF]) and (ord(c2) mod 2 = 0) then
            new_c2 := chr(ord(c2) + 1);
        end;
        // Archaic and non-slavic cyrillic 480-4BF = D280-D2BF
        // These mostly require just adding 1 to get the lowercase
        #$D2:
        begin
          case c2 of
            #$80:
            begin
              new_c2 := chr(ord(c2) + 1);
            end;
            // #$81 is already lowercase
            // #$82-#$89 ???
            #$8A..#$BF:
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
          end;
        end;
        {
        Codepoints  04C0..04FF
        }
        #$D3:
        begin
          case c2 of
            #$80: new_c2 := #$8F;
            #$81..#$8E:
            begin
              if ord(c2) mod 2 = 1 then
                new_c2 := chr(ord(c2) + 1);
            end;
            #$90..#$BF:
            begin
              if ord(c2) mod 2 = 0 then
                new_c2 := chr(ord(c2) + 1);
            end;
          end;
        end;
        {
        Codepoints  0500..053F

        Armenian starts in 0531
        }
        #$D4:
        begin
          if ord(c2) mod 2 = 0 then
            new_c2 := chr(ord(c2) + 1);

          // Armenian
          if c2 in [#$B1..#$BF] then
          begin
            new_c1 := #$D5;
            new_c2 := chr(ord(c2) - $10);
          end;
        end;
        {
        Codepoints  0540..057F

        Armenian
        }
        #$D5:
        begin
          case c2 of
            #$80..#$8F:
            begin
              new_c2 := chr(ord(c2) + $30);
            end;
            #$90..#$96:
            begin
              new_c1 := #$D6;
              new_c2 := chr(ord(c2) - $10);
            end;
          end;
        end;
        end;
        // Common code 2-byte modifiable chars
        if (CounterDiff <> 0) then
        begin
          OutStr^ := new_c1;
          OutStr[1] := new_c2;
        end
        else
        begin
          if (new_c1 <> c1) then OutStr^ := new_c1;
          if (new_c2 <> c2) then OutStr[1] := new_c2;
        end;
        inc(InStr, 2);
        inc(OutStr, 2);
      end;
      {
      Characters with 3 bytes
      }
      #$E1:
      begin
        new_c1 := c1;
        c2 := InStr[1];
        c3 := InStr[2];
        new_c2 := c2;
        new_c3 := c3;
        {
        Georgian codepoints 10A0-10C5 => 2D00-2D25

        In UTF-8 this is:
        E1 82 A0 - E1 82 BF => E2 B4 80 - E2 B4 9F
        E1 83 80 - E1 83 85 => E2 B4 A0 - E2 B4 A5
        }
        case c2 of
        #$82:
        if (c3 in [#$A0..#$BF]) then
        begin
          new_c1 := #$E2;
          new_c2 := #$B4;
          new_c3 := chr(ord(c3) - $20);
        end;
        #$83:
        if (c3 in [#$80..#$85]) then
        begin
          new_c1 := #$E2;
          new_c2 := #$B4;
          new_c3 := chr(ord(c3) + $20);
        end;
        {
        Extra chars between 1E00..1EFF

        Blocks of chars:
          1E00..1E3F    E1 B8 80..E1 B8 BF
          1E40..1E7F    E1 B9 80..E1 B9 BF
          1E80..1EBF    E1 BA 80..E1 BA BF
          1EC0..1EFF    E1 BB 80..E1 BB BF
        }
        #$B8..#$BB:
        begin
          // Start with a default and change for some particular chars
          if ord(c3) mod 2 = 0 then
            new_c3 := chr(ord(c3) + 1);

          { Only 1E96..1E9F are different E1 BA 96..E1 BA 9F

          1E96;LATIN SMALL LETTER H WITH LINE BELOW;Ll;0;L;0068 0331;;;;N;;;;;
          1E97;LATIN SMALL LETTER T WITH DIAERESIS;Ll;0;L;0074 0308;;;;N;;;;;
          1E98;LATIN SMALL LETTER W WITH RING ABOVE;Ll;0;L;0077 030A;;;;N;;;;;
          1E99;LATIN SMALL LETTER Y WITH RING ABOVE;Ll;0;L;0079 030A;;;;N;;;;;
          1E9A;LATIN SMALL LETTER A WITH RIGHT HALF RING;Ll;0;L;<compat> 0061 02BE;;;;N;;;;;
          1E9B;LATIN SMALL LETTER LONG S WITH DOT ABOVE;Ll;0;L;017F 0307;;;;N;;;1E60;;1E60
          1E9C;LATIN SMALL LETTER LONG S WITH DIAGONAL STROKE;Ll;0;L;;;;;N;;;;;
          1E9D;LATIN SMALL LETTER LONG S WITH HIGH STROKE;Ll;0;L;;;;;N;;;;;
          1E9E;LATIN CAPITAL LETTER SHARP S;Lu;0;L;;;;;N;;;;00DF; => C3 9F
          1E9F;LATIN SMALL LETTER DELTA;Ll;0;L;;;;;N;;;;;
          }
          if (c2 = #$BA) and (c3 in [#$96..#$9F]) then new_c3 := c3;
          // LATIN CAPITAL LETTER SHARP S => to german Beta
          if (c2 = #$BA) and (c3 = #$9E) then
          begin
            inc(InStr, 3);
            OutStr^ := #$C3;
            inc(OutStr);
            OutStr^ := #$9F;
            inc(OutStr);
            inc(CounterDiff, 1);
            Continue;
          end;
        end;
        {
        Extra chars between 1F00..1FFF

        Blocks of chars:
          1E00..1E3F    E1 BC 80..E1 BC BF
          1E40..1E7F    E1 BD 80..E1 BD BF
          1E80..1EBF    E1 BE 80..E1 BE BF
          1EC0..1EFF    E1 BF 80..E1 BF BF
        }
        #$BC:
        begin
          // Start with a default and change for some particular chars
          if (ord(c3) mod $10) div 8 = 1 then
            new_c3 := chr(ord(c3) - 8);
        end;
        #$BD:
        begin
          // Start with a default and change for some particular chars
          case c3 of
          #$80..#$8F, #$A0..#$AF: if (ord(c3) mod $10) div 8 = 1 then
                        new_c3 := chr(ord(c3) - 8);
          {
          1F50;GREEK SMALL LETTER UPSILON WITH PSILI;Ll;0;L;03C5 0313;;;;N;;;;;
          1F51;GREEK SMALL LETTER UPSILON WITH DASIA;Ll;0;L;03C5 0314;;;;N;;;1F59;;1F59
          1F52;GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA;Ll;0;L;1F50 0300;;;;N;;;;;
          1F53;GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA;Ll;0;L;1F51 0300;;;;N;;;1F5B;;1F5B
          1F54;GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA;Ll;0;L;1F50 0301;;;;N;;;;;
          1F55;GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA;Ll;0;L;1F51 0301;;;;N;;;1F5D;;1F5D
          1F56;GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI;Ll;0;L;1F50 0342;;;;N;;;;;
          1F57;GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI;Ll;0;L;1F51 0342;;;;N;;;1F5F;;1F5F
          1F59;GREEK CAPITAL LETTER UPSILON WITH DASIA;Lu;0;L;03A5 0314;;;;N;;;;1F51;
          1F5B;GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA;Lu;0;L;1F59 0300;;;;N;;;;1F53;
          1F5D;GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA;Lu;0;L;1F59 0301;;;;N;;;;1F55;
          1F5F;GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI;Lu;0;L;1F59 0342;;;;N;;;;1F57;
          }
          #$99,#$9B,#$9D,#$9F: new_c3 := chr(ord(c3) - 8);
          end;
        end;
        #$BE:
        begin
          // Start with a default and change for some particular chars
          case c3 of
          #$80..#$B9: if (ord(c3) mod $10) div 8 = 1 then
                        new_c3 := chr(ord(c3) - 8);
          {
          1FB0;GREEK SMALL LETTER ALPHA WITH VRACHY;Ll;0;L;03B1 0306;;;;N;;;1FB8;;1FB8
          1FB1;GREEK SMALL LETTER ALPHA WITH MACRON;Ll;0;L;03B1 0304;;;;N;;;1FB9;;1FB9
          1FB2;GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI;Ll;0;L;1F70 0345;;;;N;;;;;
          1FB3;GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI;Ll;0;L;03B1 0345;;;;N;;;1FBC;;1FBC
          1FB4;GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI;Ll;0;L;03AC 0345;;;;N;;;;;
          1FB6;GREEK SMALL LETTER ALPHA WITH PERISPOMENI;Ll;0;L;03B1 0342;;;;N;;;;;
          1FB7;GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI;Ll;0;L;1FB6 0345;;;;N;;;;;
          1FB8;GREEK CAPITAL LETTER ALPHA WITH VRACHY;Lu;0;L;0391 0306;;;;N;;;;1FB0;
          1FB9;GREEK CAPITAL LETTER ALPHA WITH MACRON;Lu;0;L;0391 0304;;;;N;;;;1FB1;
          1FBA;GREEK CAPITAL LETTER ALPHA WITH VARIA;Lu;0;L;0391 0300;;;;N;;;;1F70;
          1FBB;GREEK CAPITAL LETTER ALPHA WITH OXIA;Lu;0;L;0386;;;;N;;;;1F71;
          1FBC;GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI;Lt;0;L;0391 0345;;;;N;;;;1FB3;
          1FBD;GREEK KORONIS;Sk;0;ON;<compat> 0020 0313;;;;N;;;;;
          1FBE;GREEK PROSGEGRAMMENI;Ll;0;L;03B9;;;;N;;;0399;;0399
          1FBF;GREEK PSILI;Sk;0;ON;<compat> 0020 0313;;;;N;;;;;
          }
          #$BA:
          begin
            new_c2 := #$BD;
            new_c3 := #$B0;
          end;
          #$BB:
          begin
            new_c2 := #$BD;
            new_c3 := #$B1;
          end;
          #$BC: new_c3 := #$B3;
          end;
        end;
        end;

        if (CounterDiff <> 0) then
        begin
          OutStr^ := new_c1;
          OutStr[1] := new_c2;
          OutStr[2] := new_c3;
        end
        else
        begin
          if c1 <> new_c1 then OutStr^ := new_c1;
          if c2 <> new_c2 then OutStr[1] := new_c2;
          if c3 <> new_c3 then OutStr[2] := new_c3;
        end;

        inc(InStr, 3);
        inc(OutStr, 3);
      end;
      {
      More Characters with 3 bytes, so exotic stuff between:
      $2126..$2183                    E2 84 A6..E2 86 83
      $24B6..$24CF    Result:=u+26;   E2 92 B6..E2 93 8F
      $2C00..$2C2E    Result:=u+48;   E2 B0 80..E2 B0 AE
      $2C60..$2CE2                    E2 B1 A0..E2 B3 A2
      }
      #$E2:
      begin
        new_c1 := c1;
        c2 := InStr[1];
        c3 := InStr[2];
        new_c2 := c2;
        new_c3 := c3;
        // 2126;OHM SIGN;Lu;0;L;03A9;;;;N;OHM;;;03C9; E2 84 A6 => CF 89
        if (c2 = #$84) and (c3 = #$A6) then
        begin
          inc(InStr, 3);
          OutStr^ := #$CF;
          inc(OutStr);
          OutStr^ := #$89;
          inc(OutStr);
          inc(CounterDiff, 1);
          Continue;
        end
        {
        212A;KELVIN SIGN;Lu;0;L;004B;;;;N;DEGREES KELVIN;;;006B; E2 84 AA => 6B
        }
        else if (c2 = #$84) and (c3 = #$AA) then
        begin
          inc(InStr, 3);
          if c3 = #$AA then OutStr^ := #$6B
          else OutStr^ := #$E5;
          inc(OutStr);
          inc(CounterDiff, 2);
          Continue;
        end
        {
        212B;ANGSTROM SIGN;Lu;0;L;00C5;;;;N;ANGSTROM UNIT;;;00E5; E2 84 AB => C3 A5
        }
        else if (c2 = #$84) and (c3 = #$AB) then
        begin
          inc(InStr, 3);
          OutStr^ := #$C3;
          inc(OutStr);
          OutStr^ := #$A5;
          inc(OutStr);
          inc(CounterDiff, 1);
          Continue;
        end
        {
        2160;ROMAN NUMERAL ONE;Nl;0;L;<compat> 0049;;;1;N;;;;2170; E2 85 A0 => E2 85 B0
        2161;ROMAN NUMERAL TWO;Nl;0;L;<compat> 0049 0049;;;2;N;;;;2171;
        2162;ROMAN NUMERAL THREE;Nl;0;L;<compat> 0049 0049 0049;;;3;N;;;;2172;
        2163;ROMAN NUMERAL FOUR;Nl;0;L;<compat> 0049 0056;;;4;N;;;;2173;
        2164;ROMAN NUMERAL FIVE;Nl;0;L;<compat> 0056;;;5;N;;;;2174;
        2165;ROMAN NUMERAL SIX;Nl;0;L;<compat> 0056 0049;;;6;N;;;;2175;
        2166;ROMAN NUMERAL SEVEN;Nl;0;L;<compat> 0056 0049 0049;;;7;N;;;;2176;
        2167;ROMAN NUMERAL EIGHT;Nl;0;L;<compat> 0056 0049 0049 0049;;;8;N;;;;2177;
        2168;ROMAN NUMERAL NINE;Nl;0;L;<compat> 0049 0058;;;9;N;;;;2178;
        2169;ROMAN NUMERAL TEN;Nl;0;L;<compat> 0058;;;10;N;;;;2179;
        216A;ROMAN NUMERAL ELEVEN;Nl;0;L;<compat> 0058 0049;;;11;N;;;;217A;
        216B;ROMAN NUMERAL TWELVE;Nl;0;L;<compat> 0058 0049 0049;;;12;N;;;;217B;
        216C;ROMAN NUMERAL FIFTY;Nl;0;L;<compat> 004C;;;50;N;;;;217C;
        216D;ROMAN NUMERAL ONE HUNDRED;Nl;0;L;<compat> 0043;;;100;N;;;;217D;
        216E;ROMAN NUMERAL FIVE HUNDRED;Nl;0;L;<compat> 0044;;;500;N;;;;217E;
        216F;ROMAN NUMERAL ONE THOUSAND;Nl;0;L;<compat> 004D;;;1000;N;;;;217F;
        }
        else if (c2 = #$85) and (c3 in [#$A0..#$AF]) then new_c3 := chr(ord(c3) + $10)
        {
        2183;ROMAN NUMERAL REVERSED ONE HUNDRED;Lu;0;L;;;;;N;;;;2184; E2 86 83 => E2 86 84
        }
        else if (c2 = #$86) and (c3 = #$83) then new_c3 := chr(ord(c3) + 1)
        {
        $24B6..$24CF    Result:=u+26;   E2 92 B6..E2 93 8F

        Ex: 24B6;CIRCLED LATIN CAPITAL LETTER A;So;0;L;<circle> 0041;;;;N;;;;24D0; E2 92 B6 => E2 93 90
        }
        else if (c2 = #$92) and (c3 in [#$B6..#$BF]) then
        begin
          new_c3 := #$93;
          new_c3 := chr(ord(c3) - $26);
        end
        else if (c2 = #$93) and (c3 in [#$80..#$8F]) then new_c3 := chr(ord(c3) + 26)
        {
        $2C00..$2C2E    Result:=u+48;   E2 B0 80..E2 B0 AE

        2C00;GLAGOLITIC CAPITAL LETTER AZU;Lu;0;L;;;;;N;;;;2C30; E2 B0 80 => E2 B0 B0

        2C10;GLAGOLITIC CAPITAL LETTER NASHI;Lu;0;L;;;;;N;;;;2C40; E2 B0 90 => E2 B1 80
        }
        else if (c2 = #$B0) and (c3 in [#$80..#$8F]) then new_c3 := chr(ord(c3) + $30)
        else if (c2 = #$B0) and (c3 in [#$90..#$AE]) then
        begin
          new_c2 := #$B1;
          new_c3 := chr(ord(c3) - $10);
        end
        {
        $2C60..$2CE2                    E2 B1 A0..E2 B3 A2

        2C60;LATIN CAPITAL LETTER L WITH DOUBLE BAR;Lu;0;L;;;;;N;;;;2C61; E2 B1 A0 => +1
        2C61;LATIN SMALL LETTER L WITH DOUBLE BAR;Ll;0;L;;;;;N;;;2C60;;2C60
        2C62;LATIN CAPITAL LETTER L WITH MIDDLE TILDE;Lu;0;L;;;;;N;;;;026B; => 	C9 AB
        2C63;LATIN CAPITAL LETTER P WITH STROKE;Lu;0;L;;;;;N;;;;1D7D; => E1 B5 BD
        2C64;LATIN CAPITAL LETTER R WITH TAIL;Lu;0;L;;;;;N;;;;027D; => 	C9 BD
        2C65;LATIN SMALL LETTER A WITH STROKE;Ll;0;L;;;;;N;;;023A;;023A
        2C66;LATIN SMALL LETTER T WITH DIAGONAL STROKE;Ll;0;L;;;;;N;;;023E;;023E
        2C67;LATIN CAPITAL LETTER H WITH DESCENDER;Lu;0;L;;;;;N;;;;2C68; => E2 B1 A8
        2C68;LATIN SMALL LETTER H WITH DESCENDER;Ll;0;L;;;;;N;;;2C67;;2C67
        2C69;LATIN CAPITAL LETTER K WITH DESCENDER;Lu;0;L;;;;;N;;;;2C6A; => E2 B1 AA
        2C6A;LATIN SMALL LETTER K WITH DESCENDER;Ll;0;L;;;;;N;;;2C69;;2C69
        2C6B;LATIN CAPITAL LETTER Z WITH DESCENDER;Lu;0;L;;;;;N;;;;2C6C; => E2 B1 AC
        2C6C;LATIN SMALL LETTER Z WITH DESCENDER;Ll;0;L;;;;;N;;;2C6B;;2C6B
        2C6D;LATIN CAPITAL LETTER ALPHA;Lu;0;L;;;;;N;;;;0251; => C9 91
        2C6E;LATIN CAPITAL LETTER M WITH HOOK;Lu;0;L;;;;;N;;;;0271; => C9 B1
        2C6F;LATIN CAPITAL LETTER TURNED A;Lu;0;L;;;;;N;;;;0250; => C9 90

        2C70;LATIN CAPITAL LETTER TURNED ALPHA;Lu;0;L;;;;;N;;;;0252; => C9 92
        }
        else if (c2 = #$B1) then
        begin
          case c3 of
          #$A0: new_c3 := chr(ord(c3)+1);
          #$A2,#$A4,#$AD..#$AF,#$B0:
          begin
            inc(InStr, 3);
            OutStr^ := #$C9;
            inc(OutStr);
            case c3 of
            #$A2: OutStr^ := #$AB;
            #$A4: OutStr^ := #$BD;
            #$AD: OutStr^ := #$91;
            #$AE: OutStr^ := #$B1;
            #$AF: OutStr^ := #$90;
            #$B0: OutStr^ := #$92;
            end;
            inc(OutStr);
            inc(CounterDiff, 1);
            Continue;
          end;
          #$A3:
          begin
            new_c2 := #$B5;
            new_c3 := #$BD;
          end;
          #$A7,#$A9,#$AB: new_c3 := chr(ord(c3)+1);
          {
          2C71;LATIN SMALL LETTER V WITH RIGHT HOOK;Ll;0;L;;;;;N;;;;;
          2C72;LATIN CAPITAL LETTER W WITH HOOK;Lu;0;L;;;;;N;;;;2C73;
          2C73;LATIN SMALL LETTER W WITH HOOK;Ll;0;L;;;;;N;;;2C72;;2C72
          2C74;LATIN SMALL LETTER V WITH CURL;Ll;0;L;;;;;N;;;;;
          2C75;LATIN CAPITAL LETTER HALF H;Lu;0;L;;;;;N;;;;2C76;
          2C76;LATIN SMALL LETTER HALF H;Ll;0;L;;;;;N;;;2C75;;2C75
          2C77;LATIN SMALL LETTER TAILLESS PHI;Ll;0;L;;;;;N;;;;;
          2C78;LATIN SMALL LETTER E WITH NOTCH;Ll;0;L;;;;;N;;;;;
          2C79;LATIN SMALL LETTER TURNED R WITH TAIL;Ll;0;L;;;;;N;;;;;
          2C7A;LATIN SMALL LETTER O WITH LOW RING INSIDE;Ll;0;L;;;;;N;;;;;
          2C7B;LATIN LETTER SMALL CAPITAL TURNED E;Ll;0;L;;;;;N;;;;;
          2C7C;LATIN SUBSCRIPT SMALL LETTER J;Ll;0;L;<sub> 006A;;;;N;;;;;
          2C7D;MODIFIER LETTER CAPITAL V;Lm;0;L;<super> 0056;;;;N;;;;;
          2C7E;LATIN CAPITAL LETTER S WITH SWASH TAIL;Lu;0;L;;;;;N;;;;023F; => C8 BF
          2C7F;LATIN CAPITAL LETTER Z WITH SWASH TAIL;Lu;0;L;;;;;N;;;;0240; => C9 80
          }
          #$B2,#$B5: new_c3 := chr(ord(c3)+1);
          #$BE,#$BF:
          begin
            inc(InStr, 3);
            case c3 of
            #$BE: OutStr^ := #$C8;
            #$BF: OutStr^ := #$C9;
            end;
            OutStr^ := #$C8;
            inc(OutStr);
            case c3 of
            #$BE: OutStr^ := #$BF;
            #$BF: OutStr^ := #$80;
            end;
            inc(OutStr);
            inc(CounterDiff, 1);
            Continue;
          end;
          end;
        end
        {
        2C80;COPTIC CAPITAL LETTER ALFA;Lu;0;L;;;;;N;;;;2C81; E2 B2 80 => E2 B2 81
        ...
        2CBE;COPTIC CAPITAL LETTER OLD COPTIC OOU;Lu;0;L;;;;;N;;;;2CBF; E2 B2 BE => E2 B2 BF
        2CBF;COPTIC SMALL LETTER OLD COPTIC OOU;Ll;0;L;;;;;N;;;2CBE;;2CBE
        ...
        2CC0;COPTIC CAPITAL LETTER SAMPI;Lu;0;L;;;;;N;;;;2CC1; E2 B3 80 => E2 B2 81
        2CC1;COPTIC SMALL LETTER SAMPI;Ll;0;L;;;;;N;;;2CC0;;2CC0
        ...
        2CE2;COPTIC CAPITAL LETTER OLD NUBIAN WAU;Lu;0;L;;;;;N;;;;2CE3; E2 B3 A2 => E2 B3 A3
        2CE3;COPTIC SMALL LETTER OLD NUBIAN WAU;Ll;0;L;;;;;N;;;2CE2;;2CE2 <=
        }
        else if (c2 = #$B2) then
        begin
          if ord(c3) mod 2 = 0 then new_c3 := chr(ord(c3) + 1);
        end
        else if (c2 = #$B3) and (c3 in [#$80..#$A3]) then
        begin
          if ord(c3) mod 2 = 0 then new_c3 := chr(ord(c3) + 1);
        end;

        if (CounterDiff <> 0) then
        begin
          OutStr^ := new_c1;
          OutStr[1] := new_c2;
          OutStr[2] := new_c3;
        end
        else
        begin
          if c1 <> new_c1 then OutStr^ := new_c1;
          if c2 <> new_c2 then OutStr[1] := new_c2;
          if c3 <> new_c3 then OutStr[2] := new_c3;
        end;

        inc(InStr, 3);
        inc(OutStr, 3);
      end;
      {
      FF21;FULLWIDTH LATIN CAPITAL LETTER A;Lu;0;L;<wide> 0041;;;;N;;;;FF41; EF BC A1 => EF BD 81
      ...
      FF3A;FULLWIDTH LATIN CAPITAL LETTER Z;Lu;0;L;<wide> 005A;;;;N;;;;FF5A; EF BC BA => EF BD 9A
      }
      #$EF:
      begin
        c2 := InStr[1];
        c3 := InStr[2];

        if (c2 = #$BC) and (c3 in [#$A1..#$BA]) then
        begin
          OutStr^ := c1;
          OutStr[1] := #$BD;
          OutStr[2] := chr(ord(c3) - $20);
        end;

        if (CounterDiff <> 0) then
        begin
          OutStr^ := c1;
          OutStr[1] := c2;
          OutStr[2] := c3;
        end;

        inc(InStr, 3);
        inc(OutStr, 3);
      end;
    else
      // Copy the character if the string was disaligned by previous changes
      if (CounterDiff <> 0) then OutStr^:= c1;
      inc(InStr);
      inc(OutStr);
    end; // Case InStr^
  end; // while

  // Final correction of the buffer size
  SetLength(Result,OutStr - PChar(Result));
end;

function UTF8LowerString(const s: string): string;
begin
  Result:=UTF8LowerCase(s);
end;


{
  AInStr - The input string
  ALanguage - The language. Use '' for maximum speed if one desires to ignore the language
              The language should be specified in the format from ISO 639-1,
              which uses 2 characters to represent each language.
              If the language has no code in ISO 639-1, then the 3-chars code
              from ISO 639-2 should be used.
              Example: "tr" - Turkish language locale

  Data from here: ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt

  The columns in the file UnicodeData.txt are explained here:
  http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#Case Mappings
}
function UTF8UpperCase(const AInStr: string; ALanguage: string=''): string;
var
  i, InCounter, OutCounter: PtrInt;
  OutStr: PChar;
  CharLen: integer;
  CharProcessed: Boolean;
  NewCharLen: integer;
  NewChar, OldChar: Word;
  // Language identification
  IsTurkish: Boolean;

  procedure CorrectOutStrSize(AOldCharSize, ANewCharSize: Integer);
  begin
    if not (ANewCharSize > AOldCharSize) then Exit; // no correction needed
    if (ANewCharSize > 20) or (AOldCharSize > 20) then Exit; // sanity check
    // Fix for bug 23428
    // If the string wasn't decreased by previous char changes,
    // and our current operation will make it bigger, then for safety
    // increase the buffer
    if (ANewCharSize > AOldCharSize) and (OutCounter >= InCounter-1) then
    begin
      SetLength(Result, Length(Result)+ANewCharSize-AOldCharSize);
      OutStr := PChar(Result);
    end;
  end;

begin
  // Start with the same string, and progressively modify
  Result:=AInStr;
  UniqueString(Result);
  OutStr := PChar(Result);

  // Language identification
  IsTurkish := (ALanguage = 'tr') or (ALanguage = 'az'); // Turkish and Azeri have a special handling

  InCounter:=1; // for AInStr
  OutCounter := 0; // for Result
  while InCounter<=length(AInStr) do
  begin
    { First ASCII chars }
    if (AInStr[InCounter] <= 'z') and (AInStr[InCounter] >= 'a') then
    begin
      // Special turkish handling
      // small dotted i to capital dotted i
      if IsTurkish and (AInStr[InCounter] = 'i') then
      begin
        SetLength(Result,Length(Result)+1);// Increase the buffer
        OutStr := PChar(Result);
        OutStr[OutCounter]:=#$C4;
        OutStr[OutCounter+1]:=#$B0;
        inc(InCounter);
        inc(OutCounter,2);
      end
      else
      begin
        OutStr[OutCounter]:=chr(ord(AInStr[InCounter])-32);
        inc(InCounter);
        inc(OutCounter);
      end;
    end
    { Now everything else }
    else
    begin
      CharLen := UTF8CharacterLength(@AInStr[InCounter]);
      CharProcessed := False;
      NewCharLen := CharLen;

      if CharLen = 2 then
      begin
        OldChar := (Ord(AInStr[InCounter]) shl 8) or Ord(AInStr[InCounter+1]);
        NewChar := 0;

        // Major processing
        case OldChar of
        // Latin Characters 0000–0FFF http://en.wikibooks.org/wiki/Unicode/Character_reference/0000-0FFF
        $C39F:        NewChar := $5353; // ß => SS
        $C3A0..$C3B6,$C3B8..$C3BE: NewChar := OldChar - $20;
        $C3BF:        NewChar := $C5B8; // ÿ
        $C481..$C4B0: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0130 = C4 B0
        // turkish small undotted i to capital undotted i
        $C4B1:
        begin
          OutStr[OutCounter]:='I';
          NewCharLen := 1;
          CharProcessed := True;
        end;
        $C4B2..$C4B7: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // $C4B8: ĸ without upper/lower
        $C4B9..$C4BF: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C580: NewChar := $C4BF; // border between bytes
        $C581..$C588: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        // $C589 ŉ => ?
        $C58A..$C5B7: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // $C5B8: // Ÿ already uppercase
        $C5B9..$C5BE: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C5BF: // 017F
        begin
          OutStr[OutCounter]:='S';
          NewCharLen := 1;
          CharProcessed := True;
        end;
        // 0180 = C6 80 -> A convoluted part
        $C680: NewChar := $C983;
        $C682..$C685: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        $C688: NewChar := $C687;
        $C68C: NewChar := $C68B;
        // 0190 = C6 90 -> A convoluted part
        $C692: NewChar := $C691;
        $C695: NewChar := $C7B6;
        $C699: NewChar := $C698;
        $C69A: NewChar := $C8BD;
        $C69E: NewChar := $C8A0;
        // 01A0 = C6 A0 -> A convoluted part
        $C6A0..$C6A5: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        $C6A8: NewChar := $C6A7;
        $C6AD: NewChar := $C6AC;
        // 01B0 = C6 B0
        $C6B0: NewChar := $C6AF;
        $C6B3..$C6B6: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C6B9: NewChar := $C6B8;
        $C6BD: NewChar := $C6BC;
        $C6BF: NewChar := $C7B7;
        // 01C0 = C7 80
        $C784..$C786: NewChar := $C784;
        $C787..$C789: NewChar := $C787;
        $C78A..$C78C: NewChar := $C78A;
        $C78E: NewChar := $C78D;
        // 01D0 = C7 90
        $C790: NewChar := $C78F;
        $C791..$C79C: if OldChar mod 2 = 0 then NewChar := OldChar - 1;
        $C79D: NewChar := $C68E;
        $C79F: NewChar := $C79E;
        // 01E0 = C7 A0
        $C7A0..$C7AF: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 01F0 = C7 B0
        $C7B2..$C7B3: NewChar := $C7B1;
        $C7B5: NewChar := $C7B4;
        $C7B8..$C7BF: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0200 = C8 80
        // 0210 = C8 90
        $C880..$C89F: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0220 = C8 A0
        // 0230 = C8 B0
        $C8A2..$C8B3: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        $C8BC: NewChar := $C8BB;
        $C8BF:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$BE;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        // 0240 = C9 80
        $C980:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$BF;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C982: NewChar := $C981;
        $C986..$C98F: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 0250 = C9 90
        $C990:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$AF;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C991:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$AD;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C992:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$B0;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C993: NewChar := $C681;
        $C994: NewChar := $C686;
        $C996: NewChar := $C689;
        $C997: NewChar := $C68A;
        $C999: NewChar := $C68F;
        $C99B: NewChar := $C690;
        // 0260 = C9 A0
        $C9A0: NewChar := $C693;
        $C9A3: NewChar := $C694;
        $C9A5:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$EA;
          OutStr[OutCounter+1]:= #$9E;
          OutStr[OutCounter+2]:= #$8D;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C9A8: NewChar := $C697;
        $C9A9: NewChar := $C696;
        $C9AB:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$A2;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C9AF: NewChar := $C69C;
        // 0270 = C9 B0
        $C9B1:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$AE;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        $C9B2: NewChar := $C69D;
        $C9B5: NewChar := $C69F;
        $C9BD:
        begin
          CorrectOutStrSize(2, 3);
          OutStr[OutCounter]  := #$E2;
          OutStr[OutCounter+1]:= #$B1;
          OutStr[OutCounter+2]:= #$A4;
          NewCharLen := 3;
          CharProcessed := True;
        end;
        // 0280 = CA 80
        $CA80: NewChar := $C6A6;
        $CA83: NewChar := $C6A9;
        $CA88: NewChar := $C6AE;
        $CA89: NewChar := $C984;
        $CA8A: NewChar := $C6B1;
        $CA8B: NewChar := $C6B2;
        $CA8C: NewChar := $C985;
        // 0290 = CA 90
        $CA92: NewChar := $C6B7;
        {
        03A0 = CE A0

        03AC;GREEK SMALL LETTER ALPHA WITH TONOS;Ll;0;L;03B1 0301;;;;N;GREEK SMALL LETTER ALPHA TONOS;;0386;;0386
        03AD;GREEK SMALL LETTER EPSILON WITH TONOS;Ll;0;L;03B5 0301;;;;N;GREEK SMALL LETTER EPSILON TONOS;;0388;;0388
        03AE;GREEK SMALL LETTER ETA WITH TONOS;Ll;0;L;03B7 0301;;;;N;GREEK SMALL LETTER ETA TONOS;;0389;;0389
        03AF;GREEK SMALL LETTER IOTA WITH TONOS;Ll;0;L;03B9 0301;;;;N;GREEK SMALL LETTER IOTA TONOS;;038A;;038A
        }
        $CEAC: NewChar := $CE86;
        $CEAD: NewChar := $CE88;
        $CEAE: NewChar := $CE89;
        $CEAF: NewChar := $CE8A;
        {
        03B0 = CE B0

        03B0;GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS;Ll;0;L;03CB 0301;;;;N;GREEK SMALL LETTER UPSILON DIAERESIS TONOS;;;;
        03B1;GREEK SMALL LETTER ALPHA;Ll;0;L;;;;;N;;;0391;;0391
        ...
        03BF;GREEK SMALL LETTER OMICRON;Ll;0;L;;;;;N;;;039F;;039F
        }
        $CEB1..$CEBF: NewChar := OldChar - $20; // Greek Characters
        {
        03C0 = CF 80

        03C0;GREEK SMALL LETTER PI;Ll;0;L;;;;;N;;;03A0;;03A0 CF 80 => CE A0
        03C1;GREEK SMALL LETTER RHO;Ll;0;L;;;;;N;;;03A1;;03A1
        03C2;GREEK SMALL LETTER FINAL SIGMA;Ll;0;L;;;;;N;;;03A3;;03A3
        03C3;GREEK SMALL LETTER SIGMA;Ll;0;L;;;;;N;;;03A3;;03A3
        03C4;GREEK SMALL LETTER TAU;Ll;0;L;;;;;N;;;03A4;;03A4
        ....
        03CB;GREEK SMALL LETTER UPSILON WITH DIALYTIKA;Ll;0;L;03C5 0308;;;;N;GREEK SMALL LETTER UPSILON DIAERESIS;;03AB;;03AB
        03CC;GREEK SMALL LETTER OMICRON WITH TONOS;Ll;0;L;03BF 0301;;;;N;GREEK SMALL LETTER OMICRON TONOS;;038C;;038C
        03CD;GREEK SMALL LETTER UPSILON WITH TONOS;Ll;0;L;03C5 0301;;;;N;GREEK SMALL LETTER UPSILON TONOS;;038E;;038E
        03CE;GREEK SMALL LETTER OMEGA WITH TONOS;Ll;0;L;03C9 0301;;;;N;GREEK SMALL LETTER OMEGA TONOS;;038F;;038F
        03CF;GREEK CAPITAL KAI SYMBOL;Lu;0;L;;;;;N;;;;03D7;
        }
        $CF80,$CF81,$CF83..$CF8B: NewChar := OldChar - $E0; // Greek Characters
        $CF82: NewChar := $CEA3;
        $CF8C: NewChar := $CE8C;
        $CF8D: NewChar := $CE8E;
        $CF8E: NewChar := $CE8F;
        {
        03D0 = CF 90

        03D0;GREEK BETA SYMBOL;Ll;0;L;<compat> 03B2;;;;N;GREEK SMALL LETTER CURLED BETA;;0392;;0392 CF 90 => CE 92
        03D1;GREEK THETA SYMBOL;Ll;0;L;<compat> 03B8;;;;N;GREEK SMALL LETTER SCRIPT THETA;;0398;;0398 => CE 98
        03D5;GREEK PHI SYMBOL;Ll;0;L;<compat> 03C6;;;;N;GREEK SMALL LETTER SCRIPT PHI;;03A6;;03A6 => CE A6
        03D6;GREEK PI SYMBOL;Ll;0;L;<compat> 03C0;;;;N;GREEK SMALL LETTER OMEGA PI;;03A0;;03A0 => CE A0
        03D7;GREEK KAI SYMBOL;Ll;0;L;;;;;N;;;03CF;;03CF => CF 8F
        03D9;GREEK SMALL LETTER ARCHAIC KOPPA;Ll;0;L;;;;;N;;;03D8;;03D8
        03DB;GREEK SMALL LETTER STIGMA;Ll;0;L;;;;;N;;;03DA;;03DA
        03DD;GREEK SMALL LETTER DIGAMMA;Ll;0;L;;;;;N;;;03DC;;03DC
        03DF;GREEK SMALL LETTER KOPPA;Ll;0;L;;;;;N;;;03DE;;03DE
        }
        $CF90: NewChar := $CE92;
        $CF91: NewChar := $CE98;
        $CF95: NewChar := $CEA6;
        $CF96: NewChar := $CEA0;
        $CF97: NewChar := $CF8F;
        $CF99..$CF9F: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        // 03E0 = CF A0
        $CFA0..$CFAF: if OldChar mod 2 = 1 then NewChar := OldChar - 1;
        {
        03F0 = CF B0

        03F0;GREEK KAPPA SYMBOL;Ll;0;L;<compat> 03BA;;;;N;GREEK SMALL LETTER SCRIPT KAPPA;;039A;;039A => CE 9A
        03F1;GREEK RHO SYMBOL;Ll;0;L;<compat> 03C1;;;;N;GREEK SMALL LETTER TAILED RHO;;03A1;;03A1 => CE A1
        03F2;GREEK LUNATE SIGMA SYMBOL;Ll;0;L;<compat> 03C2;;;;N;GREEK SMALL LETTER LUNATE SIGMA;;03F9;;03F9
        03F5;GREEK LUNATE EPSILON SYMBOL;Ll;0;L;<compat> 03B5;;;;N;;;0395;;0395 => CE 95
        03F8;GREEK SMALL LETTER SHO;Ll;0;L;;;;;N;;;03F7;;03F7
        03FB;GREEK SMALL LETTER SAN;Ll;0;L;;;;;N;;;03FA;;03FA
        }
        $CFB0: NewChar := $CE9A;
        $CFB1: NewChar := $CEA1;
        $CFB2: NewChar := $CFB9;
        $CFB5: NewChar := $CE95;
        $CFB8: NewChar := $CFB7;
        $CFBB: NewChar := $CFBA;
        // 0400 = D0 80 ... 042F everything already uppercase
        // 0430 = D0 B0
        $D0B0..$D0BF: NewChar := OldChar - $20; // Cyrillic alphabet
        // 0440 = D1 80
        $D180..$D18F: NewChar := OldChar - $E0; // Cyrillic alphabet
        // 0450 = D1 90
        $D190..$D19F: NewChar := OldChar - $110; // Cyrillic alphabet
        end;

        if NewChar <> 0 then
        begin
          OutStr[OutCounter]  := Chr(Hi(NewChar));
          OutStr[OutCounter+1]:= Chr(Lo(NewChar));
          CharProcessed := True;
        end;
      end;

      // Copy the character if the string was disaligned by previous changed
      // and no processing was done in this character
      if (InCounter <> OutCounter+1) and (not CharProcessed) then
      begin
        for i := 0 to CharLen-1 do
          OutStr[OutCounter+i]  :=AInStr[InCounter+i];
      end;

      inc(InCounter, CharLen);
      inc(OutCounter, NewCharLen);
    end;
  end; // while

  // Final correction of the buffer size
  SetLength(Result,OutCounter);
end;

function UTF8UpperString(const s: string): string;
begin
  Result:=UTF8UpperCase(s);
end;


function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
  StopOnNonUTF8: Boolean): PtrInt;
// return -1 if ok
var
  CharLen: Integer;
  c: Char;
begin
  if (p<>nil) then begin
    Result:=0;
    while Result<Count do begin
      c:=p^;
      if ord(c)<%10000000 then begin
        // regular single byte ASCII character (#0 is a character, this is Pascal ;)
        CharLen:=1;
      end else if ord(c)<=%11000001 then begin
        // single byte character, between valid UTF-8 encodings
        // %11000000 and %11000001 map 2 byte to #0..#128, which is invalid and used for XSS attacks
        if StopOnNonUTF8 or (ord(c)>=192) then
          exit;
        CharLen:=1;
      end else if ord(c)<=%11011111 then begin
        // could be 2 byte character (%110xxxxx %10xxxxxx)
        if (Result<Count-1)
        and ((ord(p[1]) and %11000000) = %10000000) then
          CharLen:=2
        else
          exit; // missing following bytes
      end
      else if ord(c)<=%11101111 then begin
        // could be 3 byte character (%1110xxxx %10xxxxxx %10xxxxxx)
        if (Result<Count-2)
        and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then begin
          if (ord(c)=%11100000) and (ord(p[1])<=%10011111) then
            exit; // XSS attack: 3 bytes are mapped to the 1 or 2 byte codes
          CharLen:=3;
        end else
          exit; // missing following bytes
      end
      else if ord(c)<=%11110111 then begin
        // could be 4 byte character (%11110xxx %10xxxxxx %10xxxxxx %10xxxxxx)
        if (Result<Count-3)
        and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000)
        and ((ord(p[3]) and %11000000) = %10000000) then begin
          if (ord(c)=%11110000) and (ord(p[1])<=%10001111) then
            exit; // XSS attack: 4 bytes are mapped to the 1-3 byte codes
          CharLen:=4;
        end else
          exit; // missing following bytes
      end
      else begin
        if StopOnNonUTF8 then
          exit;
        CharLen:=1;
      end;
      inc(Result,CharLen);
      inc(p,CharLen);
      if Result>Count then begin
        dec(Result,CharLen);
        exit; // missing following bytes
      end;
    end;
  end;
  // ok
  Result:=-1;
end;

function ValidUTF8String(const s: String): String;
var
  p, cur: PChar;
  l, lr: integer;
  NeedFree: Boolean;
begin
  if FindInvalidUTF8Character(PChar(s), Length(s)) <> -1 then
  begin
    NeedFree := True;
    GetMem(p, Length(s) + 1);
    StrPCopy(p, s);
    UTF8FixBroken(p);
  end
  else
  begin
    p := PChar(s);
    NeedFree := False;
  end;

  Result := '';
  cur := p;
  while cur^ <> #0 do
  begin
    l := UTF8CharacterLength(cur);
    if (l = 1) and (cur^ < #32) then
      Result := Result + '#' + IntToStr(Ord(cur^))
    else
    begin
      lr := Length(Result);
      SetLength(Result, lr + l);
      System.Move(cur^, Result[lr + 1], l);
    end;
    inc(cur, l)
  end;

  if NeedFree then
    FreeMem(p);
end;

function Utf8StringOfChar(AUtf8Char: String; N: Integer): String;
var
  UCharLen, i: Integer;
  C1, C2, C3: Char;
  PC: PChar;
begin
  Result := '';
  if (N <= 0) or (Utf8Length(AUtf8Char) <> 1) then Exit;
  UCharLen := Length(AUtf8Char);
  Case UCharLen of
    1: Result := StringOfChar(AUtf8Char[1], N);
    2:
    begin
      SetLength(Result, 2 * N);
      System.FillWord(Result[1], N, PWord(Pointer(AUtf8Char))^);
     end;
    3:
    begin
      SetLength(Result, 3 * N);
      C1 := AUtf8Char[1];
      C2 := AUtf8Char[2];
      C3 := AUtf8Char[3];
      PC := PChar(Result);
      for i:=1 to N do
      begin
        PC[0] := C1;
        PC[1] := C2;
        PC[2] := C3;
        inc(PC,3);
      end;
    end;
    4:
    begin
      SetLength(Result, 4 * N);
      System.FillDWord(Result[1], N, PDWord(Pointer(AUtf8Char))^);
    end;
    else
    begin
      //In November 2003 UTF-8 was restricted by RFC 3629 to four bytes to match
      //the constraints of the UTF-16 character encoding.
      //http://en.wikipedia.org/wiki/UTF-8
      Result := StringOfChar('?', N);
    end;
  end;
end;

function Utf8AddChar(AUtf8Char: String; const S: String; N: Integer): String;
var
  L : Integer;
begin
  Result := S;
  if Utf8Length(AUtf8Char) <> 1 then Exit;
  L := Utf8Length(Result);
  if L < N then
  begin
    Result := Utf8StringOfChar(AUtf8Char, N-l) + Result;
  end;
end;

function Utf8AddCharR(AUtf8Char: String; const S: String; N: Integer): String;
var
  L : Integer;
begin
  Result := S;
  if Utf8Length(AUtf8Char) <> 1 then Exit;
  L := Utf8Length(Result);
  if L < N then
    Result := Result + Utf8StringOfChar(AUtf8Char, N-l);
end;


function UTF8PadLeft(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
begin
  Result := Utf8AddChar(AUtf8Char, S, N);
end;

function UTF8PadRight(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
begin
  Result := Utf8AddCharR(AUtf8Char, S, N);
end;

function UTF8PadCenter(const S: String; const N: Integer; const AUtf8Char: String = #32): String;
var
  ULen: PtrInt;
begin
  ULen := Utf8Length(S);
  if ULen < N then
    begin
      Result := Utf8StringOfChar(AUtf8Char,(N div 2) - (ULen div 2)) + S;
      Result := Result + Utf8StringOfChar(AUtf8Char, N - Utf8Length(Result));
    end
  else
    Result := S;
end;

function Utf8LeftStr(const AText: String; const ACount: Integer): String;
begin
  Result := Utf8Copy(AText,1,ACount);
end;

function Utf8RightStr(const AText: String; const ACount: Integer): String;
var
  j,l:integer;
begin
  l := Utf8Length(AText);
  j := ACount;
  if (j > l) then j := l;
  Result := Utf8Copy(AText,l-j+1,j);
end;

function UTF8QuotedStr(const S, Quote: string): string;
// replace all Quote in S with double Quote and enclose the result in Quote.
var
  QuoteC: Char;
  p, QuoteP, CopyPos: PChar;
  QuoteLen: SizeInt;
begin
  Result:=Quote;
  p:=PChar(S);
  CopyPos:=p;
  QuoteC:=Quote[1];
  QuoteP:=PChar(Quote);
  QuoteLen:=length(Quote);
  repeat
    if (p^=#0) and (p-PChar(S)=length(S)) then
      break;
    if (p^=QuoteC) and CompareMem(p,QuoteP,QuoteLen) then begin
      inc(p,QuoteLen);
      Result+=copy(S,CopyPos-PChar(S)+1,p-CopyPos)+Quote;
      CopyPos:=p;
    end else
      inc(p);
  until false;
  Result+=copy(S,CopyPos-PChar(S)+1,p-CopyPos)+Quote;
end;

function Utf8StartsText(const ASubText, AText: string): Boolean;
var
  TextLen, SubTextLen: PtrInt;
begin
  Result := False;
  if (ASubText <> '') then
  begin
    TextLen := Utf8Length(AText);
    SubTextLen := Utf8Length(ASubText);
    if (TextLen >= SubTextLen) then
      Result := (Utf8CompareText(Utf8Copy(AText,1,SubTextLen),ASubText) = 0);
  end;
end;

function Utf8EndsText(const ASubText, AText: string): Boolean;
var
  TextLen, SubTextLen: PtrInt;
begin
  Result := False;
  if (ASubText <> '') then
  begin
    TextLen := Utf8Length(AText);
    SubTextLen := Utf8Length(ASubText);
    if (TextLen >= SubTextLen) then
      Result := (Utf8CompareText(Utf8Copy(AText,TextLen-SubTextLen+1,SubTextLen),ASubText) = 0);
  end;
end;

function UTF8WrapText(S, BreakStr :string; BreakChars :TSysCharSet; MaxCol: integer): string;
var
  P :PChar;
  CharLen :integer;
  RightSpace : Integer = 0;
  N :integer = 0;
  i : Integer;
  j : Integer;
  Len :integer = 0;
  ResultLen, RP :Integer;
begin
  Result := '';
  if (S = '') or (MaxCol = 0) or (BreakStr = '') or (BreakChars = []) then Exit;
  P := PChar(S);
  while P^ <> #0 do
  begin
    CharLen := UTF8CharacterLength(P);
    i := 1;
    j := 0;
    ResultLen := Length(Result);
    SetLength(Result, ResultLen + CharLen);
    while i <= CharLen do
    begin
      Result[ResultLen + i] := (P + J)^;
      Inc(i);
      Inc(j);
    end;
    Inc(N);
    if P^ = BreakStr[Length(BreakStr)] then
      N := 0;
    if N > MaxCol then
    begin
      Len := Length(Result);
      RP := Len;
      while not (Result[RP] in BreakChars) do
        Dec(RP);
      RightSpace := Len - RP;
      if (RightSpace > 0) and (RightSpace < MaxCol) then
      begin
        Dec(P, RightSpace);
        SetLength(Result, Len - RightSpace);
      end;
      Result := Result + BreakStr;
      N := 0;
    end;
    Inc(P, CharLen);
  end;
end;

function UTF8WrapText(S :string; MaxCol: integer): string;
begin
  Result := UTF8WrapText(S, LineEnding, [' ', '-', #9], MaxCol);
end;

function UTF8Trim(const s: string; Flags: TUTF8TrimFlags): string;
var
  p: PChar;
  u: Cardinal;
  StartP: PtrUInt;
  l: Integer;
  KeepAllNonASCII: boolean;
begin
  Result:=s;
  if Result='' then exit;
  KeepAllNonASCII:=[u8tKeepControlCodes,u8tKeepNoBreakSpaces]*Flags=[u8tKeepControlCodes,u8tKeepNoBreakSpaces];
  if not (u8tKeepStart in Flags) then begin
    // trim start
    p:=PChar(Result);
    repeat
      l:=1;
      case p^ of
      #0:
        if p-PChar(Result)=length(Result) then
        begin
          // everything was trimmed
          exit('')
        end else if u8tKeepControlCodes in Flags then
          break;
      ' ': ;
      #10,#13:
        if u8tKeepLineBreaks in Flags then
          break;
      #9:
        if u8tKeepTabs in Flags then
          break;
      #1..#8,#11,#12,#14..#31,#127:
        if u8tKeepControlCodes in Flags then
          break;
      #128..#255:
        begin
          if KeepAllNonASCII then break;
          u:=UTF8CharacterToUnicode(p,l);
          if (l<=1) then break; // invalid character
          case u of
          128..159, // C1 set of control codes
          8206, 8207: // left-to-right, right-to-left mark
            if u8tKeepControlCodes in Flags then break;
          160,   // no break space
          $2007, // figure space
          $2026, // narrow no-break space
          $FEFF: // zero with no-break space
            if u8tKeepNoBreakSpaces in Flags then break;
          else
            break;
          end;
        end;
      else
        break;
      end;
      inc(p,l);
    until false;
    if p>PChar(Result) then begin
      Result:=copy(Result,p-PChar(Result)+1,length(Result));
      if Result='' then exit;
    end;
  end;

  if not (u8tKeepEnd in Flags) then begin
    // trim end
    p:=@Result[length(Result)];
    repeat
      case p^ of
      #0:
        if u8tKeepControlCodes in Flags then
          break;
      ' ': ;
      #10,#13:
        if u8tKeepLineBreaks in Flags then
          break;
      #9:
        if u8tKeepTabs in Flags then
          break;
      #1..#8,#11,#12,#14..#31,#127:
        if u8tKeepControlCodes in Flags then
          break;
      #128..#255:
        begin
          if KeepAllNonASCII then break;
          StartP:=UTF8FindNearestCharStart(PChar(Result),length(Result),p-PChar(Result));
          u:=UTF8CharacterToUnicode(PChar(Result)+StartP,l);
          if (l<=1) then break; // invalid character
          case u of
          128..159, // C1 set of control codes
          8206, 8207: // left-to-right, right-to-left mark
            if u8tKeepControlCodes in Flags then break;
          160,   // no break space
          $2007, // figure space
          $2026, // narrow no-break space
          $FEFF: // zero with no-break space
            if u8tKeepNoBreakSpaces in Flags then break;
          else
            break;
          end;
          p:=PChar(Result)+StartP;
        end;
      else
        break;
      end;
      dec(p);
    until p<PChar(Result);
    // p is on last good byte
    SetLength(Result,p+1-PChar(Result));
  end;
end;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);
var
  i: Integer;
begin
  AnsiList.Clear;
  if UTF8List=nil then exit;
  for i:=0 to UTF8List.Count-1 do
    AnsiList.Add(UTF8ToSys(UTF8List[i]));
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareStr
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S1 > S2.
  Compare two UTF8 encoded strings, case sensitive.
  Internally it uses CompareMemRange, which returns -1 if a byte of S1 is lower than S2.
 ------------------------------------------------------------------------------}
function UTF8CompareStr(const S1, S2: string): PtrInt;
begin
  Result := UTF8CompareStr(PChar(Pointer(S1)),length(S1),
                            PChar(Pointer(S2)),length(S2));
end;

function UTF8CompareStrP(S1, S2: PChar): PtrInt;
begin
  Result:=UTF8CompareStr(S1,StrLen(S1),S2,StrLen(S2));
end;

function UTF8CompareStr(S1: PChar; Count1: SizeInt; S2: PChar; Count2: SizeInt
  ): PtrInt;
var
  Count: SizeInt;
begin
  Result := 0;
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  Result := CompareMemRange(Pointer(S1),Pointer(S2), Count); // Note: CompareMemRange can handle nil if Count=0
  if Result<>0 then exit;
  if Count1>Count2 then
    Result:=1
  else if Count1<Count2 then
    Result:=-1
  else
    Result:=0;
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareText
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S1 > S2.
  Compare two UTF8 encoded strings, case insensitive.
  Note: Use this function instead of AnsiCompareText.
  This function guarantees proper collation on all supported platforms.
  Internally it uses UTF8CompareStr.
 ------------------------------------------------------------------------------}
function UTF8CompareText(const S1, S2: string): PtrInt;
var
  S1Lower, S2Lower: string;
begin
  S1Lower := UTF8LowerCase(S1);
  S2Lower := UTF8LowerCase(S2);
  Result := UTF8CompareStr(S1Lower, S2Lower);
end;

function UTF8CompareStrCollated(const S1, S2: string): PtrInt;
begin
  {$IFDEF MSWINDOWS}
    Result := AnsiCompareStr(UTF8ToSys(S1), UTF8ToSys(S2));
  {$ELSE}
    Result := WideCompareStr(WideString(S1),WideString(S2));
  {$ENDIF}
end;

function CompareStrListUTF8LowerCase(List: TStringList; Index1, Index2: Integer
  ): Integer;
begin
  Result:=UTF8CompareText(List[Index1],List[Index2]);
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
var
  DestI, SrcI: SizeUInt;
  B1, B2, B3, B4: Byte;
  W: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := System.WideChar('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);

      // skip trailing UTF-8 char bytes
      while (Count > 0) do
      begin
        if (Byte(Src[SrcI]) and %11000000) <> %10000000 then Break;
        Inc(SrcI);
        Dec(Count);
      end;

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualWideCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestWideCharCount) and (SrcI < SrcCharCount) do
  begin
    B1 := Byte(Src[SrcI]);
    Inc(SrcI);

    if B1 < 128 then // single byte UTF-8 char
    begin
      Dest[DestI] := System.WideChar(B1);
      Inc(DestI);
    end
    else
    begin
      if SrcI >= SrcCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      B2 := Byte(Src[SrcI]);
      Inc(SrcI);

      if (B1 and %11100000) = %11000000 then // double byte UTF-8 char
      begin
        if (B2 and %11000000) = %10000000 then
        begin
          Dest[DestI] := System.WideChar(((B1 and %00011111) shl 6) or (B2 and %00111111));
          Inc(DestI);
        end
        else // invalid character, assume single byte UTF-8 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else
      begin
        if SrcI >= SrcCharCount then
          if UnfinishedCharError then Exit(trInvalidChar)
          else Break;

        B3 := Byte(Src[SrcI]);
        Inc(SrcI);

        if (B1 and %11110000) = %11100000 then // triple byte UTF-8 char
        begin
          if ((B2 and %11000000) = %10000000) and ((B3 and %11000000) = %10000000) then
          begin
            W := ((B1 and %00011111) shl 12) or ((B2 and %00111111) shl 6) or (B3 and %00111111);
            if (W < $D800) or (W > $DFFF) then // to single wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar(W);
              Inc(DestI);
            end
            else // invalid UTF-16 character, assume double byte UTF-8 char
              if InvalidCharError(2) then Exit(trInvalidChar);
          end
          else // invalid character, assume double byte UTF-8 char
            if InvalidCharError(2) then Exit(trInvalidChar);
        end
        else
        begin
          if SrcI >= SrcCharCount then
            if UnfinishedCharError then Exit(trInvalidChar)
            else Break;

          B4 := Byte(Src[SrcI]);
          Inc(SrcI);

          if ((B1 and %11111000) = %11110000) and ((B2 and %11000000) = %10000000)
            and ((B3 and %11000000) = %10000000) and ((B4 and %11000000) = %10000000) then
          begin // 4 byte UTF-8 char
            C := ((B1 and %00011111) shl 18) or ((B2 and %00111111) shl 12)
              or ((B3 and %00111111) shl 6)  or (B4 and %00111111);
            // to double wide char UTF-16 char
            Dest[DestI] := System.WideChar($D800 or ((C - $10000) shr 10));
            Inc(DestI);
            if DestI >= DestWideCharCount then Break;
            Dest[DestI] := System.WideChar($DC00 or ((C - $10000) and %0000001111111111));
            Inc(DestI);
          end
          else // invalid character, assume triple byte UTF-8 char
            if InvalidCharError(3) then Exit(trInvalidChar);
        end;
      end;
    end;
  end;

  if DestI >= DestWideCharCount then
  begin
    DestI := DestWideCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualWideCharCount := DestI + 1;
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
var
  DestI, SrcI: SizeUInt;
  W1, W2: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := Char('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := Char('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);
      // skip trailing UTF-16 wide char
      if (Word(Src[SrcI]) and $FC00) = $DC00 then Inc(SrcI);

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestCharCount) and (SrcI < SrcWideCharCount) do
  begin
    W1 := Word(Src[SrcI]);
    Inc(SrcI);

    if (W1 < $D800) or (W1 > $DFFF) then // single wide char UTF-16 char
    begin
      if W1 < $0080 then // to single byte UTF-8 char
      begin
        Dest[DestI] := Char(W1);
        Inc(DestI);
      end
      else
        if W1 < $0800 then // to double byte UTF-8 char
        begin
          Dest[DestI] := Char(%11000000 or ((W1 and %11111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end
        else
        begin // to triple byte UTF-8 char
          Dest[DestI] := Char(%11100000 or ((W1 and %1111000000000000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((W1 and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end;
    end
    else
    begin
      if SrcI >= SrcWideCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      W2 := Word(Src[SrcI]);
      Inc(SrcI);

      if (W1 and $F800) = $D800 then // double wide char UTF-16 char
      begin
        if (W2 and $FC00) = $DC00 then
        begin
          C := (W1 - $D800) shl 10 + (W2 - $DC00) + $10000;

          // to 4 byte UTF-8 char
          Dest[DestI] := Char(%11110000 or (C shr 18));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and $3F000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (C and %111111));
          Inc(DestI);
        end
        else // invalid character, assume single wide char UTF-16 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else // invalid character, assume single wide char UTF-16 char
        if InvalidCharError(1) then Exit(trInvalidChar);
    end;
  end;

  if DestI >= DestCharCount then
  begin
    DestI := DestCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualCharCount := DestI + 1;
end;

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
  Avoid copying the result string since on windows a widestring requires a full
  copy
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: AnsiString): UnicodeString;
begin
  Result:=UTF8ToUTF16(PChar(S),length(S));
end;

function UTF8ToUTF16(const P: PChar; ByteCnt: SizeUInt): UnicodeString;
var
  L: SizeUInt;
begin
  if ByteCnt=0 then
    exit('');
  SetLength(Result, ByteCnt);
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(Result), Length(Result) + 1, P, ByteCnt,
    [toInvalidCharToSymbol], L) = trNoError
  then SetLength(Result, L - 1)
  else Result := '';
end;

{------------------------------------------------------------------------------
  Name:    UTF16ToUTF8
  Params:  S - Source UTF-16 string (system endian)
  Returns: UTF-8 encoded string

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function UTF16ToUTF8(const S: UnicodeString): AnsiString;
begin
  Result := UTF16ToUTF8(PWideChar(S),length(S));
end;

function UTF16ToUTF8(const P: PWideChar; WideCnt: SizeUInt): AnsiString;
var
  L: SizeUInt;
begin
  if WideCnt=0 then
    exit('');

  SetLength(Result, WideCnt * 3);
  // bytes of UTF-8 <= 3 * wide chars of UTF-16 string
  // e.g. %11100000 10100000 10000000 (UTF-8) is $0800 (UTF-16)
  if ConvertUTF16ToUTF8(PChar(Result), Length(Result) + 1, P, WideCnt,
    [toInvalidCharToSymbol], L) = trNoError then
  begin
    SetLength(Result, L - 1);
  end else
    Result := '';
end;

procedure LazGetLanguageIDs(var Lang, FallbackLang: String);

  {$IFDEF DARWIN}
  function GetLanguage: boolean;
  var
    Ref: CFStringRef;
    LangArray: CFMutableArrayRef;
    StrSize: CFIndex;
    StrRange: CFRange;
    Locals: CFArrayRef;
    Bundle: CFBundleRef;
  begin
    Result := false;
    Bundle:=CFBundleGetMainBundle;
    if Bundle=nil then exit;
    Locals:=CFBundleCopyBundleLocalizations(Bundle);
    if Locals=nil then exit;
    LangArray := CFBundleCopyLocalizationsForPreferences(Locals, nil);
    try
      if CFArrayGetCount(LangArray) > 0 then
      begin
        Ref := CFArrayGetValueAtIndex(LangArray, 0);
        StrRange.location := 0;
        StrRange.length := CFStringGetLength(Ref);

        StrSize:=0;
        CFStringGetBytes(Ref, StrRange, kCFStringEncodingUTF8,
          Ord('?'), False, nil, 0, StrSize);
        SetLength(Lang, StrSize);

        if StrSize > 0 then
        begin
          CFStringGetBytes(Ref, StrRange, kCFStringEncodingUTF8,
            Ord('?'), False, @Lang[1], StrSize, StrSize);
          Result:=true;
          FallbackLang := Copy(Lang, 1, 2);
        end;
      end;
    finally
      CFRelease(LangArray);
      CFRelease(Locals);
    end;
  end;
  {$ENDIF}
begin
{$IFDEF DARWIN}
  if not GetLanguage then
    GetLanguageIDs(Lang, FallbackLang);
{$ELSE}
  GetLanguageIDs(Lang, FallbackLang);
{$ENDIF}
end;

{
This routine will strip country information from the language ID
making it more simple

Ideally the resulting ID from here should conform to ISO 639-1
or ISO 639-2, if the language has no code in ISO 639-1
}
procedure LazGetShortLanguageID(var Lang: String);
var
  FallbackLang: String;
begin
  FallbackLang:='';
  LazGetLanguageIDs(Lang, FallbackLang);

  // Simply making sure its length is at most 2 should be enough for most languages
  if Length(Lang) > 2 then Lang := Lang[1] + Lang[2];
end;

procedure ReplaceSubstring(var s: string; StartPos, Count: SizeInt;
  const Insertion: string);
var
  MaxCount: SizeInt;
  InsertionLen: SizeInt;
  SLen: SizeInt;
  RestLen: SizeInt;
  p: PByte;
begin
  SLen:=length(s);
  if StartPos>SLen then begin
    s:=s+Insertion;
    exit;
  end;
  if StartPos<1 then StartPos:=1;
  if Count<0 then Count:=0;
  MaxCount:=SLen-StartPos+1;
  if Count>MaxCount then
    Count:=MaxCount;
  InsertionLen:=length(Insertion);
  if (Count=0) and (InsertionLen=0) then
    exit; // nothing to do
  if (Count=InsertionLen) then begin
    if CompareMem(PByte(s)+StartPos-1,Pointer(Insertion),Count) then
      // already the same content
      exit;
    UniqueString(s);
  end else begin
    RestLen:=SLen-StartPos-Count+1;
    if InsertionLen<Count then begin
      // shorten
      if RestLen>0 then begin
        UniqueString(s);
        p:=PByte(s)+StartPos-1;
        System.Move((p+Count)^,(p+InsertionLen)^,RestLen);
      end;
      Setlength(s,SLen-Count+InsertionLen);
    end else begin
      // longen
      Setlength(s,SLen-Count+InsertionLen);
      if RestLen>0 then begin
        p:=PByte(s)+StartPos-1;
        System.Move((p+Count)^,(p+InsertionLen)^,RestLen);
      end;
    end;
  end;
  if InsertionLen>0 then
    System.Move(PByte(Insertion)^,(PByte(s)+StartPos-1)^,InsertionLen);
end;

procedure InitFPUpchars;
var
  c: Char;
begin
  for c:=Low(char) to High(char) do begin
    FPUpChars[c]:=upcase(c);
  end;
end;

initialization
  InitFPUpchars;
  InitLazUtf8;
finalization
  FinalizeLazUTF8;

end.

