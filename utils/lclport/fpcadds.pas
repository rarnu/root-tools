{
 /***************************************************************************
                                FPCAdds.pas
                                -----------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FPCAdds;

{$mode objfpc}{$H+}{$inline on}

{$i lazutils_defines.inc}

interface

uses
  Classes, SysUtils;

type
  TStreamSeekType = int64;
  TMemStreamSeekType = PtrInt;
  TCompareMemSize = PtrUInt;
  PHandle = ^THandle;

function StrToWord(const s: string): word;

function AlignToPtr(const p: Pointer): Pointer;
function AlignToInt(const p: Pointer): Pointer;

implementation

function StrToWord(const s: string): word;
var
  p: Integer;
begin
  Result:=0;
  p:=1;
  while (p<=length(s)) do begin
    Result:=Result*10+ord(s[p])-ord('0');
    inc(p);
  end;
end;

function AlignToPtr(const p: Pointer): Pointer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(Pointer));
{$ELSE}
  Result := p;
{$ENDIF}
end;

function AlignToInt(const p: Pointer): Pointer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(integer));
{$ELSE}
  Result := p;
{$ENDIF}
end;

{$ifdef UTF8_RTL}
initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
{$IFEND}

end.
