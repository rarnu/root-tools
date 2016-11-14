{  $Id: lclintf.pas 49494 2015-07-04 23:08:00Z juha $  }
{
 /***************************************************************************
                                LCLIntf.pas
                                -----------
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

This unit is being created specifically for compatibility with Delphi. It
contains selected functions that are included in the Delphi Windows unit.
These functions are mostly abstract and implemented by the LCL interfaces.

For LCL users:
The functions can be used to make porting of Delphi applications easier
and are not 100% emulating winapi functions, not even under windows. They were
implemented and tested with some common Delphi libraries.
The LCL contains many extra functions that the Delphi VCL does not have.
For example:
Instead of using the common windows functions SaveDC and RestoreDC use instead
the Canvas.SaveHandleState and Canvas.RestoreHandleState.
}

unit LCLIntf;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  {$IFDEF Windows}Windows, ShellApi,{$ENDIF}
  {$IFDEF UNIX}Unix, {$ENDIF}
  Types, Math, Classes, SysUtils, LCLType, LCLProc, GraphType, InterfaceBase,
  FileUtil, LazFileUtils, UTF8Process, Maps, LMessages, LazUTF8, lazutf8sysutils,
  LCLStrConsts;

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}
{$DEFINE ClientRectBugFix}

// All winapi related stuff (Delphi compatible)
{$I winapih.inc}
// All interface communication (Our additions)
{$I lclintfh.inc}

function PredefinedClipboardFormat(AFormat: TPredefinedClipboardFormat): TClipboardFormat;


function MsgKeyDataToShiftState(KeyData: PtrInt): TShiftState;

function GetTickCount: DWord; inline;
function GetTickCount64: QWord; inline;

{$IFDEF DebugLCL}
function GetTickStep: DWord;
{$ENDIF}

function FindDefaultBrowser(out ABrowser, AParams: String): Boolean;

type
  TOpenParamStringProc = function (AString: string): Boolean of object;

var
  OnShowSelectItemDialogResult: TOnShowSelectItemDialogResult = nil;
  OnListViewDialogResult: TOnShowSelectItemDialogResult = nil; // -1 in the position indicates the dialog was cancelled

  OpenURLWidgetsetImplementation: TOpenParamStringProc = nil;
  OpenDocumentWidgetsetImplementation: TOpenParamStringProc = nil;

implementation

type
  { TTimerID }

  TTimerID = class
    procedure TimerNotify;
  end;

  PTimerInfo = ^TTimerInfo;
  TTimerInfo = record
    Wnd: HWND;
    IDEvent: UINT_PTR;
    TimerProc: TTimerProc;
    Handle: THandle;
  end;

var
  MTimerMap: TMap = nil;   // hWnd + nIDEvent -> ID
  MTimerInfo: TMap = nil;  // ID -> TTimerInfo
  MTimerSeq: Cardinal;

  FPredefinedClipboardFormats:
    array[TPredefinedClipboardFormat] of TClipboardFormat;
  LowerCaseChars: array[char] of char;
  UpperCaseChars: array[char] of char;


  { TTimerMap }

procedure TTimerID.TimerNotify;
var
  Info: PTimerInfo;
  ID: Cardinal;
begin
  if MTimerInfo = nil then Exit;

  // this is a bit of a hack.
  // to pass the ID if the timer, it is passed as an cast to self
  // So there isn't realy an instance of TTimerID
  ID := PtrUInt(Self);
  Info := MTimerInfo.GetDataPtr(ID);
  if Info = nil then Exit;

  if Info^.TimerProc = nil
  then begin
    // send message
    PostMessage(Info^.Wnd, LM_TIMER, Info^.IDEvent, 0);
  end
  else begin
    // a timerproc was passed
    Info^.TimerProc(Info^.Wnd, LM_TIMER, Info^.IDEvent, GetTickCount);
  end;
end;

function GetTickCount(): DWord;
begin
  Result := DWord(lazutf8sysutils.GetTickCount64());
end;

function GetTickCount64(): QWord;
begin
  Result := lazutf8sysutils.GetTickCount64();
end;

{$IFDEF DebugLCL}
var
  LastTickValid: boolean;
  LastTick: DWord;

function GetTickStep: DWord;
var
  CurTick: DWord;
begin
  CurTick:=GetTickCount;
  if LastTickValid then begin
    if LastTick<=CurTick then
      Result:=CurTick-LastTick
    else begin
      // tick counter has restarted
      Result:=CurTick+(DWord($FFFFFFFF)-LastTick+1);
    end;
  end else begin
    Result:=0;
  end;
  LastTickValid:=true;
  LastTick:=CurTick;
end;
{$ENDIF}


function PredefinedClipboardFormat(AFormat: TPredefinedClipboardFormat): TClipboardFormat;
begin
  if FPredefinedClipboardFormats[AFormat]=0 then begin
    if WidgetSet=nil then
      raise Exception.Create(rsNoWidgetSet);
    FPredefinedClipboardFormats[AFormat]:=
      ClipboardRegisterFormat(PredefinedClipboardMimeTypes[AFormat]);
  end;
  Result:=FPredefinedClipboardFormats[AFormat];
end;

function MsgKeyDataToShiftState(KeyData: PtrInt): TShiftState;
begin
  Result := [];

  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_LWIN) < 0 then Include(Result, ssMeta);
  if KeyData and $20000000 <> 0 then Include(Result, ssAlt);
end;

{$I winapi.inc}
{$I lclintf.inc}

// System APIs which have an operating-system specific implementation
// They should be moved to FPC eventually
{$I sysenvapis.inc}

procedure InternalInit;
var
  AClipboardFormat: TPredefinedClipboardFormat;
  c: char;
  s: string;
begin
  for AClipboardFormat:=Low(TPredefinedClipboardFormat) to
    High(TPredefinedClipboardFormat) do
      FPredefinedClipboardFormats[AClipboardFormat]:=0;
  for c:=Low(char) to High(char) do begin
    s:=lowercase(c);
    LowerCaseChars[c]:=s[1];
    UpperCaseChars[c]:=upcase(c);
  end;
  {$IFDEF DebugLCL}
  LastTickValid:=false;
  {$ENDIF}
end;

initialization
  InternalInit;

finalization
  FreeAndNil(MTimerMap);
  FreeAndNil(MTimerInfo);
end.
