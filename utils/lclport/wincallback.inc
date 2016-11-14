{%MainUnit CustomDrawnInt.pas}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{*************************************************************}
{            callback routines                                }
{*************************************************************}

function WndClassName(Wnd: HWND): WideString; inline;
var
  winClassName: array[0..19] of WideChar;
begin
  GetClassName(Wnd, @winClassName, 20);
  Result := winClassName;
end;

{------------------------------------------------------------------------------
 Function: CallDefaultWindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
 Returns: 0 if Msg is handled; non-zero long-integer result otherwise

 Passes message on to 'default' handler. This can be a control specific window
 procedure or the default window procedure.
 ------------------------------------------------------------------------------}

var
  DisabledForms: TList = nil;

function CheckMouseMovement: boolean;
  // returns true if mouse did not move between lmousebutton down
var
  lCursorPos: TPoint;
  moveX, moveY: integer;
begin
Result := true;
{  GetCursorPos(lCursorPos);
  moveX := lCursorPos.X - MouseDownPos.X;
  moveY := lCursorPos.Y - MouseDownPos.Y;
  Result := (-3 <= moveX) and (moveX <= 3) and (-3 <= moveY) and (moveY <= 3);}
end;

{------------------------------------------------------------------------------
 Function: WindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the specified window, in parameter Window, by
  Windows or other applications
 ------------------------------------------------------------------------------}
function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult; {$if defined(win32) or defined(win64)}stdcall{$else}cdecl{$endif};
begin
end;

{------------------------------------------------------------------------------
 Procedure: TimerCallBackProc
 Params: window_hnd - handle of window for timer message, not set in this implementation
         msg        - WM_TIMER message
         idEvent    - timer identifier
         dwTime     - current system time

 Calls the timerfunction in the Timer Object in the LCL
 ------------------------------------------------------------------------------}

//procedure TimerCallBackProc(window_hwnd : hwnd; msg , idEvent: UINT_PTR; dwTime: DWORD);
procedure TimerCallBackProc(window_hwnd : HWND; msg: UINT; idEvent: UINT_PTR; dwTime: UINT);   //=== ct9999 fo CodeTyphon Studio
 {$if defined(win32) or defined(win64)}stdcall{$else}cdecl{$endif};
Var
  TimerInfo: PWinCETimerInfo;
  n: Integer;
begin
  n := FTimerData.Count;
  while (n>0) do begin
    dec(n);
    TimerInfo := FTimerData[n];
    if TimerInfo^.TimerID=idEvent then begin
      TimerInfo^.TimerFunc;
      break;
    end;
  end;
end;
