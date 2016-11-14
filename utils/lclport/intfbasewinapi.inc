{%MainUnit ../interfacebase.pp}

{ $Id: intfbasewinapi.inc 51539 2016-02-08 19:18:10Z sekelsenmat $ }
{******************************************************************************
                                  TWidgetSet

                                   WinApi stuff

  !! In this file only winapi related code as defined in winapih.inc
     Most routines implement only the default

  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove

function TWidgetSet.Arc(DC: HDC;
  Left, Top, Right, Bottom, Angle16Deg, Angle16DegLength: Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(Left, Top, Right-Left, Bottom-Top, Angle16Deg, Angle16DegLength, 0,
                      Points, Count);
  Polyline(DC, Points, Count);
  ReallocMem(Points, 0);
  Result := True;
end;

function TWidgetSet.AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(x1, y1, x2-x1, y2-y1, Angle1, Angle2, 0, Points, Count);
  Inc(Count);
  ReallocMem(Points, Count*SizeOf(TPoint));
  Points[Count - 1] := Points[0];
  Polygon(DC, Points, Count, True);
  ReallocMem(Points, 0);
  Result := True;
end;

function TWidgetSet.BeginPaint(Handle: hwnd; var PS: TPaintStruct): hdc;
begin
  Result:=GetDC(Handle);
end;

function TWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := False;
end;

function TWidgetSet.CallNextHookEx(hhk : HHOOK; ncode : Integer;
  WParam: WParam; LParam: LParam) : Integer;
begin
  Result := 0;
end;

function TWidgetSet.CallWindowProc(lpPrevWndFunc: TFarProc; Handle: HWND;
  Msg: UINT; WParam: WParam; LParam: LParam): Integer;
begin
  Result:=0;
end;

function TWidgetSet.ClientToScreen(Handle : HWND; var P : TPoint) : Boolean;
begin
  Result := False;
end;

function TWidgetSet.CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
begin
  Result := ERROR;
end;

function TWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result := 0;
end;

function TWidgetSet.CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo; wUsage: UINT): HBITMAP;
begin
  Result := 0;
end;

function TWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  Result := 0;
end;

function TWidgetSet.CreateBrushWithRadialGradient(const LogBrush: TLogRadialGradient): HBRUSH;
begin
  Result := 0;
end;

function TWidgetSet.CreateCaret(Handle : HWND; Bitmap : hBitmap; width, Height : Integer) : Boolean;
begin
  Result := False;
end;

function TWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := 0;
end;

function TWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := 0;
end;

function TWidgetSet.CreateDIBSection(DC: HDC;
  const BitmapInfo: tagBitmapInfo; Usage: UINT;
  var Bits: Pointer; SectionHandle: THandle; Offset: DWORD): HBITMAP;
begin
  Result := 0;
end;

function TWidgetSet.CreateEllipticRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  Result:=ERROR;

end;

function TWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := 0;
end;

function TWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  // this functions is needed, because the fontname in TLogFont is limited to
  // 32 characters. If the interface does not support long font names, it can
  // simple omit this function
  Result := CreateFontIndirect(LogFont);
end;

function TWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
begin
  Result := 0;
end;

function TWidgetSet.CreatePalette(const LogPalette: TLogPalette): HPalette;
begin
  Result := 0;
end;

function TWidgetSet.CreatePatternBrush(ABitmap: HBITMAP): HBRUSH;
begin
  Result := 0;
end;

function TWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := 0;
end;

function TWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  Result := 0;
end;

function TWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
begin
  Result := 0;
end;

{
  In each of the 4 corners an ellipse will be created which has the expected
  nWidthEllipse and nHeightEllipse. Then we need to create a square in the
  appropriate corner of the ellipse, according to where it will be placed.
  CornerCutRgn = Execute AND between the rectangle and ellipse.
  CornerCutRgn = Execute SUB between the CornerCutRgn and the rectangle. (To invert it)
  Execute SUB between each r2 and the rectangle and get the final rounded rectangle.
}
function TWidgetSet.CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse, nHeightEllipse: Integer): HRGN;
var
  RoundRgn, CornerSquareRgn, CornerCutRgn: HRGN;
  nHalfX,nHalfY, tmp:integer;
begin
  if (X1 > X2) then begin
    tmp := X1;
    X1 := X2;
    X2 := tmp;
  end;

  if (Y1 > Y2) then begin
    tmp := Y1;
    Y1 := Y2;
    Y2 := tmp;
  end;

  // The resulting region
  Result := CreateRectRgn(X1, Y1, X2, Y2);

  if not ((nWidthEllipse = 0) and (nHeightEllipse = 0)) then
  begin
    nWidthEllipse := abs(nWidthEllipse);
    nHeightEllipse := abs(nHeightEllipse);

    if (nWidthEllipse > X2 - X1) then nWidthEllipse := X2 - X1;
    if (nHeightEllipse > Y2 - Y1) then nHeightEllipse := Y2 - Y1;

    nHalfX := nWidthEllipse div 2;
    nHalfY := nHeightEllipse div 2;

    // We create this region with dummy values just because
    // CombineRgn requires an existing region to receive the result
    CornerCutRgn := CreateRectRgn(0, 0, nWidthEllipse, nHeightEllipse);

    // Top-left corner
    RoundRgn := CreateEllipticRgn(X1, Y1, X1 + nWidthEllipse, Y1 + nHeightEllipse);
    CornerSquareRgn := CreateRectRgn(X1, Y1, X1 + nHalfX, Y1 + nHalfY);
    CombineRgn(CornerCutRgn, RoundRgn, CornerSquareRgn, RGN_AND);
    CombineRgn(CornerCutRgn, CornerSquareRgn, CornerCutRgn, RGN_DIFF);
    CombineRgn(Result, Result, CornerCutRgn, RGN_DIFF);
    DeleteObject(RoundRgn);
    DeleteObject(CornerSquareRgn);

    // Bottom-left corner
    RoundRgn := CreateEllipticRgn(X1, Y2 - nHeightEllipse, X1 + nWidthEllipse, Y2);
    CornerSquareRgn := CreateRectRgn(X1, Y2 - nHalfX, X1 + nHalfY, Y2);
    CombineRgn(CornerCutRgn, RoundRgn, CornerSquareRgn, RGN_AND);
    CombineRgn(CornerCutRgn, CornerSquareRgn, CornerCutRgn, RGN_DIFF);
    CombineRgn(Result, Result, CornerCutRgn, RGN_DIFF);
    DeleteObject(RoundRgn);
    DeleteObject(CornerSquareRgn);

    // Top-Right corner
    RoundRgn := CreateEllipticRgn(X2 - nWidthEllipse, Y1, X2, Y1 + nHeightEllipse);
    CornerSquareRgn := CreateRectRgn(X2 - nHalfX, Y1, X2, Y1 + nHalfY);
    CombineRgn(CornerCutRgn, RoundRgn, CornerSquareRgn, RGN_AND);
    CombineRgn(CornerCutRgn, CornerSquareRgn, CornerCutRgn, RGN_DIFF);
    CombineRgn(Result, Result, CornerCutRgn, RGN_DIFF);
    DeleteObject(RoundRgn);
    DeleteObject(CornerSquareRgn);

    // Bottom-Right corner
    RoundRgn := CreateEllipticRgn(X2 - nWidthEllipse, Y2 - nHeightEllipse, X2, Y2);
    CornerSquareRgn := CreateRectRgn(X2 - nHalfX, Y2 - nHalfY, X2, Y2);
    CombineRgn(CornerCutRgn, RoundRgn, CornerSquareRgn, RGN_AND);
    CombineRgn(CornerCutRgn, CornerSquareRgn, CornerCutRgn, RGN_DIFF);
    CombineRgn(Result, Result, CornerCutRgn, RGN_DIFF);
    DeleteObject(RoundRgn);
    DeleteObject(CornerSquareRgn);
  
    DeleteObject(CornerCutRgn);
  end;
end;

procedure TWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
begin

end;

function TWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  Result := False;
end;

function TWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result := False;
end;

function TWidgetSet.DestroyCaret(Handle : HWND): Boolean;
begin
  Result := False;
end;

function TWidgetSet.DestroyCursor(Handle: HCURSOR): Boolean;
begin
  Result := DestroyIcon(Handle);
end;

function TWidgetSet.DestroyIcon(Handle: HICON): Boolean;
begin
  Result := False;
end;

function TWidgetSet.DrawFrameControl(DC: HDC; const Rect : TRect; uType, uState : Cardinal) : Boolean;
var
  Details: TThemedElementDetails;
  ButtonDetail: TThemedButton;
  ToolBarDetail: TThemedToolBar;
begin
  if uType = DFC_BUTTON then
  begin
    ButtonDetail := tbButtonDontCare;
    case (uState and $1F) of
      DFCS_BUTTONCHECK:
        if uState and DFCS_CHECKED <> 0 then
          ButtonDetail := tbCheckBoxCheckedNormal
        else
          ButtonDetail := tbCheckBoxUncheckedNormal;
      DFCS_BUTTONRADIO:
        if uState and DFCS_CHECKED <> 0 then
          ButtonDetail := tbRadioButtonCheckedNormal
        else
          ButtonDetail := tbRadioButtonUncheckedNormal;
      DFCS_BUTTONPUSH:
        ButtonDetail := tbPushButtonNormal;
    end;

    if (uState and DFCS_INACTIVE) <> 0 then
      inc(ButtonDetail, 3)
    else
    if (uState and DFCS_PUSHED) <> 0 then
      inc(ButtonDetail, 2)
    else
    if (uState and DFCS_HOT) <> 0 then
      inc(ButtonDetail, 1);

    if (uState and DFCS_BUTTONPUSH <> 0) and (uState and DFCS_FLAT <> 0) then
    begin
      ToolBarDetail := ttbButtonNormal;
      inc(ToolBarDetail, Integer(ButtonDetail) - Integer(tbPushButtonNormal));
      Details := ThemeServices.GetElementDetails(ToolBarDetail);
    end
    else
      Details := ThemeServices.GetElementDetails(ButtonDetail);

    ThemeServices.DrawElement(DC, Details, Rect);
{    if uState and DFCS_ADJUSTRECT then
      Rect := ThemeServices.ContentRect(DC, Details, Rect);
}
    Result := True;
  end
  else
    Result := False;
end;

function TWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): Boolean;
begin
  Result := false;
end;

function TWidgetSet.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
begin
  Result := False;
end;

function TWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
var
  AP : TSize;
  TM : TTextmetric;
  theRect     : TRect;
  aLeft,aTop,
  I       : Integer;
  Lines : TStrings;
  TDC : hDC;
  tmpString,
  AStr : String;
  pIndex,
  pX1, pX2, pY : Longint;
  B, P : HGDIOBJ;
  LogP : TLogPen;
  MaxLength : Integer;
  Pt : TPoint;

  function LeftOffset : Longint;
  begin
    If (Flags and DT_Right) = DT_Right then
      Result := DT_Right
    else
      If (Flags and DT_CENTER) = DT_CENTER then
        Result := DT_CENTER
    else
      Result := DT_LEFT;
  end;

  function TopOffset : Longint;
  begin
    If (Flags and DT_BOTTOM) = DT_BOTTOM then
      Result := DT_BOTTOM
    else
      If (Flags and DT_VCENTER) = DT_VCENTER then
        Result := DT_VCENTER
    else
      Result := DT_Top;
  end;

  function SingleLine : Boolean;
  begin
    Result := (Flags and DT_SingleLine) = DT_SingleLine;
  end;

  function WordWrap : Boolean;
  begin
    Result := (Flags and DT_WordBreak) = DT_WordBreak;
  end;

  function CalcRect : Boolean;
  begin
    Result := (Flags and DT_CalcRect) = DT_CalcRect;
  end;

  function NOCLIP : Boolean;
  begin
    Result := (Flags and DT_NOCLIP) = DT_NOCLIP;
  end;

  function NoPrefix : Boolean;
  begin
    Result := (Flags and DT_NoPrefix) = DT_NoPrefix;
  end;

  function Breakable(Breaks : TList; Index : Integer) : Boolean;
  begin
    If not Assigned(Breaks) then
      exit(false);
    Result := Breaks.IndexOf(Pointer(PtrInt(Index))) <> -1;
  end;

  function NextBreakable(Breaks : TList; Index : Integer) : Integer;
  begin
    Result := -1;
    If (not Assigned(Breaks)) or
      (not Breakable(Breaks,Index))
    then
      exit;
    If Breaks.IndexOf(Pointer(PtrInt(Index))) >= Breaks.Count - 1 then
      exit;
    Result := integer(PtrUInt(Breaks[Breaks.IndexOf(Pointer(PtrInt(Index))) + 1]));
  end;

  function GetBreakablePoints(const Source : String) : TList;
  var
    I : Integer;
  begin
    Result := TList.Create;
    If Length(Source) < 1 then
      exit;
    For I := 1 to Length(Source) do
      If Source[I] = ' ' then
        If not Breakable(Result, I) then
          Result.Add(Pointer(PtrInt(I)));
    If not Breakable(Result, Length(Source)) then
      Result.Add(Pointer(PtrInt(Length(Source))));
    If not Breakable(Result, 0) then
      Result.Insert(0,nil);
  end;

  function TextExtent(Handle : hDC; const Source : String) : TSize;
  var
    pStr : PChar;
  begin
    pStr := StrAlloc(Length(Source)+1);
    try
      StrPCopy(pStr, Source);
      GetTextExtentPoint(Handle, pStr, Length(Source), Result);
    finally
      StrDispose(PStr);
    end;
  end;

  function GetStringLength(Handle : hDC; const Source : String;
    FromPos, ToPos : Integer) : Integer;
  var
    Tmp : String;
  begin
    Tmp := Copy(Source,FromPos,ToPos - FromPos);
    Result := TextExtent(Handle, Tmp).cX;
    SetLength(Tmp,0);
  end;

  function GetStringHeight(Handle : hDC; const Source : String;
    FromPos, ToPos : Integer) : Integer;
  var
    Tmp : String;
  begin
    Tmp := Copy(Source,FromPos,ToPos - FromPos);
    Result := TextExtent(Handle,Tmp).cY;
    SetLength(Tmp,0);
  end;

  function BreakString(const Source : String) : TStrings;
  var
    I, FromPos, ToPos : Integer;
    Breaks : TList;
  begin
    Result := TStringList.Create;
    Breaks := GetBreakablePoints(Source);
    If Breaks.Count <= 0 then begin
      Result.Append(Source);
      Breaks.Free;
      exit;
    end;
    FromPos := 1;
    ToPos := 0;
    I := 1;
    Repeat
      If Breakable(Breaks,I) then begin
        If NextBreakable(Breaks,I) <> -1 then begin
          TmpString := Copy(Source, FromPos, NextBreakable(Breaks,I) - FromPos + 1);
          If not NoPrefix then
            DeleteAmpersands(tmpString);
          If TextExtent(DC, TmpString).cX > MaxLength
          then begin
            ToPos := I;
            Result.Append(Copy(Source,FromPos,ToPos - FromPos + 1));
            FromPos := ToPos + 1;
            I := FromPos;
          end
          Else
            I := NextBreakable(Breaks,I);
        end
        else begin
          ToPos := I;
          Result.Append(Copy(Source,FromPos,ToPos - FromPos + 1));
          FromPos := ToPos + 1;
          I := FromPos;
        end;
      end else
        I := I + 1;
    until I > Length(Source);
    SetLength(TmpString,0);
    Breaks.Free;
  end;

  function DoBreakString(const AStr : String) : TStrings;
  var
    TS : TStrings;
    Num : Longint;
    OldText, NewText : String;
  begin
    Result := TStringList.Create;
    If not SingleLine then begin
      OldText := AStr;
      Num := pos(#10,OldText);
      while Num > 0 do begin
        NewText := Copy(OldText,1,Num);
        Case OldText[Num] of
          #13,#10 : Delete(NewText,Num,1);
        end;
        If Num -1 > 0 then
          Case OldText[Num-1] of
            #13,#10 : Delete(NewText,Num-1,1);
          end;
        If WordWrap then begin
          TS := BreakString(Copy(NewText,1,Length(NewText)));
          Result.AddStrings(TS);
          TS.Free;
        end
        else
          Result.Append(Copy(NewText,1,Length(NewText)));
        Delete(OldText,1,Num);
        Num := pos(#10,OldText);
        SetLength(NewText,0);
      end;
      if OldText <> '' then
        If WordWrap then begin
          TS := BreakString(Copy(OldText,1,Length(OldText)));
          Result.AddStrings(TS);
          TS.Free;
        end
        else
          Result.Append(Copy(OldText,1,Length(OldText)));
    end
    else
      Result.Append(AStr);
  end;

  procedure CalcTextRect;
  var
    J, wT,hT : Integer;
  begin
    theRect:=Rect;
    {Initialize text width/height}
    wT := Tm.tmAveCharWidth*StrLen(Str);
    hT := TM.tmHeight;

    If Lines.Count = 1 then begin
      {Get text width/height if only one line}
      AStr := Lines[0];
      If not NoPrefix then
        DeleteAmpersands(aStr);
      hT := TM.tmHeight;
      wT := GetStringLength(DC, AStr,1, Length(AStr) + 1);
    end
    else begin
      {Get text width/height if more than one line}
      hT := hT* (Lines.Count);
      wT := 0;
      For J := 0 to Lines.Count - 1 do begin
        AStr := Lines[J];
        If not NoPrefix then
          DeleteAmpersands(aStr);
        If wT < GetStringLength(DC, AStr,1, Length(AStr) + 1)
        then
          wT := GetStringLength(DC, AStr,1, Length(AStr) + 1);
      end;
    end;

    theRect.Right := theRect.Left + wT;
    If not CalcRect then
      Case LeftOffset of
        DT_CENTER :
          OffsetRect(theRect, (Rect.Right - theRect.Right) div 2, 0);
        DT_Right :
          OffsetRect(theRect, Rect.Right - theRect.Right, 0);
      end;

    theRect.Bottom := theRect.Top + hT;

    {If SingleLine allow vertical offset}
    If not CalcRect then
      If SingleLine then
        Case TopOffset of
          DT_VCENTER :
            OffsetRect(theRect, 0, (Rect.Bottom - theRect.Bottom) div 2);
          DT_Bottom :
            OffsetRect(theRect, 0, Rect.Bottom - theRect.Bottom);
        end;
  end;
var
  pStr: PChar;
begin
  Result := 0;
  Lines := nil;

  {Get accurate string length, if none was given}
  If Count < 0 then
    Count := StrLen(Str);

  {Calculate # Lines, etc.}
  if Count > 0 then
  begin
    pStr := StrAlloc(Count + 1);
    try
      StrLCopy(pStr, Str, Count);
      pStr[Count] := #0;
      AStr := String(pStr);
    finally
      StrDispose(pStr);
    end;
  end
  else
    AStr := '';

  tmpString := Copy(AStr, 1, Length(AStr));

  {Get font & string metrics}
  GetTextMetrics(DC, TM);
  If not NoPrefix then
    DeleteAmpersands(tmpString);

  If tmpString > '' then begin
    AP:=TextExtent(DC, tmpString);
    AP.cX := AP.cX div Length(tmpString);
  end
  else begin
    AP.cY := TM.tmHeight;
    AP.cX := TM.tmAveCharWidth;
  end;

  {Break string into individual lines}
  MaxLength := (Rect.Right - Rect.Left);
  Lines := DoBreakString(AStr);

  {Error occcured...}
  If Lines = nil then
    exit;

  {Calculate the text's bounding rect}
  CalcTextRect;

  {If just calculating rect, finish up here}
  If CalcRect then begin
    Rect:=theRect;
    Lines.Free;
    exit;
  end;

  {Backup device-context}
  TDC := SaveDC(DC);

  {Set clipping area if enabled}
  If not NOCLIP then begin
    If theRect.Right > Rect.Right then
      theRect.Right := Rect.Right;
    If theRect.Bottom > Rect.Bottom then
      theRect.Bottom := Rect.Bottom;
    IntersectClipRect(DC, theRect.Left, theRect.Top,
      theRect.Right, theRect.Bottom);
  end;

  {Select NULL brush}
  B := SelectObject(DC, GetStockObject(NULL_BRUSH));

  {Create & select pen of font color}
  LogP.lopnStyle := PS_SOLID;
  LogP.lopnWidth.X := 1;
  LogP.lopnColor := GetTextColor(DC);
  P := SelectObject(DC, CreatePenIndirect(LogP));

  for I := 0 to Lines.Count - 1 do begin
    {Set vertical position for line}
    aTop := theRect.Top + I*TM.tmHeight;

    if (aTop >= Rect.Top) and (aTop <= Rect.Bottom)
    then begin
      AStr := Lines[I];

      {Remove ampersands & get index of prefix}
      If not NoPrefix then
        pIndex := DeleteAmpersands(aStr)
      else
        pIndex := -1;

      {Offset line according to orientation}
      Case LeftOffset of
        DT_Left:
          aLeft := theRect.Left;
        DT_Center:
          aLeft := theRect.Left + (theRect.Right - theRect.Left) div 2
                   - TextExtent(DC, aStr).cX div 2;
        DT_Right:
          aLeft := theRect.Right - TextExtent(DC, AStr).cX;
      end;

      {Draw line of Text}
      TextOut(DC, aLeft, aTop, PChar(AStr), Length(AStr));

      {Prefix drawing}
      If pIndex > 0 then begin
        {Get prefix line position}
        pX1 := aLeft + GetStringLength(DC, AStr, 1, pIndex);
        pX2 := pX1 + GetStringLength(DC, AStr, pIndex, pIndex + 1);
        pY := aTop + tm.tmHeight - TM.tmDescent + 1;

        {Draw prefix line}
        MoveToEx(DC, pX1, PY, @Pt);
        LineTo(DC, pX2, pY);

        {Reset pen position}
        MoveToEx(DC, Pt.X, Pt.Y, nil);
      end;
    end;
  end;

  {Reset brush}
  SelectObject(DC, B);

  {Reset pen}
  DeleteObject(SelectObject(DC, P));

  {Finalize Lines}
  Lines.Free;

  {Restore device-context}
  RestoreDC(DC, TDC);

  Result := 1;
end;

function TWidgetSet.DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := False;
end;

function TWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Result := False;
end;

function TWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := False;
end;

function TWidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  Result:=1;
end;

procedure TWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
begin

end;

function TWidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
begin
  Result := False;
end;

function TWidgetSet.EnumFontFamilies(DC: HDC; Family: Pchar; EnumFontFamProc: FontEnumProc; LParam:Lparam):longint;
begin

  Result := 0;
end;

function TWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin

  Result := 0;
end;

function TWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
  X, Y : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  If X2 < X1 then begin
    X := X2;
    X2 := X1;
    X1 := X;
  end;
  If Y2 < Y1 then begin
    Y := Y2;
    Y2 := Y1;
    Y1 := Y;
  end;
  If (ABS(Y2 - Y1) > 0) and (ABS(X2 - X1) > 0) then begin
    PolyBezierArcPoints(x1, y1, x2 - x1, y2 - y1,0,360*16,0,Points,Count);
    Polygon(DC, Points, Count, True);
    ReallocMem(Points, 0);
  end;
  Result := True;
end;

function TWidgetSet.EqualRgn(Rgn1: HRGN; Rgn2: HRGN): Boolean;
begin
  Result := False;
end;

function TWidgetSet.ExcludeClipRect(dc: hdc;
  Left, Top, Right, Bottom : Integer) : Integer;
var
  RRGN : hRGN;
begin
  If DCClipRegionValid(DC) then begin
    //DebugLn('TWidgetSet.ExcludeClipRect A DC=',DbgS(DC),' Rect=',Left,',',Top,',',Right,',',Bottom);
    // create the rectangle region, that should be excluded
    RRGN := CreateRectRgn(Left,Top,Right,Bottom);
    Result := ExtSelectClipRGN(DC, RRGN, RGN_DIFF);
    //DebugLn('TWidgetSet.ExcludeClipRect B Result=',Result);
    DeleteObject(RRGN);
  end else
    Result:=ERROR;
end;

function TWidgetSet.ExtCreatePen(dwPenStyle, dwWidth: DWord;
  const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
var
  ALogPen: TLogPen;
begin
  // if there is no widgetset implementation use this best match
  ALogPen.lopnColor := lplb.lbColor;
  ALogPen.lopnStyle := dwPenStyle;
  ALogPen.lopnWidth := Point(dwWidth, 0);
  Result := CreatePenIndirect(ALogPen);
end;

function TWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := False;
end;

function TWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint): Integer;
var
  OldC, Clip: hRGN;
begin
  OldC := CreateEmptyRegion;
  GetClipRGN(DC, OldC);
  Clip := CreateEmptyRegion;
  //DebugLn('TWidgetSet.ExtSelectClipRGN A OldC=',DbgS(OldC),
  //  ' Clip=',DbgS(Clip),8),' RGn=',DbgS(Cardinal(RGN),' Mode=',dbgs(Mode));
  Result := CombineRGN(Clip, OldC, RGN, Mode);
  //DebugLn('TWidgetSet.ExtSelectClipRGN B Result=',Result);
  if Result <> ERROR then
    Result := SelectClipRGN(DC, Clip);
  DeleteObject(Clip);
  DeleteObject(OldC);
end;

function TWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  Result := False;
end;

function TWidgetSet.FloodFill(DC: HDC; X, Y: Integer;
  Color: TGraphicsColor;
  FillStyle: TGraphicsFillStyle;
  Brush: HBRUSH): Boolean;
begin
  Result := false;
end;

function TWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL;
begin
  Result := false;
end;

function TWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH) : integer;
begin
  Result:= 0;
end;

function TWidgetSet.GetActiveWindow : HWND;
begin
  Result := 0;
end;

function TWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint; Bits: Pointer): Longint;
begin
  Result := 0;
end;

function TWidgetSet.GetBkColor(DC: HDC): TColorRef;
begin
  Result := 0;
end;

function TWidgetSet.GetCapture : HWND;
begin
  Result := 0;
end;

function TWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetClientRect(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  lpRect^ := Rect(0,0,0,0);
  Result := SIMPLEREGION;
end;

function TWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN) : Longint;
begin
  Result := -1;
end;

function TWidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := 0;
end;

function TWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetDC(hWnd: HWND): HDC;
begin
  Result := 0;
end;

function TWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := 0;
end;

function TWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap;
  StartScan, NumScans: UINT; Bits: Pointer;
  var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := 0;
end;

function TWidgetSet.GetDoubleClickTime: UINT;
begin
  // use windows default
  Result := 500;
end;

function TWidgetSet.GetFocus: HWND;
begin
  Result := 0;
end;

function TWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result := 0;
end;

function TWidgetSet.GetForegroundWindow: HWND;
begin
  Result := 0;
end;

function TWidgetSet.GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := 0;
end;

function TWidgetSet.GetMapMode(DC: HDC): Integer;
begin
  Result := 0;
end;

function TWidgetset.GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
begin
  Result := 0;
end;

function TWidgetSet.GetPaletteEntries(Palette: HPALETTE;
  StartIndex, NumEntries: UINT; var PaletteEntries): UINT;
begin
  Result := 0;
end;

function TWidgetSet.GetParent(Handle : HWND): HWND;
begin
  Result := 0;
end;

function TWidgetSet.GetProp(Handle : hwnd; Str : PChar): Pointer;
begin
  Result := nil;
end;

function TWidgetSet.GetRgnBox(RGN: HRGN; lpRect: PRect): Longint;
begin
  Result := SIMPLEREGION;
end;

function TWidgetSet.GetROP2(DC: HDC): integer;
begin
  Result := 0;
end;

function TWidgetSet.GetScrollInfo(Handle: HWND; SBStyle: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  Result := 0;
end;

function TWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  Result := 0;
end;

function TWidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbColor := GetSysColor(nIndex);
  LogBrush.lbStyle := 0; // BS_CLEAR
  LogBrush.lbHatch := 0;
  Result := CreateBrushIndirect(LogBrush);
end;

function TWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := 0;
  case nIndex of
  SM_CXEDGE: Result:=2;
  SM_CYEDGE: Result:=2;
  end;
end;

function TWidgetSet.GetSystemPaletteEntries(DC: HDC;
  StartIndex, NumEntries: UINT; var PaletteEntries): UINT;
begin
  Result := 0;
end;

function TWidgetSet.GetTextColor(DC: HDC) : TColorRef;
begin
  Result := 0;
end;

{ Returns in MaxCount how many characters fit into a given MaxWidth
  It also returns the width of each character

  MaxCount is given in the number of UTF-8 characters, not bytes
}
function TWidgetSet.GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): Boolean;
var
  lPasStr, lCurSubStr: String;
  lPasStrLen, i: PtrInt;
  lCurSize: TSize;
  lBestFitFound: Boolean = False;
begin
  // First obtain the size information which duplicates GetTextExtentPoint
  Result := GetTextExtentPoint(DC,Str,Count,Size);
  // Now calculate MaxCount and PartialWidths
  lPasStr := StrPas(Str);
  if (Str = nil) or (Count <= 0) or (lPasStr = '') then
  begin
    Result := False;
    if MaxCount <> nil then
      MaxCount^ := 0;
    exit;
  end;
  lPasStrLen := UTF8Length(lPasStr);
  for i := 1 to lPasStrLen do
  begin
    if (not lBestFitFound) then
    begin
      lCurSubStr := UTF8Copy(lPasStr, 1, i);
      Self.GetTextExtentPoint(DC, PChar(lCurSubStr), Length(lCurSubStr), lCurSize);

      // Calculate the summed partial widths
      if PartialWidths<>nil then PartialWidths[i-1] := lCurSize.cx;

      // Calculate the width until the utilized size gets bigger then the desired one
      // Give up when the size surpases MaxWidth to be faster
      if (MaxCount <> nil) then
      begin
        if lCurSize.cx <= MaxWidth then MaxCount^ := i
        else lBestFitFound := True;
      end;
    end;
  end;
end;

// Note that Count is the number of bytes in the utf-8 encoded string Str
function TWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer;
  var Size: TSize): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetTextExtentPoint32(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
begin
  Result := GetTextExtentPoint(DC,Str,Count,Size);
end;

function TWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result := False;
end;

function TWidgetSet.GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
begin
  Result := 0;
end;

function TWidgetSet.GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
begin
  Result := 0;
end;

function TWidgetSet.GetWindowExtEx(DC: HDC; Size: PSize): Integer;
begin
  Result := 0;
end;

function TWidgetSet.GetWindowLong(Handle : hwnd; int : Integer): PtrInt;
begin
  Result := 0;
end;

function TWidgetSet.GetWindowOrgEx(dc: hdc; var P: TPoint): Integer;
begin
  Result := GetWindowOrgEx(dc, @P);
end;

function TWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
begin
  Result := 0;
  if P <> nil then
    P^ := Point(0, 0);
end;

function TWidgetSet.GetWindowRect(Handle : hwnd; var Rect : TRect): Integer;
{ After the call, ARect will be the control area in screen coordinates.
  That means, Left and Top will be the screen coordinate of the TopLeft pixel
  of the Handle object and Right and Bottom will be the screen coordinate of
  the BottomRight pixel. }
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: GradientFill
  Params: DC - DeviceContext to perform on
          Vertices - array of Points W/Color & Alpha
          NumVertices - Number of Vertices
          Meshes - array of Triangle or Rectangle Meshes,
                   each mesh representing one Gradient Fill
          NumMeshes - Number of Meshes
          Mode - Gradient Type, either Triangle,
                 Vertical Rect, Horizontal Rect

  Returns: true on success

  Performs multiple Gradient Fills, either a Three way Triangle Gradient,
  or a two way Rectangle Gradient, each Vertex point also supports optional
  Alpha/Transparency for more advanced Gradients.
 ------------------------------------------------------------------------------}
function TWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices: Longint;
  Meshes: Pointer; NumMeshes: Longint; Mode: Longint): Boolean;

  function DoFillTriangle: Boolean; inline;
  begin
    Result := (Mode and GRADIENT_FILL_TRIANGLE) = GRADIENT_FILL_TRIANGLE;
  end;

  function DoFillVRect: Boolean; inline;
  begin
    Result := (Mode and GRADIENT_FILL_RECT_V) = GRADIENT_FILL_RECT_V;
  end;

  function CreateIntfImage(W, H: Integer; Clear: Boolean): TLazIntfImage;
  begin
    Result := TLazIntfImage.Create(W, H, [riqfRGB, riqfAlpha, riqfUpdate]);
    Result.CreateData;
    if Clear then
      Result.FillPixels(FPColor(0, 0, 0, $0000));
  end;

  procedure DrawIntfImage(Image: TLazIntfImage; R: TRect);
  var
    Bmp, Mask, Old: HBitmap;
    BmpDC: HDC;
  begin
    Image.CreateBitmaps(Bmp, Mask, True);
    BmpDC := CreateCompatibleDC(0);
    Old := SelectObject(BmpDC, Bmp);
    MaskBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, BmpDC, 0, 0, Mask, 0, 0);
    DeleteObject(SelectObject(BmpDC, Old));
    if Mask <> 0 then
      DeleteObject(Mask);
    DeleteDC(BmpDC);
  end;

  function GetRectangleGradientColor(const BeginColor, EndColor: TFPColor; const Position, TotalSteps: Longint): TFPColor; inline;
  var
    A1: Word absolute BeginColor.alpha;
    R1: Word absolute BeginColor.red;
    G1: Word absolute BeginColor.green;
    B1: Word absolute BeginColor.blue;
    A2: Word absolute Endcolor.alpha;
    R2: Word absolute Endcolor.red;
    G2: Word absolute Endcolor.green;
    B2: Word absolute Endcolor.blue;
  begin
    Result.alpha := (A1 + (Position * (A2 - A1) div TotalSteps));
    Result.red := (R1 + (Position * (R2 - R1) div TotalSteps));
    Result.green := (G1 + (Position * (G2 - G1) div TotalSteps));
    Result.blue := (B1 + (Position * (B2 - B1) div TotalSteps));
  end;

  function GetTriangleBounds(const v1, v2, v3: TTriVertex): TRect;
  begin
    with v1 do
    begin
      Result.Left := x;
      Result.Top := y;
      Result.BottomRight := Result.TopLeft;
    end;
    with v2 do
    begin
      if x < Result.Left then
        Result.Left := x;
      if x > Result.Right then
        Result.Right := x;
      if y < Result.Top then
        Result.Top := y;
      if y > Result.Bottom then
        Result.Bottom := y;
    end;
    with v3 do
    begin
      if x < Result.Left then
        Result.Left := x;
      if x > Result.Right then
        Result.Right := x;
      if y < Result.Top then
        Result.Top := y;
      if y > Result.Bottom then
        Result.Bottom := y;
    end;
  end;

  {
    implementation of Arjen Nienhuis:
    http://www.winehq.org/pipermail/wine-patches/2003-June/006544.html
    Arjen has granted us the rights to include this code with our modified LGPL2 license
  }

  procedure GradientFillTriangle(Image: TLazIntfImage; v1, v2, v3: TTriVertex);
  var
    t, v: TTriVertex;
    y, y2, dy, dy2: Integer;
    x, x1, x2, r1, r2, g1, g2, b1, b2: Integer;
    dx: Integer;
  begin
    if (v1.y > v2.y) then
    begin
      t := v1;
      v1 := v2;
      v2 := t;
    end;

    if (v2.y > v3.y) then
    begin
      t := v2;
      v2 := v3;
      v3 := t;
      if (v1.y > v2.y) then
      begin
        t := v1;
        v1 := v2;
        v2 := t;
      end;
    end;
    // v1.y <= v2.y <= v3.y
    dy := v3.y - v1.y;
    for y := 0 to dy - 1 do
    begin
      // v1.y <= y < v3.y
      if y < (v2.y - v1.y) then
        v :=  v1
      else
        v := v3;
      // (v.y <= y < v2.y) || (v2.y <= y < v.y)
      dy2 := v2.y - v.y;
      y2 := y + v1.y - v.y;
      x1 := (v3.x     * y  + v1.x     * (dy  - y )) div dy;
      x2 := (v2.x     * y2 + v. x     * (dy2 - y2)) div dy2;
      r1 := (v3.Red   * y  + v1.Red   * (dy  - y )) div dy;
      r2 := (v2.Red   * y2 + v. Red   * (dy2 - y2)) div dy2;
      g1 := (v3.Green * y  + v1.Green * (dy  - y )) div dy;
      g2 := (v2.Green * y2 + v. Green * (dy2 - y2)) div dy2;
      b1 := (v3.Blue  * y  + v1.Blue  * (dy  - y )) div dy;
      b2 := (v2.Blue  * y2 + v. Blue  * (dy2 - y2)) div dy2;
      if (x1 < x2) then
      begin
        dx := x2 - x1;
        for x := 0 to dx - 1 do
          Image.Colors[x + x1, y + v1.y] := FPColor(
            (r1 * (dx - x) + r2 * x) div dx,
            (g1 * (dx - x) + g2 * x) div dx,
            (b1 * (dx - x) + b2 * x) div dx);
      end
      else
      begin
        dx := x1 - x2;
        for x := 0 to dx - 1 do
          Image.Colors[x + x2, y + v1.y] := FPColor(
            (r2 * (dx - x) + r1 * x) div dx,
            (g2 * (dx - x) + g1 * x) div dx,
            (b2 * (dx - x) + b1 * x) div dx);
      end;
    end;
  end;


  function FillTriMesh(Mesh: TGradientTriangle): Boolean;
  var
    v1, v2, v3: TTriVertex;
    R: TRect;
    Image: TLazIntfImage;
  begin
    with Mesh do
    begin
      Result :=
        (Vertex1 < Cardinal(NumVertices)) and
        (Vertex2 < Cardinal(NumVertices)) and
        (Vertex3 < Cardinal(NumVertices));

      if (Vertex1 = Vertex2) or (Vertex1 = Vertex3) or (Vertex2 = Vertex3) or not Result then
        Exit;
    end;

    v1 := Vertices[Mesh.Vertex1];
    v2 := Vertices[Mesh.Vertex2];
    v3 := Vertices[Mesh.Vertex3];
    R := GetTriangleBounds(v1, v2, v3);
    with R do
    begin
      dec(v1.x, Left);
      dec(v2.x, Left);
      dec(v3.x, Left);

      dec(v1.y, Top);
      dec(v2.y, Top);
      dec(v3.y, Top);
    end;

    Image := CreateIntfImage(R.Right - R.Left, R.Bottom - R.Top, True);
    GradientFillTriangle(Image, v1, v2, v3);
    DrawIntfImage(Image, R);
    Image.Free;
    Result := True;
  end;

  function FillRectMesh(Mesh: TGradientRect): Boolean;
  var
    TL, BR: TTriVertex;
    StartColor, EndColor, CurColor: TFPColor;
    I, J: Longint;
    SwapColors: Boolean;
    Steps: Integer;
    Image: TLazIntfImage;
    R: TRect;
  begin
    with Mesh do
    begin
      Result := (UpperLeft < Cardinal(NumVertices)) and (LowerRight < Cardinal(NumVertices));
      if (LowerRight = UpperLeft) or not Result then
        Exit;
      TL := Vertices[UpperLeft];
      BR := Vertices[LowerRight];
      SwapColors := (BR.Y < TL.Y) and (BR.X < TL.X);
      if BR.X < TL.X then
      begin
        I := BR.X;
        BR.X := TL.X;
        TL.X := I;
      end;
      if BR.Y < TL.Y then
      begin
        I := BR.Y;
        BR.Y := TL.Y;
        TL.Y := I;
      end;
      StartColor := FPColor(TL.Red, TL.Green, TL.Blue);
      EndColor := FPColor(BR.Red, BR.Green, BR.Blue);
      if SwapColors then
      begin
        CurColor := StartColor;
        StartColor := EndColor;
        EndColor := CurColor;
      end;

      R := Rect(TL.X, TL.Y, BR.X, BR.Y);
      dec(BR.X, TL.X);
      dec(BR.Y, TL.Y);
      TL.X := 0;
      TL.Y := 0;
      Image := CreateIntfImage(BR.X, BR.Y, False);

      if DoFillVRect then
      begin
        Steps := BR.Y;
        for I := 0 to Steps - 1 do
        begin
          CurColor := GetRectangleGradientColor(StartColor, EndColor, I, Steps);
          for J := TL.X to BR.X - 1 do
            Image.Colors[J, I] := CurColor;
        end
      end
      else
      begin
        Steps := BR.X;
        for I := 0 to Steps - 1 do
        begin
          CurColor := GetRectangleGradientColor(StartColor, EndColor, I, Steps);
          for J := TL.Y to BR.Y - 1 do
            Image.Colors[I, J] := CurColor;
        end;
      end;
      DrawIntfImage(Image, R);
      Image.Free;
    end;
  end;

const
  MeshSize: array[Boolean] of PtrUInt = (
    SizeOf(tagGradientRect),
    SizeOf(tagGradientTriangle)
  );
var
  I : Integer;
begin
  Result := Assigned(Meshes) and (NumMeshes >= 1) and (NumVertices >= 2) and Assigned(Vertices);
  if Result and DoFillTriangle then
    Result := NumVertices >= 3;
  if Result then
  begin
    Result := False;

    //Sanity Checks For Vertices Size vs. Count
    if MemSize(Vertices) < PtrUInt(SizeOf(TTriVertex) * NumVertices) then
      Exit;

    //Sanity Checks For Meshes Size vs. Count
    if MemSize(Meshes) < (MeshSize[DoFillTriangle] * Cardinal(NumMeshes)) then
      Exit;

    for I := 0 to NumMeshes - 1 do
    begin
      if DoFillTriangle then
      begin
        if not FillTriMesh(PGradientTriangle(Meshes)[I]) then
          exit;
      end
      else
      begin
        if not FillRectMesh(PGradientRect(Meshes)[I]) then
          exit;
      end;
    end;
    Result := True;
  end;
end;

function TWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result := False;
end;

function TWidgetSet.InitStockFont(AFont: TObject; AStockFont: TStockFont): Boolean;
begin
  Result := False;
end;

function TWidgetSet.IsHelpKey(Key: Word; Shift: TShiftState): Boolean;
{ Asks if the passed key is determined by widgetset to show help }
begin
  Result := (Shift = []) and (Key = VK_F1);
end;

procedure TWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
begin

end;

function TWidgetSet.IntersectClipRect(DC: HDC; Left, Top, Right, Bottom: Integer): Integer;
var
  R: TRect;
  RRGN: hRGN;
begin
  R := Rect(Left, Top, Right, Bottom);
  LPtoDP(DC, R, 2);
  with R do
    RRGN := CreateRectRgn(Left, Top, Right, Bottom);
  if not DCClipRegionValid(DC) then
    Result := SelectClipRGN(DC, RRGN)
  else
    Result := ExtSelectClipRGN(DC, RRGN, RGN_AND);
  DeleteObject(RRGN);
end;

function TWidgetSet.InvalidateRect(aHandle : HWND; ARect : pRect; bErase : Boolean) : Boolean;
begin
  Result := false;
end;

function TWidgetSet.InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
begin
  Result := False;
end;

function TWidgetSet.IsDBCSLeadByte(TestChar: Byte): boolean;
begin
  Result := false;
end;

function TWidgetSet.IsIconic(handle: HWND): boolean;
begin
  Result := False;
end;

function TWidgetSet.IsWindow(handle: HWND): boolean;
begin
  Result := False;
end;

function TWidgetSet.IsWindowEnabled(handle: HWND): boolean;
begin
  Result := false;
end;

function TWidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  Result := false;
end;

function TWidgetSet.IsZoomed(handle: HWND): boolean;
begin
  Result := False;
end;

procedure TWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
begin

end;

function TWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TWidgetSet.LoadBitmap(hInstance: THandle; lpBitmapName: PChar): HBitmap;
begin
  Result := LoadBitmapFunction(hInstance, lpBitmapName);
end;

function TWidgetSet.LoadCursor(hInstance: THandle; lpCursorName: PChar): HCursor;
begin
  Result := LoadCursorFunction(hInstance, lpCursorName);
end;

function TWidgetSet.LoadIcon(hInstance: THandle; lpIconName: PChar): HIcon;
begin
  Result := LoadIconFunction(hInstance, lpIconName);
end;

function TWidgetSet.LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := False;
end;

function TWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType : Cardinal): integer;
begin
  Result := 0;
end;

function TWidgetSet.MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR;
begin
  Result := MONITOR_UNIMPL;
end;

function TWidgetSet.MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR;
begin
  Result := MONITOR_UNIMPL;
end;

function TWidgetSet.MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR;
begin
  Result := MONITOR_UNIMPL;
end;

function TWidgetSet.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer;
  Rop: DWORD): Boolean;
begin
  Result := StretchMaskBlt(DestDC,X,Y,Width,Height,SrcDC,XSrc,YSrc,Width,Height,
                           Mask,XMask,YMask,ROp);
end;

function TWidgetSet.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean;
begin
  Result := MaskBlt(DestDC,X,Y,Width,Height,SrcDC,XSrc,YSrc,
                    Mask,XMask,YMask,SRCCOPY);
end;

function TWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  Result := False;
end;

function TWidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
begin
  Result := Error;
end;

function TWidgetSet.PaintRgn(DC: HDC; RGN: HRGN): Boolean;
begin
  Result := False;
end;

function TWidgetSet.PeekMessage(var lpMsg : TMsg; Handle : HWND;
  wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := False;
end;

function TWidgetSet.Pie(DC: HDC; x1, y1, x2, y2,
                        sx, sy, ex, ey: Integer): Boolean;
var
  A1, A2: extended;
begin
  Coords2Angles(x1, y1, x2-x1, y2-y1, sx, sy, ex, ey, A1, A2);
  Result := RadialPie(DC, x1, y1, x2, y2, RoundToInt(A1), RoundToInt(A2));
end;

function TWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled,Continuous: boolean): boolean;
var
  APoints : PPoint;
  ACount : Longint;
begin
  APoints := nil;
  ACount := 0;
  PolyBezier2Polyline(Points,NumPts,APoints,ACount,Continuous);
  If Filled then
    Result := Polygon(DC,APoints,ACount, False)
  else
    Result := Polyline(DC,APoints,ACount);
  ReallocMem(APoints,0);
End;

function TWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
begin
  Result := false;
end;

function TWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
begin
  Result := false;
end;

function TWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): Boolean;
begin
  Result := False;
end;

function TWidgetSet.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
begin
  Result := false;
end;

function TWidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  Result := 0;
end;

function TWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Determines if the specified rectangle is within the boundaries of a region.
  Default implementation, widgetsets will implement their own versions
 ------------------------------------------------------------------------------}
function TWidgetSet.RectInRegion(RGN: HRGN; ARect: TRect): Boolean;
var
  RectRgn, EmptyRgn: HRGN;
begin
  with ARect do
    RectRgn := CreateRectRgn(Left,Top,Right,Bottom);
  try
    EmptyRgn := CreateEmptyRegion;
    try
      Result := CombineRgn(EmptyRgn, RectRgn, RGN, RGN_AND) <> NULLREGION;
      if Result then
        Result := CombineRgn(EmptyRgn, EmptyRgn, RectRgn, RGN_XOR) = NULLREGION;
    finally
      DeleteObject(EmptyRgn);
    end;
  finally
    DeleteObject(RectRgn);
  end;
end;

{------------------------------------------------------------------------------
  Function: RectVisible
  Params:  dc : hdc; ARect: TRect
  Returns: True if ARect is not completely clipped away.
 ------------------------------------------------------------------------------}
function TWidgetSet.RectVisible(DC: HDC; const ARect: TRect): Boolean;
var
  ClipRGN, RectRgn: hRGN;
  Intersection: hRGN;
  CombineResult: Integer;
begin
  Result := False;
  if (ARect.Left >= ARect.Right) or
     (ARect.Top >= ARect.Bottom) or
     not DCClipRegionValid(DC) then Exit;

  ClipRGN := CreateEmptyRegion;
  if GetClipRGN(DC, ClipRGN) > 0 then
  begin
    RectRgn := CreateRectRGN(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    Intersection := CreateEmptyRegion;
    //debugln(['TWidgetSet.RectVisible ARect=',dbgs(ARect)]);
    CombineResult := CombineRGN(Intersection, RectRGN, ClipRGN, RGN_AND);
    if CombineResult in [SimpleRegion, ComplexRegion] then
      Result := True;
    DeleteObject(Intersection);
    DeleteObject(RectRgn);
  end;
  DeleteObject(ClipRGN);
end;

function TWidgetSet.RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
begin
  // check if validate flag is set, we have no default for it.
  if RDW_VALIDATE and flags <> 0 then Exit(False);

  if hrgnUpdate = 0
  then Result := InvalidateRect(Wnd, lprcUpdate, RDW_ERASE and flags <> 0)
  else Result := InvalidateRgn(Wnd, hrgnUpdate, RDW_ERASE and flags <> 0)
end;

function TWidgetSet.ReleaseCapture : Boolean;
begin
  Result := True;
end;

function TWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := 0;
end;

function TWidgetSet.RemoveProp(Handle: hwnd; Str: PChar): THandle;
begin
  Result := 0;
end;

function TWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result := False;
end;

function TWidgetSet.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer) : Boolean;
var
  T: Integer;
  Points: PPoint;
  Count: Integer;

  procedure AddArcPoints(Left, Top, Right, Bottom, Angle1, Angle2: Integer);
  var
    P: PPoint;
    C: Integer;
    I: Integer;
  begin
    P := nil;
    try
      PolyBezierArcPoints(Left, Top, Right - Left, Bottom - Top, Angle1, Angle2,
        0, P, C);

      ReallocMem(Points, (Count + C) * SizeOf(TPoint));
      for I := 0 to Pred(C) do
        Points[Count + Pred(C) - I] := P[I];
        
      Inc(Count, C);
    finally
      FreeMem(P);
    end;
  end;
  
begin
  Result := True;

  if X2 < X1 then
  begin
    T := X1;
    X1 := X2;
    X2 := T;
  end;

  if Y2 < Y1 then
  begin
    T := Y1;
    Y1 := Y2;
    Y2 := T;
  end;

  if (X2 - X1 <= 0) or (Y2 - Y1 <= 0) then Exit;

  Dec(X2);
  Dec(Y2);

  if not ((RX <= 0) or (RY <= 0)) then
  begin
    if X2 - X1 < RX then RX := X2 - X1;
    if Y2 - Y1 < RY then RY := Y2 - Y1;

    //debugln('TWidgetSet.RoundRect ',dbgs(Rect(X1,Y1,X2,Y2)),' ',dbgs(Point(RX,RY)));
    Points := nil;
    Count := 0;
    try
      AddArcPoints(X1, Y1, X1 + RX, Y1 + RY,  90 * 16, 90 * 16);
      AddArcPoints(X2 - RX, Y1, X2, Y1 + RY,   0 * 16, 90 * 16);
      AddArcPoints(X2 - RX, Y2 - RY, X2, Y2, 270 * 16, 90 * 16);
      AddArcPoints(X1, Y2 - RY, X1 + RX, Y2, 180 * 16, 90 * 16);

      Polygon(DC, Points, Count, False);
    finally
      FreeMem(Points);
    end;
  end
  else
    Rectangle(DC, X1, Y1, X2, Y2);
end;

function TWidgetSet.SaveDC(DC: HDC) : Integer;
begin
  Result := 0;
end;

function TWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := 0;
end;

function TWidgetSet.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  Result := ERROR;
end;

function TWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result := 0;
end;

function TWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Result := 0;
end;

function TWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
begin
  Result := 0;
end;

function TWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  Result:=GetActiveWindow;
end;

function TWidgetSet.SetBkColor(DC: HDC; Color: TColorRef): TColorRef; //pbd
begin
  Result := 0;
end;

function TWidgetSet.SetBkMode(DC: HDC; bkMode : Integer): Integer;
begin
  Result := 0;
end;

function TWidgetSet.SetCapture(AHandle: HWND): HWND;
begin
  Result := 0;
End;

function TWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetCaretPosEx(handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetCursor(hCursor: HICON): HCURSOR;
begin
  Result := 0;
end;

function TWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  // Your default here
  Result := False;
end;

function TWidgetSet.SetFocus(hWnd: HWND): HWND;
begin
  Result := 0;
end;

function TWidgetSet.SetForegroundWindow(hWnd : HWND): Boolean;
begin
  Result := false;
end;

function TWidgetSet.SetMapMode(DC: HDC; fnMapMode : Integer): Integer;
begin
  Result := 0;
end;

function TWidgetSet.SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
begin
  Result := 0;
end;

function TWidgetSet.SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
begin
  Result := True;
end;

function TWidgetSet.SetRectRgn(aRGN: HRGN; X1, Y1, X2, Y2 : Integer): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
begin
  Result := 0;
end;

function TWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
begin
  Result := 0;
end;

function TWidgetSet.SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer;
begin
  Result := 0;
end;

function TWidgetSet.SetSysColors(cElements: Integer; const lpaElements;
  const lpaRgbValues): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  Result := 0;
end;

function TWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := CLR_INVALID;
end;

function TWidgetSet.SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetWindowLong(Handle: HWND;
  Idx: Integer; NewLong : PtrInt): PtrInt;
begin
  Result := -1;
end;

function TWidgetSet.SetWindowOrgEx(dc : hdc; NewX, NewY : Integer;
  OldPoint: PPoint) : Boolean;
begin
  Result := False;
end;

function TWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
       X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result:=false;
end;

function TWidgetset.SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean): longint;
begin
  Result := 0;
end;

function TWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := False;
end;

function TWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result := False;
end;

function TWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result:=false;
end;

function TWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  Result := False;
end;

function TWidgetSet.StretchDIBits(DC: HDC;
  DestX, DestY, DestWidth, DestHeight, SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  Bits: Pointer; var BitsInfo: TBitmapInfo; Usage: UINT; Rop: DWORD): Integer;
begin
  Result := 0;
end;

function TWidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  case uiAction of
    SPI_GETWHEELSCROLLLINES: begin
      PDword(pvPAram)^ := 3; // default value
      Result := True;
    end;
  else
    Result := False;
  end
end;

function TWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result := false;
end;

function TWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
  Result := false;
end;

function TWidgetSet.WindowFromPoint(Point : TPoint) : HWND;
begin
  Result := 0;
end;

//##apiwiz##eps##   // Do not remove
