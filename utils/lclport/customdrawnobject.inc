{%MainUnit customdrawnint.pas}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//---------------------------------------------------------------

{$IFnDEF WithOldDebugln}

{$ELSE}
procedure TCDWidgetSet.AccumulatingDebugOut(AStr: string);
begin
  AccumulatedStr := AccumulatedStr + AStr;
end;
{$ENDIF}

function TCDWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TCDThemeServices.Create;
end;

procedure TCDWidgetSet.GenericAppInit;
begin

  // Init stock objects
  FStockBlackBrush := TFPCustomBrush.Create;
  FStockBlackBrush.FPColor := colBlack;
  FStockDKGrayBrush := TFPCustomBrush.Create;
  FStockDKGrayBrush.FPColor := colDkGray;
  FStockGrayBrush := TFPCustomBrush.Create;
  FStockGrayBrush.FPColor := colGray;
  FStockLtGrayBrush := TFPCustomBrush.Create;
  FStockLtGrayBrush.FPColor := colLtGray;
  FStockNullBrush := TFPCustomBrush.Create;
  FStockNullBrush.Style := bsClear;
  FStockWhiteBrush := TFPCustomBrush.Create;
  FStockWhiteBrush.FPColor := colWhite;

  FStockBlackPen := TFPCustomPen.Create;
  FStockBlackPen.FPColor := colBlack;
  FStockNullPen := TFPCustomPen.Create;
  FStockNullPen.Style := psClear;
  FStockWhitePen := TFPCustomPen.Create;
  FStockWhitePen.FPColor := colWhite;

  FDefaultGUIFont := TFPCustomFont.Create;
end;

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCDWidgetSet.Create;
begin
  inherited Create;

  CDWidgetSet := Self;
  FTerminating := False;
  DefaultFontSize := 10;

  ActivityClassName := 'com/pascal/lcltest/LCLActivity';

  FClipBoardFormats := TStringList.Create;
  FClipBoardFormats.Add('foo'); // 0 is reserved
  FClipBoardFormats.Add('text/plain'); // 1 - text/plain should always be supported

  // To be resistent against backend issues
  CDWidgetset.ScreenFormat := clfARGB32;

  BackendCreate;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCDWidgetSet.Destroy;
begin
  BackendDestroy;

  FClipBoardFormats.Free;

  CDWidgetSet := nil;
  inherited Destroy;
end;

function TCDWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCustomDrawn;
end;

function TCDWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
    {$ifdef CD_Cocoa}
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon:
      Result := LCL_CAPABILITY_NO;
    {$endif}
    lcReceivesLMClearCutCopyPasteReliably: Result := LCL_CAPABILITY_NO;
    lcAntialiasingEnabledByDefault: Result := LCL_CAPABILITY_NO;
    lcAllowChildControlsInNativeControls: Result := LCL_CAPABILITY_YES
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;

function TCDWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  LazCanvas: TLazCanvas;
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  LazCanvas := TLazCanvas(CanvasHandle);
  
  Result := FPColorToTColor(LazCanvas.Colors[X, Y]);
end;

procedure TCDWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  LazCanvas: TLazCanvas;
begin
  if not IsValidDC(CanvasHandle) then Exit;
  LazCanvas := TLazCanvas(CanvasHandle);

  LazCanvas.Colors[X, Y] := TColorToFPColor(AColor);
end;

procedure TCDWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me
end;

procedure TCDWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
{var
  DC: TQtDeviceContext;}
begin
{  if IsValidDC(CanvasHandle) then
  begin
    if CanvasHandle = 1 then
      DC := QtDefaultContext
    else
      DC := TQtDeviceContext(CanvasHandle);
    DC.setRenderHint(QPainterAntialiasing, AEnabled);
  end;}
end;

procedure TCDWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

function CDMessageBoxFunction(Text, Caption : PChar; Flags : Longint) : Integer;
begin
  Result := CDWidgetset.MessageBox(0, Text, Caption, Flags);
end;

//------------------------------------------------------------------------
