{%MainUnit ../interfacebase.pp}
{ $Id: interfacebase.inc 41389 2013-05-25 09:22:25Z juha $
******************************************************************************
                                  TWidgetSet

                                   WinApi stuff


  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
 *  This file is part of the Lazarus Component Library (LCL)
 *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,
 *  for details about the license.
 *****************************************************************************
}

constructor TWidgetSet.Create;
begin
  inherited Create;
  FThemeServices := CreateThemeServices;
end;

procedure TWidgetSet.BeforeDestruction;
begin
  FreeAndNil(FThemeServices); // destroy before destructor to release resources
  inherited BeforeDestruction;
end;

function TWidgetSet.GetAppHandle: THandle;
begin
  Result := 0;
end;

procedure TWidgetSet.SetAppHandle(const AValue: THandle);
begin

end;

procedure TWidgetSet.PassCmdLineOptions;
begin
end;

function TWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TThemeServices.Create;
end;

procedure TWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  if Assigned(ALoop) then ALoop;
end;

procedure TWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
end;

procedure TWidgetSet.AppSetTitle(const ATitle: string);
begin
end;

procedure TWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
end;

function TWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  Result := False;
end;

function TWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  Result := False;
end;

procedure TWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
end;

function TWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint,
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon,
    lcModalWindow,
    lcReceivesLMClearCutCopyPasteReliably,
    lcEmulatedMDI:
      Result := LCL_CAPABILITY_YES;
    lcSendsUTF8KeyPress:
      Result := LCL_CAPABILITY_YES;
    lcAccessibilitySupport: Result := LCL_CAPABILITY_NO;
  else
    Result := LCL_CAPABILITY_NO;
  end;
end;

procedure TWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
end;

procedure TWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

