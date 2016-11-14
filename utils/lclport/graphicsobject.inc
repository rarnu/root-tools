{%MainUnit ../controls.pp}
{******************************************************************************
                               TGraphicsObject
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

procedure TGraphicsObject.DoChange(var msg);
begin
  Changed;
end;

procedure TGraphicsObject.Changing;
begin
  //DebugLn(Format('Trace:[TgraphicsObject.Changing] %s', [ClassName]));
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TGraphicsObject.Changed;
begin
  //DebugLn(Format('Trace:[TgraphicsObject.Changed] %s', [ClassName]));
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphicsObject.Lock;
begin

end;

procedure TGraphicsObject.UnLock;
begin

end;
