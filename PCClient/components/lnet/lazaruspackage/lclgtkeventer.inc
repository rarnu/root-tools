
uses
  lCommon;

const
  G_IO_IN   =  1;
  G_IO_OUT  =  4;
  G_IO_PRI  =  2;
  G_IO_ERR  =  8;
  G_IO_HUP  = 16;
  G_IO_NVAL = 32;

  ALL_FLAGS = G_IO_ERR or G_IO_NVAL or G_IO_HUP or G_IO_PRI or G_IO_IN or G_IO_OUT;

procedure TLCLEventer.HandleIgnoreError(aHandle: TLHandle);
begin
  // TODO fix or remove alltogether
end;

procedure TLCLEventer.HandleIgnoreWrite(aHandle: TLHandle);
var
  LHI: PLCLHandleInfo;
begin
  LHI := GetInternalData(aHandle);
  if aHandle.IgnoreWrite then
    LHI^.Flags := LHI^.Flags and (not G_IO_OUT)
  else
    LHI^.Flags := LHI^.Flags or G_IO_OUT;
  SetEventHandlerFlags(LHI^.EventHandle, LHI^.Flags);
end;

procedure TLCLEventer.HandleIgnoreRead(aHandle: TLHandle);
var
  LHI: PLCLHandleInfo;
begin
  LHI := GetInternalData(aHandle);
  if aHandle.IgnoreRead then
    LHI^.Flags := LHI^.Flags and (not G_IO_IN)
  else
    LHI^.Flags := LHI^.Flags or G_IO_IN;
  SetEventHandlerFlags(LHI^.EventHandle, LHI^.Flags);
end;

procedure TLCLEventer.HandleEvents(aData: PtrInt; aFlags: DWord);
var
  LHI: PLCLHandleInfo;
  Temp: TLHandle;
begin
  LHI := PLCLHandleInfo(aData);
  Temp := LHI^.Handle;
  if not FInLoop then begin
    FInLoop := True;

    if aFlags and G_IO_OUT = G_IO_OUT then
      if not Temp.Dispose and Assigned(Temp.OnWrite) then
        Temp.OnWrite(Temp);

    if aFlags and G_IO_IN = G_IO_IN then
      if not Temp.Dispose and Assigned(Temp.OnRead) then
        Temp.OnRead(Temp);

    if  (not Temp.Dispose)
    and ((aFlags and G_IO_ERR = G_IO_ERR)
      or (aFlags and G_IO_NVAL = G_IO_NVAL)
      or (aFlags and G_IO_HUP = G_IO_HUP)) then
      if Assigned(Temp.OnError) then
        Temp.OnError(Temp, 'Handle error' + LStrError(LSocketError));

    SetEventHandlerFlags(LHI^.EventHandle, LHI^.Flags);

    if Temp.Dispose then
      AddForFree(Temp);
    FInLoop := False;

    if Assigned(FFreeRoot) then
      FreeHandles;
  end else
    SetEventHandlerFlags(LHI^.EventHandle, 0);
end;

function TLCLEventer.AddHandle(aHandle: TLHandle): Boolean;
var
  LHI: PLCLHandleInfo;
begin
  Result := False;
  
  SetHandleEventer(aHandle);
  
  LHI := GetMem(SizeOf(TLCLHandleInfo));
  LHI^.Handle := aHandle;
  SetInternalData(aHandle, LHI);
  LHI^.EventHandle := AddEventHandler(aHandle.Handle, ALL_FLAGS,
                                    @HandleEvents, PtrUInt(LHI));
  LHI^.Flags := ALL_FLAGS;
  if not Assigned(LHI^.EventHandle) then
    Bail('Error on handler', -1)
  else
    Result  :=  True;
end;

procedure TLCLEventer.InternalUnplugHandle(aHandle: TLHandle);
var
  LHI: PLCLHandleInfo;
begin
  LHI := GetInternalData(aHandle);
  RemoveEventHandler(LHI^.EventHandle);
  FreeMem(LHI);
  inherited InternalUnplugHandle(aHandle);
end;


