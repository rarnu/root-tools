
uses
  lCommon, WinSock2, Win32Int, Win32Def, avglvltree; // for the guy below

const
  ALL_FLAGS = FD_ACCEPT or FD_READ or FD_CLOSE or FD_CONNECT or FD_WRITE;

var
  WaitSockets: TPointerToPointerTree;

procedure TLCLEventer.HandleIgnoreError(aHandle: TLHandle);
begin
  // don't do anything
end;

procedure TLCLEventer.HandleIgnoreWrite(aHandle: TLHandle);
begin
  // don't do anything
end;

procedure TLCLEventer.HandleIgnoreRead(aHandle: TLHandle);
begin
  // don't do anything
end;

function TLCLEventer.HandleEvents(aHandle: THandle; aFlags: DWord): LongInt;
var
  Temp: TLHandle;
  buf: Byte;
begin
  Result := 0;
  
  if WaitSockets.Contains(Pointer(aHandle)) then
    Temp := TLHandle(WaitSockets[Pointer(aHandle)])
  else
    Exit;
  
  if not FInLoop then begin
    FInLoop := True;

    if aFlags and FD_CONNECT = FD_CONNECT then
      if not Temp.Dispose and Assigned(Temp.OnWrite) then
        Temp.OnWrite(Temp);

    if aFlags and FD_READ  = FD_READ then
      if not Temp.Dispose and Assigned(Temp.OnRead) then begin
        Temp.OnRead(Temp);
        recv(Temp.Handle, @buf, SizeOf(buf), MSG_PEEK);
      end;

    if aFlags and FD_CLOSE = FD_CLOSE then
      if not Temp.Dispose and Assigned(Temp.OnRead) then
        Temp.OnRead(Temp);

    if aFlags and FD_ACCEPT = FD_ACCEPT then
      if not Temp.Dispose and Assigned(Temp.OnRead) then
        Temp.OnRead(Temp);

    if aFlags and FD_WRITE  = FD_WRITE then
      if not Temp.Dispose and Assigned(Temp.OnWrite) then
        Temp.OnWrite(Temp);

    if Temp.Dispose then
      AddForFree(Temp);

    FInLoop := False;

    if Assigned(FFreeRoot) then
      FreeHandles;
  end;
end;

constructor TLCLEventer.Create;
begin
  inherited;
  TWin32WidgetSet(WidgetSet).OnAsyncSocketMsg := @HandleEvents;
end;

function TLCLEventer.AddHandle(aHandle: TLHandle): Boolean;
begin
  Result := True;
  SetHandleEventer(aHandle);
  WaitSockets.Values[Pointer(aHandle.Handle)] := aHandle;
  if WSAAsyncSelect(aHandle.Handle, TWin32WidgetSet(WidgetSet).AppHandle,
                                         WM_LCL_SOCK_ASYNC, ALL_FLAGS) <> 0 then
    Bail('Error on AsyncSelect', WSAGetLastError);
end;

procedure TLCLEventer.UnregisterHandle(aHandle: TLHandle);
begin
  if WaitSockets.Contains(Pointer(aHandle.Handle)) then begin
    WaitSockets.Remove(Pointer(aHandle.Handle));

    if WSAAsyncSelect(aHandle.Handle, TWin32WidgetSet(WidgetSet).AppHandle,
                                           0, 0) <> 0 then
      Bail('Error on AsyncSelect', WSAGetLastError);
  end;
end;

procedure TLCLEventer.InternalUnplugHandle(aHandle: TLHandle);
begin
  UnregisterHandle(aHandle);
  inherited InternalUnplugHandle(aHandle);
end;

initialization
  WaitSockets := TPointerToPointerTree.Create;

finalization
  WaitSockets.Free;


