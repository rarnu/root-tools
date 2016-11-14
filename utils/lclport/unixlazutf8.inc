{%MainUnit lazutf8.pas}

function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
begin
  Result := SysToUTF8(S);
end;

function UTF8ToConsole(const s: string): string;
begin
  Result := UTF8ToSys(s);
end;

function WinCPToUTF8(const s: string): string;
begin
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
end;

function UTF8ToWinCP(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=UTF8ToAnsi(s)
  else
    Result:=s;
end;

function ParamStrUTF8(Param: Integer): string;
begin
  Result:=SysToUTF8(ObjPas.ParamStr(Param));
end;

procedure InitLazUtf8;
begin
  //dummy procedure
end;

procedure FinalizeLazUTF8;
begin
  //dummy procedure
end;

