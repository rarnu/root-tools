unit unt_command;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

procedure ExecuteCommandP(cmd, path: string);
function ExecuteCommandF(cmd, path: string): TStringList;
procedure ExecuteCommandT(cmd, path: string);
procedure ExecuteCommandH(cmd, path: string);

implementation

procedure ExecuteCommandP(cmd, path: string);
var
  AProcess: TProcess;
begin
  {$IFNDEF WINDOWS}
  WriteLn(cmd);
  {$ENDIF}
  AProcess := TProcess.Create(nil);
  AProcess.CurrentDirectory := path;
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.ShowWindow := {$IFDEF WINDOWS} swoHIDE {$ELSE} swoNone {$ENDIF};
  try
    AProcess.Execute;
  except
    on Ex: Exception do
    begin
      {$IFNDEF WINDOWS}
      WriteLn('Error: ' + Ex.Message);
      {$ENDIF}
    end;
  end;
  AProcess.Free;
end;

function ExecuteCommandF(cmd, path: string): TStringList;
var
  AProcess: TProcess;
begin
  {$IFNDEF WINDOWS}
  WriteLn(cmd);
  {$ENDIF}
  AProcess := TProcess.Create(nil);
  Result := TStringList.Create;
  AProcess.CurrentDirectory := path;
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.ShowWindow := {$IFDEF WINDOWS} swoHIDE {$ELSE} swoNone {$ENDIF};
  try
    AProcess.Execute;
    Result.LoadFromStream(AProcess.Output);
  except
    on Ex: Exception do
    begin
      {$IFNDEF WINDOWS}
      WriteLn('Error: ' + Ex.Message);
      {$ENDIF}
    end;
  end;
  AProcess.Free;
end;

procedure ExecuteCommandT(cmd, path: string);
var
  AProcess: TProcess;
begin
  {$IFNDEF WINDOWS}
  WriteLn(cmd);
  {$ENDIF}
  AProcess := TProcess.Create(nil);
  AProcess.CurrentDirectory := path;
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poNewConsole];
  AProcess.ShowWindow := swoShowNormal;
  try
    AProcess.Execute;
  except
    on Ex: Exception do
    begin
      {$IFNDEF WINDOWS}
      WriteLn('Error: ' + Ex.Message);
      {$ENDIF}
    end;
  end;
  AProcess.Free;
end;

procedure ExecuteCommandH(cmd, path: string);
var
  AProcess: TProcess;
begin
  {$IFNDEF WINDOWS}
  WriteLn(cmd);
  {$ENDIF}
  AProcess := TProcess.Create(nil);
  AProcess.CurrentDirectory := path;
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poNewProcessGroup, poNoConsole];
  AProcess.ShowWindow := swoShowNormal;
  try
    AProcess.Execute;
  except
    on Ex: Exception do
    begin
      {$IFNDEF WINDOWS}
      WriteLn('Error: ' + Ex.Message);
      {$ENDIF}
    end;
  end;
  AProcess.Free;
end;


end.

