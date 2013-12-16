unit unt_server;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, process;

function ExecuteScript(AScript: string): TStringList;
function ExecuteCmd(ACmd: string): TStringList;

implementation

function Execute(AScript: string): TStringList;
const
    READ_BYTES = 2048;
var
    memoryStream: TMemoryStream;
    proc: TProcess;
    numBytes: longint;
    bytesRead: longint;
    cmd: string;
begin
    memoryStream := TMemoryStream.Create;
    bytesRead := 0;
    proc := TProcess.Create(nil);
    cmd := ExtractFilePath(ParamStr(0)) + 'script/' + AScript;
    WriteLn(cmd);
    proc.CommandLine := cmd;
    proc.CurrentDirectory := ExtractFilePath(ParamStr(0));
    proc.Options := [poUsePipes];
    proc.Execute;
    while True do
    begin
        memoryStream.SetSize(bytesRead + READ_BYTES);
        numBytes := proc.Output.Read((memoryStream.Memory + bytesRead)^, READ_BYTES);
        if numBytes > 0 then
        begin
            Inc(bytesRead, numBytes);
        end
        else
        begin
            Break;
        end;
    end;
    memoryStream.SetSize(bytesRead);
    Result := TStringList.Create;
    Result.LoadFromStream(memoryStream);
    proc.Free;
    memoryStream.Free;
end;

function ExecuteScript(AScript: string): TStringList;
begin

end;

function ExecuteCmd(ACmd: string): TStringList;
var
    proc: TProcess;
begin
    WriteLn(ACmd);
    proc := TProcess.Create(nil);
    proc.CurrentDirectory := ExtractFilePath(ParamStr(0));
    proc.CommandLine := ACmd;
    proc.Options := [poWaitOnExit, poUsePipes];
    proc.Execute;
    Result := TStringList.Create;
    Result.LoadFromStream(proc.Output);
    proc.Free;
end;

end.



