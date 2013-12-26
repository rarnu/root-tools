unit unt_command;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, process;

procedure ExecuteCommandP(cmd, path: string);
function ExecuteCommandF(cmd, path: string): TStringList;

implementation

procedure ExecuteCommandP(cmd, path: string);
var
    AProcess: TProcess;
begin
    AProcess := TProcess.Create(nil);
    AProcess.CurrentDirectory := path;
    AProcess.CommandLine := cmd;
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    AProcess.ShowWindow := swoNone;
    try
        AProcess.Execute;
    except
        on Ex: Exception do
        begin
            WriteLn('');
            WriteLn('Error: ' + Ex.Message);
            WriteLn('');
        end;
    end;
    AProcess.Free;
end;

function ExecuteCommandF(cmd, path: string): TStringList;
var
    AProcess: TProcess;
begin
    WriteLn(cmd);
    AProcess := TProcess.Create(nil);
    Result := TStringList.Create;
    AProcess.CurrentDirectory := path;
    AProcess.CommandLine := cmd;
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    AProcess.ShowWindow := swoNone;
    try
        AProcess.Execute;
        Result.LoadFromStream(AProcess.Output);
    except
        on Ex: Exception do
        begin
            WriteLn('');
            WriteLn('Error: ' + Ex.Message);
            WriteLn('');
        end;
    end;
    AProcess.Free;
end;


end.

