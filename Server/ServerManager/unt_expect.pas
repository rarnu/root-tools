unit unt_expect;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, unt_config, unt_server;

procedure BuildCommandScript(ACmd: array of string; AFileName: string; AConfig: TConfig);
procedure ChmodScript(AFileName: string; AConfig: TConfig);


implementation

procedure BuildCommandScript(ACmd: array of string; AFileName: string; AConfig: TConfig);
var
    shellScript: TStringList;
    cmdList: string;
    i: integer;
begin
    cmdList := '';
    for i := 0 to Length(ACmd) - 1 do
    begin
        cmdList := cmdList + ' "' + ACmd[i] + '" ';
    end;
    shellScript := TStringList.Create;
    shellScript.Add('#!/usr/bin/expect'#10);
    shellScript.Add('set password ' + AConfig.UserPassword + #10);
    shellScript.Add('spawn ssh ' + AConfig.UserName + '@' + AConfig.ServerAddress + cmdList);
    shellScript.Add('expect {'#10);
    shellScript.Add('   "password:" { set timeout 500; send "$password\r" }');
    shellScript.Add('   "yes/no" { set timeout 500; send "yes\r"; exp_continue; }');
    shellScript.Add('}'#10);
    shellScript.Add('expect eof'#10);
    shellScript.SaveToFile(ExtractFilePath(ParamStr(0)) + 'script/' + AFileName);
    shellScript.Free;
end;

procedure ChmodScript(AFileName: string; AConfig: TConfig);
var
    path: string;
    scriptPath: string;
    cmd: string;
begin
    path := ExtractFilePath(ParamStr(0)) + 'script/';
    scriptPath := path + AFileName;
    cmd := path + 'chmod.sh "' + AConfig.RootPassword + '" "' + scriptPath + '"';
    ExecuteCmd(cmd);
end;

end.

