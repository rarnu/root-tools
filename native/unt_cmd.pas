unit unt_cmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, jni2, jni_utils, android;

const
  _ED = #10; // \n

type
  { TCommandResult }

  TCommandResult = class
  private
    FEnv: PJNIEnv;
    FErrorString: string;
    FResultString: string;
  public
    constructor Create(j: PJNIEnv; r: string; e: string);
    function mapping: jobject;
  public
    property ResultString: string read FResultString write FResultString;
    property ErrorString: string read FErrorString write FErrorString;
  end;

var
  SU_STYLE: Integer = -1;

function internalRun(cmds: array of String; out resultString: string): Boolean;

implementation

function GetSuStyle: Integer;
var
  outstr: string;
begin
  Result := -1;
  RunCommand('su', ['--help'], outstr, [poUsePipes, poWaitOnExit, poStderrToOutPut]);
  if (outstr.Contains('not found')) then begin
    Exit;
  end;
  if (outstr.Contains('-c,')) or (outstr.Contains('-c ')) then begin
    Result := 0;
  end else begin
    Result := 1;
  end;
end;

function internalRun(cmds: array of String; out resultString: string): Boolean;
var
  i: Integer;
  r: Boolean;
  outstr: string;
  params: array[0..1] of String;
begin
  if SU_STYLE = -1 then begin
    SU_STYLE := GetSuStyle;
  end;
  Result := True;
  resultString:= '';
  for i := 0 to Length(cmds) - 1 do begin
    if SU_STYLE = 0 then begin
      params[0] := '-c';
    end else begin
      params[0] := '0';
    end;
    params[1] := cmds[i];
    r := RunCommand('su', params, outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
    resultString += outstr +  _ED;
    Result := Result and r;
  end;
end;

{ TCommandResult }

constructor TCommandResult.Create(j: PJNIEnv; r: string; e: string);
begin
  FEnv:= j;
  FResultString:= r;
  FErrorString:= e;
end;

function TCommandResult.mapping: jobject;
var
  cls: jclass;
  clsInit: jmethodID;
  obj: jobject;
begin
  cls := FEnv^^.FindClass(FEnv, 'com/rarnu/tools/neo/api/CommandResult');
  clsInit:= FEnv^^.GetMethodID(FEnv, cls, '<init>', '(Ljava/lang/String;Ljava/lang/String;)V');
  obj := FEnv^^.NewObjectA(FEnv, cls, clsInit, argsToJValues(FEnv, [FResultString, FErrorString]));
  Result := obj;
end;

end.

