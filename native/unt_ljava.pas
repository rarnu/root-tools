unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils;

// JNI real method
function FreezeApplication(packageName: String; isFreezed: Boolean): Boolean;
function FreeComponents(packageName: string; components: TStringArray; isFreezed: Boolean): Boolean;

implementation

function FreezeApplication(packageName: String; isFreezed: Boolean): Boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s', [IfThen(isFreezed, 'disable', 'enable'), packageName]);
  Result := internalRun([cmd], outstr);
end;

function FreeComponents(packageName: string; components: TStringArray;
  isFreezed: Boolean): Boolean;
var
  cmd: string = '';
  outstr: string;
  i: Integer;
begin
  for i := 0 to Length(components) - 1 do begin
    cmd += Format('pm %s %s/%s;', [IfThen(isFreezed, 'disable', 'enable'), packageName, components[i]]);
  end;
  Result := internalRun([cmd], outstr);
end;

end.

