unit unt_ljava;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_cmd, strutils;

// JNI real method
function FreezeApplication(packageName: String; isFreezed: Boolean): Boolean;

implementation

function FreezeApplication(packageName: String; isFreezed: Boolean): Boolean;
var
  cmd: string;
  outstr: string;
begin
  cmd := Format('pm %s %s', [IfThen(isFreezed, 'disable', 'enable'), packageName]);
  Result := internalRun([cmd], outstr);
end;

end.

