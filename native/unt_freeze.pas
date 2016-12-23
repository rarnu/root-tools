unit unt_freeze;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  FREEZE_FILE_PATH: string;

procedure updateFreezeList(pkg: string; comp: string; enabled: boolean);
procedure freezeOnLoad(pkg: string);

implementation

procedure updateFreezeList(pkg: string; comp: string; enabled: boolean);
begin
  // TODO: update freeze list

end;

procedure freezeOnLoad(pkg: string);
begin
  // TODO: freeze on load
end;

end.

