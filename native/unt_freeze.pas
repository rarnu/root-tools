unit unt_freeze;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, android;

const
  FREEZE_FILE_PATH = '/data/data/com.rarnu.tools.neo/files/';

procedure updateFreezeList(pkg: string; comp: string; enabled: boolean);
procedure freezeOnLoad();

implementation

uses unt_cmd, unt_ljava;

procedure updateFreezeList(pkg: string; comp: string; enabled: boolean);
var
  filePath: string;
  i: Integer;
  findComp: Boolean;
  cmd: string;
  outstr: string;
begin
  // update freeze list
  if (comp.Trim = '') then begin
    // pkg
    filePath:= FREEZE_FILE_PATH + 'pkg_' + pkg;
    if enabled then begin
      if (FileExists(filePath)) then DeleteFile(filePath);
    end else begin
      if (not FileExists(filePath)) then begin
        with TStringList.Create do begin
          SaveToFile(filePath);
          Free;
        end;
      end;
    end;
  end else begin
    // comp
    filePath:= FREEZE_FILE_PATH + 'cmp_' + pkg;
    with TStringList.Create do begin
      if (FileExists(filePath)) then LoadFromFile(filePath);
      if not enabled then begin
        findComp:= False;
        for i := 0 to Count - 1 do begin
          if Strings[i].Trim = comp then begin
            findComp:= True;
            break;
          end;
        end;
        if (not findComp) then begin
          Add(comp);
        end;
      end else begin
        for i:= 0 to Count - 1 do begin
          if Strings[i].Trim = comp then begin
            Delete(i);
            Break;
          end;
        end;
      end;
      SaveToFile(filePath);
      Free;
    end;
  end;
  cmd := Format('chmod -R 777 %s', [FREEZE_FILE_PATH]);
  internalRun([cmd], outstr);
  LOGE(PChar(outstr));
end;

procedure freezeOnLoad;
var
  pkg: string;
  i: Integer;
  sn: string;
  src: TSearchRec;
begin
  // freeze on load
  if FindFirst(FREEZE_FILE_PATH + '*', faAnyFile, src) = 0 then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      sn := src.Name;
      if (sn.StartsWith('pkg_')) then begin
        pkg := sn.Replace('pkg_', '');
        FreezeApplication(pkg, True);
        LOGE(PChar(Format('freezeOnLoad => %s', [pkg])));
      end else if (sn.StartsWith('cmp_')) then begin
        pkg := sn.Replace('cmp_', '');
        with TStringList.Create do begin
          LoadFromFile(FREEZE_FILE_PATH + src.Name);
          for i := 0 to Count - 1 do begin
            if (Strings[i].Trim <> '') then begin
              FreezeComponent(pkg, Strings[i], True);
              LOGE(PChar(Format('freezeOnLoad => %s/%s', [pkg, Strings[i]])));
            end;
          end;
          Free;
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

end.

