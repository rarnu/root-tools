program _profile_;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, sysutils, math, FileUtil, process, strutils;

function IsNewBiggerThanOld(old: string; new: string): Boolean;
var
  oldP, newP: Integer;
  oldVer, newVer: Integer;
  oldRemain, newRemain: string;
  i, len: Integer;
begin
  Result := False;
  oldP:= old.IndexOf('_');
  newP:= new.IndexOf('_');
  oldVer:= StrToInt(old.Substring(0, oldP));
  newVer:= StrToInt(new.Substring(0, newP));

  oldRemain:= old.Substring(oldP + 1);
  newRemain:= new.Substring(newP + 1);
  if (newVer <> oldVer) then begin
    Exit(newVer > oldVer);
  end;
  len := Min(Length(oldRemain), Length(newRemain));
  for i := 1 to len do begin
    if (newRemain[i] > oldRemain[i]) then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure CopyProfile(pkg: string; ver: string);
var
  path: string;
  newPath: string;
  v: string;
begin
  newPath := ExtractFilePath(ParamStr(0)) + 'profile/' + pkg + '/';
  if (not DirectoryExists(newPath)) then begin
    ForceDirectories(newPath);
  end;
  path:= ExtractFilePath(ParamStr(0)) + 'upfile/' + pkg + '/' + ver;
  v := ver.Substring(0, ver.IndexOf('_'));
  newPath += v + '.lst';
  CopyFile(path, newPath, [cffOverwriteFile]);
end;

procedure FindAllVersion(pkg: string; path: string);
var
  src: TSearchRec;
  currentVer: string = '';
begin
  if FindFirst(path + '*.lst', faAnyFile, src) = 0 then begin
    repeat
      if (src.Name = '..') or (src.Name = '.') then begin
        Continue;
      end;
      if (currentVer = '') then begin
        currentVer:= src.Name;
      end else begin
        if (IsNewBiggerThanOld(currentVer, src.Name)) then begin
          currentVer:= src.Name;
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
  CopyProfile(pkg, currentVer);
end;

procedure DeleteAllPackages();
var
  path: string;
begin
  // delete
  path := ExtractFilePath(ParamStr(0)) + 'upfile/';
  DeleteDirectory(path, True);
end;

procedure FindAllPackages();
var
  src: TSearchRec;
  path: string;
begin
  path:= ExtractFilePath(ParamStr(0)) + 'upfile/';
  if FindFirst(path + '*', faDirectory, src) = 0 then begin
    repeat
      if (src.Name = '..') or (src.Name = '.') then begin
        Continue;
      end;
      FindAllVersion(src.Name, path + src.Name + '/');
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure Chmod();
var
  path: string;
  outstr: string;
begin
  path:= ExtractFilePath(ParamStr(0)) + 'profile';
  RunCommand('chmod', ['-R', '777', path], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
end;

begin
  FindAllPackages();
  Chmod();
  DeleteAllPackages();
end.

