{%MainUnit lazfileutils.pas}

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsUnixAbsolute(TheFilename);
end;

function FileOpenUTF8(const FileName: string; Mode: Integer): THandle;
begin
  Result := SysUtils.FileOpen(UTF8ToSys(FileName), Mode);
end;

function FileCreateUTF8(const FileName: string): THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName));
end;

function FileCreateUTF8(const FileName: string; Rights: Cardinal): THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName), Rights);
end;

function FileCreateUtf8(const FileName: String; ShareMode: Integer;
  Rights: Cardinal): THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName), ShareMode, Rights);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
  InvalidateFileStateCache(Filename);
end;

function FileExistsUTF8(const Filename: string): boolean;
begin
  Result:=SysUtils.FileExists(UTF8ToSys(Filename));
end;

function DirectoryExistsUTF8(const Directory: string): Boolean;
begin
  Result:=SysUtils.DirectoryExists(UTF8ToSys(Directory));
end;

function FileAgeUTF8(const FileName: string): Longint;
begin
  Result:=SysUtils.FileAge(UTF8ToSys(Filename));
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result := SysUtils.FileSetDate(UTF8ToSys(Filename), Age);
  InvalidateFileStateCache(Filename);
end;

function FileSizeUtf8(const Filename: string): int64;
var
  st: baseunix.stat;
  SysFileName: String;
begin
  SysFileName := Utf8ToSys(FileName);
  if not fpstat(pointer(SysFileName),st{%H-})>=0 then
    exit(-1);
  Result := st.st_size;
end;

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
var
  LinkFilename: string;
  AText: string;
  Depth: Integer;
begin
  Result:=Filename;
  Depth:=0;
  while Depth<12 do begin
    inc(Depth);
    LinkFilename:=FpReadLink(Result);
    if LinkFilename='' then begin
      AText:='"'+Filename+'"';
      case fpGetErrno() of
      ESysEAcces:
        AText:='read access denied for '+AText;
      ESysENoEnt:
        AText:='a directory component in '+AText
                            +' does not exist or is a dangling symlink';
      ESysENotDir:
        AText:='a directory component in '+AText+' is not a directory';
      ESysENoMem:
        AText:='insufficient memory';
      ESysELoop:
        AText:=AText+' has a circular symbolic link';
      else
        // not a symbolic link, just a regular file
        exit;
      end;
      if (not ExceptionOnError) then begin
        Result:='';
        exit;
      end;
      raise EFOpenError.Create(AText);
    end else begin
      if not FilenameIsAbsolute(LinkFilename) then
        Result:=ResolveDots(ExtractFilePath(Result)+LinkFilename)
      else
        Result:=LinkFilename;
    end;
  end;
  // probably an endless loop
  if ExceptionOnError then
    raise EFOpenError.Create('too many links, maybe an endless loop.')
  else
    Result:='';
end;

function GetPhysicalFilename(const Filename: string;
  OnError: TPhysicalFilenameOnError): string;
begin
  Result:=GetUnixPhysicalFilename(Filename,OnError=pfeException);
  if (Result='') and (OnError=pfeOriginal) then
    Result:=Filename;
end;

function GetUnixPhysicalFilename(const Filename: string;
  ExceptionOnError: boolean): string;
var
  OldPath: String;
  NewPath: String;
  p: PChar;
begin
  Result:=Filename;
  p:=PChar(Result);
  repeat
    while p^='/' do
      inc(p);
    if p^=#0 then exit;
    if p^<>'/' then
    begin
      repeat
        inc(p);
      until p^ in [#0,'/'];
      OldPath:=LeftStr(Result,p-PChar(Result));
      NewPath:=ReadAllLinks(OldPath,ExceptionOnError);
      if NewPath='' then exit('');
      if OldPath<>NewPath then
      begin
        Result:=NewPath+copy(Result,length(OldPath)+1,length(Result));
        p:=PChar(Result)+length(NewPath);
      end;
    end;
  until false;
  Result:=ResolveDots(Result);
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SysUtils.CreateDir(UTF8ToSys(NewDir));
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result:=SysUtils.RemoveDir(UTF8ToSys(Dir));
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result:=SysUtils.DeleteFile(UTF8ToSys(Filename));
  if Result then
    InvalidateFileStateCache;
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result:=SysUtils.RenameFile(UTF8ToSys(OldName),UTF8ToSys(NewName));
  if Result then
    InvalidateFileStateCache;
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(UTF8ToSys(NewDir));
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
begin
  Result:=SysUtils.FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Rslt.Name:=UTF8ToSys(Rslt.Name);
  Result:=SysUtils.FindNext(Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;


function ExpandFileNameUTF8(const FileName: string; BaseDir: string): string;
var
  IsAbs: Boolean;
  CurDir, HomeDir, Fn: String;
begin
  Fn := FileName;
  ForcePathDelims(Fn);
  IsAbs := FileNameIsUnixAbsolute(Fn);
  if (not IsAbs) then
  begin
    CurDir := GetCurrentDirUtf8;
    if ((Length(Fn) > 1) and (Fn[1] = '~') and (Fn[2] = '/')) or (Fn = '~') then
    begin
      HomeDir := SysToUtf8(GetEnvironmentVariable('HOME'));
      if not FileNameIsUnixAbsolute(HomeDir) then
        HomeDir := ExpandFileNameUtf8(HomeDir,'');
      Fn := HomeDir + Copy(Fn,2,length(Fn));
      IsAbs := True;
    end;
  end;
  if IsAbs then
  begin
    Result := ResolveDots(Fn);
  end
  else
  begin
    if (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    Fn := ResolveDots(Fn);
    //if BaseDir is not absolute then this needs to be expanded as well
    if not FileNameIsUnixAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;

function GetCurrentDirUTF8: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;

function FileIsExecutable(const AFilename: string): boolean;
var
  Info : Stat;
begin
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info{%H-})<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
var
  AText: String;
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if DirPathExists(AFilename) then begin
    raise Exception.Create(Format(lrsFileIsADirectoryAndNotAnExecutable, [
      AFilename]));
  end;
  if BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)<>0 then
  begin
    AText:='"'+AFilename+'"';
    case fpGetErrno() of
    ESysEAcces:
      AText:=Format(lrsReadAccessDeniedFor, [AText]);
    ESysENoEnt:
      AText:=Format(lrsADirectoryComponentInDoesNotExistOrIsADanglingSyml, [
        AText]);
    ESysENotDir:
      AText:=Format(lrsADirectoryComponentInIsNotADirectory, [Atext]);
    ESysENoMem:
      AText:=lrsInsufficientMemory;
    ESysELoop:
      AText:=Format(lrsHasACircularSymbolicLink, [AText]);
    else
      AText:=Format(lrsIsNotExecutable, [AText]);
    end;
    raise Exception.Create(AText);
  end;
  // ToDo: xxxbsd
end;

function FileIsSymlink(const AFilename: string): boolean;
begin
  Result := FpReadLink(AFilename) <> '';
end;

procedure CheckIfFileIsSymlink(const AFilename: string);
var
  AText: string;
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if FpReadLink(AFilename)='' then begin
    AText:='"'+AFilename+'"';
    case fpGetErrno() of
    ESysEAcces:
      AText:=Format(lrsReadAccessDeniedFor, [AText]);
    ESysENoEnt:
      AText:=Format(lrsADirectoryComponentInDoesNotExistOrIsADanglingSyml2, [
        AText]);
    ESysENotDir:
      AText:=Format(lrsADirectoryComponentInIsNotADirectory2, [Atext]);
    ESysENoMem:
      AText:=lrsInsufficientMemory;
    ESysELoop:
      AText:=Format(lrsHasACircularSymbolicLink, [AText]);
    else
      AText:=Format(lrsIsNotASymbolicLink, [AText]);
    end;
    raise Exception.Create(AText);
  end;
end;

function FileIsHardLink(const AFilename: string): boolean;
var
  H: THandle;
  FileInfo: stat;
begin
  Result := false;
  H := FileOpenUtf8(aFilename, fmOpenRead);
  if H <> feInvalidHandle then
  begin
    if FPFStat(H, FileInfo{%H-}) = 0 then
      Result := (FileInfo.st_nlink > 1);
    FileClose(H);
  end;
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result := (BaseUnix.FpAccess(AFilename, BaseUnix.W_OK) = 0);
end;


function IsUNCPath(const Path: String): Boolean;
begin
  Result := false;
end;

function ExtractUNCVolume(const Path: String): String;
begin
  Result := '';
end;

function GetFileDescription(const AFilename: string): string;
var
  info: Stat;
  // permissions
  // user
  // group
  // size
  // date
  // time
  mode: mode_t;
begin
  Result:='';
  if not (FpStat(AFilename,info{%H-})=0) then exit;

  // permissions
  // file type
  mode:= info.st_mode;
  if STAT_IFLNK and mode=STAT_IFLNK then
    Result:=Result+'l'
  else
  if STAT_IFDIR and mode=STAT_IFDIR then
    Result:=Result+'d'
  else
  if STAT_IFBLK and mode=STAT_IFBLK then
    Result:=Result+'b'
  else
  if STAT_IFCHR and mode=STAT_IFCHR then
    Result:=Result+'c'
  else
    Result:=Result+'-';
  // user permissions
  if STAT_IRUSR and mode=STAT_IRUsr then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWUsr and mode=STAT_IWUsr then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXUsr and mode=STAT_IXUsr then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // group permissions
  if STAT_IRGRP and mode=STAT_IRGRP then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWGRP and mode=STAT_IWGRP then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXGRP and mode=STAT_IXGRP then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // other permissions
  if STAT_IROTH and mode=STAT_IROTH then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWOTH and mode=STAT_IWOTH then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXOTH and mode=STAT_IXOTH then
    Result:=Result+'x'
  else
    Result:=Result+'-';


  // user name
  //Result:=Result+' Owner: '+IntToStr(info.uid)+'.'+IntToStr(info.gid);

  // size
  Result:=Result+lrsSize+IntToStr(info.st_size);

  // date + time
  Result:=Result+lrsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result:=Result+'?';
  end;
end;


function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
begin
  Result := SysToUTF8(SysUtils.GetAppConfigDir(Global));
  if Result = '' then exit;
  if Create and not ForceDirectoriesUTF8(Result) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Result]));
end;

function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean;
  CreateDir: boolean): string;
var
  Dir: string;
begin
  Result := SysToUTF8(SysUtils.GetAppConfigFile(Global,SubDir));
  if not CreateDir then exit;
  Dir := ExtractFilePath(Result);
  if Dir = '' then exit;
  if not ForceDirectoriesUTF8(Dir) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
end;

function GetShellLinkTarget(const FileName: string): string;
begin
  Result := Filename;
end;

procedure InitLazFileUtils;
begin
  //dummy
end;

procedure FinalizeLazFileUtils;
begin
  //dummy
end;
