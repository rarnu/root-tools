{%MainUnit lazfileutils.pas}



function ReadAllLinks(const Filename: string;
                      ExceptionOnError: boolean): string;
begin
  // not supported under Windows
  Result:=Filename;
end;

function GetPhysicalFilename(const Filename: string;
        OnError: TPhysicalFilenameOnError): string;
begin
  if OnError=pfeEmpty then ;
  Result:=Filename;
end;



// ******** Start of WideString specific implementations ************

const
  ShareModes: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);

  AccessModes: array[0..2] of Cardinal  = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);

function WinToDosTime(Var Wtime : TFileTime; var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft{%H-})
    {$ifndef WinCE}
    and FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo)
    {$endif}
    ;
end;

Function DosToWinTime(DosTime:longint; Var Wintime : TFileTime):longbool;
var
 lft : TFileTime;
begin
 DosToWinTime:=
   {$ifndef wince}
   DosDateTimeToFileTime(longrec(DosTime).hi,longrec(DosTime).lo,@lft) and
   {$endif}
   LocalFileTimeToFileTime(lft,Wintime);                                        ;
end;

function GetCurrentDirUtf8: String;
{$ifndef WinCE}
var
  w   : WideString;
  res : Integer;
  {$endif}
begin
  {$ifdef WinCE}
  Result := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  res:=GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res:=Windows.GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Result:=UTF8Encode(w);
  {$endif}
end;

procedure GetDirUtf8(DriveNr: Byte; var Dir: String);
{This procedure may not be threadsafe, because SetCurrentDirectory isn't}
{$ifndef WinCE}
var
  w, D: WideString;
  SavedDir: WideString;
  res : Integer;
{$endif}
begin
  {$ifdef WinCE}
  Dir := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  //writeln('GetDirWide START');
  if not (DriveNr = 0) then
  begin
    res := GetCurrentDirectoryW(0, nil);
    SetLength(SavedDir, res);
    res:=Windows.GetCurrentDirectoryW(res, @SavedDir[1]);
    SetLength(SavedDir,res);

    D := WideChar(64 + DriveNr) + ':';
    if not SetCurrentDirectoryW(@D[1]) then
    begin
      Dir := Char(64 + DriveNr) + ':\';
      SetCurrentDirectoryW(@SavedDir[1]);
      Exit;
    end;
  end;
  res := GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res := GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Dir:=UTF8Encode(w);
  if not (DriveNr = 0) then SetCurrentDirectoryW(@SavedDir[1]);
  //writeln('GetDirWide END');
  {$endif}
end;


function FileOpenUtf8(Const FileName : string; Mode : Integer) : THandle;

begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), dword(AccessModes[Mode and 3]),
                         dword(ShareModes[(Mode and $F0) shr 4]), nil, OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL, 0);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
end;


function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result := FileCreateUtf8(FileName, fmShareExclusive, 0);
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result := FileCreateUtf8(FileName, fmShareExclusive, Rights);
end;

function FileCreateUtf8(Const FileName : string;  ShareMode: Integer; {%H-}Rights: Cardinal) : THandle;
begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ or GENERIC_WRITE,
                       dword(ShareModes[(ShareMode and $F0) shr 4]), nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


function FileGetAttrUtf8(const FileName: String): Longint;
begin
  Result:=Integer(Windows.GetFileAttributesW(PWideChar(UTF8Decode(FileName))));
end;

function FileSetAttrUtf8(const Filename: String; Attr: longint): Longint;
begin
  if Windows.SetFileAttributesW(PWideChar(UTF8Decode(FileName)), Attr) then
    Result:=0
  else
    Result := Integer(Windows.GetLastError);
end;

function FileAgeUtf8(const FileName: String): Longint;
var
  Hnd: THandle;
  FindData: TWin32FindDataW;
begin
  Result := -1;
  Hnd := FindFirstFileW(PWideChar(UTF8ToUTF16(FileName)), FindData{%H-});
   if Hnd <> Windows.INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Hnd);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        If WinToDosTime(FindData.ftLastWriteTime,Result) then
          exit;
    end;
end;

function FileSetDateUtf8(const FileName: String; Age: Longint): Longint;
var
 FT:TFileTime;
 fh: HANDLE;
begin
   try
     fh := CreateFileW(PWideChar(UTF8ToUTF16(FileName)),
                       FILE_WRITE_ATTRIBUTES,
                       0, nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
     if (fh <> feInvalidHandle) and (DosToWinTime(Age,FT{%H-}) and SetFileTime(fh, nil, nil, @FT)) then
       Result := 0
     else
       Result := GetLastError;
   finally
     if (fh <> feInvalidHandle) then FileClose(fh);
   end;
end;


function FileSizeUtf8(const Filename: string): int64;
var
  FindData: TWIN32FindDataW;
  FindHandle: THandle;
  Str: WideString;
begin
  // Fix for the bug 14360:
  // Don't assign the widestring to TSearchRec.name because it is of type
  // string, which will generate a conversion to the system encoding
  Str := UTF8Decode(Filename);
  FindHandle := Windows.FindFirstFileW(PWideChar(Str), FindData{%H-});
  if FindHandle = Windows.Invalid_Handle_value then
  begin
    Result := -1;
    exit;
  end;
  Result := (int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;

function CreateDirUtf8(const NewDir: String): Boolean;
begin
  Result:=Windows.CreateDirectoryW(PWideChar(UTF8Decode(NewDir)), nil);
end;

function RemoveDirUtf8(const Dir: String): Boolean;
begin
  Result:=Windows.RemoveDirectoryW(PWideChar(UTF8Decode(Dir)));
end;

function DeleteFileUtf8(const FileName: String): Boolean;
begin
  Result:=Windows.DeleteFileW(PWideChar(UTF8Decode(FileName)));
end;

function RenameFileUtf8(const OldName, NewName: String): Boolean;
begin
  Result:=MoveFileW(PWideChar(UTF8Decode(OldName)), PWideChar(UTF8Decode(NewName)));
end;

function SetCurrentDirUtf8(const NewDir: String): Boolean;
begin
  {$ifdef WinCE}
  raise Exception.Create('[SetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  Result:=Windows.SetCurrentDirectoryW(PWidechar(UTF8Decode(NewDir)));
  {$endif}
end;

{$IF DEFINED(WinCE) OR (FPC_FULLVERSION>=30000)}
  {$define FindData_W}
{$IFEND}

function FindMatch(var f: TSearchRec) : Longint;
begin
  { Find file with correct attribute }
  While (F.FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if FindNextUTF8(F)<>0 then
      begin
        Result:=GetLastError;
        exit;
      end;
   end;
  { Convert some attributes back }
  WinToDosTime(F.FindData.ftLastWriteTime,F.Time);
  f.size:=F.FindData.NFileSizeLow+(qword(maxdword)+1)*F.FindData.NFileSizeHigh;
  f.attr:=F.FindData.dwFileAttributes;
  { The structures are different at this point
    in win32 it is the ansi structure with a utf-8 string
    in wince it is a wide structure }
  {$ifdef FindData_W}
  {$IFDEF ACP_RTL}
  f.Name:=String(UnicodeString(F.FindData.cFileName));
  {$ELSE}
  f.Name:=UTF8Encode(UnicodeString(F.FindData.cFileName));
  {$ENDIF}
  {$else}
  f.Name:=F.FindData.cFileName;
  {$endif}
  Result:=0;
end;

{$IFNDEF FindData_W}

{ This function does not really convert from wide to ansi, but from wide to
  a utf-8 encoded ansi version of the data structures in win32 and does
  nothing in wince

  See FindMatch also }
procedure FindWideToAnsi(const wide: TWIN32FINDDATAW; var ansi: TWIN32FINDDATA);
var
  ws: WideString;
  an: AnsiString;
begin
  SetLength(ws, length(wide.cAlternateFileName));
  Move(wide.cAlternateFileName[0], ws[1], length(ws)*2);
  an := AnsiString(ws); // no need to utf8 for cAlternateFileName (it's always ansi encoded)
  Move(an[1], ansi.cAlternateFileName, sizeof(ansi.cAlternateFileName));

  ws := PWideChar(@wide.cFileName[0]);
  an := UTF8Encode(ws);
  ansi.cFileName := an;
  if length(an)<length(ansi.cFileName) then ansi.cFileName[ length(an)]:=#0;

  with ansi do
  begin
    dwFileAttributes := wide.dwFileAttributes;
    ftCreationTime := wide.ftCreationTime;
    ftLastAccessTime := wide.ftLastAccessTime;
    ftLastWriteTime := wide.ftLastWriteTime;
    nFileSizeHigh := wide.nFileSizeHigh;
    nFileSizeLow := wide.nFileSizeLow;
    dwReserved0 := wide.dwReserved0;
    dwReserved1 := wide.dwReserved1;
  end;
end;
{$ENDIF}

function FindFirstUtf8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
var
  find: TWIN32FINDDATAW;
begin
  Rslt.Name:=Path;
  Rslt.Attr:=attr;
  Rslt.ExcludeAttr:=(not Attr) and ($1e);
                 { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  { FindFirstFile is a Win32 Call }
  {$IFDEF ACP_RTL}
  Rslt.FindHandle:=Windows.FindFirstFileW(PWideChar(WideString(Path)),find{%H-});
  {$ELSE}
  Rslt.FindHandle:=Windows.FindFirstFileW(PWideChar(UTF8Decode(Path)),find{%H-});
  {$ENDIF}
  If Rslt.FindHandle=Windows.Invalid_Handle_value then
  begin
    Result:=GetLastError;
    Exit;
  end;
  { Find file with correct attribute }
  {$IFNDEF FindData_W}
  FindWideToAnsi(find, Rslt.FindData);
  {$ELSE}
  Rslt.FindData := find;
  {$IFEND}
  Result := FindMatch(Rslt);
end;


function FindNextUtf8(var Rslt: TSearchRec): Longint;
var
  wide: TWIN32FINDDATAW;
begin
  if FindNextFileW(Rslt.FindHandle, wide{%H-}) then
  begin
    {$IFNDEF FindData_W}
    FindWideToAnsi(wide, Rslt.FindData);
    {$ELSE}
    Rslt.FindData := wide;
    {$ENDIF}
    Result := FindMatch(Rslt);
  end
  else
    Result := Integer(GetLastError);
end;

{$IFDEF WINCE}
// In WinCE these API calls are in Windows unit
function GetWindowsSpecialDirW(ID :  Integer) : String;
Var
  APath : Array[0..MAX_PATH] of WideChar;
  WS: WideString;
  Len: SizeInt;
begin
  Result := '';
  if SHGetSpecialFolderPath(0, APath, ID, True) then
  begin
    Len := StrLen(APath);
    SetLength(WS, Len);
    System.Move(APath[0], WS[1], Len * SizeOf(WideChar));
    Result := AppendPathDelim(Utf16ToUtf8(WS));
  end
  else
    Result:='';
end;
{$ELSE}

Type
  PFNSHGetFolderPathW = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PWChar): HRESULT; stdcall;

var
  SHGetFolderPathW : PFNSHGetFolderPathW = Nil;
  CFGDLLHandle : THandle = 0;

Procedure InitDLL;
Var
  pathBuf: array[0..MAX_PATH-1] of char;
  pathLength: Integer;
begin
  { Load shfolder.dll using a full path, in order to prevent spoofing (Mantis #18185)
    Don't bother loading shell32.dll because shfolder.dll itself redirects SHGetFolderPath
    to shell32.dll whenever possible. }
  pathLength:=GetSystemDirectory(pathBuf, MAX_PATH);
  if (pathLength>0) and (pathLength<MAX_PATH-14) then { 14=length('\shfolder.dll'#0) }
  begin
    StrLCopy(@pathBuf[pathLength],'\shfolder.dll',MAX_PATH-pathLength-1);
    CFGDLLHandle:=LoadLibrary(pathBuf);

    if (CFGDLLHandle<>0) then
    begin
      Pointer(ShGetFolderPathW):=GetProcAddress(CFGDLLHandle,'SHGetFolderPathW');
      If @ShGetFolderPathW=nil then
      begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
      end;
    end;
  end;
  If (@ShGetFolderPathW=Nil) then
    Raise Exception.Create('Could not determine SHGetFolderPathW Function');
end;

function GetWindowsSpecialDirW(ID :  Integer) : String;
Var
  APath : Array[0..MAX_PATH] of WideChar;
  WS: WideString;
  Len: SizeInt;
begin
  Result := '';
  if (CFGDLLHandle = 0) then
    InitDLL;
  If (SHGetFolderPathW <> Nil) then
  begin
    FillChar(APath{%H-}, SizeOf(APath), #0);
    if SHGetFolderPathW(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0]) = S_OK then
    begin
      Len := StrLen(APath);
      SetLength(WS, Len);
      System.Move(APath[0], WS[1], Len * SizeOf(WideChar));
      Result := AppendPathDelim(Utf16ToUtf8(WS));
    end;
  end
  else
    Result := SysToUtf8(GetWindowsSpecialDir(ID));
end;

{$ENDIF WINCE}

function DGetAppConfigDir({%H-}Global : Boolean) : String;
begin
  Result := ChompPathDelim(ExtractFilePath(ParamStrUtf8(0)));
end;


function GetAppConfigDirUtf8(Global: Boolean; Create: boolean = false): string;
const
  CSIDL_GLOBAL = {$IFDEF WINCE}CSIDL_WINDOWS{$ELSE}CSIDL_COMMON_APPDATA{$ENDIF WINCE};
  CSIDL_LOCAL = {$IFDEF WINCE}CSIDL_APPDATA{$ELSE}CSIDL_LOCAL_APPDATA{$ENDIF};
begin
  If Global then
    Result := GetWindowsSpecialDirW(CSIDL_GLOBAL)
  else
    Result := GetWindowsSpecialDirW(CSIDL_LOCAL);
  If (Result <> '') then
    begin
      if VendorName <> '' then
        Result := AppendPathDelim(Result + VendorName);
      Result := AppendPathDelim(Result + ApplicationName);
    end
  else
    Result := AppendPathDelim(DGetAppConfigDir(Global));
  if Result = '' then exit;
  if Create and not ForceDirectoriesUtf8(Result) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Result]));
end;

function GetAppConfigFileUtf8(Global: Boolean; SubDir: boolean;
  CreateDir: boolean): string;
var
  Dir: string;
begin
  Result := GetAppConfigDirUtf8(Global);
  if SubDir then
    Result := AppendPathDelim(Result + 'Config');
  Result := Result + ApplicationName + ConfigExtension;
  if not CreateDir then exit;
  Dir := ExtractFilePath(Result);
  if Dir = '' then exit;
  if not ForceDirectoriesUTF8(Dir) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
end;


function GetShellLinkTarget(const FileName: string): string;
{$IFnDEF WINCE}
var
  ShellLinkW: IShellLinkW;
  PersistFile: IPersistFile;
  WideFileName: WideString;
  WidePath: array [0 .. MAX_PATH] of WideChar;
  WinFindData: WIN32_FIND_DATAW;
  {$ENDIF WINCE}
begin
  Result := FileName;
  {$IFnDEF WINCE}
  if (LowerCase(ExtractFileExt(FileName)) = '.lnk') then
  begin
    if (CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
                         IShellLinkW, ShellLinkW) = S_OK) then
    if (ShellLinkW.QueryInterface(IPersistFile, PersistFile) = S_OK) then
    begin
      WideFileName := Utf8ToUtf16(FileName);
      FillChar(WinFindData{%H-}, SizeOf(WinFindData), 0);
      if (PersistFile.Load(POleStr(WideFileName), STGM_READ) = S_OK) then
      begin
        if (ShellLinkW.GetPath(WidePath, Length(WidePath),
                               @WinFindData, SLGP_UNCPRIORITY) = S_OK) then
        begin
          Result := Utf16toUtf8(WidePath); // implicit conversion
        end;
      end;
    end;
  end;
  {$ENDIF WINCE}
end;

// ******** End of WideString specific implementations ************


function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsWinAbsolute(TheFilename);
end;


function ExpandFileNameUtf8(const FileName: string; {const} BaseDir: String = ''): String;
var
  IsAbs, StartsWithRoot, CanUseBaseDir : Boolean;
  {$ifndef WinCE}
  HasDrive: Boolean;
  FnDrive, CurDrive, BaseDirDrive: Char;
  {$endif}
  CurDir, Fn: String;
begin
  //writeln('LazFileUtils.ExpandFileNameUtf8');
  //writeln('FileName = "',FileName,'"');
  //writeln('BaseDir  = "',BaseDir,'"');

  Fn := FileName;
  //if Filename uses ExtendedLengthPath scheme then it cannot be expanded
  //AND it should not be altered by ForcePathDelims or ResolveDots
  //See: http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
  if (Length(Fn) > 3) and (Fn[1] = PathDelim) and (Fn[2] = PathDelim) and
     (Fn[3] = '?') and (Fn[4] = PathDelim) //Do NOT use AllowDirectorySeparators here!
     then Exit;
  ForcePathDelims(Fn);
  IsAbs := FileNameIsWinAbsolute(Fn);
  if not IsAbs then
  begin
    StartsWithRoot := (Fn = '\') or
                      ((Length(Fn) > 1) and
                      (Fn[1] = DirectorySeparator) and
                      (Fn[2] <> DirectorySeparator));
    {$ifndef WinCE}
    HasDrive := (Length(Fn) > 1) and
                (Fn[2] = ':') and
                (UpCase(Fn[1]) in ['A'..'Z']);

    if HasDrive then
    begin
      FnDrive := UpCase(Fn[1]);
      GetDirUtf8(Byte(FnDrive)-64, CurDir{%H-});
      CurDrive := UpCase(GetCurrentDirUtf8[1]);
    end
    else
    begin
      CurDir := GetCurrentDirUtf8;
      FnDrive := UpCase(CurDir[1]);
      CurDrive := FnDrive;
    end;

    //writeln('HasDrive = ',HasDrive,' Fn = ',Fn);
    //writeln('CurDir = ',CurDir);
    //writeln('CurDrive = ',CurDrive);
    //writeln('FnDrive  = ',FnDrive);

    if (Length(BaseDir) > 1) and (UpCase(BaseDir[1]) in ['A'..'Z']) and (BaseDir[2] = ':') then
    begin
      BaseDirDrive := BaseDir[1]
    end
    else
    begin
      if HasDrive then
        BaseDirDrive := CurDrive
      else
        BaseDirDrive := #0;
    end;

    //You cannot use BaseDir if both FileName and BaseDir includes a drive and they are not the same
    CanUseBaseDir := ((BaseDirDrive = #0) or
                     (not HasDrive) or
                     (HasDrive and (FnDrive = BaseDirDrive)))
                     and (BaseDir <> '');

    //writeln('CanUseBaseDir = ',CanUseBaseDir);

    if not HasDrive and StartsWithRoot and not CanUseBaseDir then
    begin
      //writeln('HasDrive and StartsWithRoot');
      Fn := Copy(CurDir,1,2) + Fn;
      HasDrive := True;
      IsAbs := True;
    end;
    //FileNames like C:foo, strip Driveletter + colon
    if HasDrive and not IsAbs then Delete(Fn,1,2);

    //writeln('HasDrive = ',Hasdrive,' Fn = ',Fn);
    {$else}
    CanUseBaseDir := True;
    {$endif WinCE}
  end;
  if IsAbs then
  begin
    //writeln('IsAbs = True -> Exit');
    Result := ResolveDots(Fn);
  end
  else
  begin
    if not CanUseBaseDir or (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
    begin
      if (Length(Fn) > 0) and (Fn[1] = DirectorySeparator) then Delete(Fn,1,1);
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    end;

    Fn := ResolveDots(Fn);
    //if BaseDir is something like 'z:foo\' or '\' then this needs to be expanded as well
    if not FileNameIsAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;



function FileExistsUTF8(const Filename: string): boolean;
var
  Attr: Longint;
begin
  Attr := FileGetAttrUTF8(FileName);
  if Attr <> -1 then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;

function DirectoryExistsUTF8(const Directory: string): boolean;
var
  Attr: Longint;
begin
  Attr := FileGetAttrUTF8(Directory);
  if Attr <> -1 then
    Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result := False;
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=FileExistsUTF8(AFilename);
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
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
end;

function FileIsSymlink(const AFilename: string): boolean;
{$ifndef wince}
const
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
  IO_REPARSE_TAG_SYMLINK     = $A000000C;
var
  Attr: Longint;
  Rec: TSearchRec;
{$endif}
begin
{$ifndef wince}
  Attr := FileGetAttrUTF8(AFilename);
  if (Attr <> -1) and (Attr and FILE_ATTRIBUTE_REPARSE_POINT <> 0) then
  begin
    FindFirstUTF8(AFilename, Attr, Rec);
    if Rec.FindHandle <> feInvalidHandle then
    begin
      Windows.FindClose(Rec.FindHandle);
      Result := (Rec.FindData.dwReserved0 = IO_REPARSE_TAG_SYMLINK) or (Rec.FindData.dwReserved0 = IO_REPARSE_TAG_MOUNT_POINT);
    end
    else
      Result := False;
  end
  else
{$endif}
    Result := False;
end;

procedure CheckIfFileIsSymlink(const AFilename: string);
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if not FileIsSymLink(AFilename) then
    raise Exception.Create(Format(lrsIsNotASymbolicLink, [AFilename]));
end;


function FileIsHardLink(const AFilename: string): boolean;
{$ifndef wince}
var
  H: THandle;
  FileInfo: BY_HANDLE_FILE_INFORMATION;
  {$endif}
begin
  Result := false;
  {$ifndef wince}
  //HardLinks are not supported in Win9x platform
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then Exit;
  H := FileOpenUtf8(aFilename, fmOpenRead);
  if (H <> feInvalidHandle) then
  begin
    FillChar(FileInfo{%H-}, SizeOf(BY_HANDLE_FILE_INFORMATION),0);
    if GetFileInformationByHandle(H, FileInfo) then
      Result := (FileInfo.nNumberOfLinks > 1);
    FileClose(H);
  end;
  {$endif}
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result:=FileExistsUTF8(AFilename);
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result := ((FileGetAttrUTF8(AFilename) and faReadOnly) = 0);
end;


function IsUNCPath(const Path: String): Boolean;
begin
  Result := (Length(Path) > 2) and (Path[1] in AllowDirectorySeparators) and (Path[2] in AllowDirectorySeparators);
end;

function ExtractUNCVolume(const Path: String): String;
var
  I, Len: Integer;

  // the next function reuses Len variable
  function NextPathDelim(const Start: Integer): Integer;// inline;
  begin
    Result := Start;
    while (Result <= Len) and not (Path[Result] in AllowDirectorySeparators) do
      inc(Result);
  end;

begin
  if not IsUNCPath(Path) then
    Exit('');
  I := 3;
  Len := Length(Path);
  if Path[I] = '?' then
  begin
    // Long UNC path form like:
    // \\?\UNC\ComputerName\SharedFolder\Resource or
    // \\?\C:\Directory
    inc(I);
    if not (Path[I] in AllowDirectorySeparators) then
      Exit('');
    if UpperCase(Copy(Path, I + 1, 3)) = 'UNC' then
    begin
      inc(I, 4);
      if I < Len then
        I := NextPathDelim(I + 1);
      if I < Len then
        I := NextPathDelim(I + 1);
    end;
  end
  else
  begin
    I := NextPathDelim(I);
    if I < Len then
      I := NextPathDelim(I + 1);
  end;
  Result := Copy(Path, 1, I);
end;

function GetFileDescription(const AFilename: string): string;
begin
  // date + time
  Result:=lrsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result:=Result+'?';
  end;
end;


procedure InitLazFileUtils;
begin
end;

procedure FinalizeLazFileUtils;
begin
  {$IFnDEF WINCE}
  if CFGDLLHandle <> 0 then
    FreeLibrary(CFGDllHandle);
  {$ENDIF WINCE}
end;
