{%MainUnit fileutil.pas}
{******************************************************************************
                                  Fileutil
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
 
{$IFnDEF DisableWrapperFunctions}
function NeedRTLAnsi: boolean;
begin
  Result := LazUtf8.NeedRTLAnsi;
end;

procedure SetNeedRTLAnsi(NewValue: boolean);
begin
  LazUtf8.SetNeedRTLAnsi(NewValue);
end;

function UTF8ToSys(const s: string): string;
begin
  Result := LazUtf8.UTF8ToSys(s);
end;

function SysToUTF8(const s: string): string;
begin
  Result := LazUtf8.SysToUTF8(s);
end;

function ConsoleToUTF8(const s: string): string;// converts OEM encoded string to UTF8 (used with some Windows specific functions)
begin
  Result := LazUtf8.ConsoleToUTF8(s);
end;

function UTF8ToConsole(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
begin
  Result := LazUtf8.UTF8ToConsole(s);
end;

function ParamStrUTF8(Param: Integer): string;
begin
  Result := LazUtf8.ParamStrUTF8(Param);
end;

function GetEnvironmentStringUTF8(Index: Integer): string;
begin
  Result := LazUtf8.GetEnvironmentStringUTF8(Index);
end;

function GetEnvironmentVariableUTF8(const EnvVar: string): String;
begin
  Result := LazUtf8.GetEnvironmentVariableUTF8(EnvVar);
end;

function SysErrorMessageUTF8(ErrorCode: Integer): String;
begin
  Result := LazUtf8.SysErrorMessageUTF8(ErrorCode);
end;

function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
begin
  Result := LazFileUtils.GetAppConfigDirUTF8(Global, Create);
end;

function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean; CreateDir: boolean): string;
begin
  Result := LazFileUtils.GetAppConfigFileUTF8(Global, SubDir, CreateDir);
end;

function ExtractFileNameOnly(const AFilename: string): string;
begin
  Result := LazFileUtils.ExtractFileNameOnly(AFilename);
end;

function FileExistsUTF8(const Filename: string): boolean;
begin
  Result := LazFileUtils.FileExistsUTF8(FileName);
end;

function FileAgeUTF8(const FileName: string): Longint;
begin
  Result := LazFileUtils.FileAgeUTF8(FileName);
end;

function DirectoryExistsUTF8(const Directory: string): Boolean; inline;
begin
  Result := LazFileUtils.DirectoryExistsUTF8(Directory);
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result := LazFileUtils.FindFirstUtf8(Path, Attr, Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Result := LazFileUtils.FindNextUTF8(Rslt);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  LazFileUtils.FindCloseUtf8(F);
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result := LazFileUtils.FileSetDateUTF8(FileName, Age);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result := LazFileUtils.FileGetAttrUtf8(FileName);
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result := LazFileUtils.FileSetAttrUtf8(Filename, Attr);
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result := LazFileUtils.DeleteFileUtf8(FileName);
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result := LazFileUtils.RenameFileUtf8(OldName, NewName);
end;

function FileSearchUTF8(const Name, DirList: String; ImplicitCurrentDir : Boolean = True): String;
begin
  Result := LazFileUtils.FileSearchUTF8(Name, DirList, ImplicitCurrentDir);
end;

function FileIsReadOnlyUTF8(const FileName: String): Boolean;
begin
  Result := LazFileUtils.FileIsReadOnlyUTF8(FileName);
end;

function GetCurrentDirUTF8: String;
begin
  Result := LazFileUtils.GetCurrentDirUtf8();
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result := LazFileUtils.SetCurrentDirUtf8(NewDir);
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result := LazFileUtils.CreateDirUtf8(NewDir);
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result := LazFileUtils.RemoveDirUtf8(Dir);
end;

function ForceDirectoriesUTF8(const Dir: string): Boolean;
begin
  Result := LazFileUtils.ForceDirectoriesUTF8(Dir);
end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  Result := LazFileUtils.FileOpenUtf8(FileName, Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result := LazFileUtils.FileCreateUtf8(FileName);
end;

function GetTempFilename(const Directory, Prefix: string): string;
begin
  Result := LazFileUtils.GetTempFileNameUTF8(Directory, Prefix);
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result := LazFileUtils.FileCreateUtf8(FileName, Rights);
end;

function CleanAndExpandFilename(const Filename: string): string;
begin
  Result := LazFileutils.CleanAndExpandFilename(FileName);
end;

function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result := LazFileUtils.CleanAndExpandDirectory(FileName);
end;

function ExpandFileNameUTF8(const FileName: string): string;
begin
  Result := LazFileUtils.ExpandFileNameUTF8(Filename);
end;

function CompareFileExt(const Filename, Ext: string; CaseSensitive: boolean): integer;
begin
  Result := LazFileUtils.CompareFileExt(Filename, Ext, CaseSensitive);
end;

function CompareFileExt(const Filename, Ext: string): integer;
begin
  Result := LazFileUtils.CompareFileExt(Filename, Ext);
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  Result := LazFileUtils.CompareFilenames(Filename1, Filename2);
end;

function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
begin
  Result := LazFileUtils.CompareFilenamesIgnoreCase(Filename1, Filename2);
end;

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result := LazFileUtils.FilenameIsAbsolute(TheFileName);
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  Result := LazFileUtils.FilenameIsWinAbsolute(TheFileName);
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result := LazFileUtils.FilenameIsUnixAbsolute(TheFileName);
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
begin
  LazFileUtils.CheckIfFileIsExecutable(AFileName);
end;

procedure CheckIfFileIsSymlink(const AFilename: string);
begin
  LazFileUtils.CheckIfFileIsSymlink(AFilename);
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result:= LazFileUtils.FileIsReadable(AFilename);
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result := LazFileUtils.FileIsWritable(AFilename);
end;

function FileIsText(const AFilename: string): boolean;
begin
  Result := LazFileUtils.FileIsText(AFilename);
end;

function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
begin
  Result := LazFileUtils.FileIsText(AFileName, FileReadable);
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result := LazFileUtils.FileIsExecutable(AFileName);
end;

function FileIsSymlink(const AFilename: string): boolean;
begin
  Result := LazFileUtils.FileIsSymlink(AFilename);
end;

function FileIsHardLink(const AFilename: string): boolean;
begin
  Result := LazFileUtils.FileIsHardLink(AFilename);
end;

function GetFileDescription(const AFilename: string): string;
begin
  Result := LazFileUtils.GetFileDescription(AFilename);
end;

function ReadAllLinks(const Filename: string; ExceptionOnError: boolean): string;
begin
  Result:=LazFileUtils.ReadAllLinks(Filename,ExceptionOnError);
end;

function TryReadAllLinks(const Filename: string): string;
begin
  Result:=LazFileUtils.TryReadAllLinks(Filename);
end;

function TrimFilename(const AFilename: string): string;
begin
  Result := LazFileUtils.TrimFilename(AFileName);
end;

function DirPathExists(const FileName: String): Boolean;
begin
  Result := LazFileUtils.DirPathExists(FileName);
end;

function ForceDirectory(DirectoryName: string): boolean;
begin
  Result := LazFileUtils.ForceDirectory(DirectoryName);
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
begin
  Result := LazFileUtils.DirectoryIsWritable(DirectoryName);
end;

function AppendPathDelim(const Path: string): string;
begin
  Result := LazFileUtils.AppendPathDelim(Path);
end;

function ChompPathDelim(const Path: string): string;
begin
  Result := LazFileUtils.ChompPathDelim(Path);
end;

function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean; AlwaysRequireSharedBaseFolder: Boolean): string;
begin
  Result := LazFileUtils.CreateRelativePath(FileName, BaseDirectory, UsePointDirectory, AlwaysRequireSharedBaseFolder);
end;

{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
begin
  Result := LazFileUtils.GetDarwinSystemFilename(Filename);
end;
{$ENDIF}

{$ENDIF DisableWrapperFunctions}

// ToDo: For ExpandUNCFileNameUTF8
//
// Don't convert to and from Sys, because this RTL routines
// simply work in simple string operations, without calling native
// APIs which would really require Ansi
//
// The Ansi conversion just ruins Unicode strings
//
// See bug http://bugs.freepascal.org/view.php?id=20229
// It needs fixing like we did for LazFileUtils.ExpandFileNameUtf8(Filename) on Windows

function ExpandUNCFileNameUTF8(const FileName: string): string;
begin
  Result:=SysUtils.ExpandUNCFileName(Filename);
end;

function FileSize(const Filename: string): int64;
begin
  Result := LazFileUtils.FileSizeUtf8(FileName);
end;

function ComparePhysicalFilenames(const Filename1, Filename2: string): integer;
var
  File1: String;
  File2: String;
begin
  File1:=GetPhysicalFilename(Filename1,pfeOriginal);
  File2:=GetPhysicalFilename(Filename2,pfeOriginal);
  Result:=LazFileUtils.CompareFilenames(File1,File2);
end;

function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer;
var
  File1: string;
  File2: string;
  {$IFNDEF NotLiteralFilenames}
  i: Integer;
  {$ENDIF}
begin
  if (Len1=0) or (Len2=0) then begin
    Result:=Len1-Len2;
    exit;
  end;
  if ResolveLinks then begin
    SetLength(File1,Len1);
    System.Move(Filename1^,File1[1],Len1);
    SetLength(File2,Len2);
    System.Move(Filename2^,File2[1],Len2);
    if ResolveLinks then
      Result:=ComparePhysicalFilenames(File1,File2)
    else
      Result:=LazFileUtils.CompareFilenames(File1,File2);
  end else begin
    {$IFDEF NotLiteralFilenames}
    SetLength(File1,Len1);
    System.Move(Filename1^,File1[1],Len1);
    SetLength(File2,Len2);
    System.Move(Filename2^,File2[1],Len2);
    Result:=LazFileUtils.CompareFilenames(File1,File2);
    {$ELSE}
    Result:=0;
    i:=0;
    while (Result=0) and ((i<Len1) and (i<Len2)) do begin
      Result:=Ord(Filename1[i])
             -Ord(Filename2[i]);
      Inc(i);
    end;
    if Result=0 Then
      Result:=Len1-Len2;
    {$ENDIF}
  end;
end;

function FilenameIsPascalUnit(const Filename: string): boolean;
var
  i: Integer;
begin
  for i:=Low(PascalFileExt) to High(PascalFileExt) do
    if LazFileUtils.CompareFileExt(Filename,PascalFileExt[i],false)=0 then
      exit(true);
  Result:=false;
end;

function ExtractFileNameWithoutExt(const AFilename: string): string;
var
  p: Integer;
begin
  Result:=AFilename;
  p:=length(Result);
  while (p>0) do begin
    case Result[p] of
      PathDelim: exit;
      {$ifdef windows}
      '/': if ('/' in AllowDirectorySeparators) then exit;
      {$endif}
      '.': exit(copy(Result,1, p-1));
    end;
    dec(p);
  end;
end;

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink {$endif unix};
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=LazFileUtils.CleanAndExpandDirectory(DirectoryName);
  if LazFileUtils.FindFirstUTF8(CurSrcDir+GetAllFilesMask,DeleteMask,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if ((FileInfo.Attr and faDirectory)>0)
         {$ifdef unix} and ((FileInfo.Attr and faSymLink)=0) {$endif unix} then begin
        if not DeleteDirectory(CurFilename,false) then exit;
      end else begin
        if not LazFileUtils.DeleteFileUTF8(CurFilename) then exit;
      end;
    until LazFileUtils.FindNextUTF8(FileInfo)<>0;
  end;
  LazFileUtils.FindCloseUTF8(FileInfo);
  if (not OnlyChildren) and (not LazFileUtils.RemoveDirUTF8(CurSrcDir)) then exit;
  Result:=true;
end;

function ProgramDirectory: string;
var
  Flags: TSearchFileInPathFlags;
begin
  Result:=LazUTF8.ParamStrUTF8(0);
  if ExtractFilePath(Result)='' then begin
    // program was started via PATH
    {$IFDEF WINDOWS}
    Flags:=[];
    {$ELSE}
    Flags:=[sffDontSearchInBasePath];
    {$ENDIF}
    Result:=SearchFileInPath(Result,'',LazUTF8.GetEnvironmentVariableUTF8('PATH'),PathSeparator,Flags);
  end;
  // resolve links
  Result:=GetPhysicalFilename(Result,pfeOriginal);
  // extract file path and expand to full name
  Result:=LazFileUtils.ExpandFileNameUTF8(ExtractFilePath(Result));
end;

function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
  BaseDir: String;
begin
  Result:=SearchPath;
  if (SearchPath='') or (BaseDirectory='') then exit;
  BaseDir:=LazFileUtils.AppendPathDelim(BaseDirectory);

  PathLen:=length(Result);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (Result[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (Result[EndPos]<>';') do inc(EndPos);
    CurDir:=copy(Result,StartPos,EndPos-StartPos);
    if not LazFileUtils.FilenameIsAbsolute(CurDir) then begin
      NewCurDir:=BaseDir+CurDir;
      if NewCurDir<>CurDir then begin
        DiffLen:=length(NewCurDir)-length(CurDir);
        Result:=copy(Result,1,StartPos-1)+NewCurDir
                +copy(Result,EndPos,PathLen-EndPos+1);
        inc(EndPos,DiffLen);
        inc(PathLen,DiffLen);
      end;
    end;
    StartPos:=EndPos;
  end;
end;

function CreateAbsolutePath(const Filename, BaseDirectory: string): string;
begin
  if (Filename='') or LazFileUtils.FilenameIsAbsolute(Filename) then
    Result:=Filename
  {$IFDEF Windows}
  else if (Filename[1]='\') then
    // only use drive of BaseDirectory
    Result:=ExtractFileDrive(BaseDirectory)+Filename
  {$ENDIF}
  else
    Result:=LazFileUtils.AppendPathDelim(BaseDirectory)+Filename;
  Result:=LazFileUtils.TrimFilename(Result);
end;

function FileIsInPath(const Filename, Path: string): boolean;
var
  ExpFile: String;
  ExpPath: String;
  l: integer;
begin
  ExpFile:=LazFileUtils.CleanAndExpandFilename(Filename);
  ExpPath:=LazFileUtils.CleanAndExpandDirectory(Path);
  l:=length(ExpPath);
  Result:=(l>0) and (length(ExpFile)>l) and (ExpFile[l] in AllowDirectorySeparators)
          and (LazFileUtils.CompareFilenames(ExpPath,LeftStr(ExpFile,l))=0);
end;

function FileIsInDirectory(const Filename, Directory: string): boolean;
var
  ExpFile: String;
  ExpDir: String;
  LenFile: Integer;
  LenDir: Integer;
  p: LongInt;
begin
  ExpFile:=LazFileUtils.CleanAndExpandFilename(Filename);
  ExpDir:=LazFileUtils.CleanAndExpandDirectory(Directory);
  LenFile:=length(ExpFile);
  LenDir:=length(ExpDir);
  p:=LenFile;
  while (p>0) and not (ExpFile[p] in AllowDirectorySeparators) do dec(p);
  Result:=(p=LenDir) and (p<LenFile)
          and (LazFileUtils.CompareFilenames(ExpDir,LeftStr(ExpFile,p))=0);
end;

function CopyFile(const SrcFilename, DestFilename: String;
                  Flags: TCopyFileFlags=[cffOverwriteFile]; ExceptionOnError: Boolean=False): Boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result := False;
  // check overwrite
  if (not (cffOverwriteFile in Flags)) and LazFileUtils.FileExistsUTF8(DestFileName) then
    exit;
  // check directory
  if (cffCreateDestDirectory in Flags)
  and (not LazFileUtils.DirectoryExistsUTF8(ExtractFilePath(DestFileName)))
  and (not LazFileUtils.ForceDirectoriesUTF8(ExtractFilePath(DestFileName))) then
    exit;
  TryCount := 0;
  While TryCount <> 3 Do Begin
    SrcHandle := LazFileUtils.FileOpenUTF8(SrcFilename, fmOpenRead or fmShareDenyWrite);
    if (THandle(SrcHandle)=feInvalidHandle) then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount := 0;
      Break;
    End;
  End;
  If TryCount > 0 Then
  begin
    if ExceptionOnError then
      raise EFOpenError.CreateFmt({SFOpenError}'Unable to open file "%s"', [SrcFilename])
    else
      exit;
  end;
  try
    DestHandle := LazFileUtils.FileCreateUTF8(DestFileName);
    if (THandle(DestHandle)=feInvalidHandle) then
    begin
      if ExceptionOnError then
        raise EFCreateError.CreateFmt({SFCreateError}'Unable to create file "%s"',[DestFileName])
      else
        Exit;
    end;
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if ReadCount<=0 then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if WriteCount<ReadCount then
        begin
          if ExceptionOnError then
            raise EWriteError.CreateFmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
          else
            Exit;
        end;
      until false;
    finally
      FileClose(DestHandle);
    end;
    if (cffPreserveTime in Flags) then
      LazFileUtils.FileSetDateUTF8(DestFilename, FileGetDate(SrcHandle));
    Result := True;
  finally
    FileClose(SrcHandle);
  end;
end;

function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: Boolean; ExceptionOnError: Boolean): boolean;
// Flags parameter can be used for the same thing.
var
  Flags: TCopyFileFlags;
begin
  if PreserveTime then
    Flags:=[cffPreserveTime, cffOverwriteFile]
  else
    Flags:=[cffOverwriteFile];
  Result := CopyFile(SrcFilename, DestFilename, Flags, ExceptionOnError);
end;

{ TCopyDirTree for CopyDirTree function }
type
  TCopyDirTree = class(TFileSearcher)
  private
    FSourceDir: string;
    FTargetDir: string;
    FFlags: TCopyFileFlags;
    FCopyFailedCount:Integer;
  protected
    procedure DoFileFound; override;
    procedure DoDirectoryFound; override;
  end;

procedure TCopyDirTree.DoFileFound;
var
  NewLoc: string;
begin
  // ToDo: make sure StringReplace works in all situations !
  NewLoc:=StringReplace(FileName, FSourceDir, FTargetDir, []);
  if not CopyFile(FileName, NewLoc, FFlags) then
    Inc(FCopyFailedCount);
end;

procedure TCopyDirTree.DoDirectoryFound;
var
  NewPath:String;
begin
  NewPath:=StringReplace(FileName, FSourceDir, FTargetDir, []);
  // ToDo: make directories also respect cffPreserveTime flag.
  if not LazFileUtils.DirectoryExistsUTF8(NewPath) then
    if not LazFileUtils.ForceDirectoriesUTF8(NewPath) then
      Inc(FCopyFailedCount);
end;

function CopyDirTree(const SourceDir, TargetDir: string; Flags: TCopyFileFlags=[]): Boolean;
var
  Searcher: TCopyDirTree;
begin
  Result:=False;
  Searcher:=TCopyDirTree.Create;
  try
    // Destination directories are always created. User setting has no effect!
    Flags:=Flags+[cffCreateDestDirectory];
    Searcher.FFlags:=Flags;
    Searcher.FCopyFailedCount:=0;
    Searcher.FSourceDir:=LazFileUtils.TrimFilename(SetDirSeparators(SourceDir));
    Searcher.FTargetDir:=LazFileUtils.TrimFilename(SetDirSeparators(TargetDir));

    // Don't even try to copy to a subdirectory of SourceDir.
    {$ifdef CaseInsensitiveFilenames}
      {$ifdef ACP_RTL}
      if AnsiStartsText(Searcher.FSourceDir, Searcher.FTargetDir) then Exit;
      {$else ACP_RTL}
      if Utf8StartsText(Searcher.FSourceDir, Searcher.FTargetDir) then Exit;
      {$endif}
    {$ELSE}
      if AnsiStartsStr(Searcher.FSourceDir, Searcher.FTargetDir) then Exit;
    {$ENDIF}
    Searcher.Search(SourceDir);
    Result:=Searcher.FCopyFailedCount=0;
  finally
    Searcher.Free;
  end;
end;

function GetAllFilesMask: string;
begin
  {$IFDEF WINDOWS}
  Result:='*.*';
  {$ELSE}
  Result:='*';
  {$ENDIF}
end;

function GetExeExt: string;
begin
  {$IFDEF WINDOWS}
  Result:='.exe';
  {$ELSE}
  Result:='';
  {$ENDIF}
end;

function ReadFileToString(const Filename: String): String;
var
  SrcHandle: THandle;
  ReadCount: LongInt;
  s: String;
begin
  Result := '';
  s:='';
  try
    Setlength(s, FileSize(Filename));
    if s='' then exit;
    SrcHandle := LazFileUtils.FileOpenUTF8(Filename, fmOpenRead or fmShareDenyWrite);
    if (THandle(SrcHandle)=feInvalidHandle) then
      exit;
    try
      ReadCount:=FileRead(SrcHandle,s[1],length(s));
      if ReadCount<length(s) then
        exit;
    finally
      FileClose(SrcHandle);
    end;
    Result:=s;
  except
    // ignore errors, Result string will be empty
  end;
end;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): string;
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
//debugln('[SearchFileInPath] Filename="',Filename,'" BasePath="',BasePath,'" SearchPath="',SearchPath,'" Delimiter="',Delimiter,'"');
  if (Filename='') then begin
    Result:='';
    exit;
  end;
  // check if filename absolute
  if LazFileUtils.FilenameIsAbsolute(Filename) then begin
    if LazFileUtils.FileExistsUTF8(Filename) then begin
      Result:=LazFileUtils.CleanAndExpandFilename(Filename);
      exit;
    end else begin
      Result:='';
      exit;
    end;
  end;
  Base:=LazFileUtils.CleanAndExpandDirectory(BasePath);
  // search in current directory
  if (not (sffDontSearchInBasePath in Flags))
  and LazFileUtils.FileExistsUTF8(Base+Filename) then begin
    Result:=LazFileUtils.CleanAndExpandFilename(Base+Filename);
    exit;
  end;
  // search in search path
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (pos(SearchPath[p],Delimiter)<1) do inc(p);
    CurPath:=LazFileUtils.TrimFilename(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not LazFileUtils.FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      Result:=LazFileUtils.CleanAndExpandFilename(LazFileUtils.AppendPathDelim(CurPath)+Filename);
      if LazFileUtils.FileExistsUTF8(Result) then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function SearchAllFilesInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): TStrings;
  
  procedure Add(NewFilename: string);
  var
    i: Integer;
  begin
    NewFilename:=LazFileUtils.TrimFilename(NewFilename);
    if not LazFileUtils.FileExistsUTF8(NewFilename) then exit;
    if Result=nil then begin
      Result:=TStringList.Create;
    end else begin
      for i:=0 to Result.Count-1 do
        if LazFileUtils.CompareFilenames(Result[i],NewFilename)=0 then exit;
    end;
    Result.Add(NewFilename);
  end;
  
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
  Result:=nil;
  if (Filename='') then exit;
  // check if filename absolute
  if LazFileUtils.FilenameIsAbsolute(Filename) then begin
    Add(LazFileUtils.CleanAndExpandFilename(Filename));
    exit;
  end;
  Base:=LazFileUtils.CleanAndExpandDirectory(BasePath);
  // search in current directory
  if (not (sffDontSearchInBasePath in Flags)) then begin
    Add(LazFileUtils.CleanAndExpandFilename(Base+Filename));
  end;
  // search in search path
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (pos(SearchPath[p],Delimiter)<1) do inc(p);
    CurPath:=LazFileUtils.TrimFilename(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not LazFileUtils.FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      Add(LazFileUtils.CleanAndExpandFilename(LazFileUtils.AppendPathDelim(CurPath)+Filename));
    end;
    StartPos:=p+1;
  end;
end;

function FindDiskFilename(const Filename: string): string;
// Searches for the filename case on disk.
// The file must exist.
// For example:
//   If Filename='file' and there is only a 'File' then 'File' will be returned.
var
  StartPos: Integer;
  EndPos: LongInt;
  FileInfo: TSearchRec;
  CurDir: String;
  CurFile: String;
  AliasFile: String;
  Ambiguous: Boolean;
begin
  Result:=Filename;
  if not LazFileUtils.FileExistsUTF8(Filename) then exit;
  //Sanitize result first (otherwise result can contain things like foo/\bar on Windows)
  Result := ResolveDots(Result);
  // check every directory and filename
  StartPos:=1;
  {$IFDEF WINDOWS}
  // uppercase Drive letter and skip it
  if ((length(Result)>=2) and (Result[1] in ['A'..'Z','a'..'z'])
  and (Result[2]=':')) then begin
    StartPos:=3;
    if Result[1] in ['a'..'z'] then
      Result[1]:=upcase(Result[1]);
  end;
  {$ENDIF}
  repeat
    // skip PathDelim
    while (StartPos<=length(Result)) and (Result[StartPos] in AllowDirectorySeparators) do
      inc(StartPos);
    // find end of filename part
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and not (Result[EndPos] in AllowDirectorySeparators) do
      inc(EndPos);
    if EndPos>StartPos then begin
      // search file
      CurDir:=copy(Result,1,StartPos-1);
      CurFile:=copy(Result,StartPos,EndPos-StartPos);
      AliasFile:='';
      Ambiguous:=false;
      if LazFileUtils.FindFirstUTF8(CurDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if LazFileUtils.CompareFilenamesIgnoreCase(FileInfo.Name,CurFile)=0 then begin
            //debugln('FindDiskFilename ',FileInfo.Name,' ',CurFile);
            if FileInfo.Name=CurFile then begin
              // file found, has already the correct name
              AliasFile:='';
              break;
            end else begin
              // alias found, but has not the correct name
              if AliasFile='' then begin
                AliasFile:=FileInfo.Name;
              end else begin
                // there are more than one candidate
                Ambiguous:=true;
              end;
            end;
          end;
        until LazFileUtils.FindNextUTF8(FileInfo)<>0;
      end;
      LazFileUtils.FindCloseUTF8(FileInfo);
      if (AliasFile<>'') and (not Ambiguous) then begin
        // better filename found -> replace
        Result:=CurDir+AliasFile+copy(Result,EndPos,length(Result));
      end;
    end;
    StartPos:=EndPos+1;
  until StartPos>length(Result);
end;

function FindDiskFileCaseInsensitive(const Filename: string): string;
var
  FileInfo: TSearchRec;
  ShortFilename: String;
  CurDir: String;
begin
  Result:='';
  CurDir:=ExtractFilePath(ResolveDots(Filename));
  if LazFileUtils.FindFirstUTF8(CurDir+GetAllFilesMask,faAnyFile, FileInfo)=0 then begin
    ShortFilename:=ExtractFilename(Filename);
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;
      if LazFileUtils.CompareFilenamesIgnoreCase(FileInfo.Name,ShortFilename)=0 then begin
        if FileInfo.Name=ShortFilename then begin
          // fits exactly
          //Don't return (unaltered) Filename: otherwise possible changes by ResolveDots get lost
          Result:=CurDir+FileInfo.Name;
          break;
        end;
        // fits case insensitive
        Result:=CurDir+FileInfo.Name;
        // search further
      end;
    until LazFileUtils.FindNextUTF8(FileInfo)<>0;
  end;
  LazFileUtils.FindCloseUTF8(FileInfo);
end;

function FindDefaultExecutablePath(const Executable: string;
  const BaseDir: string): string;
begin
  if LazFileUtils.FilenameIsAbsolute(Executable) then begin
    Result:=Executable;
    if LazFileUtils.FileExistsUTF8(Result) then exit;
    {$IFDEF Windows}
    if ExtractFileExt(Result)='' then begin
      Result:=Result+'.exe';
      if LazFileUtils.FileExistsUTF8(Result) then exit;
    end;
    {$ENDIF}
  end else begin
    Result:=SearchFileInPath(Executable,BaseDir,
                             LazUTF8.GetEnvironmentVariableUTF8('PATH'), PathSeparator,
                             [sffDontSearchInBasePath]);
    if Result<>'' then exit;
    {$IFDEF Windows}
    if ExtractFileExt(Executable)='' then begin
      Result:=SearchFileInPath(Executable+'.exe',BaseDir,
                               LazUTF8.GetEnvironmentVariableUTF8('PATH'), PathSeparator,
                               [sffDontSearchInBasePath]);
      if Result<>'' then exit;
    end;
    {$ENDIF}
  end;
  Result:='';
end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  FList.Add(FileName);
end;

constructor TListFileSearcher.Create(AList: TStrings);
begin
  inherited Create;
  FList := AList;
end;

procedure FindAllFiles(AList: TStrings; const SearchPath: String;
  SearchMask: String; SearchSubDirs: Boolean; DirAttr: Word);
var
  Searcher: TListFileSearcher;
begin
  Searcher := TListFileSearcher.Create(AList);
  Searcher.DirectoryAttribute := DirAttr;
  try
    Searcher.Search(SearchPath, SearchMask, SearchSubDirs);
  finally
    Searcher.Free;
  end;
end;

function FindAllFiles(const SearchPath: String; SearchMask: String;
  SearchSubDirs: Boolean; DirAttr: Word): TStringList;
begin
  Result := TStringList.Create;
  FindAllFiles(Result, SearchPath, SearchMask, SearchSubDirs, DirAttr);
end;

{ TListDirectoriesSearcher }

constructor TListDirectoriesSearcher.Create(AList: TStrings);
begin
  inherited Create;
  FDirectoriesList := AList;
end;

procedure TListDirectoriesSearcher.DoDirectoryFound;
begin
  FDirectoriesList.Add(FileName);
end;

function FindAllDirectories(const SearchPath : string;
  SearchSubDirs: Boolean = True): TStringList;
begin
  Result := TStringList.Create;
  FindAllDirectories(Result, SearchPath, SearchSubDirs);
end;

procedure FindAllDirectories(AList: TStrings; const SearchPath: String;
  SearchSubDirs: Boolean = true);
var
  Searcher :TFileSearcher;
begin
  Assert(AList <> nil);
  Searcher := TListDirectoriesSearcher.Create(AList);
  try
    Searcher.Search(SearchPath, AllFilesMask, SearchSubDirs);
  finally
    Searcher.Free;
  end;
end;

{ TFileIterator }

function TFileIterator.GetFileName: String;
begin
  Result := FPath + FFileInfo.Name;
end;

procedure TFileIterator.Stop;
begin
  FSearching := False;
end;

function TFileIterator.IsDirectory: Boolean;
begin
  Result := (FFileInfo.Attr and faDirectory) <> 0;
end;

{ TFileSearcher }

procedure TFileSearcher.RaiseSearchingError;
begin
  raise Exception.Create('The file searcher is already searching!');
end;

procedure TFileSearcher.DoDirectoryEnter;
begin
  if Assigned(FonDirectoryEnter) then FOnDirectoryEnter(Self);
end;

procedure TFileSearcher.DoDirectoryFound;
begin
  if Assigned(FOnDirectoryFound) then OnDirectoryFound(Self);
end;

procedure TFileSearcher.DoFileFound;
begin
  if Assigned(FOnFileFound) then OnFileFound(Self);
end;

constructor TFileSearcher.Create;
begin
  inherited Create;
  FMaskSeparator := ';';
  FFollowSymLink := True;
  FFileAttribute := faAnyFile;
  FDirectoryAttribute := faDirectory;
  FSearching := False;
end;

procedure TFileSearcher.Search(ASearchPath: String; ASearchMask: String;
  ASearchSubDirs: Boolean; CaseSensitive: Boolean = False);
var
  MaskList: TMaskList;
  SearchDirectories: TStringList;

  procedure DoSearch(const APath: String; const ALevel: Integer);
  var
    P: String;
    PathInfo: TSearchRec;
  begin
    P := APath + AllDirectoryEntriesMask;

    if LazFileUtils.FindFirstUTF8(P, FileAttribute, PathInfo) = 0 then
    try
      repeat
        // skip special files
        if (PathInfo.Name = '.') or (PathInfo.Name = '..') or
          (PathInfo.Name = '') then Continue;
        // Deal with both files and directories
        if (PathInfo.Attr and faDirectory) = 0 then
        begin             // File
          {$IFDEF Windows}
          if (MaskList = nil) or MaskList.MatchesWindowsMask(PathInfo.Name)
          {$ELSE}
          if (MaskList = nil) or MaskList.Matches(PathInfo.Name)
          {$ENDIF}
          then begin
            FPath := APath;
            FLevel := ALevel;
            FFileInfo := PathInfo;
            DoFileFound;
          end;
        end
        else begin        // Directory
          FPath := APath;
          FLevel := ALevel;
          FFileInfo := PathInfo;
          DoDirectoryFound;
        end;

      until (LazFileUtils.FindNextUTF8(PathInfo) <> 0) or not FSearching;
    finally
      LazFileUtils.FindCloseUTF8(PathInfo);
    end;

    if ASearchSubDirs or (ALevel > 0) then
      // search recursively in directories
      if LazFileUtils.FindFirstUTF8(P, DirectoryAttribute, PathInfo) = 0 then
      try
        repeat
          if (PathInfo.Name = '.') or (PathInfo.Name = '..') or
             (PathInfo.Name = '') or ((PathInfo.Attr and faDirectory) = 0) or
             (not FFollowSymLink and LazFileUtils.FileIsSymlink(APath + PathInfo.Name))
          then Continue;

          FPath := APath;
          FLevel := ALevel;
          FFileInfo := PathInfo;
          DoDirectoryEnter;
          if not FSearching then Break;

          DoSearch(LazFileUtils.AppendPathDelim(APath + PathInfo.Name), Succ(ALevel));

        until (LazFileUtils.FindNextUTF8(PathInfo) <> 0);
      finally
        LazFileUtils.FindCloseUTF8(PathInfo);
      end;
  end;

var
  p: SizeInt;
  Dir: String;
  i: Integer;
  OtherDir: String;
begin
  if FSearching then RaiseSearchingError;

  MaskList := TMaskList.Create(ASearchMask, FMaskSeparator, CaseSensitive);
  // empty mask = all files mask
  if MaskList.Count = 0 then
    FreeAndNil(MaskList);

  FSearching := True;
  SearchDirectories:=TStringList.Create;
  try
    while ASearchPath<>'' do begin
      p:=Pos(';',ASearchPath);
      if p<1 then
        p:=length(ASearchPath)+1;
      Dir:=LazFileUtils.TrimFilename(LeftStr(ASearchPath,p-1));
      Delete(ASearchPath,1,p);
      if Dir='' then continue;
      Dir:=LazFileUtils.ChompPathDelim(Dir);
      for i:=SearchDirectories.Count-1 downto 0 do
      begin
        OtherDir:=SearchDirectories[i];
        if (LazFileUtils.CompareFilenames(Dir,OtherDir)=0)
        or (ASearchSubDirs and (FileIsInPath(Dir,OtherDir))) then
        begin
          // directory Dir is already searched
          Dir:='';
          break;
        end;
        if ASearchSubDirs and FileIsInPath(OtherDir,Dir) then
          // directory Dir includes the old directory => delete
          SearchDirectories.Delete(i);
      end;
      if Dir<>'' then
        SearchDirectories.Add(Dir);
    end;
    //Search currentdirectory if ASearchPath = ''
    if (SearchDirectories.Count=0) then
      DoSearch('',0)
    else
    begin
      for i:=0 to SearchDirectories.Count-1 do
        DoSearch(LazFileUtils.AppendPathDelim(SearchDirectories[i]), 0);
    end;
  finally
    SearchDirectories.Free;
    FSearching := False;
    if MaskList <> nil then MaskList.Free;
  end;
end;

