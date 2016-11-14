{
  All functions are thread safe unless explicitely stated
}
unit LazFileUtils;

{$mode objfpc}{$H+}
{$i lazutils_defines.inc}
interface

uses
  Classes, SysUtils, SysConst, LazUTF8, LazUtilsStrConsts;


{$IFDEF Windows}
  {$define CaseInsensitiveFilenames}
  {$define HasUNCPaths}
{$ENDIF}
{$IFDEF darwin}
  {$define CaseInsensitiveFilenames}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
  {$DEFINE NotLiteralFilenames} // e.g. HFS+ normalizes file names
{$ENDIF}

function CompareFilenames(const Filename1, Filename2: string): integer; overload;
function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer; overload;
function CompareFileExt(const Filename, Ext: string): integer; overload;

function CompareFilenameStarts(const Filename1, Filename2: string): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer): integer; overload;
function CompareFilenamesP(Filename1, Filename2: PChar;
  IgnoreCase: boolean = false // false = use default
  ): integer;

function DirPathExists(DirectoryName: string): boolean;
function DirectoryIsWritable(const DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
function FilenameIsAbsolute(const TheFilename: string):boolean;
function FilenameIsWinAbsolute(const TheFilename: string):boolean;
function FilenameIsUnixAbsolute(const TheFilename: string):boolean;
function ForceDirectory(DirectoryName: string): boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);
procedure CheckIfFileIsSymlink(const AFilename: string);
function FileIsExecutable(const AFilename: string): boolean;
function FileIsSymlink(const AFilename: string): boolean;
function FileIsHardLink(const AFilename: string): boolean;
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
function FilenameIsTrimmed(const TheFilename: string): boolean;
function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
function TrimFilename(const AFilename: string): string;
function ResolveDots(const AFilename: string): string;
Procedure ForcePathDelims(Var FileName: string);
Function GetForcedPathDelims(Const FileName: string): String;
function CleanAndExpandFilename(const Filename: string): string; // empty string returns current directory
function CleanAndExpandDirectory(const Filename: string): string; // empty string returns current directory
function TrimAndExpandFilename(const Filename: string; const BaseDir: string = ''): string; // empty string returns empty string
function TrimAndExpandDirectory(const Filename: string; const BaseDir: string = ''): string; // empty string returns empty string
function TryCreateRelativePath(const Dest, Source: String; UsePointDirectory: boolean;
                               AlwaysRequireSharedBaseFolder: Boolean; out RelPath: String): Boolean;
function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean = false; AlwaysRequireSharedBaseFolder: Boolean = True): string;
function FileIsInPath(const Filename, Path: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;

// search paths
function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string): string;
function MinimizeSearchPath(const SearchPath: string): string;
function FindPathInSearchPath(APath: PChar; APathLen: integer;
                              SearchPath: PChar; SearchPathLen: integer): PChar; overload;
function FindPathInSearchPath(const APath, SearchPath: string): integer; overload;

// file operations
function FileExistsUTF8(const Filename: string): boolean;
function FileAgeUTF8(const FileName: string): Longint;
function DirectoryExistsUTF8(const Directory: string): Boolean;
function ExpandFileNameUTF8(const FileName: string; {const} BaseDir: string = ''): string;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
function FindNextUTF8(var Rslt: TSearchRec): Longint;
procedure FindCloseUTF8(var F: TSearchrec); inline;
function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
function FileGetAttrUTF8(const FileName: String): Longint;
function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
function DeleteFileUTF8(const FileName: String): Boolean;
function RenameFileUTF8(const OldName, NewName: String): Boolean;
function FileSearchUTF8(const Name, DirList : String; ImplicitCurrentDir : Boolean = True): String;
function FileIsReadOnlyUTF8(const FileName: String): Boolean;
function GetCurrentDirUTF8: String;
function SetCurrentDirUTF8(const NewDir: String): Boolean;
function CreateDirUTF8(const NewDir: String): Boolean;
function RemoveDirUTF8(const Dir: String): Boolean;
function ForceDirectoriesUTF8(const Dir: string): Boolean;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle; overload;
function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle; overload;
Function FileCreateUtf8(Const FileName : String; ShareMode : Integer; Rights : Cardinal) : THandle; overload;

function FileSizeUtf8(const Filename: string): int64;
function GetFileDescription(const AFilename: string): string;
function ReadAllLinks(const Filename: string;
                 {%H-}ExceptionOnError: boolean): string; // if a link is broken returns ''
function TryReadAllLinks(const Filename: string): string; // if a link is broken returns Filename
function GetShellLinkTarget(const FileName: string): string;

// for debugging
function DbgSFileAttr(Attr: LongInt): String;


type
  TPhysicalFilenameOnError = (pfeException,pfeEmpty,pfeOriginal);
function GetPhysicalFilename(const Filename: string;
        OnError: TPhysicalFilenameOnError): string;
{$IFDEF Unix}
function GetUnixPhysicalFilename(const Filename: string;
                      ExceptionOnError: boolean): string; // if a link is broken returns ''
{$ENDIF}

function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean = false;
  CreateDir: boolean = false): string;
function GetTempFileNameUTF8(const Dir, Prefix: String): String;

// UNC paths
function IsUNCPath(const {%H-}Path: String): Boolean;
function ExtractUNCVolume(const {%H-}Path: String): String;
function ExtractFileRoot(FileName: String): String;

// darwin paths
{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
function GetDarwinNormalizedFilename(Filename: string; nForm:Integer=2): string;
{$ENDIF}

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);
function StrToCmdLineParam(const Param: string): string;
function MergeCmdLineParams(ParamList: TStrings): string;

type
  TInvalidateFileStateCacheEvent = procedure(const Filename: string);
var
  OnInvalidateFileStateCache: TInvalidateFileStateCacheEvent = nil;
procedure InvalidateFileStateCache(const Filename: string = ''); inline;

implementation

// to get more detailed error messages consider the os
uses
{$IFDEF Windows}
  Windows {$IFnDEF WinCE}, ShlObj, ActiveX, WinDirs{$ENDIF};
{$ELSE}
  {$IFDEF darwin}
  MacOSAll,
  {$ENDIF}
  Unix, BaseUnix;
{$ENDIF}

{$I lazfileutils.inc}
{$IFDEF windows}
  {$I winlazfileutils.inc}
{$ELSE}
  {$I unixlazfileutils.inc}
{$ENDIF}

function CompareFilenames(const Filename1, Filename2: string): integer;
{$IFDEF darwin}
var
  F1: CFStringRef;
  F2: CFStringRef;
{$ENDIF}
begin
  {$IFDEF darwin}
  if Filename1=Filename2 then exit(0);
  if (Filename1='') or (Filename2='') then
    exit(length(Filename2)-length(Filename1));
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Result:=CFStringCompare(F1,F2,kCFCompareNonliteral
          {$IFDEF CaseInsensitiveFilenames}+kCFCompareCaseInsensitive{$ENDIF});
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
    {$IFDEF CaseInsensitiveFilenames}
    Result:=UTF8CompareText(Filename1, Filename2);
    {$ELSE}
    Result:=CompareStr(Filename1, Filename2);
    {$ENDIF}
  {$ENDIF}
end;

function CompareFilenamesIgnoreCase(const Filename1, Filename2: string
  ): integer;
{$IFDEF darwin}
var
  F1: CFStringRef;
  F2: CFStringRef;
{$ENDIF}
begin
  {$IFDEF darwin}
  if Filename1=Filename2 then exit(0);
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Result:=CFStringCompare(F1,F2,kCFCompareNonliteral+kCFCompareCaseInsensitive);
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
  Result:=UTF8CompareText(Filename1, Filename2);
  {$ENDIF}
end;

function CompareFileExt(const Filename, Ext: string; CaseSensitive: boolean): integer;
// Ext can contain a point or not
var
  n, e : AnsiString;
  FileLen, FilePos, ExtLen, ExtPos: integer;
begin
  FileLen := length(Filename);
  ExtLen := length(Ext);
  FilePos := FileLen;
  while (FilePos>=1) and (Filename[FilePos]<>'.') do dec(FilePos);
  if FilePos < 1 then begin
    // no extension in filename
    Result:=1;
    exit;
  end;
  // skip point
  inc(FilePos);
  ExtPos := 1;
  if (ExtPos <= ExtLen) and (Ext[1] = '.') then inc(ExtPos);

  // compare extensions
  n := Copy(Filename, FilePos, length(FileName));
  e := Copy(Ext, ExtPos, length(Ext));
  if CaseSensitive then
    Result := CompareStr(n, e)
  else
    Result := UTF8CompareText(n, e);
  if Result < 0
    then Result := -1
  else
    if Result > 0 then Result := 1;
end;

function CompareFileExt(const Filename, Ext: string): integer;
begin
  Result := CompareFileExt(Filename, Ext, False);
end;

function ExtractFileNameOnly(const AFilename: string): string;
var
  StartPos: Integer;
  ExtPos: Integer;
begin
  StartPos:=length(AFilename)+1;
  while (StartPos>1)
  and not (AFilename[StartPos-1] in AllowDirectorySeparators)
  {$IFDEF Windows}and (AFilename[StartPos-1]<>':'){$ENDIF}
  do
    dec(StartPos);
  ExtPos:=length(AFilename);
  while (ExtPos>=StartPos) and (AFilename[ExtPos]<>'.') do
    dec(ExtPos);
  if (ExtPos<StartPos) then ExtPos:=length(AFilename)+1;
  Result:=copy(AFilename,StartPos,ExtPos-StartPos);
end;

{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
var
  s: CFStringRef;
  l: CFIndex;
begin
  if Filename='' then exit('');
  s:=CFStringCreateWithCString(nil,Pointer(Filename),kCFStringEncodingUTF8);
  l:=CFStringGetMaximumSizeOfFileSystemRepresentation(s);
  SetLength(Result,l);
  if Result<>'' then begin
    CFStringGetFileSystemRepresentation(s,@Result[1],length(Result));
    SetLength(Result,StrLen(PChar(Result)));
  end;
  CFRelease(s);
end;

// borrowed from CarbonProcs
function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(AString);

    CFStringGetBytes(AString, StrRange, Encoding,
      Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
      CFStringGetBytes(AString, StrRange, Encoding,
        Ord('?'), False, @Result[1], StrSize, StrSize);
  end;
end;

//NForm can be one of
//kCFStringNormalizationFormD = 0; // Canonical Decomposition
//kCFStringNormalizationFormKD = 1; // Compatibility Decomposition
//kCFStringNormalizationFormC = 2; // Canonical Decomposition followed by Canonical Composition
//kCFStringNormalizationFormKC = 3; // Compatibility Decomposition followed by Canonical Composition
function GetDarwinNormalizedFilename(Filename: string; nForm:Integer=2): string;
var
  theString: CFStringRef;
  Mutable: CFMutableStringRef;
begin
  theString:=CFStringCreateWithCString(nil, Pointer(FileName), kCFStringEncodingUTF8);
  Mutable := CFStringCreateMutableCopy(nil, 0, theString);
  if (NForm<0) or (NForm>3) then NForm := kCFStringNormalizationFormC;
  CFStringNormalize(Mutable, NForm);
  Result := CFStringToStr(Mutable,  kCFStringEncodingUTF8);
  CFRelease(Mutable);
  CFRelease(theString);
end;

{$ENDIF}

function CompareFilenameStarts(const Filename1, Filename2: string): integer;
var
  len1: Integer;
  len2: Integer;
begin
  len1:=length(Filename1);
  len2:=length(Filename2);
  if len1=len2 then begin
    Result:=CompareFilenames(Filename1,Filename2);
    exit;
  end else if len1>len2 then
    Result:=CompareFilenames(copy(Filename1,1,len2),Filename2)
  else
    Result:=CompareFilenames(Filename1,copy(Filename2,1,len1));
  if Result<>0 then exit;
  if len1<len2 then
    Result:=-1
  else
    Result:=1;
end;

function CompareFilenames(Filename1: PChar; Len1: integer; Filename2: PChar;
  Len2: integer): integer;
var
  {$IFDEF NotLiteralFilenames}
  File1: string;
  File2: string;
  {$ELSE}
  i: Integer;
  {$ENDIF}
begin
  if (Len1=0) or (Len2=0) then begin
    Result:=Len1-Len2;
    exit;
  end;
  {$IFDEF NotLiteralFilenames}
  SetLength(File1,Len1);
  System.Move(Filename1^,File1[1],Len1);
  SetLength(File2,Len2);
  System.Move(Filename2^,File2[1],Len2);
  Result:=CompareFilenames(File1,File2);
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

function CompareFilenamesP(Filename1, Filename2: PChar;
  IgnoreCase: boolean = false): integer;
var
  {$IFDEF darwin}
  F1: CFStringRef;
  F2: CFStringRef;
  Flags: CFStringCompareFlags;
  {$ELSE}
  File1, File2: string;
  Len1: SizeInt;
  Len2: SizeInt;
  {$ENDIF}
begin
  if (Filename1=nil) or (Filename1^=#0) then begin
    if (Filename2=nil) or (Filename2^=#0) then begin
      // both empty
      exit(0);
    end else begin
      // filename1 empty, filename2 not empty
      exit(-1);
    end;
  end else if (Filename2=nil) or (Filename2^=#0) then begin
    // filename1 not empty, filename2 empty
    exit(1);
  end;

  {$IFDEF CaseInsensitiveFilenames}
  // this platform is by default case insensitive
  IgnoreCase:=true;
  {$ENDIF}
  {$IFDEF darwin}
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Flags:=kCFCompareNonliteral;
  if IgnoreCase then Flags+=kCFCompareCaseInsensitive;
  Result:=CFStringCompare(F1,F2,Flags);
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
  if IgnoreCase then begin
    // compare case insensitive
    Len1:=StrLen(Filename1);
    SetLength(File1,Len1);
    System.Move(Filename1^,File1[1],Len1);
    Len2:=StrLen(Filename2);
    SetLength(File2,Len2);
    System.Move(Filename2^,File2[1],Len2);
    Result:=UTF8CompareText(File1,File2);
  end else begin
    // compare literally
    while (Filename1^=Filename2^) and (Filename1^<>#0) do begin
      inc(Filename1);
      Inc(Filename2);
    end;
    Result:=ord(Filename1^)-ord(Filename2^);
  end;
  {$ENDIF}
end;

function DirPathExists(DirectoryName: string): boolean;
begin
  Result:=DirectoryExistsUTF8(ChompPathDelim(DirectoryName));
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
var
  TempFilename: String;
  s: String;
  fHandle: THANDLE;
begin
  Result:=false;
  if not DirPathExists(DirectoryName) then exit;
  TempFilename:=SysUtils.GetTempFilename(AppendPathDelim(DirectoryName),'tstperm');
  fHandle := FileCreateUtf8(TempFileName, fmCreate, 438);
  if (THandle(fHandle) <> feInvalidHandle) then
  begin
    s:='WriteTest';
    if FileWrite(fHandle,S[1],Length(S)) > 0 then Result := True;
    FileClose(fHandle);
    if not DeleteFileUTF8(TempFilename) then
      InvalidateFileStateCache(TempFilename);
  end;
end;

function ForceDirectory(DirectoryName: string): boolean;
var i: integer;
  Dir: string;
begin
  DirectoryName:=AppendPathDelim(DirectoryName);
  i:=1;
  while i<=length(DirectoryName) do begin
    if DirectoryName[i] in AllowDirectorySeparators then begin
      Dir:=copy(DirectoryName,1,i-1);
      if not DirPathExists(Dir) then begin
        Result:=CreateDirUTF8(Dir);
        if not Result then exit;
      end;
    end;
    inc(i);
  end;
  Result:=true;
end;


function FileIsText(const AFilename: string): boolean;
var
  FileReadable: Boolean;
begin
  Result:=FileIsText(AFilename,FileReadable);
  if FileReadable then ;
end;

function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
var
  Buf: string;
  Len: integer;
  NewLine: boolean;
  p: PChar;
  ZeroAllowed: Boolean;
  fHandle: THandle;
begin
  Result:=false;
  FileReadable:=true;
  fHandle := FileOpenUtf8(AFileName, fmOpenRead or fmShareDenyNone);
  if (THandle(fHandle) <> feInvalidHandle)  then
  begin
    try
      Len:=1024;
      SetLength(Buf,Len+1);
      Len := FileRead(fHandle,Buf[1],Len);

      if Len>0 then begin
        Buf[Len+1]:=#0;
        p:=PChar(Buf);
        ZeroAllowed:=false;
        if (p[0]=#$EF) and (p[1]=#$BB) and (p[2]=#$BF) then begin
          // UTF-8 BOM (Byte Order Mark)
          inc(p,3);
        end else if (p[0]=#$FF) and (p[1]=#$FE) then begin
          // ucs-2le BOM FF FE
          inc(p,2);
          ZeroAllowed:=true;
        end else if (p[0]=#$FE) and (p[1]=#$FF) then begin
          // ucs-2be BOM FE FF
          inc(p,2);
          ZeroAllowed:=true;
        end;
        NewLine:=false;
        while true do begin
          case p^ of
          #0:
            if p-PChar(Buf)>=Len then
              break
            else if not ZeroAllowed then
              exit;
          // #10,#13: new line
          // #12: form feed
          // #26: end of file
          #1..#8,#11,#14..#25,#27..#31: exit;
          #10,#13: NewLine:=true;
          end;
          inc(p);
        end;
        if NewLine or (Len<1024) then
          Result:=true;
      end else
        Result:=true;
    finally
      FileClose(fHandle);
    end
  end
  else
    FileReadable := False;
end;

function FilenameIsTrimmed(const TheFilename: string): boolean;
begin
  Result:=FilenameIsTrimmed(PChar(Pointer(TheFilename)),// pointer type cast avoids #0 check
                            length(TheFilename));
end;

function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
var
  i: Integer;
  c: Char;
begin
  Result:=false;
  if NameLen<=0 then begin
    Result:=true;
    exit;
  end;
  // check heading spaces
  if StartPos[0]=' ' then exit;
  // check trailing spaces
  if StartPos[NameLen-1]=' ' then exit;
  // check ./ at start
  if (StartPos[0]='.') and (StartPos[1] in AllowDirectorySeparators) then exit;
  i:=0;
  while i<NameLen do begin
    c:=StartPos[i];
    if not (c in AllowDirectorySeparators) then
      inc(i)
    else begin
      if c<>PathDelim then exit;
      inc(i);
      if i=NameLen then break;

      // check for double path delimiter
      if (StartPos[i] in AllowDirectorySeparators) then exit;

      if (StartPos[i]='.') and (i>0) then begin
        inc(i);
        // check /./ or /. at end
        if (StartPos[i] in AllowDirectorySeparators) or (i=NameLen) then exit;
        if StartPos[i]='.' then begin
          inc(i);
          // check /../ or /.. at end
          if (StartPos[i] in AllowDirectorySeparators) or (i=NameLen) then exit;
        end;
      end;
    end;
  end;
  Result:=true;
end;

function TrimFilename(const AFilename: string): string;
//Trim leading and trailing spaces
//then call ResolveDots to trim double path delims and expand special dirs like .. and .

var
  Len, Start: Integer;
begin
  Result := AFileName;
  Len := Length(AFileName);
  if (Len = 0) or FilenameIsTrimmed(Result) then exit;
  if AFilename[1] = #32 then
  begin
    Start := 1;
    while (Start <= Len) and (AFilename[Start] = #32) do Inc(Start);
    System.Delete(Result,1,Start-1);
    Len := Length(Result);
  end;
  while (Len > 0) and (Result[Len] = #32) do Dec(Len);
  SetLength(Result, Len);
  Result := ResolveDots(Result);
end;

procedure ForcePathDelims(var FileName: string);
var
  i: Integer;
begin
  for i:=1 to length(FileName) do
    {$IFDEF Windows}
    if Filename[i]='/' then
      Filename[i]:='\';
    {$ELSE}
    if Filename[i]='\' then
      Filename[i]:='/';
    {$ENDIF}
end;

function GetForcedPathDelims(const FileName: string): String;
begin
  Result:=FileName;
  ForcePathDelims(Result);
end;

{------------------------------------------------------------------------------
  function CleanAndExpandFilename(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandFilename(const Filename: string): string;
begin
  Result:=ExpandFileNameUTF8(TrimFileName(Filename));
end;

{------------------------------------------------------------------------------
  function CleanAndExpandDirectory(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result:=AppendPathDelim(CleanAndExpandFilename(Filename));
end;

function TrimAndExpandFilename(const Filename: string; const BaseDir: string): string;
begin
  Result:=ChompPathDelim(TrimFilename(Filename));
  if Result='' then exit;
  Result:=TrimFilename(ExpandFileNameUTF8(Result,BaseDir));
end;

function TrimAndExpandDirectory(const Filename: string; const BaseDir: string): string;
begin
  Result:=TrimFilename(Filename);
  if Result='' then exit;
  Result:=TrimFilename(AppendPathDelim(ExpandFileNameUTF8(Result,BaseDir)));
end;



{------------------------------------------------------------------------------
  function FileIsInPath(const Filename, Path: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsInPath(const Filename, Path: string): boolean;
var
  ExpFile: String;
  ExpPath: String;
  l: integer;
begin
  if Path='' then begin
    Result:=false;
    exit;
  end;
  ExpFile:=ResolveDots(Filename);
  ExpPath:=AppendPathDelim(ResolveDots(Path));
  l:=length(ExpPath);
  Result:=(l>0) and (length(ExpFile)>l) and (ExpFile[l]=PathDelim)
          and (CompareFilenames(ExpPath,LeftStr(ExpFile,l))=0);
end;

function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and not (Path[length(Path)] in AllowDirectorySeparators) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

function ChompPathDelim(const Path: string): string;
var
  Len, MinLen: Integer;
begin
  if Path = '' then
    exit;

  Result:=Path;
  Len:=length(Result);
  if (Result[1] in AllowDirectorySeparators) then begin
    MinLen := 1;
    {$IFDEF HasUNCPaths}
    if (Len >= 2) and (Result[2] in AllowDirectorySeparators) then
      MinLen := 2; // keep UNC '\\', chomp 'a\' to 'a'
    {$ENDIF}
  end
  else begin
    MinLen := 0;
    {$IFdef MSWindows}
    if (Len >= 3) and (Result[1] in ['a'..'z', 'A'..'Z'])  and
       (Result[2] = ':') and (Result[3] in AllowDirectorySeparators)
    then
      MinLen := 3;
    {$ENDIF}
  end;

  while (Len > MinLen) and (Result[Len] in AllowDirectorySeparators) do dec(Len);
  if Len<length(Result) then
    SetLength(Result,Len);
end;

function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string
  ): string;
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
  BaseDir:=AppendPathDelim(BaseDirectory);

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
    if not FilenameIsAbsolute(CurDir) then begin
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

function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string
  ): string;
var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
begin
  Result:=SearchPath;
  if (SearchPath='') or (BaseDirectory='') then exit;

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
    if FilenameIsAbsolute(CurDir) then begin
      NewCurDir:=CreateRelativePath(CurDir,BaseDirectory);
      if (NewCurDir<>CurDir) and (NewCurDir='') then
        NewCurDir:='.';
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

function MinimizeSearchPath(const SearchPath: string): string;
// trim the paths, remove doubles and empty paths
var
  StartPos: Integer;
  EndPos: LongInt;
  NewPath: String;
begin
  Result:=SearchPath;
  StartPos:=1;
  while StartPos<=length(Result) do begin
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do
      inc(EndPos);
    if StartPos<EndPos then begin
      // trim path and chomp PathDelim
      if (Result[EndPos-1] in AllowDirectorySeparators)
      or (not FilenameIsTrimmed(@Result[StartPos],EndPos-StartPos)) then begin
        NewPath:=ChompPathDelim(
                           TrimFilename(copy(Result,StartPos,EndPos-StartPos)));
        Result:=copy(Result,1,StartPos-1)+NewPath+copy(Result,EndPos,length(Result));
        EndPos:=StartPos+length(NewPath);
      end;
      // check if path already exists
      if (Length(Result) > 0) and
         (FindPathInSearchPath(@Result[StartPos],EndPos-StartPos, @Result[1],StartPos-1) <> nil)
      then begin
        // remove path
        System.Delete(Result,StartPos,EndPos-StartPos+1);
      end else begin
        StartPos:=EndPos+1;
      end;
    end else begin
      // remove empty path
      System.Delete(Result,StartPos,1);
    end;
  end;
  if (Result<>'') and (Result[length(Result)]=';') then
    SetLength(Result,length(Result)-1);
end;

function FindPathInSearchPath(APath: PChar; APathLen: integer;
  SearchPath: PChar; SearchPathLen: integer): PChar;
var
  StartPos: Integer;
  EndPos: LongInt;
  NextStartPos: LongInt;
  CmpPos: LongInt;
  UseQuickCompare: Boolean;
  PathStr: String;
  CurFilename: String;
begin
  Result:=nil;
  if SearchPath=nil then exit;
  if (APath=nil) or (APathLen=0) then exit;
  // ignore trailing PathDelim at end
  while (APathLen>1) and (APath[APathLen-1] in AllowDirectorySeparators) do dec(APathLen);

  {$IFDEF CaseInsensitiveFilenames}
  UseQuickCompare:=false;
  {$ELSE}
    {$IFDEF NotLiteralFilenames}
    CmpPos:=0;
    while (CmpPos<APathLen) and (ord(APath[CmpPos]<128)) do inc(CmpPos);
    UseQuickCompare:=CmpPos=APathLen;
    {$ELSE}
    UseQuickCompare:=true;
    {$ENDIF}
  {$ENDIF}
  if not UseQuickCompare then begin
    SetLength(PathStr,APathLen);
    System.Move(APath^,PathStr[1],APathLen);
  end;

  StartPos:=0;
  while StartPos<SearchPathLen do begin
    // find current path bounds
    NextStartPos:=StartPos;
    while (SearchPath[NextStartPos]<>';') and (NextStartPos<SearchPathLen) do
      inc(NextStartPos);
    EndPos:=NextStartPos;
    // ignore trailing PathDelim at end
    while (EndPos>StartPos+1) and (SearchPath[EndPos-1] in AllowDirectorySeparators) do
      dec(EndPos);
    // compare current path
    if UseQuickCompare then begin
      if EndPos-StartPos=APathLen then begin
        CmpPos:=0;
        while CmpPos<APathLen do begin
          if APath[CmpPos]<>SearchPath[StartPos+CmpPos] then
            break;
          inc(CmpPos);
        end;
        if CmpPos=APathLen then begin
          Result:=@SearchPath[StartPos];
          exit;
        end;
      end;
    end else if EndPos>StartPos then begin
      // use CompareFilenames
      CurFilename:='';
      SetLength(CurFilename,EndPos-StartPos);
      System.Move(SearchPath[StartPos],CurFilename[1],EndPos-StartPos);
      if CompareFilenames(PathStr,CurFilename)=0 then begin
        Result:=@SearchPath[StartPos];
        exit;
      end;
    end;
    StartPos:=NextStartPos+1;
  end;
end;

function FindPathInSearchPath(const APath, SearchPath: string): integer;
var
  p: PChar;
  SearchP: PChar;
begin
  SearchP:=PChar(SearchPath);
  p:=FindPathInSearchPath(PChar(APath),length(APath),SearchP,length(SearchPath));
  if p=nil then
    Result:=-1
  else
    Result:=p-SearchP+1;
end;

function FileSearchUTF8(const Name, DirList: String; ImplicitCurrentDir : Boolean = True): String;
Var
  I : longint;
  Temp : String;

begin
  Result:=Name;
  temp:=SetDirSeparators(DirList);
  // Start with checking the file in the current directory
  If ImplicitCurrentDir and (Result <> '') and FileExistsUTF8(Result) Then
    exit;
  while True do begin
    If Temp = '' then
      Break; // No more directories to search - fail
    I:=pos(PathSeparator,Temp);
    If I<>0 then
      begin
        Result:=Copy (Temp,1,i-1);
        system.Delete(Temp,1,I);
      end
    else
      begin
        Result:=Temp;
        Temp:='';
      end;
    If Result<>'' then
      Result:=AppendPathDelim(Result)+Name;
    If (Result <> '') and FileExistsUTF8(Result) Then
      exit;
  end;
  Result:='';
end;

function FileIsReadOnlyUTF8(const FileName: String): Boolean;
begin
  Result:=FileGetAttrUTF8(FileName) and faReadOnly > 0;
end;



function GetTempFileNameUTF8(const Dir, Prefix: String): String;
var
  I: Integer;
  Start: String;
begin
  if Assigned(OnGetTempFile) then
    Result:=OnGetTempFile(Dir,Prefix)
  else
  begin
    if (Dir='') then
      Start:=GetTempDir
    else
      Start:=IncludeTrailingPathDelimiter(Dir);
    if (Prefix='') then
      Start:=Start+'TMP'
    else
      Start:=Start+Prefix;
    I:=0;
    repeat
      Result:=Format('%s%.5d.tmp',[Start,I]);
      Inc(I);
    until not FileExistsUTF8(Result);
  end;
end;

function ForceDirectoriesUTF8(const Dir: string): Boolean;
var
  E: EInOutError;
  ADrv : String;

  function DoForceDirectories(Const Dir: string): Boolean;
  var
    ADir : String;
    APath: String;
  begin
    Result:=True;
    ADir:=ExcludeTrailingPathDelimiter(Dir);
    if (ADir='') then Exit;
    if Not DirectoryExistsUTF8(ADir) then
      begin
        APath := ExtractFilePath(ADir);
        //this can happen on Windows if user specifies Dir like \user\name/test/
        //and would, if not checked for, cause an infinite recusrsion and a stack overflow
        if (APath = ADir) then
          Result := False
        else
          Result:=DoForceDirectories(APath);
        if Result then
          Result := CreateDirUTF8(ADir);
      end;
  end;

  function IsUncDrive(const Drv: String): Boolean;
  begin
    Result := (Length(Drv) > 2) and (Drv[1] in AllowDirectorySeparators) and (Drv[2] in AllowDirectorySeparators);
  end;

begin
  Result := False;
  ADrv := ExtractFileDrive(Dir);
  if (ADrv<>'') and (not DirectoryExistsUTF8(ADrv))
  {$IFNDEF FORCEDIR_NO_UNC_SUPPORT} and (not IsUncDrive(ADrv)){$ENDIF} then Exit;
  if Dir='' then
    begin
      E:=EInOutError.Create(SCannotCreateEmptyDir);
      E.ErrorCode:=3;
      Raise E;
    end;
  Result := DoForceDirectories(GetForcedPathDelims(Dir));
end;

function TryReadAllLinks(const Filename: string): string;
begin
  Result:=ReadAllLinks(Filename,false);
  if Result='' then
    Result:=Filename;
end;

procedure InvalidateFileStateCache(const Filename: string);
begin
  if Assigned(OnInvalidateFileStateCache) then
    OnInvalidateFileStateCache(Filename);
end;

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);
// split spaces, quotes are parsed as single parameter
// if ReadBackslash=true then \" is replaced to " and not treated as quote
// #0 is always end
type
  TMode = (mNormal,mApostrophe,mQuote);
var
  p: Integer;
  Mode: TMode;
  Param: String;
begin
  p:=1;
  while p<=length(Params) do
  begin
    // skip whitespace
    while (p<=length(Params)) and (Params[p] in [' ',#9,#10,#13]) do inc(p);
    if (p>length(Params)) or (Params[p]=#0) then
      break;
    //writeln('SplitCmdLineParams After Space p=',p,'=[',Params[p],']');
    // read param
    Param:='';
    Mode:=mNormal;
    while p<=length(Params) do
    begin
      case Params[p] of
      #0:
        break;
      '\':
        begin
          inc(p);
          if ReadBackslash then
            begin
            // treat next character as normal character
            if (p>length(Params)) or (Params[p]=#0) then
              break;
            if ord(Params[p])<128 then
            begin
              Param+=Params[p];
              inc(p);
            end else begin
              // next character is already a normal character
            end;
          end else begin
            // treat backslash as normal character
            Param+='\';
          end;
        end;
      '''':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mApostrophe;
          mApostrophe:
            Mode:=mNormal;
          mQuote:
            Param+='''';
          end;
        end;
      '"':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mQuote;
          mApostrophe:
            Param+='"';
          mQuote:
            Mode:=mNormal;
          end;
        end;
      ' ',#9,#10,#13:
        begin
          if Mode=mNormal then break;
          Param+=Params[p];
          inc(p);
        end;
      else
        Param+=Params[p];
        inc(p);
      end;
    end;
    //writeln('SplitCmdLineParams Param=#'+Param+'#');
    ParamList.Add(Param);
  end;
end;

function StrToCmdLineParam(const Param: string): string;
{ <empty> -> ''
  word -> word
  word1 word2 -> 'word1 word2'
  word's -> "word's"
  a" -> 'a"'
  "a" -> '"a"'
  'a' -> "'a'"
  #0 character -> cut the rest
}
const
  NoQuot = ' ';
  AnyQuot = '*';
var
  Quot: Char;
  p: PChar;
  i: Integer;
begin
  Result:=Param;
  if Result='' then
    Result:=''''''
  else begin
    p:=PChar(Result);
    Quot:=NoQuot;
    repeat
      case p^ of
      #0:
        begin
          i:=p-PChar(Result);
          if i<length(Result) then
            Delete(Result,i+1,length(Result));
          case Quot of
          AnyQuot: Result:=''''+Result+'''';
          '''': Result+='''';
          '"':  Result+='"';
          end;
          break;
        end;
      ' ',#9,#10,#13:
        begin
          if Quot=NoQuot then
            Quot:=AnyQuot;
          inc(p);
        end;
      '''':
        begin
          case Quot of
          NoQuot,AnyQuot:
            begin
              // need "
              Quot:='"';
              i:=p-PChar(Result);
              System.Insert('"',Result,1);
              p:=PChar(Result)+i+1;
            end;
          '"':
            inc(p);
          '''':
            begin
              // ' within a '
              // => end ', start "
              i:=p-PChar(Result)+1;
              System.Insert('''"',Result,i);
              p:=PChar(Result)+i+1;
              Quot:='"';
            end;
          end;
        end;
      '"':
        begin
          case Quot of
          NoQuot,AnyQuot:
            begin
              // need '
              Quot:='''';
              i:=p-PChar(Result);
              System.Insert('''',Result,1);
              p:=PChar(Result)+i+1;
            end;
          '''':
            inc(p);
          '"':
            begin
              // " within a "
              // => end ", start '
              i:=p-PChar(Result)+1;
              System.Insert('"''',Result,i);
              p:=PChar(Result)+i+1;
              Quot:='''';
            end;
          end;
        end;
      else
        inc(p);
      end;
    until false;
  end;
end;

function MergeCmdLineParams(ParamList: TStrings): string;
var
  i: Integer;
begin
  Result:='';
  if ParamList=nil then exit;
  for i:=0 to ParamList.Count-1 do
  begin
    if i>0 then Result+=' ';
    Result+=StrToCmdLineParam(ParamList[i]);
  end;
end;

{
  Returns
  - DriveLetter + : + PathDelim on Windows (if present) or
  - UNC Share on Windows if present or
  - PathDelim if FileName starts with PathDelim on Unix or Wince or
  - Empty string of non eof the above applies
}
function ExtractFileRoot(FileName: String): String;
var
  Len: Integer;
begin
  Result := '';
  Len := Length(FileName);
  if (Len > 0) then
  begin
    if IsUncPath(FileName) then
    begin
      Result := ExtractUNCVolume(FileName);
      // is it like \\?\C:\Directory?  then also include the "C:\" part
      if (Result = '\\?\') and (Length(FileName) > 6) and
         (FileName[5] in ['a'..'z','A'..'Z']) and (FileName[6] = ':') and (FileName[7] in AllowDirectorySeparators)
      then
        Result := Copy(FileName, 1, 7);
    end
    else
    begin
      {$if defined(unix) or defined(wince)}
      if (FileName[1] = PathDelim) then Result := PathDelim;
      {$else}
      if (Len > 2) and (FileName[1] in ['a'..'z','A'..'Z']) and (FileName[2] = ':') and (FileName[3] in AllowDirectorySeparators) then
        Result := UpperCase(Copy(FileName,1,3));
      {$endif}
    end;
  end;
end;

initialization
  InitLazFileUtils;

finalization
  FinalizeLazFileUtils;

end.

end.

