{
 /***************************************************************************
                               fileutil.pas
                               ------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the LazUtils package.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ ****************************************************************************
BB: 2013-05-19

Note to developers:

This unit should contain functions and procedures to
maintain compatibility with Delphi's FileUtil unit.

File routines that specifically deal with UTF8 filenames should go into
the LazFileUtils unit.

***************************************************************************** }
unit FileUtil;

{$mode objfpc}{$H+}
{$i lazutils_defines.inc}

interface

uses
  Classes, SysUtils,
  Masks, LazUTF8, LazFileUtils, StrUtils;
  
{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

const
  UTF8FileHeader = #$ef#$bb#$bf;
  FilenamesCaseSensitive = {$IFDEF CaseInsensitiveFilenames}false{$ELSE}true{$ENDIF};// lower and upper letters are treated the same
  FilenamesLiteral = {$IFDEF NotLiteralFilenames}false{$ELSE}true{$ENDIF};// file names can be compared using = string operator

// basic functions similar to the RTL but working with UTF-8 instead of the
// system encoding

// AnsiToUTF8 and UTF8ToAnsi need a widestring manager under Linux, BSD, MacOSX
// but normally these OS use UTF-8 as system encoding so the widestringmanager
// is not needed.
{$IFnDEF DisableWrapperFunctions}
// *** Wrappers for LazUTF8 ***
function NeedRTLAnsi: boolean; inline; deprecated 'Use the function in LazUTF8 unit';
procedure SetNeedRTLAnsi(NewValue: boolean); inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8ToSys(const s: string): string; inline; deprecated 'Use the function in LazUTF8 unit';
function SysToUTF8(const s: string): string; inline; deprecated 'Use the function in LazUTF8 unit';
function ConsoleToUTF8(const s: string): string; inline; deprecated 'Use the function in LazUTF8 unit';
function UTF8ToConsole(const s: string): string; inline; deprecated 'Use the function in LazUTF8 unit';
// environment
function ParamStrUTF8(Param: Integer): string; inline; deprecated 'Use the function in LazUTF8 unit';
function GetEnvironmentStringUTF8(Index: Integer): string; inline; deprecated 'Use the function in LazUTF8 unit';
function GetEnvironmentVariableUTF8(const EnvVar: string): String; inline; deprecated 'Use the function in LazUTF8 unit';
// other
function SysErrorMessageUTF8(ErrorCode: Integer): String; inline; deprecated 'Use the function in LazUTF8 unit';
// *** Wrappers for LazFileUtils ***
// environment
function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string; inline; deprecated 'Use the function in LazFileUtils unit';
function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean = false;
  CreateDir: boolean = false): string; inline; deprecated 'Use the function in LazFileUtils unit';
// file operations
function ExtractFileNameOnly(const AFilename: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function FileExistsUTF8(const Filename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileAgeUTF8(const FileName: string): Longint; inline; deprecated 'Use the function in LazFileUtils unit';
function DirectoryExistsUTF8(const Directory: string): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint; inline; deprecated 'Use the function in LazFileUtils unit';
function FindNextUTF8(var Rslt: TSearchRec): Longint; inline; deprecated 'Use the function in LazFileUtils unit';
procedure FindCloseUTF8(var F: TSearchrec); inline; deprecated 'Use the function in LazFileUtils unit';
function FileSetDateUTF8(const FileName: String; Age: Longint): Longint; inline; deprecated 'Use the function in LazFileUtils unit';
function FileGetAttrUTF8(const FileName: String): Longint; inline; deprecated 'Use the function in LazFileUtils unit';
function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint; inline; deprecated 'Use the function in LazFileUtils unit';
function DeleteFileUTF8(const FileName: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function RenameFileUTF8(const OldName, NewName: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileSearchUTF8(const Name, DirList : String; ImplicitCurrentDir : Boolean = True): String; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsReadOnlyUTF8(const FileName: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function GetCurrentDirUTF8: String; inline; deprecated 'Use the function in LazFileUtils unit';
function SetCurrentDirUTF8(const NewDir: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function CreateDirUTF8(const NewDir: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function RemoveDirUTF8(const Dir: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function ForceDirectoriesUTF8(const Dir: string): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle; inline; deprecated 'Use the function in LazFileUtils unit';
function FileCreateUTF8(Const FileName : string) : THandle; overload; inline; deprecated 'Use the function in LazFileUtils unit';
function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle; overload; inline; deprecated 'Use the function in LazFileUtils unit';
function GetTempFilename(const Directory, Prefix: string): string; inline; deprecated 'Use the function GetTempFileNameUTF8 in LazFileUtils unit';
// file names, attributes and states
function CleanAndExpandFilename(const Filename: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function CleanAndExpandDirectory(const Filename: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function ExpandFileNameUTF8(const FileName: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function CompareFileExt(const Filename, Ext: string; CaseSensitive: boolean): integer; overload; inline; deprecated 'Use the function in LazFileUtils unit';
function CompareFileExt(const Filename, Ext: string): integer; overload; inline; deprecated 'Use the function in LazFileUtils unit';
function CompareFilenames(const Filename1, Filename2: string): integer; inline; deprecated 'Use the function in LazFileUtils unit';
function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer; inline; deprecated 'Use the function in LazFileUtils unit';
function FilenameIsAbsolute(const TheFilename: string):boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FilenameIsWinAbsolute(const TheFilename: string):boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FilenameIsUnixAbsolute(const TheFilename: string):boolean; inline; deprecated 'Use the function in LazFileUtils unit';
procedure CheckIfFileIsExecutable(const AFilename: string); inline; deprecated 'Use the function in LazFileUtils unit';
procedure CheckIfFileIsSymlink(const AFilename: string); inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsReadable(const AFilename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsWritable(const AFilename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsText(const AFilename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsText(const AFilename: string; out FileReadable: boolean): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsExecutable(const AFilename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsSymlink(const AFilename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function FileIsHardLink(const AFilename: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function GetFileDescription(const AFilename: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function ReadAllLinks(const Filename: string; ExceptionOnError: boolean): string; inline; deprecated 'Use the function in LazFileUtils unit';
function TryReadAllLinks(const Filename: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function TrimFilename(const AFilename: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
// directories
function DirPathExists(const FileName: String): Boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function ForceDirectory(DirectoryName: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function DirectoryIsWritable(const DirectoryName: string): boolean; inline; deprecated 'Use the function in LazFileUtils unit';
function AppendPathDelim(const Path: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function ChompPathDelim(const Path: string): string; inline; deprecated 'Use the function in LazFileUtils unit';
function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean = false; AlwaysRequireSharedBaseFolder: Boolean = True): string; inline; deprecated 'Use the function in LazFileUtils unit';
{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string; inline;
{$ENDIF}

{$ENDIF DisableWrapperFunctions}

// file and directory operations
function ComparePhysicalFilenames(const Filename1, Filename2: string): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer; overload;
function ExtractShortPathNameUTF8(Const FileName : String) : String;
function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
function ProgramDirectory: string;

function ExpandUNCFileNameUTF8(const FileName: string): string;
function FileSize(const Filename: string): int64; overload; inline;
function ExtractFileNameWithoutExt(const AFilename: string): string;
function FilenameIsPascalUnit(const Filename: string): boolean;
function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
function CreateAbsolutePath(const Filename, BaseDirectory: string): string;
function FileIsInPath(const Filename, Path: string): boolean;
function FileIsInDirectory(const Filename, Directory: string): boolean;

function GetAllFilesMask: string; inline;
function GetExeExt: string; inline;
function ReadFileToString(const Filename: string): string;

// file search
type
  TSearchFileInPathFlag = (
    sffDontSearchInBasePath,
    sffSearchLoUpCase
    );
  TSearchFileInPathFlags = set of TSearchFileInPathFlag;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): string; overload;
function SearchAllFilesInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): TStrings;
function FindDiskFilename(const Filename: string): string;
function FindDiskFileCaseInsensitive(const Filename: string): string;
function FindDefaultExecutablePath(const Executable: string; const BaseDir: string = ''): string;

type

  { TFileIterator }

  TFileIterator = class
  private
    FPath: String;
    FLevel: Integer;
    FFileInfo: TSearchRec;
    FSearching: Boolean;
    function GetFileName: String;
  public
    procedure Stop;

    function IsDirectory: Boolean;
  public
    property FileName: String read GetFileName;
    property FileInfo: TSearchRec read FFileInfo;
    property Level: Integer read FLevel;
    property Path: String read FPath;

    property Searching: Boolean read FSearching;
  end;

  TFileFoundEvent = procedure (FileIterator: TFileIterator) of object;
  TDirectoryFoundEvent = procedure (FileIterator: TFileIterator) of object;
  TDirectoryEnterEvent = procedure (FileIterator: TFileIterator) of object;

  { TFileSearcher }

  TFileSearcher = class(TFileIterator)
  private
    FMaskSeparator: char;
    FFollowSymLink: Boolean;
    FOnFileFound: TFileFoundEvent;
    FOnDirectoryFound: TDirectoryFoundEvent;
    FOnDirectoryEnter: TDirectoryEnterEvent;
    FFileAttribute: Word;
    FDirectoryAttribute: Word;
    procedure RaiseSearchingError;
  protected
    procedure DoDirectoryEnter; virtual;
    procedure DoDirectoryFound; virtual;
    procedure DoFileFound; virtual;
  public
    constructor Create;
    procedure Search(ASearchPath: String; ASearchMask: String = '';
      ASearchSubDirs: Boolean = True; CaseSensitive: Boolean = False);
  public
    property MaskSeparator: char read FMaskSeparator write FMaskSeparator;
    property FollowSymLink: Boolean read FFollowSymLink write FFollowSymLink;
    property FileAttribute: Word read FFileAttribute write FFileAttribute default faAnyfile;
    property DirectoryAttribute: Word read FDirectoryAttribute write FDirectoryAttribute default faDirectory;
    property OnDirectoryFound: TDirectoryFoundEvent read FOnDirectoryFound write FOnDirectoryFound;
    property OnFileFound: TFileFoundEvent read FOnFileFound write FOnFileFound;
    property OnDirectoryEnter: TDirectoryEnterEvent read FOnDirectoryEnter write FOnDirectoryEnter;
  end;

  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  private
    FList: TStrings;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(AList: TStrings);
  end;

  { TListDirectoriesSearcher }

  TListDirectoriesSearcher = class(TFileSearcher)
  private
    FDirectoriesList :TStrings;
  protected
    procedure DoDirectoryFound; override;
  public
    constructor Create(AList: TStrings);
  end;

function FindAllFiles(const SearchPath: String; SearchMask: String = '';
  SearchSubDirs: Boolean = True; DirAttr: Word = faDirectory): TStringList; overload;
procedure FindAllFiles(AList: TStrings; const SearchPath: String;
  SearchMask: String = ''; SearchSubDirs: Boolean = True; DirAttr: Word = faDirectory); overload;

function FindAllDirectories(const SearchPath: string;
  SearchSubDirs: Boolean = True): TStringList; overload;
procedure FindAllDirectories(AList: TStrings; const SearchPath: String;
  SearchSubDirs: Boolean = true); overload;

// flags for copy
type
  TCopyFileFlag = (
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    );
  TCopyFileFlags = set of TCopyFileFlag;

// Copy a file and a whole directory tree
function CopyFile(const SrcFilename, DestFilename: string;
                  Flags: TCopyFileFlags=[cffOverwriteFile]; ExceptionOnError: Boolean=False): boolean;
function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean; ExceptionOnError: Boolean=False): boolean;
function CopyDirTree(const SourceDir, TargetDir: string; Flags: TCopyFileFlags=[]): Boolean;

// filename parts
const
  PascalFileExt: array[1..3] of string = ('.pas','.pp','.p');
  PascalSourceExt: array[1..6] of string = ('.pas','.pp','.p','.lpr','.dpr','.dpk');

  AllDirectoryEntriesMask = '*';

implementation

uses
{$IFDEF windows}
  Windows;
{$ELSE}
  Unix;
{$ENDIF}

{$I fileutil.inc}
{$IFDEF windows}
  {$i winfileutil.inc}
{$ELSE}
  {$i unixfileutil.inc}
{$ENDIF}

end.

