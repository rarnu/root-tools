{
 /***************************************************************************
                               UTF8Process.pp
                               ---------------
                   Initial Revision  : Tue Dec 06 09:00:00 CET 2005


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit UTF8Process;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION<20701}
  {$DEFINE UseOldTProcess}
{$ENDIF}

{$IFNDEF UseOldTProcess}
  {$IFDEF MSWINDOWS}
    {$DEFINE UseTProcessW}
  {$ELSE}
    {$DEFINE UseTProcessAlias}
  {$ENDIF}
{$ENDIF}


interface

uses
  Classes, SysUtils, Process,
  {$IF defined(UseSeparateTProcessW) or defined(UseTProcessW)}
  pipes,
  {$ENDIF}
  FileUtil, LazFileUtils, LazUTF8, LazUtilsStrConsts;

  {$IFDEF UseOldTProcess}
type
  { TProcessUTF8 }

  TProcessUTF8 = class(TProcess)
  private
    FApplicationNameUTF8: string;
    FCommandLineUTF8: string;
    FConsoleTitleUTF8: string;
    FCurrentDirectoryUTF8: string;
    FDesktopUTF8: string;
    FEnvironmentUTF8: TStrings;
    FExecutableUTF8: string;
    FParametersUTF8: TStrings;
    procedure SetApplicationNameUTF8(const AValue: string);
    procedure SetCommandLineUTF8(const AValue: string);
    procedure SetConsoleTitleUTF8(const AValue: string);
    procedure SetCurrentDirectoryUTF8(const AValue: string);
    procedure SetDesktopUTF8(const AValue: string);
    procedure SetEnvironmentUTF8(const AValue: TStrings);
    procedure SetExecutableUTF8(AValue: string);
    procedure SetParametersUTF8(AValue: TStrings);
    procedure UpdateEnvironment;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property ApplicationName: string read FApplicationNameUTF8 write SetApplicationNameUTF8;
    property CommandLine: string read FCommandLineUTF8 write SetCommandLineUTF8;
    property ConsoleTitle: string read FConsoleTitleUTF8 write SetConsoleTitleUTF8;
    property CurrentDirectory: string read FCurrentDirectoryUTF8 write SetCurrentDirectoryUTF8;
    property Desktop: string read FDesktopUTF8 write SetDesktopUTF8;
    property Environment: TStrings read FEnvironmentUTF8 write SetEnvironmentUTF8;
    property Executable: string read FExecutableUTF8 Write SetExecutableUTF8;
    property Parameters: TStrings read FParametersUTF8 write SetParametersUTF8;
  end;
  {$ENDIF}

  {$IFDEF UseTProcessW}
const
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrCannotExecute     = 'Failed to execute %s : %d';
type
  { TProcessUTF8 }

  TProcessUTF8 = class(TProcess)
  protected
    procedure SetProcessHandle(aProcessHandle : THandle);
    procedure SetThreadHandle(aThreadHandle : THandle);
    procedure SetProcessID(aProcessID : Integer);
  public
    procedure Execute; override;
  end;
  {$ENDIF}

  {$IFDEF UseTProcessAlias}
type
  TProcessUTF8 = class(TProcess)
  end;
  {$ENDIF}

procedure RunCmdFromPath(ProgramFilename, CmdLineParameters: string);
function FindFilenameOfCmd(ProgramFilename: string): string;

function GetSystemThreadCount: integer; // guess number of cores

procedure Register;

implementation

{$IF defined(windows)}
uses Windows;
{$ELSEIF defined(freebsd) or defined(darwin)}
uses ctypes, sysctl;
{$ELSEIF defined(linux)}
{$linklib c}
uses ctypes;
{$ENDIF}

{$IFDEF Linux}
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}

function GetSystemThreadCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  SystemInfo: SYSTEM_INFO;
  {$IFnDEF WinCE}
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  {$ENDIF}
begin
  {$IFnDEF WinCE}
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask{%H-}, SystemAffinityMask{%H-})
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
    exit;
  end;
  {$ENDIF}
  //can't get the affinity mask so we just report the total number of processors
  GetSystemInfo(SystemInfo{%H-});
  Result := SystemInfo.dwNumberOfProcessors;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
  end;

{$ELSE}
  begin
    Result:=1;
  end;
{$ENDIF}

function FindFilenameOfCmd(ProgramFilename: string): string;
begin
  Result:=TrimFilename(ProgramFilename);
  if not FilenameIsAbsolute(Result) then begin
    if Pos(PathDelim,Result)>0 then begin
      // with sub directory => relative to current directory
      Result:=CleanAndExpandFilename(Result);
    end else begin
      // search in PATH
      Result:=FindDefaultExecutablePath(Result);
    end;
  end;
  if (Result<>'') and not FileExistsUTF8(Result) then
    Result:='';
end;

// Runs a short command which should point to an executable in
// the environment PATH
// For example: ProgramFilename=ls CmdLineParameters=-l /home
// Will locate and execute the file /bin/ls
// If the command isn't found, an exception will be raised
procedure RunCmdFromPath(ProgramFilename, CmdLineParameters: string);
var
  OldProgramFilename: String;
  BrowserProcess: TProcessUTF8;
begin
  OldProgramFilename:=ProgramFilename;
  ProgramFilename:=FindFilenameOfCmd(ProgramFilename);

  if ProgramFilename='' then
    raise EFOpenError.Create(Format(lrsProgramFileNotFound, [OldProgramFilename]));
  if not FileIsExecutable(ProgramFilename) then
    raise EFOpenError.Create(Format(lrsCanNotExecute, [ProgramFilename]));

  // run
  BrowserProcess := TProcessUTF8.Create(nil);
  try
    BrowserProcess.InheritHandles:=false;
    // Encloses the executable with "" if its name has spaces
    if Pos(' ',ProgramFilename)>0 then
      ProgramFilename:='"'+ProgramFilename+'"';

    {$Push}
    {$WARN SYMBOL_DEPRECATED OFF}
    BrowserProcess.CommandLine := ProgramFilename;
    if CmdLineParameters<>'' then
      BrowserProcess.CommandLine := BrowserProcess.CommandLine + ' ' + CmdLineParameters;
    {$Pop}
    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('System',[TProcessUTF8]);
end;

{$IFDEF UseOldTProcess}
{$WARN SYMBOL_DEPRECATED OFF}
{ TProcessUTF8 }

procedure TProcessUTF8.SetApplicationNameUTF8(const AValue: string);
begin
  if FApplicationNameUTF8=AValue then exit;
  FApplicationNameUTF8:=AValue;
  inherited ApplicationName:=UTF8ToSys(FApplicationNameUTF8);
end;

procedure TProcessUTF8.SetCommandLineUTF8(const AValue: string);
var
  Src: TStrings;
  i: Integer;
begin
  if FCommandLineUTF8=AValue then exit;
  FCommandLineUTF8:=AValue;
  inherited CommandLine:=UTF8ToSys(FCommandLineUTF8);
  FExecutableUTF8:=SysToUTF8(inherited Executable);
  FParametersUTF8.Clear;
  Src:=inherited Parameters;
  if Src<>nil then
    for i:=0 to Src.Count-1 do
      FParametersUTF8.Add(SysToUTF8(Src[i]));
end;

procedure TProcessUTF8.SetConsoleTitleUTF8(const AValue: string);
begin
  if FConsoleTitleUTF8=AValue then exit;
  FConsoleTitleUTF8:=AValue;
  inherited ConsoleTitle:=UTF8ToSys(FConsoleTitleUTF8);
end;

procedure TProcessUTF8.SetCurrentDirectoryUTF8(const AValue: string);
begin
  if FCurrentDirectoryUTF8=AValue then exit;
  FCurrentDirectoryUTF8:=AValue;
  inherited CurrentDirectory:=UTF8ToSys(FCurrentDirectoryUTF8);
end;

procedure TProcessUTF8.SetDesktopUTF8(const AValue: string);
begin
  if FDesktopUTF8=AValue then exit;
  FDesktopUTF8:=AValue;
  inherited Desktop:=UTF8ToSys(FDesktopUTF8);
end;

procedure TProcessUTF8.SetEnvironmentUTF8(const AValue: TStrings);
begin
  if (FEnvironmentUTF8=AValue)
  or ((AValue<>nil) and FEnvironmentUTF8.Equals(AValue)) then exit;
  FEnvironmentUTF8.Assign(AValue);
end;

procedure TProcessUTF8.SetExecutableUTF8(AValue: string);
begin
  if FExecutableUTF8=AValue then Exit;
  FExecutableUTF8:=AValue;
  inherited Executable:=UTF8ToSys(FExecutableUTF8);
end;

procedure TProcessUTF8.SetParametersUTF8(AValue: TStrings);
begin
  if (FParametersUTF8=AValue)
  or ((AValue<>nil) and FParametersUTF8.Equals(AValue)) then exit;
  FParametersUTF8.Assign(AValue);
end;

procedure TProcessUTF8.UpdateEnvironment;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to FEnvironmentUTF8.Count-1 do
      sl.Add(UTF8ToSys(FEnvironmentUTF8[i]));
    inherited Environment:=sl;
    sl.Clear;
    for i:=0 to FParametersUTF8.Count-1 do
      sl.Add(UTF8ToSys(FParametersUTF8[i]));
    inherited Parameters:=sl;
  finally
    sl.Free;
  end;
end;

constructor TProcessUTF8.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnvironmentUTF8:=TStringList.Create;
  FParametersUTF8:=TStringList.Create;
end;

destructor TProcessUTF8.Destroy;
begin
  FreeAndNil(FEnvironmentUTF8);
  FreeAndNil(FParametersUTF8);
  inherited Destroy;
end;

procedure TProcessUTF8.Execute;
begin
  UpdateEnvironment;
  inherited Execute;
end;

{$ENDIF}

{$IFDEF UseTProcessW}
Const
  PriorityConstants : Array [TProcessPriority] of Cardinal =
                      (HIGH_PRIORITY_CLASS,IDLE_PRIORITY_CLASS,
                       NORMAL_PRIORITY_CLASS,REALTIME_PRIORITY_CLASS);

function WStrAsUniquePWideChar(var s: UnicodeString): PWideChar; inline;
begin
  UniqueString(s);
  if s<>'' then
    Result:=PWideChar(s)
  else
    Result:=nil;
end;

Function GetStartupFlags (P : TProcessUTF8): Cardinal;

begin
  Result:=0;
  if poUsePipes in P.Options then
     Result:=Result or Startf_UseStdHandles;
  if suoUseShowWindow in P.StartupOptions then
    Result:=Result or startf_USESHOWWINDOW;
  if suoUSESIZE in P.StartupOptions then
    Result:=Result or startf_usesize;
  if suoUsePosition in P.StartupOptions then
    Result:=Result or startf_USEPOSITION;
  if suoUSECOUNTCHARS in P.Startupoptions then
    Result:=Result or startf_usecountchars;
  if suoUsefIllAttribute in P.StartupOptions then
    Result:=Result or startf_USEFILLATTRIBUTE;
end;

Function GetCreationFlags(P : TProcessUTF8) : Cardinal;

begin
  Result:=CREATE_UNICODE_ENVIRONMENT;
  if poNoConsole in P.Options then
    Result:=Result or Detached_Process;
  if poNewConsole in P.Options then
    Result:=Result or Create_new_console;
  if poNewProcessGroup in P.Options then
    Result:=Result or CREATE_NEW_PROCESS_GROUP;
  If poRunSuspended in P.Options Then
    Result:=Result or Create_Suspended;
  if poDebugProcess in P.Options Then
    Result:=Result or DEBUG_PROCESS;
  if poDebugOnlyThisProcess in P.Options Then
    Result:=Result or DEBUG_ONLY_THIS_PROCESS;
  if poDefaultErrorMode in P.Options Then
    Result:=Result or CREATE_DEFAULT_ERROR_MODE;
  result:=result or PriorityConstants[P.Priority];
end;

Function MaybeQuote(Const S : String) : String;

begin
  If (Pos(' ',S)<>0) then
    Result:='"'+S+'"'
  else
     Result:=S;
end;

Function MaybeQuoteIfNotQuoted(Const S : String) : String;

begin
  If (Pos(' ',S)<>0) and (pos('"',S)=0) then
    Result:='"'+S+'"'
  else
     Result:=S;
end;

Function StringsToWChars(List : TStrings): pointer;

var
  EnvBlock: UnicodeString;
  I: Integer;

begin
  EnvBlock := '';
  For I:=0 to List.Count-1 do
    EnvBlock := EnvBlock + UTF8Decode(List[i]) + #0;
  EnvBlock := EnvBlock + #0;
  GetMem(Result, Length(EnvBlock)*2);
  CopyMemory(Result, @EnvBlock[1], Length(EnvBlock)*2);
end;

Procedure InitProcessAttributes(Out PA : TSecurityAttributes);

begin
  FillChar(PA{%H-},SizeOf(PA),0);
  PA.nLength := SizeOf(PA);
end;

Procedure InitThreadAttributes(Out TA : TSecurityAttributes);

begin
  FillChar(TA{%H-},SizeOf(TA),0);
  TA.nLength := SizeOf(TA);
end;

Procedure InitStartupInfo(P : TProcessUTF8; Out SI : STARTUPINFOW);

Const
  SWC : Array [TShowWindowOptions] of Cardinal =
             (0,SW_HIDE,SW_Maximize,SW_Minimize,SW_Restore,SW_Show,
             SW_ShowDefault,SW_ShowMaximized,SW_ShowMinimized,
               SW_showMinNOActive,SW_ShowNA,SW_ShowNoActivate,SW_ShowNormal);

begin
  FillChar(SI{%H-},SizeOf(SI),0);
  SI.dwFlags:=GetStartupFlags(P);
  if P.ShowWindow<>swoNone then
   SI.dwFlags:=SI.dwFlags or Startf_UseShowWindow
  else
    SI.dwFlags:=SI.dwFlags and not Startf_UseShowWindow;
  SI.wShowWindow:=SWC[P.ShowWindow];
  if (poUsePipes in P.Options) then
    begin
    SI.dwFlags:=SI.dwFlags or Startf_UseStdHandles;
    end;
  if P.FillAttribute<>0 then
    begin
    SI.dwFlags:=SI.dwFlags or Startf_UseFillAttribute;
    SI.dwFillAttribute:=P.FillAttribute;
    end;
   SI.dwXCountChars:=P.WindowColumns;
   SI.dwYCountChars:=P.WindowRows;
   SI.dwYsize:=P.WindowHeight;
   SI.dwXsize:=P.WindowWidth;
   SI.dwy:=P.WindowTop;
   SI.dwX:=P.WindowLeft;
end;

{ The handles that are to be passed to the child process must be
  inheritable. On the other hand, only non-inheritable handles
  allow the sending of EOF when the write-end is closed. This
  function is used to duplicate the child process's ends of the
  handles into inheritable ones, leaving the parent-side handles
  non-inheritable.
}
function DuplicateHandleFP(var handle: THandle): Boolean;

var
  oldHandle: THandle;
begin
  oldHandle := handle;
  Result := DuplicateHandle
  ( GetCurrentProcess(),
    oldHandle,
    GetCurrentProcess(),
    @handle,
    0,
    true,
    DUPLICATE_SAME_ACCESS
  );
  if Result then
    Result := CloseHandle(oldHandle);
end;


Procedure CreatePipes(Var HI,HO,HE : Thandle; Var SI : TStartupInfoW; CE : Boolean; APipeBufferSize : Cardinal);

begin
  CreatePipeHandles(SI.hStdInput,HI, APipeBufferSize);
  DuplicateHandleFP(SI.hStdInput);
  CreatePipeHandles(HO,Si.hStdOutput, APipeBufferSize);
  DuplicateHandleFP(   Si.hStdOutput);
  if CE then begin
    CreatePipeHandles(HE,SI.hStdError, APipeBufferSize);
    DuplicateHandleFP(   SI.hStdError);
    end
  else
    begin
    SI.hStdError:=SI.hStdOutput;
    HE:=HO;
    end;
end;

type
  TProcessClassTemplate = class(TComponent)
  private
    {%H-}FProcessOptions : TProcessOptions;
    {%H-}FStartupOptions : TStartupOptions;
    FProcessID : Integer;
    {%H-}FTerminalProgram: String;
    {%H-}FThreadID : Integer;
    FProcessHandle : Thandle;
    FThreadHandle : Thandle;
  end;

{ TProcessUTF8 }

procedure TProcessUTF8.SetProcessHandle(aProcessHandle: THandle);
var
  o: TProcessClassTemplate;
begin
  o:=TProcessClassTemplate.Create(nil);
  PHANDLE(Pointer(Self)+(@o.FProcessHandle-Pointer(o)))^:=aProcessHandle;
  if aProcessHandle<>ProcessHandle then
    raise Exception.Create('TProcessUTF8.SetProcessHandle failed');
  o.Free;
end;

procedure TProcessUTF8.SetThreadHandle(aThreadHandle: THandle);
var
  o: TProcessClassTemplate;
begin
  o:=TProcessClassTemplate.Create(nil);
  PHANDLE(Pointer(Self)+(@o.FThreadHandle-Pointer(o)))^:=aThreadHandle;
  if aThreadHandle<>ThreadHandle then
    raise Exception.Create('TProcessUTF8.SetThreadHandle failed');
  o.Free;
end;

procedure TProcessUTF8.SetProcessID(aProcessID: Integer);
var
  o: TProcessClassTemplate;
begin
  o:=TProcessClassTemplate.Create(nil);
  PHANDLE(Pointer(Self)+(@o.FProcessID-Pointer(o)))^:=aProcessID;
  if aProcessID<>ProcessID then
    raise Exception.Create('TProcessUTF8.SetProcessID failed');
  o.Free;
end;

procedure TProcessUTF8.Execute;
Var
  i : Integer;
  WName,WDir,WCommandLine : UnicodeString;
  PWName,PWDir,PWCommandLine : PWideChar;
  FEnv: pointer;
  FCreationFlags : Cardinal;
  FProcessAttributes : TSecurityAttributes;
  FThreadAttributes : TSecurityAttributes;
  FProcessInformation : TProcessInformation;
  FStartupInfo : STARTUPINFOW;
  HI,HO,HE : THandle;
  Cmd : String;

begin
  WName:='';
  WCommandLine:='';
  WDir:='';

  if (ApplicationName{%H-}='') and (CommandLine{%H-}='') and (Executable='') then
    Raise EProcess.Create(SNoCommandline);
  if (ApplicationName{%H-}<>'') then
    begin
    WName:=UTF8Decode(ApplicationName{%H-});
    WCommandLine:=UTF8Decode(CommandLine{%H-});
    end
  else If (CommandLine{%H-}<>'') then
    WCommandLine:=UTF8Decode(CommandLine{%H-})
  else if (Executable<>'') then
    begin
    Cmd:=MaybeQuoteIfNotQuoted(Executable);
    For I:=0 to Parameters.Count-1 do
      Cmd:=Cmd+' '+MaybeQuoteIfNotQuoted(Parameters[i]);
    WCommandLine:=UTF8Decode(Cmd);
    end;
  If CurrentDirectory<>'' then
    WDir:=UTF8Decode(CurrentDirectory);
  if Environment.Count<>0 then
    FEnv:=StringsToWChars(Environment)
  else
    FEnv:=Nil;
  Try
    FCreationFlags:=GetCreationFlags(Self);
    InitProcessAttributes(FProcessAttributes);
    InitThreadAttributes(FThreadAttributes);
    InitStartupInfo(Self,FStartupInfo);
    If poUsePipes in Options then
      CreatePipes(HI{%H-},HO{%H-},HE{%H-},FStartupInfo,Not(poStdErrToOutPut in Options), PipeBufferSize);
    Try
      // Beware: CreateProcess can alter the strings
      // Beware: nil is not the same as a pointer to a #0
      PWName:=WStrAsUniquePWideChar(WName);
      PWCommandLine:=WStrAsUniquePWideChar(WCommandLine);
      PWDir:=WStrAsUniquePWideChar(WDir);

      If Not CreateProcessW (PWName,PWCommandLine,@FProcessAttributes,@FThreadAttributes,
                   InheritHandles,FCreationFlags,FEnv,PWDir,FStartupInfo,
                   fProcessInformation{%H-}) then
        Raise EProcess.CreateFmt(SErrCannotExecute,[CommandLine{%H-},GetLastError]);
      SetProcessHandle(FProcessInformation.hProcess);
      SetThreadHandle(FProcessInformation.hThread);
      SetProcessID(FProcessINformation.dwProcessID);
    Finally
      if POUsePipes in Options then
        begin
        FileClose(FStartupInfo.hStdInput);
        FileClose(FStartupInfo.hStdOutput);
        if Not (poStdErrToOutPut in Options) then
          FileClose(FStartupInfo.hStdError);
        CreateStreams(HI,HO,HE);
        end;
    end;
    FRunning:=True;
  Finally
    If FEnv<>Nil then
      FreeMem(FEnv);
  end;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in Options) and
      not (poRunSuspended in Options) then
    WaitOnExit;
end;
{$ENDIF}

end.
