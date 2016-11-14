{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$I platform.inc}

unit iniwebsession;

{$mode objfpc}{$H+}
{ $define cgidebug}
interface

uses
  Classes, SysUtils, fphttp, inifiles, httpdefs;
  
Type

  { TIniWebSession }

  TIniWebSession = Class(TCustomSession)
  Private
    FSessionStarted : Boolean;
    FCached: Boolean;
    FIniFile : TMemInifile;
    FSessionDir: String;
    FTerminated :Boolean;
    SID : String;
    procedure UpdateIniFile;
  Protected
    Function CreateIniFile(Const AFN : String) : TMemIniFile; virtual;
    Procedure FreeIniFile;
    Procedure CheckSession;
    Function GetSessionID : String; override;
    Function GetSessionVariable(VarName : String) : String; override;
    procedure SetSessionVariable(VarName : String; const AValue: String); override;
    Property Cached : Boolean Read FCached Write FCached;
    Property SessionDir : String Read FSessionDir Write FSessionDir;
    Property IniFile : TMemIniFile Read FIniFile Write FIniFile;
  Public
    Destructor Destroy; override;
    Procedure Terminate; override;
    Procedure UpdateResponse(AResponse : TResponse); override;
    Procedure InitSession(ARequest : TRequest; OnNewSession, OnExpired: TNotifyEvent); override;
    Procedure InitResponse(AResponse : TResponse); override;
    Procedure RemoveVariable(VariableName : String); override;
    Function GetSessionDir : String;
  end;
  TIniWebSessionClass = Class of TIniWebSession;

  { TIniSessionFactory }

  TIniSessionFactory = Class(TSessionFactory)
  private
    FCached: Boolean;
    FOldFileNameScheme: Boolean;
    FSessionDir: String;
    procedure SetCached(const AValue: Boolean);
    procedure SetSessionDir(const AValue: String);
  protected
    Procedure DeleteSessionFile(const AFileName : String);virtual;
    Function SessionExpired(Ini : TMemIniFile) : boolean;
    procedure CheckSessionDir; virtual;
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
    // Sweep session direcory and delete expired files.
    procedure DoCleanupSessions; override;
    Procedure DoDoneSession(Var ASession : TCustomSession); override;
    Function SessionFilePrefix : String; virtual; 
  Public
    // Directory where sessions are kept.
    Property SessionDir : String Read FSessionDir Write SetSessionDir;
    // Are ini files cached (written in 1 go before destroying)
    Property Cached : Boolean Read FCached Write SetCached;
    // If True, the '{' and '}' will not be stripped from the session filename.
    Property OldFileNameScheme : Boolean Read FOldFileNameScheme Write FOldFileNameScheme;
  end;

Var
  IniWebSessionClass : TIniWebSessionClass = Nil;

Const
  MaxIniCreate = 5;

implementation

{$ifdef cgidebug}
uses dbugintf;
{$endif}

Const
  // Sections in ini file
  SSession   = 'Session';
  SData      = 'Data';

  KeyStart   = 'Start';         // Start time of session
  KeyLast    = 'Last';          // Last seen time of session
  KeyTimeOut = 'Timeout';       // Timeout in seconds;

resourcestring
  SErrSessionTerminated = 'No web session active: Session was terminated';
  SErrNoSession         = 'No web session active: Session was not started';

{ TIniSessionFactory }

procedure TIniSessionFactory.SetCached(const AValue: Boolean);
begin
  if FCached=AValue then exit;
  FCached:=AValue;
end;

procedure TIniSessionFactory.SetSessionDir(const AValue: String);
begin
  if FSessionDir=AValue then exit;
  FSessionDir:=AValue;
end;

procedure TIniSessionFactory.DeleteSessionFile(const AFileName: String);
begin
  DeleteFile(AFileName); // TODO : silently ignoring errors ?
end;

function TIniSessionFactory.SessionExpired(Ini: TMemIniFile): boolean;

Var
  L : TDateTime;
  T : Integer;
begin
  L:=Ini.ReadDateTime(SSession,KeyLast,0);
  T:=Ini.ReadInteger(SSession,KeyTimeOut,DefaultTimeOutMinutes);
  {$ifdef cgidebug}
  If (L=0) then
    SendDebug('No datetime in inifile (or not valid datetime : '+Ini.ReadString(SSession,KeyLast,''))
  else
    SendDebug('Last    :'+FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz',L));
  SendDebug('Timeout :'+IntToStr(t));
  {$endif}
  Result:=((Now-L)>(T/(24*60)));
  {$ifdef cgidebug}
  if Result then
    begin
    SendDebug('Timeout :'+FloatToStr(T/(24*60)));
    SendDebug('Timeout :'+FormatDateTime('hh:nn:ss.zzz',(T/(24*60))));
    SendDebug('Diff    :'+FormatDateTime('hh:nn:ss.zzz',Now-L));
    SendDebug('Ini file session expired: '+ExtractFileName(Ini.FileName));
    end;
  {$endif}
end;

procedure TIniSessionFactory.CheckSessionDir;

Var
  TD : String;

begin
  If (FSessionDir='') then
    begin
    TD:=IncludeTrailingPathDelimiter(GetTempDir(True));
    // change session dir
    {$IFDEF ANDROID}
    FSessionDir := '/sdcard/crossserver/session/';
    {$ELSE}
    FSessionDir:=TD+'fpwebsessions'+PathDelim;
    {$ENDIF}
    if Not ForceDirectories(FSessionDir) then
      FSessionDir:=TD; // Assuming temp dir is writeable as fallback
    end;
end;


function TIniSessionFactory.DoCreateSession(ARequest: TRequest): TCustomSession;

Var
  S : TIniWebSession;
begin
  CheckSessionDir;
  if IniWebSessionClass=Nil then
    S:=TIniWebSession.Create(Nil)
  else
    S:=IniWebSessionClass.Create(Nil);
  S.SessionDir:=SessionDir;
  S.SessionCookie:=SessionCookie;
  S.SessionCookiePath:=SessionCookiePath;
  S.Cached:=Cached;
  Result:=S;
end;

procedure TIniSessionFactory.DoCleanupSessions;

Var
  Info : TSearchRec;
  Ini : TMemIniFile;
  FN : string;

begin
  CheckSessionDir;
  If FindFirst(SessionDir+AllFilesMask,0,info)=0 then
    try
      Repeat
        if (Info.Attr and faDirectory=0) then
          begin
          FN:=SessionDir+Info.Name;
          Ini:=TMeminiFile.Create(FN);
          try
            If SessionExpired(Ini) then
              DeleteSessionFile(FN);
          finally
            Ini.Free;
          end;
          end;
      Until FindNext(Info)<>0;
   finally
     FindClose(Info);
   end;
end;

procedure TIniSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  FreeAndNil(ASession);
end;

Function TIniSessionFactory.SessionFilePrefix : String; 

begin
  Result:='';
end;

{ TIniWebSession }

Function TIniWebSession.GetSessionDir : String;

begin
  Result:=SessionDir;
end;

function TIniWebSession.GetSessionID: String;
begin
  If (SID='') then
    SID:=inherited GetSessionID;
  Result:=SID;
end;

Procedure TIniWebSession.UpdateIniFile;


Var
  ACount : Integer;
  OK : Boolean;

begin
  ACount:=0;
  OK:=False;
  repeat
    Inc(ACount);
    try
      TMemIniFile(FIniFile).UpdateFile;
      OK:=True;
    except
      On E : EFCreateError do
        begin
        If ACount>MaxIniCreate then
          Raise;
        Sleep(20);
        end;
      On E : EFOpenError do
        begin
        If ACount>MaxIniCreate then
          Raise;
        Sleep(20);
        end;
      On E : Exception do
        Raise;
    end;
  Until OK;
end;

function TIniWebSession.CreateIniFile(Const AFN: String): TMemIniFile;

Var
  ACount : Integer;

begin
  ACount:=0;
  Result:=Nil;
  repeat
    Inc(ACount);
    try
      Result:=TMemIniFile.Create(AFN,False);
    except
      On E : EFCreateError do
        begin
        If ACount>MaxIniCreate then
          Raise;
        Sleep(20);
        end;
      On E : EFOpenError do
        begin
        If ACount>MaxIniCreate then
          Raise;
        Sleep(20);
        end;
      On E : Exception do
        Raise;
    end;
  until (Result<>Nil);
end;

procedure TIniWebSession.FreeIniFile;
begin
  If Cached and Assigned(FIniFile) then
    UpdateIniFile;
  FreeAndNil(FIniFile);
end;


Procedure TIniWebSession.CheckSession;

begin
  If Not Assigned(FInifile) then
    if FTerminated then
      Raise EWebSessionError.Create(SErrSessionTerminated)
    else
      Raise EWebSessionError.Create(SErrNoSession)
end;

function TIniWebSession.GetSessionVariable(VarName: String): String;
begin
  CheckSession;
  Result:=FIniFile.ReadString(SData,VarName,'');
end;

procedure TIniWebSession.SetSessionVariable(VarName: String;
  const AValue: String);
begin
  CheckSession;
  FIniFile.WriteString(SData,VarName,AValue);
  If Not Cached then
    UpdateIniFile;
end;

destructor TIniWebSession.Destroy;
begin
  // In case an exception occured and UpdateResponse is not called,
  // write the updates to disk and free FIniFile
  FreeIniFile;
  inherited Destroy;
end;

procedure TIniWebSession.Terminate;
begin
  FTerminated:=True;
  If Assigned(FIniFile) Then
    begin
    DeleteFile(Finifile.FileName);
    FreeAndNil(FIniFile);
    end;
end;

procedure TIniWebSession.UpdateResponse(AResponse: TResponse);
begin
  // Do nothing. Init has done the job.
  FreeIniFile;
end;

procedure TIniWebSession.InitSession(ARequest: TRequest; OnNewSession,OnExpired: TNotifyEvent);

Var
  S,FN : String;
  SF : TIniSessionFactory;
  
begin
  SF:=SessionFactory as TIniSessionFactory;
{$ifdef cgidebug}SendMethodEnter('TIniWebSession.InitSession');{$endif}
  // First initialize all session-dependent properties to their default, because
  // in Apache-modules or fcgi programs the session-instance is re-used
  SID := '';
  FSessionStarted := False;
  FTerminated := False;
  // If a exception occured during a prior request FIniFile is still not freed
  if assigned(FIniFile) then FreeIniFile;
  If (SessionCookie='') then
    SessionCookie:=SFPWebSession;
  S:=ARequest.CookieFields.Values[SessionCookie];
  // have session cookie ?
  If (S<>'') then
    begin
    FN:=IncludeTrailingPathDelimiter(SessionDir)+SF.SessionFilePrefix+S;
{$ifdef cgidebug}SendDebug('Existing session. Reading ini file:'+FN);{$endif}
    FIniFile:=CreateIniFile(FN);
    if SF.SessionExpired(FIniFile) then
      begin
      // Expire session.
      If Assigned(OnExpired) then
        OnExpired(Self);
      SF.DeleteSessionFile(FIniFIle.FileName);
      FreeAndNil(FInifile);
      S:='';
      end
    else
      SID:=S;
    end;
  If (S='') then
    begin
    If Assigned(OnNewSession) then
      OnNewSession(Self);
    GetSessionID;
    S:=IncludeTrailingPathDelimiter(SessionDir)+SF.SessionFilePrefix+SessionID;
{$ifdef cgidebug}SendDebug('Expired or new session. Creating new Ini file : '+S);{$endif}
    FIniFile:=CreateIniFile(S);
    FIniFile.WriteDateTime(SSession,KeyStart,Now);
    FIniFile.WriteInteger(SSession,KeyTimeOut,Self.TimeOutMinutes);
    FSessionStarted:=True;
    end;
  FIniFile.WriteDateTime(SSession,KeyLast,Now);
  If not FCached then
    UpdateIniFile;
{$ifdef cgidebug}SendMethodExit('TIniWebSession.InitSession');{$endif}
end;

procedure TIniWebSession.InitResponse(AResponse: TResponse);

Var
  C : TCookie;

begin
{$ifdef cgidebug}SendMethodEnter('TIniWebSession.InitResponse: '+SID);{$endif}
  C:=AResponse.Cookies.FindCookie(SessionCookie);
  If (C=Nil) then
    begin
    C:=AResponse.Cookies.Add;
    C.Name:=SessionCookie;
    end;
  If FTerminated then
    begin
{$ifdef cgidebug}SendDebug('Session terminated');{$endif}
    C.Value:='';
    end
  else
    begin
{$ifdef cgidebug}SendDebug('Existing session or Session started');{$endif}
    C.Value:=SID;
    C.Path:=SessionCookiePath;
    end;
{$ifdef cgidebug}SendMethodExit('TIniWebSession.InitResponse');{$endif}
end;

procedure TIniWebSession.RemoveVariable(VariableName: String);
begin
{$ifdef cgidebug}SendMethodEnter('TIniWebSession.RemoveVariable');{$endif}
  CheckSession;
  FIniFile.DeleteKey(SData,VariableName);
  If Not Cached then
    UpdateIniFile;
{$ifdef cgidebug}SendMethodExit('TIniWebSession.RemoveVariable');{$endif}
end;


initialization
  SessionFactoryClass:=TIniSessionFactory;
end.

