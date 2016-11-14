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
unit websession deprecated;

{$mode objfpc}{$H+}
{ $define cgidebug}
interface

uses
  Classes, SysUtils, fphttp, iniwebsession, httpdefs;
  
Type
  TIniWebSession = iniwebsession.TIniWebSession;

  TFPWebSession = Class(iniwebsession.TIniWebSession)
  Public
    Property Cached;
    property SessionCookie;
    Property SessionCookiePath;
    Property SessionDir;
  end;

Type
  TGetSessionEvent = Procedure(Var ASession : TCustomSession) of object;

Var
  GlobalSessionDir : String deprecated;
  OnGetDefaultSession : TGetSessionEvent deprecated;

Function GetDefaultSession : TCustomSession;

implementation
type

  { TWebSessionFactory }

  TWebSessionFactory = Class(TIniSessionFactory)
  Protected
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
  end;


Function GetDefaultSession : TCustomSession;

Var
  TD : String;

begin
{$ifdef cgidebug}SendMethodEnter('GetDefaultSession');{$endif}
  Result:=Nil;
  If (GlobalSessionDir='') then
    begin
    TD:=IncludeTrailingPathDelimiter(GetTempDir(True));
    // change session dir
    {$IFDEF ANDROID}
    GlobalSessionDir := '/sdcard/crossserver/session/';
    {$ELSE}
    GlobalSessionDir:=TD+'fpwebsessions'+PathDelim;
    {$ENDIF}
    if Not ForceDirectories(GlobalSessionDir) then
      GlobalSessionDir:=TD; // Assuming temp dir is writeable
    end
  else
    GlobalSessionDir:=IncludeTrailingPathDelimiter(GlobalSessionDir);
{$ifdef cgidebug}SendDebug('GetDefaultSession, session dir: '+GlobalSessionDir);{$endif}
  If Assigned(OnGetDefaultSession) then
    OnGetDefaultSession(Result);
  if (Result=Nil) then
    begin
    {$ifdef cgidebug}Senddebug('Creating iniwebsession');{$endif}
    if (SessionFactory is TIniSessionFactory) then
      if ((SessionFactory as TIniSessionFactory).SessionDir='') then
        (SessionFactory as TIniSessionFactory).SessionDir:=GlobalSessionDir;
    Result:=SessionFactory.CreateSession(Nil);
    end;
{$ifdef cgidebug}SendMethodExit('GetDefaultSession');{$endif}
end;

{ TWebSessionFactory }

function TWebSessionFactory.DoCreateSession(ARequest: TRequest
  ): TCustomSession;
begin
  Result:=Nil;
  if Assigned(OnGetDefaultSession) then
    OnGetDefaultSession(Result);
  if Result=Nil then
  Result:=inherited DoCreateSession(ARequest);
end;


initialization
  IniWebSessionClass:=TFPWebSession;
  SessionFactoryClass:=TWebSessionFactory;
end.

