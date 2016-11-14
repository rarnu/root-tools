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
{$mode objfpc}
{$H+}
unit fpcgi;

interface

uses SysUtils,Classes,CustCgi;

Type

  { TCGIApplication }

  TCGIApplication = Class(TCustomCGIApplication)
  end;

Var
  Application : TCGIApplication;
  ShowCleanUpErrors : Boolean = False;
  
Implementation

uses CustApp;

Procedure InitCGI;

begin
  Application:=TCGIApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneCGI;

begin
  Try
    if CustomApplication=Application then
      CustomApplication := nil;
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

Initialization
  InitCGI;
  
Finalization
  DoneCGI;
  
end.
