{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2009 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
unit fpfcgi;

interface

uses SysUtils,Classes,custfcgi;

Type

  { TFCGIApplication }

  TFCGIApplication = Class(TCustomFCGIApplication)
  end;

Var
  Application : TFCGIApplication;
  ShowCleanUpErrors : Boolean = False;
  
Implementation

uses CustApp;

Procedure InitFCGI;

begin
  Application:=TFCGIApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneFCGI;

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
  InitFCGI;
  
Finalization
  DoneFCGI;
  
end.
