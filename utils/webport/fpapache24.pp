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
unit fpapache24;

interface
uses
  sysutils, custapp, custapache24;

Type
  // Backwards compatibility defines.
  TApacheHandler = custapache24.TApacheHandler;
  TApacheRequest = custapache24.TApacheRequest;
  TApacheResponse = custapache24.TApacheResponse;
  THandlerPriority = custapache24.THandlerPriority;
  TBeforeRequestEvent = custapache24.TBeforeRequestEvent;
  TCustomApacheApplication = custapache24.TCustomApacheApplication;


  TApacheApplication = Class(TCustomApacheApplication)
  Public
    Property HandlerPriority;
    Property BeforeModules;
    Property AfterModules;
    Property AllowDefaultModule;
    Property OnGetModule;
    Property BaseLocation;
    Property ModuleName;
    Property MaxRequests;
    Property IdleWebModuleCount;
    Property WorkingWebModuleCount;
  end;


Implementation

Procedure InitApache;

begin
  Application:=TApacheApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneApache;

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
  InitApache;
  
Finalization
  DoneApache;
  
end.
