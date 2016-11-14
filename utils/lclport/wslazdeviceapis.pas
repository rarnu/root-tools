{
 *****************************************************************************
 *                             WSLazDeviceAPIS.pas                           *
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSLazDeviceAPIS;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Types, Math, LazDeviceAPIs,
////////////////////////////////////////////////////
  WSLCLClasses, WSFactory;

type
  { TWSLazDeviceAPIS }
  
  TWSLazDeviceAPIsClass = class of TWSLazDeviceAPIs;
  TWSLazDeviceAPIs = class(TWSObject)
  public
    class procedure RequestPositionInfo(AMethod: TLazPositionMethod); virtual;
    //
    class procedure SendMessage(AMsg: TLazDeviceMessage); virtual;
    //
    class procedure StartReadingAccelerometerData(); virtual;
    class procedure StopReadingAccelerometerData(); virtual;
    // TLazDevice
    class function GetDeviceManufacturer: string; virtual;
    class function GetDeviceModel: string; virtual;
    class function GetScreenRotation(AScreenIndex: Integer): TScreenRotation; virtual;
    class procedure Vibrate(ADurationMS: Cardinal); virtual;
  end;

{ WidgetSetRegistration }
procedure RegisterLazDeviceAPIs;

implementation

{ WidgetSetRegistration }

procedure RegisterLazDeviceAPIs;
const
  Done: Boolean = False;
begin
  if Done then exit;
  RegisterWSLazDeviceAPIs(TWSLazDeviceAPIs);
  Done := True;
end;

{ TWSLazDeviceAPIs }

class procedure TWSLazDeviceAPIs.RequestPositionInfo(AMethod: TLazPositionMethod);
begin

end;

class procedure TWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
begin

end;

class procedure TWSLazDeviceAPIs.StartReadingAccelerometerData;
begin

end;

class procedure TWSLazDeviceAPIs.StopReadingAccelerometerData;
begin

end;

class function TWSLazDeviceAPIs.GetDeviceManufacturer: string;
begin
  Result := '';
end;

class function TWSLazDeviceAPIs.GetDeviceModel: string;
begin
  Result := '';
end;

class function TWSLazDeviceAPIs.GetScreenRotation(AScreenIndex: Integer
  ): TScreenRotation;
begin
  Result := srRotation_0;
end;

class procedure TWSLazDeviceAPIs.Vibrate(ADurationMS: Cardinal);
begin

end;

end.
