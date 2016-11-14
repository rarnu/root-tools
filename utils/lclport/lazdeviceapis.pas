{
  Lazarus Component Library
  
  This is a unit for holding all hardware APIs which do not involve the screen
  nor input events. These are things like accelerometer, SMS, GPS, etc, which
  are typical of mobile devices such as smartphones

  Author: Felipe Monteiro de Carvalho 2011

  License: The same modified LGPL as the rest of the LCL
}
unit LazDeviceApis;

{$mode delphi}

interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType;

type

  // TLazAccelerometer

  { The accelerometer geometry is the following:

    Consider the device with the screen facing the user
    Typically it will have some buttons in the bottom part (represented by []):

       ^ Y axis
       |
    ________
    |      |
    |      | --> X axis
    |      |
    |[][][]|

    The Z axis goes from the device screen in the direction of the user facing it.
  }

  TLazAccelerometer = class
  private
    FOnSensorChanged: TNotifyEvent;
    FReadingStarted: Boolean;
  public
    // These fields store the last data read
    xaxis, yaxis, zaxis: Double; // in m/s^2
    procedure StartReadingAccelerometerData();
    procedure StopReadingAccelerometerData();
    property OnSensorChanged: TNotifyEvent read FOnSensorChanged write FOnSensorChanged;
  end;

  // TLazMessaging

  TLazDeviceMessageKind = (dmkSMS, dmkMMS, dmkEMail);

  TLazDeviceMessage = class
  public
    // The coments indicate in which message kind each
    // field is available.             SMS   MMS  EMail
    bccAddress: TStringList;         // N     N    Y
    Body: string;                    // Y     Y	   Y
    ccAddress: TstringList;          // N     N    Y
    destinationAddress: TStringList; // Y     Y    Y
    isRead: Boolean;                 // Y     Y    Y
    messageId: string;               // Y     Y    Y
    //messagePriority	Y	Y	Y
    messageType: TLazDeviceMessageKind;//Y    Y    Y
    ReplyToAddress: string;          // Y     Y    Y
    sourceAddress: string;           // Y     Y    Y
    Subject: string;                 // N     Y    Y
    Time: TDateTime;                 // Y     Y    Y
    validityPeriod:TTime;            // Y     N    N
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TLazMessagingStatus = (
    // Message sending status
    mssSentSuccessfully, mssSendingGeneralError, mssRadioOff, mssNoService,
    // Message receiving status (by the destination)
    mssReceivedSuccessfully, mssReceivingGeneralError
    );

  TOnMessagingStatus = procedure (AMessage: TLazDeviceMessage;
    AStatus: TLazMessagingStatus) of object;

  TLazMessaging = class
  private
    FOnMessagingStatus: TOnMessagingStatus;
    FMessages: TFPList; // of TLazDeviceMessage
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Attempt to send the specified message.
    procedure SendMessage(AMsg: TLazDeviceMessage);
    function CreateMessage: TLazDeviceMessage;
    procedure FreeMessage(AMessage: TLazDeviceMessage);
    // Called asynchronously when there is a message status
    property OnMessagingStatus: TOnMessagingStatus
      read FOnMessagingStatus write FOnMessagingStatus;
  end;

  // TLazPositionInfo

  TLazPositionMethod = (pmGPS, pmNetwork);

  TLazPositionInfo = class
  private
    FOnPositionRetrieved: TNotifyEvent;
  public
    IsPositionDataAvailable: Boolean; // Indicates if position info was read in the life of this program
    // These fields hold the last position information read
    accuracy: Double; // The horizontal accuracy of the position in meters
    altitude: Double; // Altitude in meters in relation to the sea level using the World Geodetic System 1984 (WGS84) datum.
    altitudeAccuracy: Double; // The vertical accuracy of the position in meters, or zero if not available.
    latitude: Double; // Latitude in degrees using the World Geodetic System 1984 (WGS84) datum.
    longitude: Double; // Longitude in degrees using the World Geodetic System 1984 (WGS84) datum.
    speed: Double; // In meters / second
    timeStamp: TDateTime; // The time when the latest location was established.
    procedure RequestPositionInfo(AMethod: TLazPositionMethod);
    // Called asynchronously when the position is read
    property OnPositionRetrieved: TNotifyEvent read FOnPositionRetrieved write FOnPositionRetrieved;
  end;

  // TLazDevice

  TScreenRotation = (srRotation_0, srRotation_90, srRotation_180, srRotation_270);

  TLazDevice = class
  private
    function GetDeviceManufacturer: string;
    function GetDeviceModel: string;
  public
    procedure Vibrate(ADurationMS: Cardinal);
    function GetScreenRotation(AScreenIndex: Integer): TScreenRotation;
    property Manufacturer: string read GetDeviceManufacturer;
    property Model: string read GetDeviceModel;
  end;

var
  Accelerometer: TLazAccelerometer;
  Messaging: TLazMessaging;
  PositionInfo: TLazPositionInfo;
  Device: TLazDevice;

implementation

uses wslazdeviceapis, wslclclasses;

{ TLazDevice }

function TLazDevice.GetDeviceManufacturer: string;
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  Result := WidgetsetClass.GetDeviceManufacturer();
end;

function TLazDevice.GetDeviceModel: string;
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  Result := WidgetsetClass.GetDeviceModel();
end;

procedure TLazDevice.Vibrate(ADurationMS: Cardinal);
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  WidgetsetClass.Vibrate(ADurationMS);
end;

function TLazDevice.GetScreenRotation(AScreenIndex: Integer): TScreenRotation;
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  Result := WidgetsetClass.GetScreenRotation(AScreenIndex);
end;

{ TLazAccelerometer }

procedure TLazAccelerometer.StartReadingAccelerometerData;
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  if FReadingStarted then Exit;
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  WidgetsetClass.StartReadingAccelerometerData();
  FReadingStarted := True;
end;

procedure TLazAccelerometer.StopReadingAccelerometerData;
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  if not FReadingStarted then Exit;
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  WidgetsetClass.StopReadingAccelerometerData();
  FReadingStarted := False;
end;

{ TLazPositionInfo }

procedure TLazPositionInfo.RequestPositionInfo(AMethod: TLazPositionMethod);
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  WidgetsetClass.RequestPositionInfo(AMethod);
end;

{ TLazDeviceMessage }

constructor TLazDeviceMessage.Create;
begin
  inherited Create;
  bccAddress := TStringList.Create;
  ccAddress := TStringList.Create;
  destinationAddress := TStringList.Create;
end;

destructor TLazDeviceMessage.Destroy;
begin
  bccAddress.Free;
  ccAddress.Free;
  destinationAddress.Free;
  inherited Destroy;
end;

{ TLazMessaging }

constructor TLazMessaging.Create;
begin
  FMessages := TFPList.Create;
end;

destructor TLazMessaging.Destroy;
var
  i: Integer;
begin
  // Free all messages
  for i := 0 to FMessages.Count-1 do
    TLazDeviceMessage(FMessages.Items[i]).Free;
  FMessages.Free;
  inherited Destroy;
end;

procedure TLazMessaging.SendMessage(AMsg: TLazDeviceMessage);
var
  WidgetsetClass: TWSLazDeviceAPIsClass;
begin
  WidgetsetClass := TWSLazDeviceAPIsClass(GetWSLazDeviceAPIs());
  WidgetsetClass.SendMessage(AMsg);
end;

function TLazMessaging.CreateMessage: TLazDeviceMessage;
begin
  Result := TLazDeviceMessage.Create;
  FMessages.Add(Result);
end;

procedure TLazMessaging.FreeMessage(AMessage: TLazDeviceMessage);
begin
  FMessages.Remove(AMessage);
  AMessage.Free;
end;

initialization
  RegisterLazDeviceAPIs();
  Accelerometer := TLazAccelerometer.Create;
  Messaging := TLazMessaging.Create;
  PositionInfo := TLazPositionInfo.Create;
  Device := TLazDevice.Create;
finalization
  Accelerometer.Free;
  Messaging.Free;
  PositionInfo.Free;
  Device.Free;
end.

