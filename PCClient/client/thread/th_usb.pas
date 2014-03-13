unit th_usb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basethread, unt_env;

type

  { TUsbThread }

  TUsbThread = class(TThreadBase)
  private
    FExecFlag: boolean;
    FDeviceId: string;
    FCurrentDeviceId: string;
  protected
    procedure Execute; override;
    function MakeNotifyMap: TStringList; override;
  public
    procedure SetCurrentDevice(DeviceId: string);
    procedure Start;
    procedure Stop;
  end;

implementation

{ TUsbThread }

procedure TUsbThread.Execute;
begin
  while FExecFlag do
  begin
    Sleep(3000);
    FDeviceId := GetFirstDeviceId(False);
    if FDeviceId <> FCurrentDeviceId then
    begin
      FCurrentDeviceId := FDeviceId;
      FNotify.ThreadNotify(FNotifyId, MakeNotifyMap);
    end;
  end;
end;

function TUsbThread.MakeNotifyMap: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('deviceid='+FDeviceId);
end;

procedure TUsbThread.SetCurrentDevice(DeviceId: string);
begin
  FCurrentDeviceId := DeviceId;
end;

procedure TUsbThread.Start;
begin
  FExecFlag := True;
  inherited Start;
end;

procedure TUsbThread.Stop;
begin
  FExecFlag := False;
end;

end.

