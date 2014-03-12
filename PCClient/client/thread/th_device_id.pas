unit th_device_id;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basethread, baseform, unt_env;

type

  { TDeviceIdThread }

  TDeviceIdThread = class(TThreadBase)
  private
    FKill: Boolean;
    FDeviceId: string;
  protected
    procedure Execute; override;
    function MakeNotifyMap: TStringList; override;
  public
    constructor Create(ANotifyId: Integer; ABase: TFormBase; AKill: Boolean); reintroduce;

  end;

implementation

{ TDeviceIdThread }

procedure TDeviceIdThread.Execute;
begin
  FDeviceId := GetFirstDeviceId(FKill);
end;

function TDeviceIdThread.MakeNotifyMap: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('deviceid='+FDeviceId);
end;

constructor TDeviceIdThread.Create(ANotifyId: Integer; ABase: TFormBase; AKill: Boolean);
begin
  Inherited Create(ANotifyId, ABase);
  FKill:= AKill;
end;


end.

