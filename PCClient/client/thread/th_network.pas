unit th_network;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basenetwork;

type

  { TNetworkthread }

  TNetworkthread = class(TNetworkBase)
  private
  protected
    function MakeNotifyMap: TStringList; override;
  public
  end;

implementation



{ TNetworkthread }

function TNetworkthread.MakeNotifyMap: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('update=' + FHttpResult);
end;

end.
