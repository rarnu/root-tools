unit intf_notify;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  INotifyable = interface
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList);
  end;

implementation

end.

