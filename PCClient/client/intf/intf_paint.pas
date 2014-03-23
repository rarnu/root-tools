unit intf_paint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOnPaintOnce = procedure(Sender: TObject; RealWidth: integer;
    RealHeight: integer) of object;

implementation

end.
