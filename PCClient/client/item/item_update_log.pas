unit item_update_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vg_scene, vg_controls, vg_objects, vg_listbox, Graphics;

type

  { TUpdateLogItem }

  TUpdateLogItem = class(TvgListBoxItem)
  private
    FLog: TvgText;
    function GetTitle: string;
    procedure SetTitle(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Title: string read GetTitle write SetTitle;
  end;

implementation

{ TUpdateLogItem }

function TUpdateLogItem.GetTitle: string;
begin
  Result := FLog.Text;
end;

procedure TUpdateLogItem.SetTitle(AValue: string);
begin
  FLog.Text:= AValue;
end;

constructor TUpdateLogItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 48;
  FLog := TvgText.Create(Self);
  FLog.Parent := Self;
  FLog.Align := vaClient;
  FLog.HorzTextAlign := vgTextAlignNear;
  FLog.VertTextAlign := vgTextAlignCenter;
  FLog.Padding.Left := 8;
  FLog.HitTest := False;
  FLog.Fill.SolidColor:= vgColorFromVCL(clWhite);
end;

destructor TUpdateLogItem.Destroy;
begin
  inherited Destroy;
end;

end.

