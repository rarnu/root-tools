unit item_device_build_prop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, vg_scene, vg_controls, vg_objects, vg_listbox, vg_textbox;

type

  { TBuildPropItem }

  TBuildPropItem = class(TvgListBoxItem)
  private
    FBuildName: TvgText;
    FBuildValue: TvgHudTextBox;
    function GetPropName: string;
    function GetPropValue: string;
    procedure SetPropName(AValue: string);
    procedure SetPropValue(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property PropName: string read GetPropName write SetPropName;
    property PropValue: string read GetPropValue write SetPropValue;
  end;

implementation

{ TBuildPropItem }

function TBuildPropItem.GetPropName: string;
begin
  Result := FBuildName.Text;
end;

function TBuildPropItem.GetPropValue: string;
begin
  Result := FBuildValue.Text;
end;

procedure TBuildPropItem.SetPropName(AValue: string);
begin
  FBuildName.Text := AValue;
end;

procedure TBuildPropItem.SetPropValue(AValue: string);
begin
  FBuildValue.Text := AValue;
end;

constructor TBuildPropItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 48;
  FBuildName := TvgText.Create(Self);
  FBuildName.Parent := Self;
  FBuildName.Align := vaLeft;
  FBuildName.Width := 100;
  FBuildName.Padding.Left := 8;
  FBuildName.HorzTextAlign := vgTextAlignNear;
  FBuildName.VertTextAlign := vgTextAlignCenter;
  FBuildName.Fill.SolidColor := vgColorFromVCL(clWhite);
  FBuildName.HitTest := False;

  FBuildValue := TvgHudTextBox.Create(Self);
  FBuildValue.Parent := Self;
  FBuildValue.Align := vaClient;
  FBuildValue.Padding.Left := 8;
  FBuildValue.Padding.Right := 8;
  FBuildValue.ReadOnly := True;
  FBuildValue.Padding.Top := 8;
  FBuildValue.Padding.Bottom := 8;

end;

destructor TBuildPropItem.Destroy;
begin
  inherited Destroy;
end;

end.

