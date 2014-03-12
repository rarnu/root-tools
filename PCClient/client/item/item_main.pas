unit item_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vg_scene, vg_controls, vg_objects, vg_listbox, Graphics;

type

  { TMainItem }

  TMainItem = class(TvgListBoxItem)
  private
    FTitle: TvgText;
    function GetTitle: string;
    procedure SetTitle(AValue: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Title: string read GetTitle write SetTitle;
  end;

implementation

{ TMainItem }

function TMainItem.GetTitle: string;
begin
  Result := FTitle.Text;
end;

procedure TMainItem.SetTitle(AValue: string);
begin
  FTitle.Text := AValue;
end;

constructor TMainItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height:= 48;
  FTitle := TvgText.Create(Self);
  FTitle.Parent := Self;
  FTitle.Align := vaClient;
  FTitle.HorzTextAlign := vgTextAlignNear;
  FTitle.VertTextAlign := vgTextAlignCenter;
  FTitle.Padding.Left := 16;
  FTitle.Font.Size:=14;
  FTitle.Fill.Color:='#FFFFFFFF';
  FTitle.HitTest:= False;
end;

destructor TMainItem.Destroy;
begin
  inherited Destroy;
end;

end.

