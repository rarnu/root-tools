unit page_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vg_scene, vg_controls, vg_objects, vg_ani, baseform;

type

  { TPageMain }

  TPageMain = class(TvgHudPanel)
  private
    FAni: TvgAniIndicator;
    FBtnStart: TvgHudButton;
    FBtnStop: TvgHudButton;
    FBase: TFormBase;

  protected
    procedure buttonClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ABase: TFormBase); reintroduce;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TPageMain }

procedure TPageMain.buttonClicked(Sender: TObject);
var
  ATag: integer;
begin
  ATag := TvgHudButton(Sender).Tag;
  case ATag of
    1: Start;
    2: Stop;
  end;
end;

constructor TPageMain.Create(AOwner: TComponent; ABase: TFormBase);
var
  clr: string;
begin
  inherited Create(AOwner);
  FBase := ABase;

  FAni := TvgAniIndicator.Create(Self);
  FAni.Parent := Self;
  FAni.Align := vaCenter;

  FBtnStart := TvgHudButton.Create(Self);
  FBtnStart.Parent := Self;
  FBtnStart.Position.X := 8;
  FBtnStart.Position.Y := 8;
  FBtnStart.Width := 120;
  FBtnStart.Height := 48;
  FBtnStart.Text := 'START';
  FBtnStart.Tag := 1;

  FBtnStop := TvgHudButton.Create(Self);
  FBtnStop.Parent := Self;
  FBtnStop.Position.X := 140;
  FBtnStop.Position.Y := 8;
  FBtnStop.Width := 120;
  FBtnStop.Height := 48;
  FBtnStop.Text := 'STOP';
  FBtnStop.Tag := 2;

  FBtnStart.OnClick := @buttonClicked;
  FBtnStop.OnClick := @buttonClicked;

end;

destructor TPageMain.Destroy;
begin
  inherited Destroy;
end;

procedure TPageMain.Start;
begin
  FAni.Enabled := True;
end;

procedure TPageMain.Stop;
begin
  FAni.Enabled := False;
end;

end.

