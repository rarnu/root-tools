unit baseform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  vg_scene, vg_controls, vg_objects, baseconfig;

type

  { TFormBase }

  TFormBase = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FScene: TvgScene;
    FRoot: TvgBackground;
    FWindow: TvgHudWindow;
    FPanel: TvgHudContainer;
    FConfig: TConfigBase;
  protected
    function SetWindowTitle: string; virtual; abstract;
    procedure InitForm; virtual; abstract;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); virtual; abstract;
  public
    property Window: TvgHudWindow read FWindow;
    property Panel: TvgHudContainer read FPanel;
    property Config: TConfigBase read FConfig;
  end;

var
  FormBase: TFormBase;

implementation

{$R *.lfm}

{ TFormBase }

procedure TFormBase.FormCreate(Sender: TObject);
begin
  Window.Text := SetWindowTitle;
  InitForm;
end;

procedure TFormBase.FormDestroy(Sender: TObject);
begin

end;

constructor TFormBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfig := TConfigBase.Create;

  FScene := TvgScene.Create(Self);
  FScene.Align := alClient;
  FScene.Transparency := True;
  FScene.Parent := Self;

  FRoot := TvgBackground.Create(FScene);
  FRoot.Align := vaClient;
  FRoot.HitTest := False;
  FScene.AddObject(FRoot);

  FWindow := TvgHudWindow.Create(FRoot);
  FWindow.Align := vaClient;
  Fwindow.ShowSizeGrip := False;
  FWindow.TextAlign := vgTextAlignCenter;
  FWindow.Parent := FRoot;

  FPanel := TvgHudContainer.Create(FWindow);
  FPanel.Align := vaClient;
  FPanel.Padding.Left := FConfig.PanelPaddingLeft;
  FPanel.Padding.Right := FConfig.PanelPaddingRight;
  FPanel.Padding.Top := FConfig.PanelPaddingTop;
  FPanel.Padding.Bottom := FConfig.PanelPaddingBottom;
  FPanel.Parent := FWindow;
end;

destructor TFormBase.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

end.
