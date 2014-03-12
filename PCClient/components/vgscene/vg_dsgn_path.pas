unit vg_dsgn_path;

interface

uses
  {$IFDEF WIN32}
  Windows, Messages, MMSystem,
  {$ENDIF}
  {$IFDEF FPC}
  LMessages, LResources,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Clipbrd,
  Dialogs, StdCtrls, vg_controls, vg_memo, vg_scene, vg_layouts, vg_objects;

type
  TvgPathDataDesigner = class(TForm)
    vgScene1: TvgScene;
    Root1: TvgBackground;
    previewLayout: TvgLayout;
    Layout2: TvgLayout;
    PathData: TvgMemo;
    Button2: TvgButton;
    Button3: TvgButton;
    labelMemo: TvgLabel;
    Label3: TvgLabel;
    previewPath: TvgPath;
    Button1: TvgButton;
    procedure PathDataChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  vgPathDataDesigner: TvgPathDataDesigner;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TvgPathDataDesigner.PathDataChange(Sender: TObject);
begin
  if previewLayout.Visible then
    previewPath.Data.Data := PathData.Text;
end;

procedure TvgPathDataDesigner.Button2Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TvgPathDataDesigner.Button3Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TvgPathDataDesigner.Button1Click(Sender: TObject);
begin
  pathData.SelectAll;
  pathData.ClearSelection;
  pathData.PasteFromClipboard;
end;

end.
