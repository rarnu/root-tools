unit vg_dsgn_imglist;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, vg_controls, vg_layouts, vg_listbox, vg_extctrls, vg_scene,
  vg_actions;

type
  TfrmDsgnImageList = class(TForm)
    vgScene1: TvgScene;
    Root1: TvgBackground;
    HudWindow1: TvgHudWindow;
    ImageList: TvgImageListBox;
    btnAddFiles: TvgHudButton;
    OpenDialog1: TOpenDialog;
    HudButton1: TvgHudButton;
    btnDelete: TvgHudButton;
    btnClear: TvgHudButton;
    btnCancel: TvgHudButton;
    procedure btnAddFilesClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDsgnImageList: TfrmDsgnImageList;

procedure ShowDsgnImageList(ImgList: TvgImageList);

implementation

{$R *.dfm}

procedure ShowDsgnImageList(ImgList: TvgImageList);
var
  i: integer;
begin
  frmDsgnImageList := TfrmDsgnImageList.Create(Application);
  with frmDsgnImageList do
  begin
    for i := 0 to ImgList.Count - 1 do
      ImageList.AddBitmap('', ImgList.Images[i]);
    ImageList.ItemWidth := ImgList.Width;
    ImageList.ItemHeight := ImgList.Height;
    if ShowModal = mrOK then
    begin
      ImgList.Clear;
      for i := 0 to ImageList.Count - 1 do
        if ImageList.Images[i] <> nil then
        begin
          ImgList.Add(ImageList.Images[i].Bitmap);
        end;
    end;
  end;
  frmDsgnImageList.Free;
end;

procedure TfrmDsgnImageList.btnAddFilesClick(Sender: TObject);
var
  i: integer;
begin
  OpenDialog1.Filter := DefaultFilterClass.GetFileTypes;
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count - 1 do
      ImageList.AddFile(OpenDialog1.Files[i]);
  end;
end;

procedure TfrmDsgnImageList.btnClearClick(Sender: TObject);
begin
  ImageList.Clear;
end;

procedure TfrmDsgnImageList.btnDeleteClick(Sender: TObject);
begin
  if ImageList.Selected <> nil then
    ImageList.Selected.Free;
end;

end.
