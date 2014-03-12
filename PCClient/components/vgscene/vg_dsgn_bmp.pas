unit vg_dsgn_bmp;

interface

uses
  ExtDlgs, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  vg_scene, vg_controls, vg_layouts, vg_objects, vg_textbox,
  vg_effects;

type
  TvgBitmapEditor = class(TForm)
    vgScene1: TvgScene;
    Root1: TvgBackground;
    Button1: TvgButton;
    Layout1: TvgLayout;
    Button2: TvgButton;
    btnOk: TvgButton;
    ScrollBox1: TvgScrollBox;
    Rectangle1: TvgPanel;
    Preview: TvgPaintBox;
    labelScale: TvgLabel;
    trackScale: TvgTrack;
    cropButton: TvgButton;
    Image1: TvgImage;
    btnPaste: TvgButton;
    Layout2: TvgLayout;
    btnFit: TvgButton;
    btnOriginal: TvgButton;
    editControl: TvgControl;
    Button3: TvgButton;
    GroupBox1: TvgGroupBox;
    newWidth: TvgNumberBox;
    Label1: TvgLabel;
    Label2: TvgLabel;
    newHeight: TvgNumberBox;
    resizeLayout: TvgBackground;
    ShadowEffect1: TvgShadowEffect;
    Button4: TvgButton;
    Button5: TvgButton;
    btnResize: TvgButton;
    btnSave: TvgButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewPaint(Sender: TObject; const Canvas: TvgCanvas);
    procedure trackScaleChange(Sender: TObject);
    procedure cropButtonClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure trackScaleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnPasteClick(Sender: TObject);
    procedure btnFitClick(Sender: TObject);
    procedure btnOriginalClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnResizeClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure newWidthChange(Sender: TObject);
    procedure newHeightChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FBitmap: TvgBitmap;
    FSourceRect: TvgRect;
    FCropRect: TvgSelection;
    FOldScale: single;
    FFileName: string;
  public
    { Public declarations }
    procedure AssignFromBitmap(B: TvgBitmap);
    procedure AssignToBitmap(B: TvgBitmap);
    property FileName: string read FFileName write FFileName;
  end;

var
  vgBitmapEditor: TvgBitmapEditor;

implementation

uses Clipbrd;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TvgBitmapEditor.FormCreate(Sender: TObject);
begin
  resizeLayout.Visible := false;
  FBitmap := TvgBitmap.Create(1, 1);
  FOldScale := 1;
end;

procedure TvgBitmapEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBitmap);
end;

procedure TvgBitmapEditor.Button1Click(Sender: TObject);
var
  D: TOpenDialog;
begin
  D := TOpenDialog.Create(Application);
  try
    D.Filter := 'Images|' + DefaultFilterClass.GetFileTypes;
    if D.Execute then
    begin
      FileName := D.FileName;
      FBitmap.LoadFromFile(D.FileName);
      Preview.Repaint;
      FSourceRect := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
      Preview.Width := FBitmap.Width * trackScale.Value;
      Preview.Height := FBitmap.Height * trackScale.Value;
      ScrollBox1.Realign;

      editControl.Enabled := true;
      btnOk.Enabled := true;
    end;
  finally
    D.Free;
  end;
end;

procedure TvgBitmapEditor.PreviewPaint(Sender: TObject; const Canvas: TvgCanvas);
begin
  Canvas.DrawBitmap(FBitmap, FSourceRect, vgRect(0, 0, vgRectWidth(FSourceRect) * trackScale.Value, vgRectHeight(FSourceRect) * trackScale.Value), 1);
end;

procedure TvgBitmapEditor.trackScaleChange(Sender: TObject);
begin
  Preview.Width := FBitmap.Width * trackScale.Value;
  Preview.Height := FBitmap.Height * trackScale.Value;
  ScrollBox1.Realign;

  if FCropRect <> nil then
  begin
    FCropRect.Position.X := (FCropRect.Position.X / FOldScale) * trackScale.Value;
    FCropRect.Position.Y := (FCropRect.Position.Y / FOldScale) * trackScale.Value;
    FCropRect.Width := (FCropRect.Width / FOldScale) * trackScale.Value;
    FCropRect.Height := (FCropRect.Height / FOldScale) * trackScale.Value;
  end;

  FOldScale := trackScale.Value;
  labelScale.Text := 'Scale: ' + IntToStr(Round(trackScale.Value * 100)) + '%';
end;

procedure TvgBitmapEditor.cropButtonClick(Sender: TObject);
begin
  if FCropRect = nil then
  begin
    btnFitClick(Self);
    
    FCropRect := TvgSelection.Create(Self);
    FCropRect.Parent := Preview;
    cropButton.Text := 'Full';
    FCropRect.SetBounds(Trunc(FBitmap.Width * 0.1 * trackScale.Value), Trunc(FBitmap.Height * 0.1 * trackScale.Value),
      (FBitmap.Width * 0.8 * trackScale.Value), Trunc(FBitmap.Height * 0.8 * trackScale.Value));
  end
  else
  begin
    FreeAndNil(FCropRect);
    Preview.Repaint;
    cropButton.Text := 'Crop';
  end;
end;

procedure TvgBitmapEditor.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TvgBitmapEditor.AssignFromBitmap(B: TvgBitmap);
begin
  FileName := '';
  if B <> nil then
    FBitmap.Assign(B);
  FSourceRect := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
  Preview.Width := FBitmap.Width * trackScale.Value;
  Preview.Height := FBitmap.Height * trackScale.Value;
  ScrollBox1.Realign;
  if B.Width > 1 then
  begin
    editControl.Enabled := true;
    btnOk.Enabled := true;
  end;
end;

procedure TvgBitmapEditor.AssignToBitmap(B: TvgBitmap);
var
  j: integer;
  CR: TvgRect;
begin
  if B <> nil then
  begin
    if (FCropRect <> nil) and (FCropRect.Width > 0) and (FCropRect.Height > 0) then
    begin
      CR := vgRect(FCropRect.Position.X / trackScale.Value, FCropRect.Position.Y / trackScale.Value,
        (FCropRect.Position.X + FCropRect.Width) / trackScale.Value, (FCropRect.Position.Y + FCropRect.Height) / trackScale.Value);
      B.SetSize(Trunc(vgRectWidth(CR)), Trunc(vgRectHeight(CR)));
      for j := 0 to B.Height - 1 do
        vgMoveLongword(@FBitmap.Scanline[(Trunc(CR.Top) + j)][Trunc(CR.Left)], @B.Scanline[j][0], B.Width);
      B.BitmapChanged;
    end
    else
    begin
      B.Assign(FBitmap);
    end;
  end;
end;

procedure TvgBitmapEditor.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TvgBitmapEditor.trackScaleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssDouble in Shift then trackScale.Value := 1;
end;

procedure TvgBitmapEditor.btnPasteClick(Sender: TObject);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  B.PixelFormat := pf32bit;
  B.HandleType := bmDIB;
  B.Assign(Clipboard);
  FBitmap.Assign(B);
  B.Free;
end;

procedure TvgBitmapEditor.btnFitClick(Sender: TObject);
var
  R: TvgRect;
begin
  R := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
  trackScale.Value := 1 / vgFitRect(R, ScrollBox1.LocalRect);
end;

procedure TvgBitmapEditor.btnOriginalClick(Sender: TObject);
begin
  trackScale.Value := 1;
end;

procedure TvgBitmapEditor.Button3Click(Sender: TObject);
begin
  editControl.Enabled := false;
  btnOk.Enabled := true;
  if FCropRect <> nil then cropButtonClick(Self);

  trackScale.Value := 1;
  FBitmap.SetSize(1, 1);
  FSourceRect := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
  Preview.Width := FBitmap.Width * trackScale.Value;
  Preview.Height := FBitmap.Height * trackScale.Value;
  ScrollBox1.Realign;
end;

procedure TvgBitmapEditor.btnResizeClick(Sender: TObject);
begin
  newWidth.Value := FBitmap.Width;
  newHeight.Value := FBitmap.Height;

  resizeLayout.Visible := true;
  resizeLayout.Position.Y := -resizeLayout.Height;
  resizeLayout.AnimateFloat('Position.Y', 0, 0.3);
end;

procedure TvgBitmapEditor.Button4Click(Sender: TObject);
var
  tmp: TvgBitmap;
  SaveFileName: string;
begin
  { resize }
  SaveFileName := FileName;
  tmp := TvgBitmap.Create(trunc(newWidth.Value), trunc(newHeight.Value));
  tmp.Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height), vgRect(0, 0, tmp.Width, tmp.Height), 1);
  AssignFromBitmap(tmp);
  tmp.Free;
  FileName := SaveFileName;
  { }
  resizeLayout.AnimateFloatWait('Position.Y', -resizeLayout.Height, 0.3);
  resizeLayout.Visible := false;
end;

procedure TvgBitmapEditor.Button5Click(Sender: TObject);
begin
  resizeLayout.AnimateFloatWait('Position.Y', -resizeLayout.Height, 0.3);
  resizeLayout.Visible := false;
end;

procedure TvgBitmapEditor.newWidthChange(Sender: TObject);
begin
  newHeight.Value := Round(newWidth.Value * (FBitmap.Height / FBitmap.Width));
end;

procedure TvgBitmapEditor.newHeightChange(Sender: TObject);
begin
  newWidth.Value := Round(newHeight.Value * (FBitmap.Width / FBitmap.Height));
end;

procedure TvgBitmapEditor.btnSaveClick(Sender: TObject);
begin
  SaveDialog1.FileName := FFileName;
  if SaveDialog1.Execute then
    FBitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
