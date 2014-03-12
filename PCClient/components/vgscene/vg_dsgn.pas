unit vg_dsgn;

{$I vg_define.inc}

interface
                             
uses
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, ExtDlgs, Dialogs, StdCtrls,
  vg_scene, vg_layouts, vg_objects, vg_controls, vg_colors, vg_ani,
  vg_tabcontrol, vg_textbox, vg_listbox, vg_treeview, vg_inspector;

type
  TvgBrushDesign = class(TForm)
    panelSolid: TvgRectangle;
    panelGradient: TvgRectangle;
    solidQuad: TvgColorQuad;
    solidPicker: TvgColorPicker;
    gradQuad: TvgColorQuad;
    vgBrushDesigner: TvgScene;
    solidCont: TvgRectangle;
    gradEditor: TvgGradientEdit;
    dsgnRoot: TvgBackground;
    Layout1: TvgLayout;
    ext1: TvgLabel;
    Layout2: TvgLayout;
    Layout3: TvgLayout;
    gradPicker: TvgColorPicker;
    brushTabControl: TvgTabControl;
    tabNone: TvgTabItem;
    tabSolid: TvgTabItem;
    tabGradient: TvgTabItem;
    Text1: TvgLabel;
    Text2: TvgLabel;
    Text3: TvgLabel;
    brushList: TvgListBox;
    textSolidR: TvgNumberBox;
    textSolidG: TvgNumberBox;
    textSolidB: TvgNumberBox;
    textSolidA: TvgNumberBox;
    textGradR: TvgNumberBox;
    textGradG: TvgNumberBox;
    textGradB: TvgNumberBox;
    textGradA: TvgNumberBox;
    textSolidHex: TvgTextBox;
    textGradHex: TvgTextBox;
    gradColorRect: TvgColorBox;
    solidColorRect: TvgColorBox;
    tabBitmap: TvgTabItem;
    panelBitmap: TvgLayout;
    tabRes: TvgTabItem;
    panerRes: TvgLayout;
    bitmapImage: TvgImage;
    Layout5: TvgLayout;
    btnSelectBitmap: TvgButton;
    resList: TvgListBox;
    Layout6: TvgLayout;
    btnMakeRes: TvgButton;
    Label1: TvgLabel;
    Rectangle1: TvgRectangle;
    tileModeList: TvgPopupBox;
    btnCancel: TvgButton;
    btnOK: TvgButton;
    makeResLayout: TvgLayout;
    gradAngle: TvgAngleButton;
    gradAlabel: TvgLabel;
    gradKind: TvgPopupBox;
    gradAngleLabel: TvgLabel;
    HudWindow1: TvgHudWindow;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure solidQuadChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gradEditorChange(Sender: TObject);
    procedure gradQuadChange(Sender: TObject);
    procedure brushListChange(Sender: TObject);
    procedure brushTabControlChange(Sender: TObject);
    procedure textGradRChange(Sender: TObject);
    procedure textGradHexChange(Sender: TObject);
    procedure textSolidHexChange(Sender: TObject);
    procedure btnSelectBitmapClick(Sender: TObject);
    procedure btnMakeResClick(Sender: TObject);
    procedure resListChange(Sender: TObject);
    procedure tileModeListChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure textSolidRChange(Sender: TObject);
    procedure gradAngleChange(Sender: TObject);
    procedure gradKindChange(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); //override;
    function UniqueName(S: string): string; //override;
  private
    FBrush: TvgBrush;
    FScene: IvgScene;
    FComp: TPersistent;
    procedure SetBrush(const Value: TvgBrush);
    procedure SetComp(const Value: TPersistent);
    procedure rebuilResList;
    { Private declarations }
  public
    { Public declarations }
    property Comp: TPersistent read FComp write SetComp;
    property Brush: TvgBrush read FBrush write SetBrush;
  end;

  TvgBrushStyles = set of TvgBrushStyle;

  TvgBrushDialog = class(TComponent)
  private
    FShowStyles: TvgBrushStyles;
    FShowBrushList: boolean;
    FShowMakeResource: boolean;
    FBrush: TvgBrush;
    FComponent: TComponent;
    FTitle: WideString;
    procedure SetBrush(const Value: TvgBrush);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean;
    property Brush: TvgBrush read FBrush write SetBrush;
    property Component: TComponent read FComponent write FComponent;
  published
    property ShowStyles: TvgBrushStyles read FShowStyles write FShowStyles;
    property ShowBrushList: boolean read FShowBrushList write FShowBrushList default true;
    property ShowMakeResource: boolean read FShowMakeResource write FShowMakeResource;
    property Title: WideString read FTitle write FTitle;
  end;

var
  vgDesign: TvgBrushDesign;

procedure SelectInDesign(AObject: TObject; AComp: TPersistent);
procedure ShowBrushDialog(const Brush: TvgBrush; const ShowStyles: TvgBrushStyles; const ShowBrushList: boolean = true);
procedure ShowGradientDialog(const Gradient: TvgGradient);
function ShowColorDialog(const Color: string): string;

implementation

uses TypInfo, vg_dsgn_bmp, Math;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure SelectInDesign(AObject: TObject; AComp: TPersistent);
begin
  if vgDesign = nil then
  begin
    vgDesign := TvgBrushDesign.Create(Application);
    try
    except
      vgDesign.Free;
      vgDesign := nil;
      raise;
    end;
  end;

  if AObject = nil then
  begin
    vgDesign.Comp := nil;
    vgDesign.Brush := nil;
  end;
  if AObject is TvgBrush then
  begin
    vgDesign.Comp := AComp;
    vgDesign.Brush := TvgBrush(AObject);
  end;
  if AObject is TvgObject then
  begin
    vgDesign.Comp := TPersistent(AObject);
    vgDesign.Brush := nil;
  end;

  vgDesign.Show;
end;

{ TvgBrushDesign ==============================================================}

procedure TvgBrushDesign.FormCreate(Sender: TObject);
begin
  { creeate }
end;

procedure TvgBrushDesign.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  vgDesign := nil;
end;

procedure TvgBrushDesign.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FComp) then
  begin
    Brush := nil;
    Comp := nil;
  end;
{  if (Operation = opRemove) and (AComponent = IScene) then
  begin
    IScene := nil;
  end;}
end;

procedure TvgBrushDesign.SetBrush(const Value: TvgBrush);
var
  i: integer;
begin
  FBrush := Value;
  if FBrush <> nil then
  begin
    case FBrush.Style of
      vgBrushNone:
        begin
          brushTabControl.ItemIndex := tabNone.Index;
        end;
      vgBrushSolid:
        begin
          solidPicker.Color := FBrush.SolidColor;
          gradEditor.Gradient.Assign(FBrush.Gradient);
          brushTabControl.ItemIndex := tabSolid.Index;
        end;
      vgBrushGradient:
        begin
          solidPicker.Color := FBrush.SolidColor;
          gradEditor.Gradient.Assign(FBrush.Gradient);
          gradKind.ItemIndex := Integer(gradEditor.Gradient.Style);
          if gradEditor.Gradient.StopPosition.X - gradEditor.Gradient.StartPosition.X <> 0 then
          begin
            if gradEditor.Gradient.StopPosition.X - gradEditor.Gradient.StartPosition.X > 0 then
              gradAngle.Value := -vgRadToDeg(ArcTan((gradEditor.Gradient.StopPosition.Y - gradEditor.Gradient.StartPosition.Y) / (gradEditor.Gradient.StopPosition.X - gradEditor.Gradient.StartPosition.X)))
            else
              gradAngle.Value := -vgRadToDeg(ArcTan((gradEditor.Gradient.StopPosition.Y - gradEditor.Gradient.StartPosition.Y) / (gradEditor.Gradient.StopPosition.X - gradEditor.Gradient.StartPosition.X))) - 180;
          end;
          gradAngle.Visible := gradEditor.Gradient.Style = vgLinearGradient;
          gradQuadChange(Self);
          brushTabControl.ItemIndex := tabGradient.Index;
        end;
      vgBrushVisual: ;
      vgBrushBitmap:
        begin
          brushTabControl.ItemIndex := tabBitmap.Index;
          bitmapImage.Bitmap.Assign(FBrush.Bitmap.Bitmap);
          tileModeList.ItemIndex := Integer(FBrush.Bitmap.WrapMode);
        end;
      vgBrushResource:
        begin
          brushTabControl.ItemIndex := tabRes.Index;
          rebuilResList;
        end;
    end;
    for i := 0 to brushList.Count - 1 do
      if (brushList.ItemByIndex(i) <> nil) and (TvgBrush(brushList.ItemByIndex(i).Tag) = Brush) then
        brushList.ItemIndex := i;
  end;
end;

procedure TvgBrushDesign.SetComp(const Value: TPersistent);
var
  i: integer;
  BrushButton: TvgListBoxItem;
  BrushText: TvgTextControl;
  PropCount: integer;
  PropList: PPropList;
  PropType: PTypeData;
begin
  FComp := Value;
  {$IFNDEF NOVCL}
  if (FComp <> nil) and (FComp is TvgScene) then
    FScene := TvgScene(FComp);
  {$ENDIF}
  if (FComp <> nil) and (FComp is TvgObject) then
    FScene := TvgObject(FComp).Scene;
  { find all brushes }
  Brush := nil;
  brushList.Clear;
  brushList.Height := 4;
  {$IFDEF KS_COMPILER5}
  PropCount := GetPropList(FComp.ClassInfo, [tkClass], nil);
  GetMem(PropList, SizeOf(PPropInfo) * PropCount);
  PropCount := GetPropList(FComp.ClassInfo, [tkClass], PropList);
  {$ELSE}
  PropCount := GetPropList(FComp.ClassInfo, PropList);
  {$ENDIF}
  for i := 0 to PropCount - 1 do
  begin
    if PropList[i].PropType^.Kind <> tkClass then Continue;
    PropType := GetTypeData(PropList[i].PropType{$IFNDEF FPC}^{$ENDIF});
    if PropType = nil then Continue;
    if not (PropType.ClassType.ClassName = 'TvgBrush') then Continue;
    BrushButton := TvgListBoxItem.Create(Self);
    BrushButton.Parent := brushList;
    BrushButton.Height := 23;
    BrushButton.Tag := Integer(GetObjectProp(FComp, PropList[i].Name));
    BrushText := TvgLabel.Create(Self);
    BrushText.Parent := BrushButton;
    BrushText.Align := vaClient;
    BrushText.HitTest := false;
    BrushText.Text := PropList[i].Name;
    brushList.Height := brushList.Height + BrushButton.Height;
    if Brush = nil then
      Brush := TvgBrush(GetObjectProp(FComp, PropList[i].Name));
  end;
  brushList.ItemIndex := 0;
  FreeMem(PropList, SizeOf(PPropInfo) * PropCount);
end;

function TvgBrushDesign.UniqueName(S: string): string;
begin
  if (FComp <> nil) and (FComp is TComponent) and (TComponent(FComp).Owner <> nil) and (TComponent(FComp).Owner is TCustomForm) then
    Result := TCustomForm(TComponent(FComp).Owner).Designer.UniqueName(S)
  else
  if Designer <> nil then
    Result := Designer.UniqueName(S)
  else
  begin
    Tag := Tag + 1;
    Result := S + IntToStr(Tag);
  end;
end;

procedure TvgBrushDesign.solidQuadChange(Sender: TObject);
begin
  if FBrush = nil then Exit;
  solidQuad.Alpha := ((FBrush.SolidColor and $FF000000) shr 24) / $FF;
  FBrush.SolidColor := (FBrush.SolidColor and not $FFFFFF) or ($00FFFFFF and vgHSLtoRGB(solidQuad.Hue, solidQuad.Sat, solidQuad.Lum));
  textSolidR.Value := TvgColorRec(FBrush.SolidColor).R;
  textSolidG.Value := TvgColorRec(FBrush.SolidColor).G;
  textSolidB.Value := TvgColorRec(FBrush.SolidColor).B;
  textSolidA.Value := TvgColorRec(FBrush.SolidColor).A;
  textSolidHex.Text := vgColorToStr(FBrush.SolidColor);
end;

procedure TvgBrushDesign.textSolidHexChange(Sender: TObject);
begin
  { change solid hex }
  if FBrush = nil then Exit;
  FBrush.SolidColor := vgStrToColor(textSolidHex.Text);
  solidPicker.Color := FBrush.SolidColor;
end;

procedure TvgBrushDesign.textSolidRChange(Sender: TObject);
var
  Color: TvgColor;
begin
  { solid textbox change }
  if FBrush = nil then Exit;
  Color := FBrush.SolidColor;
  TvgColorRec(Color).R := trunc(textSolidR.Value);
  TvgColorRec(Color).G := trunc(textSolidG.Value);
  TvgColorRec(Color).B := trunc(textSolidB.Value);
  TvgColorRec(Color).A := trunc(textSolidA.Value);
  FBrush.SolidColor := Color;
  solidPicker.Color := FBrush.SolidColor;
end;

procedure TvgBrushDesign.gradEditorChange(Sender: TObject);
begin
  { change gradient }
  if FBrush = nil then Exit;
  FBrush.Gradient.Assign(gradEditor.Gradient);
end;

procedure TvgBrushDesign.gradQuadChange(Sender: TObject);
begin
  { chage color in current point }
  if FBrush = nil then Exit;
  gradEditor.Gradient.Points[gradEditor.CurrentPoint].IntColor :=
    (gradEditor.Gradient.Points[gradEditor.CurrentPoint].IntColor and $FF000000) or ($00FFFFFF and vgHSLtoRGB(gradQuad.Hue, gradQuad.Sat, gradQuad.Lum));
  FBrush.Gradient.Assign(gradEditor.Gradient);
  textGradR.Value := TvgColorRec(gradColorRect.Color).R;
  textGradG.Value := TvgColorRec(gradColorRect.Color).G;
  textGradB.Value := TvgColorRec(gradColorRect.Color).B;
  textGradA.Value := TvgColorRec(gradColorRect.Color).A;
  textGradHex.Text := vgColorToStr(gradColorRect.Color);
  gradEditor.Repaint;
end;

procedure TvgBrushDesign.brushListChange(Sender: TObject);
begin
  if FScene = nil then Exit;
  if FComp = nil then Exit;
  if Sender <> nil then
    Brush := TvgBrush(TvgListBoxItem(Sender).Tag);
end;

procedure TvgBrushDesign.brushTabControlChange(Sender: TObject);
begin
  if FBrush = nil then Exit;
  if brushTabControl.ItemIndex = tabNone.Index then
    FBrush.Style := vgBrushNone;
  if brushTabControl.ItemIndex = tabSolid.Index then
    FBrush.Style := vgBrushSolid;
  if brushTabControl.ItemIndex = tabGradient.Index then
  begin
    FBrush.Style := vgBrushGradient;
    gradQuadChange(Sender);
    gradAngleLabel.Text := InttoStr(Trunc(gradAngle.Value));
  end;
  if brushTabControl.ItemIndex = tabBitmap.Index then
    FBrush.Style := vgBrushBitmap;
  if brushTabControl.ItemIndex = tabRes.Index then
    FBrush.Style := vgBrushResource;

  btnMakeRes.Visible := (brushTabControl.ItemIndex <> tabRes.Index) and (brushTabControl.ItemIndex <> tabNone.Index);
  if not btnMakeRes.Visible then
    rebuilResList;
end;

procedure TvgBrushDesign.textGradRChange(Sender: TObject);
var
  Color: TvgColor;
begin
  { change grad brush alpha }
  if FBrush = nil then Exit;
  Color := gradEditor.Gradient.Points[gradEditor.CurrentPoint].IntColor;
  TvgColorRec(Color).R := trunc(textGradR.Value);
  TvgColorRec(Color).G := trunc(textGradG.Value);
  TvgColorRec(Color).B := trunc(textGradB.Value);
  TvgColorRec(Color).A := trunc(textGradA.Value);
  gradEditor.Gradient.Points[gradEditor.CurrentPoint].IntColor := Color;
  gradEditor.UpdateGradient;
end;

procedure TvgBrushDesign.gradKindChange(Sender: TObject);
begin
  { change grad type }
  if FBrush = nil then Exit;
  gradEditor.Gradient.Style := TvgGradientStyle(gradKind.ItemIndex);
  gradEditor.UpdateGradient;
  gradAngle.Visible := gradEditor.Gradient.Style = vgLinearGradient;
end;

procedure TvgBrushDesign.gradAngleChange(Sender: TObject);
var
  Color: TvgColor;
  X, Y, Koef: single;
begin
  { change grad brush alpha }
  if FBrush = nil then Exit;
  if (Cos(vgDegToRad(gradAngle.Value)) <> 0) and (Abs(1 / Cos(vgDegToRad(gradAngle.Value))) >= 1) and (Abs(1 / Cos(vgDegToRad(gradAngle.Value))) <= 1.42) then
    X := Abs(1 / Cos(vgDegToRad(gradAngle.Value)))
  else
    X := 1;
  if (Sin(vgDegToRad(gradAngle.Value)) <> 0) and (Abs(1 / Sin(vgDegToRad(gradAngle.Value))) >= 1) and (Abs(1 / Sin(vgDegToRad(gradAngle.Value))) <= 1.42) then
    Y := Abs(1 / Sin(vgDegToRad(gradAngle.Value)))
  else
    Y := 1;
  Koef := vgMaxFloat(X, Y);
  Koef := Koef * 0.5;
  gradEditor.Gradient.StartPosition.Point := vgPoint(0.5 - (Cos(vgDegToRad(gradAngle.Value)) * Koef), 0.5 + (Sin(vgDegToRad(gradAngle.Value)) * Koef));
  gradEditor.Gradient.StopPosition.Point := vgPoint(0.5 + (Cos(vgDegToRad(gradAngle.Value)) * Koef), 0.5 - (Sin(vgDegToRad(gradAngle.Value)) * Koef));
  gradEditor.UpdateGradient;
  gradAngleLabel.Text := InttoStr(Trunc(gradAngle.Value));
end;

procedure TvgBrushDesign.textGradHexChange(Sender: TObject);
begin
  { change gradient hex }
  if FBrush = nil then Exit;
  gradEditor.Gradient.Points[gradEditor.CurrentPoint].IntColor := vgStrToColor(textGradHex.Text);
  gradEditor.UpdateGradient;
end;

procedure TvgBrushDesign.btnSelectBitmapClick(Sender: TObject);
begin
  if FBrush = nil then Exit;
  vgBitmapEditor := TvgBitmapEditor.Create(nil);
  vgBitmapEditor.AssignFromBitmap(bitmapImage.Bitmap);
  if vgBitmapEditor.ShowModal = mrOk then
  begin
    vgBitmapEditor.AssignToBitmap(bitmapImage.Bitmap);
    FBrush.Bitmap.Bitmap.Assign(bitmapImage.Bitmap);
    if (FComp <> nil) and (FComp is TvgVisualObject) then
      TvgVisualObject(FComp).Repaint;
    bitmapImage.Repaint;
  end;
  vgBitmapEditor.Free;
end;

procedure TvgBrushDesign.rebuilResList;
var
  i: integer;
  L: TList;
  item: TvgListBoxItem;
  itemText: TvgTextControl;
  rect: TvgRectangle;
begin
  if FScene = nil then Exit;
  if FScene.GetRoot = nil then Exit;
  if FBrush = nil then Exit;

  resList.Clear;

  L := TList.Create;
  FScene.GetRoot.AddObjectsToList(L);
  for i := 0 to L.Count - 1 do
    if TvgObject(L[i]) is TvgBrushObject then
    begin
      item := TvgListBoxItem.Create(Self);
      item.Tag := Integer(L[i]);
      item.Parent := resList;
      itemText := TvgLabel.Create(Self);
      itemText.Parent := item;
      itemText.Align := vaClient;
      itemText.HitTest := false;
      itemText.Text := TvgObject(L[i]).ResourceName;
      rect := TvgRectangle.Create(Self);
      rect.Parent := item;
      rect.Align := vaLeft;
      rect.HitTest := false;
      rect.Padding.Rect := vgRect(2, 2, 2, 2);
      rect.Fill.Style := vgBrushResource;
      rect.Fill.Resource.Resource := TvgBrushObject(L[i]);
      rect.Stroke.Color := '#80FFFFFF';
      if FBrush.Resource.Resource = TvgBrushObject(L[i]) then
        resList.ItemIndex := resList.Count - 1;
    end;
  L.Free;
end;

procedure TvgBrushDesign.resListChange(Sender: TObject);
begin
  if FScene = nil then Exit;
  if FScene.GetRoot = nil then Exit;
  if FBrush = nil then Exit;
  if Sender = nil then Exit;

  FBrush.Assign(TvgBrushObject(TvgListBoxItem(Sender).Tag).Brush);
  FBrush.Resource.Resource := TvgBrushObject(TvgListBoxItem(Sender).Tag);
  FBrush.Style := vgBrushResource;
end;

procedure TvgBrushDesign.btnMakeResClick(Sender: TObject);
var
  S: string;
  B: TvgBrushObject;
begin
  if FBrush = nil then Exit;
  if FScene = nil then Exit;
  if FScene.GetRoot = nil then Exit;

  { make res }
  S := UniqueName('Brush');
  if InputQuery('New TvgBrushObject', 'Enter resource name:', S) then
  begin
    B := TvgBrushObject.Create(FScene.GetOwner);
    B.Parent := FScene.GetRoot;
    B.ResourceName := S;
    B.Name := B.ResourceName;
    B.ResourceName := S;
    B.Brush.Assign(FBrush);

    rebuilResList;
  end;
end;

procedure TvgBrushDesign.tileModeListChange(Sender: TObject);
begin
  if FBrush = nil then Exit;
  FBrush.Bitmap.WrapMode := TvgWrapMode(tileModeList.ItemIndex);
  if (FComp <> nil) and (FComp is TvgVisualObject) then
    TvgVisualObject(FComp).Repaint;
end;

procedure TvgBrushDesign.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TvgBrushDesign.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

{ TvgBrushDialog }

procedure ShowBrushDialog(const Brush: TvgBrush; const ShowStyles: TvgBrushStyles; const ShowBrushList: boolean);
var
  Dlg: TvgBrushDialog;
begin
  Dlg := TvgBrushDialog.Create(nil);
  Dlg.Brush := Brush;
  Dlg.ShowStyles := ShowStyles;
  Dlg.ShowBrushList := ShowBrushList;
  if Dlg.Execute then
    Brush.Assign(Dlg.Brush);
  Dlg.Free;
end;

procedure ShowGradientDialog(const Gradient: TvgGradient);
var
  Dlg: TvgBrushDialog;
begin
  Dlg := TvgBrushDialog.Create(nil);
  Dlg.Brush.Style := vgBrushGradient;
  Dlg.Brush.Gradient := Gradient;
  Dlg.ShowStyles := [vgBrushGradient];
  Dlg.ShowBrushList := false;
  if Dlg.Execute then
    Gradient.Assign(Dlg.Brush.Gradient);
  Dlg.Free;
end;

function ShowColorDialog(const Color: string): string;
var
  Dlg: TvgBrushDialog;
begin
  Dlg := TvgBrushDialog.Create(nil);
  Dlg.Brush.Style := vgBrushSolid;
  Dlg.Brush.Color := Color;
  Dlg.ShowStyles := [vgBrushSolid];
  Dlg.ShowBrushList := false;
  if Dlg.Execute then
  begin
    Result := Dlg.Brush.Color;
  end;
  Dlg.Free;
end;


constructor TvgBrushDialog.Create(AOwner: TComponent);
begin
  inherited;
  FBrush := TvgBrush.Create(vgBrushSolid, $FF808080);
  FShowStyles := [vgBrushNone, vgBrushSolid, vgBrushGradient, vgBrushBitmap, vgBrushResource];
  FShowBrushList := true;
  FShowMakeResource := false;
  FTitle := 'Brush';
end;

destructor TvgBrushDialog.Destroy;
begin
  FBrush.Free;
  inherited;
end;

function TvgBrushDialog.Execute: boolean;
var
  Dialog: TvgBrushDesign;
  EditBrush: TvgBrush;
begin
  Dialog := TvgBrushDesign.Create(Application);
  Dialog.brushList.Visible := ShowBrushList;
  if FComponent <> nil then
    Dialog.Comp := FComponent
  else
    Dialog.brushList.Visible := false;

  Dialog.HudWindow1.Text := FTitle;
  Dialog.brushTabControl.ItemIndex := -1;
  Dialog.tabNone.Visible := vgBrushNone in ShowStyles;
  Dialog.tabSolid.Visible := vgBrushSolid in ShowStyles;
  Dialog.tabGradient.Visible := vgBrushGradient in ShowStyles;
  Dialog.tabBitmap.Visible := vgBrushBitmap in ShowStyles;
  Dialog.tabRes.Visible := vgBrushResource in ShowStyles;
  Dialog.brushTabControl.Realign;
  Dialog.btnOK.Visible := true;
  Dialog.btnCancel.Visible := true;

  Dialog.Brush := FBrush;
  Dialog.makeResLayout.Visible := ShowMakeResource;
  Dialog.dsgnRoot.Height := Dialog.dsgnRoot.Height - 1; // realign
  Dialog.ClientHeight := Trunc(Dialog.Layout6.Position.Y + Dialog.Layout6.Height + Dialog.HudWindow1.Margins.Top + Dialog.HudWindow1.Margins.Bottom);
  Result := Dialog.ShowModal = mrOk;
  Dialog.Free;
end;

procedure TvgBrushDialog.SetBrush(const Value: TvgBrush);
begin
  FBrush.Assign(Value);;
end;

end.

