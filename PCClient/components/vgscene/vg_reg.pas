unit vg_reg;

{$I vg_define.inc}

interface

uses
  {$IFDEF WIN32}
  Windows, Messages, MMSystem,
  {$ENDIF}
  {$IFDEF FPC}
  LMessages, LResources,
  {$ENDIF}
  Classes, Controls, Dialogs, ExtDlgs, SysUtils, Forms,
  {$IFNDEF FPC}
    {$IFDEF KS_COMPILER6_UP} DesignIntf, DesignEditors, VCLEditors {$ELSE} DsgnIntf {$ENDIF},
  {$ELSE}
  LazIDEIntf, PropEdits, ComponentEditors, FormEditingIntf,
  {$ENDIF}
  vg_scene;

type

  TvgIDEDesigner = class(TvgDesigner{$IFNDEF FPC}, IDesignNotification{$ENDIF})
  private
    {$IFNDEF FPC}
    {$IFDEF KS_COMPILER6_UP}
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
    {$ELSE}
    procedure ItemDeleted(const AItem: IPersistent);
    procedure ItemInserted(const AItem: IPersistent);
    procedure ItemsModified(const ADesigner: IUnknown);
    procedure SelectionChanged(const ASelection: IDesignerSelections);
    procedure DesignerInitialized(const ADesigner: IUnknown);
    procedure DesignerClosed(const ADesigner: IUnknown);
    {$ENDIF}
    {$ENDIF}
  public
    procedure SelectObject(ADesigner: TComponent; AObject: TvgObject; MultiSelection: array of TvgObject); override;
    procedure Modified(ADesigner: TComponent); override;
    function UniqueName(ADesigner: TComponent; ClassName: string): string; override;
    function IsSelected(ADesigner: TComponent; const AObject: TObject): boolean; override;
    procedure EditStyle(const Res: TvgResources; const ASelected: string); override;
  end;

type

  TvgBrushProperty = class(TClassProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;

  TvgBitmapProperty = class(TClassProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;

  TvgPathDataProperty = class(TClassProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;

  TvgResourceProperty = class(TClassProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;

  TvgWideStringsProperty = class(TClassProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;

  TvgFontProperty = class(TClassProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;

  TvgColorProperty = class(TStringProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

  TvgGradientProperty = class(TStringProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

  TvgTriggerProperty = class(TStringProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TvgLangProperty = class(TStringProperty)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TvgImgListEditor = class(TComponentEditor)
  private
  public
    procedure Edit; override;
  end;

  TvgLangEditor = class(TComponentEditor)
  private
  public
    procedure Edit; override;
  end;

procedure Register;

implementation {===============================================================}

uses vg_objects, vg_controls, vg_ani, vg_dsgn, vg_colors, vg_listbox, vg_tabcontrol,
  vg_effects, Graphics, vg_layouts, vg_textbox, vg_dsgn_path, vg_memo,
  vg_extctrls, vg_dsgn_bmp, vg_treeview, vg_dsgn_styles, vg_inspector,
  vg_grid, vg_dbctrls, vg_dsgn_lang, vg_actions, vg_dsgn_imglist;

{ TvgIDEDesigner }

{$IFNDEF FPC}

{$IFDEF KS_COMPILER6_UP}

procedure TvgIDEDesigner.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
end;

procedure TvgIDEDesigner.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin
end;

procedure TvgIDEDesigner.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
end;

procedure TvgIDEDesigner.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
end;

procedure TvgIDEDesigner.ItemsModified(const ADesigner: IDesigner);
begin
end;

procedure TvgIDEDesigner.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
  if (ASelection.Count > 0) then
    CallDesignSelect(ASelection.Get(0));
end;

{$ELSE}

procedure TvgIDEDesigner.ItemDeleted(const AItem: IPersistent);
begin
end;

procedure TvgIDEDesigner.ItemInserted(const AItem: IPersistent);
begin
end;

procedure TvgIDEDesigner.ItemsModified(const ADesigner: IUnknown);
begin
end;

procedure TvgIDEDesigner.SelectionChanged(const ASelection: IDesignerSelections);
begin
{  if (ASelection.Count > 0) then
    CallDesignSelect(ASelection.Get(0));}
end;

procedure TvgIDEDesigner.DesignerInitialized(const ADesigner: IUnknown);
begin
end;

procedure TvgIDEDesigner.DesignerClosed(const ADesigner: IUnknown);
begin
end;

{$ENDIF}

procedure TvgIDEDesigner.Modified(ADesigner: TComponent);
var
  D: IDesigner;
begin
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    TCustomForm(ADesigner).Designer.QueryInterface(IDesigner, D);
    if Assigned(D) then
    begin
      D.Modified;
    end;
  end;
end;

procedure TvgIDEDesigner.SelectObject(ADesigner: TComponent; AObject: TvgObject; MultiSelection: array of TvgObject);
var
  D: {$IFDEF KS_COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF};
  List: IDesignerSelections;
  i: integer;
begin
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    TCustomForm(ADesigner).Designer.QueryInterface({$IFDEF KS_COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}, D);
    if Assigned(D) then
    begin
      D.SelectComponent(AObject);
      {$IFDEF KS_COMPILER6_UP}
      if Length(MultiSelection) > 0 then
      begin
        List := CreateSelectionList;
        D.GetSelections(List);
        for i := 0 to High(MultiSelection) do
          List.Add(MultiSelection[i]);
        D.SetSelections(List);
        List := nil;
      end;
      {$ENDIF}
    end;
  end;
end;

function TvgIDEDesigner.IsSelected(ADesigner: TComponent; const AObject: TObject): boolean;
var
  D: {$IFDEF KS_COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF};
  List: IDesignerSelections;
  i: integer;
begin
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    TCustomForm(ADesigner).Designer.QueryInterface({$IFDEF KS_COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}, D);
    if Assigned(D) then
    begin
      {$IFDEF KS_COMPILER6_UP}
      List := CreateSelectionList;
      D.GetSelections(List);
      if List.Count > 0 then
        for i := 0 to List.Count - 1 do
          if List.Items[i] = AObject then
          begin
            Result := true;
            List := nil;
            Exit;
          end;
      List := nil;
      {$ENDIF}
    end;
  end;
  Result := false;
end;

function TvgIDEDesigner.UniqueName(ADesigner: TComponent;
  ClassName: string): string;
var
  D: IDesigner;
begin
  Result := '';
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    TCustomForm(ADesigner).Designer.QueryInterface(IDesigner, D);
    if Assigned(D) then
    begin
      if (ClassName <> '') and (Pos('Tvg', ClassName) = 1) then
        Delete(ClassName, 1, 3);
      Result := D.UniqueName('T' + ClassName);
    end;
  end;
end;

{$ELSE}

procedure TvgIDEDesigner.Modified(ADesigner: TComponent);
begin
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    if TCustomForm(ADesigner).Designer <> nil then
    begin
      TCustomForm(ADesigner).Designer.Modified;
    end;
  end;
end;

procedure TvgIDEDesigner.SelectObject(ADesigner: TComponent; AObject: TvgObject; MultiSelection: array of TvgObject);
begin
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    if TCustomForm(ADesigner).Designer <> nil then
    begin
      TCustomForm(ADesigner).Designer.SelectOnlyThisComponent(AObject);
    end;
  end;
end;

function TvgIDEDesigner.IsSelected(ADesigner: TComponent; const AObject: TObject): boolean;
begin
  Result := false;
end;

function TvgIDEDesigner.UniqueName(ADesigner: TComponent;
  ClassName: string): string;
begin
  Result := '';
  if (ADesigner is TCustomForm) and (TCustomform(ADesigner).Designer <> nil) then
  begin
    if TCustomForm(ADesigner).Designer <> nil then
    begin
      if (ClassName <> '') and (Pos('Tvg', ClassName) = 1) then
        Delete(ClassName, 1, 3);
      Result := TCustomForm(ADesigner).Designer.UniqueName('T' + ClassName);
    end;
  end;
end;


{$ENDIF}

procedure TvgIDEDesigner.EditStyle(const Res: TvgResources; const ASelected: string);
begin
  inherited;
  DesignResources(Res, ASelected);
end;

{ TvgBrushProperty }

procedure TvgBrushProperty.Edit;
begin
  selectInDesign(TvgBrush(GetOrdValue), GetComponent(0));
end;

function TvgBrushProperty.GetValue: String;
begin
  Result := '(Brush)';
end;

function TvgBrushProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TvgBitmapProperty }

procedure TvgBitmapProperty.Edit;
begin
  vgBitmapEditor := TvgBitmapEditor.Create(nil);
  vgBitmapEditor.AssignFromBitmap(TvgBitmap(GetOrdValue));
  if vgBitmapEditor.ShowModal = mrOk then
  begin
    vgBitmapEditor.AssignToBitmap(TvgBitmap(GetOrdValue));
    Modified;
  end;
  vgBitmapEditor.Free;
end;

function TvgBitmapProperty.GetValue: String;
begin
  Result := '(Bitmap)';
end;

function TvgBitmapProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

{ TvgPathDataProperty }

procedure TvgPathDataProperty.Edit;
var
  S: string;
  EditDlg: TvgPathDataDesigner;
begin
  EditDlg := TvgPathDataDesigner.Create(Application);
  EditDlg.PathData.Lines.Text := TvgPathData(GetOrdValue).Data;
  EditDlg.PathData.WordWrap := true;
  if EditDlg.ShowModal = mrOk then
  begin
    TvgPathData(GetOrdValue).Data := EditDlg.PathData.Lines.Text;
    if GetComponent(0) is TvgVisualObject then
      TvgVisualObject(GetComponent(0)).Repaint;
    Modified;
  end;
  EditDlg.Free;
end;

function TvgPathDataProperty.GetValue: String;
begin
  Result := '(PathData)';
end;

function TvgPathDataProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

{ TvgResourceProperty }

procedure TvgResourceProperty.Edit;
begin
  if DesignResources(TvgResources(GetComponent(0)), '') then
    Modified;
end;

function TvgResourceProperty.GetValue: String;
begin
  Result := '(Resource)';
end;

function TvgResourceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TvgWideStringsProperty }

procedure TvgWideStringsProperty.Edit;
var
  EditDlg: TvgPathDataDesigner;
  SaveWrap: boolean;
begin
  EditDlg := TvgPathDataDesigner.Create(Application);
  EditDlg.Caption := 'WideStrings Editor';
  EditDlg.PathData.WordWrap := false;
  if GetComponent(0) is TvgMemo then
  begin
    EditDlg.PathData.WordWrap := false;
    SaveWrap := TvgMemo(GetComponent(0)).WordWrap;
    TvgMemo(GetComponent(0)).WordWrap := false;
    EditDlg.PathData.Lines.Assign(TvgWideStrings(GetOrdValue));
    TvgMemo(GetComponent(0)).WordWrap := SaveWrap;
  end
  else
  begin
    EditDlg.PathData.WordWrap := true;
    EditDlg.PathData.Lines.Assign(TvgWideStrings(GetOrdValue));
  end;
  EditDlg.previewLayout.Visible := false;
  EditDlg.labelMemo.Text := 'Type items:';
  if EditDlg.ShowModal = mrOk then
  begin
    TvgWideStrings(GetOrdValue).Text := EditDlg.PathData.Lines.Text;
    if GetComponent(0) is TvgVisualObject then
      TvgVisualObject(GetComponent(0)).Repaint;
    Modified;
  end;
  EditDlg.Free;
end;

function TvgWideStringsProperty.GetValue: String;
begin
  Result := '(WideStrings)';
end;

function TvgWideStringsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TvgFontProperty }

procedure TvgFontProperty.Edit;
var
  F: TFontDialog;
begin
  F := TFontDialog.Create(Application);
  F.Font.Assign(TvgFont(GetOrdValue));
  if F.Execute then
  begin
    TvgFont(GetOrdValue).Assign(F.Font);
    Modified;
  end;
  F.Free;
end;

function TvgFontProperty.GetValue: String;
begin
  Result := '(Font)';
end;

function TvgFontProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TvgColorProperty ============================================================}

{ TvgColorProperty }

procedure TvgColorProperty.Edit;
var
  D: TvgBrushDialog;
begin
  D := TvgBrushDialog.Create(Application);
  D.ShowStyles := [vgBrushSolid];
  D.ShowBrushList := false;
  D.Brush.Color := GetStrValue;
  if D.Execute then
  begin
    SetStrValue(D.Brush.Color);
    Modified;
  end;
  D.Free;
end;

function TvgColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect, paRevertable];
end;

function TvgColorProperty.GetValue: string;
begin
  try
    Result := GetStrValue;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TvgColorProperty.SetValue(const Value: string);
begin
  try
    SetStrValue(Value);
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{ TvgGradientProperty ============================================================}

{ TvgGradientProperty }

procedure TvgGradientProperty.Edit;
var
  D: TvgBrushDialog;
begin
  D := TvgBrushDialog.Create(Application);
  D.ShowStyles := [vgBrushGradient];
  D.ShowBrushList := false;
  D.Brush.Style := vgBrushGradient;
  D.Brush.Gradient.Assign(TvgGradient(GetOrdValue));
  if D.Execute then
  begin
    TvgGradient(GetOrdValue).Assign(D.Brush.Gradient);
    Modified;
  end;
  D.Free;
end;

function TvgGradientProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TvgGradientProperty.GetValue: string;
begin
  Result := '(Gradient)';
end;

procedure TvgGradientProperty.SetValue(const Value: string);
begin
  try
    SetStrValue(Value);
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{ TvgTriggerProperty ============================================================}

{ TvgTriggerProperty }

function TvgTriggerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TvgTriggerProperty.GetValues(Proc: TGetStrProc);
begin
  try
    Proc('IsMouseOver=true');
    Proc('IsMouseOver=false');
    Proc('IsFocused=true');
    Proc('IsFocused=false');
    Proc('IsVisible=true');
    Proc('IsVisible=false');
    Proc('IsDragOver=true');
    Proc('IsDragOver=false');
    Proc('IsOpen=true');
    Proc('IsOpen=false');
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TvgTriggerProperty.GetValue: string;
begin
  try
    Result := GetStrValue;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TvgTriggerProperty.SetValue(const Value: string);
begin
  try
    SetStrValue(Value);
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{ TvgLangProperty ============================================================}

{ TvgLangProperty }

function TvgLangProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TvgLangProperty.Edit;
begin
  ShowDsgnLang(TvgLang(GetComponent(0)));
end;

{ TvgImgListEditor ============================================================}

{ TvgImgListEditor }

procedure TvgImgListEditor.Edit;
begin
  ShowDsgnImageList(TvgImageList(Component));
end;

{ TvgLangEditor ============================================================}

{ TvgLangEditor }

procedure TvgLangEditor.Edit;
begin
  ShowDsgnLang(TvgLang(Component));
end;

{ Register Routines ===========================================================}

procedure Register;
begin
  RegisterComponents('VGScene', [TvgScene, TvgResources, TvgLang, TvgImageList, TvgBrushDialog]);
  RegisterNoIcon([TvgRectAnimation, 
    TvgBitmapListAnimation, TvgGradientAnimation, TvgFloatKeyAnimation, TvgPathSwitcher, TvgColorKeyAnimation, TvgPathAnimation, 
    TvgColorAnimation, TvgFloatAnimation, TvgBitmapAnimation, TvgCalendarBox, TvgGroupBox, TvgPathCheckBox, 
    TvgCheckBox, TvgRadioButton, TvgPopupBox, TvgPopupButton, TvgCornerButton, TvgCircleButton, 
    TvgAngleButton, TvgButton, TvgPathButton, TvgColorButton, TvgBitmapStateButton, TvgSpeedButton, 
    TvgRoundButton, TvgBitmapButton, TvgColorPanel, TvgComboColorBox, TvgColorQuad, TvgGradientEdit, 
    TvgAlphaTrackBar, TvgBWTrackBar, TvgHueTrackBar, TvgColorBox, TvgColorPicker, TvgCompoundTrackBar, 
    TvgCalendar, TvgCompoundNumberBox, TvgCompoundTextBox, TvgCompoundPopupBox, TvgCompoundImage, TvgCompoundMemo, 
    TvgCompoundColorButton, TvgCompoundAngleBar, TvgProgressBar, TvgCalloutPanel, TvgVertScrollBox, TvgTrackBar, 
    TvgFramedScrollBox, TvgScrollBar, TvgScrollBox, TvgValueLabel, TvgAniIndicator, TvgTrack, 
    TvgSplitter, TvgExpander, TvgImageControl, TvgLabel, TvgFramedVertScrollBox, TvgSmallScrollBar, 
    TvgPanel, TvgTabControl, TvgDBLabel, TvgDBTextBox, TvgDBNavigator, TvgDBMemo, 
    TvgDBImage, TvgDBGrid, TvgDesignFrame, TvgSelection, TvgSelectionPoint, TvgInspector, 
    TvgReflectionEffect, TvgInnerGlowEffect, TvgBlurEffect, TvgShadowEffect, TvgGlowEffect, TvgBevelEffect, 
    TvgImageViewer, TvgDropTarget, TvgIPhoneButton, TvgHeader, TvgStringGrid, TvgGrid, 
    TvgProgressColumn, TvgImageColumn, TvgColumn, TvgPopupColumn, TvgCheckColumn, TvgHudNumberBox, 
    TvgHudScrollBar, TvgHudStringComboBox, TvgHudAlphaTrackBar, TvgHudCloseButton, TvgHudCornerButton, TvgHudSizeGrip, 
    TvgHudRadioButton, TvgHudComboColorBox, TvgHudMemo, TvgHudStringListBox, TvgHudTrack, TvgHudHorzImageListBox, 
    TvgHudSpeedButton, TvgHudWindow, TvgHudListBox, TvgHudHorzListBox, TvgHudRoundTextBox, TvgHudLabel, 
    TvgHudCircleButton, TvgHudGroupBox, TvgHudStatusBar, TvgHudPanel, TvgHudBWTrackBar, TvgHudHueTrackBar, 
    TvgHudComboTrackBar, TvgHudImageListBox, TvgHudSpinBox, TvgHudComboBox, TvgHudAngleButton, TvgHudButton, 
    TvgHudRoundButton, TvgHudCheckBox, TvgHudPopupBox, TvgHudTextBox, TvgHudComboTextBox, TvgHudTrackBar, 
    TvgHudTabControl, TvgHeaderItem, TvgImageListBoxItem, TvgHudTabItem, TvgTabItem, TvgTreeViewItem, 
    TvgListBoxItem, TvgGridLayout, TvgFrame, TvgLayout, TvgSplitLayout, TvgScaledLayout, 
    TvgNonVGLayout, TvgStringComboBox, TvgHorzImageListBox, TvgStringListBox, TvgComboBox, TvgHorzListBox, 
    TvgImageListBox, TvgListBox, TvgPlotGrid, TvgPopup, TvgMessagePopup, TvgBitmapObject, 
    TvgPathObject, TvgBrushObject, TvgPie, TvgScrollArrowRight, TvgArc, TvgPath, 
    TvgPaintBox, TvgCircle, TvgBlurRoundRect, TvgBlurRectangle, TvgRectangle, TvgScrollArrowLeft, 
    TvgLine, TvgCalloutRectangle, TvgText, TvgImage, TvgSidesRectangle, TvgEllipse, 
    TvgRoundRect, TvgTextBoxClearBtn, TvgTextBox, TvgComboTextBox, TvgNumberBox, TvgRoundTextBox, 
    TvgMemo, TvgCalendarTextBox, TvgSpinBox, TvgComboTrackBar, TvgStatusBar, TvgToolButton, 
    TvgToolPathButton, TvgToolBar, TvgTreeView, TvgSizeGrip, TvgBackground, TvgCloseButton,
    TvgPopupItem
  ]);
  RegisterComponentEditor(TvgImageList, TvgImgListEditor);
  RegisterComponentEditor(TvgLang, TvgLangEditor);
  RegisterPropertyEditor(TypeInfo(String), TvgLang, 'Lang', TvgLangProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgEffect, 'Trigger', TvgTriggerProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgAnimation, 'Trigger', TvgTriggerProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgAnimation, 'TriggerInverse', TvgTriggerProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgBrush, 'Color', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgColorKey, 'Value', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgGlowEffect, 'GlowColor', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgShadowEffect, 'ShadowColor', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgColorAnimation, 'StartValue', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgColorAnimation, 'StopValue', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgColorKey, 'Value', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgColorButton, 'Color', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgCompoundColorButton, 'Value', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgColorPanel, 'Color', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgComboColorBox, 'Color', TvgColorProperty);
  RegisterPropertyEditor(TypeInfo(TvgGradient), nil, '', TvgGradientProperty);
  RegisterPropertyEditor(TypeInfo(TvgBrush), nil, '', TvgBrushProperty);
  RegisterPropertyEditor(TypeInfo(TvgBitmap), nil, '', TvgBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TvgPathData), nil, '', TvgPathDataProperty);
  RegisterPropertyEditor(TypeInfo(TvgWideStrings), nil, '', TvgWideStringsProperty);
  RegisterPropertyEditor(TypeInfo(TvgFont), nil, '', TvgFontProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TvgResources, 'Resource', TvgResourceProperty);
end;

initialization
  vgDesigner := TvgIDEDesigner.Create(nil);
  {$IFDEF KS_COMPILER6_UP}
  RegisterDesignNotification(TvgIDEDesigner(vgDesigner));
  {$ENDIF}
finalization
  {$IFDEF KS_COMPILER6_UP}
  UnregisterDesignNotification(TvgIDEDesigner(vgDesigner));
  {$ENDIF}
  vgDesigner.Free;
  vgDesigner := nil;
end.

