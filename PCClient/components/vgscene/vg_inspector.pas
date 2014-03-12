unit vg_inspector;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFDEF LCL} LCLType, {$ENDIF}
  Controls, Dialogs,
  Classes, SysUtils,
  vg_scene, vg_objects, vg_controls, vg_treeview, vg_textbox;

type

  TvgOnChangeProperty = procedure (Sender: TObject; PropertyName: string) of object;

  TvgInspector = class(TvgTreeView)
  private
    FSelectedObject: TComponent;
    FEditBox: TvgTextBox;
    FComboBox: TvgPopupBox;
    FEditButton: TvgButton;
    FDisabledProperties: TStrings;
    FShowProperties: boolean;
    FShowEvents: boolean;
    FOnChangeProperty: TvgOnChangeProperty;
    procedure RebuildList;
    procedure RebuildEditor;
    procedure SetSelectedObject(const Value: TComponent);
    procedure SetDisabledProperties(const Value: TStrings);
    procedure SetShowEvents(const Value: boolean);
    procedure SetShowProperties(const Value: boolean);
    procedure UpdateEditorPos;
  protected
    function Editor: TvgControl;
    procedure InsAddObject(ItemRoot: TvgObject; Root: TObject);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
    procedure SetSelected(const Value: TvgTreeViewItem); override;
    procedure VScrollChange(Sender: TObject); override;
    procedure DoEditorChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DisabledProperties: TStrings read FDisabledProperties write SetDisabledProperties;
    property SelectedObject: TComponent read FSelectedObject write SetSelectedObject;
    property ShowProperties: boolean read FShowProperties write SetShowProperties default true;
    property ShowEvents: boolean read FShowEvents write SetShowEvents default false;
    property OnChangeProperty: TvgOnChangeProperty read FOnChangeProperty write FOnChangeProperty;
  end;

implementation {===============================================================}

uses TypInfo, vg_dsgn, vg_dsgn_bmp, vg_dsgn_path;

{ TvgInspector }

constructor TvgInspector.Create(AOwner: TComponent);
begin
  inherited;
  FShowProperties := true;
  FShowEvents := false;
  FResource := 'treeviewstyle';
  HideSelectionUnfocused := false;
  FDisabledProperties := TStringList.Create;
  FEditButton := TvgButton.Create(Self);
  FEditButton.ResourceName := 'inspectorEditor';
  FEditButton.TagString := 'self';
  FEditButton.DisableFocusEffect := true;
  FEditButton.Visible := false;
  FEditButton.Stored := false;
  FEditButton.Parent := Self;
  FEditButton.OnClick := DoEditorChange;
  FEditButton.Visible := false;
  FEditButton.Position.X := 2000;
  FEditBox := TvgTextBox.Create(Self);
  FEditBox.ResourceName := 'inspectorEditor';
  FEditBox.TagString := 'self';
  FEditBox.DisableFocusEffect := true;
  FEditBox.Padding.Rect := vgRect(0, 0, 0, 0);
  FEditBox.Stored := false;
  FEditBox.Parent := Self;
  FEditBox.OnChange := DoEditorChange;
  FEditBox.Visible := false;
  FEditBox.Position.X := 2000;
  FComboBox := TvgPopupBox.Create(Self);
  FComboBox.ResourceName := 'inspectorEditor';
  FComboBox.TagString := 'self';
  FComboBox.DisableFocusEffect := true;
  FComboBox.Stored := false;
  FComboBox.Parent := Self;
  FComboBox.TextAlign := vgTextAlignNear;
  FComboBox.OnChange := DoEditorChange;
  FComboBox.Visible := false;
  FcomboBox.Position.X := 2000;
end;

destructor TvgInspector.Destroy;
begin
  if (FSelectedObject <> nil) and (FSelectedObject is TvgObject) then
    TvgObject(FSelectedObject).RemoveFreeNotify(Self);
  FDisabledProperties.Free;
  inherited;
end;

procedure TvgInspector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSelectedObject) then
  begin
    SelectedObject := nil;
  end;
end;

procedure TvgInspector.InsAddObject(ItemRoot: TvgObject; Root: TObject);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  i, j, PropCount: integer;
  Obj: TObject;
  Item: TvgTreeViewItem;
  Value: TvgLabel;
  Cls: TClass;
begin
  if Root = nil then Exit;
  {$IFDEF KS_COMPILER5}
  PropCount := GetPropList(PTypeInfo(Root.ClassInfo), [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray], nil);
  GetMem(PropList, SizeOf(PPropInfo) * PropCount);
  PropCount := GetPropList(PTypeInfo(Root.ClassInfo), [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray], PropList);
  {$ELSE}
  PropCount := GetPropList(PTypeInfo(Root.ClassInfo), PropList);
  {$ENDIF}
  if PropCount = 0 then Exit;

  try
    {$IFDEF KS_COMPILER6_UP}
    SortPropList(PropList, PropCount);
    {$ENDIF}
    for i := 0 to PropCount - 1 do
    begin
      with PropList[i]^ do
      begin
        if FDisabledProperties.IndexOf(Name) >= 0 then Continue;
        PropInfo := GetPropInfo(Root, Name);
        if PropInfo = nil then Continue;
        if PropInfo^.SetProc = nil then Continue;

        if (PropType^.Kind in [tkMethod]) and ((not FShowEvents) or (Root <> FSelectedObject)) then Continue;
        if not (PropType^.Kind in [tkMethod]) and (not FShowProperties) then Continue;

        Item := TvgTreeViewItem.Create(Self);
        Item.Parent := ItemRoot;
        Item.Text := Name;
        Item.Locked := true;
        Item.Stored := false;
        Value := TvgLabel.Create(Self);
        Value.ResourceName := 'inspectorEditor';
        Value.Locked := true;
        Value.Stored := false;
        Value.Parent := Item;
        Value.SetBounds(Width / 2, 0, Width, Item.Height + 2);
        Value.TextAlign := vgTextAlignNear;
        Value.WordWrap := false;
        Item.TagObject := Root;
        Item.TagString := Name;
        Item.Tag := Integer(Value);
        case PropType^.Kind of
          tkInteger:
            begin
              Value.Text := IntToStr(GetOrdProp(Root, Name));
            end;
          tkFloat:
            begin
              Value.Text := FloatToStr(GetFloatProp(Root, Name));
            end;
          tkClass:
            begin
              Obj := GetObjectProp(Root, Name);
              if (Obj is TvgObject) then
              begin
                TvgLabel(Value).Text := TComponent(Obj).Name;
                if TComponent(Obj).Name = '' then
                  TvgLabel(Value).Text := TvgObject(Obj).ResourceName;
              end
              else
              if Obj <> nil then
              begin
                Item.IsExpanded := false;
                TvgLabel(Value).Text := Obj.ClassName;
                if not (Obj is TvgObject) then
                  InsAddObject(Item, Obj);
              end;
            end;
           {$IFDEF KS_COMPILER11}tkUString, {$ENDIF}tkWString:
            begin
              {$IFDEF KS_COMPILER6_UP}
              Value.Text := GetWideStrProp(Root, Name);
              {$ELSE}
              Value.Text := GetStrProp(Root, Name);
              {$ENDIF}
            end;
          tkString, tkLString:
            begin
              Value.Text := GetStrProp(Root, Name);
            end;
          tkMethod:
            begin
              Value.Text := GetStrProp(Root, Name);
            end;
          tkEnumeration:
            begin
              Value.Text := GetEnumProp(Root, Name);
            end;
          {$IFDEF FPC}
          tkBool:
            begin
              Value.Text := GetEnumProp(Root, Name);
            end;
          {$ENDIF}
          else
            Value.Text := GetStrProp(Root, Name);
        end;
      end;
    end;
  finally
    FreeMem(PropList, SizeOf(PPropInfo) * PropCount);
  end;
end;

procedure TvgInspector.RebuildList;
begin
  BeginUpdate;
  try
    FEditButton.TagObject := nil;
    FEditBox.TagObject := nil;
    FComboBox.TagObject := nil;
    Clear;
    if FSelectedObject <> nil then
      InsAddObject(Self, FSelectedObject);
  finally
    EndUpdate;
  end;
end;

procedure TvgInspector.RebuildEditor;
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;
  Editor: TvgControl;
  L: TList;
  i: integer;
  Obj: TObject;
  M: TMethod;
begin
  FEditButton.Visible := false;
  FEditBox.Visible := false;
  FComboBox.Visible := false;
  if (Selected = nil) or (Selected.TagObject = nil) then Exit;

  PropInfo := GetPropInfo(Selected.TagObject, Selected.TagString);
  if PropInfo = nil then Exit;

  TypeData := GetTypeData(PropInfo.PropType{$IFNDEF FPC}^{$ENDIF});
  if TypeData = nil then Exit;

  if FEditButton.TagObject <> nil then
    TvgVisualObject(FEditButton.TagObject).Visible := true;
  if FEditBox.TagObject <> nil then
    TvgVisualObject(FEditBox.TagObject).Visible := true;
  if FComboBox.TagObject <> nil then
    TvgVisualObject(FComboBox.TagObject).Visible := true;
  with PropInfo^ do
  begin
    case PropType^.Kind of
      tkMethod:
        begin
          M := GetMethodProp(Selected.TagObject, Selected.TagString);
          if vgDesigner <> nil then
            FEditBox.Text := vgDesigner.GetMethodName(M);
          FEditBox.SelectAll;
          Editor := FEditBox;
        end;
      tkClass:
        begin
          Obj := GetObjectProp(Selected.TagObject, Selected.TagString);
          if (Obj is TvgGradient) or (Obj is TvgBrush) or (Obj is TvgBitmap) or (Obj is TvgPathData) or (Obj is TvgFont) then
          begin
            FEditButton.Text := 'Edit...';
            Editor := FEditButton;
          end
          else
            Editor := nil;
        end;
      tkInteger:
        begin
          FEditBox.Text := IntToStr(GetOrdProp(Selected.TagObject, Selected.TagString));
          Editor := FEditBox;
        end;
      tkFloat:
        begin
          FEditBox.Text := FloatToStr(GetFloatProp(Selected.TagObject, Selected.TagString));
          Editor := FEditBox;
        end;
      tkWString:
        begin
          {$IFDEF KS_COMPILER6_UP}
          FEditBox.Text := GetWideStrProp(Selected.TagObject, Selected.TagString);
          {$ELSE}
          FEditBox.Text := GetStrProp(Selected.TagObject, Selected.TagString);
          {$ENDIF}
          Editor := FEditBox;
        end;
      tkString, tkLString:
        begin
          FEditBox.Text := GetStrProp(Selected.TagObject, Selected.TagString);
          FEditBox.SelectAll;
          Editor := FEditBox;
        end;
      tkEnumeration:
        begin
          FComboBox.Items.Clear;
          FComboBox.ItemIndex := -1;
          for i := TypeData.MinValue to TypeData.MaxValue do
            FComboBox.Items.Add(GetEnumName(PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}, i));
          FComboBox.ItemIndex := GetOrdProp(Selected.TagObject, Selected.TagString);
          Editor := FComboBox;
        end;
      {$IFDEF FPC}
      tkBool:
        begin
          FComboBox.Items.Clear;
          for i := TypeData.MinValue to TypeData.MaxValue do
            FComboBox.Items.Add(GetEnumName(PropInfo.PropType{$IFNDEF FPC}^{$ENDIF}, i));
          FComboBox.ItemIndex := GetOrdProp(Selected.TagObject, Selected.TagString);
          Editor := FComboBox;
        end;
      {$ENDIF}
    else
      FEditBox.Text := GetStrProp(Selected.TagObject, Selected.TagString);
      Editor := FEditBox;
    end;
  end;
  if Editor <> nil then
  begin
    Editor.Visible := true;
    Editor.Locked := true;
    Editor.Stored := false;
    Editor.TagObject := TObject(Selected.Tag);
    if Editor.TagObject <> nil then
      TvgVisualObject(Editor.TagObject).Visible := false;
    UpdateEditorPos;
    if Editor.Visible then
      Editor.SetFocus;
  end;
end;

procedure TvgInspector.DoEditorChange(Sender: TObject);
var
  Obj: TObject;
  F: TFontDialog;
  S: string;
  EditDlg: TvgPathDataDesigner;
  i: integer;
  L: TList;
  PropInfo: PPropInfo;
  M: TMethod;
begin
  if FUpdating > 0 then Exit;

  if (Selected <> nil) and (Selected.TagObject <> nil) then
  begin
    if (Sender = FEditButton) and FEditButton.Visible then
    begin
      { Designer }
      Obj := GetObjectProp(Selected.TagObject, Selected.TagString);
      if Obj is TvgBrush then
      begin
        SelectInDesign(Obj, TPersistent(SelectedObject));
      end;
      if Obj is TvgGradient then
      begin
        ShowGradientDialog(TvgGradient(Obj));
      end;
      if Obj is TvgBitmap then
      begin
        vgBitmapEditor := TvgBitmapEditor.Create(nil);
        vgBitmapEditor.AssignFromBitmap(TvgBitmap(Obj));
        if vgBitmapEditor.ShowModal = mrOk then
        begin
          vgBitmapEditor.AssignToBitmap(TvgBitmap(Obj));
        end;
        vgBitmapEditor.Free;
      end;
      if Obj is TvgPathData then
      begin
        EditDlg := TvgPathDataDesigner.Create(Self);
        EditDlg.PathData.Lines.Text := TvgPathData(Obj).Data;
        if EditDlg.ShowModal = mrOk then
        begin
          TvgPathData(Obj).Data := EditDlg.PathData.Lines.Text;
        end;
        EditDlg.Free;
      end;
      if Obj is TvgFont then
      begin
        F := TFontDialog.Create(nil);
        F.Font.Assign(TvgFont(Obj));
        if F.Execute then
        begin
          TvgFont(Obj).Assign(F.Font);
        end;
        F.Free;
      end;
    end;
    if (Sender = FEditBox) and FEditBox.Visible then
    begin
      PropInfo := GetPropInfo(Selected.TagObject, Selected.TagString);
      if PropInfo^.PropType^.Kind = tkMethod then
      begin
        if vgDesigner <> nil then
        begin
          M := vgDesigner.AddMethod(TvgTextBox(Sender).Text);
          SetMethodProp(Selected.TagObject, PropInfo, M);
          if (M.Code <> nil) and (TvgObject(Sender).TagObject <> nil) then
            TvgLabel(TvgObject(Sender).TagObject).Text := TvgTextBox(Sender).Text;
        end;
      end
      else
      begin
        SetPropValue(Selected.TagObject, Selected.TagString, TvgTextBox(Sender).Text);
        if TvgObject(Sender).TagObject <> nil then
          TvgLabel(TvgObject(Sender).TagObject).Text := TvgTextBox(Sender).Text;
      end;
    end;
    if (Sender = FComboBox) and FComboBox.Visible then
    begin
      SetPropValue(Selected.TagObject, Selected.TagString, TvgPopupBox(Sender).ItemIndex);
      if TvgObject(Sender).TagObject <> nil then
        TvgLabel(TvgObject(Sender).TagObject).Text := TvgPopupBox(Sender).Text;
    end;
    if Assigned(FOnChangeProperty) then
      FOnChangeProperty(Self, Selected.TagString);
  end;
end;

procedure TvgInspector.SetSelectedObject(const Value: TComponent);
begin
  if FSelectedObject <> Value then
  begin
    if (Editor <> nil) and (Editor.Visible) and (Editor <> FEditButton) then
      DoEditorChange(Editor);
    if (FSelectedObject <> nil) and (FSelectedObject is TvgObject) then
      TvgObject(FSelectedObject).RemoveFreeNotify(Self);
    FSelectedObject := Value;
    if (FSelectedObject <> nil) and (FSelectedObject is TvgObject) then
      TvgObject(FSelectedObject).AddFreeNotify(Self);
    RebuildList;
    RebuildEditor;
  end;
end;

procedure TvgInspector.SetSelected(const Value: TvgTreeViewItem);
begin
  if Value <> Selected then
  begin
    if (Editor <> nil) and (Editor.Visible) and (Editor <> FEditButton) then
      DoEditorChange(Editor);
    inherited;
    RebuildEditor;
  end;
end;

procedure TvgInspector.SetDisabledProperties(const Value: TStrings);
begin
  FDisabledProperties.Assign(Value);
end;

function TvgInspector.Editor: TvgControl;
begin
  if FComboBox.Visible then
    Result := FComboBox
  else
  if FEditBox.Visible then
    Result := FEditBox
  else
    Result := FEditButton;
end;

procedure TvgInspector.VScrollChange(Sender: TObject);
begin
  inherited;
  UpdateEditorPos;
end;

procedure TvgInspector.UpdateEditorPos;
var
  R: TvgRect;
begin
  if (Editor <> nil) and (Selected <> nil) and (ContentLayout <> nil) then
  begin
    R := GetItemRect(Selected);
    R.TopLeft := FContent.LocalToAbsolute(R.TopLeft);
    R.BottomRight := FContent.LocalToAbsolute(R.BottomRight);
    R.TopLeft := AbsoluteToLocal(R.TopLeft);
    R.BottomRight := AbsoluteToLocal(R.BottomRight);
    with R do
    begin
      Editor.SetBounds(Width / 2 + (Selected.Level * Selected.Height), Top + 1,
        ContentLayout.Width - (Width / 2) - FContent.Position.X + ContentLayout.Position.X - (Selected.Level * Selected.Height) - 1,
        Selected.Height - 2);
    end;
    if Editor.Position.Y < 0 then
      Editor.Opacity := 0
    else
    if Editor.Position.Y + Editor.Height > Height then
      Editor.Opacity := 0
    else
      Editor.Opacity := 1
  end;
end;

procedure TvgInspector.DialogKey(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Count > 0) and (Selected <> nil) and (Scene <> nil) and
     (Scene.GetFocused = Editor) then
  begin
    case Key of
      VK_ADD: Selected.IsExpanded := true;
      VK_SUBTRACT: Selected.IsExpanded := false;
      VK_HOME:
        if Editor <> FEditBox then
          Selected := ItemByGlobalIndex(0)
        else
          Exit;
      VK_END:
        if Editor <> FEditBox then
          Selected := ItemByGlobalIndex(GlobalCount - 1)
        else
          Exit;
      VK_UP:
        if Selected.GlobalIndex > 0 then
          Selected := ItemByGlobalIndex(Selected.GlobalIndex - 1);
      VK_DOWN:
        if Selected.GlobalIndex < GlobalCount - 1 then
          Selected := ItemByGlobalIndex(Selected.GlobalIndex + 1);
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgInspector.SetShowEvents(const Value: boolean);
begin
  if FShowEvents <> Value then
  begin
    FShowEvents := Value;
    RebuildList
  end;
end;

procedure TvgInspector.SetShowProperties(const Value: boolean);
begin
  if FShowProperties <> Value then
  begin
    FShowProperties := Value;
    RebuildList
  end;
end;

initialization
  RegisterVGObjects('Design', [TvgInspector]);
end.

