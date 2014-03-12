unit vg_dsgn_styles;

interface

uses
  {$IFDEF WIN32}
  Registry,
  {$ENDIF}
  {$IFNDEF FPC}
  Windows,
  {$ELSE}
  LCLType,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, TypInfo,
  Dialogs, vg_scene, vg_layouts, vg_inspector, vg_treeview, vg_controls,
  vg_objects, vg_dsgn, ExtCtrls, StdCtrls, vg_listbox, vg_textbox;

type
  TvgStyleDesigner = class(TForm)
    vgScene1: TvgScene;
    Root1: TvgBackground;
    ObjectsTree: TvgTreeView;
    vgScene2: TvgScene;
    Root2: TvgBackground;
    Inspector: TvgInspector;
    DesignScene: TvgScene;
    vgScene3: TvgScene;
    Root4: TvgBackground;
    OpenDialog1: TOpenDialog;
    Button1: TvgButton;
    SaveDialog1: TSaveDialog;
    Button2: TvgButton;
    btnClear: TvgButton;
    btnLoadDefault: TvgButton;
    btnBack: TvgButton;
    rectBack: TvgRectangle;
    vgBrushDialog1: TvgBrushDialog;
    Button3: TvgButton;
    Button4: TvgButton;
    btnCancel: TvgButton;
    ClearTimer: TTimer;
    Resources1: TvgResources;
    textFilter: TvgTextBoxClearBtn;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    vgScene4: TvgScene;
    Root3: TvgBackground;
    StatusBar1: TvgStatusBar;
    Label1: TvgLabel;
    procedure ObjectsTreeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure ObjectsTreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ClearTimerTimer(Sender: TObject);
    procedure ObjectsTreeDragChange(SourceItem, DestItem: TvgTreeViewItem;
      Allow: Boolean);
    procedure InspectorChangeProperty(Sender: TObject;
      PropertyName: String);
    procedure ObjectsTreeChangeCheck(Sender: TObject);
    procedure DesignRootDragOver(Sender: TObject;
      const Data: TvgDragObject; const Point: TvgPoint;
      var Accept: Boolean);
    procedure textFilterChangeTracking(Sender: TObject);
  private
    { Private declarations }
    FDragObj: TvgObject;
    FResource: TvgResources;
    procedure UpdateTree;
    procedure LoadFromStrings(Str: TStrings);
    procedure SaveToStrings(Str: TStrings);
    procedure DoDeleteButton(Sender: TObject);
    procedure DoVisCheck(Sender: TObject);
    procedure DeleteObject(AObject: TvgObject; FreeObject: boolean);
    procedure TreeItemApplyResource(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
  end;

var
  vgStyleDesigner: TvgStyleDesigner;

function DesignResources(AResource: TvgResources; Current: string): boolean;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

type

  TvgHackVisual = class(TvgVisualObject);
  TvgHackBackground = class(TvgBackground);

  TvgStyleIDEDesigner = class(TvgDesigner)
  private
  public
    procedure SelectObject(ADesigner: TComponent; AObject: TvgObject; MultiSelection: array of TvgObject); override;
    procedure Modified(ADesigner: TComponent); override;
    function UniqueName(ADesigner: TComponent; ClassName: string): string; override;
    function IsSelected(ADesigner: TComponent; const AObject: TObject): boolean; override;
    procedure AddObject(AObject: TvgObject); override;
    procedure DeleteObject(AObject: TvgObject); override;
  end;


{ TvgStyleIDEDesigner }

procedure TvgStyleIDEDesigner.Modified(ADesigner: TComponent);
begin
end;

function TvgStyleIDEDesigner.IsSelected(ADesigner: TComponent; const AObject: TObject): boolean;
begin
  Result := false;
end;

procedure TvgStyleIDEDesigner.SelectObject(ADesigner: TComponent; AObject: TvgObject; MultiSelection: array of TvgObject);
begin
  if AObject = vgStyleDesigner.DesignScene.Root then Exit;

  vgStyleDesigner.Inspector.SelectedObject := AObject;
  if vgStyleDesigner.Inspector.SelectedObject is TvgObject then
    TvgObject(vgStyleDesigner.Inspector.SelectedObject).AddFreeNotify(vgStyleDesigner);
  if AObject.TagObject <> nil then
  begin
    vgStyleDesigner.ObjectsTree.Selected := TvgTreeViewItem(AObject.TagObject);
  end;
end;

function TvgStyleIDEDesigner.UniqueName(ADesigner: TComponent;
  ClassName: string): string;
begin
  Result := '';
end;

function DesignResources(AResource: TvgResources; Current: string): boolean;
var
  SaveDesigner: TvgDesigner;
  S: string;
begin
  Result := false;
  if AResource <> nil then
  begin
    SaveDesigner := vgDesigner;
    vgDesigner := TvgStyleIDEDesigner.Create(Application);

    vgStyleDesigner := TvgStyleDesigner.Create(Application);
    {$IFDEF WIN32}
    with TRegistry.Create do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\ksdev\vgstyledesigner', true) then
      begin
        if ReadString('position') <> '' then
        begin
          vgStyleDesigner.Position := poDesigned;
          with vgStringToRect(ReadString('position')) do
          begin
            vgStyleDesigner.Left := round(left);
            vgStyleDesigner.Top := round(top);
            vgStyleDesigner.Width := round(right);
            vgStyleDesigner.Height := round(bottom);
          end;
        end;
        if ReadString('left') <> '' then
          vgStyleDesigner.vgScene1.width := round(vgStrToFloat(ReadString('left')));
        if ReadString('right') <> '' then
          vgStyleDesigner.vgScene2.width := round(vgStrToFloat(ReadString('right')));
      end;
      Free;
    end;
    {$ENDIF}
    {$IFDEF FPC}
    Application.ProcessMessages;
    {$ENDIF}
    vgStyleDesigner.DesignScene.DesignTime := true;
    vgStyleDesigner.FResource := AResource;

    if vgStyleDesigner.DesignScene.Root <> nil then
      vgStyleDesigner.DesignScene.Root.DeleteChildren;

    AResource.FillStrings;
    vgStyleDesigner.LoadFromStrings(AResource.Resource);

    vgStyleDesigner.ObjectsTree.CollapseAll;

    if (vgStyleDesigner.DesignScene.Root <> nil) and (vgStyleDesigner.DesignScene.Root.FindResource(Current) <> nil) then
      vgDesigner.SelectObject(nil, vgStyleDesigner.DesignScene.Root.FindResource(Current), []);

    if vgStyleDesigner.ShowModal = mrOk then
    begin
      vgStyleDesigner.SaveToStrings(AResource.Resource);
      Result := true;
    end;
    {$IFDEF WIN32}
    with TRegistry.Create do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\ksdev\vgstyledesigner', true) then
      begin
        WriteString('position', vgRectToString(vgRect(vgStyleDesigner.Left, vgStyleDesigner.Top, vgStyleDesigner.Width, vgStyleDesigner.Height)));
        WriteString('left', vgFloatToStr(vgStyleDesigner.vgScene1.width));
        WriteString('right', vgFloatToStr(vgStyleDesigner.vgScene2.width));
      end;
      Free;
    end;
    {$ENDIF}
    vgStyleDesigner.vgScene1.Free;
    vgStyleDesigner.DesignScene.Free;
    vgStyleDesigner.vgScene2.Free;
    vgStyleDesigner.vgScene3.Free;

    vgStyleDesigner.Free;
    vgStyleDesigner := nil;

    vgDesigner.Free;

    vgDesigner := SaveDesigner;
  end;
end;

procedure TvgStyleIDEDesigner.AddObject(AObject: TvgObject);
begin
  inherited;
  vgStyleDesigner.UpdateTree;
end;

procedure TvgStyleIDEDesigner.DeleteObject(AObject: TvgObject);
begin
  inherited;
end;

{ TvgStyleDesigner }

procedure TvgStyleDesigner.textFilterChangeTracking(Sender: TObject);
begin
  UpdateTree;
end;

procedure TvgStyleDesigner.UpdateTree;
 procedure ProcessObject(Obj: TvgObject; Node: TvgTreeViewItem);
 var
   i: integer;
   N: TvgTreeViewItem;
   S: AnsiString;
 begin
   S := Obj.ResourceName;
   if S = '' then
     S := Obj.ClassName
   else
     S := S + ': ' + Obj.ClassName;

   if (textFilter.Text <> '') and (Obj.Parent = DesignScene.Root) then
   begin
     if Pos(textFilter.Text, S) = 0 then Exit;
   end;

   if Obj = DesignScene.Root then
   begin
     N := nil;
   end
   else
   if Node <> nil then
   begin
     N := TvgTreeViewItem.Create(Self);
     N.Parent := Node;
     N.Text := S;
     N.TagObject := Obj;
     N.Resource := 'objecttreeitemstyle';
     N.OnApplyResource := TreeItemApplyResource;
     if Obj.IsVisual then
       N.IsChecked := not Obj.Visual.Locked;
   end
   else
   begin
     N := TvgTreeViewItem.Create(Self);
     N.Parent := ObjectsTree;
     N.Text := S;
     N.TagObject := Obj;
     N.Resource := 'objecttreeitemstyle';
     N.OnApplyResource := TreeItemApplyResource;
     if Obj.IsVisual then
       N.IsChecked := not Obj.Visual.Locked;
   end;
   Obj.TagObject := N;
   if Obj.isVisual and (Obj.Parent = DesignScene.Root) then
   begin
     Obj.Visual.DesignHide := true;
     // center
     with Obj.Visual do
     begin
       SetBounds(Trunc((DesignScene.Width - Width) / 2), Trunc((DesignScene.Height - Height) / 2), Width, Height);
     end;
   end;
   if Obj.ChildrenCount > 0 then
     for i := 0 to Obj.ChildrenCount - 1 do
     begin
       if (DesignScene.Root is TvgBackground) and (Obj.Children[i] = TvgHackBackground(DesignScene.Root).FResourceLink) then Continue;
       ProcessObject(Obj.Children[i], N);
     end;
 end;
begin
  if DesignScene.Root <> nil then
  begin
    ObjectsTree.Clear;
    ObjectsTree.BeginUpdate;
    ProcessObject(DesignScene.Root, nil);
    ObjectsTree.EndUpdate;
    DesignScene.AddUpdateRect(vgRect(0, 0, 1000, 1000));
  end;
end;

procedure TvgStyleDesigner.TreeItemApplyResource(Sender: TObject);
begin
  TvgTreeViewItem(Sender).Binding['deletebutton'] := EventToVariant(DoDeleteButton);
  TvgTreeViewItem(Sender).Binding['vischeck'] := EventToVariant(DoVisCheck);
  if TvgTreeViewItem(Sender).FindBinding('vischeck') <> nil then
  begin
    TvgObject(TvgTreeViewItem(Sender).FindBinding('vischeck')).TagObject := TvgTreeViewItem(Sender).TagObject;
    TvgVisualObject(TvgTreeViewItem(Sender).FindBinding('vischeck')).Visible := TvgTreeViewItem(Sender).TagObject is TvgVisualObject;
    if TvgTreeViewItem(Sender).TagObject is TvgVisualObject then
      TvgTreeViewItem(Sender).Binding['vischeck'] := TvgVisualObject(TvgTreeViewItem(Sender).TagObject).Visible;
  end;
  if not (TvgTreeViewItem(Sender).TagObject is TvgVisualObject) then
    TvgVisualObject(TvgTreeViewItem(Sender).FindResource('check')).Visible := false;
end;

procedure TvgStyleDesigner.DoVisCheck(Sender: TObject);
begin
  TvgVisualObject(TvgObject(Sender).TagObject).Visible := TvgObject(Sender).Data;
end;

procedure TvgStyleDesigner.ObjectsTreeChange(Sender: TObject);
var
  New, Old: TvgObject;
begin
  if (ObjectsTree.Selected <> nil) and (ObjectsTree.Selected.TagObject <> nil) then
  begin
    Old := TvgObject(Inspector.SelectedObject);
    if (Old <> nil) and (Old.Parent <> nil) then
      while Old.Parent <> DesignScene.Root do
        Old := Old.Parent;
    New := TvgObject(ObjectsTree.Selected.TagObject);
    while New.Parent <> DesignScene.Root do
      New := New.Parent;
    if New.IsVisual then
    begin
      // center
      New.Visual.Position.X := round((DesignScene.Width - New.Visual.Width) / 2);
      New.Visual.Position.Y := round((DesignScene.Height - New.Visual.Height) / 2);
      //
      if (Old <> nil) and (Old.IsVisual) then
      begin
        if Old <> DesignScene.Root then
          Old.Visual.DesignHide := true
      end
      else
        if (Old <> nil) and (Old.Parent <> nil) and (Old.Parent.IsVisual) then
        begin
          if Old.Parent <> DesignScene.Root then
            Old.Parent.Visual.DesignHide := true;
        end;
      if New <> DesignScene.Root then
        New.Visual.DesignHide := false;
    end
    else
    begin
      if (New <> nil) and (New.Parent <> nil) and (New.Parent.IsVisual) then
      begin
        New.Parent.Visual.DesignHide := false;
      end;
    end;
    Inspector.SelectedObject := TvgObject(ObjectsTree.Selected.TagObject);
    if TvgObject(ObjectsTree.Selected.TagObject).isVisual then
      DesignScene.Selected := TvgVisualObject(ObjectsTree.Selected.TagObject);
  end;
end;

procedure TvgStyleDesigner.Button1Click(Sender: TObject);
var
  S: TStrings;
begin
  { Load style }
  if OpenDialog1.Execute then
  begin
    S := TStringList.Create;
    S.LoadFromFile(OpenDialog1.FileName);
    LoadFromStrings(S);
    S.Free;
  end;
end;

procedure TvgStyleDesigner.Button2Click(Sender: TObject);
var
  S: TStrings;
begin
  { Save style }
  if SaveDialog1.Execute then
  begin
    S := TStringList.Create;
    SaveToStrings(S);
    S.SaveToFile(SaveDialog1.FileName);
    S.Free;
  end;
end;

procedure TvgStyleDesigner.LoadFromStrings(Str: TStrings);
var
  S: TStream;
  i: integer;
  Root: TvgObject;
begin
  S := TMemoryStream.Create;
  try
    Inspector.SelectedObject := nil;
    DesignScene.Selected := nil;
    ObjectsTree.Selected := nil;

    Str.SaveToStream(S);
    if S.Position > 0 then
    begin
      S.Position := 0;
      if DesignScene.Root <> nil then
        DesignScene.Root.Free;
      Root := CreateObjectFromStream(nil, S);
      DesignScene.AddObject(Root);
      for i := 0 to Root.ChildrenCount - 1 do
        if Root.Children[i].isVisual then
        begin
{          TvgVisualObject(Root.FChildren[i]).Position.X := random(400);
          TvgVisualObject(Root.FChildren[i]).Position.Y := random(400);}
          TvgVisualObject(Root.Children[i]).Locked := false;
        end;
      if Root.ChildrenCount > 0 then
        vgDesigner.SelectObject(nil, Root.Children[0], []);
      UpdateTree;
      DesignScene.RealignRoot;

      if Root is TvgBackground then
      begin
        btnBack.Visible := true;
        rectBack.Fill.Assign(TvgBackground(Root).Fill);
      end
      else
        btnBack.Visible := false;
    end;
  finally
    S.Free;
  end;
end;

procedure TvgStyleDesigner.SaveToStrings(Str: TStrings);
var
  S: TStream;
  i: integer;
  Root: TvgObject;
begin
  { Save style }
  S := TMemoryStream.Create;
  try
    Root := DesignScene.Root;
    Str.Clear;
    for i := 0 to Root.ChildrenCount - 1 do
      if Root.Children[i].isVisual then
      begin
        TvgVisualObject(Root.Children[i]).Locked := false;
      end;
    Root.SaveToStream(S);
    S.Position := 0;
    Str.LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TvgStyleDesigner.Button3Click(Sender: TObject);
begin
  { apply }
  if FResource <> nil then
    SaveToStrings(FResource.Resource);
end;

procedure TvgStyleDesigner.btnClearClick(Sender: TObject);
begin
  { clear }
  btnClear.Enabled := false;
  ClearTimer.Enabled := true;
end;

procedure TvgStyleDesigner.ClearTimerTimer(Sender: TObject);
var
  R: TvgBackground;
begin
  ClearTimer.Enabled := false;
  Inspector.SelectedObject := nil;
  DesignScene.Selected := nil;
  ObjectsTree.Selected := nil;

  DesignScene.Root.Free;
  R := TvgBackground.Create(nil);
  DesignScene.AddObject(R);

  UpdateTree;
  btnClear.Enabled := true;
end;

procedure TvgStyleDesigner.btnLoadDefaultClick(Sender: TObject);
var
  i: integer;
  S: TStream;
  Root: TvgObject;
begin
  { Default }
  if DefaultStyles = nil then Exit;

  Inspector.SelectedObject := nil;
  DesignScene.Selected := nil;
  ObjectsTree.Selected := nil;
  Root := nil;

  {$IFDEF FPC}
  if System.FindResource(HInstance, PChar('defaultvgstyle'), RT_RCDATA) <> 0 then
  {$ELSE}
  if Windows.FindResource(HInstance, PChar('defaultvgstyle'), RT_RCDATA) <> 0 then
  {$ENDIF}
  begin
    S := TResourceStream.Create(HInstance, 'defaultvgstyle', RT_RCDATA);
    Root := CreateObjectFromBinStream(nil, S);
    S.Free;
  end;
  DesignScene.Root.Free;
  DesignScene.AddObject(Root);
  for i := 0 to Root.ChildrenCount - 1 do
    if Root.Children[i].isVisual then
    begin
      TvgVisualObject(Root.Children[i]).Locked := false;
    end;

  if Root.ChildrenCount > 0 then
    vgDesigner.SelectObject(nil, Root.Children[0], []);
  UpdateTree;

  DesignScene.RealignRoot;

  if Root is TvgBackground then
  begin
    btnBack.Visible := true;
    rectBack.Fill.Assign(TvgBackground(Root).Fill);
  end
  else
    btnBack.Visible := false;
end;

procedure TvgStyleDesigner.FormCreate(Sender: TObject);
begin
  OpenDialog1.Filter := 'VGScene style|*.vgstyle';
  SaveDialog1.Filter := 'VGScene style|*.vgstyle';
end;

procedure TvgStyleDesigner.DeleteObject(AObject: TvgObject; FreeObject: boolean);
var
  A, P: TvgObject;
  Sel: TvgTreeViewItem;
begin
  if AObject <> nil then
  begin
    AObject.RemoveFreeNotify(vgStyleDesigner);

    Sel := TvgTreeViewItem(AObject.TagObject);
    A := AObject;
    P := A.Parent;
    if P <> nil then
    begin
      Inspector.SelectedObject := P;
      if P.TagObject <> nil then
        ObjectsTree.Selected := TvgTreeViewItem(P.TagObject);
    end
    else
    begin
      Inspector.SelectedObject := nil;
      if P.TagObject <> nil then
        ObjectsTree.Selected := nil;
    end;
    Sel.Release;
    if FreeObject then
      A.Release
    else
      A.TagObject := nil;
    ObjectsTree.Realign;
  end;
end;

procedure TvgStyleDesigner.btnBackClick(Sender: TObject);
begin
  vgBrushDialog1.Brush.Assign(rectBack.Fill);
  if vgBrushDialog1.Execute then
  begin
    rectBack.Fill.Assign(vgBrushDialog1.Brush);
    if (DesignScene.Root <> nil) and (DesignScene.Root is TvgBackground) then
      TvgBackground(DesignScene.Root).Fill.Assign(vgBrushDialog1.Brush);
    TvgBackground(DesignScene.Root).Repaint;
  end;
end;

procedure TvgStyleDesigner.DoDeleteButton(Sender: TObject);
begin
  if ObjectsTree.Selected = nil then Exit;
  if (Sender is TvgSpeedButton) then
  begin// fixed fast click deletebutton
    TvgSpeedButton(Sender).Enabled := False;
  end;
  DeleteObject(TvgObject(ObjectsTree.Selected.TagObject), true);
end;

procedure TvgStyleDesigner.ObjectsTreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Button = mbRight) then
  begin
    DesignScene.OpenDesignPopup;
  end;
end;

procedure TvgStyleDesigner.ObjectsTreeDragChange(SourceItem,
  DestItem: TvgTreeViewItem; Allow: Boolean);
begin
  if DestItem = nil then
    TvgObject(SourceItem.TagObject).Parent := DesignScene.Root
  else
    TvgObject(SourceItem.TagObject).Parent := TvgObject(DestItem.TagObject)
end;

procedure TvgStyleDesigner.InspectorChangeProperty(Sender: TObject;
  PropertyName: String);
begin
  if vgStyleDesigner.Inspector.SelectedObject = nil then Exit;
  if TvgTreeViewItem(TvgObject(vgStyleDesigner.Inspector.SelectedObject).TagObject) = nil then Exit;

  if PropertyName = 'ResourceName' then
  begin
    TvgTreeViewItem(TvgObject(vgStyleDesigner.Inspector.SelectedObject).TagObject).Text :=
      GetPropValue(vgStyleDesigner.Inspector.SelectedObject, PropertyName) + ': ' + vgStyleDesigner.Inspector.SelectedObject.ClassName;
  end;
end;

procedure TvgStyleDesigner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited ;
  if (Operation = opRemove) and (AComponent is TvgObject) and Assigned(TvgObject(AComponent).Scene) and (TvgObject(AComponent).Scene.GetComponent = DesignScene) then
    DeleteObject(TvgObject(AComponent), false);
end;

procedure TvgStyleDesigner.ObjectsTreeChangeCheck(Sender: TObject);
begin
  if (ObjectsTree.Selected <> nil) and (ObjectsTree.Selected.TagObject <> nil) and (TvgObject(ObjectsTree.Selected.TagObject).IsVisual) then
  begin
    TvgVisualObject(ObjectsTree.Selected.TagObject).Locked := not ObjectsTree.Selected.IsChecked;
  end;
end;

procedure TvgStyleDesigner.DesignRootDragOver(Sender: TObject;
  const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean);
begin
  Accept := true;
end;

end.
