unit vg_dsgn_lang;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, vg_scene, vg_layouts, vg_listbox, vg_controls,
  vg_objects, vg_textbox;

type
  TvgLangDesigner = class(TForm)
    vgScene1: TvgScene;
    Root1: TvgHudWindow;
    vgResources1: TvgResources;
    OriginalList: TvgHudListBox;
    btnAddItem: TvgHudCornerButton;
    langList: TvgHudPopupBox;
    ToolBar1: TvgToolBar;
    inputLang: TvgHudTextBox;
    HudLabel1: TvgHudLabel;
    layoutSelect: TvgLayout;
    HudLabel2: TvgHudLabel;
    btnAddNewLang: TvgHudButton;
    btnCancalAdd: TvgHudButton;
    layoutAdd: TvgLayout;
    layoutAddText: TvgLayout;
    btnAddText: TvgHudButton;
    btnCancelAddText: TvgHudButton;
    inputAddText: TvgHudTextBox;
    btnRemoveItem: TvgHudCornerButton;
    btnCollect: TvgHudCornerButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    btnAddLang: TvgHudButton;
    btnLoadTxt: TvgHudButton;
    btnCreateTemplate: TvgHudCornerButton;
    btnLoadLng: TvgHudCornerButton;
    btnSaveLng: TvgHudCornerButton;
    OpenDialog2: TOpenDialog;
    SaveDialog2: TSaveDialog;
    procedure btnAddClick(Sender: TObject);
    procedure btnAddLangClick(Sender: TObject);
    procedure langListChange(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnRemoveItemClick(Sender: TObject);
    procedure btnAddNewLangClick(Sender: TObject);
    procedure btnCancalAddClick(Sender: TObject);
    procedure btnCancelAddTextClick(Sender: TObject);
    procedure btnAddTextClick(Sender: TObject);
    procedure btnCollectClick(Sender: TObject);
    procedure btnCreateTemplateClick(Sender: TObject);
    procedure btnLoadTxtClick(Sender: TObject);
    procedure btnLoadLngClick(Sender: TObject);
    procedure btnSaveLngClick(Sender: TObject);
  private
    { Private declarations }
    FLang: TvgLang;
    FCurLang: WideString;
    procedure RebuildOriginalList;
    procedure DoTranslateChanged(Sender: TObject);
  public
    { Public declarations }
  end;

var
  vgLangDesigner: TvgLangDesigner;

procedure ShowDsgnLang(Lang: TvgLang);

implementation

{$R *.dfm}

procedure ShowDsgnLang(Lang: TvgLang);
begin
  vgLangDesigner := TvgLangDesigner.Create(Application);
  with vgLangDesigner do
  begin
    FLang := Lang;
    langList.Items.Assign(Lang.Resources);
    if langList.Items.Count > 0 then
      langList.ItemIndex := Lang.Resources.IndexOf(Lang.Lang);

    layoutAdd.Visible := langList.Items.Count = 0;
    layoutSelect.Visible := langList.Items.Count > 0;

    RebuildOriginalList;
    if ShowModal = mrOk then
    begin
      FLang.Lang := langList.Text;
    end;
  end;
  vgLangDesigner.Free;
end;

procedure TvgLangDesigner.RebuildOriginalList;
var
  i: integer;
  Str: TvgWideStrings;
  Item: TvgListboxItem;
begin
  OriginalList.Clear;
  if FLang.Original.Count = 0 then
  begin
    // create original from Collection
    CollectLangStart;
    UpdateLang;
    FLang.Original.Assign(CollectLangStrings);
    CollectLangFinish;
  end;
  Str := FLang.Original;
  for i := 0 to Str.Count - 1 do
  begin
    Item := TvgListboxItem.Create(Self);
    Item.AutoTranslate := false;
    Item.Resource := 'langitem';
    Item.Text := Str[i];
    Item.TextAlign := vgTextAlignCenter;
    Item.Height := 22;
    Item.Parent := OriginalList;
    if (FLang.Resources.Count > 0) and (langList.ItemIndex >= 0) then
    begin
      if FLang.LangStr[langList.Text] <> nil then
      begin
        Item.Binding['translate'] := FLang.LangStr[langList.Text].Values[Str[i]];
        Item.Binding['translate'] := EventToVariant(DoTranslateChanged);
      end;
    end
    else
      Item.FindBinding('translate').Visual.Visible := false;
  end;
end;

procedure TvgLangDesigner.btnAddClick(Sender: TObject);
var
  List: TvgListBox;
begin
  { add new lang }
  List := TvgListBox.Create(Self);
end;

procedure TvgLangDesigner.DoTranslateChanged(Sender: TObject);
begin
  if (FLang.LangStr[langList.Text] <> nil) and (OriginalList.Selected <> nil) then
    with FLang.LangStr[langList.Text] do
    begin
      Values[OriginalList.Selected.Text] := TvgTextBox(Sender).Text;
    end;
end;

procedure TvgLangDesigner.btnAddLangClick(Sender: TObject);
var
  S: string;
begin
  if inputLang.Text = '' then Exit;
  S := inputLang.Text;
  if Length(S) > 2 then
    Delete(S, 3, MaxInt);
  FLang.AddLang(S);
  langList.Items := FLang.Resources;
  langList.ItemIndex := langList.Items.IndexOf(S);
  RebuildOriginalList;

  layoutAdd.Visible := false;
  layoutSelect.Visible := true;
end;

procedure TvgLangDesigner.langListChange(Sender: TObject);
begin
  RebuildOriginalList;
end;

procedure TvgLangDesigner.btnAddNewLangClick(Sender: TObject);
begin
  layoutAdd.Visible := true;
  layoutSelect.Visible := false;
  btnCancalAdd.Visible := langList.Items.Count > 0;

  inputLang.Text := '';
  inputLang.SetFocus;
end;

procedure TvgLangDesigner.btnCancalAddClick(Sender: TObject);
begin
  if langList.Items.Count > 0 then
  begin
    layoutAdd.Visible := false;
    layoutSelect.Visible := true;
  end;
end;

procedure TvgLangDesigner.btnAddItemClick(Sender: TObject);
begin
  { Add Word }
  layoutAdd.Visible := false;
  layoutSelect.Visible := false;
  layoutAddText.Visible := true;
  inputAddText.Text := '';
  inputAddText.SetFocus;
  RebuildOriginalList;
end;

procedure TvgLangDesigner.btnRemoveItemClick(Sender: TObject);
begin
  { Remove Word }
  if OriginalList.ItemIndex >= 0 then
  begin
    FLang.Original.Delete(OriginalList.ItemIndex);
    RebuildOriginalList;
  end;
end;

procedure TvgLangDesigner.btnCancelAddTextClick(Sender: TObject);
begin
  layoutAdd.Visible := langList.Items.Count = 0;
  layoutSelect.Visible := langList.Items.Count > 0;
  layoutAddText.Visible := false;
end;

procedure TvgLangDesigner.btnAddTextClick(Sender: TObject);
begin
  btnCancelAddTextClick(Sender);
  FLang.Original.Add(inputAddText.Text);
  RebuildOriginalList;
  OriginalList.ItemIndex := OriginalList.Count - 1;
end;

procedure TvgLangDesigner.btnCollectClick(Sender: TObject);
var
  Str: TvgWideStrings;
  i: integer;
begin
  CollectLangStart;
  UpdateLang;
  Str := TvgWideStringList.Create;
  Str.Assign(CollectLangStrings);
  for i := 0 to Str.Count - 1 do
    if FLang.Original.IndexOf(Str[i]) < 0 then
      FLang.Original.Add(Str[i]);
  Str.Free;
  CollectLangFinish;
  RebuildOriginalList;
end;

procedure TvgLangDesigner.btnCreateTemplateClick(Sender: TObject);
var
  Str: TvgWideStrings;
  i: integer;
begin
  if SaveDialog1.Execute then
  begin
    Str := TvgWideStringList.Create;
    Str.Assign(FLang.Original);
    for i := 0 to Str.Count - 1 do
      Str[i] := Str[i] + '=';
    Str.SaveToFile(SaveDialog1.FileName);
    Str.Free;
  end;
end;

procedure TvgLangDesigner.btnLoadTxtClick(Sender: TObject);
var
  Str: TvgWideStrings;
  i: integer;
begin
  if OpenDialog1.Execute then
  begin
    FLang.AddLang(inputLang.Text);
    langList.Items := FLang.Resources;
    langList.ItemIndex := langList.Items.IndexOf(inputLang.Text);
    RebuildOriginalList;

    layoutAdd.Visible := false;
    layoutSelect.Visible := true;

    Str := TvgWideStringList.Create;
    Str.LoadFromFile(OpenDialog1.FileName);
    for i := 0 to Str.Count - 1 do
      if FLang.LangStr[langList.Text].IndexOfName(Str.Names[i]) < 0 then
        FLang.LangStr[langList.Text].Add(Str[i])
      else
        FLang.LangStr[langList.Text].Values[Str.Names[i]] := Str.Values[Str.Names[i]];
    Str.Free;
    RebuildOriginalList;
  end;
end;

procedure TvgLangDesigner.btnLoadLngClick(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    FLang.LoadFromFile(OpenDialog2.FileName);
    RebuildOriginalList;
  end;
end;

procedure TvgLangDesigner.btnSaveLngClick(Sender: TObject);
begin
  if SaveDialog2.Execute then
  begin
    FLang.SaveToFile(SaveDialog2.FileName);
  end;
end;

end.

