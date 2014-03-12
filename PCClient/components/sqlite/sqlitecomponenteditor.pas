{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit SqliteComponentEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs, StdCtrls,
  Buttons, customsqliteds, ComponentEditors, LazarusPackageIntf, LazIdeIntf,
  fieldseditor;

type

  {TSqliteEditor}
  
  TSqliteEditor = class(TFieldsComponentEditor)
  private
    FVerbOffset: Integer;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

  { TSqliteTableEditorForm }

  TSqliteTableEditorForm = class(TForm)
    butCreate: TButton;
    butClose: TButton;
    butAdd: TButton;
    butDelete: TButton;
    comboFieldType: TComboBox;
    editFieldName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblFilePath: TLabel;
    listFields: TListBox;
    DataSet: TCustomSqliteDataSet;
    procedure LoadCurrentFields;
    procedure FillComboValues;
    procedure SetComboValue(AObject: TObject);
    procedure SqliteTableEditorFormShow(Sender: TObject);
    procedure butAddClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure butDeleteClick(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure comboFieldTypeChange(Sender: TObject);
    procedure editFieldNameEditingDone(Sender: TObject);
    procedure listFieldsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 
  
implementation

uses
  db;

function StringListHasDuplicates(const List:TStrings):boolean;
var
  i,j:Integer;
begin
  Result:=False;
  for i := 0 to List.Count - 1 do
    for j:= i+1 to List.Count - 1 do
      if AnsiCompareText(List[i],List[j]) = 0 then
      begin
        Result:=True;
        Exit;
      end;
end;

{TSqliteEditor}

procedure TSqliteEditor.ExecuteVerb(Index: Integer);
begin
  case Index - FVerbOffset of
    0: Edit;
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TSqliteEditor.GetVerb(Index: Integer): string;
begin
  case Index - FVerbOffset of
    0:
    begin
      Result := 'Create/Edit Table'
    end;
    else
     Result := inherited GetVerb(Index);
  end;
end;

function TSqliteEditor.GetVerbCount: Integer;
begin
  FVerbOffset := inherited GetVerbCount;
  Result := FVerbOffset + 1;
end;

procedure TSqliteEditor.Edit;
var
  ADataSet:TCustomSqliteDataSet;
  OldDir, ProjectDir:String;
begin
  ADataSet:=TCustomSqliteDataSet(GetComponent);
  if ADataSet.Filename = '' then
  begin
    ShowMessage('FileName not set: it''s not possible to create/edit a table');
    exit;
  end;  
  if ADataSet.TableName = '' then
  begin
    ShowMessage('TableName not set: it''s not possible to create/edit a table');
    exit;
  end;
    
  with TSqliteTableEditorForm.Create(Application) do
  begin
    try
      // In case Filename is a relative one, change dir to project dir
      // so the datafile will be created in the right place
      OldDir := GetCurrentDirUTF8;
      ProjectDir := ExtractFilePath (LazarusIDE.ActiveProject.MainFile.FileName);
      if ProjectDir <> '' then
        SetCurrentDirUTF8(ProjectDir);
      Dataset := ADataset;
      ShowModal;
    finally
      SetCurrentDirUTF8(OldDir);
      Free;
    end;  
  end;    
end;  

{ TSqliteTableEditorForm }

procedure TSqliteTableEditorForm.butAddClick(Sender: TObject);
begin
  //In the case there's no items
  editFieldName.Enabled:=True;
  comboFieldType.Enabled:=True;
  listFields.Items.AddObject('AFieldName',TObject(ftString));
  listFields.ItemIndex:=listFields.Items.Count-1;
  editFieldName.Text:='AFieldName';
  editFieldName.SetFocus;
end;

procedure TSqliteTableEditorForm.LoadCurrentFields;
var
  OldSql:String;
  OldActive:Boolean;
  i:Integer;
begin
  with Dataset do
  begin
    OldSql:=Sql;
    OldActive:=Active;
    Sql:='Select * from '+TableName+' where 1 = 0';//dummy sql
    Close;
    Open;
    for i:=0 to FieldDefs.Count - 1 do
      listFields.Items.AddObject(FieldDefs[i].Name,
                                 TObject(PtrInt(FieldDefs[i].DataType)));
    listFields.ItemIndex:=0;
    Sql:=OldSql;
    Active:=OldActive;
  end;
end;

procedure TSqliteTableEditorForm.FillComboValues;
begin
  with comboFieldType.Items do
  begin
    Clear;
    AddObject('String',TObject(ftString));
    AddObject('Integer',TObject(ftInteger));
    AddObject('LargeInt',TObject(ftLargeInt));
    AddObject('AutoInc',TObject(ftAutoInc));
    AddObject('Word',TObject(ftWord));
    AddObject('Float',TObject(ftFloat));
    AddObject('Currency',TObject(ftCurrency));
    AddObject('Boolean',TObject(ftBoolean));
    AddObject('DateTime',TObject(ftDateTime));
    AddObject('Date',TObject(ftDate));
    AddObject('Time',TObject(ftTime));
    AddObject('Memo',TObject(ftMemo));
  end;
end;

procedure TSqliteTableEditorForm.SetComboValue(AObject: TObject);
var
  AIndex:Integer;
begin
  AIndex:=comboFieldType.Items.IndexOfObject(AObject);

  if AIndex <> -1 then
    comboFieldType.ItemIndex:=AIndex
  else
    raise Exception.Create('TableEditor - FieldType not recognized');
end;

procedure TSqliteTableEditorForm.SqliteTableEditorFormShow(Sender: TObject);
begin
  FillComboValues;
  if Dataset.TableExists then
  begin
    LoadCurrentFields;
  end
  else
  begin
    editFieldName.Enabled:=False;
    comboFieldType.Enabled:=False;
  end;
  lblFilePath.Caption:='File Path: '+ExpandFileNameUTF8(DataSet.FileName);
  label3.caption:='Table Name: '+ DataSet.TableName;
end;

procedure TSqliteTableEditorForm.butCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSqliteTableEditorForm.butDeleteClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex:=listFields.ItemIndex;
  if AIndex <> -1 then
  begin
    listFields.Items.Delete(AIndex);
    if listFields.Items.Count = 0 then
    begin
      editFieldName.Text:='';
      editFieldName.Enabled:=False;
      comboFieldType.ItemIndex:=-1;
      comboFieldType.Enabled:=False;
    end
    else
    begin
      if AIndex <> 0 then
        listFields.ItemIndex:=Pred(AIndex)
      else
        listFields.ItemIndex:=AIndex;
    end;
  end;
end;

procedure TSqliteTableEditorForm.butOkClick(Sender: TObject);
var
  i:Integer;
begin
  if listFields.Items.Count = 0 then
  begin;
    ShowMessage('No fields added - Table will not be created');
    Exit;
  end;
  
  if StringListHasDuplicates(listFields.Items) then
  begin
    ShowMessage('It''s not allowed fields with the same name');
    Exit;
  end;
  
  if Dataset.TableExists then
  begin
    if MessageDlg('A Table named "'+Dataset.TableName+'" already exists. Are you sure you want to replace this table?'#13#10'All data stored will be lost',
       mtWarning,[mbYes,MbNo],0) = mrNo then
      exit
    else
      DataSet.ExecSQL('DROP TABLE '+DataSet.TableName+';');
  end;

  with DataSet.FieldDefs do
  begin
    Clear;
    for i:= 0 to listFields.Items.Count - 1 do
      Add(listFields.Items[i],TFieldType(PtrInt(listFields.Items.Objects[i])));
  end;
  DataSet.CreateTable;

  if Dataset.TableExists then
    ShowMessage('Table created successfully')
  else
    ShowMessage('It was not possible to create the table');
end;

procedure TSqliteTableEditorForm.comboFieldTypeChange(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items.Objects[listFields.ItemIndex]:=TObject(comboFieldType.Items.Objects[comboFieldType.ItemIndex]);
end;

procedure TSqliteTableEditorForm.editFieldNameEditingDone(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items[listFields.ItemIndex]:=editFieldName.Text;
end;

procedure TSqliteTableEditorForm.listFieldsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if (listFields.ItemIndex <> -1) then
  begin
    editFieldName.Text:=listFields.Items[listFields.ItemIndex];
    SetComboValue(listFields.Items.Objects[listFields.ItemIndex]);
  end;
end;

initialization
  {$i sqlitecomponenteditor.lrs}
  
end.

