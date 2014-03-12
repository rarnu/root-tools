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
unit registersqlite;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf, PropEdits,
  ComponentEditors, sqliteds, SqliteComponentEditor;
  
procedure Register;

implementation

procedure RegisterUnitSqliteds;
begin
  RegisterComponents('Data Access',[TSqliteDataset]);
end;  

procedure Register;

begin
  RegisterUnit('sqliteds',@RegisterUnitSqliteds);
  RegisterComponentEditor(TSqliteDataset,TSqliteEditor) ;
  RegisterPropertyEditor(TypeInfo(String),TSqliteDataset,'FileName',
                         TFileNamePropertyEditor);
end; 

initialization
{$i sqliteicon.lrs}
 
end.
