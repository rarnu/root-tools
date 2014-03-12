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
unit registersqlite3;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf, PropEdits,
  ComponentEditors, sqlite3ds, SqliteComponentEditor;
  
procedure Register;

implementation

procedure RegisterUnitSqlite3ds;
begin
  RegisterComponents('Data Access',[TSqlite3Dataset]);
end;  

procedure Register;

begin
  RegisterUnit('sqlite3ds',@RegisterUnitSqlite3ds);
  RegisterComponentEditor(TSqlite3Dataset,TSqliteEditor) ;
  RegisterPropertyEditor(TypeInfo(String),TSqlite3Dataset,'FileName',
                         TFileNamePropertyEditor);
end; 

initialization
{$i sqlite3icon.lrs}
 
end.
