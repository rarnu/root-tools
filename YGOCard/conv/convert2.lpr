program convert2;

{$mode objfpc}{$H+}

uses
  Interfaces,
  process,
  Classes, SysUtils, CustApp, CsvDocument, Sqlite3DS, FileUtil;

type

  { TConvert2 }

  TConvert2 = class(TCustomApplication)
  private
    CSV: TCSVDocument;
    SQlite: TSqlite3Dataset;
  protected
    procedure DoRun; override;
    procedure GenerateAndroidDB(output: string);
    procedure LoadCSV(FileName: string);
    procedure ImportData(Table: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Convert2 }

procedure ExecuteCommand(cmd, path: string);
var
  AProcess: TProcess;
begin

  WriteLn(cmd);
  AProcess := TProcess.Create(nil);
  AProcess.CurrentDirectory := path;
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.ShowWindow := swoNone;
  try
     AProcess.Execute;
  except
    on Ex: Exception do
    begin
      WriteLn('');
      WriteLn('Error: ' + Ex.Message);
      WriteLn('');
    end;
  end;
  AProcess.Free;
end;

procedure TConvert2.DoRun;
var
  output: string;
  ygodata, ygoeffect: string;
  path: string;
begin
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('data') then
  begin
    ygodata:= GetOptionValue('data');
  end;
  if HasOption('effect') then
  begin
    ygoeffect := GetOptionValue('effect');
  end;
  if HasOption('output') then
  begin
    output:=GetOptionValue('output');
  end;

  if (ygodata = '') or (ygoeffect = '') or (output = '') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  WriteLn('Start importing...');

  GenerateAndroidDB(output);
  LoadCSV(ygodata);
  ImportData('YGODATA');
  WriteLn('');
  LoadCSV(ygoeffect);
  ImportData('YGOEFFECT');
  WriteLn('');
  WriteLn('Import finish');
  Terminate;
end;

procedure TConvert2.GenerateAndroidDB(output: string);
var
  dbPath: string;
begin
  dbPath:= output + '/yugioh.db';
  if FileExists(dbPath) then
  begin
    DeleteFile(dbPath);
  end;
  SQlite.FileName:=dbPath;
  SQlite.SQL:='create table android_metadata(locale text)';
  SQlite.ExecSQL;
  SQlite.SQL:='insert into android_metadata values (''en_US'')';
  SQlite.ExecSQL;
  SQlite.SQL:='CREATE TABLE YGODATA (_id int primary key,CardID int,CardPhAl text,CardCamp text,JPCardName text,SCCardName text,TCCardName text,ENCardName text,ENCardName2 text,JPCardType text,SCCardType text,TCCardType text,ENCardType text,JPDCardType text,SCDCardType text,TCDCardType text,ENDCardType text,JPCardRace text,SCCardRace text,TCCardRace text,ENCardRace text,CardBagNum text,JPCardAttribute text,SCCardAttribute text,TCCardAttribute text,ENCardAttribute text,CardStarNum int,SCCardRare text,TCCardRare text,ENCardRare text,CardAtk int,CardAtk2 text,CardDef int,CardDef2 text,JPCardDepict text,SCCardDepict text,TCCardDepict text,ENCardDepict text,SCCardBan text,TCCardBan text,ENCardBan text,CardIsYKDT int,CardIsTKEN int,CardIsCZN text,CardPass text,CardAdjust text,CardLover int,CardUnion text,CardOnceName text,CardAbbrName text,CardEfficeType text)';
  SQlite.ExecSQL;
  SQlite.SQL:='create table YGOEFFECT(_id int primary key, ID int, EFFECT text)';
  SQlite.ExecSQL;

end;

procedure TConvert2.LoadCSV(FileName: string);
begin
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    CSV.CSVText:=Text;
    Free;
  end;
end;

function GetFieldTypeName(cn: string): string;
var
  r: string;
begin
  r := '';
  if (cn = 'TIntegerField') or (cn = 'TWordField') or (cn = 'TLongintField') then
  begin
    r := 'int';
  end
  else
  begin
    r := 'text';
  end;
  Result := r;
end;

procedure TConvert2.ImportData(Table: string);
var
  rc: Integer;
  cc: Integer;
  i, j: Integer;
  s: string;
begin
  cc := CSV.ColCount[1];
  rc := CSV.RowCount;

  SQlite.Close;
  SQlite.SQL:='';
  SQlite.TableName:=Table;
  SQlite.Open;

  for i:=1 to rc - 1 do
  begin
    SQlite.Insert;
    SQlite.Fields[0].AsInteger:=StrToInt(CSV.Cells[0, i]);
    for j:=0 to cc - 1 do
    begin
      if GetFieldTypeName(SQlite.Fields[j+1].ClassName) = 'int' then
      begin
        SQlite.Fields[j+1].AsInteger:=StrToIntDef(CSV.Cells[j, i], 0);
      end
      else
      begin
        s := Trim(CSV.Cells[j, i]);
        s := StringReplace(s, '　','',[rfReplaceAll,rfIgnoreCase]);
        if s = '' then
        begin
          s := ' ';
        end;
        SQlite.Fields[j+1].AsString := s;
      end;
    end;
    SQlite.Post;
    Write(Format(#13'Importing %s Data: %d', [Table, i]));
  end;
  SQlite.ApplyUpdates;

end;

constructor TConvert2.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  CSV := TCSVDocument.Create;
  SQlite := TSqlite3Dataset.Create(nil);
end;

destructor TConvert2.Destroy;
begin
  CSV.Free;
  SQlite.Free;
  inherited Destroy;
end;

procedure TConvert2.WriteHelp;
begin
  WriteLn('Data Prepare: ');
  WriteLn('  mdb-export YGODAT.DAT YGODATA > YGODATA.CSV');
  WriteLn('  mdb-export YGODAT.DAT YGOEFFECT > YGOEFFECT.CSV');
  writeln('Usage: ',ExeName,' -h');
  WriteLn('  -data <YGODATA.CSV>');
  WriteLn('  -effect <YGOEFFECT.CSV>');
  WriteLn('  -output <output path>');
end;

var
  Application: TConvert2;
begin
  Application:=TConvert2.Create(nil);
  Application.Title:='Convert2';
  Application.Run;
  Application.Free;
end.

