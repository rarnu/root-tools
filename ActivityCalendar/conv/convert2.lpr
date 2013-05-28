program convert2;

{$mode objfpc}{$H+}

uses
  Interfaces,
  process,
  Classes,
  SysUtils,
  CustApp,
  CsvDocument,
  Sqlite3DS,
  FileUtil;

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

  procedure TConvert2.DoRun;
  var
    output: string;
    activeData: string;
  begin
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('data') then
    begin
      activeData := GetOptionValue('data');
    end;

    if HasOption('output') then
    begin
      output := GetOptionValue('output');
    end;

    if (output = '') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    WriteLn('Start importing...');

    GenerateAndroidDB(output);
    if activeData <> '' then
    begin
      LoadCSV(activeData);
      ImportData('ACTIVITY');
      WriteLn('');
    end;
    WriteLn('Import finish');
    Terminate;
  end;

  procedure TConvert2.GenerateAndroidDB(output: string);
  var
    dbPath: string;
  begin
    dbPath := output + '/activity.db';
    if FileExists(dbPath) then
    begin
      DeleteFile(dbPath);
    end;
    SQlite.FileName := dbPath;
    SQlite.SQL := 'create table android_metadata(locale text)';
    SQlite.ExecSQL;
    SQlite.SQL := 'insert into android_metadata values (''en_US'')';
    SQlite.ExecSQL;
    SQlite.SQL :=
      'CREATE TABLE ACTIVITY (_id int primary key, city text not null, year int not null, start_month int not null, start_day int not null, end_month int not null, end_day int not null, start_hour int not null, start_minute int not null, end_hour int not null, end_minute int not null, title text not null, url text, source text, location text, weight int, tags text, content text, status int not null)';
    SQlite.ExecSQL;

  end;

  procedure TConvert2.LoadCSV(FileName: string);
  begin
    with TStringList.Create do
    begin
      LoadFromFile(FileName);
      CSV.CSVText := Text;
      Free;
    end;
  end;

  procedure TConvert2.ImportData(Table: string);
  var
    rc: integer;
    i: integer;
    strStart: string;
    strEnd: string;
    strWeight: string;
  begin
    rc := CSV.RowCount;

    SQlite.Close;
    SQlite.SQL := '';
    SQlite.TableName := Table;
    SQlite.Open;

    for i := 1 to rc - 1 do
    begin
      SQlite.Insert;

      // _id
      SQlite.Fields[0].AsInteger := i;
      // city
      SQlite.Fields[1].AsString := csv.Cells[0, i];

      // year, start_month, start_day // 2013/4/28
      strStart := csv.Cells[1, i];
      with TStringList.Create do
      begin
        Delimiter := '/';
        DelimitedText := strStart;
        SQlite.Fields[2].AsInteger := StrToInt(Strings[0]);
        SQlite.Fields[3].AsInteger := StrToInt(Strings[1]);
        SQlite.Fields[4].AsInteger := StrToInt(Strings[2]);
        Free;
      end;

      // end_month, end_day
      strEnd := csv.Cells[2, i];
      if (strEnd = '') then
      begin
        SQlite.Fields[5].AsInteger := -1;
        SQlite.Fields[6].AsInteger := -1;
      end
      else
      begin
        with TStringList.Create do
        begin
          Delimiter := '/';
          DelimitedText := strEnd;
          SQlite.Fields[5].AsInteger := StrToInt(Strings[1]);
          SQlite.Fields[6].AsInteger := StrToInt(Strings[2]);
          Free;
        end;
      end;

      // start_hour, start_minute
      strStart := csv.Cells[3, i];
      if strStart = '' then
      begin
        SQlite.Fields[7].AsInteger := -1;
        SQlite.Fields[8].AsInteger := -1;
      end
      else
      begin
        with TStringList.Create do
        begin
          Delimiter := ':';
          DelimitedText := strStart;
          SQlite.Fields[7].AsInteger := StrToInt(Strings[0]);
          SQlite.Fields[8].AsInteger := StrToInt(Strings[1]);
          Free;
        end;
      end;

      // end_hour, end_minute
      strEnd := csv.Cells[4, i];
      if strEnd = '' then
      begin
        SQlite.Fields[9].AsInteger := -1;
        SQlite.Fields[10].AsInteger := -1;
      end
      else
      begin
        with TStringList.Create do
        begin
          Delimiter := ':';
          DelimitedText := strEnd;
          SQlite.Fields[9].AsInteger := StrToInt(Strings[0]);
          SQlite.Fields[10].AsInteger := StrToInt(Strings[1]);
          Free;
        end;
      end;

      // title
      SQlite.Fields[11].AsString := csv.Cells[5, i];
      // url
      SQlite.Fields[12].AsString := csv.Cells[6, i];
      // source
      SQlite.Fields[13].AsString := csv.Cells[7, i];
      // location
      SQlite.Fields[14].AsString := csv.Cells[8, i];
      // weight
      strWeight := csv.Cells[9, i];
      if strWeight = '' then
      begin
        SQlite.Fields[15].AsInteger := -1;
      end
      else
      begin
        SQlite.Fields[15].AsInteger := StrToInt(strWeight);
      end;
      // tag
      SQlite.Fields[16].AsString := csv.Cells[10, i];
      // content
      SQlite.Fields[17].AsString := csv.Cells[11, i];
      // status
      SQlite.Fields[18].AsInteger := 1;

      SQlite.Post;
      Write(Format(#13'Importing %s Data: %d', [Table, i]));
    end;
    SQlite.ApplyUpdates;

  end;

  constructor TConvert2.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
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
    writeln('Usage: ', ExeName, ' -h');
    WriteLn('  -data <DATA.CSV>');
    WriteLn('  -output <output path>');
  end;

var
  Application: TConvert2;
begin
  Application := TConvert2.Create(nil);
  Application.Run;
  Application.Free;
end.

