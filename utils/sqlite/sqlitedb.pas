{$mode objfpc}
{$h+}

unit SQLitedb;

interface

uses  Classes,strings,sqlite;

type
  TSQLiteExecCallback = function(Sender: pointer; Columns: Integer; ColumnValues: ppchar; ColumnNames: ppchar): integer of object; cdecl;
  TSQLiteBusyCallback = function(Sender: TObject; ObjectName: PChar; BusyCount: integer): integer of object; cdecl;
  TOnData = Procedure(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: String)  of object;
  TOnBusy = Procedure(Sender: TObject; ObjectName: String; BusyCount: integer; var Cancel: Boolean) of object;
  TOnQueryComplete = Procedure(Sender: TObject) of object;

  TSQLite = class(TObject)
  private
    fSQLite: Pointer;
    fMsg: String;
    fIsOpen: Boolean;
    fBusy: Boolean;
    fError: Integer;
    fVersion: String;
    fEncoding: String;
    fTable: TStrings;
    fLstName: TStringList;
    fLstVal: TStringList;
    fOnData: TOnData;
    fOnBusy: TOnBusy;
    fOnQueryComplete: TOnQueryComplete;
    fBusyTimeout: integer;
    fPMsg: PChar;
    fChangeCount: integer;
        fNb_Champ :  Integer;
        fList_FieldName : TStringList;
        fList_Field : TList;
    procedure SetBusyTimeout(Timeout: integer);
  public
    constructor Create(DBFileName: String);
    destructor Destroy; override;
    function Query(Sql: String; Table: TStrings ): boolean;
    function ErrorMessage(ErrNo: Integer): string;
    function IsComplete(Sql: String): boolean;
    function LastInsertRow: integer;
    function Cancel: boolean;
    function DatabaseDetails(Table: TStrings): boolean;
    property LastErrorMessage: string read fMsg;
    property LastError: Integer read fError;
    property Version: String read fVersion;
    property Encoding: String read fEncoding;
    property OnData: TOnData read fOnData write fOnData;
    property OnBusy: TOnBusy read fOnBusy write fOnBusy;
    property OnQueryComplete: TOnQueryComplete read fOnQueryComplete write fOnQueryComplete;
    property BusyTimeout: Integer read fBusyTimeout write SetBusyTimeout;
    property ChangeCount: Integer read fChangeCount;
    property List_FieldName: TStringList read fList_FieldName write fList_FieldName;
    property List_Field: TList read fList_Field write fList_Field;
    property Nb_Champ: integer read fNb_Champ write fNb_Champ;

  procedure SQLOnData(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: String);

  end;
  function Pas2SQLStr(const PasString: string): string;
  function SQL2PasStr(const SQLString: string): string;
  function QuoteStr(const s: string; QuoteChar: Char ): string;
  function UnQuoteStr(const s: string; QuoteChar: Char ): string;
  procedure ValueList(const ColumnNames, ColumnValues: String; NameValuePairs: TStrings);

implementation

Const
  DblQuote: Char    = '"';
  SngQuote: Char    = #39;
  Crlf: String      = #13#10;
  Tab: Char         = #9;

var
  MsgNoError: String;

function QuoteStr(const s: string; QuoteChar: Char ): string;
begin
  Result := Concat(QuoteChar, s, QuoteChar);
end;

function UnQuoteStr(const s: string; QuoteChar: Char ): string;
begin
  Result := s;
  if length(Result) > 1 then
  begin
    if Result[1] = QuoteChar then
      Delete(Result, 1, 1);
    if Result[Length(Result)] = QuoteChar then
      Delete(Result, Length(Result), 1);
  end;
end;

function Pas2SQLStr(const PasString: string): string;
var
  n: integer;
begin
  Result := SQL2PasStr(PasString);
  n := Length(Result);
  while n > 0 do
  begin
    if Result[n] = SngQuote then
      Insert(SngQuote, Result, n);
    dec(n);
  end;
  Result := QuoteStr(Result,#39);
end;

function SQL2PasStr(const SQLString: string): string;
const
  DblSngQuote: String = #39#39;
var
  p: integer;
begin
  Result := SQLString;
  p := pos(DblSngQuote, Result);
  while p > 0 do
  begin
    Delete(Result, p, 1);
    p := pos(DblSngQuote, Result);
  end;
  Result := UnQuoteStr(Result,#39);
end;

procedure ValueList(const ColumnNames, ColumnValues: String; NameValuePairs: TStrings);
var
  n: integer;
  lstName, lstValue: TStringList;
begin
  if NameValuePairs <> nil then
  begin
    lstName := TStringList.Create;
    lstValue := TStringList.Create;
    lstName.CommaText := ColumnNames;
    lstValue.CommaText := ColumnValues;
    NameValuePairs.Clear;
    if lstName.Count = LstValue.Count then
      if lstName.Count > 0 then
        for n := 0 to lstName.Count - 1 do
          NameValuePairs.Append(Concat(lstName.Strings[n], '=', lstValue.Strings[n]));
    lstValue.Free;
    lstName.Free;
  end;
end;



function SystemErrorMsg(ErrNo: Integer ): String;
var
  buf: PChar;
  size: Integer;
  MsgLen: Integer;
begin
{  size := 256;
  GetMem(buf, size);
  If ErrNo = - 1 then
    ErrNo := GetLastError;
  MsgLen := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, 0, buf, size, nil);
  if MsgLen = 0 then
    Result := 'ERROR'
  else
    Result := buf;}
end;

function BusyCallback(Sender: pointer; ObjectName: PChar; BusyCount: integer): integer; cdecl;
var
  sObjName: String;
  bCancel: Boolean;
begin
  Result := -1;
  with TObject(Sender) as TSQLite do
  begin
    if Assigned(fOnBusy) then
    begin
      bCancel := False;
      sObjName := ObjectName;
      fOnBusy(Tobject(Sender), sObjName, BusyCount, bCancel);
      if bCancel then
        Result := 0;
    end;
  end;
end;

function ExecCallback(Sender: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPchar): integer; cdecl;
var
  PVal, PName: ^PChar;
  n: integer;
  sVal, sName: String;
begin
  Result := 0;
  with TObject(Sender) as TSQLite do
  begin
    if (Assigned(fOnData) or Assigned(fTable)) then
    begin
      fLstName.Clear;
      fLstVal.Clear;
      if Columns > 0 then
      begin
        PName := ColumnNames;
        PVal := ColumnValues;
        for n := 0 to Columns - 1 do
        begin
          fLstName.Append(PName^);
          fLstVal.Append(PVal^);
          inc(PName);
          inc(PVal);
        end;
      end;
      sVal := fLstVal.CommaText;
      sName := fLstName.CommaText;
      if Assigned(fOnData) then
        fOnData(TObject(Sender), Columns, sName, sVal);
      if Assigned(fTable) then
      begin
        if fTable.Count = 0 then
          fTable.Append(sName);
        fTable.Append(sVal);
      end;
    end;
  end;
end;


procedure TSQLite.SQLOnData(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: String);
Var i : Integer;
          InterS,val : String;
          Field : TStringList;

          function Pos1(a: String ; s : char) : integer;
          var i,j : Integer;

          begin
          j:=-1;
                for i:=1 to length(a) Do
                begin
                        if a[i] = s then
                        begin
                                j:=i;
                                break;
                        end;
                end;
                result:=j;
          end;
begin
        If Nb_Champ = -1 Then
        Begin // Put the fields name in List_FieldName
                Nb_Champ:=Columns;
                InterS:=ColumnNames;
                While (Pos1(InterS,',') > 0)  do
                begin
                        val:=copy(InterS,1,Pos1(InterS,',')-1);
                        InterS:=copy(InterS,Pos1(InterS,',')+1,length(InterS));
                        List_FieldName.add(val);
                end;
                if length(InterS) > 0 then List_FieldName.add(InterS);
        end;
        // Put the list of TStringList of value
        Field :=TStringList.Create;
        InterS:=ColumnValues;
        While (Pos1(InterS,',') > 0)  do
        begin
                val:=copy(InterS,1,Pos1(InterS,',')-1);
                InterS:=copy(InterS,Pos1(InterS,',')+1,length(InterS));
                Field.add(val);
        end;
        if length(InterS) > 0 then Field.add(InterS);
        List_Field.add(Field);
end;

constructor TSQLite.Create(DBFileName: String);
var
  fPMsg1: PChar;
  name : pchar;
begin
  inherited Create;
  List_FieldName := TStringList.Create;
  List_Field := TList.Create;
  fError := SQLITE_ERROR;
  fIsOpen := False;
  fLstName := TStringList.Create;
  fLstVal := TStringList.Create;
  fOnData := nil;
  fOnBusy := nil;
  fOnQueryComplete := nil;
  fChangeCount := 0;
   name:=StrAlloc (length(DBFileName)+1);
   strpcopy(name,DBFileName);
   OnData:=@SQLOnData;
    fSQLite := SQLite_Open(name, 1, @fPMsg);
    SQLite_FreeMem(fPMsg);
    if fSQLite <> nil then
    begin
      //fVersion := String(SQLite_Version);
      //fEncoding := SQLite_Encoding;
      fIsOpen := True;
      fError := SQLITE_OK;
    end;
  fMsg := ErrorMessage(fError);
end;

destructor TSQLite.Destroy;
begin
  if fIsOpen then
    SQLite_Close(fSQLite);
  fIsOpen := False;
  fLstName.Free;
  fLstVal.Free;
  fSQLite := nil;
  fOnData := nil;
  fOnBusy := nil;
  fOnQueryComplete := nil;
  fLstName := nil;
  fLstVal := nil;
  List_FieldName.destroy;
  List_Field.destroy;
  inherited Destroy;
end;

function TSQLite.Query(Sql: String; Table: TStrings ): boolean;
//var
//  fPMsg: PChar;
//var Psql : pchar;
begin
  fError := SQLITE_ERROR;
  if fIsOpen then
  begin
    fPMsg := nil;
    fBusy := True;
    fTable := Table;
    if fTable <> nil then
      fTable.Clear;
   List_FieldName.clear;
   List_Field.clear;
   Nb_Champ:=-1;
    fError := SQLite_Exec(fSQLite, PChar(sql), @ExecCallback, Self, @fPMsg);
    SQLite_FreeMem(fPMsg);
    fChangeCount := SQLite_Changes(fSQLite);
    fTable := nil;
    fBusy := False;
    if Assigned(fOnQueryComplete) then
      fOnQueryComplete(Self);
  end;
  fMsg := ErrorMessage(fError);
  Result := (fError = SQLITE_OK);
end;

function TSQLite.Cancel: boolean;
begin
  Result := False;
  if fBusy and fIsOpen then
  begin
    do_SQLite_interrupt(fSQLite);
    fBusy := false;
    Result := True;
  end;
end;

procedure TSQLite.SetBusyTimeout(Timeout: Integer);
begin
  fBusyTimeout := Timeout;
  if fIsOpen then
  begin
    SQLite_Busy_Timeout(fSQLite, fBusyTimeout);
    if fBusyTimeout > 0 then
      SQLite_Busy_Handler(fSQLite, @BusyCallback, Self)
    else
      SQLite_Busy_Handler(fSQLite, nil, nil);
  end;
end;

function TSQLite.LastInsertRow: integer;
begin
  if fIsOpen then
    Result := SQLite_Last_Insert_RowID(fSQLite)
  else
    Result := -1;
end;

function TSQLite.ErrorMessage(ErrNo: Integer): string;
begin
  if ErrNo = 0 then
    Result := MsgNoError
  else
    Result := SQLite_Error_String(ErrNo);
end;

function TSQLite.IsComplete(Sql: String): boolean;
var Psql : pchar;
begin
  Psql:=StrAlloc (length(Sql)+1);
  strpcopy(Psql,Sql);
  Result := SQLite_Complete(Psql)<>0;
  strdispose(Psql);
end;

function TSQLite.DatabaseDetails(Table: TStrings): boolean;
begin
  Result := Query('SELECT * FROM SQLITE_MASTER;', Table);
end;

initialization

finalization

end.
