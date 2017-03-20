{$mode objfpc}
{$h+}
{*************************************************************
SQLite3 Object Oriented handle
O. Rinaudo - 2005 - orinaudo@gmail.com
G. Marcou  - 2007 - g.marcou@chimie.u-strasbg.fr
*************************************************************}

unit SQLite3db;

{$DEFINE LOAD_DYNAMICALLY} //=== ct9999 =============

interface

uses  Classes,strings,
{$ifdef LOAD_DYNAMICALLY}  //=== ct9999 =============
  sqlite3dyn;
{$else}
  sqlite3;
{$endif}
{*************************************************************}
{*************************************************************}
type
   TSQLiteExecCallback = function(Sender: pointer; Columns: Integer; ColumnValues: ppchar; ColumnNames: ppchar): integer of object; cdecl;
   TSQLiteBusyCallback = function(Sender: TObject; BusyCount: integer): longint of object; cdecl;
   TOnData = Procedure(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: String)  of object;
   TOnBusy = Procedure(Sender: TObject; BusyCount: integer; var Cancel: Boolean) of object;
   TOnQueryComplete = Procedure(Sender: TObject) of object;

   TSQLite = class(TObject)
{*************************************************************}
{*************************************************************}   
   private
   type
     TFieldList = class(TList)
       protected
         procedure Notify(Ptr: Pointer; Action: TListNotification); override;
     end;
   var
   fSQLite:Psqlite3;
   fMsg: String;
   fIsOpen: Boolean;
   fBusy: Boolean;
   fError: longint;
   fVersion: String;
   fEncoding: String;
   fTable: TStrings;
   fLstName: TStringList;
   fLstVal: TStringList;
   fOnData: TOnData;
   fOnBusy: TOnBusy;
   fOnQueryComplete: TOnQueryComplete;
   fBusyTimeout: longint;
   fPMsg: PAnsiChar;
   fChangeCount: longint;
   fNb_Champ :  Integer;
   fList_FieldName : TStringList;
   fList_Field : TList;
   procedure SetBusyTimeout(Timeout: integer);
{*************************************************************}
{*************************************************************}   
   public
   constructor Create(const DBFileName: String);
   destructor Destroy; override;
   function Query(const Sql: String; Table: TStrings ): boolean;
   function ErrorMessage(ErrNo: Integer): string;
   function IsComplete(Sql: String): boolean;
   function LastInsertRow: integer;
   function Cancel: boolean;
   function DatabaseDetails(Table: TStrings): boolean;
   property LastErrorMessage: string read fMsg;
   property LastError: longint read fError;
   property Version: String read fVersion;
   property Encoding: String read fEncoding;
   property OnData: TOnData read fOnData write fOnData;
   property OnBusy: TOnBusy read fOnBusy write fOnBusy;
   property OnQueryComplete: TOnQueryComplete read fOnQueryComplete write fOnQueryComplete;
   property BusyTimeout: longint read fBusyTimeout write SetBusyTimeout;
   property ChangeCount: longint read fChangeCount;
   property List_FieldName: TStringList read fList_FieldName;
   property List_Field: TList read fList_Field;
   property Nb_Champ: integer read fNb_Champ write fNb_Champ;
   procedure SQLOnData(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: String);
 end;

function Pas2SQLStr(const PasString: string): string;
function SQL2PasStr(const SQLString: string): string;
function QuoteStr(const s: string; QuoteChar: Char ): string;
function UnQuoteStr(const s: string; QuoteChar: Char ): string;
procedure ValueList(const ColumnNames, ColumnValues: String; NameValuePairs: TStrings);
{*************************************************************}
{*************************************************************}
implementation
Const
   DblQuote: Char      = '"';
   SngQuote: Char      = #39;
   DblSngQuote: String = #39#39;
   Crlf: String	       = #13#10;
   Tab: Char	       = #9;
var
   MsgNoError : String;
{*************************************************************}
{*************************************************************}
function QuoteStr(const s: string; QuoteChar: Char ): string;
{*************************************************************
SQlite3 enclosing string with quotes
G. Marcou
*************************************************************}
begin
   Result := Concat(QuoteChar, s, QuoteChar);
end;
{*************************************************************}
function UnQuoteStr(const s: string; QuoteChar: Char ): string;
{*************************************************************
SQlite3 Remove enclosing quotes from string
G. Marcou
*************************************************************}
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
{*************************************************************}
function Pas2SQLStr(const PasString: string): string;
{*************************************************************
SQlite3 SQL string are use double quotes, Pascal string use
single quote.
G. Marcou
*************************************************************}
var
   n : integer;
begin
   Result := SQL2PasStr(PasString);
   n := Length(Result);
   while n > 0 do
   begin
      if Result[n] = SngQuote then
	 Insert(SngQuote, Result, n);
      dec(n);
   end;
   Result := QuoteStr(Result,SngQuote);
end;
{*************************************************************}
function SQL2PasStr(const SQLString: string): string;
{*************************************************************
SQlite3 SQL string are use double quotes, Pascal string use
single quote.
G. Marcou
*************************************************************}
var
   p : integer;
begin
   Result := SQLString;
   p := pos(DblSngQuote, Result);
   while p > 0 do
   begin
      Delete(Result, p, 1);
      p := pos(DblSngQuote, Result);
   end;
   Result := UnQuoteStr(Result,SngQuote);
end;
{*************************************************************}
procedure ValueList(const ColumnNames, ColumnValues : String;
NameValuePairs					    : TStrings);
{*************************************************************
SQlite3 build (name=value) pair list
G. Marcou
*************************************************************}
var
   n		     : integer;
   lstName, lstValue : TStringList;
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
{*************************************************************}
{function SystemErrorMsg(ErrNo: Integer ): String;
var
  buf: PChar;
  size: Integer;
  MsgLen: Integer;
begin}
{  size := 256;
  GetMem(buf, size);
  If ErrNo = - 1 then
    ErrNo := GetLastError;
  MsgLen := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, 0, buf, size, nil);
  if MsgLen = 0 then
    Result := 'ERROR'
  else
    Result := buf;}
{end;}
{*************************************************************}
function BusyCallback(Sender : pointer;
BusyCount		     : integer): longint; cdecl;
{*************************************************************
SQlite3 busy callback 
G. Marcou
*************************************************************}
var
  bCancel: Boolean;
begin
  Result := -1;
  with TObject(Sender) as TSQLite do
  begin
    if Assigned(fOnBusy) then
    begin
      bCancel := False;
      fOnBusy(Tobject(Sender), BusyCount, bCancel);
      if bCancel then
        Result := 0;
    end;
  end;
end;
{*************************************************************}
function ExecCallback(Sender : Pointer;
Columns			     : Integer;
ColumnValues		     : PPChar;
ColumnNames		     : PPchar): integer; cdecl;
{*************************************************************
SQlite3 Build table and data from callback
G. Marcou
*************************************************************}
var
   PVal, PName : ^PChar;
   n	       : integer;
   sVal, sName : String;
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
{*************************************************************}
procedure TSQLite.SQLOnData(Sender : TObject;
Columns				   : Integer;
ColumnNames, ColumnValues	   : String);
{*************************************************************
SQlite3 Fill up field list names and field list values
G. Marcou
*************************************************************}
Var
   InterS,val : String;
   Field      : TStringList;
   {************************************************}
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
   end; { Pos1 }
   {*************************************************}
begin
   If Nb_Champ = -1 Then
   Begin {Put the fields name in List_FieldName}
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
   {Put the list of TStringList of value}
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

{*************************************************************}
procedure TSQLite.TFieldList.Notify(Ptr: Pointer; Action: TListNotification);
{*************************************************************}
begin
  if Action=lnDeleted then
    TObject(Ptr).Free;
  inherited;
end;

{*************************************************************}
constructor TSQLite.Create(const DBFileName: String);
{*************************************************************
SQlite3 constructor
G. Marcou
*************************************************************}
begin
   inherited Create;
   fList_FieldName := TStringList.Create;
   fList_Field := TFieldList.Create;
   fError := SQLITE_ERROR;
   fIsOpen := False;
   fLstName := TStringList.Create;
   fLstVal := TStringList.Create;
   fOnData := nil;
   fOnBusy := nil;
   fOnQueryComplete := nil;
   fChangeCount := 0;
   OnData:=@SQLOnData;
   sqlite3_open(PAnsiChar(DBFileName), @fSQLite);
   sqlite3_free(fPMsg);
   if fSQLite <> nil then
   begin
      //fVersion := String(SQLite_Version);
      //fEncoding := SQLite_Encoding;
      fIsOpen := True;
      fError := SQLITE_OK;
   end;
   fMsg := sqlite3_errmsg(fSQLite);
end;
{*************************************************************}
destructor TSQLite.Destroy;
{*************************************************************
SQLite3 destructor
G. Marcou
*************************************************************}
begin
   if fIsOpen then
      fError:=sqlite3_close(fSQLite);
   fIsOpen := False;
   fLstName.Free;
   fLstVal.Free;
   fSQLite := nil;
   fOnData := nil;
   fOnBusy := nil;
   fOnQueryComplete := nil;
   fLstName := nil;
   fLstVal := nil;
   fList_FieldName.destroy;
   fList_Field.destroy;
   inherited Destroy;
end;
{*************************************************************}
function TSQLite.Query(const Sql: String; Table: TStrings ): boolean;
{*************************************************************
SQLite3 query the database
G. Marcou
*************************************************************}
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
      fError := sqlite3_exec(fSQLite, PAnsiChar(sql), @ExecCallback, Self, @fPMsg);
      sqlite3_free(fPMsg);
      fChangeCount := sqlite3_changes(fSQLite);
      fTable := nil;
      fBusy := False;
      if Assigned(fOnQueryComplete) then
	 fOnQueryComplete(Self);
   end;
   fMsg := ErrorMessage(fError);
   Result := (fError = SQLITE_OK);
end;
{*************************************************************}
function TSQLite.Cancel: boolean;
{*************************************************************
SQLite3 interrupt database
G. Marcou
*************************************************************}
begin
   Result := False;
   if fBusy and fIsOpen then
   begin
      sqlite3_interrupt(fSQLite);
      fBusy := false;
      Result := True;
   end;
end;
{*************************************************************}
procedure TSQLite.SetBusyTimeout(Timeout: Integer);
{*************************************************************
SQLite3 busy timeout
G. Marcou
*************************************************************}
begin
   fBusyTimeout := Timeout;
   if fIsOpen then
   begin
      fError:=sqlite3_busy_timeout(fSQLite, fBusyTimeout);
      if fBusyTimeout > 0 then
	 sqlite3_busy_handler(fSQLite, @BusyCallback, Self)
      else
	 sqlite3_busy_handler(fSQLite, nil, nil);
   end;
end;
{*************************************************************}
function TSQLite.LastInsertRow: longint;
{*************************************************************
SQLite3 Get ID of the last inserted row
G. Marcou
*************************************************************}
begin
  if fIsOpen then
    Result := sqlite3_last_insert_rowid(fSQLite)
  else
    Result := -1;
end;
{*************************************************************}
function TSQLite.ErrorMessage(ErrNo: Integer): string;
{*************************************************************
SQLite3 Return comprehensive error message
G. Marcou
*************************************************************}
begin
  if ErrNo = 0 then
    Result := MsgNoError
  else
    Result := sqlite3_errmsg(fSQLite);
end;
{*************************************************************}
function TSQLite.IsComplete(Sql: String): boolean;
{*************************************************************
SQLite3 Return true when complete
G. Marcou
*************************************************************}
var Psql : pchar;
begin
   Psql:=StrAlloc (length(Sql)+1);
   strpcopy(Psql,Sql);
//   Writeln('Testing: ',psql);
   Result := sqlite3_complete(Psql)<>0;
   strdispose(Psql);
end;
{*************************************************************}
function TSQLite.DatabaseDetails(Table: TStrings): boolean;
{*************************************************************
SQLite3 Query the database
G. Marcou
*************************************************************}
begin
  Result := Query('SELECT * FROM SQLITE_MASTER;', Table);
end;
{*************************************************************}
{*************************************************************}
initialization

finalization

end.
