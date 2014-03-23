unit unt_json;

{$MODE objfpc}{$H+}


interface

{$DEFINE THREADSAFE}
{$DEFINE NEW_STYLE_GENERATE}

uses
  Classes,
  SysUtils,
  variants;

type
  TlkJSONtypes = (jsBase, jsNumber, jsString, jsBoolean, jsNull,
    jsList, jsObject);

  TlkJSONbase = class
  protected
    function GetValue: variant; virtual;
    procedure SetValue(const AValue: variant); virtual;
    function GetChild(idx: Integer): TlkJSONbase; virtual;
    procedure SetChild(idx: Integer; const AValue: TlkJSONbase);
      virtual;
    function GetCount: Integer; virtual;
    function GetField(AName: Variant):TlkJSONbase; virtual;
  public
    property Field[AName: Variant]: TlkJSONbase read GetField;
    property Count: Integer read GetCount;
    property Child[idx: Integer]: TlkJSONbase read GetChild write SetChild;
    property Value: variant read GetValue write SetValue;
    class function SelfType: TlkJSONtypes; virtual;
    class function SelfTypeName: string; virtual;
  end;

  TlkJSONnumber = class(TlkJSONbase)
  protected
    FValue: extended;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure AfterConstruction; override;
    class function Generate(AValue: extended = 0): TlkJSONnumber;
    class function SelfType: TlkJSONtypes; override;
    class function SelfTypeName: string; override;
  end;

  TlkJSONstring = class(TlkJSONbase)
  protected
    FValue: WideString;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure AfterConstruction; override;
    class function Generate(const wsValue: WideString = ''):
      TlkJSONstring;
    class function SelfType: TlkJSONtypes; override;
    class function SelfTypeName: string; override;
  end;

  TlkJSONboolean = class(TlkJSONbase)
  protected
    FValue: Boolean;
    function GetValue: Variant; override;
    procedure SetValue(const AValue: Variant); override;
  public
    procedure AfterConstruction; override;
    class function Generate(AValue: Boolean = true): TlkJSONboolean;
    class function SelfType: TlkJSONtypes; override;
    class function SelfTypeName: string; override;
  end;

  TlkJSONnull = class(TlkJSONbase)
  protected
    function GetValue: Variant; override;
    function Generate: TlkJSONnull;
  public
    class function SelfType: TlkJSONtypes; override;
    class function SelfTypeName: string; override;
  end;

  TlkJSONFuncEnum = procedure(ElName: string; Elem: TlkJSONbase;
    data: pointer; var Continue: Boolean) of object;

  TlkJSONcustomlist = class(TlkJSONbase)
  protected
//    FValue: array of TlkJSONbase;
    fList: TList;
    function GetCount: Integer; override;
    function GetChild(idx: Integer): TlkJSONbase; override;
    procedure SetChild(idx: Integer; const AValue: TlkJSONbase);
      override;
    function ForEachElement(idx: Integer; var nm: string):
      TlkJSONbase; virtual;

    function GetField(AName: Variant):TlkJSONbase; override;

    function _Add(obj: TlkJSONbase): Integer; virtual;
    procedure _Delete(iIndex: Integer); virtual;
    function _IndexOf(obj: TlkJSONbase): Integer; virtual;
  public
    procedure ForEach(fnCallBack: TlkJSONFuncEnum; pUserData:
      pointer);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function getInt(idx: Integer): Integer; virtual;
    function getString(idx: Integer): string; virtual;
    function getWideString(idx: Integer): WideString; virtual;
    function getDouble(idx: Integer): Double; virtual;
    function getBoolean(idx: Integer): Boolean; virtual;
  end;

  TlkJSONlist = class(TlkJSONcustomlist)
  protected
  public
    function Add(obj: TlkJSONbase): Integer; overload;

    function Add(aboolean: Boolean): Integer; overload;
    function Add(nmb: double): Integer; overload;
    function Add(s: string): Integer; overload;
    function Add(const ws: WideString): Integer; overload;
    function Add(inmb: Integer): Integer; overload;

    procedure Delete(idx: Integer);
    function IndexOf(obj: TlkJSONbase): Integer;
    class function Generate: TlkJSONlist;
    class function SelfType: TlkJSONtypes; override;
    class function SelfTypeName: string; override;
  end;

  TlkJSONobjectmethod = class(TlkJSONbase)
  protected
    FValue: TlkJSONbase;
    FName: WideString;
    procedure SetName(const AValue: WideString);
  public
    property ObjValue: TlkJSONbase read FValue;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Name: WideString read FName write SetName;
    class function Generate(const aname: WideString; aobj: TlkJSONbase):
      TlkJSONobjectmethod;
  end;

{$IFDEF USE_HASH}
  PlkHashItem = ^TlkHashItem;
  TlkHashItem = packed record
    hash: cardinal;
    index: Integer;
  end;

  TlkHashFunction = function(const ws: WideString): cardinal of
    object;

  TlkHashTable = class
  private
    FParent: TObject; // TCB:parent for check chaining op.
    FHashFunction: TlkHashFunction;
    procedure SetHashFunction(const AValue: TlkHashFunction);
  protected
    a_x: array[0..255] of TList;
    procedure hswap(j, k, l: Integer);
    function InTable(const ws: WideString; var i, j, k: cardinal):
      Boolean;
  public
    function counters: string;

    function DefaultHashOf(const ws: WideString): cardinal;
    function SimpleHashOf(const ws: WideString): cardinal;

    property HashOf: TlkHashFunction read FHashFunction write
      SetHashFunction;

    function IndexOf(const ws: WideString): Integer;

    procedure AddPair(const ws: WideString; idx: Integer);
    procedure Delete(const ws: WideString);

    constructor Create;
    destructor Destroy; override;
  end;

{$ELSE}

// implementation based on "Arne Andersson, Balanced Search Trees Made Simpler"

  PlkBalNode = ^TlkBalNode;
  TlkBalNode = packed record
    left,right: PlkBalNode;
    level: byte;
    key: Integer;
    nm: WideString;
  end;

  TlkBalTree = class
  protected
    fdeleted,flast,fbottom,froot: PlkBalNode;
    procedure skew(var t:PlkBalNode);
    procedure split(var t:PlkBalNode);
  public
    function counters: string;

    procedure Clear;

    function Insert(const ws: WideString; x: Integer): Boolean;
    function Delete(const ws: WideString): Boolean;

    function IndexOf(const ws: WideString): Integer;

    constructor Create;
    destructor Destroy; override;
  end;
{$ENDIF USE_HASH}

  TlkJSONobject = class(TlkJSONcustomlist)
  protected
{$IFDEF USE_HASH}
    ht: TlkHashTable;
{$ELSE}
    ht: TlkBalTree;
{$ENDIF USE_HASH}
    FUseHash: Boolean;
    function GetFieldByIndex(idx: Integer): TlkJSONbase;
    function GetNameOf(idx: Integer): WideString;
    procedure SetFieldByIndex(idx: Integer; const AValue: TlkJSONbase);
{$IFDEF USE_HASH}
    function GetHashTable: TlkHashTable;
{$ELSE}
    function GetHashTable: TlkBalTree;
{$ENDIF USE_HASH}
    function ForEachElement(idx: Integer; var nm: string): TlkJSONbase;
      override;
    function GetField(AName: Variant):TlkJSONbase; override;
  public
    property UseHash: Boolean read FUseHash;
{$IFDEF USE_HASH}
    property HashTable: TlkHashTable read GetHashTable;
{$ELSE}
    property HashTable: TlkBalTree read GetHashTable;
{$ENDIF USE_HASH}

    function Add(const aname: WideString; aobj: TlkJSONbase): Integer;
      overload;

    function OldGetField(nm: WideString): TlkJSONbase;
    procedure OldSetField(nm: WideString; const AValue: TlkJSONbase);

    function Add(const aname: WideString; aboolean: Boolean): Integer; overload;
    function Add(const aname: WideString; nmb: double): Integer; overload;
    function Add(const aname: WideString; s: string): Integer; overload;
    function Add(const aname: WideString; const ws: WideString): Integer;
      overload;
    function Add(const aname: WideString; inmb: Integer): Integer; overload;

    procedure Delete(idx: Integer);
    function IndexOfName(const aname: WideString): Integer;
    function IndexOfObject(aobj: TlkJSONbase): Integer;
    property Field[nm: WideString]: TlkJSONbase read OldGetField
      write OldSetField; default;

    constructor Create(bUseHash: Boolean = true);
    destructor Destroy; override;

    class function Generate(AUseHash: Boolean = true): TlkJSONobject;
    class function SelfType: TlkJSONtypes; override;
    class function SelfTypeName: string; override;

    property FieldByIndex[idx: Integer]: TlkJSONbase read GetFieldByIndex
    write SetFieldByIndex;
    property NameOf[idx: Integer]: WideString read GetNameOf;

    function getDouble(idx: Integer): Double; overload; override;
    function getInt(idx: Integer): Integer; overload; override;
    function getString(idx: Integer): string; overload; override;
    function getWideString(idx: Integer): WideString; overload; override;
    function getBoolean(idx: Integer): Boolean; overload; override;

    function {$ifdef TCB_EXT}getDoubleFromName{$else}getDouble{$endif}
      (nm: string): Double; overload;
    function {$ifdef TCB_EXT}getIntFromName{$else}getInt{$endif}
      (nm: string): Integer; overload;
    function {$ifdef TCB_EXT}getStringFromName{$else}getString{$endif}
      (nm: string): string; overload;
    function {$ifdef TCB_EXT}getWideStringFromName{$else}getWideString{$endif}
      (nm: string): WideString; overload;
    function {$ifdef TCB_EXT}getBooleanFromName{$else}getBoolean{$endif}
      (nm: string): Boolean; overload;
  end;

  TlkJSON = class
  public
    class function ParseText(const txt: string): TlkJSONbase;
    class function GenerateText(obj: TlkJSONbase): string;
  end;

{$IFNDEF KOL}
  TlkJSONstreamed = class(TlkJSON)
    class function LoadFromStream(src: TStream): TlkJSONbase;
    class procedure SaveToStream(obj: TlkJSONbase; dst: TStream);
    class function LoadFromFile(srcname: string): TlkJSONbase;
    class procedure SaveToFile(obj: TlkJSONbase; dstname: string);
  end;
{$ENDIF}

function GenerateReadableText(vObj: TlkJSONbase; var vLevel:
  Integer): string;

implementation

uses math,strutils;

type
  ElkIntException = class(Exception)
  public
    idx: Integer;
    constructor Create(AIdx: Integer; AMsg: string);
  end;

// author of next two functions is Kusnassriyanto Saiful Bahri

function Indent(vTab: Integer): string;
begin
  result := DupeString('  ', vTab);
end;

function GenerateReadableText(vObj: TlkJSONbase; var vLevel:
  Integer): string;
var
  i: Integer;
  vStr: string;
  xs: TlkJSONstring;
begin
  vLevel := vLevel + 1;
  if vObj is TlkJSONObject then
    begin
      vStr := '';
      for i := 0 to TlkJSONobject(vObj).Count - 1 do
        begin
          if vStr <> '' then
            begin
              vStr := vStr + ','#13#10;
            end;
          vStr := vStr + Indent(vLevel) +
            GenerateReadableText(TlkJSONobject(vObj).Child[i], vLevel);
        end;
      if vStr <> '' then
        begin
          vStr := '{'#13#10 + vStr + #13#10 + Indent(vLevel - 1) + '}';
        end
      else
        begin
          vStr := '{}';
        end;
      result := vStr;
    end
  else if vObj is TlkJSONList then
    begin
      vStr := '';
      for i := 0 to TlkJSONList(vObj).Count - 1 do
        begin
          if vStr <> '' then
            begin
              vStr := vStr + ','#13#10;
            end;
          vStr := vStr + Indent(vLevel) +
              GenerateReadableText(TlkJSONList(vObj).Child[i], vLevel);
        end;
      if vStr <> '' then
        begin
          vStr := '['#13#10 + vStr + #13#10 + Indent(vLevel - 1) + ']';
        end
      else
        begin
          vStr := '[]';
        end;
      result := vStr;
    end
  else if vObj is TlkJSONobjectmethod then
    begin
      vStr := '';
      xs := TlkJSONstring.Create;
      try
        xs.Value := TlkJSONobjectMethod(vObj).Name;
        vStr := GenerateReadableText(xs, vLevel);
        vLevel := vLevel - 1;
        vStr := vStr + ':' + GenerateReadableText(TlkJSONbase(
          TlkJSONobjectmethod(vObj).ObjValue), vLevel);
      //vStr := vStr + ':' + GenerateReadableText(TlkJSONbase(vObj), vLevel);
        vLevel := vLevel + 1;
        result := vStr;
      finally
        xs.Free;
      end;
    end
  else
    begin
      if vObj is TlkJSONobjectmethod then
        begin
          if TlkJSONobjectMethod(vObj).Name <> '' then
            begin
            end;
        end;
      result := TlkJSON.GenerateText(vObj);
    end;
  vLevel := vLevel - 1;
end;

// author of this routine is IVO GELOV

function code2utf(iNumber: Integer): UTF8String;
begin
  if iNumber < 128 then Result := chr(iNumber)
  else if iNumber < 2048 then
    Result := chr((iNumber shr 6) + 192) + chr((iNumber and 63) + 128)
  else if iNumber < 65536 then
    Result := chr((iNumber shr 12) + 224) + chr(((iNumber shr 6) and
      63) + 128) + chr((iNumber and 63) + 128)
  else if iNumber < 2097152 then
    Result := chr((iNumber shr 18) + 240) + chr(((iNumber shr 12) and
      63) + 128) + chr(((iNumber shr 6) and 63) + 128) +
      chr((iNumber and 63) + 128);
end;

{ TlkJSONbase }

function TlkJSONbase.GetChild(idx: Integer): TlkJSONbase;
begin
  result := nil;
end;

function TlkJSONbase.GetCount: Integer;
begin
  result := 0;
end;

function TlkJSONbase.GetField(AName: Variant):TlkJSONbase;
begin
  result := self;
end;

function TlkJSONbase.GetValue: variant;
begin
  result := variants.Null;
end;

class function TlkJSONbase.SelfType: TlkJSONtypes;
begin
  result := jsBase;
end;

class function TlkJSONbase.SelfTypeName: string;
begin
  result := 'jsBase';
end;

procedure TlkJSONbase.SetChild(idx: Integer; const AValue:
  TlkJSONbase);
begin

end;

procedure TlkJSONbase.SetValue(const AValue: variant);
begin

end;

{ TlkJSONnumber }

procedure TlkJSONnumber.AfterConstruction;
begin
  inherited;
  FValue := 0;
end;

class function TlkJSONnumber.Generate(AValue: extended):
  TlkJSONnumber;
begin
  result := TlkJSONnumber.Create;
  result.FValue := AValue;
end;

function TlkJSONnumber.GetValue: Variant;
begin
  result := FValue;
end;

class function TlkJSONnumber.SelfType: TlkJSONtypes;
begin
  result := jsNumber;
end;

class function TlkJSONnumber.SelfTypeName: string;
begin
  result := 'jsNumber';
end;

procedure TlkJSONnumber.SetValue(const AValue: Variant);
begin
  FValue := VarAsType(AValue, varDouble);
end;

{ TlkJSONstring }

procedure TlkJSONstring.AfterConstruction;
begin
  inherited;
  FValue := '';
end;

class function TlkJSONstring.Generate(const wsValue: WideString):
  TlkJSONstring;
begin
  result := TlkJSONstring.Create;
  result.FValue := wsValue;
end;

function TlkJSONstring.GetValue: Variant;
begin
  result := FValue;
end;

class function TlkJSONstring.SelfType: TlkJSONtypes;
begin
  result := jsString;
end;

class function TlkJSONstring.SelfTypeName: string;
begin
  result := 'jsString';
end;

procedure TlkJSONstring.SetValue(const AValue: Variant);
begin
  FValue := VarToWideStr(AValue);
end;

{ TlkJSONboolean }

procedure TlkJSONboolean.AfterConstruction;
begin
  FValue := false;
end;

class function TlkJSONboolean.Generate(AValue: Boolean):
  TlkJSONboolean;
begin
  result := TlkJSONboolean.Create;
  result.Value := AValue;
end;

function TlkJSONboolean.GetValue: Variant;
begin
  result := FValue;
end;

class function TlkJSONboolean.SelfType: TlkJSONtypes;
begin
  Result := jsBoolean;
end;

class function TlkJSONboolean.SelfTypeName: string;
begin
  Result := 'jsBoolean';
end;

procedure TlkJSONboolean.SetValue(const AValue: Variant);
begin
  FValue := boolean(AValue);
end;

{ TlkJSONnull }

function TlkJSONnull.Generate: TlkJSONnull;
begin
  result := TlkJSONnull.Create;
end;

function TlkJSONnull.GetValue: Variant;
begin
  result := variants.Null;
end;

class function TlkJSONnull.SelfType: TlkJSONtypes;
begin
  result := jsNull;
end;

class function TlkJSONnull.SelfTypeName: string;
begin
  result := 'jsNull';
end;

{ TlkJSONcustomlist }

function TlkJSONcustomlist._Add(obj: TlkJSONbase): Integer;
begin
  if not Assigned(obj) then
    begin
      result := -1;
      exit;
    end;
  result := fList.Add(obj);
end;

procedure TlkJSONcustomlist.AfterConstruction;
begin
  inherited;
  fList := TList.Create;
end;

procedure TlkJSONcustomlist.BeforeDestruction;
var
  i: Integer;
begin
  for i := (Count - 1) downto 0 do _Delete(i);
  fList.Free;
  inherited;
end;

// renamed

procedure TlkJSONcustomlist._Delete(iIndex: Integer);
var
  idx: Integer;
begin
  if not ((iIndex < 0) or (iIndex >= Count)) then
    begin
      if fList.Items[iIndex] <> nil then
        TlkJSONbase(fList.Items[iIndex]).Free;
      idx := pred(fList.Count);
      if iIndex<idx then
        begin
          fList.Items[iIndex] := fList.Items[idx];
          fList.Delete(idx);
        end
      else
        begin
          fList.Delete(iIndex);
        end;
    end;
end;

function TlkJSONcustomlist.GetChild(idx: Integer): TlkJSONbase;
begin
  if (idx < 0) or (idx >= Count) then
    begin
      result := nil;
    end
  else
    begin
      result := TlkJSONbase(fList.Items[idx]);
    end;
end;

function TlkJSONcustomlist.GetCount: Integer;
begin
  result := fList.Count;
end;

function TlkJSONcustomlist._IndexOf(obj: TlkJSONbase): Integer;
begin
  result := fList.IndexOf(obj);
end;

procedure TlkJSONcustomlist.SetChild(idx: Integer; const AValue:
  TlkJSONbase);
begin
  if not ((idx < 0) or (idx >= Count)) then
    begin
      if fList.Items[idx] <> nil then
        TlkJSONbase(fList.Items[idx]).Free;
      fList.Items[idx] := AValue;
    end;
end;

procedure TlkJSONcustomlist.ForEach(fnCallBack: TlkJSONFuncEnum;
  pUserData:
  pointer);
var
  iCount: Integer;
  IsContinue: Boolean;
  anJSON: TlkJSONbase;
  wsObject: string;
begin
  if not assigned(fnCallBack) then exit;
  IsContinue := true;
  for iCount := 0 to GetCount - 1 do
    begin
      anJSON := ForEachElement(iCount, wsObject);
      if assigned(anJSON) then
        fnCallBack(wsObject, anJSON, pUserData, IsContinue);
      if not IsContinue then break;
    end;
end;

///---- renamed to here

function TlkJSONcustomlist.GetField(AName: Variant):TlkJSONbase;
var
  index: Integer;
begin
  if VarIsNumeric(AName) then
    begin
      index := integer(AName);
      result := GetChild(index);
    end
  else
    begin
      result := inherited GetField(AName);
    end;
end;

function TlkJSONcustomlist.ForEachElement(idx: Integer; var nm:
  string): TlkJSONbase;
begin
  nm := inttostr(idx);
  result := GetChild(idx);
end;

function TlkJSONcustomlist.getDouble(idx: Integer): Double;
var
  jn: TlkJSONnumber;
begin
  jn := Child[idx] as TlkJSONnumber;
  if not assigned(jn) then result := 0
  else result := jn.Value;
end;

function TlkJSONcustomlist.getInt(idx: Integer): Integer;
var
  jn: TlkJSONnumber;
begin
  jn := Child[idx] as TlkJSONnumber;
  if not assigned(jn) then result := 0
  else result := round(int(jn.Value));
end;

function TlkJSONcustomlist.getString(idx: Integer): string;
var
  js: TlkJSONstring;
begin
  js := Child[idx] as TlkJSONstring;
  if not assigned(js) then result := ''
  else result := VarToStr(js.Value);
end;

function TlkJSONcustomlist.getWideString(idx: Integer): WideString;
var
  js: TlkJSONstring;
begin
  js := Child[idx] as TlkJSONstring;
  if not assigned(js) then result := ''
  else result := VarToWideStr(js.Value);
end;

function TlkJSONcustomlist.getBoolean(idx: Integer): Boolean;
var
  jb: TlkJSONboolean;
begin
  jb := Child[idx] as TlkJSONboolean;
  if not assigned(jb) then result := false
  else result := jb.Value;
end;

{ TlkJSONobjectmethod }

procedure TlkJSONobjectmethod.AfterConstruction;
begin
  inherited;
  FValue := nil;
  FName := '';
end;

procedure TlkJSONobjectmethod.BeforeDestruction;
begin
  FName := '';
  if FValue <> nil then
    begin
      FValue.Free;
      FValue := nil;
    end;
  inherited;
end;

class function TlkJSONobjectmethod.Generate(const aname: WideString;
  aobj: TlkJSONbase): TlkJSONobjectmethod;
begin
  result := TlkJSONobjectmethod.Create;
  result.FName := aname;
  result.FValue := aobj;
end;

procedure TlkJSONobjectmethod.SetName(const AValue: WideString);
begin
  FName := AValue;
end;

{ TlkJSONlist }

function TlkJSONlist.Add(obj: TlkJSONbase): Integer;
begin
  result := _Add(obj);
end;

function TlkJSONlist.Add(nmb: double): Integer;
begin
  Result := self.Add(TlkJSONnumber.Generate(nmb));
end;

function TlkJSONlist.Add(aboolean: Boolean): Integer;
begin
  Result := self.Add(TlkJSONboolean.Generate(aboolean));
end;

function TlkJSONlist.Add(inmb: Integer): Integer;
begin
  Result := self.Add(TlkJSONnumber.Generate(inmb));
end;

function TlkJSONlist.Add(const ws: WideString): Integer;
begin
  Result := self.Add(TlkJSONstring.Generate(ws));
end;

function TlkJSONlist.Add(s: string): Integer;
begin
  Result := self.Add(TlkJSONstring.Generate(s));
end;

procedure TlkJSONlist.Delete(idx: Integer);
begin
  _Delete(idx);
end;

class function TlkJSONlist.Generate: TlkJSONlist;
begin
  result := TlkJSONlist.Create;
end;

function TlkJSONlist.IndexOf(obj: TlkJSONbase): Integer;
begin
  result := _IndexOf(obj);
end;

class function TlkJSONlist.SelfType: TlkJSONtypes;
begin
  result := jsList;
end;

class function TlkJSONlist.SelfTypeName: string;
begin
  result := 'jsList';
end;

{ TlkJSONobject }

function TlkJSONobject.Add(const aname: WideString; aobj:
  TlkJSONbase):
  Integer;
var
  mth: TlkJSONobjectmethod;
begin
  if not assigned(aobj) then
    begin
      result := -1;
      exit;
    end;
  mth := TlkJSONobjectmethod.Create;
  mth.FName := aname;
  mth.FValue := aobj;
  result := self._Add(mth);
  if FUseHash then
{$IFDEF USE_HASH}
    ht.AddPair(aname, result);
{$ELSE}
    ht.Insert(aname, result);
{$ENDIF USE_HASH}
end;

procedure TlkJSONobject.Delete(idx: Integer);
var
  i,j,k:cardinal;
  mth: TlkJSONobjectmethod;
begin
  if (idx >= 0) and (idx < Count) then
    begin
//      mth := FValue[idx] as TlkJSONobjectmethod;
      mth := TlkJSONobjectmethod(fList.Items[idx]);
      if FUseHash then
        begin
          ht.Delete(mth.FName);
        end;
    end;
  _Delete(idx);
{$ifdef USE_HASH}
  if (idx<Count) and (FUseHash) then
    begin
      mth := TlkJSONobjectmethod(fList.Items[idx]);
      ht.AddPair(mth.FName,idx);
    end;
{$endif}
end;

class function TlkJSONobject.Generate(AUseHash: Boolean = true):
  TlkJSONobject;
begin
  result := TlkJSONobject.Create(AUseHash);
end;

function TlkJSONobject.OldGetField(nm: WideString): TlkJSONbase;
var
  mth: TlkJSONobjectmethod;
  i: Integer;
begin
  i := IndexOfName(nm);
  if i = -1 then
    begin
      result := nil;
    end
  else
    begin
//      mth := TlkJSONobjectmethod(FValue[i]);
      mth := TlkJSONobjectmethod(fList.Items[i]);
      result := mth.FValue;
    end;
end;

function TlkJSONobject.IndexOfName(const aname: WideString): Integer;
var
  mth: TlkJSONobjectmethod;
  i: Integer;
begin
  if not FUseHash then
    begin
      result := -1;
      for i := 0 to Count - 1 do
        begin
//          mth := TlkJSONobjectmethod(FValue[i]);
          mth := TlkJSONobjectmethod(fList.Items[i]);
          if mth.Name = aname then
            begin
              result := i;
              break;
            end;
        end;
    end
  else
    begin
      result := ht.IndexOf(aname);
    end;
end;

function TlkJSONobject.IndexOfObject(aobj: TlkJSONbase): Integer;
var
  mth: TlkJSONobjectmethod;
  i: Integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
    begin
//      mth := TlkJSONobjectmethod(FValue[i]);
      mth := TlkJSONobjectmethod(fList.Items[i]);
      if mth.FValue = aobj then
        begin
          result := i;
          break;
        end;
    end;
end;

procedure TlkJSONobject.OldSetField(nm: WideString; const AValue:
  TlkJSONbase);
var
  mth: TlkJSONobjectmethod;
  i: Integer;
begin
  i := IndexOfName(nm);
  if i <> -1 then
    begin
//      mth := TlkJSONobjectmethod(FValue[i]);
      mth := TlkJSONobjectmethod(fList.Items[i]);
      mth.FValue := AValue;
    end;
end;

function TlkJSONobject.Add(const aname: WideString; nmb: double):
  Integer;
begin
  Result := self.Add(aname, TlkJSONnumber.Generate(nmb));
end;

function TlkJSONobject.Add(const aname: WideString; aboolean: Boolean):
  Integer;
begin
  Result := self.Add(aname, TlkJSONboolean.Generate(aboolean));
end;

function TlkJSONobject.Add(const aname: WideString; s: string):
  Integer;
begin
  Result := self.Add(aname, TlkJSONstring.Generate(s));
end;

function TlkJSONobject.Add(const aname: WideString; inmb: Integer):
  Integer;
begin
  Result := self.Add(aname, TlkJSONnumber.Generate(inmb));
end;

function TlkJSONobject.Add(const aname, ws: WideString): Integer;
begin
  Result := self.Add(aname, TlkJSONstring.Generate(ws));
end;

class function TlkJSONobject.SelfType: TlkJSONtypes;
begin
  Result := jsObject;
end;

class function TlkJSONobject.SelfTypeName: string;
begin
  Result := 'jsObject';
end;

function TlkJSONobject.GetFieldByIndex(idx: Integer): TlkJSONbase;
var
  nm: WideString;
begin
  nm := GetNameOf(idx);
  if nm <> '' then
    begin
      result := Field[nm];
    end
  else
    begin
      result := nil;
    end;
end;

function TlkJSONobject.GetNameOf(idx: Integer): WideString;
var
  mth: TlkJSONobjectmethod;
begin
  if (idx < 0) or (idx >= Count) then
    begin
      result := '';
    end
  else
    begin
      mth := Child[idx] as TlkJSONobjectmethod;
      result := mth.Name;
    end;
end;

procedure TlkJSONobject.SetFieldByIndex(idx: Integer;
  const AValue: TlkJSONbase);
var
  nm: WideString;
begin
  nm := GetNameOf(idx);
  if nm <> '' then
    begin
      Field[nm] := AValue;
    end;
end;

function TlkJSONobject.ForEachElement(idx: Integer;
  var nm: string): TlkJSONbase;
begin
  nm := GetNameOf(idx);
  result := GetFieldByIndex(idx);
end;

function TlkJSONobject.GetField(AName: Variant):TlkJSONbase;
begin
  if VarIsStr(AName) then
    result := OldGetField(VarToWideStr(AName))
  else
    result := inherited GetField(AName);
end;

{$IFDEF USE_HASH}
function TlkJSONobject.GetHashTable: TlkHashTable;
{$ELSE}
function TlkJSONobject.GetHashTable: TlkBalTree;
{$ENDIF USE_HASH}
begin
  result := ht;
end;

constructor TlkJSONobject.Create(bUseHash: Boolean);
begin
  inherited Create;
  FUseHash := bUseHash;
{$IFDEF USE_HASH}
  ht := TlkHashTable.Create;
  ht.FParent := self;
{$ELSE}
  ht := TlkBalTree.Create;
{$ENDIF}
end;

destructor TlkJSONobject.Destroy;
begin
  if assigned(ht) then FreeAndNil(ht);
  inherited;
end;

function TlkJSONobject.getDouble(idx: Integer): Double;
var
  jn: TlkJSONnumber;
begin
  jn := FieldByIndex[idx] as TlkJSONnumber;
  if not assigned(jn) then result := 0
  else result := jn.Value;
end;

function TlkJSONobject.getInt(idx: Integer): Integer;
var
  jn: TlkJSONnumber;
begin
  jn := FieldByIndex[idx] as TlkJSONnumber;
  if not assigned(jn) then result := 0
  else result := round(int(jn.Value));
end;

function TlkJSONobject.getString(idx: Integer): string;
var
  js: TlkJSONstring;
begin
  js := FieldByIndex[idx] as TlkJSONstring;
  if not assigned(js) then result := ''
  else result := vartostr(js.Value);
end;

function TlkJSONobject.getWideString(idx: Integer): WideString;
var
  js: TlkJSONstring;
begin
  js := FieldByIndex[idx] as TlkJSONstring;
  if not assigned(js) then result := ''
  else result := VarToWideStr(js.Value);
end;

{$ifdef TCB_EXT}
function TlkJSONobject.getDoubleFromName(nm: string): Double;
{$else}
function TlkJSONobject.getDouble(nm: string): Double;
{$endif}
begin
  result := getDouble(IndexOfName(nm));
end;

{$ifdef TCB_EXT}
function TlkJSONobject.getIntFromName(nm: string): Integer;
{$else}
function TlkJSONobject.getInt(nm: string): Integer;
{$endif}
begin
  result := getInt(IndexOfName(nm));
end;

{$ifdef TCB_EXT}
function TlkJSONobject.getStringFromName(nm: string): string;
{$else}
function TlkJSONobject.getString(nm: string): string;
{$endif}
begin
  result := getString(IndexOfName(nm));
end;

{$ifdef TCB_EXT}
function TlkJSONobject.getWideStringFromName(nm: string): WideString;
{$else}
function TlkJSONobject.getWideString(nm: string): WideString;
{$endif}
begin
  result := getWideString(IndexOfName(nm));
end;

function TlkJSONobject.getBoolean(idx: Integer): Boolean;
var
  jb: TlkJSONboolean;
begin
  jb := FieldByIndex[idx] as TlkJSONboolean;
  if not assigned(jb) then result := false
  else result := jb.Value;
end;

{$ifdef TCB_EXT}
function TlkJSONobject.getBooleanFromName(nm: string): Boolean;
{$else}
function TlkJSONobject.getBoolean(nm: string): Boolean;
{$endif}
begin
  result := getBoolean(IndexOfName(nm));
end;

{ TlkJSON }

class function TlkJSON.GenerateText(obj: TlkJSONbase): string;
var
{$IFDEF HAVE_FORMATSETTING}
  fs: TFormatSettings;
{$ENDIF}
  pt1, pt0, pt2: PChar;
  ptsz: cardinal;

{$IFNDEF NEW_STYLE_GENERATE}

  function gn_base(obj: TlkJSONbase): string;
  var
    ws: string;
    i, j: Integer;
    xs: TlkJSONstring;
  begin
    result := '';
    if not assigned(obj) then exit;
    if obj is TlkJSONnumber then
      begin
{$IFDEF HAVE_FORMATSETTING}
        result := FloatToStr(TlkJSONnumber(obj).FValue, fs);
{$ELSE}
        result := FloatToStr(TlkJSONnumber(obj).FValue);
        i := pos(DecimalSeparator, result);
        if (DecimalSeparator <> '.') and (i > 0) then
          result[i] := '.';
{$ENDIF}
      end
    else if obj is TlkJSONstring then
      begin
        ws := UTF8Encode(TlkJSONstring(obj).FValue);
        i := 1;
        result := '"';
        while i <= length(ws) do
          begin
            case ws[i] of
              '/', '\', '"': result := result + '\' + ws[i];
              #8: result := result + '\b';
              #9: result := result + '\t';
              #10: result := result + '\n';
              #13: result := result + '\r';
              #12: result := result + '\f';
            else
              if ord(ws[i]) < 32 then
                result := result + '\u' + inttohex(ord(ws[i]), 4)
              else
                result := result + ws[i];
            end;
            inc(i);
          end;
        result := result + '"';
      end
    else if obj is TlkJSONboolean then
      begin
        if TlkJSONboolean(obj).FValue then
          result := 'true'
        else
          result := 'false';
      end
    else if obj is TlkJSONnull then
      begin
        result := 'null';
      end
    else if obj is TlkJSONlist then
      begin
        result := '[';
        j := TlkJSONobject(obj).Count - 1;
        for i := 0 to j do
          begin
            if i > 0 then result := result + ',';
            result := result + gn_base(TlkJSONlist(obj).Child[i]);
          end;
        result := result + ']';
      end
    else if obj is TlkJSONobjectmethod then
      begin
        try
          xs := TlkJSONstring.Create;
          xs.FValue := TlkJSONobjectmethod(obj).FName;
          result := gn_base(TlkJSONbase(xs)) + ':';
          result := result +
            gn_base(TlkJSONbase(TlkJSONobjectmethod(obj).FValue));
        finally
          if assigned(xs) then FreeAndNil(xs);
        end;
      end
    else if obj is TlkJSONobject then
      begin
        result := '{';
        j := TlkJSONobject(obj).Count - 1;
        for i := 0 to j do
          begin
            if i > 0 then result := result + ',';
            result := result + gn_base(TlkJSONobject(obj).Child[i]);
          end;
        result := result + '}';
      end;
  end;
{$ELSE}

  procedure get_more_memory;
  var
    delta: cardinal;
  begin
    delta := 50000;
    if pt0 = nil then
      begin
        pt0 := AllocMem(delta);
        ptsz := 0;
        pt1 := pt0;
      end
    else
      begin
        ReallocMem(pt0, ptsz + delta);
        pt1 := pointer(cardinal(pt0) + ptsz);
      end;
    ptsz := ptsz + delta;
    pt2 := pointer(cardinal(pt1) + delta);
  end;

  procedure mem_ch(ch: char);
  begin
    if pt1 >= pt2 then get_more_memory;
    pt1^ := ch;
    inc(pt1);
  end;

  procedure mem_write(rs: string);
  var
    i: Integer;
  begin
    for i := 1 to length(rs) do
      begin
        if pt1 >= pt2 then get_more_memory;
        pt1^ := rs[i];
        inc(pt1);
      end;
  end;

  procedure gn_base(obj: TlkJSONbase);
  var
    ws: string;
    i, j: Integer;
    xs: TlkJSONstring;
  begin
    if not assigned(obj) then exit;
    if obj is TlkJSONnumber then
      begin
{$IFDEF HAVE_FORMATSETTING}
        mem_write(FloatToStr(TlkJSONnumber(obj).FValue, fs));
{$ELSE}
        ws := FloatToStr(TlkJSONnumber(obj).FValue);
        i := pos(DecimalSeparator, ws);
        if (DecimalSeparator <> '.') and (i > 0) then ws[i] := '.';
        mem_write(ws);
{$ENDIF}
      end
    else if obj is TlkJSONstring then
      begin
        ws := UTF8Encode(TlkJSONstring(obj).FValue);
        i := 1;
        mem_ch('"');
        while i <= length(ws) do
          begin
            case ws[i] of
              '/', '\', '"':
                begin
                  mem_ch('\');
                  mem_ch(ws[i]);
                end;
              #8: mem_write('\b');
              #9: mem_write('\t');
              #10: mem_write('\n');
              #13: mem_write('\r');
              #12: mem_write('\f');
            else
              if ord(ws[i]) < 32 then
                mem_write('\u' + inttohex(ord(ws[i]), 4))
              else
                mem_ch(ws[i]);
            end;
            inc(i);
          end;
        mem_ch('"');
      end
    else if obj is TlkJSONboolean then
      begin
        if TlkJSONboolean(obj).FValue then
          mem_write('true')
        else
          mem_write('false');
      end
    else if obj is TlkJSONnull then
      begin
        mem_write('null');
      end
    else if obj is TlkJSONlist then
      begin
        mem_ch('[');
        j := TlkJSONobject(obj).Count - 1;
        for i := 0 to j do
          begin
            if i > 0 then mem_ch(',');
            gn_base(TlkJSONlist(obj).Child[i]);
          end;
        mem_ch(']');
      end
    else if obj is TlkJSONobjectmethod then
      begin
        try
          xs := TlkJSONstring.Create;
          xs.FValue := TlkJSONobjectmethod(obj).FName;
          gn_base(TlkJSONbase(xs));
          mem_ch(':');
          gn_base(TlkJSONbase(TlkJSONobjectmethod(obj).FValue));
        finally
          if assigned(xs) then FreeAndNil(xs);
        end;
      end
    else if obj is TlkJSONobject then
      begin
        mem_ch('{');
        j := TlkJSONobject(obj).Count - 1;
        for i := 0 to j do
          begin
            if i > 0 then mem_ch(',');
            gn_base(TlkJSONobject(obj).Child[i]);
          end;
        mem_ch('}');
      end;
  end;
{$ENDIF NEW_STYLE_GENERATE}

begin
{$IFDEF HAVE_FORMATSETTING}
  GetLocaleFormatSettings(GetThreadLocale, fs);
  fs.DecimalSeparator := '.';
{$ENDIF}
{$IFDEF NEW_STYLE_GENERATE}
  pt0 := nil;
  get_more_memory;
  gn_base(obj);
  mem_ch(#0);
  result := string(pt0);
  freemem(pt0);
{$ELSE}
  result := gn_base(obj);
{$ENDIF}
end;

class function TlkJSON.ParseText(const txt: string): TlkJSONbase;
{$IFDEF HAVE_FORMATSETTING}
var
  fs: TFormatSettings;
{$ENDIF}

  function js_base(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean; forward;

  function xe(idx: Integer): Boolean;
  {$IFDEF FPC}inline;
  {$ENDIF}
  begin
    result := idx <= length(txt);
  end;

  procedure skip_spc(var idx: Integer);
  {$IFDEF FPC}inline;
  {$ENDIF}
  begin
    while (xe(idx)) and (ord(txt[idx]) < 33) do
      inc(idx);
  end;

  procedure add_child(var o, c: TlkJSONbase);
  var
    i: Integer;
  begin
    if o = nil then
      begin
        o := c;
      end
    else
      begin
        if o is TlkJSONobjectmethod then
          begin
            TlkJSONobjectmethod(o).FValue := c;
          end
        else if o is TlkJSONlist then
          begin
            TlkJSONlist(o)._Add(c);
          end
        else if o is TlkJSONobject then
          begin
            i := TlkJSONobject(o)._Add(c);
            if TlkJSONobject(o).UseHash then
{$IFDEF USE_HASH}
              TlkJSONobject(o).ht.AddPair(TlkJSONobjectmethod(c).Name, i);
{$ELSE}
              TlkJSONobject(o).ht.Insert(TlkJSONobjectmethod(c).Name, i);
{$ENDIF USE_HASH}
          end;
      end;
  end;

  function js_boolean(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  var
    js: TlkJSONboolean;
  begin
    skip_spc(idx);
    if copy(txt, idx, 4) = 'true' then
      begin
        result := true;
        ridx := idx + 4;
        js := TlkJSONboolean.Create;
        js.FValue := true;
        add_child(o, TlkJSONbase(js));
      end
    else if copy(txt, idx, 5) = 'false' then
      begin
        result := true;
        ridx := idx + 5;
        js := TlkJSONboolean.Create;
        js.FValue := false;
        add_child(o, TlkJSONbase(js));
      end
    else
      begin
        result := false;
      end;
  end;

  function js_null(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  var
    js: TlkJSONnull;
  begin
    skip_spc(idx);
    if copy(txt, idx, 4) = 'null' then
      begin
        result := true;
        ridx := idx + 4;
        js := TlkJSONnull.Create;
        add_child(o, TlkJSONbase(js));
      end
    else
      begin
        result := false;
      end;
  end;

  function js_integer(idx: Integer; var ridx: Integer): Boolean;
  begin
    result := false;
    while (xe(idx)) and (txt[idx] in ['0'..'9']) do
      begin
        result := true;
        inc(idx);
      end;
    if result then ridx := idx;
  end;

  function js_number(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  var
    js: TlkJSONnumber;
    ws: string;
  {$IFNDEF HAVE_FORMATSETTING}
    i: Integer;
  {$ENDIF}
  begin
    skip_spc(idx);
    result := xe(idx);
    if not result then exit;
    if txt[idx] in ['+', '-'] then
      begin
        inc(idx);
        result := xe(idx);
      end;
    if not result then exit;
    result := js_integer(idx, idx);
    if not result then exit;
    if (xe(idx)) and (txt[idx] = '.') then
      begin
        inc(idx);
        result := js_integer(idx, idx);
        if not result then exit;
      end;
    if (xe(idx)) and (txt[idx] in ['e', 'E']) then
      begin
        inc(idx);
        if (xe(idx)) and (txt[idx] in ['+', '-']) then inc(idx);
        result := js_integer(idx, idx);
        if not result then exit;
      end;
    if not result then exit;
    js := TlkJSONnumber.Create;
    ws := copy(txt, ridx, idx - ridx);
{$IFDEF HAVE_FORMATSETTING}
    js.FValue := StrToFloat(ws, fs);
{$ELSE}
    i := pos('.', ws);
    if (DecimalSeparator <> '.') and (i > 0) then
      ws[pos('.', ws)] := DecimalSeparator;
    js.FValue := StrToFloat(ws);
{$ENDIF}
    add_child(o, TlkJSONbase(js));
    ridx := idx;
  end;

{

}
  function js_string(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;

    function strSpecialChars(const s: string): string;
    var
      i, j : integer;
    begin
      i := Pos('\', s);
      if (i = 0) then
        Result := s
      else
      begin
        Result := Copy(s, 1, i-1);
        j := i;
        repeat
          if (s[j] = '\') then
          begin
            inc(j);
            case s[j] of
              '\': Result := Result + '\';
              '"': Result := Result + '"';
              '''': Result := Result + '''';
              '/': Result := Result + '/';
              'b': Result := Result + #8;
              'f': Result := Result + #12;
              'n': Result := Result + #10;
              'r': Result := Result + #13;
              't': Result := Result + #9;
              'u':
                begin
                  Result := Result + code2utf(strtoint('$' + copy(s, j + 1, 4)));
                  inc(j, 4);
                end;
            end;
          end
          else
            Result := Result + s[j];
          inc(j);
        until j > length(s);
      end;
    end;

  var
    js: TlkJSONstring;
    fin: Boolean;
    ws: String;
    i,j,widx: Integer;
  begin
    skip_spc(idx);

    result := xe(idx) and (txt[idx] = '"');
    if not result then exit;

    inc(idx);
    widx := idx;

    fin:=false;
    REPEAT
      i := 0;
      j := 0;
      while (widx<=length(txt)) and (j=0) do
        begin
          if (i=0) and (txt[widx]='\') then i:=widx;
          if (j=0) and (txt[widx]='"') then j:=widx;
          inc(widx);
        end;
// incorrect string!!!
      if j=0 then
        begin
          result := false;
          exit;
        end;
// if we have no slashed chars in string
      if (i=0) or (j<i) then
        begin
          ws := copy(txt,idx,j-idx);
          idx := j;
          fin := true;
        end
// if i>0 and j>=i - skip slashed char
      else
        begin
          widx:=i+2;
        end;
    UNTIL fin;

    ws := strSpecialChars(ws);
    inc(idx);

    js := TlkJSONstring.Create;
{$ifdef USE_D2009}
    js.FValue := UTF8ToString(ws);
{$else}
    js.FValue := UTF8Decode(ws);
{$endif}
    add_child(o, TlkJSONbase(js));
    ridx := idx;
  end;

  function js_list(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  var
    js: TlkJSONlist;
  begin
    result := false;
    try
      js := TlkJSONlist.Create;
      skip_spc(idx);
      result := xe(idx);
      if not result then exit;
      result := txt[idx] = '[';
      if not result then exit;
      inc(idx);
      while js_base(idx, idx, TlkJSONbase(js)) do
        begin
          skip_spc(idx);
          if (xe(idx)) and (txt[idx] = ',') then inc(idx);
        end;
      skip_spc(idx);
      result := (xe(idx)) and (txt[idx] = ']');
      if not result then exit;
      inc(idx);
    finally
      if not result then
        begin
          js.Free;
        end
      else
        begin
          add_child(o, TlkJSONbase(js));
          ridx := idx;
        end;
    end;
  end;

  function js_method(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  var
    mth: TlkJSONobjectmethod;
    ws: TlkJSONstring;
  begin
    result := false;
    try
      ws := nil;
      mth := TlkJSONobjectmethod.Create;
      skip_spc(idx);
      result := xe(idx);
      if not result then exit;
      result := js_string(idx, idx, TlkJSONbase(ws));
      if not result then exit;
      skip_spc(idx);
      result := xe(idx) and (txt[idx] = ':');
      if not result then exit;
      inc(idx);
      mth.FName := ws.FValue;
      result := js_base(idx, idx, TlkJSONbase(mth));
    finally
      if ws <> nil then ws.Free;
      if result then
        begin
          add_child(o, TlkJSONbase(mth));
          ridx := idx;
        end
      else
        begin
          mth.Free;
        end;
    end;
  end;

  function js_object(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  var
    js: TlkJSONobject;
  begin
    result := false;
    try
      js := TlkJSONobject.Create;
      skip_spc(idx);
      result := xe(idx);
      if not result then exit;
      result := txt[idx] = '{';
      if not result then exit;
      inc(idx);
      while js_method(idx, idx, TlkJSONbase(js)) do
        begin
          skip_spc(idx);
          if (xe(idx)) and (txt[idx] = ',') then inc(idx);
        end;
      skip_spc(idx);  
      result := (xe(idx)) and (txt[idx] = '}');
      if not result then exit;
      inc(idx);
    finally
      if not result then
        begin
          js.Free;
        end
      else
        begin
          add_child(o, TlkJSONbase(js));
          ridx := idx;
        end;
    end;
  end;

  function js_base(idx: Integer; var ridx: Integer; var o:
    TlkJSONbase): Boolean;
  begin
    skip_spc(idx);
    result := js_boolean(idx, idx, o);
    if not result then result := js_null(idx, idx, o);
    if not result then result := js_number(idx, idx, o);
    if not result then result := js_string(idx, idx, o);
    if not result then result := js_list(idx, idx, o);
    if not result then result := js_object(idx, idx, o);
    if result then ridx := idx;
  end;

var
  idx: Integer;
begin
{$IFDEF HAVE_FORMATSETTING}
  GetLocaleFormatSettings(GetThreadLocale, fs);
  fs.DecimalSeparator := '.';
{$ENDIF}

  result := nil;
  if txt = '' then exit;
  try
    idx := 1;
    // skip a BOM utf8 marker
    if copy(txt,idx,3)=#239#187#191 then
      begin
        inc(idx,3);
    // if there are only a BOM - exit;
        if idx>length(txt) then exit;
      end;
    if not js_base(idx, idx, result) then FreeAndNil(result);
  except
    if assigned(result) then FreeAndNil(result);
  end;
end;

{ ElkIntException }

constructor ElkIntException.Create(AIdx: Integer; AMsg: string);
begin
  self.idx := AIdx;
  inherited Create(AMsg);
end;

{ TlkHashTable }

{$IFDEF USE_HASH}
procedure TlkHashTable.AddPair(const ws: WideString; idx: Integer);
var
  i, j, k: cardinal;
  p: PlkHashItem;
  find: boolean;
begin
  find := false;
  if InTable(ws, i, j, k) then
    begin
// if string is already in table, changing index
      if TlkJSONobject(FParent).GetNameOf(PlkHashItem(a_x[j].Items[k])^.index) = ws then
        begin
           PlkHashItem(a_x[j].Items[k])^.index := idx;
           find := true;
        end;
    end;
  if find = false then
    begin
      GetMem(p,sizeof(TlkHashItem));
      k := a_x[j].Add(p);
      p^.hash := i;
      p^.index := idx;
      while (k>0) and (PlkHashItem(a_x[j].Items[k])^.hash < PlkHashItem(a_x[j].Items[k-1])^.hash) do
        begin
          a_x[j].Exchange(k,k-1);
          dec(k);
        end;
    end;
end;

function TlkHashTable.counters: string;
var
  i, j: Integer;
  ws: string;
begin
  ws := '';
  for i := 0 to 15 do
    begin
      for j := 0 to 15 do
//        ws := ws + format('%.3d ', [length(a_h[i * 16 + j])]);
        ws := ws + format('%.3d ', [a_x[i * 16 + j].Count]);
      ws := ws + #13#10;
    end;
  result := ws;
end;

procedure TlkHashTable.Delete(const ws: WideString);
var
  i, j, k: cardinal;
begin
  if InTable(ws, i, j, k) then
    begin
//      while k < high(a_h[j]) do
//        begin
//          hswap(j, k, k + 1);
//          inc(k);
//        end;
//      SetLength(a_h[j], k);
      FreeMem(a_x[j].Items[k]);
      a_x[j].Delete(k);
    end;
end;

{$IFDEF THREADSAFE}
const
  rnd_table: array[0..255] of byte =
  (216, 191, 234, 201, 12, 163, 190, 205, 128, 199, 210, 17, 52, 43,
    38, 149, 40, 207, 186, 89, 92, 179, 142, 93, 208, 215, 162,
    161, 132, 59, 246, 37, 120, 223, 138, 233, 172, 195, 94, 237, 32,
    231, 114, 49, 212, 75, 198, 181, 200, 239, 90, 121, 252, 211,
    46, 125, 112, 247, 66, 193, 36, 91, 150, 69, 24, 255, 42, 9, 76,
    227, 254, 13, 192, 7, 18, 81, 116, 107, 102, 213, 104, 15, 250,
    153, 156, 243, 206, 157, 16, 23, 226, 225, 196, 123, 54, 101,
    184, 31, 202, 41, 236, 3, 158, 45, 96, 39, 178, 113, 20, 139, 6,
    245, 8, 47, 154, 185, 60, 19, 110, 189, 176, 55, 130, 1, 100,
    155, 214, 133, 88, 63, 106, 73, 140, 35, 62, 77, 0, 71, 82, 145,
    180,
    171, 166, 21, 168, 79, 58, 217, 220, 51, 14, 221, 80, 87, 34, 33,
    4, 187, 118, 165, 248, 95, 10, 105, 44, 67, 222, 109, 160, 103,
    242, 177, 84, 203, 70, 53, 72, 111, 218, 249, 124, 83, 174, 253,
    240, 119, 194, 65, 164, 219, 22, 197, 152, 127, 170, 137, 204,
    99, 126, 141, 64, 135, 146, 209, 244, 235, 230, 85, 232, 143,
    122, 25, 28, 115, 78, 29, 144, 151, 98, 97, 68, 251, 182, 229,
    56,
    159, 74, 169, 108, 131, 30, 173, 224, 167, 50, 241, 148, 11, 134,
    117, 136, 175, 26, 57, 188, 147, 238, 61, 48, 183, 2, 129,
    228, 27, 86, 5);
{$ELSE}
var
  rnd_table: array[0..255] of byte;
{$ENDIF}

function TlkHashTable.DefaultHashOf(const ws: WideString): cardinal;
{$IFDEF DOTNET}
var
  i, j: Integer;
  x1, x2, x3, x4: byte;
begin
  result := 0;
//  result := 0;
  x1 := 0;
  x2 := 1;
  for i := 1 to length(ws) do
    begin
      j := ord(ws[i]);
// first version of hashing
      x1 := (x1 + j) {and $FF};
      x2 := (x2 + 1 + (j shr 8)) {and $FF};
      x3 := rnd_table[x1];
      x4 := rnd_table[x3];
      result := ((x1 * x4) + (x2 * x3)) xor result;
    end;
end;
{$ELSE}
var
  x1, x2, x3, x4: byte;
  p: PWideChar;
begin
  result := 0;
  x1 := 0;
  x2 := 1;
  p := PWideChar(ws);
  while p^ <> #0 do
    begin
      inc(x1, ord(p^)) {and $FF};
      inc(x2, 1 + (ord(p^) shr 8)) {and $FF};
      x3 := rnd_table[x1];
      x4 := rnd_table[x3];
      result := ((x1 * x4) + (x2 * x3)) xor result;
      inc(p);
    end;
end;
{$ENDIF}

procedure TlkHashTable.hswap(j, k, l: Integer);
//var
//  h: TlkHashItem;
begin
//  h := a_h[j, k];
//  a_h[j, k] := a_h[j, l];
//  a_h[j, l] := h;
  a_x[j].Exchange(k, l);
end;

function TlkHashTable.IndexOf(const ws: WideString): Integer;
var
  i, j, k: Cardinal;
begin
  if not InTable(ws, i, j, k) then
    begin
      result := -1;
    end
  else
    begin
//      result := a_h[j, k].index;
      result := PlkHashItem(a_x[j].Items[k])^.index;
    end;
end;

function TlkHashTable.InTable(const ws: WideString; var i, j, k:
  cardinal):
  Boolean;
var
  l, wu, wl: Integer;
  x: Cardinal;
  fin: Boolean;
begin
  i := HashOf(ws);
  j := i and $FF;
  result := false;
{using "binary" search always, because array is sorted}
  if a_x[j].Count-1 >= 0 then
    begin
      wl := 0;
      wu := a_x[j].Count-1;
      repeat
        fin := true;
        if PlkHashItem(a_x[j].Items[wl])^.hash = i then
          begin
            k := wl;
            result := true;
          end
        else if PlkHashItem(a_x[j].Items[wu])^.hash = i then
          begin
            k := wu;
            result := true;
          end
        else if (wu - wl) > 1 then
          begin
            fin := false;
            x := (wl + wu) shr 1;
            if PlkHashItem(a_x[j].Items[x])^.hash > i then
              begin
                wu := x;
              end
            else
              begin
                wl := x;
              end;
          end;
      until fin;
    end;

// verify k index in chain
  if result = true then
    begin
      while (k > 0) and (PlkHashItem(a_x[j].Items[k])^.hash = PlkHashItem(a_x[j].Items[k-1])^.hash) do dec(k);
      repeat
        fin := true;
        if TlkJSONobject(FParent).GetNameOf(PlkHashItem(a_x[j].Items[k])^.index) <> ws then
          begin
            if k < a_x[j].Count-1 then
              begin
                inc(k);
                fin := false;
              end
            else
              begin
                result := false;
              end;
          end
        else
          begin
            result := true;
          end;
      until fin;
    end;
end;

{$IFNDEF THREADSAFE}

procedure init_rnd;
var
  x0: Integer;
  i: Integer;
begin
  x0 := 5;
  for i := 0 to 255 do
    begin
      x0 := (x0 * 29 + 71) and $FF;
      rnd_table[i] := x0;
    end;
end;
{$ENDIF}

procedure TlkHashTable.SetHashFunction(const AValue:
  TlkHashFunction);
begin
  FHashFunction := AValue;
end;

constructor TlkHashTable.Create;
var
  i: Integer;
begin
  inherited;
//  for i := 0 to 255 do SetLength(a_h[i], 0);
  for i := 0 to 255 do a_x[i] := TList.Create;
  HashOf := {$IFDEF FPC}@{$ENDIF}DefaultHashOf;
end;

destructor TlkHashTable.Destroy;
var
  i, j: Integer;
begin
//  for i := 0 to 255 do SetLength(a_h[i], 0);
  for i := 0 to 255 do
    begin
      for j := 0 to a_x[i].Count - 1 do Freemem(a_x[i].Items[j]);
      a_x[i].Free;
    end;
  inherited;
end;

function TlkHashTable.SimpleHashOf(const ws: WideString): cardinal;
var
  i: Integer;
begin
  result := length(ws);
  for i := 1 to length(ws) do result := result + ord(ws[i]);
end;
{$ENDIF USE_HASH}

class function TlkJSONstreamed.LoadFromFile(srcname: string):
  TlkJSONbase;
var
  fs: TFileStream;
begin
  result := nil;
  if not FileExists(srcname) then exit;
  try
    fs := TFileStream.Create(srcname, fmOpenRead);
    result := LoadFromStream(fs);
  finally
    if Assigned(fs) then FreeAndNil(fs);
  end;
end;

class function TlkJSONstreamed.LoadFromStream(src: TStream):
  TlkJSONbase;
var
  ws: string;
  len: int64;
begin
  result := nil;
  if not assigned(src) then exit;
  len := src.Size - src.Position;
  SetLength(ws, len);
  src.Read(pchar(ws)^, len);
  result := ParseText(ws);
end;

class procedure TlkJSONstreamed.SaveToFile(obj: TlkJSONbase;
  dstname: string);
var
  fs: TFileStream;
begin
  if not assigned(obj) then exit;
  try
    fs := TFileStream.Create(dstname, fmCreate);
    SaveToStream(obj, fs);
  finally
    if Assigned(fs) then FreeAndNil(fs);
  end;
end;

class procedure TlkJSONstreamed.SaveToStream(obj: TlkJSONbase;
  dst: TStream);
var
  ws: string;
begin
  if not assigned(obj) then exit;
  if not assigned(dst) then exit;
  ws := GenerateText(obj);
  dst.Write(pchar(ws)^, length(ws));
end;

{ TlkBalTree }

{$IFNDEF USE_HASH}
procedure TlkBalTree.Clear;

  procedure rec(t: PlkBalNode);
  begin
    if t^.left<>fbottom then rec(t^.left);
    if t^.right<>fbottom then rec(t^.right);
    t^.nm := '';
    dispose(t);
  end;

begin
  if froot<>fbottom then rec(froot);
  froot := fbottom;
  fdeleted := fbottom;
end;

function TlkBalTree.counters: string;
begin
  result := format('Balanced tree root node level is %d',[froot^.level]);
end;

constructor TlkBalTree.Create;
begin
  inherited Create;
  new(fbottom);
  fbottom^.left := fbottom;
  fbottom^.right := fbottom;
  fbottom^.level := 0;
  fdeleted := fbottom;
  froot := fbottom;
end;

function TlkBalTree.Delete(const ws: WideString): Boolean;

  procedure UpdateKeys(t: PlkBalNode; idx: integer);
  begin
    if t <> fbottom then begin
      if t^.key > idx then
        t^.key := t^.key - 1;
      UpdateKeys(t^.left, idx);
      UpdateKeys(t^.right, idx);
    end;
  end;

  function del(var t: PlkBalNode): Boolean;
  begin
    result := false;
    if t<>fbottom then begin
      flast := t;
      if ws<t^.nm then
        result := del(t^.left)
      else begin
        fdeleted := t;
        result := del(t^.right);
      end;
      if (t = flast) and (fdeleted <> fbottom) and (ws = fdeleted^.nm) then begin
        UpdateKeys(froot, fdeleted^.key);
        fdeleted^.key := t^.key;
        fdeleted^.nm := t^.nm;
        t := t^.right;
        flast^.nm := '';
        dispose(flast);
        result := true;
      end
      else if (t^.left^.level < (t^.level - 1)) or (t^.right^.level < (t^.level - 1)) then begin
        t^.level := t^.level - 1;
        if t^.right^.level > t^.level then
          t^.right^.level := t^.level;
        skew(t);
        skew(t^.right);
        skew(t^.right^.right);
        split(t);
        split(t^.right);
      end;
    end;
  end;

{
// mine version, buggy, see tracker message
// [ 2229135 ] Value deletion is broken by "Nobody/Anonymous - nobody"

  function del(var t: PlkBalNode): Boolean;
  begin
    result := false;
    if t<>fbottom then
      begin
        flast := t;
        if ws<t.nm then
          result := del(t.left)
        else
          begin
            fdeleted := t;
            result := del(t.right);
          end;
        if (t = flast) and (fdeleted<>fbottom) and (ws = t.nm) then
          begin
            fdeleted.key := t.key;
            fdeleted.nm := t.nm;
            t := t.right;
            flast.nm := '';
            dispose(flast);
            result := true;
          end
        else if (t.left.level<(t.level-1)) or (t.right.level<(t.level-1)) then
          begin
            t.level := t.level-1;
            if t.right.level>t.level then t.right.level := t.level;
            skew(t);
            skew(t.right);
            skew(t.right.right);
            split(t);
            split(t.right);
          end;
      end;
  end;
}

begin
  result := del(froot);
end;

destructor TlkBalTree.Destroy;
begin
  Clear;
  dispose(fbottom);
  inherited;
end;

function TlkBalTree.IndexOf(const ws: WideString): Integer;
var
  tk: PlkBalNode;
begin
  result := -1;
  tk := froot;
  while (result=-1) and (tk<>fbottom) do
    begin
      if tk^.nm = ws then result := tk^.key
      else if ws<tk^.nm then tk := tk^.left
      else tk := tk^.right;
    end;
end;

function TlkBalTree.Insert(const ws: WideString; x: Integer): Boolean;

  function ins(var t: PlkBalNode): Boolean;
  begin
    if t = fbottom then
      begin
        new(t);
        t^.key := x;
        t^.nm := ws;
        t^.left := fbottom;
        t^.right := fbottom;
        t^.level := 1;
        result := true;
      end
    else
      begin
        if ws < t^.nm then
          result := ins(t^.left)
        else if ws > t^.nm then
          result := ins(t^.right)
        else result := false;
        skew(t);
        split(t);
      end;
  end;

begin
  result := ins(froot);
end;

procedure TlkBalTree.skew(var t: PlkBalNode);
var
  temp: PlkBalNode;
begin
  if t^.left^.level = t^.level then
    begin
      temp := t;
      t := t^.left;
      temp^.left := t^.right;
      t^.right := temp;
    end;
end;

procedure TlkBalTree.split(var t: PlkBalNode);
var
  temp: PlkBalNode;
begin
  if t^.right^.right^.level = t^.level then
    begin
      temp := t;
      t := t^.right;
      temp^.right := t^.left;
      t^.left := temp;
      t^.level := t^.level+1;
    end;
end;
{$ENDIF USE_HASH}

initialization
{$IFNDEF THREADSAFE}
{$IFDEF USE_HASH}
  init_rnd;
{$ENDIF USE_HASH}
{$ENDIF THREADSAFE}
end.
