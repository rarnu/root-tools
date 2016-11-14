{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  Base for REST classes 
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
unit restbase;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGBASEOBJMEMLEAK}

interface

uses
  typinfo, fpjson, Classes, SysUtils, contnrs;

Type
  ERESTAPI = Class(Exception);
  TStringArray = Array of string;
  TStringArrayArray = Array of TStringArray;
  TUnicodeStringArray = Array of UnicodeString;
  TIntegerArray = Array of Integer;
  TInt64Array = Array of Int64;
  TInt32Array = Array of Integer;
  TFloatArray = Array of TJSONFloat;
  TFloatArrayArray = Array of TFloatArray;
  TDoubleArray = Array of TJSONFloat;
  TDoubleArrayArray = Array of TDoubleArray;
  TDateTimeArray = Array of TDateTime;
  TBooleanArray = Array of boolean;
  TChildType = (ctArray,ctObject);
  TChildTypes = Set of TChildType;

  { TBaseObject }
  TObjectOption = (ooStartRecordingChanges,ooCreateObjectOnGet);
  TObjectOptions = set of TObjectOption;
  TDateTimeType = (dtNone,dtDateTime,dtDate,dtTime);

Const
  DefaultObjectOptions = [ooStartRecordingChanges]; // Default for constructor.
  IndexShift = 3; // Number of bits reserved for flags.

Type
{$M+}

  TBaseObject = CLass(TObject)
  Private
    FObjectOptions : TObjectOptions;
    fadditionalProperties : TJSONObject;
    FBits : TBits;
    Function GetDynArrayProp(P: PPropInfo) : Pointer; virtual;
    procedure SetDynArrayProp(P: PPropInfo; AValue : Pointer); virtual;
    procedure SetObjectOptions(AValue: TObjectOptions);
    Function GetAdditionalProperties : TJSONObject;
  protected
{$ifdef ver2_6}
    // Version 2.6.4 has a bug for i386 where the array cannot be set through RTTI.
    // This is a helper method that sets the length of the array to the desired length,
    // After which the new array pointer is read again.
    // AName is guaranteed to be lowercase
    Procedure SetArrayLength(const AName : String; ALength : Longint); virtual;
{$endif}
    Procedure MarkPropertyChanged(AIndex : Integer);
    Function IsDateTimeProp(Info : PTypeInfo) : Boolean;
    Function DateTimePropType(Info : PTypeInfo) : TDateTimeType;
    // Load properties
    Procedure ClearProperty(P: PPropInfo); virtual;
    Procedure SetBooleanProperty(P: PPropInfo; AValue: Boolean); virtual;
    Procedure SetFloatProperty(P: PPropInfo; AValue: Extended); virtual;
    Procedure SetInt64Property(P: PPropInfo; AValue: Int64); virtual;
    {$ifndef ver2_6}
    Procedure SetQWordProperty(P: PPropInfo; AValue: QWord); virtual;
    {$endif}
    Procedure SetIntegerProperty(P: PPropInfo; AValue: Integer); virtual;
    Procedure SetStringProperty(P: PPropInfo; AValue: String); virtual;
    Procedure SetArrayProperty(P: PPropInfo; AValue : TJSONArray); virtual;
    Procedure SetObjectProperty(P: PPropInfo; AValue : TJSONObject); virtual;
    Procedure SetSetProperty(P: PPropInfo; AValue : TJSONArray); virtual;
    Procedure SetEnumProperty(P: PPropInfo; AValue : TJSONData); virtual;
    // Save properties
    Function GetBooleanProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetIntegerProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetInt64Property(P: PPropInfo) : TJSONData; virtual;
    Function GetQwordProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetFloatProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetStringProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetSetProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetEnumeratedProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetArrayProperty(P: PPropInfo) : TJSONData; virtual;
    Function GetObjectProperty(P: PPropInfo) : TJSONData; virtual;
    // Clear properties on
    Procedure ClearChildren(ChildTypes : TChildTypes); virtual;
    Class Function ClearChildTypes : TChildTypes; virtual;
  Public
    Constructor Create(AOptions : TObjectOptions = DefaultObjectOptions); Virtual;
    Destructor Destroy; override;
    Procedure StartRecordPropertyChanges;
    Procedure ClearPropertyChanges;
    Procedure StopRecordPropertyChanges;
    Function IsPropertyModified(Info : PPropInfo) : Boolean;
    Function IsPropertyModified(const AName : String) : Boolean;
    Class Function AllowAdditionalProperties : Boolean; virtual;
    Class Function GetTotalPropCount : Integer; virtual;
    Class Function GetCurrentPropCount : Integer; virtual;
    Class Function GetParentPropCount : Integer; virtual;
    Class Function ExportPropertyName(Const AName : String) : string; virtual;
    Class Function CleanPropertyName(Const AName : String) : string;
    Class Function CreateObject(Const AKind : String) : TBaseObject;
    Class Procedure RegisterObject;
    Class Function ObjectRestKind : String; virtual;
    Procedure LoadPropertyFromJSON(Const AName : String; JSON : TJSONData); virtual;
    Function SavePropertyToJSON(Info : PPropInfo) : TJSONData; virtual;
    Procedure LoadFromJSON(JSON : TJSONObject); virtual;
    Procedure SaveToJSON(JSON : TJSONObject); virtual;
    Function SaveToJSON : TJSONObject;
    Property ObjectOptions : TObjectOptions Read FObjectOptions Write SetObjectOptions;
    Property additionalProperties : TJSONObject Read GetAdditionalProperties;
  end;
  TBaseObjectClass = Class of TBaseObject;
  TObjectArray =  Array of TBaseObject;
  TObjectArrayArray =  Array of TObjectArray;

  TBaseListEnumerator = class
  private
    FList: TFPObjectList;
    FPosition: Integer;
  public
    constructor Create(AList: TFPObjectList);
    function GetCurrent: TBaseObject; virtual;
    function MoveNext: Boolean;
    property Current: TBaseObject read GetCurrent;
  end;
  TBaseListEnumeratorClass = Class of TBaseListEnumerator;

  { TBaseObjectList }

  TBaseObjectList = Class(TBaseObject)
  private
    FList : TFPObjectList;
  Protected
    function GetO(Aindex : Integer): TBaseObject;
    procedure SetO(Aindex : Integer; AValue: TBaseObject);
    Class Function ObjectClass : TBaseObjectClass; virtual;
    Function DoCreateEnumerator(AEnumClass : TBaseListEnumeratorClass) : TBaseListEnumerator;
  Public
    Constructor Create(AOptions : TObjectOptions = DefaultObjectOptions); Override;
    Destructor Destroy; override;
    function GetEnumerator : TBaseListEnumerator;
    Function AddObject(Const AKind : String) : TBaseObject; virtual;
    Property Objects [Aindex : Integer] : TBaseObject Read GetO Write SetO; default;
  end;

  { TBaseObjectList }

  { TBaseNamedObjectList }

  TBaseNamedObjectList = Class(TBaseObject)
  private
    FList : TStringList;
    function GetN(Aindex : Integer): String;
    function GetO(Aindex : Integer): TBaseObject;
    function GetON(AName : String): TBaseObject;
    procedure SetN(Aindex : Integer; AValue: String);
    procedure SetO(Aindex : Integer; AValue: TBaseObject);
    procedure SetON(AName : String; AValue: TBaseObject);
  Protected
    Class Function ObjectClass : TBaseObjectClass; virtual;
  Public
    Constructor Create(AOptions : TObjectOptions = DefaultObjectOptions); Override;
    Destructor Destroy; override;
    Function AddObject(Const AName,AKind : String) : TBaseObject; virtual;
    Property Names [Aindex : Integer] : String Read GetN Write SetN;
    Property Objects [Aindex : Integer] : TBaseObject Read GetO Write SetO;
    Property ObjectByName [AName : String] : TBaseObject Read GetON Write SetON; default;
  end;

  // used to catch a general JSON schema.
  { TJSONSchema }

  TJSONSchema = Class(TBaseObject)
  private
    FSchema: String;
  Public
    Procedure SetArrayProperty(P: PPropInfo; AValue : TJSONArray); override;
    Procedure LoadFromJSON(JSON : TJSONObject); override;
    Property Schema : String Read FSchema Write FSchema;
  end;
  TJSONSchemaArray = Array of TJSONSchema;
  TTJSONSchemaArray = TJSONSchemaArray;

  { TObjectFactory }

  TObjectFactory = Class(TComponent)
  Private
    FList : TClassList;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure RegisterObject(A : TBaseObjectClass);
    Function GetObjectClass(Const AKind : String) : TBaseObjectClass;
  end;

Function  RESTFactory : TObjectFactory;

Function DateTimeToRFC3339(ADate :TDateTime):string;
Function DateToRFC3339(ADate :TDateTime):string;
Function TimeToRFC3339(ADate :TDateTime):string;
Function TryRFC3339ToDateTime(const Avalue: String; out ADateTime: TDateTime): Boolean;
Function RFC3339ToDateTime(const Avalue: String): TDateTime;

implementation

Var
  Fact : TObjectFactory;

function DateTimeToRFC3339(ADate :TDateTime):string;

begin
  Result:=FormatDateTime('yyyy-mm-dd"T"hh":"nn":"ss"."zzz"Z"',ADate);
end;

function DateToRFC3339(ADate: TDateTime): string;
begin
  Result:=FormatDateTime('yyyy-mm-dd',ADate);
end;

function TimeToRFC3339(ADate :TDateTime):string;

begin
  Result:=FormatDateTime('hh":"nn":"ss"."zzz',ADate);
end;


Function TryRFC3339ToDateTime(const Avalue: String; out ADateTime: TDateTime): Boolean;

//          1         2
// 12345678901234567890123
// yyyy-mm-ddThh:nn:ss.zzz

Type
  TPartPos = (ppTime,ppYear,ppMonth,ppDay,ppHour,ppMinute,ppSec);
  TPos = Array [TPartPos] of byte;

Const
  P : TPos = (11,1,6,9,12,15,18);

var
  lY, lM, lD, lH, lMi, lS: Integer;

begin
  if Trim(AValue) = '' then
    begin
    Result:=True;
    ADateTime:=0;
    end;
  lY:=StrToIntDef(Copy(AValue,P[ppYear],4),-1);
  lM:=StrToIntDef(Copy(AValue,P[ppMonth],2),-1);
  lD:=StrToIntDef(Copy(AValue,P[ppDay],2),-1);
  if (Length(AValue)>=P[ppTime]) then
    begin
    lH:=StrToIntDef(Copy(AValue,P[ppHour],2),-1);
    lMi:=StrToIntDef(Copy(AValue,P[ppMinute],2),-1);
    lS:=StrToIntDef(Copy(AValue,P[ppSec],2),-1);
    end
  else
    begin
    lH:=0;
    lMi:=0;
    lS:=0;
    end;
  Result:=(lY>=0) and (lM>=00) and (lD>=0) and (lH>=0) and (lMi>=0) and (ls>=0);
  if Not Result then
    ADateTime:=0
  else
    { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
    if (lY = 0) or (lM = 0) or (lD = 0) then
      ADateTime:=EncodeTime(lH, lMi, lS, 0)
    else
      ADateTime:=EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;

Function CountProperties(TypeInfo : PTypeInfo; Recurse : Boolean): Integer;

   function aligntoptr(p : pointer) : pointer;inline;

   begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   end;

var
  hp : PTypeData;
  pd : ^TPropData;

begin
  Result:=0;
  while Assigned(TypeInfo) do
    begin
    // skip the name
    hp:=GetTypeData(Typeinfo);
    // the class info rtti the property rtti follows immediatly
    pd:=aligntoptr(pointer(pointer(@hp^.UnitName)+Length(hp^.UnitName)+1));
    Result:=Result+Pd^.PropCount;
    if Recurse then
      TypeInfo:=HP^.ParentInfo
    else
      TypeInfo:=Nil;
    end;
end;


Function RFC3339ToDateTime(const Avalue: String): TDateTime;

begin
  if Not TryRFC3339ToDateTime(AValue,Result) then
    Result:=0;
end;

Function RESTFactory : TObjectFactory;

begin
  if Fact=Nil then
    Fact:=TObjectfactory.Create(Nil);
  Result:=Fact;
end;

{ TObjectFactory }

Constructor TObjectFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList:=TClassList.Create;
end;

Destructor TObjectFactory.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

Procedure TObjectFactory.RegisterObject(A: TBaseObjectClass);
begin
  Flist.Add(A);
end;

Function TObjectFactory.GetObjectClass(Const AKind: String): TBaseObjectClass;

Var
  I : Integer;
  N : String;

begin
  I:=FList.Count-1;
  Result:=Nil;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=TBaseObjectClass(FList[i]);
    N:=Result.ObjectRestKind;
    if CompareText(N,AKind)<>0 then
      Result:=nil;
    Dec(I);
    end;
end;


{ TBaseNamedObjectList }

function TBaseNamedObjectList.GetN(Aindex : Integer): String;
begin
  Result:=Flist[AIndex];
end;

function TBaseNamedObjectList.GetO(Aindex: Integer): TBaseObject;
begin
  Result:=TBaseObject(Flist.Objects[AIndex]);
end;

function TBaseNamedObjectList.GetON(AName : String): TBaseObject;

Var
  I : Integer;

begin
  I:=FList.IndexOf(AName);
  if I<>-1 then
    Result:=GetO(I)
  else
    Result:=Nil;
end;

procedure TBaseNamedObjectList.SetN(Aindex : Integer; AValue: String);
begin
  Flist[AIndex]:=Avalue
end;

procedure TBaseNamedObjectList.SetO(Aindex: Integer; AValue: TBaseObject);
begin
  Flist.Objects[AIndex]:=Avalue
end;

procedure TBaseNamedObjectList.SetON(AName : String; AValue: TBaseObject);
Var
  I : Integer;

begin
  I:=FList.IndexOf(AName);
  if I<>-1 then
    SetO(I,AValue)
  else
    Flist.AddObject(AName,AValue);
end;

Class Function TBaseNamedObjectList.ObjectClass: TBaseObjectClass;
begin
  Result:=TBaseObject;
end;

Constructor TBaseNamedObjectList.Create(AOptions : TObjectOptions = DefaultObjectOptions);
begin
  inherited Create(AOptions);
  FList:=TStringList.Create;
  Flist.OwnsObjects:=True;
end;

Destructor TBaseNamedObjectList.Destroy;
begin
  FreeAndNil(Flist);
  inherited Destroy;
end;

Function TBaseNamedObjectList.AddObject(Const AName, AKind: String
  ): TBaseObject;
begin
  Result:=CreateObject(AKind);
  ObjectByName[AName]:=Result;
end;
{ TJSONSchema }

Procedure TJSONSchema.SetArrayProperty(P: PPropInfo; AValue: TJSONArray);
begin
  Schema:=AValue.asJSON
end;

Procedure TJSONSchema.LoadFromJSON(JSON: TJSONObject);
begin
  Schema:=JSON.AsJSON;
end;

{ TBaseObjectList }

function TBaseObjectList.GetO(Aindex : Integer): TBaseObject;
begin
  Result:=TBaseObject(FList[AIndex]);
end;

procedure TBaseObjectList.SetO(Aindex : Integer; AValue: TBaseObject);
begin
  FList[AIndex]:=AValue;
end;

class function TBaseObjectList.ObjectClass: TBaseObjectClass;
begin
  Result:=TBaseObject;
end;

function TBaseObjectList.DoCreateEnumerator(AEnumClass: TBaseListEnumeratorClass
  ): TBaseListEnumerator;
begin
  Result:=AEnumClass.Create(FList);
end;

constructor TBaseObjectList.Create(AOptions: TObjectOptions);
begin
  inherited Create(AOptions);
  FList:=TFPObjectList.Create;
end;

destructor TBaseObjectList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TBaseObjectList.GetEnumerator: TBaseListEnumerator;
begin
  Result:=TBaseListEnumerator.Create(FList);
end;

function TBaseObjectList.AddObject(const AKind: String): TBaseObject;

Var
  C : TBaseObjectClass;
begin
  if (AKind<>'') then
    begin
    C:=RestFactory.GetObjectClass(AKind);
    if Not C.InheritsFrom(ObjectClass) then
      Raise ERestAPI.CreateFmt('Cannot add object of kind "%s" to list, associated class "%s" is not a descendent of list class "%s"',[AKind,C.ClassName,ObjectClass.ClassName]);
    end;
  Result:=ObjectClass.Create;
  FList.Add(Result);
end;

constructor TBAseListEnumerator.Create(AList: TFPObjectList);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TBaseListEnumerator.GetCurrent: TBaseObject;
begin
  Result := TBaseObject(FList[FPosition]);
end;

function TBaseListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TBaseObject }

function TBaseObject.GetDynArrayProp(P: PPropInfo): Pointer;
begin
  Result:=Pointer(GetObjectProp(Self,P));
end;


procedure TBaseObject.SetDynArrayProp(P: PPropInfo; AValue: Pointer);
begin
  SetObjectProp(Self,P,TObject(AValue));
end;

procedure TBaseObject.SetObjectOptions(AValue: TObjectOptions);
begin
  if FObjectOptions=AValue then Exit;
  FObjectOptions:=AValue;
  if ooStartRecordingChanges in FObjectOptions then
    StartRecordPropertyChanges
end;

procedure TBaseObject.MarkPropertyChanged(AIndex: Integer);
begin
  If Assigned(FBits) then
    FBits.SetOn(GetParentPropCount+(AIndex shr IndexShift));
end;

function TBaseObject.IsDateTimeProp(Info: PTypeInfo): Boolean;
begin
  Result:=DateTimePropType(Info)<>dtNone;
end;

function TBaseObject.DateTimePropType(Info: PTypeInfo): TDateTimeType;
begin
  Result:=dtNone;
  if (Info=TypeInfo(TDateTime)) then
    Result:=dtDateTime
  else if (Info=TypeInfo(TDate)) then
    Result:=dtDate
  else if (Info=TypeInfo(TTime)) then
    Result:=dtTime
end;

procedure TBaseObject.ClearProperty(P: PPropInfo);
begin
  Case P^.PropType^.Kind of
    tkInteger,
    tkChar,
    tkEnumeration,
    tkBool,
    tkSet : SetOrdProp(Self,P,0);
    tkFloat : SetFloatProp(Self,P,0.0);
    tkSString,
    tkLString,
    tkUChar,
    tkAString: SetStrProp(Self,P,'');
    tkWChar,
    tkWString: SetWideStrProp(Self,P,'');
    tkUString:  SetUnicodeStrProp(Self,P,'');
    tkInt64,
    tkQWord : SetInt64Prop(Self,P,0);
    tkClass :
      begin
      GetObjectProp(Self,P).Free;
      SetObjectProp(Self,P,Nil);
      end
  else
    // Do nothing
  end;
end;

procedure TBaseObject.SetBooleanProperty(P: PPropInfo; AValue: Boolean);
begin
  SetOrdProp(Self,P,Ord(AValue));
end;

procedure TBaseObject.SetFloatProperty(P: PPropInfo; AValue: Extended);

begin
  SetFloatProp(Self,P,AValue);
end;

procedure TBaseObject.SetIntegerProperty(P: PPropInfo; AValue: Integer);

begin
  SetOrdProp(Self,P,AValue);
end;

procedure TBaseObject.SetInt64Property(P: PPropInfo; AValue: Int64);

begin
  SetInt64Prop(Self,P,AValue);
end;

{$ifndef ver2_6}
procedure TBaseObject.SetQWordProperty(P: PPropInfo; AValue: QWord);

begin
  SetInt64Prop(Self,P,Int64(AValue));
end;
{$endif}

procedure TBaseObject.SetStringProperty(P: PPropInfo; AValue: String);
Var
  D : TDateTime;
begin
  if not IsDateTimeProp(P^.PropType) then
    SetStrProp(Self,P,AValue)
  else if TryRFC3339ToDateTime(AValue,D) then
    SetFloatProp(Self,P,D)
  else
    SetFloatProp(Self,P,0)
end;

procedure TBaseObject.SetArrayProperty(P: PPropInfo; AValue: TJSONArray);

Var
  T : PTypeData;
  L : TBaseObjectList;
  D : TJSONEnum;
  O : TObjectArray;
  I : Integer;
  PA : ^pdynarraytypeinfo;
  ET : PTypeInfo;
  LPN,AN : String;
  AP : Pointer;
  S : TJSONSchema;

begin
  if P^.PropType^.Kind=tkClass then
    begin
    T:=GetTypeData(P^.PropType);
    if T^.ClassType.InheritsFrom(TBaseObjectList) then
      begin
      L:=TBaseObjectList(TBaseObjectClass(T^.ClassType).Create);
      SetObjectProp(Self,P,L);
      For D in AValue do
        L.AddObject('').LoadFromJSON(D.Value as TJSONObject);
      end
    else if T^.ClassType.InheritsFrom(TJSONSchema) then
      begin
      S:=TJSONSchema.Create;
      S.SetArrayProperty(P,AValue);
      SetObjectProp(Self,P,S);
      end
    else
      Raise ERESTAPI.CreateFmt('Unsupported class %s for property %s',[T^.ClassType.ClassName,P^.Name]);
    end
  else if P^.PropType^.Kind=tkDynArray then
    begin
    // Get array value
    AP:=GetObjectProp(Self,P);
    i:=Length(P^.PropType^.name);
    PA:=@(pdynarraytypeinfo(P^.PropType)^.elesize)+i;
    PA:=@(pdynarraytypeinfo(P^.PropType)^.eletype)+i;
    ET:=PTYpeInfo(PA^);
    if ET^.Kind=tkClass then
      begin
      // get object type name
      AN:=PTYpeInfo(PA^)^.Name;
      // Free all objects
      O:=TObjectArray(AP);
      For I:=0 to Length(O)-1 do
        FreeAndNil(O[i]);
      end;
    // Clear array
{$ifdef ver2_6}
    LPN:=Lowercase(P^.Name);
    SetArrayLength(LPN,0);
{$else}
    I:=0;
    DynArraySetLength(AP,P^.PropType,1,@i);
{$endif}
    // Now, set new length
    I:=AValue.Count;
    // Writeln(ClassName,' (Array) Setting length of array property ',P^.Name,' (type: ',P^.PropType^.Name,')  to ',AValue.Count);
{$ifdef ver2_6}
    // Workaround for bug in 2.6.4 that cannot set the array prop correctly.
    // Call helper routine and re-get array value
    SetArrayLength(LPN,i);
    AP:=GetObjectProp(Self,P);
{$else}
    DynArraySetLength(AP,P^.PropType,1,@i);
    I:=Length(TObjectArray(AP));
    SetDynArrayProp(P,AP);
{$endif}
    // Fill in all elements
    For I:=0 to AValue.Count-1 do
      begin
      Case ET^.Kind of
        tkClass :
          begin
          // Writeln(ClassName,' Adding instance of type: ',AN);
          TObjectArray(AP)[I]:=CreateObject(AN);
          TObjectArray(AP)[I].LoadFromJSON(AValue.Objects[i]);
          end;
        tkFloat :
          if IsDateTimeProp(ET) then
            TDateTimeArray(AP)[I]:=RFC3339ToDateTime(AValue.Strings[i])
          else
            TFloatArray(AP)[I]:=AValue.Floats[i];
        tkInt64 :
          TInt64Array(AP)[I]:=AValue.Int64s[i];
        tkBool :
          begin
          TBooleanArray(AP)[I]:=AValue.Booleans[i];
          end;
        tkInteger :
         TIntegerArray(AP)[I]:=AValue.Integers[i];
        tkUstring,
        tkWstring :
          TUnicodeStringArray(AP)[I]:=UTF8Decode(AValue.Strings[i]);
        tkString,
        tkAstring,
        tkLString :
          begin
          // Writeln('Setting String ',i,': ',AValue.Strings[i]);
          TStringArray(AP)[I]:=AValue.Strings[i];
          end;
      else
        Raise ERESTAPI.CreateFmt('%s: unsupported array element type : ',[ClassName,GetEnumName(TypeInfo(TTypeKind),Ord(ET^.Kind))]);
      end;
      end;
    end;
end;

procedure TBaseObject.SetObjectProperty(P: PPropInfo; AValue: TJSONObject);
Var
  O : TBaseObject;
  A: Pointer;
  T : PTypeData;
  D : TJSONEnum;
  AN : String;
  I : Integer;
  L : TBaseObjectList;
  NL : TBaseNamedObjectList;
  PA : ^pdynarraytypeinfo;

begin
  if P^.PropType^.Kind=tkDynArray then
    begin
    A:=GetDynArrayProp(P);
    For I:=0 to Length(TObjectArray(A))-1 do
      FreeAndNil(TObjectArray(A)[i]);
    // Writeln(ClassName,' (Object) Setting length of array property ',P^.Name,'(type: ',P^.PropType^.Name,')  to ',AValue.Count,' (current: ',Length(TObjectArray(A)),')');
    SetLength(TObjectArray(A),AValue.Count);
    i:=Length(P^.PropType^.name);
    PA:=@(pdynarraytypeinfo(P^.PropType)^.elesize)+i;
    PA:=@(pdynarraytypeinfo(P^.PropType)^.eletype)+i;
    AN:=PTYpeInfo(PA^)^.Name;
    I:=0;
    For D in AValue do
      begin
      O:=CreateObject(AN);
      TObjectArray(A)[I]:=O;
      // Writeln(ClassName,' Adding instance of type: ',AN,' for key ',D.Key);
      if IsPublishedProp(O,'name') then
        SetStrProp(O,'name',D.Key);
      O.LoadFromJSON(D.Value as TJSONObject);
      Inc(I);
      end;
    // Writeln(ClassName,' Done with array ',P^.Name,', final array length: ', Length(TObjectArray(A)));
    SetDynArrayProp(P,A);
    {
      For I:=0 to Length(TObjectArray(A))-1 do
        if IsPublishedProp(TObjectArray(A)[i],'name') then
    SetDynArrayProp(P,AP);
      //   Writeln(ClassName,'.',P^.name,'[',i,'] : ',getStrProp(TObjectArray(A)[I],'name'));
      B:=GetDynArrayProp(P);
      If Pointer(B)<>Pointer(A) then
      //  Writeln(ClassName,': Array ',P^.Name,'was not set correctly');
    }
    Exit;
    end;
  if Not (P^.PropType^.Kind=tkClass) then
    Raise ERESTAPI.CreateFmt('%s: Unsupported type for property %s',[ClassName,P^.Name]);
  T:=GetTypeData(P^.PropType);
  if T^.ClassType.InheritsFrom(TBaseObject) then
    begin
    O:=TBaseObject(GetObjectProp(Self,P,TBaseObject));
    if O=Nil then
      begin
      O:=TBaseObjectClass(T^.ClassType).Create;
      SetObjectProp(Self,P,O);
      end;
    O.LoadFromJSON(AValue);
    end
  else if T^.ClassType.InheritsFrom(TBaseObjectList) then
    begin
    L:=TBaseObjectList(TBaseObjectClass(T^.ClassType).Create);
    SetObjectProp(Self,P,L);
    For D in AValue do
      L.AddObject('').LoadFromJSON(D.Value as TJSONObject);
    end
  else if T^.ClassType.InheritsFrom(TBaseNamedObjectList) then
    begin
    NL:=TBaseNamedObjectList(TBaseObjectClass(T^.ClassType).Create);
    SetObjectProp(Self,P,L);
    For D in AValue do
      NL.AddObject(D.Key,'').LoadFromJSON(D.Value as TJSONObject);
    end
  else
    Raise ERESTAPI.CreateFmt('%s: unsupported class %s for property %s',[ClassName, T^.ClassType.ClassName,P^.Name]);
end;

procedure TBaseObject.SetSetProperty(P: PPropInfo; AValue: TJSONArray);

type
  TSet = set of 0..31;

var
  S,I,V : Integer;
  CurValue: string;
  EnumTyp: PTypeInfo;
  EnumTypData: PTypeData;

begin
  S:=0;
  EnumTyp:=GetTypeData(P^.PropType)^.CompType;
  EnumTypData:=GetTypeData(EnumTyp);
  For I:=0 to AValue.Count-1 do
    begin
    CurValue:=AValue.Strings[i];
    if Not TryStrToInt(CurValue,V) then
      V:=GetEnumValue(EnumTyp,CurValue);
    if (V<EnumTypData^.MinValue) or (V>EnumTypData^.MaxValue) or (V>31) then
      Raise ERESTAPI.CreateFmt('%s: Invalid value %s for property %s',[ClassName, CurValue,P^.Name]);
    Include(TSet(S),V);
    end;
  SetOrdProp(Self,P,S);
end;

procedure TBaseObject.SetEnumProperty(P: PPropInfo; AValue: TJSONData);
Var
  I : Integer;

begin
  I:=GetEnumValue(P^.PropType,AValue.AsString);
  if (I=-1) then
    Raise ERESTAPI.CreateFmt('%s: Invalid value %s for property %s',[ClassName, AValue.AsString,P^.Name]);
  SetOrdProp(Self,P,I);
end;

function TBaseObject.GetBooleanProperty(P: PPropInfo): TJSONData;
begin
  Result:=TJSONBoolean.Create(GetOrdProp(Self,P)<>0);
end;

function TBaseObject.GetIntegerProperty(P: PPropInfo): TJSONData;
begin
  Result:=TJSONIntegerNumber.Create(GetOrdProp(Self,P));
end;

function TBaseObject.GetInt64Property(P: PPropInfo): TJSONData;
begin
  Result:=TJSONInt64Number.Create(GetInt64Prop(Self,P));
end;

function TBaseObject.GetQwordProperty(P: PPropInfo): TJSONData;
begin
  Result:=TJSONInt64Number.Create(Int64(GetInt64Prop(Self,P)));
end;

function TBaseObject.GetFloatProperty(P: PPropInfo): TJSONData;
begin
  Case DateTimePropType(P^.PropType) of
    dtDateTime:
      Result:=TJSONString.Create(DateTimeToRFC3339(GetFloatProp(Self,P)));
    dtDate:
      Result:=TJSONString.Create(DateToRFC3339(GetFloatProp(Self,P)));
    dtTime:
      Result:=TJSONString.Create(TimeToRFC3339(GetFloatProp(Self,P))) ;
  else
    Result:=TJSONFloatNumber.Create(GetFloatProp(Self,P));
  end;
end;

function TBaseObject.GetStringProperty(P: PPropInfo): TJSONData;
begin
  Result:=TJSONString.Create(GetStrProp(Self,P));
end;

function TBaseObject.GetSetProperty(P: PPropInfo): TJSONData;

type
  TSet = set of 0..31;
var
  Typ: PTypeInfo;
  S, i: integer;
begin
  Result:=TJSONArray.Create;
  Typ:=GetTypeData(P^.PropType)^.CompType;
  S:=GetOrdProp(Self,P);
  for i:=Low(TSet) to High(TSet) do
    if (i in TSet(S)) then
      TJSONArray(Result).Add(TJSONString.Create(GetEnumName(Typ,i)));
end;


function TBaseObject.GetEnumeratedProperty(P: PPropInfo): TJSONData;
begin
  Result:=TJSONString.Create(GetEnumProp(Self,P));
end;

function TBaseObject.GetArrayProperty(P: PPropInfo): TJSONData;

Var
  AO : TObject;
  I : Integer;
  PA : ^pdynarraytypeinfo;
  ET : PTypeInfo;
  AP : Pointer;
  A : TJSONArray;
  O : TJSONObject;

begin
  A:=TJSONArray.Create;
  Result:=A;
  // Get array value type
  AP:=GetObjectProp(Self,P);
  i:=Length(P^.PropType^.name);
  PA:=@(pdynarraytypeinfo(P^.PropType)^.eletype)+i;
  ET:=PTYpeInfo(PA^);
  // Fill in all elements
  Case ET^.Kind of
  tkClass:
    For I:=0 to Length(TObjectArray(AP))-1 do
      begin
      // Writeln(ClassName,' Adding instance of type: ',AN);
      AO:=TObjectArray(AP)[I];
      if AO.InheritsFrom(TBaseObject) then
        begin
        O:=TJSONObject.Create;
        A.Add(O);
        TBaseObject(AO).SaveToJSON(O);
        end;
      end;
  tkFloat:
    if IsDateTimeProp(ET) then
      For I:=0 to Length(TDateTimeArray(AP))-1 do
        A.Add(TJSONString.Create(DateTimeToRFC3339(TDateTimeArray(AP)[I])))
    else
      For I:=0 to Length(TFloatArray(AP))-1 do
        A.Add(TJSONFloatNumber.Create(TFloatArray(AP)[I]));
  tkInt64:
    For I:=0 to Length(TInt64Array(AP))-1 do
      A.Add(TJSONInt64Number.Create(TInt64Array(AP)[I]));
  tkBool:
    For I:=0 to Length(TInt64Array(AP))-1 do
      A.Add(TJSONBoolean.Create(TBooleanArray(AP)[I]));
  tkInteger :
    For I:=0 to Length(TIntegerArray(AP))-1 do
     A.Add(TJSONIntegerNumber.Create(TIntegerArray(AP)[I]));
  tkUstring,
  tkWstring :
    For I:=0 to Length(TUnicodeStringArray(AP))-1 do
      A.Add(TJSONString.Create(TUnicodeStringArray(AP)[I]));
  tkString,
  tkAstring,
  tkLString :
    For I:=0 to Length(TStringArray(AP))-1 do
      A.Add(TJSONString.Create(TStringArray(AP)[I]));
  else
    Raise ERESTAPI.CreateFmt('%s: unsupported array element type : ',[ClassName,GetEnumName(TypeInfo(TTypeKind),Ord(ET^.Kind))]);
  end;
end;

function TBaseObject.GetObjectProperty(P: PPropInfo): TJSONData;

Var
  O : TObject;

begin
  O:=GetObjectProp(Self,P);
  if (O is TBaseObject) then
    Result:=TBaseObject(O).SaveToJSON
  else
    Result:=Nil; // maybe we need to add an option to return null ?
end;

procedure TBaseObject.ClearChildren(ChildTypes: TChildTypes);

Type
  TObjectArr = Array of TObject;

var
  PL: PPropList;
  P : PPropInfo;
  i,j,count,len:integer;
  A : pointer;
  PA : ^pdynarraytypeinfo;
  O : TObject;

begin
  Count:=GetPropList(Self,PL);
  try
    for i:=0 to Count-1 do
      begin
      P:=PL^[I];
      case P^.PropType^.Kind of
        tkClass:
          if (ctObject in ChildTypes) then
            begin
            // Writeln(ClassName,' Examining object: ',P^.Name);
            O:=GetObjectProp(Self,P);
            O.Free;
            SetObjectProp(Self,P,Nil);
            end;
        tkDynArray:
          if (ctArray in ChildTypes) then
            begin
            len:=Length(P^.PropType^.Name);
            PA:=@(pdynarraytypeinfo(P^.PropType)^.eletype)+len;
            if PTYpeInfo(PA^)^.Kind=tkClass then
              begin
              A:=GetDynArrayProp(P);
//              Writeln(ClassName,' Examining array: ',P^.Name,'Count:',Length(TObjectArr(A)));
              For J:=0 to Length(TObjectArr(A))-1 do
                begin
                FreeAndNil(TObjectArr(A)[J]);
                end;
              end;
            // Length is set to nil by destructor
            end;
      end;
      end;
  finally
    FreeMem(PL);
  end;
end;

class function TBaseObject.ClearChildTypes: TChildTypes;
begin
  Result:=[ctArray,ctObject]
end;


{$IFDEF DEBUGBASEOBJMEMLEAK}
Var
  ObjCounter : TStrings;
{$ENDIF}
constructor TBaseObject.Create(AOptions: TObjectOptions);
begin
{$IFDEF DEBUGBASEOBJMEMLEAK}
  if ObjCounter=Nil then
    ObjCounter:=TStringList.Create;
  ObjCounter.Values[ClassName]:=IntToStr(StrToIntDef(ObjCounter.Values[ClassName],0)+1);
{$ENDIF}
  ObjectOptions:=AOptions;
  // Do nothing
end;

destructor TBaseObject.Destroy;

begin
  StopRecordPropertyChanges;
{$IFDEF DEBUGBASEOBJMEMLEAK}
  ObjCounter.Values[ClassName]:=IntToStr(StrToIntDef(ObjCounter.Values[ClassName],0)-1);
{$ENDIF}
  FreeAndNil(fadditionalProperties);
  if ClearChildTypes<>[] then
    ClearChildren(ClearChildTypes);
  inherited;
end;

procedure TBaseObject.StartRecordPropertyChanges;
begin
  if Assigned(FBits) then
    FBits.ClearAll
  else
    FBits:=TBits.Create(GetTotalPropCount);
end;

procedure TBaseObject.ClearPropertyChanges;
begin
  FBits.ClearAll;
end;

procedure TBaseObject.StopRecordPropertyChanges;
begin
  FreeAndNil(FBits);
end;

function TBaseObject.IsPropertyModified(Info: PPropInfo): Boolean;
begin
  Result:=Not Assigned(FBits) or FBits.Bits[Info^.NameIndex]
end;

function TBaseObject.IsPropertyModified(const AName: String): Boolean;
begin
  Result:=IsPropertyModified(GetPropInfo(Self,AName));
end;

function TBaseObject.GetAdditionalProperties: TJSONObject;
begin
  if (fAdditionalProperties=Nil) and AllowAdditionalProperties then
    fAdditionalProperties:=TJSONObject.Create;
  Result:=fAdditionalProperties
end;

{$IFDEF VER2_6}
procedure TBaseObject.SetArrayLength(Const AName: String; ALength: Longint);
begin
  Raise ERestAPI.CreateFmt('Unknown Array %s',[AName]);
end;
{$ENDIF}

class function TBaseObject.AllowAdditionalProperties: Boolean;
begin
  Result:=False;
end;

class function TBaseObject.ExportPropertyName(const AName: String): string;
begin
  Result:=AName;
end;

class function TBaseObject.CleanPropertyName(const AName: String): string;

Const
   KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
       'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
       'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
       'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'+
       'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
       'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
       'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
       'private;published;length;setlength;';
Var
  I : Integer;

begin
  Result:=Aname;
  For I:=Length(Result) downto 1 do
    If Not ((Upcase(Result[i]) in ['_','A'..'Z'])
             or ((I>1) and (Result[i] in (['0'..'9'])))) then
     Delete(Result,i,1);
  if Pos(';'+lowercase(Result)+';',KW)<>0 then
   Result:='_'+Result
end;

class function TBaseObject.CreateObject(const AKind: String): TBaseObject;

Var
  C : TBaseObjectClass;

begin
  C:=RESTFactory.GetObjectClass(AKind);
  if C<>Nil then
    Result:=C.Create
  else
    Raise ERESTAPI.CreateFmt('Unknown class : "%s"',[AKind]);
  // Do nothing
end;

class procedure TBaseObject.RegisterObject;
begin
  RESTFactory.RegisterObject(Self);
end;

class function TBaseObject.ObjectRestKind: String;
begin
  Result:=ClassName;
end;

class function TBaseObject.GetTotalPropCount: Integer;
begin
  Result:=GetTypeData(ClassInfo)^.PropCount;
end;

class function TBaseObject.GetCurrentPropCount: Integer;
begin
  Result:=CountProperties(ClassInfo,False);
end;

class function TBaseObject.GetParentPropCount: Integer;

begin
  if (ClassParent=TBaseObject) or (ClassParent=Nil) then
    Result:=0
  else
    Result:=TBaseObjectClass(ClassParent).GetTotalPropCount;
end;

procedure TBaseObject.LoadPropertyFromJSON(const AName: String; JSON: TJSONData
  );

Var
  P : PPropInfo;
  o : TJSONObject;

begin
  // Writeln(ClassName,' loading : ',ANAme,' -> ',CleanPropertyName(aName));
  P:=GetPropInfo(Self,CleanPropertyName(aName));
  if (P=Nil) then
    begin
    o:=additionalProperties;
    if o=Nil then
      Raise ERESTAPI.CreateFmt('%s : Unknown property "%s"',[ClassName,AName]);
    o.Add(aName,JSON.Clone);
    end
  else
    case JSON.JSONType of
      jtstring  :
        if (P^.PropType^.Kind=tkEnumeration) then
          SetEnumProperty(P,JSON)
        else
          SetStringproperty(P,JSON.AsString);
      jtNumber  :
        case TJSONNumber(JSON).NumberType of
          ntFloat   : SetFloatProperty(P,JSON.asFloat);
          ntInteger : SetIntegerProperty(P,JSON.asInteger);
          ntInt64   : SetInt64Property(P,JSON.asInt64);
{$ifndef ver2_6}
          ntqword   : SetQWordProperty(P,JSON.asQWord);
{$endif}
        end;
      jtNull    : ClearProperty(P);
      jtBoolean : SetBooleanProperty(P,json.AsBoolean);
      jtArray   :
        if P^.PropType^.Kind=tkSet then
          SetSetProperty(P,TJSONArray(json))
        else
          SetArrayProperty(P,TJSONArray(json));
      jtObject   : SetObjectProperty(P,TJSONObject(json));
    end;
end;

function TBaseObject.SavePropertyToJSON(Info: PPropInfo): TJSONData;

begin
  Result:=Nil;
  if Not IsPropertyModified(Info) then
    Exit;
  Case Info^.PropType^.Kind of
   tkSet         : Result:=GetSetProperty(Info);
   tkEnumeration : Result:=GetEnumeratedProperty(Info);
   tkAstring,
   tkUstring,
   tkWString,
   tkwchar,
   tkuchar,
   tkString   : Result:=GetStringProperty(Info);
   tkFloat    : Result:=GetFloatProperty(Info);
   tkBool     : Result:=GetBooleanProperty(Info);
   tkClass    : Result:=GetObjectProperty(Info);
   tkDynArray : Result:=GetArrayProperty(Info);
   tkQWord    : Result:=GetQWordProperty(Info);
   tkInt64    : Result:=GetInt64Property(Info);
   tkInteger  : Result:=GetIntegerProperty(Info);
  end;
end;

procedure TBaseObject.LoadFromJSON(JSON: TJSONObject);

Var
  D : TJSONEnum;

begin
  StopRecordPropertyChanges;
  For D in JSON Do
    LoadPropertyFromJSON(D.Key,D.Value);
  StartRecordPropertyChanges;
end;

procedure TBaseObject.SaveToJSON(JSON: TJSONObject);

var
  PL: PPropList;
  P : PPropInfo;
  I,Count : integer;
  D : TJSONData;

begin
  Count:=GetPropList(Self,PL);
  try
    for i:=0 to Count-1 do
      begin
      P:=PL^[I];
      D:=SavePropertyToJSON(P);
      if (D<>Nil) then
        JSON.add(ExportPropertyName(P^.Name),D);
      end;
  finally
    FreeMem(PL);
  end;
end;

function TBaseObject.SaveToJSON: TJSONObject;
begin
  Result:=TJSONObject.Create;
  try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

finalization
{$IFDEF DEBUGBASEOBJMEMLEAK}
  if Assigned(ObjCounter) then
    begin
    Writeln(StdErr,'Object allocate-free count: ');
    Writeln(StdErr,ObjCounter.Text);
    FreeAndNil(ObjCounter);
    end;
{$ENDIF}
  FreeAndNil(Fact);
end.

