unit vg_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, vg_scene, vg_controls, vg_objects,
  typinfo, strutils, matchutil;

type

  { TVgParser }
  TvgObjectArray = array of TvgObject;

  TvgParser = class
  private
    FParent: TvgObject;
    procedure InnerFindWidgets(AParent: TvgObject; APattern: string; UseMark: Boolean; var ARet: TvgObjectArray);
    function IsBasicProp(AName: string): Boolean;
    function InnerFindWidget(AParent: TvgObject; Aid: Integer; AName: string; UseName: Boolean = False): TvgObject;
    procedure InnerFindClasses(AParent: TvgObject; AClassName: string; AClass: TClass; UseClass: Boolean; var Aret: TvgObjectArray);
  protected
    procedure SetProperties(AObj: TObject; AName: string; AValue: string);
    function ParseParent(AJson: TJSONObject): TvgObject;
    procedure ParseBasic(AObj: TvgObject; AJson: TJSONObject);
    procedure ParsePosition(AObj: TvgObject; AJson: TJSONObject);
    procedure ParseOther(AObj: TvgObject; AJson: TJSONObject);
    procedure ParseOne(AJson: TJSONObject);
  public
    constructor Create;
    constructor Create(AParent: TvgObject);
    procedure Parse(AJson: TJSONArray);
    function FindById(AId: Integer): TvgObject; overload;
    function FindByName(AName: string): TvgObject; overload;
    function FindByNamePattern(APattern: string):TvgObjectArray;
    function FindByMarkPattern(APattern: string): TvgObjectArray;
    function FindByClass(AClassName: string): TvgObjectArray; overload;
    function FindByClass(AClass: TClass): TvgObjectArray; overload;
  public
    property Parent: TvgObject read FParent write FParent;
  end;

implementation

{ TVgParser }

procedure TvgParser.InnerFindWidgets(AParent: TvgObject; APattern: string;
  UseMark: Boolean; var ARet: TvgObjectArray);
var
  i: Integer;
begin
  for i:=0 to AParent.ChildrenCount -1 do
  begin
    if AParent.Children[i] is TvgObject then
    begin
      if UseMark then
      begin
        if IsMatched(AParent.Children[i].Mark, APattern) then
        begin
          SetLength(ARet, Length(ARet)+1);
          ARet[Length(ARet)-1] := AParent.Children[i];
        end;
      end
      else
      begin
        if IsMatched(AParent.Children[i].Name, APattern) then
        begin
          SetLength(ARet, Length(ARet)+1);
          ARet[Length(ARet)-1] := AParent.Children[i];
        end;
      end;
      InnerFindWidgets(AParent.Children[i], APattern, UseMark, ARet);
    end;
  end;
end;

function TvgParser.IsBasicProp(AName: string): Boolean;
const
  DONOT_PARSE: array[0..8] of string = ('class','parent','id','name','x','y','width','height','align');
var
  i: Integer;
begin
  Result := False;
  for i:=0 to Length(DONOT_PARSE) -1 do
  begin
    if AName = DONOT_PARSE[i] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TvgParser.InnerFindWidget(AParent: TvgObject; Aid: Integer;
  AName: string; UseName: Boolean): TvgObject;
var
  i: Integer;
  SName: string;
begin
  Result := nil;
  AName:=TRim(LowerCase(AName));
  for i:= 0 to AParent.ChildrenCount -1 do
  begin
    if Result <> nil then
       Break;
    if AParent.Children[i] is TvgObject then begin
      if UseName then begin
        SName := Trim(LowerCase(AParent.Children[i].Name));
        if SName = AName then begin
          Result := AParent.Children[i];
          Break;
        end else begin
          Result := InnerFindWidget(AParent.Children[i], Aid, AName, UseName);
        end;
      end else begin
        if AParent.Children[i].Id = Aid then begin
          Result := AParent.Children[i];
          Break;
        end else
          Result := InnerFindWidget(AParent.Children[i], Aid, AName, UseName);
      end;
    end;
  end;
end;

procedure TvgParser.InnerFindClasses(AParent: TvgObject; AClassName: string;
  AClass: TClass; UseClass: Boolean; var Aret: TvgObjectArray);
var
  i: Integer;
begin
  for i:=0 to AParent.ChildrenCount -1 do
  begin
    if AParent.Children[i] is TvgObject then
    begin
      if UseClass then begin
        if AParent.Children[i].ClassType = AClass then begin
          SetLength(ARet, Length(ARet)+1);
          ARet[Length(ARet)-1] := AParent.Children[i];
        end;
      end else begin
        if AParent.Children[i].ClassName = AClassName then begin
          SetLength(ARet, Length(ARet)+1);
          ARet[Length(ARet)-1] := AParent.Children[i];
        end;
      end;
      InnerFindClasses(AParent.Children[i], AClassName, AClass, UseClass, ARet);
    end;
  end;

end;

procedure TvgParser.SetProperties(AObj: TObject; AName: string;
  AValue: string);
var
  APName: string;
  ARName: string;
  APropInfo: PPropInfo;
  ASubObj: TObject;
  AJsonParser: TJSONParser;
  AJsonObj: TJSONData;
begin
  if not AnsiContainsText(AName, '.') then
  begin
    APropInfo := GetPropInfo(AObj, AName);
    if APropInfo <> nil then
    begin
      case APropInfo^.PropType^.Kind of
        tkChar, tkSString, tkLString, tkAString, tkWString, tkWChar, tkUString:SetStrProp(AObj, AName, AValue);
        tkInteger, tkInt64, tkQWord: SetInt64Prop(AObj, AName, StrToInt(AValue));
        tkFloat:SetFloatProp(AObj, AName, StrToFloat(AValue));
        tkBool:SetPropValue(AObj, AName, (LowerCase(AValue)='true'));
        tkEnumeration:SetEnumProp(AObj, AName, AValue);
        tkClass:
          begin
            if (APropInfo^.PropType^.Name = 'TJSONData') or (APropInfo^.PropType^.Name = 'TJSONArray') or (APropInfo^.PropType^.Name = 'TJSONObject') then begin
              AJsonParser := TJSONParser.Create(AValue);
              AJsonObj := AJsonParser.Parse;
              AJsonParser.Free;
              SetObjectProp(AObj, AName, AJsonObj);
            end;
          end;
      end;
    end;
  end
  else
  begin
    APName:=LeftStr(AName, Pos('.', AName)-1);
    ARName:=Copy(AName, Length(APName)+2, Length(AName)-Length(APName)-1);
    APropInfo:=GetPropInfo(AObj, APName);
    if APropInfo <> nil then
    begin
      case APropInfo^.PropType^.Kind of
        tkClass,tkRecord:ASubObj:=GetObjectProp(AObj, APName);
      end;
      SetProperties(ASubObj, ARName, AValue);
    end;
  end;
end;

function TvgParser.ParseParent(AJson: TJSONObject): TvgObject;
var
  strParent: string;
  intParent: Integer;
  objParent: TvgObject;
  jParent: TJSONData;
begin
  jParent := AJson.Find('parent');
  if jParent = nil then begin
    Result := FParent;
    Exit;
  end;
  if AJson.Nulls['parent'] then
  begin
    Result := FParent;
    Exit;
  end;
  strParent:=AJson.Strings['parent'];
  if (strParent = '') or (strParent = 'nil') or (strParent = 'null') then begin
    objParent := FParent;
  end else begin
    intParent:= StrToIntDef(strParent, -1);
    if intParent <> -1 then
      objParent := FindById(intParent)
    else
      objParent := FindByName(strParent);
    if objParent = nil then
      raise Exception.Create(Format('parent %s not exists', [strParent]));
  end;
  Result := objParent;
end;

procedure TvgParser.ParseBasic(AObj: TvgObject; AJson: TJSONObject);
begin
  try AObj.Id:=AJson.Integers['id']; except end;
  try AObj.Name:=AJson.Strings['name']; except end;
  try AObj.Mark:=AJson.Strings['mark']; except end;
end;

procedure TvgParser.ParsePosition(AObj: TvgObject; AJson: TJSONObject);
var
  vobj: TvgVisualObject;
begin
  vobj := TvgVisualObject(AObj);
  try vobj.Position.X:=AJson.Integers['x']; except end;
  try vobj.Position.Y:=AJson.Integers['y']; except end;
  try vobj.Width:=AJson.Integers['width']; except end;
  try vobj.Height:=AJson.Integers['height']; except end;
  try SetEnumProp(vobj, 'Align', AJson.Strings['align']); except end;
end;

procedure TvgParser.ParseOther(AObj: TvgObject; AJson: TJSONObject);

var
  i: Integer;
  AValue: string;
begin
  for i:=0 to AJson.Count -1 do
    if not IsBasicProp(AJson.Names[i]) then
    begin
      AValue:=AJson.Items[i].AsJSON;
      if LeftStr(AValue, 1) = '"' then
        AValue:=RightStr(AValue, Length(AValue)-1);
      if RightStr(AValue, 1) = '"' then
        AValue:=LeftStr(AValue, Length(AValue)-1);
      SetProperties(AObj, AJson.Names[i], AValue);
    end;
end;

procedure TvgParser.ParseOne(AJson: TJSONObject);
var
  objcls: TvgObjectClass;
  obj: TvgObject;
  objParent: TvgObject;
begin
  objParent := ParseParent(AJson);
  objcls:=TvgObjectClass(GetClass(AJson.Strings['class']));
  obj := objcls.Create(objParent);
  obj.Parent := objParent;
  ParseBasic(obj, AJson);
  ParsePosition(obj, AJson);
  ParseOther(obj, AJson);
end;

constructor TvgParser.Create;
begin
end;

constructor TvgParser.Create(AParent: TvgObject);
begin
  FParent := AParent;
end;

procedure TvgParser.Parse(AJson: TJSONArray);
var
  i: integer;
begin
  if not Assigned(FParent) then
    raise Exception.Create('parent cannot be empty.');
  for i:=0 to AJson.Count -1 do
    ParseOne(AJson.Objects[i]);
end;

function TvgParser.FindById(AId: Integer): TvgObject;
begin
  Result := InnerFindWidget(FParent, AId, '', False);
end;

function TvgParser.FindByName(AName: string): TvgObject;
begin
  Result := InnerFindWidget(FParent, -1, AName, True);
end;

function TvgParser.FindByNamePattern(APattern: string): TvgObjectArray;
begin
  InnerFindWidgets(FParent, APattern, False, Result);
end;

function TvgParser.FindByMarkPattern(APattern: string): TvgObjectArray;
begin
  InnerFindWidgets(FParent, APattern, True, Result);
end;

function TvgParser.FindByClass(AClassName: string): TvgObjectArray;
begin
  InnerFindClasses(FParent, AClassName, nil, False, Result);
end;

function TvgParser.FindByClass(AClass: TClass): TvgObjectArray;
begin
  InnerFindClasses(FParent, '', AClass, True, Result);
end;

end.

