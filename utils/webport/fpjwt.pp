{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  JSON Web Token implementation
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
unit fpjwt;

{$mode objfpc}{$H+}

interface

uses
  TypInfo, Classes, SysUtils, fpjson, base64;

Type

  { TBaseJWT }

  TBaseJWT = Class(TPersistent)
  private
  Protected
    // Override this to disable writing a property to the JSON.
    function WriteProp(P: PPropInfo; All: Boolean): Boolean; virtual;
    function GetAsEncodedString: String; virtual;
    procedure SetAsEncodedString(AValue: String); virtual;
    function GetAsString: TJSONStringType; virtual;
    procedure SetAsString(AValue: TJSONStringType);virtual;
    Procedure DoLoadFromJSON(JSON : TJSONObject);virtual;
    Procedure DoSaveToJSON(JSON : TJSONObject; All : Boolean);virtual;
  Public
    Constructor Create; virtual;
    Procedure LoadFromJSON(JSON : TJSONObject);
    Procedure SaveToJSON(JSON : TJSONObject; All : Boolean);
    // Decode Base64 string. Padds the String with = to a multiple of 4
    Class Function DecodeString(S : String) : String;
    // Decode Base64 string and return a JSON Object. Padds the String with = to a multiple of 4
    Class Function DecodeStringToJSON(S : String) : TJSONObject;
    // Get/Set as string. This is normally the JSON form.
    Property AsString : TJSONStringType Read GetAsString Write SetAsString;
    // Set as string. This is normally the JSON form, encoded as Base64.
    Property AsEncodedString : String Read GetAsEncodedString Write SetAsEncodedString;
  end;

  { TJOSE }

  TJOSE = Class(TBaseJWT)
  private
    Falg: String;
    Fcrit: String;
    Fcty: String;
    Fjku: String;
    Fjwk: String;
    Fkid: String;
    Ftyp: String;
    Fx5c: String;
    Fx5t: String;
    Fx5ts256: String;
    Fx5u: String;
  Published
    // Registered names. Keep the case lowercase, the RTTI must match the registered name.
    Property cty : String Read Fcty Write Fcty;
    Property typ : String Read Ftyp Write Ftyp;
    Property alg : String Read Falg Write Falg;
    Property jku : String Read Fjku Write fjku;
    Property jwk : String Read Fjwk Write fjwk;
    Property kid : String Read Fkid Write fkid;
    Property x5u : String Read Fx5u Write fx5u;
    Property x5c : String Read Fx5c Write fx5c;
    Property x5t : String Read Fx5t Write fx5t;
    Property x5ts256 : String Read Fx5ts256 Write fx5ts256;
    Property crit : String Read Fcrit Write fcrit;
  end;
  TJOSEClass = Class of TJOSE;

  { TClaims }

  TClaims = Class(TBaseJWT)
  private
    FAud: String;
    FExp: Int64;
    FIat: Int64;
    FIss: String;
    FJTI: String;
    FNbf: Int64;
    FSub: String;
  Published
    // Registered Claim Names. Keep the case lowercase, the RTTI must match the registered name.
    Property iss : String Read FIss Write FIss;
    Property sub : String Read FSub Write FSub;
    Property aud : String Read FAud Write FAud;
    Property exp : Int64 Read FExp Write FExp;
    Property nbf : Int64 Read FNbf Write FNbf;
    Property iat : Int64 Read FIat Write FIat;
    Property jti : String Read FJTI Write FJTI;
  end;
  TClaimsClass = Class of TClaims;

  { TJWT }

  TJWT = Class(TBaseJWT)
  private
    FClaims: TClaims;
    FJOSE: TJOSE;
    FSignature: String;
    procedure SetClaims(AValue: TClaims);
    procedure SetJOSE(AValue: TJOSE);
  Protected
    Function CreateJOSE : TJOSE; Virtual;
    Function CreateClaims : TClaims; Virtual;
    // AsString and AsEncodedString are the same in this case.
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(AValue: TJSONStringType);override;
    function GetAsEncodedString: String;override;
    Procedure SetAsEncodedString (AValue : String);override;
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    // Owned by the JWT. The JSON header.
    Property JOSE : TJOSE Read FJOSE Write SetJOSE;
    // Owned by the JWT. The set of claims. The actuall class will depend on the descendant.
    Property Claims : TClaims Read FClaims Write SetClaims;
    Property Signature : String Read FSignature Write FSignature;
  end;

implementation

uses strutils;

{ TJWT }

procedure TJWT.SetClaims(AValue: TClaims);
begin
  if FClaims=AValue then Exit;
  FClaims:=AValue;
end;

procedure TJWT.SetJOSE(AValue: TJOSE);
begin
  if FJOSE=AValue then Exit;
  FJOSE:=AValue;
end;

function TJWT.CreateJOSE: TJOSE;
begin
  Result:=TJOSE.Create;
end;

function TJWT.CreateClaims: TClaims;
begin
  Result:=TClaims.Create;
end;

function TJWT.GetAsString: TJSONStringType;
begin
  Result:=EncodeStringBase64(JOSE.AsString);
  Result:=Result+'.'+EncodeStringBase64(Claims.AsString);
  If (Signature<>'') then
    Result:=Result+'.'+Signature;
end;


function TJWT.GetAsEncodedString: String;
begin
  Result:=GetAsString;
end;

procedure TJWT.SetAsEncodedString(AValue: String);
begin
  SetAsString(AValue);
end;

constructor TJWT.Create;
begin
  Inherited;
  FJOSE:=CreateJOSE;
  FClaims:=CreateCLaims;
end;

destructor TJWT.Destroy;
begin
  FreeAndNil(FJOSE);
  FreeAndNil(FClaims);
  Inherited;
end;

procedure TJWT.SetAsString(AValue: TJSONStringType);

Var
  J,C,S : String;

begin
  J:=ExtractWord(1,AValue,['.']);
  C:=ExtractWord(2,AValue,['.']);
  S:=ExtractWord(3,AValue,['.']);
  JOSE.AsEncodedString:=J;
  Claims.AsEncodedString:=C;
  Signature:=S;
end;

{ TBaseJWT }

function TBaseJWT.GetAsEncodedString: String;
begin
  Result:=EncodeStringBase64(AsString)
end;

procedure TBaseJWT.SetAsEncodedString(AValue: String);

begin
  AsString:=DecodeString(AValue);
end;

function TBaseJWT.GetAsString: TJSONStringType;

Var
  O : TJSONObject;

begin
  O:=TJSONObject.Create;
  try
    SaveToJSON(O,False);
    Result:=O.AsJSON;
  finally
    O.Free;
  end;
end;

procedure TBaseJWT.SetAsString(AValue: TJSONStringType);
Var
  D : TJSONData;
  O : TJSONObject absolute D;

begin
  D:=GetJSON(AValue);
  try
    if D is TJSONObject then
      LoadFromJSON(O);
  finally
    D.Free;
  end;
end;

procedure TBaseJWT.DoLoadFromJSON(JSON: TJSONObject);

Var
  D : TJSONEnum;
  P : PPropinfo;

begin
  For D in JSON Do
    begin
    P:=GetPropInfo(Self,D.Key);
    if (P<>Nil) and not D.Value.IsNull then
      Case P^.PropType^.Kind of
        tkInteger : SetOrdProp(Self,P,D.Value.AsInteger);
        tkChar :
            if D.Value.AsString<>'' then
              SetOrdProp(Self,P,Ord(D.Value.AsString[1]));
        tkEnumeration :
          if (D.Value.JSONType=jtNumber) and (TJSONNumber(D.Value).NumberType=ntInteger) then
            SetOrdProp(Self,P,D.Value.AsInteger)
          else
            SetOrdProp(Self,P,GetEnumValue(p^.PropType,D.Value.AsString));
        tkFloat :
          SetFloatProp(Self,P,D.Value.AsFloat);
        tkSString,tkLString,tkAString :
            SetStrProp(Self,P,D.Value.AsString);
        tkWChar, tkUString,tkWString,tkUChar:
            SetWideStrProp(Self,P,D.Value.AsString);
        tkBool :
          SetOrdProp(Self,P,Ord(D.Value.AsBoolean));
        tkInt64,tkQWord:
          SetInt64Prop(Self,P,Ord(D.Value.AsInt64));
        end;
   end;
end;

function TBaseJWT.WriteProp(P: PPropInfo; All: Boolean): Boolean;

begin
  Result:=True;
end;

procedure TBaseJWT.DoSaveToJSON(JSON: TJSONObject; All: Boolean);


Var
  D : TJSONEnum;
  P : PPropinfo;
  PL : PPropList;
  I,VI,Count : Integer;
  VF : Double;
  C : Char;
  CW : WideChar;
  I64 : Int64;
  W : UnicodeString;
  S : String;

begin
  Count:=GetPropList(Self,PL);
  try
    For I:=0 to Count-1 do
      begin
      P:=PL^[i];
      if WriteProp(P,All) then
        Case P^.PropType^.Kind of
          tkInteger :
            begin
            VI:=GetOrdProp(Self,P);
            if All or (VI<>0) then
              JSON.Add(P^.Name,VI);
            end;
          tkChar :
            begin
            C:=Char(GetOrdProp(Self,P));
            if All or (C<>#0) then
              if C=#0 then
                JSON.Add(p^.Name,'')
              else
                JSON.Add(p^.Name,C);
            end;
          tkEnumeration :
            begin
            vi:=GetOrdProp(Self,P);
            JSON.Add(P^.Name,GetEnumName(p^.PropType,VI));
            end;
          tkFloat :
            begin
            VF:=GetFloatProp(Self,P);
            If All or (VF<>0) then
              JSON.Add(P^.Name,VF);
            end;
          tkSString,tkLString,tkAString :
            begin
            S:=GetStrProp(Self,P);
            if All or (S<>'') then
              JSON.Add(P^.Name,S);
            end;
          tkWChar:
            begin
            CW:=WideChar(GetOrdProp(Self,P));
            if All or (CW<>#0) then
              if CW=#0 then
                JSON.Add(p^.Name,'')
              else
                JSON.Add(p^.Name,Utf8Encode(WideString(CW)));
            end;
          tkUString,tkWString,tkUChar:
             begin
              W:=GetWideStrProp(Self,P);
              if All or (W<>'') then
                JSON.Add(P^.Name,Utf8Encode(W));
              end;
          tkBool :
            JSON.Add(P^.Name,(GetOrdProp(Self,P)<>0));
          tkInt64,tkQWord:
            begin
            I64:=GetInt64Prop(Self,P);
            if All or (I64<>0) then
              JSON.Add(p^.Name,I64);
            end;
          end;
      end;
  finally
    FreeMem(PL);
  end;
end;

constructor TBaseJWT.Create;
begin
  Inherited Create;
end;

procedure TBaseJWT.LoadFromJSON(JSON: TJSONObject);
begin
  DoLoadFromJSon(JSON);
end;

procedure TBaseJWT.SaveToJSON(JSON: TJSONObject; All: Boolean);
begin
  DoSaveToJSon(JSON,All);
end;

class function TBaseJWT.DecodeString(S: String): String;

Var
  R : Integer;

begin
  R:=(length(S) mod 4);
  if R<>0 then
    S:=S+StringOfChar('=',4-r);
  Result:=DecodeStringBase64(S);
end;

class function TBaseJWT.DecodeStringToJSON(S: String): TJSONObject;

Var
  D : TJSONData;
begin
  D:=GetJSON(DecodeString(S));
  if not (D is TJSONData) then
    FreeAndNil(D);
  Result:=TJSONObject(D);
end;

end.

