{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  REST classes code generator base.
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}

unit restcodegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TCodegenLogType = (cltInfo);
  TCodegenLogTypes = Set of TCodegenLogType;
  TCodeGeneratorLogEvent = Procedure (Sender : TObject; LogType : TCodegenLogType; Const Msg : String) of object;
  { TRestCodeGenerator }

  TRestCodeGenerator = Class(TComponent)
  Private
    FAddTimeStamp: Boolean;
    FBaseClassName: String;
    FBaseListClassName: String;
    FClassPrefix: String;
    FExtraUnits: String;
    FLicenseText: TStrings;
    FOnLog: TCodeGeneratorLogEvent;
    FOutputUnitName: String;
    FSource : TStrings;
    Findent : String;
  Protected
    // Source manipulation
    Procedure DoLog(Const Msg : String; AType : TCodegenLogType = cltInfo);
    Procedure DoLog(Const Fmt : String; Args : Array of const; AType : TCodegenLogType = cltInfo);
    Procedure CreateHeader; virtual;
    Procedure IncIndent;
    Procedure DecIndent;
    Function MakePascalString(S: String; AddQuotes: Boolean=False): String;
    Function PrettyPrint(Const S: string): String;
    Procedure AddLn(Const Aline: string);
    Procedure AddLn(Const Alines : array of string);
    Procedure AddLn(Const Alines : TStrings);
    Procedure AddLn(Const Fmt: string; Args : Array of const);
    Procedure Comment(Const AComment : String; Curly : Boolean = False);
    Procedure Comment(Const AComment : Array of String);
    Procedure Comment(Const AComment : TStrings);
    Procedure ClassHeader(Const AClassName: String); virtual;
    Procedure SimpleMethodBody(Lines: Array of string); virtual;
    Function BaseUnits : String; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure SaveToStream(const AStream: TStream);
    Procedure SaveToFile(Const AFileName : string);
    Procedure LoadFromFile(Const AFileName : string);
    Procedure LoadFromStream(Const AStream : TStream); virtual; abstract;
    Procedure Execute; virtual; abstract;
    Property Source : TStrings Read FSource;
  Published
    Property BaseClassName : String Read FBaseClassName Write FBaseClassName;
    Property BaseListClassName : String Read FBaseListClassName Write FBaseListClassName;
    Property OutputUnitName : String Read FOutputUnitName Write FOutputUnitName;
    Property ExtraUnits : String Read FExtraUnits Write FExtraUnits;
    Property ClassPrefix : String Read FClassPrefix Write FClassPrefix;
    Property LicenseText : TStrings Read FLicenseText;
    Property OnLog : TCodeGeneratorLogEvent Read FOnLog Write FOnlog;
    Property AddTimeStamp : Boolean Read FAddTimeStamp Write FAddTimeStamp;
  end;

implementation

{ TRestCodeGenerator }
procedure TRestCodeGenerator.IncIndent;
begin
  FIndent:=FIndent+StringOfChar(' ',2);
end;

procedure TRestCodeGenerator.DecIndent;

Var
  L : Integer;
begin
  L:=Length(Findent);
  if L>0  then
    FIndent:=Copy(FIndent,1,L-2)
end;

procedure TRestCodeGenerator.AddLn(const Aline: string);

begin
  FSource.Add(FIndent+ALine);
end;

procedure TRestCodeGenerator.AddLn(const Alines: array of string);

Var
  S : String;

begin
  For s in alines do
    Addln(S);
end;

procedure TRestCodeGenerator.AddLn(const Alines: TStrings);
Var
  S : String;

begin
  For s in alines do
    Addln(S);
end;

procedure TRestCodeGenerator.AddLn(const Fmt: string; Args: array of const);
begin
  AddLn(Format(Fmt,Args));
end;

procedure TRestCodeGenerator.Comment(const AComment: String; Curly: Boolean);
begin
  if Curly then
    AddLn('{ '+AComment+' }')
  else
    AddLn('//'+AComment);
end;

procedure TRestCodeGenerator.Comment(const AComment: array of String);
begin
  AddLn('{');
  IncIndent;
  AddLn(AComment);
  DecIndent;
  AddLn('}');
end;

procedure TRestCodeGenerator.Comment(const AComment: TStrings);
begin
  AddLn('{');
  IncIndent;
  AddLn(AComment);
  DecIndent;
  AddLn('}');
end;



constructor TRestCodeGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSource:=TstringList.Create;
  FLicenseText:=TstringList.Create;
end;

destructor TRestCodeGenerator.Destroy;
begin
  FreeAndNil(FLicenseText);
  FreeAndNil(FSource);
  inherited Destroy;
end;


procedure TRestCodeGenerator.LoadFromFile(const AFileName: string);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TRestCodeGenerator.SaveToStream(const AStream : TStream);

begin
  if (FSource.Count=0) then
    Execute;
  FSource.SaveToStream(AStream)
end;

procedure TRestCodeGenerator.SaveToFile(const AFileName: string);

Var
  F : TFileStream;
  B : Boolean;

begin
  B:=False;
  F:=Nil;
  try
    B:=(Source.Count=0) and (OutputUnitName='');
    if B then
      OutputUnitname:=ChangeFileExt(ExtractFileName(AFileName),'');
    F:=TFileStream.Create(aFilename,fmCreate);
    SaveToStream(F);
  finally
    F.Free;
    if B then
      OutputUnitName:='';
  end;
end;

procedure TRestCodeGenerator.DoLog(const Msg: String; AType: TCodegenLogType);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,Atype,Msg);
end;

procedure TRestCodeGenerator.DoLog(const Fmt: String; Args: array of const;
  AType: TCodegenLogType);
begin
  DoLog(Format(Fmt,Args),AType);
end;

procedure TRestCodeGenerator.CreateHeader;

Var
  B,S : String;

begin
  if LicenseText.Count>0 then
    Comment(LicenseText);
  if AddTimeStamp then
    Comment('Generated on: '+DateTimeToStr(Now));
  addln('{$MODE objfpc}');
  addln('{$H+}');
  addln('');
  addln('interface');
  addln('');
  S:=ExtraUnits;
  B:=BaseUnits;
  if (B<>'') then
    if (S<>'') then
      begin
      if (B[Length(B)]<>',') then
        B:=B+',';
      S:=B+S;
      end
    else
      S:=B;
  addln('uses sysutils, classes, %s;',[S]);
  addln('');
end;

procedure TRestCodeGenerator.SimpleMethodBody(Lines: array of string);

Var
   S : String;

begin
  AddLn('');
  AddLn('begin');
  IncIndent;
  For S in Lines do
    AddLn(S);
  DecIndent;
  AddLn('end;');
  AddLn('');
end;

function TRestCodeGenerator.BaseUnits: String;
begin
  Result:='';
end;


function TRestCodeGenerator.MakePascalString(S: String; AddQuotes: Boolean
  ): String;

begin
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
  if AddQuotes then
    Result:=''''+Result+'''';
end;

function TRestCodeGenerator.PrettyPrint(const S: string): String;

begin
  If (S='') then
    Result:=''
  else
    Result:=Upcase(S[1])+Copy(S,2,Length(S)-1);
end;

procedure TRestCodeGenerator.ClassHeader(const AClassName: String);

begin
  AddLn('');
  AddLn('{ '+StringOfChar('-',68));
  AddLn('  '+AClassName);
  AddLn('  '+StringOfChar('-',68)+'}');
  AddLn('');
end;

end.

