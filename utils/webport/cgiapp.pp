{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    TCGIApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}

unit cgiapp;

Interface

uses
  CustApp,Classes, SysUtils, httpdefs;

Const
  CGIVarCount = 23 deprecated;

Type
  TCGIVarArray = Array [1..CGIVarCount] of String;

Const
  CgiVarNames : TCGIVarArray =
   ('AUTH_TYPE',
    'CONTENT_LENGTH',
    'CONTENT_TYPE',
    'GATEWAY_INTERFACE',
    'PATH_INFO',
    'PATH_TRANSLATED',
    'QUERY_STRING', 'REMOTE_ADDR',
    'REMOTE_HOST',
    'REMOTE_IDENT',
    'REMOTE_USER',
    'REQUEST_METHOD',
    'SCRIPT_NAME',
    'SERVER_NAME',
    'SERVER_PORT',
    'SERVER_PROTOCOL',
    'SERVER_SOFTWARE',
    'HTTP_ACCEPT',
    'HTTP_ACCEPT_CHARSET',
    'HTTP_ACCEPT_ENCODING',
    'HTTP_IF_MODIFIED_SINCE',
    'HTTP_REFERER',
    'HTTP_USER_AGENT')  deprecated;

Type

  TCgiApplication = Class(TCustomApplication)
  Private
    FResponse : TStream;
    FEmail : String;
    FAdministrator : String;
    FContentTypeEmitted : Boolean;
    FCGIVars : TCGIVarArray;
    FRequestVars,
    FFormFiles : TStrings;
    Function GetCGIVar (Index : Integer) : String;
    Procedure InitCGIVars;
    Procedure InitRequestVars;
    Procedure InitPostVars;
    Procedure InitGetVars;
    Procedure SetContentLength (Value : Integer);
    Procedure SetCGIVar(Index : Integer; Value : String);
    Function GetContentLength : Integer;
    Function GetServerPort : Word;
    Function GetEmail : String;
    Function GetAdministrator : String;
    Procedure ProcessQueryString(Const FQueryString : String);
    Function GetRequestVariable(Const VarName : String) : String;
    Function GetRequestVariableCount : Integer;
    Procedure ProcessURLEncoded(M : TMemoryStream);
    Procedure ProcessMultiPart(M : TMemoryStream; Const Boundary : String);
  Public
    Constructor Create(AOwner : TComponent); override; deprecated;
    Destructor Destroy; override; deprecated;
    Procedure AddResponse(Const S : String); deprecated;
    Procedure AddResponse(Const Fmt : String; Args : Array of const); deprecated;
    Procedure AddResponseLn(Const S : String); deprecated;
    Procedure AddResponseLn(Const Fmt : String; Args : Array of const); deprecated;
    Procedure Initialize; override; deprecated;
    Procedure GetCGIVarList(List : TStrings); deprecated;
    Procedure GetRequestVarList(List : TStrings); deprecated;
    Procedure GetRequestVarList(List : TStrings; NamesOnly : Boolean); deprecated;
    Procedure ShowException(E: Exception);override; deprecated;
    Procedure DeleteFormFiles; deprecated;
    Function EmitContentType : Boolean; deprecated;
    Function GetTempCGIFileName : String; deprecated;
    Function VariableIsUploadedFile(Const VarName : String) : boolean; deprecated;
    Function UploadedFileName(Const VarName : String) : String; deprecated;
    Property AuthType : String Index 1 Read GetCGIVar; deprecated;
    Property ContentLength : Integer Read GetContentLength Write SetContentLength; deprecated; // Index 2
    Property ContentType : String Index 3 Read GetCGIVar Write SetCGIVar; deprecated;
    Property GatewayInterface : String Index 4 Read GetCGIVar; deprecated;
    Property PathInfo : String index 5 read GetCGIvar; deprecated;
    Property PathTranslated : String Index 6 read getCGIVar; deprecated;
    Property QueryString : String Index 7 read getcgivar; deprecated;
    Property RemoteAddress : String Index 8 read GetCGIVar; deprecated;
    Property RemoteHost : String Index 9 read GetCGIVar; deprecated;
    Property RemoteIdent : String Index 10 read GetCGIVar; deprecated;
    Property RemoteUser : String Index 11 read GetCGIVar; deprecated;
    Property RequestMethod : String Index 12 read GetCGIVar; deprecated;
    Property ScriptName : String Index 13 read GetCGIVar; deprecated;
    Property ServerName : String Index 14 read GetCGIVar; deprecated;
    Property ServerPort : Word Read GetServerPort; deprecated; // Index 15
    Property ServerProtocol : String Index 16 read GetCGIVar; deprecated;
    Property ServerSoftware : String Index 17 read GetCGIVar; deprecated;
    Property HTTPAccept : String Index 18 read GetCGIVar; deprecated;
    Property HTTPAcceptCharset : String Index 19 read GetCGIVar; deprecated;
    Property HTTPAcceptEncoding : String Index 20 read GetCGIVar; deprecated;
    Property HTTPIfModifiedSince : String Index 21 read GetCGIVar; deprecated; // Maybe change to TDateTime ??
    Property HTTPReferer : String Index 22 read GetCGIVar; deprecated;
    Property HTTPUserAgent : String Index 23 read GetCGIVar; deprecated;
    Property Email : String Read GetEmail Write FEmail; deprecated;
    Property Administrator : String Read GetAdministrator Write FAdministrator; deprecated;
    Property RequestVariables[VarName : String] : String Read GetRequestVariable; deprecated;
    Property RequestVariableCount : Integer Read GetRequestVariableCount; deprecated;
    Property Response : TStream Read FResponse; deprecated;
  end;

  ECGI = Class(Exception);

ResourceString
  SWebMaster = 'webmaster' deprecated;
  SCGIError  = 'CGI Error' deprecated;
  SAppEncounteredError = 'The application encountered the following error:' deprecated;
  SError     = 'Error: ' deprecated;
  SNotify    = 'Notify: ' deprecated;
  SErrNoContentLength = 'No content length passed from server!' deprecated;
  SErrUnsupportedContentType = 'Unsupported content type: "%s"' deprecated;
  SErrNoRequestMethod = 'No REQUEST_METHOD passed from server.' deprecated;
  SErrInvalidRequestMethod = 'Invalid REQUEST_METHOD passed from server.' deprecated;

Implementation

uses
  iostream;
  
Type
  TFormFile = Class(TObject)
  Private
    FFileName : String;
  Public  
    Constructor Create(Const AFileName : String);
    Property FileName : String Read FFileName Write FFileName;
  end;  
  
Constructor  TFormFile.Create(Const AFileName : String);

begin
  FFileName:=AFileName;
end;

{$ifdef cgidebug}
Var
  flog : Text;

Procedure Log(Msg : String);

begin
  Writeln(flog,Msg);
end;

Procedure Log(Msg : String;Args : Array of const);

begin
  Writeln(flog,Format(Msg,Args));
end;

Procedure InitLog;

begin
  Assign(flog,'/tmp/cgi.log');
  Rewrite(flog);
  Log('---- Start of log session ---- ');
end;

Procedure DoneLog;

begin
  Close(Flog);
end;
{$endif}

Constructor TCgiApplication.Create(AOwner : TComponent);

begin
  Inherited Create(AOwner);
  FRequestVars:=TStringList.Create;
  FFormFiles:=TStringList.Create;
end;

Destructor TCgiApplication.Destroy;

Var
  i : Integer;

begin
  DeleteFormFiles;
  FFormFiles.Free;
  FRequestVars.Free;
  Inherited;
end;

Function  TCgiApplication.GetCGIVar (Index : Integer) : String;

begin
  Result:=FCGIVars[Index];
end;

Procedure TCgiApplication.InitCGIVars;

Var
  I : Integer;
  L : TStrings;

begin
  L:=TStringList.Create;
  Try
    GetEnvironmentList(L);
    For I:=1 to CGIVarCount do
      FCGIVars[i]:=L.Values[CGIVarNames[i]];
  Finally
    L.Free;
  end;
end;

Function TCgiApplication.GetTempCGIFileName : String;

begin
  Result:=GetTempFileName('/tmp/','CGI')
end;

Procedure TCgiApplication.DeleteFormFiles;

Var
  I,P : Integer;
  FN : String;
  FF : TFormFile;
begin
  For I:=0 to FFormFiles.Count-1 do
    begin
    FF:=TFormFile(FFormFiles.Objects[i]);
    If Assigned(FF) then
      begin
      If FileExists(FF.FileName) then
        DeleteFile(FF.FileName);
      FF.Free;
      end;  
    end;
end;

Procedure TCgiApplication.Initialize;

begin
  StopOnException:=True;
  Inherited;
  InitCGIVars;
  InitRequestVars;
  FResponse:=TIOStream.Create(iosOutput);
end;

Procedure TCgiApplication.GetCGIVarList(List : TStrings);

Var
  I : Integer;

begin
  List.Clear;
  For I:=1 to cgiVarCount do
    List.Add(CGIVarNames[i]+'='+FCGIVars[i]);
end;

Procedure TCgiApplication.GetRequestVarList(List : TStrings);

begin
  GetRequestVarList(List,False);
end;

Procedure TCgiApplication.GetRequestVarList(List : TStrings; NamesOnly : Boolean);

Var
  I,J : Integer;
  S : String;

begin
  List.BeginUpdate;
  Try
    List.Clear;
    // Copy one by one, there may be CR/LF in the variables, causing 'Text' to go wrong.
    If Assigned(FRequestVars) then
      For I:=0 to FRequestVars.Count-1 do
        begin
        S:=FRequestVars[i];
        If NamesOnly then
          begin
          J:=Pos('=',S);
          If (J>0) then
            S:=Copy(S,1,J-1);
          end;
        List.Add(S);
        end;
  finally
    List.EndUpdate;
  end;
end;


Function TCgiApplication.GetContentLength : Integer;

begin
  Result:=StrToIntDef(GetCGIVar(2),-1);
end;

Procedure TCgiApplication.SetContentLength (Value : Integer);

begin
  SetCGIVar(2,IntToStr(Value));
end;

Procedure TCgiApplication.SetCGIVar(Index : Integer; Value : String);

begin
  If Index in [1..cgiVarCount] then
    FCGIVars[Index]:=Value;
end;


Function TCgiApplication.GetServerPort : Word;
begin
  Result:=StrToIntDef(GetCGIVar(15),0);
end;

Function TCgiApplication.EmitContentType : Boolean;

Var
  S: String;

begin
  Result:=Not FContentTypeEmitted;
  If result then
    begin
    S:=ContentType;
    If (S='') then
      S:='text/html';
    AddResponseLn('Content-Type: '+ContentType);
    AddResponseLn('');
    FContentTypeEmitted:=True;
    end;
end;

Procedure TCgiApplication.ShowException(E: Exception);

Var
  TheEmail : String;
  FrameCount: integer;
  Frames: PPointer;
  FrameNumber:Integer;

begin
  If not FContentTypeEmitted then
    begin
    ContentType:='text/html';
    EmitContentType;
    end;
  If (ContentType='text/html') then
    begin
    AddResponseLN('<html><head><title>'+Title+': '+SCGIError+'</title></head>');
    AddResponseLN('<body>');
    AddResponseLN('<center><hr><h1>'+Title+': ERROR</h1><hr></center><br><br>');
    AddResponseLN(SAppEncounteredError+'<br>');
    AddResponseLN('<ul>');
    AddResponseLN('<li>'+SError+' <b>'+E.Message+'</b>');
    AddResponseLn('<li> Stack trace:<br>');
    AddResponseLn(BackTraceStrFunc(ExceptAddr)+'<br>');
    FrameCount:=ExceptFrameCount;
    Frames:=ExceptFrames;
    for FrameNumber := 0 to FrameCount-1 do
      AddResponseLn(BackTraceStrFunc(Frames[FrameNumber])+'<br>');
    AddResponseLn('</ul><hr>');
    TheEmail:=Email;
    If (TheEmail<>'') then
      AddResponseLN('<h5><p><i>'+SNotify+Administrator+': <a href="mailto:'+TheEmail+'">'+TheEmail+'</a></i></p></h5>');
    AddResponseLN('</body></html>');
    end;
end;

Function TCgiApplication.GetEmail : String;

Var
  H : String;

begin
  If (FEmail='') then
    begin
    H:=ServerName;
    If (H<>'') then
      Result:=Administrator+'@'+H
    else
      Result:='';
    end
  else
    Result:=Email;
end;

Function TCgiApplication.GetAdministrator : String;

begin
  If (FADministrator<>'') then
    Result:=FAdministrator
  else
    Result:=SWebMaster;
end;

Procedure TCgiApplication.InitRequestVars;

var
  R : String;

begin
  R:=RequestMethod;
  if (R='') then
    Raise ECGI.Create(SErrNoRequestMethod);
  if CompareText(R,'POST')=0 then
    InitPostVars
  else if CompareText(R,'GET')=0 then
    InitGetVars
  else
    Raise ECGI.CreateFmt(SErrInvalidRequestMethod,[R]);
end;

Procedure TCgiApplication.ProcessURLEncoded(M : TMemoryStream);


var
  FQueryString : String;

begin
  SetLength(FQueryString,M.Size); // Skip added Null.
  M.Read(FQueryString[1],M.Size);
  ProcessQueryString(FQueryString);
end;

Type
  TFormItem = Class(TObject)
    DisPosition : String;
    Name : String;
    isFile : Boolean;
    FileName : String;
    ContentType : String;
    DLen : Integer;
    Data : String;
    Procedure Process;
  end;

Procedure TFormItem.Process;

  Function GetLine(Var S : String) : String;

  Var
    P : Integer;

  begin
    P:=Pos(#13#10,S);
    If (P<>0) then
      begin
      Result:=Copy(S,1,P-1);
      Delete(S,1,P+1);
      end;
  end;

  Function GetWord(Var S : String) : String;

  Var
    I,len : Integer;
    Quoted : Boolean;
    C : Char;

  begin
    len:=length(S);
    quoted:=false;
    Result:='';
    for i:=1 to len do
      Begin
      c:=S[i];
      if (c='"') then
        Quoted:=Not Quoted
      else
        begin
        if not (c in [' ','=',';',':']) or Quoted then
          Result:=Result+C;
        if (c in [';',':','=']) and (not quoted) then
          begin
          Delete(S,1,I);
          Exit;
          end;
        end;
      end;
     S:='';
  end;

Var
  Line : String;
  Words : TStringList;
  i,len : integer;
  c : char;
  S : string;
  quoted : boolean;

begin
  Line:=GetLine(Data);
  While (Line<>'') do
    begin
    S:=GetWord(Line);
    While (S<>'') do
      begin
      If CompareText(S,'Content-Disposition')=0 then
        Disposition:=GetWord(Line)
      else if CompareText(S,'name')=0 Then
        Name:=GetWord(Line)
      else if CompareText(S,'filename')=0 then
        begin
        FileName:=GetWord(Line);
        isFile:=True;
        end
      else if CompareText(S,'Content-Type')=0 then
        ContentType:=GetWord(Line);
      S:=GetWord(Line);
      end;
    Line:=GetLine(Data);
    end;
  // Now Data contains the rest of the data, plus a CR/LF. Strip the CR/LF
  Len:=Length(Data);
  If (len>2) then
    Data:=Copy(Data,1,Len-2);
end;

Function MakeString(PStart,PEnd : Pchar) : String;

begin
  SetLength(Result,PEnd-PStart);
  If Length(Result)>0 then
    Move(PStart^,Result[1],Length(Result));
end;

procedure FormSplit(var Cnt : String; boundary: String; List : TList);

// Splits the form into items
var
  Sep : string;
  Clen,slen, p:longint;
  FI : TFormItem;

begin
  Sep:='--'+boundary+#13+#10;
  Slen:=length(Sep);
  CLen:=Pos('--'+Boundary+'--',Cnt);
  // Cut last marker
  Cnt:=Copy(Cnt,1,Clen-1);
  // Cut first marker
  Delete(Cnt,1,Slen);
  Clen:=Length(Cnt);
  While Clen>0 do
    begin
    Fi:=TFormItem.Create;
    List.Add(Fi);
    P:=pos(Sep,Cnt);
    If (P=0) then
      P:=CLen+1;
    FI.Data:=Copy(Cnt,1,P-1);
    delete(Cnt,1,P+SLen-1);
    CLen:=Length(Cnt);
    end;
end;

function GetNextLine(Var Data: String):string;

Var
  p : Integer;

begin
  P:=Pos(#13#10,Data);
  If (P<>0) then
    begin
    Result:=Copy(Data,1,P-1);
    Delete(Data,1,P+1);
    end;
end;

Procedure TCgiApplication.ProcessMultiPart(M : TMemoryStream; Const Boundary : String);

Var
  L : TList;
  B : String;
  I,Index : Integer;
  S,FF,key, Value : String;
  FI : TFormItem;
  F : TStream;

begin
  i:=Pos('=',Boundary);
  B:=Copy(Boundary,I+1,Length(Boundary)-I);
  I:=Length(B);
  If (I>0) and (B[1]='"') then
    B:=Copy(B,2,I-2);
  L:=TList.Create;
  Try
    SetLength(S,M.Size);
    If Length(S)>0 then
      Move(M.Memory^,S[1],M.Size);
    FormSplit(S,B,L);
    For I:=L.Count-1 downto 0 do
      begin
      FI:=TFormItem(L[i]);
      FI.Process;
      If (FI.Name='') then
        Raise ECGI.CreateFmt('Invalid multipart encoding: %s',[FI.Data]);
      Key:=FI.Name;
      If Not FI.IsFile Then
        begin
        Value:=FI.Data
        end
      else
        begin
        Value:=FI.FileName;
        FF:=GetTempCGIFileName;
        FFormFiles.AddObject(Key,TFormFile.Create(FF));
        F:=TFileStream.Create(FF,fmCreate);
        Try
          if Length(FI.Data)>0 then
            F.Write(FI.Data[1],Length(FI.Data));
        finally
          F.Free;
        end;
        FI.Free;
        L[i]:=Nil;
        end;
      FRequestVars.Add(Key+'='+Value)
      end;
  Finally
    For I:=0 to L.Count-1 do
      TObject(L[i]).Free;
    L.Free;
  end;
end;

Type
  TCapacityStream = Class(TMemoryStream)
  Public
    Property Capacity;
  end;

Procedure TCgiApplication.InitPostVars;

Var
  M  : TCapacityStream;
  I  : TIOStream;
  Cl : Integer;
  B  : Byte;

begin
  CL:=ContentLength;
  M:=TCapacityStream.Create;
  Try
    I:=TIOStream.Create(iosInput);
    Try
      if (CL<>0) then
        begin
        M.Capacity:=(Cl);
        M.CopyFrom(I,Cl);
        end
      else
        begin
        B:=0;
        While (I.Read(B,1)>0) do
          M.Write(B,1)
        end;
    Finally
      I.Free;
    end;
    if CompareText(ContentType,'MULTIPART/FORM-DATA')=0 then
      ProcessMultiPart(M,ContentType)
    else if CompareText(ContentType,'APPLICATION/X-WWW-FORM-URLENCODED')=0 then
      ProcessUrlEncoded(M)
    else
      Raise ECGI.CreateFmt(SErrUnsupportedContentType,[ContentType]);
  finally
    M.Free;
  end;
end;

Procedure TCgiApplication.InitGetVars;

Var
  FQueryString : String;

begin
  FQueryString:=QueryString;
  If (FQueryString<>'') then
    ProcessQueryString(FQueryString);
end;

const
   hexTable = '0123456789ABCDEF';

Procedure TCgiApplication.ProcessQueryString(Const FQueryString : String);


var
  queryItem : String;
  delimiter : Char;
  aString : String;
  aSepStr : String;
  aPos    : Integer;
  aLenStr : Integer;
  aLenSep : Integer;

  function hexConverter(h1, h2 : Char) : Char;

  var
    B : Byte;

  begin
    B:=(Pos(upcase(h1),hexTable)-1)*16;
    B:=B+Pos(upcase(h2),hexTable)-1;
    Result:=chr(B);
  end;

  procedure Convert_ESC_Chars;

  var
    index : Integer;

  begin
    Index:=Length(QueryItem);
    While (Index>0) do
      begin
      If QueryItem[Index]='+' then
        QueryItem[Index]:=' '
      else If (QueryItem[Index]='%') and (Index<Length(QueryItem)-1) then
        begin
        QueryItem[Index]:=hexConverter(QueryItem[Index+1],QueryItem[index+2]);
        System.Delete(QueryItem,Index+1,2);
        end;
      dec(Index);
      end;
  end;

  procedure InitToken(aStr, aSep : String);

  begin
    aString := aStr;
    aSepStr := aSep;
    aPos    := 1;
    aLenStr := Length(aString);
    aLenSep := Length(aSepStr);
  end;

  function NextToken(var aToken : String; out aSepChar : Char) : Boolean;

  var
    i : Integer;
    j : Integer;
    BoT : Integer;
    EoT : Integer;
    isSep : Boolean;

  begin
    BoT:=aPos;
    EoT:=aPos;
    for i:=aPos to aLenStr do
      begin
      IsSep := false;
      for j := 1 to aLenSep do
        begin
        if aString[i] = aSepStr[j] then
          begin
          IsSep := true;
          Break;
          end;
        end;
      if IsSep then
        begin
        EoT  := i;
        aPos := i + 1;
        aSepChar := aString[i];
        Break;
        end
      else
        begin
        if i = aLenStr then
          begin
          EoT  := i;
          aPos := i;
          Break;
          end;
        end;
      end;
    if aPos < aLenStr then
      begin
      aToken := Copy(aString, BoT, EoT - BoT);
      Result := true;
      end
    else
      begin
      if aPos = aLenStr then
        begin
        aToken := Copy(aString, BoT, EoT - BoT + 1);
        Result := true;
        aPos   := aPos + 1;
        end
      else
        begin
        Result := false;
       end;
    end;
  end;


begin
   InitToken(FQueryString, '&');
   while NextToken(QueryItem, delimiter) do
     begin
     if (QueryItem<>'') then
       begin
       Convert_ESC_Chars;
       FRequestVars.Add(QueryItem);
       end;
     end;
end;

Function TCGIApplication.GetRequestVariable(Const VarName : String) : String;

begin
 If Assigned(FRequestVars) then
   Result:=FRequestVars.Values[VarName];
end;

Function TCGIApplication.GetRequestVariableCount : Integer;

begin
  If Assigned(FRequestVars) then
    Result:=FRequestVars.Count
  else
    Result:=0;
end;

Procedure TCGIApplication.AddResponse(Const S : String);

Var
  L : Integer;

begin
  L:=Length(S);
  If L>0 then
    FResponse.Write(S[1],L);
end;

Procedure TCGIApplication.AddResponse(Const Fmt : String; Args : Array of const);

begin
  AddResponse(Format(Fmt,Args));
end;

Procedure TCGIApplication.AddResponseLN(Const S : String);


begin
  AddResponse(S+LineEnding);
end;

Procedure TCGIApplication.AddResponseLN(Const Fmt : String; Args : Array of const);

begin
  AddResponseLN(Format(Fmt,Args));
end;

Function TCGIApplication.VariableIsUploadedFile(Const VarName : String) : boolean;

begin
  Result:=FFormFiles.IndexOf(VarName)<>-1;
end;

Function TCGIApplication.UploadedFileName(Const VarName : String) : String;

Var
  FF : TFormFile;
  i : Integer;
 
begin
  Result:='';
  I:=FFormFiles.IndexOf(VarName);
  If (I<>-1) then
    begin
    FF:=TFormFile(FFormFiles.Objects[i]);
    If Assigned(FF) then
      Result:=FF.FileName;
    end;  
end;

{$ifdef cgidebug}
Initialization
  initLog;

Finalization
  DoneLog;
{$endif}
end.
