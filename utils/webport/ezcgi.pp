{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ezcgi;

{$mode delphi}
{$H+ }

interface

uses classes, sysutils;

const
   hexTable = '0123456789ABCDEF';

type
   ECGIException = class(Exception);

   TEZcgi = class(TObject)
   private
      { Private declarations }
      FVariables : TStringList;
      FName : String;
      FEmail : String;
      FQueryString : String;

      { Token variables }
      aString : String;
      aSepStr : String;
      aPos    : Integer;
      aLenStr : Integer;
      aLenSep : Integer;

      procedure InitToken(aStr, aSep : String);
      function NextToken(var aToken : String; out aSepChar : Char) : Boolean;

      procedure GetQueryItems;
      procedure ProcessRequest;
      procedure LoadEnvVariables;
      function GetVal(Index : String) : String;
      function GetName(Index : Integer) : String;
      function GetVariable(Index : Integer) : String;
      function GetVarCount : Integer;
      procedure ReadPostQuery;
      procedure ReadGetQuery;
   protected
      { Protected declarations }

      procedure OutputError(errorMessage : String);
   public
      { Public declarations }
      constructor Create;
      destructor Destroy; override;
      procedure Run;
      procedure WriteContent(ctype : String);
      procedure PutLine(sOut : String);
      function GetValue(Index : String; defaultValue : String) : String;

      procedure DoPost; virtual;
      procedure DoGet; virtual;

      property Values[Index : String] : String read GetVal;
      property Names[Index : Integer] : String read GetName;
      property Variables[Index : Integer] : String read GetVariable;
      property VariableCount : Integer read GetVarCount;

      property Name : String read FName write FName;
      property Email : String read FEmail write FEmail;
   end;

implementation

{ *********** Public Methods *************** }

constructor TEZcgi.Create;
begin
   FName := 'No name available';
   FEmail := 'Email address unavailable';

   FVariables := TStringList.Create;

   LoadEnvVariables;

end;

destructor TEZcgi.Destroy;
begin
   FVariables.Free;
end;

procedure TEZcgi.Run;
begin
   ProcessRequest;
end;

procedure TEZcgi.DoPost;
begin
  // Must be overridden by child class
end;

procedure TEZcgi.DoGet;
begin
  // Must be overridden by child class
end;

procedure TEZcgi.WriteContent(ctype : String);
begin
   writeln('Content-Type: ',ctype);
   writeln;
end;

procedure TEZcgi.PutLine(sOut : String);
begin
   writeln(sOut);
end;

function TEZcgi.GetValue(Index, defaultValue : String) : String;
begin
   result := GetVal(Index);
   if result = '' then
      result := defaultValue;
end;


{ *********** Private Methods *************** }

procedure TEZcgi.LoadEnvVariables;

   procedure GetEData(variable : String);
   var
      tempStr : String;
   begin
      // This is a system dependent call !!
      tempStr := GetEnvironmentVariable(variable);
      if tempStr <> '' then
         FVariables.Add(variable + '=' + tempStr);
   end;

begin

   { Standard CGI Environment Variables }
   GetEData('AUTH_TYPE');
   GetEData('CONTENT_LENGTH');
   GetEData('CONTENT_TYPE');
   GetEData('GATEWAY_INTERFACE');
   GetEData('PATH_INFO');
   GetEData('PATH_TRANSLATED');
   GetEData('QUERY_STRING');
   GetEData('REMOTE_ADDR');
   GetEData('REMOTE_HOST');
   GetEData('REMOTE_IDENT');
   GetEData('REMOTE_USER');
   GetEData('REQUEST_METHOD');
   GetEData('SCRIPT_NAME');
   GetEData('SERVER_NAME');
   GetEData('SERVER_PORT');
   GetEData('SERVER_PROTOCOL');
   GetEData('SERVER_SOFTWARE');


   { Standard HTTP Environment Variables }
   GetEData('HTTP_ACCEPT');
   GetEData('HTTP_ACCEPT_CHARSET');
   GetEData('HTTP_ACCEPT_ENCODING');
   GetEData('HTTP_IF_MODIFIED_SINCE');
   GetEData('HTTP_REFERER');
   GetEData('HTTP_USER_AGENT');
end;

procedure TEZcgi.ProcessRequest;
var
   request : String;
begin

   request := GetVal('REQUEST_METHOD');

   if request = '' then
      OutputError('No REQUEST_METHOD passed from server!')
   else if request = 'POST' then
   begin
      ReadPostQuery;
      DoPost;
   end
   else if request = 'GET' then
      begin
         ReadGetQuery;
         DoGet;
      end
   else
      OutputError('Invalid REQUEST_METHOD passed from server!');
end;

function TEZcgi.GetVal(Index : String) : String;
begin
   result := FVariables.Values[Index];
end;

function TEZcgi.GetName(Index : Integer) : String;
begin
   result := FVariables.Names[Index];
end;

function TEZcgi.GetVariable(Index : Integer) : String;
begin
   result := FVariables[Index];
end;

function TEZcgi.GetVarCount : Integer;
begin
   result := FVariables.Count;
end;

procedure TEZcgi.ReadPostQuery;
var
   index : Integer;
   ch : Char;
   temp : String;
   code : Word;
   contentLength : Integer;
   theType : String;

begin

   temp := GetVal('CONTENT_LENGTH');
   if Length(temp) > 0 then
   begin
      Val(temp, contentLength, code);
      if code <> 0 then
         contentLength := 0;
   end;

   if contentLength = 0 then
      OutputError('No content length passed from server!');

   theType := UpperCase(GetVal('CONTENT_TYPE'));

   if theType <> 'APPLICATION/X-WWW-FORM-URLENCODED' then
      OutputError('No content type passed from server!');

   FQueryString := '';

   for index := 0 to contentLength-1 do
   begin
      Read(ch);
      FQueryString := FQueryString + ch;
   end;

   GetQueryItems;
end;

procedure TEZcgi.ReadGetQuery;
begin
   FQueryString := GetVal('QUERY_STRING');

   if FQueryString = '' then
      OutputError('No QUERY_STRING passed from server!');

   GetQueryItems;
end;

procedure TEZcgi.GetQueryItems;
var
   queryItem : String;
   delimiter : Char;

   function hexConverter(h1, h2 : Char) : Char;
   var
      thex : byte;
   begin
      tHex := (Pos(upcase(h1), hexTable) - 1) * 16;
      tHex := tHex + Pos(upcase(h2), hexTable) - 1;

      result := chr(thex);
   end;

   procedure Convert_ESC_Chars;
   var
      index : Integer;
   begin
      repeat
         index := Pos('+', queryItem);
         if index > 0 then
            queryItem[index] := Chr(32);
      until index = 0;
      repeat
         index := Pos('%', queryItem);
         if index > 0 then
         begin
            queryItem[index] := hexConverter(queryItem[index + 1], queryItem[index + 2]);
            system.Delete(queryItem, index + 1, 2);
         end;
      until index = 0;
   end;

begin
   InitToken(FQueryString, '&');

   while NextToken(queryItem, delimiter) do
   begin
      if queryItem <> '' then
      begin
         Convert_ESC_Chars;
         FVariables.Add(queryItem);
      end;
   end;
end;

procedure TEZcgi.OutputError(errorMessage : String);
begin
   WriteContent('text/html');
   writeln('<html><head><title>CGI ERROR</title></head>');
   writeln('<body>');
   writeln('<center><hr><h1>CGI ERROR</h1><hr></center><br><br>');
   writeln('This CGI application encountered the following error: <br>');
   writeln('<ul><br>');
   writeln('<li> error: ',errorMessage,'<br><hr>');
   writeln('<h5><p><i>Notify ',FName,' <a href="mailto:',FEmail,'">',FEmail,'</a></i></p></h5>');
   writeln('</body></html>');

   Raise ECGIException.Create(errorMessage);
end;

procedure TEZcgi.InitToken(aStr, aSep : String);
begin
     aString := aStr;
     aSepStr := aSep;
     aPos    := 1;
     aLenStr := Length(aString);
     aLenSep := Length(aSepStr);
end;

function TEZcgi.NextToken(var aToken : String; out aSepChar : Char) : Boolean;
var
   i : Integer;
   j : Integer;
   BoT : Integer;
   EoT : Integer;
   isSep : Boolean;

begin
   BoT := aPos;
   EoT := aPos;
   for i := aPos to aLenStr do
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

end.
