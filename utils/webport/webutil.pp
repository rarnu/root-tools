{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit webutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpprotocol, httpdefs;

procedure DumpRequest (ARequest : TRequest; Dump : TStrings; Environment : Boolean = False);

implementation



procedure DumpRequest (ARequest : TRequest; Dump : TStrings; Environment : Boolean = False);

  Procedure AddNV(Const N,V : String);
  
  begin
    Dump.Add('<TR><TD>'+N+'</TD><TD>'+V+'</TD></TR>');
  end;

Var
  I,J   : integer;
  N,V : String;
  H : THeader;
  VA : THTTPVariableType;

begin
  With ARequest, Dump do
    begin
    // All possible headers
    Add('<H1>HTTP 1.1 request headers:</H1>');
    Add('<TABLE BORDER="1"><TR><TD>Header</TD><TD>Value</TD></TR>');
    For H in THeader do
      if (hdRequest in HTTPHeaderDirections[H]) then
        AddNV(HTTPHeaderNames[H],GetHeader(H));
    Add('</TABLE><P>');
    // Actually sent headers
    Add('<H1>Actually sent request headers:</H1>');
    Add('<TABLE BORDER="1"><TR><TD>Header</TD><TD>Value</TD></TR>');
    For H in THeader do
      if (hdRequest in HTTPHeaderDirections[H]) and HeaderIsSet(H) then
        AddNV(HTTPHeaderNames[H],GetHeader(H));
    For Va in HeaderBasedVariables do
      begin
      V:=GetHTTPVariable(Va);
      if V<>'' then
        AddNV(THTTPHeader.GetVariableHeaderName(Va),V);
      end;
    For I:=0 to CustomHeaders.Count-1 do
      begin
      CustomHeaders.GetNameValue(I,N,V);
      AddNV(N,V);
      end;
    Add('</TABLE><P>');

    // Actually sent headers, as text
    Add('<H1>Actually sent request headers as text:</H1>');
    Add('<pre>');
    For H in THeader do
      if (hdRequest in HTTPHeaderDirections[H]) and HeaderIsSet(H) then
        Add(HTTPHeaderNames[H]+': '+GetHeader(H));
     For Va in HeaderBasedVariables do
       begin
        V:=GetHTTPVariable(Va);
        if V<>'' then
          Add(THTTPHeader.GetVariableHeaderName(Va)+': '+V);
       end;
     For I:=0 to CustomHeaders.Count-1 do
       begin
       CustomHeaders.GetNameValue(I,N,V);
         Add(N+': '+V);
       end;
    Add('</PRE>');
    // Additional headers
    Add('<H1>Additional protocol variables:</H1>');
    Add('<TABLE BORDER="1"><TR><TD>Header</TD><TD>Value</TD></TR>');
    AddNV('PathInfo',PathInfo);
    AddNV('PathTranslated',PathTranslated);
    AddNV('RemoteAddress',RemoteAddress);
    AddNV('RemoteHost',RemoteHost);
    AddNV('ScriptName',ScriptName);
    AddNV('ServerPort',IntToStr(ServerPort));
    AddNV('Method',Method);
    AddNV('URL',URL);
    AddNV('Query',Query);
    AddNV('Host',Host);
    AddNV('Content',Content);
    Add('</TABLE><P>');
    // Additional headers
    If (QueryFields.Count>0) then
      begin
      Add('<H1>Request variables: ('+IntToStr(QueryFields.Count)+') </H1>');
      Add('<TABLE BORDER="1"><TR><TD>Name</TD><TD>Value</TD></TR>');
      For I:=0 to QueryFields.Count-1 do
        begin
        QueryFields.GetNameValue(i,N,V);
        AddNV(N,V);
        end;
      Add('</TABLE><P>');
      end;
    If (ContentFields.Count>0) then
      begin
      Add('<H1>Form post variables: ('+IntToStr(ContentFields.Count)+') </H1>');
      Add('<TABLE BORDER="1"><TR><TD>Name</TD><TD>Value</TD></TR>');
      For I:=0 to ContentFields.Count-1 do
        begin
        ContentFields.GetNameValue(i,N,V);
        AddNV(N,V);
        end;
      Add('</TABLE><P>');
      end;
    If Environment then
      begin
      Add('<H1>Environment variables: ('+IntToStr(GetEnvironmentVariableCount)+') </H1>');
      Add('<TABLE BORDER="1"><TR><TD>Name</TD><TD>Value</TD></TR>');
      For I:=1 to GetEnvironmentVariableCount do
        begin
        V:=GetEnvironmentString(i);
        j:=Pos('=',V);
        If (J>0) then
          begin
          N:=Copy(V,1,J-1);
          system.Delete(V,1,J);
          AddNV(N,V);
          end;
        end;
      Add('</TABLE><P>');
      end;
    If (Files.Count>0) then
      begin
      Add('<H1>Uploaded files: ('+IntToStr(Files.Count)+') </H1>');
      Add('<TABLE BORDER="1">');
      Add('<TR><TD>Name</TD><TD>FileName</TD><TD>Size</TD>');
      Add('<TD>Temp FileName</TD><TD>Disposition</TD><TD>Content-Type</TD><TD>Description</TD></TR>');
      For I:=0 to Files.Count-1 do
        With Files[i] do
          begin
          Add('<TR><TD>'+FieldName+'</TD><TD>'+FileName+'</TD>');
          Add('<TD>'+IntToStr(Size)+'</TD><TD>'+LocalFileName+'</TD>');
          Add('<TD>'+Disposition+'</TD><TD>'+ContentType+'</TD><TD>'+Description+'</TD></TR>');
          end;
      Add('</TABLE><P>');
      end;
    If (CookieFields.Count>0) then
      begin
      Add('<H1>Received cookies: ('+IntToStr(CookieFields.Count)+') </H1>');
      Add('<TABLE BORDER="1">');
      Add('<TR><TD>Name</TD><TD>Value</TD></TR>');
      For I:=0 to CookieFields.Count-1 do
        begin
        CookieFields.GetNameValue(i,N,V);
        AddNV(N,V);
        end;
      Add('</TABLE><P>');
      end;
    end;
end;

end.

