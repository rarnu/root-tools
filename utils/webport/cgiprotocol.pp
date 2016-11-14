unit cgiprotocol;

{$mode objfpc}{$H+}

interface

Const
  CGIVarCount = 45 ;

Type
  TCGIVarArray = Array[1..CGIVarCount] of String;

Const
  CgiVarNames : TCGIVarArray =
   ({ 1  } 'AUTH_TYPE',
    { 2  } 'CONTENT_LENGTH',
    { 3  } 'CONTENT_TYPE',
    { 4  } 'GATEWAY_INTERFACE',
    { 5  } 'PATH_INFO',
    { 6  } 'PATH_TRANSLATED',
    { 7  } 'QUERY_STRING',
    { 8  } 'REMOTE_ADDR',
    { 9  } 'REMOTE_HOST',
    { 10 } 'REMOTE_IDENT',
    { 11 } 'REMOTE_USER',
    { 12 } 'REQUEST_METHOD',
    { 13 } 'SCRIPT_NAME',
    { 14 } 'SERVER_NAME',
    { 15 } 'SERVER_PORT',
    { 16 } 'SERVER_PROTOCOL',
    { 17 } 'SERVER_SOFTWARE',
    { 18 } 'HTTP_ACCEPT',
    { 19 } 'HTTP_ACCEPT_CHARSET',
    { 20 } 'HTTP_ACCEPT_ENCODING',
    { 21 } 'HTTP_IF_MODIFIED_SINCE',
    { 22 } 'HTTP_REFERER',
    { 23 } 'HTTP_USER_AGENT',
    { 24 } 'HTTP_COOKIE',
    { 25 } 'HTTP_IF_NONE_MATCH',

     // Additional Apache vars
    { 26 } 'HTTP_CONNECTION',
    { 27 } 'HTTP_ACCEPT_LANGUAGE',
    { 28 } 'HTTP_HOST',
    { 29 } 'SERVER_SIGNATURE',
    { 30 } 'SERVER_ADDR',
    { 31 } 'DOCUMENT_ROOT',
    { 32 } 'SERVER_ADMIN',
    { 33 } 'SCRIPT_FILENAME',
    { 34 } 'REMOTE_PORT',
    { 35 } 'REQUEST_URI',
    { 36 } 'CONTENT',
    { 37 } 'HTTP_X_REQUESTED_WITH',
    { 38 } 'HTTP_AUTHORIZATION',
    { 39 } 'SCRIPT_URI',
    { 40 } 'SCRIPT_URL',
    { 41 } 'CONTEXT_DOCUMENT_ROOT',
    { 42 } 'CONTEXT_PREFIX',
    { 43 } 'HTTP_CACHE_CONTROL',
    { 44 } 'HTTP_PRAGMA',
    { 45 } 'REQUEST_SCHEME'
    );

Function IndexOfCGIVar(AVarName: String): Integer;

implementation

uses sysutils;

Function IndexOfCGIVar(AVarName: String): Integer;

begin
  Result:=CGIVarCount;
  While (Result>0) and (CompareText(AVarName,CgiVarNames[Result])<>0) do
    Dec(Result);
  If Result<1 then
    Result:=-1;
end;

end.

