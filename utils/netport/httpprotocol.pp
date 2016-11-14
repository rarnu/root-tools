unit httpprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  // HTTP 1.1 defined headers.
  THeader = (hhUnknown,
     hhAccept,hhAcceptCharset,hhAcceptEncoding, hhAcceptLanguage,
     hhAcceptRanges, hhAge, hhAllow, hhAuthorization, hhCacheControl,
     hhConnection, hhContentEncoding, hhContentLanguage,
     hhContentLength,hhContentLocation, hhContentMD5, hhContentRange,
     hhContentType, hhDate, hhETag, hhExpires, hhExpect,
     hhFrom, hhHost, hhIfMatch, hhIfModifiedSince, hhIfNoneMatch,
     hhIfRange, hhIfUnModifiedSince, hhLastModified, hhLocation, hhMaxForwards,
     hhPragma, hhProxyAuthenticate, hhProxyAuthorization, hhRange, hhReferer,
     hhRetryAfter, hhServer, hhTE, hhTrailer,
     hhTransferEncoding, hhUpgrade , hhUserAgent, hhVary,
     hhVia, hhWarning, hhWWWAuthenticate);
  THeaders = Set of THeader;
  THeaderDirection = (hdRequest,hdResponse);
  THeaderDirections = Set of THeaderDirection;

  THeadersArray = Array[THeader] of string;

Const
  HeaderAccept          = 'Accept';
  HeaderAcceptCharset   = 'Accept-Charset';
  HeaderAcceptEncoding  = 'Accept-Encoding';
  HeaderAcceptLanguage  = 'Accept-Language';
  HeaderAcceptRanges    = 'Accept-Ranges';
  HeaderAge             = 'Age';
  HeaderAllow           = 'Allow';
  HeaderAuthorization   = 'Authorization';
  HeaderCacheControl    = 'Cache-Control';
  HeaderConnection      = 'Connection';
  HeaderContentEncoding = 'Content-Encoding';
  HeaderContentLanguage = 'Content-Language';
  HeaderContentLength   = 'Content-Length';
  HeaderContentLocation = 'Content-Location';
  HeaderContentMD5      = 'Content-MD5';
  HeaderContentRange    = 'Content-Range';
  HeaderContentType     = 'Content-Type';
  HeaderDate            = 'Date';
  HeaderETag            = 'ETag';
  HeaderExpires         = 'Expires';
  HeaderExpect          = 'Expect';
  HeaderFrom            = 'From';
  HeaderHost            = 'Host';
  HeaderIfMatch         = 'If-Match';
  HeaderIfModifiedSince = 'If-Modified-Since';
  HeaderIfNoneMatch     = 'If-None-Match';
  HeaderIfRange         = 'If-Range';
  HeaderIfUnModifiedSince = 'If-Unmodified-Since';
  HeaderLastModified    = 'Last-Modified';
  HeaderLocation        = 'Location';
  HeaderMaxForwards     = 'Max-Forwards';
  HeaderPragma          = 'Pragma';
  HeaderProxyAuthenticate = 'Proxy-Authenticate';
  HeaderProxyAuthorization = 'Proxy-Authorization';
  HeaderRange           = 'Range';
  HeaderReferer         = 'Referer';
  HeaderRetryAfter      = 'Retry-After';
  HeaderServer          = 'Server';
  HeaderTE              = 'TE';
  HeaderTrailer         = 'Trailer';
  HeaderTransferEncoding = 'Transfer-Encoding';
  HeaderUpgrade         = 'Upgrade';
  HeaderUserAgent       = 'User-Agent';
  HeaderVary            = 'Vary';
  HeaderVia             = 'Via';
  HeaderWarning         = 'Warning';
  HeaderWWWAuthenticate = 'WWW-Authenticate';

  // These Headers are NOT in the HTTP 1.1 definition.
  HeaderXRequestedWith  = 'X-Requested-With';
  HeaderCookie          = 'Cookie';
  HeaderSetCookie       = 'Set-Cookie';

  HTTPDateFmt     = '"%s", dd "%s" yyyy hh:mm:ss'; // For use in FormatDateTime
  SCookieExpire   = ' "Expires="'+HTTPDateFmt+' "GMT"';
  SCookieDomain   = ' Domain=%s';
  SCookiePath     = ' Path=%s';
  SCookieSecure   = ' Secure';
  SCookieHttpOnly = ' HttpOnly';

  HTTPMonths: array[1..12] of string[3] = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  HTTPDays: array[1..7] of string[3] = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');


Const
  HTTPHeaderDirections : Array[THeader] of THeaderDirections = (
   [],
   [hdRequest],[hdRequest],[hdRequest], [hdRequest],
   [hdResponse], [hdResponse], [hdResponse], [hdRequest], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdRequest,hdResponse],
   [hdRequest,hdResponse],[hdRequest,hdResponse], [hdRequest,hdResponse], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdResponse], [hdRequest,hdResponse], [hdRequest],
   [hdRequest], [hdRequest], [hdRequest], [hdRequest], [hdRequest],
   [hdRequest], [hdRequest], [hdRequest,hdResponse], [hdResponse], [hdRequest],
   [hdRequest, hdResponse] , [hdResponse], [hdRequest], [hdRequest,hdResponse], [hdRequest],
   [hdResponse], [hdResponse], [hdRequest], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdRequest], [hdRequest,hdResponse],
   [hdRequest,hdResponse], [hdRequest,hdResponse], [hdResponse]);

  HTTPHeaderNames : THeadersArray
                 = ('',
                    HeaderAccept,HeaderAcceptCharset,HeaderAcceptEncoding, HeaderAcceptLanguage,
                    HeaderAcceptRanges, HeaderAge, HeaderAllow, HeaderAuthorization, HeaderCacheControl,
                    HeaderConnection, HeaderContentEncoding, HeaderContentLanguage,
                    HeaderContentLength,HeaderContentLocation, HeaderContentMD5, HeaderContentRange,
                    HeaderContentType, HeaderDate, HeaderETag, HeaderExpires, HeaderExpect,
                    HeaderFrom, HeaderHost, HeaderIfMatch, HeaderIfModifiedSince, HeaderIfNoneMatch,
                    HeaderIfRange, HeaderIfModifiedSince, HeaderLastModified, HeaderLocation, HeaderMaxForwards ,
                    HeaderPragma, HeaderProxyAuthenticate, HeaderProxyAuthorization, HeaderRange, HeaderReferer,
                    HeaderRetryAfter, HeaderServer, HeaderTE, HeaderTrailer,
                    HeaderTransferEncoding, HeaderUpgrade , HeaderUserAgent, HeaderVary,
                    HeaderVia, HeaderWarning, HeaderWWWAuthenticate);

Function HeaderName(AHeader : THeader) : String;
Function HeaderType(AHeader : String) : THeader;
Function HTTPDecode(const AStr: String): String;
Function HTTPEncode(const AStr: String): String;
Function IncludeHTTPPathDelimiter(const AStr: String): String;
Function ExcludeHTTPPathDelimiter(const AStr: String): String;

implementation

function HeaderName(AHeader: THeader): String;

begin
  Result:=HTTPHeaderNames[AHeader];
end;

function HeaderType(AHeader: String): THeader;

begin
  Result:=High(THeader);
  While (Result>hhUnknown) and (CompareText(HTTPHeaderNames[Result],AHeader)<>0) do
    Result:=Pred(Result);
end;

function HTTPDecode(const AStr: String): String;

var
  S,SS, R : PChar;
  H : String[3];
  L,C : Integer;

begin
  L:=Length(Astr);
  SetLength(Result,L);
  If (L=0) then
    exit;
  S:=PChar(AStr);
  SS:=S;
  R:=PChar(Result);
  while (S-SS)<L do
    begin
    case S^ of
      '+': R^ := ' ';
      '%': begin
           Inc(S);
           if ((S-SS)<L) then
             begin
             if (S^='%') then
               R^:='%'
             else
               begin
               H:='$00';
               H[2]:=S^;
               Inc(S);
               If (S-SS)<L then
                 begin
                 H[3]:=S^;
                 Val(H,PByte(R)^,C);
                 If (C<>0) then
                   R^:=' ';
                 end;
               end;
             end;
           end;
      else
        R^ := S^;
      end;
    Inc(R);
    Inc(S);
    end;
  SetLength(Result,R-PChar(Result));
end;

function HTTPEncode(const AStr: String): String;

const
  HTTPAllowed = ['A'..'Z','a'..'z',
                 '*','@','.','_','-',
                 '0'..'9',
                 '$','!','''','(',')'];

var
  SS,S,R: PChar;
  H : String[2];
  L : Integer;

begin
  L:=Length(AStr);
  SetLength(Result,L*3); // Worst case scenario
  if (L=0) then
    exit;
  R:=PChar(Result);
  S:=PChar(AStr);
  SS:=S; // Avoid #0 limit !!
  while ((S-SS)<L) do
    begin
    if S^ in HTTPAllowed then
      R^:=S^
    else if (S^=' ') then
      R^:='+'
    else
      begin
      R^:='%';
      H:=HexStr(Ord(S^),2);
      Inc(R);
      R^:=H[1];
      Inc(R);
      R^:=H[2];
      end;
    Inc(R);
    Inc(S);
    end;
  SetLength(Result,R-PChar(Result));
end;

function IncludeHTTPPathDelimiter(const AStr: String): String;

Var
  l : Integer;

begin
  Result:=AStr;
  L:=Length(Result);
  If (L>0) and (Result[L]<>'/') then
    Result:=Result+'/';
end;

function ExcludeHTTPPathDelimiter(const AStr: String): String;

Var
  l : Integer;

begin
  L:=Length(AStr);
  If (L>0) and (AStr[L]='/') then
    Result:=Copy(AStr,1,L-1)
  else
    Result:=AStr;
end;

end.

