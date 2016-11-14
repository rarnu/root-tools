{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    SSL support for ssockets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sslsockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, ssockets, openssl, fpopenssl;

Const
  SSLDataCount = 4; // 0 based.

Type
  TVerifyCertificateEvent = Procedure(Sender : TObject; Allow : Boolean) of object;
  { TSSLSocketHandler }

  TSSLSocketHandler = class(TSocketHandler)
  private
    FRemoteHostName: String;
    FSSLLastErrorString: string;
    FCipherList: string;
    FVerifyPeerCert: Boolean;
    FOnVerifyCertificate: TVerifyCertificateEvent;
    FSSLType: TSSLType;
    FKeyPassword: string;
    FUsername: string;
    FPassword: string;
    FCertData : Array[0..4] of TSSLData;
    FSSL: TSSL;
    FCTX : TSSLContext;
    FSSLActive : Boolean;
    FSendHostAsSNI : Boolean;
    function GetSSLData(AIndex: Integer): TSSLData;
    procedure SetSSLData(AIndex: Integer; AValue: TSSLData);
    procedure SetSSLLastErrorString(AValue: string);
  protected
    Function FetchErrorInfo: Boolean;
    function CheckSSL(SSLResult: Integer): Boolean;
    function CheckSSL(SSLResult: Pointer): Boolean;
    function InitContext(NeedCertificate: Boolean): Boolean; virtual;
    function DoneContext: Boolean; virtual;
    function InitSslKeys: boolean;virtual;
    function DoVerifyCert:boolean;
  public
    constructor Create; override;
    Destructor Destroy; override;
    // Socket methods
    function Connect : Boolean; override;
    function Close : Boolean; override;
    function Accept : Boolean; override;
    function Shutdown(BiDirectional : Boolean): boolean; override;
    function Send(Const Buffer; Count: Integer): Integer; override;
    function Recv(Const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    Function SSLActive: Boolean;
    function CreateSelfSignedCertificate(Const AHostName: string): Boolean; virtual;
    // Result of last CheckSSL call.
    Function SSLLastError: integer;
    property SSLLastErrorString: string read FSSLLastErrorString write SetSSLLastErrorString;
  published
    property SSLType: TSSLType read FSSLType write FSSLType;
    {:Password for decrypting of encoded certificate or key.}
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property KeyPassword: string read FKeyPassword write FKeyPassword;
    property CipherList: string read FCipherList write FCipherList;
    property Certificate : TSSLData Index 0 Read GetSSLData Write SetSSLData;
    property TrustedCertificate : TSSLData Index 1 Read GetSSLData Write SetSSLData;
    property PrivateKey : TSSLData Index 2 Read GetSSLData Write SetSSLData;
    property PFX: TSSLData Index 3 Read GetSSLData Write SetSSLData;
    property CertCA: TSSLData Index 4 Read GetSSLData Write SetSSLData;
    property VerifyPeerCert: Boolean read FVerifyPeerCert Write FVerifyPeerCert;
    Property SendHostAsSNI : Boolean Read FSendHostAsSNI Write FSendHostAsSNI;
    // In case a certificate must be generated as server, this is the hostname that will be used.
    property RemoteHostName : String Read FRemoteHostName Write FRemoteHostName;
    property OnVerifyCertificate: TVerifyCertificateEvent read FOnVerifyCertificate write FOnVerifyCertificate;
  end;


implementation

{ TSocketHandler }
Resourcestring
  SErrNoLibraryInit = 'Could not initialize OpenSSL library';

Procedure MaybeInitSSLInterface;

begin
  if not IsSSLloaded then
    if not InitSSLInterface then
      Raise EInOutError.Create(SErrNoLibraryInit);
end;


{ TSSLSocketHandler }

function TSSLSocketHandler.GetSSLData(AIndex: Integer): TSSLData;
begin
  Result:=FCertData[AIndex];
end;

procedure TSSLSocketHandler.SetSSLData(AIndex: Integer; AValue: TSSLData);
begin
  FCertData[AIndex].Assign(AValue);
end;

procedure TSSLSocketHandler.SetSSLLastErrorString(AValue: string);
begin
  if FSSLLastErrorString=AValue then Exit;
  FSSLLastErrorString:=AValue;
end;


function TSSLSocketHandler.DoVerifyCert: boolean;
begin
  Result:=True;
  If Assigned(OnVerifyCertificate) then
    OnVerifyCertificate(Self,Result);
end;

constructor TSSLSocketHandler.Create;

Var
  I : Integer;
begin
  inherited Create;
  FSendHostAsSNI:=True;
  MaybeInitSSLInterface;
  FCipherList:='DEFAULT';
  For I:=0 to SSLDataCount do
    FCertData[i]:=TSSLData.Create;
end;

Destructor TSSLSocketHandler.Destroy;

Var
  I : Integer;

begin
  FreeAndNil(FSSL);
  FreeAndNil(FCTX);
  inherited Destroy;
  For I:=0 to SSLDataCount do
    FreeAndNil(FCertData[i]);
end;

function TSSLSocketHandler.CreateSelfSignedCertificate(Const AHostName: string): Boolean;

Const
  OneDay = 60*60*24;
  SixtyDays = 60*OneDay;

var
  PK : PEVP_PKEY;
  X509 : PX509;
  RSA : PRSA;
  UTC : PASN1_UTCTIME;
  SN : PX509_NAME;
  B : PBIO;

begin
  Result:=False;
  PK:=Nil;
  X509:=Nil;
  try
    PK:=EvpPkeynew;
    X509:=X509New;
    RSA:=RsaGenerateKey(1024,$10001,nil,nil);
    EvpPkeyAssign(PK,EVP_PKEY_RSA,RSA);
    X509SetVersion(X509,2);
    Asn1IntegerSet(X509getSerialNumber(X509),0);
    UTC:=Asn1UtctimeNew;
    try
      X509GmtimeAdj(UTC,-OneDay);
      X509SetNotBefore(X509,UTC);
      X509GmtimeAdj(UTC,SixtyDays);
      X509SetNotAfter(X509,UTC);
    finally
      Asn1UtctimeFree(UTC);
    end;
    X509SetPubkey(X509,PK);
    SN:=X509GetSubjectName(X509);
    X509NameAddEntryByTxt(SN,'C',$1001,'CZ',-1,-1,0);
    X509NameAddEntryByTxt(SN,'CN',$1001, AHostName,-1,-1,0);
    x509SetIssuerName(X509,SN);
    x509Sign(X509,PK,EvpGetDigestByName('SHA1'));
    B:=BioNew(BioSMem);
    try
      i2dX509Bio(B,X509);
      Certificate.Value:=BioToString(B);
    finally
      BioFreeAll(b);
    end;
    B:=BioNew(BioSMem);
    try
      i2dPrivatekeyBio(B,PK);
      Privatekey.Value:=BioToString(B);
    finally
      BioFreeAll(b);
    end;
  finally
    X509free(X509);
    EvpPkeyFree(PK);
  end;
end;

function TSSLSocketHandler.Connect: Boolean;
begin
  Result:=Inherited Connect;
  if Result and InitContext(False) then
    begin
    Result:=CheckSSL(FSSL.SetFD(FSocket.Handle));
    if Result then
     begin
     if FSendHostAsSNI  and (FSocket is TInetSocket) then
       FSSL.Ctrl(SSL_CTRL_SET_TLSEXT_HOSTNAME,TLSEXT_NAMETYPE_host_name,PAnsiChar(AnsiString((FSocket as TInetSocket).Host)));
     Result:=CheckSSL(FSSL.Connect);
     if Result and VerifyPeerCert then
       Result:=(FSSL.VerifyResult<>0) or (not DoVerifyCert);
     if Result then
       FSSLActive:=True;
     end;
    end;
end;

function TSSLSocketHandler.Close: Boolean;
begin
  Result:=Shutdown(False);
end;

Function TSSLSocketHandler.FetchErrorInfo : Boolean;

var
  S : AnsiString;

begin
  FSSLLastErrorString:='';
  FLastError:=ErrGetError;
  ErrClearError;
  Result:=(FLastError>=1);
  if not Result then
    begin
    S:=StringOfChar(#0,256);
    ErrErrorString(FLastError,S,256);
    FSSLLastErrorString:=s;
    end;
end;

function TSSLSocketHandler.CheckSSL(SSLResult : Integer) : Boolean;

begin
  Result:=SSLResult>=1;
  if Not Result then
     begin
     FLastError:=SSLResult;
     FetchErrorInfo;
     end;
end;

function TSSLSocketHandler.CheckSSL(SSLResult: Pointer): Boolean;
begin
  Result:=(SSLResult<>Nil);
  if not Result then
    Result:=FetchErrorInfo;
end;

function TSSLSocketHandler.DoneContext: Boolean;

begin
  FreeAndNil(FSSL);
  FreeAndNil(FCTX);
  ErrRemoveState(0);
  FSSLActive:=False;
  Result:=True;
end;

Function HandleSSLPwd(buf : PAnsiChar; len:Integer; flags:Integer; UD : Pointer):Integer; cdecl;

var
  Pwd: AnsiString;
  H :  TSSLSocketHandler;

begin
  if Not Assigned(UD) then
    PWD:=''
  else
    begin
    H:=TSSLSocketHandler(UD);
    Pwd:=H.KeyPassword;
    end;
  if (len<Length(Pwd)+1) then
    SetLength(Pwd,len-1);
  pwd:=pwd+#0;
  Result:=Length(Pwd);
  Move(Pointer(Pwd)^,Buf^,Result);
end;

function TSSLSocketHandler.InitSslKeys: boolean;

begin
  Result:=(FCTX<>Nil);
  if not Result then
    Exit;
  if not Certificate.Empty then
    Result:=CheckSSL(FCTX.UseCertificate(Certificate));
  if Result and not PrivateKey.Empty then
    Result:=CheckSSL(FCTX.UsePrivateKey(PrivateKey));
  if Result and (CertCA.FileName<>'') then
    Result:=CheckSSL(FCTX.LoadVerifyLocations(CertCA.FileName,''));
  if Result and not PFX.Empty then
    Result:=CheckSSL(FCTX.LoadPFX(PFX,Self.KeyPassword));
end;

function TSSLSocketHandler.InitContext(NeedCertificate:Boolean): Boolean;

Const
  VO : Array[Boolean] of Integer = (SSL_VERIFY_NONE,SSL_VERIFY_PEER);

var
  s: AnsiString;

begin
  Result:=DoneContext;
  if Not Result then
    Exit;
  try
    FCTX:=TSSLContext.Create(SSLType);
  Except
    CheckSSL(Nil);
    Result:=False;
    Exit;
  end;
  S:=FCipherList;
  FCTX.SetCipherList(S);
  FCTX.SetVerify(VO[FVerifypeerCert],Nil);
  FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
  FCTX.SetDefaultPasswdCbUserdata(self);
  If NeedCertificate and Certificate.Empty and PFX.Empty then
    if Not CreateSelfSignedCertificate(RemoteHostName) then
      begin
      DoneContext;
      Exit(False);
      end;
   if Not InitSSLKeys then
     begin
     DoneContext;
     Exit(False);
     end;
   try
     FSSL:=TSSL.Create(FCTX);
     Result:=True;
   Except
     CheckSSL(Nil);
     DoneContext;
     Result:=False;
   end;
end;

function TSSLSocketHandler.Accept: Boolean;

begin
  Result:=InitContext(True);
  if Result then
    begin
    Result:=CheckSSL(FSSL.setfd(Socket.Handle));
    if Result then
      Result:=CheckSSL(FSSL.Accept);
    end;
  FSSLActive:=Result;
end;

function TSSLSocketHandler.Shutdown(BiDirectional : Boolean): boolean;

var
  r : integer;

begin
  Result:=assigned(FSsl);
  if Result then
    If Not BiDirectional then
      Result:=CheckSSL(FSSL.Shutdown)
    else
      begin
      r:=FSSL.Shutdown;
      if r<>0 then
        Result:=CheckSSL(r)
      else
        begin
        Result:=fpShutdown(FSocket.Handle,1)=0;
        if Result then
          Result:=CheckSSL(FSsl.Shutdown);
        end
      end;
  If Result then
    Result:=DoneContext;
end;

function TSSLSocketHandler.Send(Const Buffer; Count: Integer): Integer;
var
  e: integer;
begin
  FLastError := 0;
  FSSLLastErrorString:='';
  repeat
    Result:=FSsl.Write(@Buffer,Count);
    e:=FSsl.GetError(Result);
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    FLastError:=e;
end;

function TSSLSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

var
  e: integer;
begin
  FLastError:=0;
  FSSLLastErrorString:= '';
  repeat
    Result:=FSSL.Read(@Buffer ,Count);
    e:=FSSL.GetError(Result);
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    FLastError:=e;
end;

function TSSLSocketHandler.BytesAvailable: Integer;
begin
  Result:= FSSL.Pending;
end;

Function TSSLSocketHandler.SSLActive: Boolean;
begin
  Result:=FSSLActive;
end;

Function TSSLSocketHandler.SSLLastError: integer;
begin
  Result:=FLastError;
end;

end.

