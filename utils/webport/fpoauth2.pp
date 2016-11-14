{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  OAuth2 web request handler classes 
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
unit fpoauth2;

{$mode objfpc}{$H+}

interface

uses
  Typinfo,Classes, SysUtils, fpjson, fpjwt, fpwebclient;

Type
  { TOAuth2Config }
  TAccessType = (atOnline,atOffline);
  TAbstracTOAuth2ConfigStore = Class;
  EOAuth2 = Class(Exception);
  { TOAuth2Config }

  { TJWTIDToken }

  TJWTIDToken = Class(TJWT)
  private
    FClaimsClass: TClaimsClass;
    FJOSEClass: TJOSEClass;
  Protected
    Function CreateClaims : TClaims; override;
    Function CreateJOSE : TJOSE; override;
    Property ClaimsClass: TClaimsClass Read FClaimsClass;
    Property JOSEClass: TJOSEClass Read FJOSEClass;
  Public
    // Pass on the actual Claims/JOSE class to be used. When Nil, defaults are used.
    Constructor CreateWithClasses(AClaims: TClaimsClass; AJOSE : TJOSEClass);
    // Extract a unique user ID from the claims. By default, this calls GetUniqueUserName
    Function GetUniqueUserID : String; virtual;
    // Extract a unique user name from the claims. Must be overridden by descendents.
    Function GetUniqueUserName : String; virtual;
    // Extract a user display name from the claims. By default, this calls GetUniqueUserName
    Function GetUserDisplayName : String; virtual;
  end;
  // OAuth2 client and server settings.

  TOAuth2Config = Class(TPersistent)
  private
    FAuthScope: String;
    FAuthURL: String;
    FClientID: String;
    FClientSecret: String;
    FRedirectURI: String;
    FDeveloperKey: String;
    FHostedDomain: String;
    FIncludeGrantedScopes: Boolean;
    FOpenIDRealm: String;
    FTokenURL: String;
    FAccessType: TAccessType;
  Protected
  Public
    Procedure Assign(Source : TPersistent); override;
    Procedure SaveToStrings(L : TStrings);
  Published
    //
    // Local OAuth2 client config part.
    //
    Property ClientID : String Read FClientID Write FClientID;
    Property ClientSecret : String Read FClientSecret Write FClientSecret;
    Property RedirectURI : String Read FRedirectURI Write FRedirectURI;
    Property AccessType : TAccessType Read FAccessType Write FAccessType;
    // Specific for google.
    Property DeveloperKey : String Read FDeveloperKey Write FDeveloperKey;
    Property OpenIDRealm : String Read FOpenIDRealm Write FOpenIDRealm;
    //
    // Auth Provider part
    //
    // Domain part, can be substituted on URL to refresh access token
    Property HostedDomain : String Read FHostedDomain Write FHostedDomain;
    // URL to authenticate a user. used in creating the redirect URL. Can contain %HostedDomain%
    Property AuthURL: String Read FAuthURL Write FAuthURL;
    // URL To exchange authorization code for access token. Can contain %HostedDomain%
    Property TokenURL: String Read FTokenURL Write FTokenURL;
    // Authorized Scopes (Google parlance) or resources (Microsoft parlance)
    Property AuthScope: String Read FAuthScope Write FAuthScope;
    // Google specific: adds AuthScope to existing scopes (incremental increase of authorization).
    Property IncludeGrantedScopes : Boolean Read FIncludeGrantedScopes Write FIncludeGrantedScopes;
  end;
  TOAuth2ConfigClass = Class of TOAuth2Config;

  { TOAuth2Session }
  //
  // User config part
  //

  TOAuth2Session = Class(TPersistent)
  Private
    FRefreshToken: String;
    FLoginHint: String;
    FIDToken: String;
    FState: String;
    FAccessToken: String;
    FAuthTokenType: String;
    FAuthCode: String;
    FAuthExpires: TDateTime;
    FAuthExpiryPeriod: Integer;
    procedure SetAuthExpiryPeriod(AValue: Integer);
  Protected
    Class Function AuthExpiryMargin : Integer; virtual;
    procedure DoLoadFromJSON(AJSON: TJSONObject); virtual;
  Public
    Procedure LoadTokensFromJSONResponse(Const AJSON : String);
    Procedure LoadStartTokensFromVariables(Const Variables : TStrings);
    Procedure SaveToStrings(L : TStrings);
    procedure Assign(Source: TPersistent); override;
  Published
    // Authentication code received at the first step of the OAuth2 sequence
    Property AuthCode: String Read FAuthCode Write FAuthCode;
    // Access token to be used for authorized scopes. Received in step 2 of the OAuth2 sequence;
    Property AccessToken: String Read FAccessToken Write FAccessToken;
    // Refresh token to renew Access token. received in step 2 of the OAuth2 sequence;
    Property RefreshToken : String Read FRefreshToken Write FRefreshToken;
    // When does the authentication end, local time.
    Property AuthExpires : TDateTime Read FAuthExpires Write FAuthExpires;
    // Seconds till access token expires. Setting this will set the AuthExpires property to Now+(AuthExpiryPeriod-AuthExpiryMargin)
    Property AuthExpiryPeriod : Integer Read FAuthExpiryPeriod Write SetAuthExpiryPeriod;
    // Token type (Bearer)
    Property AuthTokenType: String Read FAuthTokenType Write FAuthTokenType;
    // State, saved as part of the user config.
    Property State : String Read FState Write FState;
    // Login hint
    Property LoginHint : String Read FLoginHint Write FLoginHint;
    // IDToken
    Property IDToken : String Read FIDToken Write FIDToken;
  end;
  TOAuth2SessionClass = Class of TOAuth2Session;

  TAbstractOAuth2ConfigStore = CLass(TComponent)
  Public
    Procedure SaveConfig(AConfig : TOAuth2Config); virtual; abstract;
    Procedure LoadConfig(AConfig : TOAuth2Config); virtual; abstract;
    Procedure SaveSession(ASession : TOAuth2Session; Const AUser : String); virtual; abstract;
    Procedure LoadSession(ASession : TOAuth2Session; Const AUser : String); virtual; abstract;
  end;
  TAbstractOAuth2ConfigStoreClass = Class of TAbstractOAuth2ConfigStore;

  TUserConsentHandler = Procedure (Const AURL : String; Out AAuthCode : String) of object;
  TOnAuthConfigChangeHandler = Procedure (Const Sender : TObject; Const AConfig : TOAuth2Config) of object;
  TOnAuthSessionChangeHandler = Procedure (Const Sender : TObject; Const ASession : TOAuth2Session) of object;
  TOnIDTokenChangeHandler = Procedure (Const Sender : TObject; Const AToken : TJWTIDToken) of object;
  TSignRequestHandler = Procedure (Const Sender : TObject; Const ARequest : TWebClientRequest)of object;

  TAuthenticateAction = (aaContinue,aaRedirect,aaFail);

  { TOAuth2Handler }

  TOAuth2Handler = Class(TAbstractRequestSigner)
  private
    FAutoStore: Boolean;
    FClaimsClass: TClaimsClass;
    FConfig: TOAuth2Config;
    FConfigLoaded: Boolean;
    FIDToken: TJWTIDToken;
    FOnAuthSessionChange: TOnAuthSessionChangeHandler;
    FOnIDTokenChange: TOnIDTokenChangeHandler;
    FSession: TOAuth2Session;
    FOnAuthConfigChange: TOnAuthConfigChangeHandler;
    FOnSignRequest: TOnAuthSessionChangeHandler;
    FOnUserConsent: TUserConsentHandler;
    FSessionLoaded: Boolean;
    FWebClient: TAbstractWebClient;
    FStore : TAbstracTOAuth2ConfigStore;
    procedure SetConfig(AValue: TOAuth2Config);
    procedure SetSession(AValue: TOAuth2Session);
    procedure SetStore(AValue: TAbstracTOAuth2ConfigStore);
  Protected
    Function RefreshToken: Boolean; virtual;
    Function CreateOauth2Config : TOAuth2Config; virtual;
    Function CreateOauth2Session : TOAuth2Session; virtual;
    Function CreateIDToken : TJWTIDToken; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure DoAuthConfigChange; virtual;
    Procedure DoAuthSessionChange; virtual;
    Procedure DoSignRequest(ARequest: TWebClientRequest); override;
    Property ConfigLoaded : Boolean Read FConfigLoaded;
    Property SessionLoaded : Boolean Read FSessionLoaded;
  Public
    Class Var DefaultConfigClass : TOAuth2ConfigClass;
    Class Var DefaultSessionClass : TOAuth2SessionClass;
  Public
    Constructor Create(AOwner : TComponent);override;
    Destructor Destroy; override;
    // Variable name for AuthScope in authentication URL.
    // Default = scope. Descendents can override this to provide correct behaviour.
    Class Function AuthScopeVariableName : String; virtual;
    // Check if config is authenticated.
    Function IsAuthenticated : Boolean; virtual;
    // Generate an authentication URL
    Function AuthenticateURL : String; virtual;
    // Check what needs to be done for authentication.
    // Do whatever is necessary to mark the request as 'authenticated'.
    Function Authenticate: TAuthenticateAction; virtual;
    // Load config from store
    procedure LoadConfig;
    // Save config to store
    procedure SaveConfig;
    // Load Session from store.If AUser is empty, then ID Token.GetUniqueUser is used.
    procedure LoadSession(Const AUser : String = '');
    // Save session in store. If AUser is empty, then ID Token.GetUniqueUser is used. Will call OnAuthSessionChange
    procedure SaveSession(Const AUser : String = '');
    // Refresh ID token from Session.IDToken. Called after token is refreshed or session is loaded.
    // This will change the actual IDToken instance.
    procedure RefreshIDToken;
    // This is populated from Config.IDToken if it is not empty. Do not cache this instance. It is recreated after a call to RefreshIDToken
    Property IDToken : TJWTIDToken Read FIDToken;
    // Set this to initialize the claims for the ID token. By default, it is TClaims
    Property ClaimsClass : TClaimsClass Read FClaimsClass Write FClaimsClass;
  Published
    // Must be set prior to calling
    Property Config : TOAuth2Config Read FConfig Write SetConfig;
    // Session info.
    Property Session : TOAuth2Session Read FSession Write SetSession;
    // Webclient used to do requests to authorization service
    Property WebClient : TAbstractWebClient Read FWebClient Write FWebClient;
    // Event handler to get user consent if no access token or refresh token is available
    Property OnUserConsent : TUserConsentHandler Read FOnUserConsent Write FOnUserConsent;
    // Called when the auth config informaion changes
    Property OnAuthConfigChange : TOnAuthConfigChangeHandler Read FOnAuthConfigChange Write FOnAuthConfigChange;
    // Called when the auth sesson information changes
    Property OnAuthSessionChange : TOnAuthSessionChangeHandler Read FOnAuthSessionChange Write FOnAuthSessionChange;
    // Called when the IDToken information changes
    Property OnIDTokenChange : TOnIDTokenChangeHandler Read FOnIDTokenChange Write FOnIDTokenChange;
    // Called when a request is signed
    Property OnSignRequest : TOnAuthSessionChangeHandler Read FOnSignRequest Write FOnSignRequest;
    // User to load/store parts of the config store.
    Property Store : TAbstracTOAuth2ConfigStore Read FStore Write SetStore;
    // Call storing automatically when needed.
    Property AutoStore : Boolean Read FAutoStore Write FAutoStore;
  end;
  TOAuth2HandlerClass = Class of TOAuth2Handler;



implementation

uses httpdefs;

Resourcestring
  SErrFailedToRefreshToken = 'Failed to refresh access token: Status %d, Error: %s';

{ TOAuth2Handler }

{ Several possibilities:
  1. Acess token is available.
     A) Access token is not yet expired
        -> All is well, continue.
     B) Access token is available, but is expired.
        Refresh token is
          i) Available
             -> get new access token using refresh token.
             (may fail -> fail)
          ii) Not available
              -> error.
  3. No access token is available.
     A) Offline
        -> Need to get user consent using callback.
        i) User consent results in Access token (AConfig.AuthToken)
           ->  Auth token is exchanged for a refresh token & access token
        ii) User consent failed or no callback.
           -> Fail
     B) Online: Need to redirect to get access token and auth token.

}

{ TTWTIDToken }

constructor TJWTIDToken.CreateWithClasses(AClaims: TClaimsClass;
  AJOSE: TJOSEClass);
begin
  FClaimsClass:=AClaims;
  FJOSEClass:=AJOSE;
  Inherited Create;
end;

function TJWTIDToken.GetUniqueUserID: String;
begin
  Result:=GetUniqueUserName;
end;

function TJWTIDToken.GetUniqueUserName: String;
begin
  Result:='';
end;

function TJWTIDToken.GetUserDisplayName: String;
begin
  Result:=GetUniqueUserName;
end;

function TJWTIDToken.CreateClaims: TClaims;
begin
  if FClaimsClass=Nil then
    Result:=Inherited CreateClaims
  else
    Result:=FClaimsClass.Create;
end;

function TJWTIDToken.CreateJOSE: TJOSE;
begin
  if FJOSEClass=Nil then
    Result:=Inherited CreateJOSE
  else
  Result:=FJOSEClass.Create;
end;

function TOAuth2Handler.Authenticate: TAuthenticateAction;

Var
  S : String;

begin
  if IsAuthenticated then
    result:=aaContinue
  else
    Case Config.AccessType of
      atonline :
        Result:=aaRedirect; // we need to let the user authenticate himself.
      atoffline :
        if Not Assigned(FOnUserConsent) then
          result:=aaFail
        else
          begin
          FOnUserConsent(AuthenticateURL,S);
          Session.AuthCode:=S;
          // Exchange authcode for access code.
          if IsAuthenticated then
            result:=aaContinue
          else
            result:=aaFail
          end;
    end;
end;

function TOAuth2Handler.AuthenticateURL: String;
begin
  Result:=Config.AuthURL
        + '?'+ AuthScopeVariableName+'='+HTTPEncode(Config.AuthScope)
        +'&redirect_uri='+HTTPEncode(Config.RedirectUri)
        +'&client_id='+HTTPEncode(Config.ClientID)
        +'&response_type=code'; // Request refresh token.
  if Assigned(Session) then
    begin
    if (Session.LoginHint<>'') then
      Result:=Result +'&login_hint='+HTTPEncode(Session.LoginHint);
    if (Session.State<>'') then
      Result:=Result +'&state='+HTTPEncode(Session.State);
    end;
end;

procedure TOAuth2Handler.SetConfig(AValue: TOAuth2Config);

begin
  if FConfig=AValue then Exit;
  FConfig.Assign(AValue);
end;

procedure TOAuth2Handler.SetSession(AValue: TOAuth2Session);
begin
  if FSession=AValue then Exit;
  FSession.Assign(AValue);
end;

procedure TOAuth2Handler.LoadConfig;

begin
  if Assigned(Store) and not ConfigLoaded then
    begin
    Store.LoadConfig(Config);
    FConfigLoaded:=True;
    end;
end;

procedure TOAuth2Handler.SaveConfig;
begin
  if Assigned(Store) then
    begin
    Store.SaveConfig(Config);
    FConfigLoaded:=True;
    end;
end;

procedure TOAuth2Handler.LoadSession(const AUser: String);

Var
  U : String;

begin
  if Assigned(Store) then
    begin
    U:=AUser;
    If (U='') and Assigned(FIDToken) then
      U:=FIDToken.GetUniqueUserID;
    Store.LoadSession(Session,AUser);
    FSessionLoaded:=True;
    if (Session.IDToken<>'') then
      RefreshIDToken;
    end;
end;

procedure TOAuth2Handler.SaveSession(const AUser: String);

Var
  U : String;

begin
  if Assigned(FOnAuthSessionChange) then
    FOnAuthSessionChange(Self,Session);
  if Assigned(Store) then
    begin
    Store.SaveSession(Session,AUser);
    FSessionLoaded:=True;
    end;
end;

procedure TOAuth2Handler.RefreshIDToken;
begin
  FreeAndNil(FIDToken);
  if (Session.IDToken<>'') then
    begin
    FIDtoken:=CreateIDToken;
    FIDToken.AsEncodedString:=Session.IDToken;
    If Assigned(FOnIDTokenChange) then
      FOnIDTokenChange(Self,FIDToken);
    end;
end;

function TOAuth2Handler.RefreshToken: Boolean;

Var
  URL,Body : String;
  D : TJSONData;
  Req: TWebClientRequest;
  Resp: TWebClientResponse;

begin
  LoadConfig;
  Req:=Nil;
  Resp:=Nil;
  D:=Nil;
  try
    Req:=WebClient.CreateRequest;
    Req.Headers.Values['Content-Type']:='application/x-www-form-urlencoded';
    url:=Config.TOKENURL;
    Body:='client_id='+HTTPEncode(Config.ClientID)+
          '&client_secret='+ HTTPEncode(Config.ClientSecret);
    if (Session.RefreshToken<>'') then
      body:=Body+'&refresh_token='+HTTPEncode(Session.RefreshToken)+
                 '&grant_type=refresh_token'
    else
      begin
      body:=Body+
            '&grant_type=authorization_code'+
            '&redirect_uri='+HTTPEncode(Config.RedirectUri)+
            '&code='+HTTPEncode(Session.AuthCode);
      end;
    Req.SetContentFromString(Body);
    Resp:=WebClient.ExecuteRequest('POST',url,Req);
    Result:=(Resp.StatusCode=200);
    if Result then
      begin
      Session.LoadTokensFromJSONResponse(Resp.GetContentAsString);
      If (Session.IDToken)<>'' then
        begin
        RefreshIDToken;
        DoAuthSessionChange;
        end;
      end
    else
      Raise EOAuth2.CreateFmt(SErrFailedToRefreshToken,[Resp.StatusCode,Resp.StatusText]);
    Result:=True;
  finally
    D.Free;
    Resp.Free;
    Req.Free;
  end;
end;

function TOAuth2Handler.CreateOauth2Config: TOAuth2Config;
begin
  Result:=DefaultConfigClass.Create;
end;

function TOAuth2Handler.CreateOauth2Session: TOAuth2Session;
begin
  Result:=DefaultSessionClass.Create;
end;

function TOAuth2Handler.CreateIDToken: TJWTIDToken;
begin
  Result:=TJWTIDToken.CreateWithClasses(ClaimsClass,Nil);
end;

procedure TOAuth2Handler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
    if AComponent=FStore then
      FStore:=Nil;
end;

function TOAuth2Handler.IsAuthenticated: Boolean;

begin
  LoadConfig;
  // See if we need to load the session
  if (Session.RefreshToken='') then
    LoadSession;
  Result:=(Session.AccessToken<>'');
  If Result then
    // have access token. Check if it is still valid.
    begin
    // Not expired ?
    Result:=(Now<Session.AuthExpires);
    // Expired, but have refresh token ?
    if (not Result) and (Session.RefreshToken<>'') then
      Result:=RefreshToken;
    end
  else if (Session.RefreshToken<>'') then
    begin
    // No access token, but have refresh token
    Result:=RefreshToken;
    end
  else  if (Session.AuthCode<>'') then
    // No access or refresh token, but have auth code.
      Result:=RefreshToken;
end;


{ TOAuth2Handler }


procedure TOAuth2Handler.DoAuthConfigChange;
begin
  If Assigned(FOnAuthConfigChange) then
    FOnAuthConfigChange(Self,Config);
  SaveConfig;
end;

procedure TOAuth2Handler.DoAuthSessionChange;
begin
  If Assigned(FOnAuthSessionChange) then
    FOnAuthSessionChange(Self,Session);
  SaveSession;
end;

procedure TOAuth2Handler.DoSignRequest(ARequest: TWebClientRequest);

Var
  TT,AT : String;
begin
  if Authenticate=aaContinue then
    begin
    TT:=Session.AuthTokenType;
    AT:=Session.AccessToken;
    Arequest.Headers.Add('Authorization: '+TT+' '+HTTPEncode(AT));
    end
  else
    Raise EOAuth2.Create('Cannot sign request: not authorized');
end;

constructor TOAuth2Handler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig:=CreateOauth2Config;
  FSession:=CreateOauth2Session;
end;

destructor TOAuth2Handler.Destroy;
begin
  FreeAndNil(FIDToken);
  FreeAndNil(FConfig);
  FreeAndNil(FSession);
  inherited Destroy;
end;

class function TOAuth2Handler.AuthScopeVariableName: String;
begin
  Result:='scope';
end;


{ TOAuth2Config }

procedure TOAuth2Handler.SetStore(AValue: TAbstracTOAuth2ConfigStore);
begin
  if FStore=AValue then Exit;
  if Assigned(FStore) then
    FStore.RemoveFreeNotification(Self);
  FStore:=AValue;
  if Assigned(FStore) then
    FStore.FreeNotification(Self);
end;

class function TOAuth2Session.AuthExpiryMargin: Integer;
begin
  Result:=10;
end;

procedure TOAuth2Session.SetAuthExpiryPeriod(AValue: Integer);
begin
  if FAuthExpiryPeriod=AValue then Exit;
  FAuthExpiryPeriod:=AValue;
  AuthExpires:=Now+AValue/SecsPerDay;
end;


procedure TOAuth2Config.Assign(Source: TPersistent);

Var
  C : TOAuth2Config;

begin
  if Source is TOAuth2Config then
    begin
    C:=Source as TOAuth2Config;
    FAuthURL:=C.AuthURL;
    FTokenURL:=C.TokenURL;
    FClientID:=C.ClientID;
    FClientSecret:=C.ClientSecret;
    FRedirectURI:=C.RedirectURI;
    FAccessType:=C.AccessType;
    FDeveloperKey:=C.DeveloperKey;
    FHostedDomain:=C.HostedDomain;
    FIncludeGrantedScopes:=C.IncludeGrantedScopes;
    FOpenIDRealm:=C.OpenIDRealm;
    FAuthScope:=C.AuthScope;
    end
  else
    inherited Assign(Source);
end;

procedure TOAuth2Config.SaveToStrings(L: TStrings);
  Procedure W(N,V : String);

  begin
    L.Add(N+'='+V);
  end;

begin
  W('AuthURL',AuthURL);
  W('TokenURL',TokenURL);
  W('ClientID',ClientID);
  W('ClientSecret',ClientSecret);
  W('RedirectURI',RedirectURI);
  W('AccessType',GetEnumName(TypeInfo(TAccessType),Ord(AccessType)));
  W('DeveloperKey',DeveloperKey);
  W('HostedDomain',HostedDomain);
  W('IncludeGrantedScopes',BoolToStr(IncludeGrantedScopes,True));
  W('OpenIDRealm',OpenIDRealm);
  W('AuthScope',AuthScope);
end;

procedure TOAuth2Session.SaveToStrings(L: TStrings);

  Procedure W(N,V : String);

  begin
    L.Add(N+'='+V);
  end;

begin
  W('AuthCode',AuthCode);
  W('RefreshToken',RefreshToken);
  W('LoginHint',LoginHint);
  W('IDToken',IDToken);
  W('AccessToken',AccessToken);
  W('AuthExpiryPeriod',IntToStr(AuthExpiryPeriod));
  W('AuthExpires',DateTimeToStr(AuthExpires));
  W('State',State);
  W('AuthTokenType',AuthTokenType);
end;

procedure TOAuth2Session.Assign(Source: TPersistent);

Var
  C : TOAuth2Session;

begin
  if Source is TOAuth2Session then
    begin
    C:=Source as TOAuth2Session;
    FAuthCode:=C.AuthCode;
    FRefreshToken:=C.RefreshToken;
    FLoginHint:=C.LoginHint;
    FIDToken:=C.IDToken;
    FAccessToken:=C.AccessToken;
    FAuthExpiryPeriod:=C.AuthExpiryPeriod;
    FAuthExpires:=C.AuthExpires;
    FState:=C.State;
    FAuthTokenType:=C.AuthTokenType;
    end
  else
    inherited Assign(Source);
end;


procedure TOAuth2Session.DoLoadFromJSON(AJSON: TJSONObject);

  Function Get(Const AName,ADefault : String) : String;

  begin
    Result:=AJSON.Get(AName,ADefault);
  end;

Var
  i : Integer;

begin
  AccessToken:=Get('access_token',AccessToken);
  RefreshToken:=Get('refresh_token',RefreshToken);
  AuthTokenType:=Get('token_type',AuthTokenType);
  IDToken:=Get('id_token',IDToken);
  // Microsoft sends expires_in as String !!
  I:=AJSON.IndexOfName('expires_in');
  if (I<>-1) then
    begin
    I:=AJSON.Items[i].AsInteger;
    if (I>0) then
      AuthExpiryPeriod:=I;
    end;
end;

procedure TOAuth2Session.LoadTokensFromJSONResponse(const AJSON: String);

Var
  D : TJSONData;

begin
  D:=GetJSON(AJSON);
  try
    DoLoadFromJSON(D as TJSONObject);
  finally
    D.Free;
  end;
end;

procedure TOAuth2Session.LoadStartTokensFromVariables(const Variables: TStrings);

  Function Get(Const AName,ADefault : String) : String;

  Var
    I : Integer;

  begin
    I:=Variables.IndexOfName(AName);
    if I=-1 then
      Result:=ADefault
    else
      Result:=Variables.ValueFromIndex[i];
  end;

begin
  AuthCode:=Get('code',AuthCode);
  LoginHint:=Get('login_hint',LoginHint);
end;


initialization
  TOAuth2Handler.DefaultConfigClass:=TOAuth2Config;
  TOAuth2Handler.DefaultSessionClass:=TOAuth2Session;
end.

