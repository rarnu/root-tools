{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  OAuth2 store using an .ini file.
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
unit fpoauth2ini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpoauth2, inifiles;

Type

  { TFPOAuth2IniStore }

  TFPOAuth2IniStore = Class(TAbstracTOAuth2ConfigStore)
  private
    FApplicationSection: String;
    FConfigFileName: String;
    FFileName: String;
    FProviderSection: String;
    FSessionFileName: String;
    FUserSection: String;
    procedure EnsureFileName;
    Procedure EnsureConfigSections;
  Protected
    Function DetectSessionFileName : String;
    Function EnsureUserSession(ASession: TOAuth2Session): Boolean; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;

    Procedure SaveConfigToIni(AIni : TCustomIniFile;AConfig : TOAuth2Config); virtual;
    Procedure LoadConfigFromIni(AIni : TCustomIniFile;AConfig : TOAuth2Config); virtual;
    Procedure SaveSessionToIni(AIni : TCustomIniFile;ASession : TOAuth2Session); virtual;
    Procedure LoadSessionFromIni(AIni : TCustomIniFile;ASession : TOAuth2Session); virtual;
    Procedure SaveConfig(AConfig : TOAuth2Config); override;
    Procedure LoadConfig(AConfig : TOAuth2Config); override;
    Procedure LoadSession(ASession : TOAuth2Session;Const AUser : String); override;
    Procedure SaveSession(Asession : TOAuth2Session;Const AUser : String); override;
  Published
    // Static configuration, readable by web process. Default is app config file.
    Property ConfigFileName: String Read FConfigFileName Write FConfigFileName;
    // Per-user (session) configuration, writeable by webprocess. Default is temp dir+'oauth-'+ConfigFileName
    Property SessionFileName: String Read FSessionFileName Write FSessionFileName;
    // Name of application section (Application)
    Property ApplicationSection : String Read FApplicationSection Write FApplicationSection;
    // Name of provider section (Provider)
    Property ProviderSection : String Read FProviderSection Write FProviderSection;
    // Name of User session section (username from ID)
    Property UserSessionSection : String Read FUserSection Write FUserSection;
  end;


implementation

uses typinfo;

Const
  // Default sections.

  SApplication = 'Application';
  SProvider    = 'Provider';

Const
  SClient            = 'Client';
  SAuth              = 'Authorization';

  KeyenableGZIP      = 'EnableGZIP';
  KeyApplicationName = 'ApplicationName';
  KeyMethod          = 'Method';

  // Application keys
  KeyClientID        = 'client_id';
  KeyClientSecret    = 'client_secret';
  KeyRedirectURI     = 'redirect_uri';
  KeyAccessType      = 'access_type';
  KeyDeveloperKey    = 'DeveloperKey';
  KeyOpenIDRealm     = 'OpenIDRealm';

  // Provider keys
  KeyHostedDomain    = 'HostedDomain';
  KeyTokenURL        = 'TokenURL';
  KeyAuthURL         = 'AuthURL';
  KeyAuthScope       = 'AuthScope';

  // User keys
  KeyAccessToken     = 'access_token';
  KeyRefreshToken    = 'refresh_token';
  KeyTokenType       = 'token_type';
  KeyExpiresAt       = 'expires_at';
  KeyExpiresIn       = 'expires_in';
  KeyLoginHint       = 'login_hint';
  KeyIDToken         = 'id_token';

{ TFPOAuth2IniStore }

Procedure Touch(FN : String);

begin
//  FileClose(FileCreate('/tmp/logs/'+fn));
end;

procedure TFPOAuth2IniStore.EnsureFileName;

begin
  If (ConfigFileName='') then
    ConfigFileName:=GetAppConfigFile(True);
  if SessionFIleName='' then
    SessionFileName:=GetTempDir(True)+'oauth-'+ExtractFileName(GetAppConfigFile(True));
end;

procedure TFPOAuth2IniStore.EnsureConfigSections;
begin
  if (ApplicationSection='') then
    ApplicationSection:=SApplication;
  if (ProviderSection='') then
    ProviderSection:=SProvider;
end;

function TFPOAuth2IniStore.DetectSessionFileName: String;
begin
  Result:=FSessionFileName;
  If Result='' then
    Result:=ConfigFileName
end;

procedure TFPOAuth2IniStore.SaveConfigToIni(AIni: TCustomIniFile; AConfig: TOAuth2Config);

begin
  EnsureConfigSections;
  Touch('saveconfigfomini');
  Touch('saveconfigfomini-app-'+ApplicationSection);
  Touch('saveconfigfomini-provider-'+ProviderSection);
  With AIni,AConfig do
    begin
    WriteString(ApplicationSection,KeyClientID,ClientID);
    WriteString(ApplicationSection,KeyClientSecret,ClientSecret);
    WriteString(ApplicationSection,KeyRedirectURI,RedirectURI);
    WriteString(ApplicationSection,KeyDeveloperKey,DeveloperKey);
    WriteString(ApplicationSection,KeyOpenIDRealm,OpenIDRealm);
    WriteString(ApplicationSection,KeyAccessType,GetEnumName(Typeinfo(TAccessType),Ord(AccessType)));
    WriteString(ProviderSection,KeyHostedDomain,HostedDomain);
    WriteString(ProviderSection,KeyTokenURL,TokenURL);
    WriteString(ProviderSection,KeyAuthURL,AuthURL);
    WriteString(ProviderSection,KeyAuthScope,AuthScope);
    end;
end;

procedure TFPOAuth2IniStore.LoadConfigFromIni(AIni: TCustomIniFile;
  AConfig: TOAuth2Config);

Var
  S : String;
  i : Integer;

begin
  EnsureConfigSections;
  Touch('Loadconfigfomini');
  Touch('Loadconfigfomini-app-'+ApplicationSection);
  Touch('Loadconfigfomini-provider-'+ProviderSection);
  With AIni,AConfig do
    begin
    ClientID:=ReadString(ApplicationSection,KeyClientID,ClientID);
    ClientSecret:=ReadString(ApplicationSection,KeyClientSecret,ClientSecret);
    RedirectURI:=AIni.ReadString(ApplicationSection,KeyRedirectURI,RedirectURI);
    DeveloperKey:=AIni.ReadString(ApplicationSection,KeyDeveloperKey,DeveloperKey);
    OpenIDRealm:=AIni.ReadString(ApplicationSection,KeyOpenIDRealm,OpenIDRealm);
    S:=AIni.ReadString(ApplicationSection,KeyAccessType,GetEnumName(Typeinfo(TAccessType),Ord(AccessType)));
    i:= GetEnumValue(TYpeinfo(TAccessType),S);
    if (I<>-1) then
      AccessType:=TAccessType(i);
    HostedDomain:=ReadString(ProviderSection,KeyHostedDomain,HostedDomain);
    TokenURL:=ReadString(ProviderSection,KeyTokenURL,TokenURL);
    AuthURL:=ReadString(ProviderSection,KeyAuthURL,AuthURL);
    AuthScope:=ReadString(ProviderSection,KeyAuthScope,AuthScope);
    end;
end;

procedure TFPOAuth2IniStore.SaveSessionToIni(AIni: TCustomIniFile;
  ASession: TOAuth2Session);
begin
  Touch('savesessiontoini'+usersessionsection);
  With AIni,ASession do
    begin
    WriteString(UserSessionSection,KeyLoginHint,LoginHint);
    WriteString(UserSessionSection,KeyAccessToken,AccessToken);
    WriteString(UserSessionSection,KeyRefreshToken,RefreshToken);
    WriteString(UserSessionSection,KeyTokenType,AuthTokenType);
    WriteInteger(UserSessionSection,KeyExpiresIn,AuthExpiryPeriod);
    WriteDateTime(UserSessionSection,KeyExpiresAt,AuthExpires);
    WriteString(UserSessionSection,KeyIDToken,IDToken);
    end;
end;

procedure TFPOAuth2IniStore.LoadSessionFromIni(AIni: TCustomIniFile;
  ASession: TOAuth2Session);
begin
  Touch('loadsessionini-'+usersessionsection);
  With AIni,ASession do
    begin
    LoginHint:=ReadString(UserSessionSection,KeyLoginHint,LoginHint);
    AccessToken:=ReadString(UserSessionSection,KeyAccessToken,AccessToken);
    RefreshToken:=ReadString(UserSessionSection,KeyRefreshToken,RefreshToken);
    AuthTokenType:=ReadString(UserSessionSection,KeyTokenType,AuthTokenType);
    AuthExpiryPeriod:=ReadInteger(UserSessionSection,KeyExpiresIn,0);
    AuthExpires:=ReadDateTime(UserSessionSection,KeyExpiresAt,AuthExpires);
    IDToken:=ReadString(UserSessionSection,KeyIDToken,'');
    end;
end;

procedure TFPOAuth2IniStore.SaveConfig(AConfig: TOAuth2Config);

Var
  Ini : TMemIniFile;

begin
  Touch('saveconfig');
  EnsureFileName;
  Ini:=TMemIniFile.Create(ConfigFileName);
  try
    SaveConfigToIni(Ini,AConfig);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TFPOAuth2IniStore.LoadConfig(AConfig: TOAuth2Config);
Var
  Ini : TMemIniFile;

begin
  Touch('loadconfig');
  EnsureFileName;
  Ini:=TMemIniFile.Create(ConfigFileName);
  try
    LoadConfigFromIni(Ini,AConfig);
  finally
    Ini.Free;
  end;
end;

function TFPOAuth2IniStore.EnsureUserSession(ASession: TOAuth2Session): Boolean;

begin
  Result:=(UserSessionSection<>'');
end;

constructor TFPOAuth2IniStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EnsureConfigSections;
end;

destructor TFPOAuth2IniStore.Destroy;
begin
  inherited Destroy;
end;

procedure TFPOAuth2IniStore.LoadSession(ASession: TOAuth2Session;
  const AUser: String);

Var
  Ini : TMemIniFile;

begin
  Touch('loadsession');
  EnsureFileName;
  If not EnsureUserSession(ASession) then
    Exit;
  Ini:=TMemIniFile.Create(SessionFileName);
  try
    LoadSessionFromIni(Ini,ASession);
  finally
    Ini.Free;
  end;
end;

procedure TFPOAuth2IniStore.SaveSession(Asession: TOAuth2Session;
  const AUser: String);

Var
  Ini : TMemIniFile;

begin
  EnsureFileName;
  If not EnsureUserSession(ASession) then
    Exit;
  Ini:=TMemIniFile.Create(SessionFileName);
  try
    SaveSessionToIni(Ini,ASession);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

end.

