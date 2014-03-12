unit baseconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, platform_mapping;

const
  SEC_COMMON = 'COMMON';
  SEC_LANGUAGE = 'LANGUAGE';

  KEY_PANEL_PADDING_LEFT = 'PanelPaddingLeft';
  KEY_PANEL_PADDING_RIGHT = 'PanelPaddingRight';
  KEY_PANEL_PADDING_TOP = 'PanelPaddingTop';
  KEY_PANEL_PADDING_BOTTOM = 'PanelPaddingBottom';
  KEY_LANGUAGE = 'Language';
  KEY_SKIN_COLOR = 'SKIN_COLOR';
  KEY_SU_PASSWORD = 'SU_PASSWORD';

type

  { TConfigBase }

  TConfigBase = class
  private
    FIni: TIniFile;
    FLangIni: TIniFile;
    FLanguage: string;
    FPanelPaddingLeft: integer;
    FPanelPaddingRight: integer;
    FPanelPaddingTop: integer;
    FPanelPaddingBottom: integer;
    FSkinColor: string;
    FSuPassword: string;

  public
    constructor Create;
    destructor Destroy; override;
    function GetString(AName: string): string;
    function GetString(AName: string; AParams: array of const): string;
    procedure Save;
  public
    property PanelPaddingLeft: integer read FPanelPaddingLeft write FPanelPaddingLeft;
    property PanelPaddingRight: integer read FPanelPaddingRight write FPanelPaddingRight;
    property PanelPaddingTop: integer read FPanelPaddingTop write FPanelPaddingTop;
    property PanelPaddingBottom: integer read FPanelPaddingBottom
      write FPanelPaddingBottom;
    property Language: string read FLanguage write FLanguage;
    property SkinColor: string read FSkinColor write FSkinColor;
    property SuPassword: string read FSuPassword write FSuPassword;
  end;

implementation

{ TConfigBase }

constructor TConfigBase.Create;
begin
  FIni := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  FPanelPaddingLeft := FIni.ReadInteger(SEC_COMMON, KEY_PANEL_PADDING_LEFT, 16);
  FPanelPaddingRight := FIni.ReadInteger(SEC_COMMON, KEY_PANEL_PADDING_RIGHT, 16);
  FPanelPaddingTop := FIni.ReadInteger(SEC_COMMON, KEY_PANEL_PADDING_TOP, 38);
  FPanelPaddingBottom := FIni.ReadInteger(SEC_COMMON, KEY_PANEL_PADDING_BOTTOM, 16);
  FLanguage := FIni.ReadString(SEC_COMMON, KEY_LANGUAGE, 'default.lng');
  FLangIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'lang' + SPL + FLanguage);
  FSkinColor := FIni.ReadString(SEC_COMMON, KEY_SKIN_COLOR, '#EA2F2F2F');
  FSuPassword:= FIni.ReadString(SEC_COMMON, KEY_SU_PASSWORD, '');
end;

destructor TConfigBase.Destroy;
begin

  FLangIni.Free;
  FIni.Free;
  inherited Destroy;
end;

function TConfigBase.GetString(AName: string): string;
begin
  Result := FLangIni.ReadString(SEC_LANGUAGE, AName, '');
end;

function TConfigBase.GetString(AName: string; AParams: array of const): string;
begin
  Result := Format(FLangIni.ReadString(SEC_LANGUAGE, AName, ''), AParams);
end;

procedure TConfigBase.Save;
begin
  FIni.WriteInteger(SEC_COMMON, KEY_PANEL_PADDING_LEFT, FPanelPaddingLeft);
  FIni.WriteInteger(SEC_COMMON, KEY_PANEL_PADDING_RIGHT, FPanelPaddingRight);
  FIni.WriteInteger(SEC_COMMON, KEY_PANEL_PADDING_TOP, FPanelPaddingTop);
  FIni.WriteInteger(SEC_COMMON, KEY_PANEL_PADDING_BOTTOM, FPanelPaddingBottom);
  FIni.WriteString(SEC_COMMON, KEY_LANGUAGE, FLanguage);
  FIni.WriteString(SEC_COMMON, KEY_SKIN_COLOR, FSkinColor);
  FIni.WriteString(SEC_COMMON, KEY_SU_PASSWORD, FSuPassword);
end;

end.
