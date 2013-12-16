unit unt_config;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, IniFiles;

type

    { TConfig }

    TConfig = class
    private
        FIni: TIniFile;
        FRootPassword: string;
        FServerAddress: string;
        FUserName: string;
        FUserPassword: string;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Load;
        procedure Save;
    public
        property UserName: string read FUserName write FUserName;
        property UserPassword: string read FUserPassword write FUserPassword;
        property ServerAddress: string read FServerAddress write FServerAddress;
        property RootPassword: string read FRootPassword write FRootPassword;
    end;

implementation

{ TConfig }

constructor TConfig.Create;
begin
    FIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'cfg.ini');
end;

destructor TConfig.Destroy;
begin
    FIni.Free;
    inherited Destroy;
end;

procedure TConfig.Load;
begin
    FUserName := FIni.ReadString('SM', 'UserName', '');
    FUserPassword := FIni.ReadString('SM', 'UserPassword', '');
    FRootPassword := FIni.ReadString('SM', 'RootPassword', '');
    FServerAddress := FIni.ReadString('SM', 'ServerAddress', '');
end;

procedure TConfig.Save;
begin
    FIni.WriteString('SM', 'UserName', FUserName);
    FIni.WriteString('SM', 'UserPassword', FUserPassword);
    FIni.WriteString('SM', 'RootPassword', FRootPassword);
    FIni.WriteString('SM', 'ServerAddress', FServerAddress);
end;

end.

