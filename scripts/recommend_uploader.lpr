program recommend_uploader;

{$mode objfpc}{$H+}

uses
    cthreads,
    Classes,
    SysUtils,
    CustApp,
    unt_command,
    unt_recommend_upload;

type

    { TRecommendUploader }

    TRecommendUploader = class(TCustomApplication)
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
    end;

    { TRecommendUploader }

    procedure TRecommendUploader.DoRun;
    var
        apk: string;
        icon: string;
        unixNme: string;
        mode: integer;
        account: string;
        password: string;
    begin
        if HasOption('h', 'help') then
        begin
            WriteHelp;
            Terminate;
            Exit;
        end;
        if HasOption('f') then
        begin
            apk := GetOptionValue('f');
        end;
        if HasOption('i') then
        begin
            icon := GetOptionValue('i');
        end;
        if HasOption('u') then
        begin
            unixNme := GetOptionValue('u');
        end;
        if HasOption('m') then
        begin
            mode := StrToIntDef(GetOptionValue('m'), -1);
        end;
        if HasOption('a') then
        begin
            account := GetOptionValue('a');
        end;
        if HasOption('p') then
        begin
            password := GetOptionValue('p');
        end;
        if (apk = '') or (icon = '') or (unixNme = '') or (mode = -1) or (account = '') or (password = '') then
        begin
            WriteHelp;
            Terminate;
            Exit;
        end;
        DoUpload(apk, icon, unixNme, mode, account, password);

        Terminate;
    end;

    constructor TRecommendUploader.Create(TheOwner: TComponent);
    begin
        inherited Create(TheOwner);
        StopOnException := True;
    end;

    destructor TRecommendUploader.Destroy;
    begin
        inherited Destroy;
    end;

    procedure TRecommendUploader.WriteHelp;
    begin
        WriteLn('Usage:  -h');
        WriteLn('  -f <apk file path>');
        WriteLn('  -i <icon file path>');
        WriteLn('  -u <unix name>');
        WriteLn('  -m <mode: 0=add, 1=update>');
        WriteLn('  -a <user account>');
        WriteLn('  -p <user password>');
    end;

var
    Application: TRecommendUploader;
begin
    Application := TRecommendUploader.Create(nil);
    Application.Run;
    Application.Free;
end.
