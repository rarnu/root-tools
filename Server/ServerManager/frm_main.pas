unit frm_main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms,
    Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, unt_expect, unt_server, unt_config,
    frm_config;

type

    { TFormMain }

    TFormMain = class(TForm)
        btnDeleteFile: TButton;
        btnDownloadFile: TButton;
        btnSelectFile: TButton;
        btnMakeDir: TButton;
        btnUploadFile: TButton;
        gbUpload: TGroupBox;
        gbDownload: TGroupBox;
        lblCurrentFile: TLabel;
        lblCurrentDirValue: TLabel;
        lblCurrentDir: TLabel;
        ListBox1: TListBox;
        ListBox2: TListBox;
        lstFile: TListBox;
        miEnv: TMenuItem;
        miAbout: TMenuItem;
        miHelp: TMenuItem;
        miServerPassword: TMenuItem;
        miSettings: TMenuItem;
        mmMain: TMainMenu;
        pnlMain: TPanel;
        spl: TSplitter;
        procedure btnMakeDirClick(Sender: TObject);
        procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
        procedure miEnvClick(Sender: TObject);
        procedure miServerPasswordClick(Sender: TObject);
    private
        FConfig: TConfig;
    public
        { public declarations }
    end;

var
    FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.btnMakeDirClick(Sender: TObject);
var
    cmd: array of string;
    returns: TStringList;
begin
    SetLength(cmd, 3);
    cmd[0] := 'ls';
    cmd[1] := '-la';
    cmd[2] := '~/';
    BuildCommandScript(cmd, 'ls.sh', FConfig);
    ChmodScript('ls.sh', FConfig);
    returns := ExecuteCmd(ExtractFilePath(ParamStr(0)) + 'script/ls.sh');
    WriteLn(returns.Text);
    returns.Free;
    WriteLn('DONE!');
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    FConfig.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
    FConfig := TConfig.Create;
    FConfig.Load;
end;

procedure TFormMain.miEnvClick(Sender: TObject);
const
    EXPECT_PATH = '/usr/bin/expect';
begin
    if FileExists(EXPECT_PATH) then
    begin
        MessageDlg('EXPECT installed', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOK], 0);
    end
    else
    begin
        MessageDlg('EXPECT not installed.\rPlease execute the command in terminal:\rsudo apt-get install expect', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    end;
end;

procedure TFormMain.miServerPasswordClick(Sender: TObject);
begin
    with TFormConfig.Create(nil) do
    begin
        edtServerAddress.Text := FConfig.ServerAddress;
        edtServerUser.Text := FConfig.UserName;
        edtServerPassword.Text := FConfig.UserPassword;
        edtRootPassword.Text := FConfig.RootPassword;
        if ShowModal = mrOk then
        begin
            FConfig.ServerAddress := edtServerAddress.Text;
            FConfig.UserName := edtServerUser.Text;
            FConfig.UserPassword := edtServerPassword.Text;
            FConfig.RootPassword := edtRootPassword.Text;
            FConfig.Save;
        end;
        Free;
    end;
end;

end.

