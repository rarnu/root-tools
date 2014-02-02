unit frm_preview_generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, strutils, IniFiles, LCLIntf, frm_user_info, unt_command;

type

  { TFormMain }

  TFormMain = class(TForm)
    btnChooseFile: TButton;
    btnChooseFont: TButton;
    btnGenerate: TButton;
    btnShowPreview: TButton;
    btnUpload: TButton;
    btnUser: TButton;
    edtFontName: TEdit;
    edtIsTop: TEdit;
    edtSavePath: TEdit;
    fd: TFontDialog;
    lblChooseFont: TLabel;
    lblFontName: TLabel;
    lblFontFile: TLabel;
    lblIsTop: TLabel;
    lblPreview: TLabel;
    lblSavePath: TLabel;
    od: TOpenDialog;
    pnlOperation: TPanel;
    pnlFile: TPanel;
    pnlFont: TPanel;
    pnlUser: TPanel;
    sdd: TSelectDirectoryDialog;
    procedure btnChooseFileClick(Sender: TObject);
    procedure btnChooseFontClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnShowPreviewClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
    procedure btnUserClick(Sender: TObject);
    procedure edtSavePathChange(Sender: TObject);
    procedure edtSavePathDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlOperationClick(Sender: TObject);
    procedure pnlFileClick(Sender: TObject);
    procedure pnlUserClick(Sender: TObject);
  private

    FIniFile: TIniFile;

    procedure GeneratePreview(AFontName: string; AFont: TFont);
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

const
  SEC_CFG = 'config';
  KEY_SAVE_PATH = 'save_path';
  KEY_USER_ACCOUNT = 'user_account';
  KEY_USER_PASSWORD = 'user_password';

procedure TFormMain.FormCreate(Sender: TObject);
var
  savePath: string;
begin
  FIniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.cfg'));
  savePath := FIniFile.ReadString(SEC_CFG, KEY_SAVE_PATH, '');
  if savePath = '' then
  begin
    savePath := GetEnvironmentVariable('HOME') + '/Pictures/';
    FIniFile.WriteString(SEC_CFG, KEY_SAVE_PATH, savePath);
  end;
  edtSavePath.Text := savePath;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FIniFile.Free;
end;

procedure TFormMain.pnlOperationClick(Sender: TObject);
begin

end;

procedure TFormMain.pnlFileClick(Sender: TObject);
begin

end;

procedure TFormMain.pnlUserClick(Sender: TObject);
begin

end;

procedure TFormMain.GeneratePreview(AFontName: string; AFont: TFont);
const
  STR_PREVIEW = '掌优RootTools';
var
  strHome: string;
  strPath: string;
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  sw: integer;
  sh: integer;
  outX: integer;
  outY: integer;
begin
  bmp := TBitmap.Create;
  bmp.SetSize(160, 80);
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect(Rect(0, 0, 160, 80));

  bmp.Canvas.Font.Name := AFont.Name;
  bmp.Canvas.Font.Size := 14;

  sw := bmp.Canvas.GetTextWidth(STR_PREVIEW);
  sh := bmp.Canvas.GetTextHeight(STR_PREVIEW);

  outX := (160 - sw) div 2;
  outY := (80 - sh) div 2;

  bmp.Canvas.TextOut(outX, outY, STR_PREVIEW);

  strHome := edtSavePath.Text;
  if not DirectoryExists(strHome) then
  begin
    ForceDirectory(strHome);
  end;
  strPath := strHome + AFontName + '.png';
  png := TPortableNetworkGraphic.Create;
  png.Assign(bmp);
  png.SaveToFile(strPath);
  png.Free;
  bmp.Free;
end;

procedure TFormMain.btnChooseFontClick(Sender: TObject);
begin
  if fd.Execute then
  begin
    btnChooseFont.Caption := fd.Font.Name;
    btnChooseFont.Font.Size := 14;
    btnChooseFont.Font.Name := fd.Font.Name;
    edtFontName.Caption := fd.Font.Name;
  end;
end;

procedure TFormMain.btnChooseFileClick(Sender: TObject);
begin
  if od.Execute then
  begin
    btnChooseFile.Caption := ExtractFileName(od.FileName);
  end;
end;

procedure TFormMain.btnGenerateClick(Sender: TObject);
begin
  if edtFontName.Text <> '' then
  begin
    GeneratePreview(edtFontName.Text, fd.Font);
  end
  else
  begin
    ShowMessage('Font Name cannot be empty');
  end;
end;

procedure TFormMain.btnShowPreviewClick(Sender: TObject);
var
  path: string;
begin
  path := edtSavePath.Text + edtFontName.Text + '.png';
  LCLIntf.OpenDocument(path);
end;

procedure TFormMain.btnUploadClick(Sender: TObject);
var
  shcmd: string;
  fontFile: string;
  fontName: string;
  previewFile: string;
  isTop: integer;
  userName: string;
  userPasword: string;
  shparam: string;
  cmd: string;
begin
  fontFile := od.FileName;
  fontName := edtFontName.Text;
  previewFile := edtSavePath.Text + fontName + '.png';
  isTop := StrToIntDef(edtIsTop.Text, 0);
  userName := FIniFile.ReadString(SEC_CFG, KEY_USER_ACCOUNT, '');
  userPasword := FIniFile.ReadString(SEC_CFG, KEY_USER_PASSWORD, '');

  shcmd := ExtractFilePath(ParamStr(0)) + 'uploadfont.sh ';
  shparam := Format('"%s" "%s" "%s" "%d" "%s" "%s"',
    [fontName, fontFile, previewFile, isTop, userName, userPasword]);
  cmd := shcmd + shparam;
  WriteLn(cmd);
  // LCLIntf.OpenDocument(cmd);
  ExecuteCommandT(cmd, ExtractFilePath(ParamStr(0)));
end;

procedure TFormMain.btnUserClick(Sender: TObject);
begin
  with TFormUserInfo.Create(nil) do
  begin
    edtAccount.Text := FIniFile.ReadString(SEC_CFG, KEY_USER_ACCOUNT, '');
    edtpassword.Text := FIniFile.ReadString(SEC_CFG, KEY_USER_PASSWORD, '');
    if ShowModal = mrOk then
    begin
      FIniFile.WriteString(SEC_CFG, KEY_USER_ACCOUNT, edtAccount.Text);
      FIniFile.WriteString(SEC_CFG, KEY_USER_PASSWORD, edtpassword.Text);
    end;
    Free;
  end;
end;

procedure TFormMain.edtSavePathChange(Sender: TObject);
begin

end;

procedure TFormMain.edtSavePathDblClick(Sender: TObject);
var
  path: string;
begin
  sdd.FileName := edtSavePath.Text;
  if sdd.Execute then
  begin
    path := sdd.FileName;
    if not AnsiEndsText('/', path) then
    begin
      path := path + '/';
    end;
    edtSavePath.Text := path;
    FIniFile.WriteString(SEC_CFG, KEY_SAVE_PATH, path);
  end;
end;

end.
