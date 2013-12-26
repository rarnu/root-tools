unit unt_recommend_upload;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, unt_command;

procedure DoUpload(AApkFile: string; AIconFile: string; AUnixName: string; AMode: integer; AAccount: string; APassword: string);

implementation

procedure DoUploadFiles(AApkFile: string; AIconFile: string; AAccount: string; APassword: string);
var
    path: string;
    cmd: string;
    ret: TStringList;

begin
    path := ExtractFilePath(ParamStr(0));
    cmd := path + 'upload.sh ';
    cmd := cmd + AAccount + ' ' + APassword + ' ' + AApkFile + ' package';
    ret := ExecuteCommandF(cmd, path);
    WriteLn('upload apk: ' + ret.Text);
    cmd := path + 'upload.sh ';
    cmd := cmd + AAccount + ' ' + APassword + ' ' + AIconFile + ' icon';
    ret := ExecuteCommandF(cmd, path);
    WriteLn('upload icon: ' + ret.Text);

end;

procedure DoAdd(AApkFile: string; AIconFile: string; AUnixName: string; APackageName: string; ALabelName: string; AAccount: string; APassword: string);
var
    path: string;
    cmd: string;
    ret: TStringList;
begin
    path := ExtractFilePath(ParamStr(0));
    DoUploadFiles(AApkFile, AIconFile, AAccount, APassword);
    cmd := 'http://rarnu.7thgen.info/root_tools/admin/upload.php?';
    cmd := cmd + 'name=' + ALabelName;
    cmd := cmd + '&package_name=' + APackageName;
    cmd := cmd + '&unix_name=' + AUnixName;
    cmd := cmd + '&icon=' + ExtractFileName(AIconFile);
    cmd := cmd + '&apk=' + ExtractFileName(AApkFile);
    cmd := cmd + '&mode=0';
    ret := ExecuteCommandF('curl ' + cmd, path);
    WriteLn('query: ' + ret.Text);
end;

procedure DoUpdate(AApkFile: string; AIconFile: string; APackageName: string; ALabelName: string; AAccount: string; APassword: string);
var
    path: string;
    cmd: string;
    ret: TStringList;
begin
    path := ExtractFilePath(ParamStr(0));
    DoUploadFiles(AApkFile, AIconFile, AAccount, APassword);
    cmd := 'http://rarnu.7thgen.info/root_tools/admin/upload.php?';
    cmd := cmd + 'name=' + ALabelName;
    cmd := cmd + '&package_name=' + APackageName;
    cmd := cmd + '&icon=' + ExtractFileName(AIconFile);
    cmd := cmd + '&apk=' + ExtractFileName(AApkFile);
    cmd := cmd + '&mode=1';
    ret := ExecuteCommandF('curl ' + cmd, path);
    WriteLn('query: ' + ret.Text);
end;

function extractPackageName(AStr: string): string;
begin
    AStr := StringReplace(AStr, 'package: name=', '', [rfIgnoreCase, rfReplaceAll]);
    AStr := StringReplace(AStr, '''', '', [rfReplaceAll, rfIgnoreCase]);
    AStr := Copy(AStr, 1, Pos(' ', AStr) - 1);
    Result := AStr;
end;

function extractLabelName(AStr: string): string;
begin
    AStr := StringReplace(AStr, 'application-label:', '', [rfIgnoreCase, rfReplaceAll]);
    AStr := StringReplace(AStr, '''', '', [rfIgnoreCase, rfReplaceAll]);
    AStr := Trim(AStr);
    Result := AStr;
end;

function extractLabelNameCn(AStr: string): string;
begin
    AStr := StringReplace(AStr, 'application-label-zh_CN:', '', [rfIgnoreCase, rfReplaceAll]);
    AStr := StringReplace(AStr, '''', '', [rfIgnoreCase, rfReplaceAll]);
    AStr := Trim(AStr);
    Result := AStr;
end;

procedure DoUpload(AApkFile: string; AIconFile: string; AUnixName: string; AMode: integer; AAccount: string; APassword: string);
var
    apkInfo: TStringList;
    path: string;
    packageName: string = '';
    labelName: string = '';
    labelNameCn: string = '';
    labelNameFinal: string;
    newIconFile: string;
    newApkFile: string;
    i: integer;
begin

    path := ExtractFilePath(ParamStr(0));
    apkInfo := ExecuteCommandF('aapt d badging ' + AApkFile, path);
    for i := 0 to apkInfo.Count - 1 do
    begin

        if (pos('package: name=', apkInfo[i]) > 0) and (packageName = '') then
        begin
            packageName := extractPackageName(apkInfo[i]);
        end;
        if (pos('application-label-zh_CN:', apkInfo[i]) > 0) and (labelNameCn = '') then
        begin
            labelNameCn := extractLabelNameCn(apkInfo[i]);
        end;
        if (pos('application-label:', apkInfo[i]) > 0) and (labelName = '') then
        begin
            labelName := extractLabelName(apkInfo[i]);
        end;
    end;
    WriteLn(packageName);

    newApkFile := ExtractFilePath(AApkFile) + packageName + '.apk';
    RenameFile(AApkFile, newApkFile);
    AApkFile := newApkFile;

    newIconFile := ExtractFilePath(AIconFile) + packageName + '.png';
    RenameFile(AIconFile, newIconFile);
    AIconFile := newIconFile;

    labelNameFinal := labelName;
    if labelNameCn <> '' then
    begin
        labelNameFinal := labelNameCn;
    end;
    if (AMode = 0) then
    begin
        DoAdd(AApkFile, AIconFile, AUnixName, packageName, labelNameFinal, AAccount, APassword);
    end
    else if (AMode = 1) then
    begin
        DoUpdate(AApkFile, AIconFile, packageName, labelNameFinal, AAccount, APassword);
    end;
end;

end.

