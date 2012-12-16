program csvconv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, csvdocument_package, CustApp,
  CSVDocument, Math;

type

  { TCSVDoc }

  TCSVDoc = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DoConvert(iFile: string; oFile: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCSVDoc }

procedure TCSVDoc.DoRun;
var
  ErrorMsg: String;
  iFile, oFile: String;

begin
  iFile := GetOptionValue('i', 'input');
  oFile:= GetOptionValue('o', 'output');
  if (iFile = '') or (oFile = '') then
  begin
    WriteLn('Usage: csvconv -i <input file> -o <output file>');
    Terminate;
    Exit;
  end;

  if not FileExists(iFile) then
  begin
    iFile:= GetCurrentDir+iFile;
  end;
  oFile := ExtractFilePath(iFile) + ExtractFileName(oFile);
  WriteLn(Format('convert from %s to %s', [iFile, oFile]));
  DoConvert(iFile, oFile);

  Terminate;
end;

procedure TCSVDoc.DoConvert(iFile: string; oFile: string);
var
  Parser: TCSVParser;
  FileStream: TFileStream;
  SLTitle, SLText: TStringList;
  text: string;
  SLResult: TStringList;
begin
  Parser:=TCSVParser.Create;
  FileStream := TFileStream.Create(iFile, fmOpenRead+fmShareDenyWrite);

  SLTitle := TStringList.Create;
  SLText := TStringList.Create;
  SLResult := TStringList.Create;

   try
    Parser.Delimiter:=',';
    Parser.SetSource(FileStream);

    while Parser.ParseNextCell do
    begin
      if Parser.CurrentRow = 0 then
         Continue;
      if Parser.CurrentCol = 0 then
         Continue;
      case Parser.CurrentCol of
      1:SLTitle.Add('        <item>'+Parser.CurrentCellText+'</item>');
      2:
        begin
             text := Parser.CurrentCellText;
             text := StringReplace(text, #13#10, '\n', [rfIgnoreCase, rfReplaceAll]);
             text := StringReplace(text, #10, '\n', [rfIgnoreCase, rfReplaceAll]);
             text := StringReplace(text, '&', '&amp;', [rfIgnoreCase, rfReplaceAll]);
             text := StringReplace(text, '<', '&lt;', [rfIgnoreCase, rfReplaceAll]);
             text := StringReplace(text, '>', '&gt;', [rfIgnoreCase, rfReplaceAll]);
             SLText.Add('        <item>'+text+'</item>');
         end;
      end;
    end;

    SLResult.Add('<?xml version="1.0" encoding="utf-8"?>');
    SLResult.Add('<resources>');
    SLResult.Add('    <string-array name="task_title">');
    SLResult.Add(SLTitle.Text);
    SLResult.Add('    </string-array>');
    SLResult.Add('    <string-array name="task_text">');
    SLResult.Add(SLText.Text);
    SLResult.Add('    </string-array>');
    SLResult.Add('</resources>');

    SLResult.SaveToFile(oFile);

  finally
    Parser.Free;
    FileStream.Free;
    SLResult.Free;
  end;
end;

constructor TCSVDoc.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCSVDoc.Destroy;
begin
  inherited Destroy;
end;

procedure TCSVDoc.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TCSVDoc;
begin
  Application:=TCSVDoc.Create(nil);
  Application.Title:='lovingyou';
  Application.Run;
  Application.Free;
end.

