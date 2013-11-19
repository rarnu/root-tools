unit unt_changer;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, strutils;

procedure ChangeFile(AFileName: string);

implementation

function IsMatch(B: array of byte; index: int64; str: string; strModified: string): integer;
var
    bstr: string;
    i: int64;
begin
    Result := 0;
    bstr := '';
    for i := index to (index + Length(str) - 1) do
    begin
        bstr := bstr + char(B[i]);
    end;
    if bstr = str then
    begin
        Result := 1;
        WriteLn('Result:' + IntToStr(Result));
    end
    else if bstr = strModified then
    begin
        Result := 2;
        WriteLn('Result:' + IntToStr(Result));
    end;

end;

function SeekKeywordPosition(B: array of byte): int64;
const
    KEY_WORD = 'com/android/internal/**';
    KEY_WORD_MODIFIED = 'com/android/internax/**';
var
    i: int64;
    matchRet: integer;
begin
    Result := -1;
    for i := 0 to (Length(B) - Length(KEY_WORD)) do
    begin
        matchRet := IsMatch(B, i, KEY_WORD, KEY_WORD_MODIFIED);
        if matchRet = 1 then
        begin
            Result := i;
            Break;
        end
        else if matchRet = 2 then
        begin
            Result := -2;
            Break;
        end;
    end;
end;


procedure ChangeFile(AFileName: string);
var
    FS: TFileStream;
    B: array of byte;
    i: int64;
    idx: int64;
begin
    FS := TFileStream.Create(AFileName, fmOpenReadWrite);
    SetLength(B, FS.Size);
    FS.Seek(0, TSeekOrigin.soBeginning);
    for i := 0 to FS.Size - 1 do
    begin
        B[i] := FS.ReadByte;
    end;

    idx := SeekKeywordPosition(B);
    if idx = -2 then
    begin
        WriteLn('File already patched.');
    end
    else
    begin
        if idx <> -1 then
        begin
            idx := idx + 19;
            FS.Seek(idx, TSeekOrigin.soBeginning);
            FS.WriteByte(Ord('x'));
        end
        else
        begin
            WriteLn('File error');
        end;
    end;

    FS.Free;
end;

end.



