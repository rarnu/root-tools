program gspw;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces,
  Classes,
  SysUtils,
  CustApp,
  Graphics;

type

  { TGetStringWidth }

  TGetStringWidth = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TGetStringWidth }

  procedure TGetStringWidth.DoRun;
  var
    fn: string;
    fs: integer;
    s: string;
    bmp: TBitmap;
    sw: integer;
    sh: integer;
    e: string;
  begin
    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('fn') then
    begin
      fn := GetOptionValue('fn');
    end;

    if HasOption('fs') then
    begin
      fs := StrToIntDef(GetOptionValue('fs'), -1);
    end;

    if HasOption('s') then
    begin
      s := GetOptionValue('s');
    end;

    if HasOption('e') then
    begin
      e := GetOptionValue('e');
    end;

    if (fn = '') or (fs = -1) or (s = '') or (e = '') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if (e <> 'w') and (e <> 'h') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    bmp := TBitmap.Create;

    bmp.Canvas.Font.Name := fn;
    bmp.Canvas.Font.Size := fs;
    sw := bmp.Canvas.GetTextWidth(s);
    sh := bmp.Canvas.GetTextHeight(s);
    if e = 'w' then
    begin
      Write(IntToStr(sw));
    end
    else if e = 'h' then
    begin
      Write(IntToStr(sh));
    end;

    bmp.Free;

    Terminate;
  end;

  constructor TGetStringWidth.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TGetStringWidth.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TGetStringWidth.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage:  -h');
    WriteLn('  -fn <font name>');
    WriteLn('  -fs <font size>');
    WriteLn('  -s <string to be tested>');
    WriteLn('  -e <export w:width, h:height>');
  end;

var
  Application: TGetStringWidth;

{$R *.res}

begin
  Application := TGetStringWidth.Create(nil);
  Application.Run;
  Application.Free;
end.

