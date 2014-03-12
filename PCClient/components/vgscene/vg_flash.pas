unit vg_flash;

{$I vg_define.inc}

interface

uses
  {$IFDEF FPC}
  LCLType, LMessages, LResources,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, Controls, SysUtils, vg_scene, fe_flashplayer;

type

  TvgFlashPlayer = class(TvgVisualObject, IfeFlash)
  private
    FBuffer: TvgBitmap;
    FDriver: TfeFlashDriver;
    FMovie: WideString;
    FPlugin: TfePlugin;
    FOnFlashCall: TfeFlashCallEvent;
    FOnFSCommand: TfeFSCommandEvent;
    procedure SetMovie(const Value: WideString);
    procedure SetPlugin(const Value: TfePlugin);
    function GetPause: boolean;
    function GetSkin: string;
    function GetVolume: integer;
    procedure SetDisableFlashCursor(const Value: boolean);
    procedure SetDisableFlashMenu(const Value: boolean);
    procedure SetPause(const Value: boolean);
    procedure SetSkin(const Value: string);
    procedure SetVolume(const Value: integer);
    function GetDisableFlashCursor: boolean;
    function GetDisableFlashMenu: boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DoPaint(Sender: TObject; const ARect: TRect);
    procedure DoEraseBackground(Sender: TObject; const ARect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { IfeFlash }
    procedure LoadMovie(const AUrl: WideString);
    procedure LoadMovieFromStream(const AStream: TStream);
    function GetVariable(const name: WideString): WideString;
    procedure SetVariable(const name: WideString; const value: WideString);
    function CallFunction(const request: WideString): WideString;
    procedure SetReturnValue(const returnValue: WideString);
    procedure Play;
    procedure StopPlay;
    procedure Rewind;
    procedure Forward;
    procedure Back;
    function GetLoop: boolean;
    function GetIsPlaying: boolean;
    function GetCurrentFrame: integer;
    function GetTotalFrames: integer;
    procedure SetLoop(const Value: boolean);
    procedure SetCurrentFrame(const Value: integer);
    property IsPlaying: boolean read GetIsPlaying;
    property Loop: boolean read GetLoop write SetLoop;
    property CurrentFrame: integer read GetCurrentFrame write SetCurrentFrame;
    property TotalFrames: integer read GetTotalFrames;
  published
    property DisableFlashCursor: boolean read GetDisableFlashCursor write SetDisableFlashCursor default false;
    property DisableFlashMenu: boolean read GetDisableFlashMenu write SetDisableFlashMenu default true;
    property Movie: WideString read FMovie write SetMovie;
    property Plugin: TfePlugin read FPlugin write SetPlugin default feDefault;
    property Skin: string read GetSkin write SetSkin;
    property Pause: boolean read GetPause write SetPause;
    property Volume: integer read GetVolume write SetVolume;
    property OnFSCommand: TfeFSCommandEvent read FOnFSCommand write FOnFSCommand;
    property OnFlashCall: TfeFlashCallEvent read FOnFlashCall write FOnFlashCall;
  end;

procedure Register;

implementation {===============================================================}

procedure Register;
begin
  RegisterNoIcon([TvgFlashPlayer]);
end;

{ TvgFlashPlayer }

constructor TvgFlashPlayer.Create(AOwner: TComponent);
begin
  inherited;
  CheckFlashPlugins; // need to call after Application created
  CanFocused := true;
  FBuffer := TvgBitmap.Create(round(Width), round(Height));
  ChangePlugin(FDriver, FPlugin, Movie, round(Width), round(Height), 0,
    nil, DoPaint, DoEraseBackground, FOnFSCommand, FOnFlashCall);
end;

destructor TvgFlashPlayer.Destroy;
begin
  FreeAndNil(FDriver);
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TvgFlashPlayer.Loaded;
begin
  inherited;
  if FMovie <> '' then
    FDriver.LoadMovie(FMovie);
end;

procedure TvgFlashPlayer.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
  if KeyChar <> #0 then
    FDriver.KeyPress(KeyChar, Shift)
  else
    FDriver.KeyDown(Key, Shift)
end;

procedure TvgFlashPlayer.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
  if KeyChar = #0 then
    FDriver.KeyUp(Key, Shift)
end;

procedure TvgFlashPlayer.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  FDriver.MouseDown(Button, Shift, trunc(X), trunc(Y));
end;

procedure TvgFlashPlayer.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
  FDriver.MouseMove(Shift, trunc(X), trunc(Y));
end;

procedure TvgFlashPlayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  inherited;
  FDriver.MouseUp(Button, Shift, trunc(X), trunc(Y));
end;

procedure TvgFlashPlayer.MouseWheel(Shift: TShiftState;
  WheelDelta: integer; var Handled: boolean);
begin
  inherited;
  FDriver.MouseWheel(Shift, 0, 0, WheelDelta, Handled);
end;

procedure TvgFlashPlayer.DoEraseBackground(Sender: TObject; const ARect: TRect);
var
  R: TRect;
  SaveIndex: integer;
begin
  if FDriver = nil then Exit;
  if FDriver.BufferBits <> nil then
    FillLongwordRect(FDriver.BufferBits, FDriver.Width, FDriver.Height, ARect.Left, ARect.Top, ARect.Right,
      ARect.Bottom, 0);
end;

procedure TvgFlashPlayer.DoPaint(Sender: TObject; const ARect: TRect);
begin
  if (FBuffer.Width <> Round(Width)) or (FBuffer.Height <> Round(Height)) then
    FBuffer.SetSize(round(Width), round(Height));
  if (FDriver.Width <> FBuffer.Width) or (FDriver.Height <> FBuffer.Height) then
    FDriver.Resize(FBuffer.Width, FBuffer.Height);
  MoveLongword(FDriver.BufferBits, FBuffer.StartLine, FBuffer.Width * FBuffer.Height);
  Repaint;
end;

procedure TvgFlashPlayer.Paint;
var
  S: cardinal;
begin
  S := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(LocalRect);
  Canvas.DrawBitmap(FBuffer, vgRect(0, 0, FBuffer.Width, FBuffer.Height), vgRect(0, 0, FBuffer.Width, FBuffer.Height), AbsoluteOpacity, true);
  Canvas.RestoreCanvas(S);
end;

procedure TvgFlashPlayer.SetMovie(const Value: WideString);
begin
  FMovie := Value;
  if not (csLoading in ComponentState) then
    FDriver.LoadMovie(FMovie);
end;

procedure TvgFlashPlayer.SetPlugin(const Value: TfePlugin);
begin
  if Plugin <> Value then
  begin
    FPlugin := Value;
    ChangePlugin(FDriver, FPlugin, Movie, round(Width), round(Height), 0,
      nil, DoPaint, DoEraseBackground, FOnFSCommand, FOnFlashCall);
  end;
end;

{ IfeFlash }

procedure TvgFlashPlayer.LoadMovie(const AUrl: WideString);
begin
  FMovie := AUrl;
  if not (csLoading in ComponentState) then
    FDriver.LoadMovie(FMovie);
  if csDesigning in ComponentState then
    FDriver.Pause := true;
end;

procedure TvgFlashPlayer.LoadMovieFromStream(const AStream: TStream);
begin
  FMovie := '';
  if not (csLoading in ComponentState) then
    FDriver.LoadMovieFromStream(AStream);
  if csDesigning in ComponentState then
    FDriver.Pause := true;
end;

function TvgFlashPlayer.GetVariable(const name: WideString): WideString;
begin
  Result := FDriver.GetVariable(name)
end;

procedure TvgFlashPlayer.SetVariable(const name: WideString; const value: WideString);
begin
  FDriver.SetVariable(name, value);
end;

function TvgFlashPlayer.CallFunction(const request: WideString): WideString;
begin
  FDriver.CallFunction(request);
end;

procedure TvgFlashPlayer.SetReturnValue(const returnValue: WideString);
begin
  FDriver.CallFunction(returnValue);
end;

procedure TvgFlashPlayer.Play;
begin
  FDriver.Play;
end;

procedure TvgFlashPlayer.StopPlay;
begin
  FDriver.StopPlay;
end;

procedure TvgFlashPlayer.Rewind;
begin
  FDriver.Rewind;
end;

procedure TvgFlashPlayer.Forward;
begin
  FDriver.Forward;
end;

procedure TvgFlashPlayer.Back;
begin
  FDriver.Back;
end;

function TvgFlashPlayer.GetLoop: boolean;
begin
  Result := FDriver.GetLoop;
end;

function TvgFlashPlayer.GetIsPlaying: boolean;
begin
  Result := FDriver.GetIsPlaying;
end;

function TvgFlashPlayer.GetCurrentFrame: integer;
begin
  Result := FDriver.GetCurrentFrame;
end;

function TvgFlashPlayer.GetTotalFrames: integer;
begin
  Result := FDriver.GetTotalFrames;
end;

procedure TvgFlashPlayer.SetLoop(const Value: boolean);
begin
  FDriver.SetLoop(Value);
end;

procedure TvgFlashPlayer.SetCurrentFrame(const Value: integer);
begin
  FDriver.SetCurrentFrame(Value);
end;

function TvgFlashPlayer.GetPause: boolean;
begin
  Result := FDriver.Pause;
end;

function TvgFlashPlayer.GetSkin: string;
begin
  Result := FDriver.Skin
end;

function TvgFlashPlayer.GetVolume: integer;
begin
  Result := FDriver.Volume;
end;

procedure TvgFlashPlayer.SetDisableFlashCursor(const Value: boolean);
begin
  FDriver.DisableFlashCursor := Value;
end;

procedure TvgFlashPlayer.SetDisableFlashMenu(const Value: boolean);
begin
  FDriver.DisableFlashMenu := Value;
end;

procedure TvgFlashPlayer.SetPause(const Value: boolean);
begin
  FDriver.Pause := Value
end;

procedure TvgFlashPlayer.SetSkin(const Value: string);
begin
  FDriver.Skin := Value
end;

procedure TvgFlashPlayer.SetVolume(const Value: integer);
begin
  FDriver.Volume := Value
end;

function TvgFlashPlayer.GetDisableFlashCursor: boolean;
begin
  Result := FDriver.DisableFlashCursor;
end;

function TvgFlashPlayer.GetDisableFlashMenu: boolean;
begin
  Result := FDriver.DisableFlashMenu;
end;

initialization
  RegisterVGObjects('Flash', [TvgFlashPlayer]);
end.


