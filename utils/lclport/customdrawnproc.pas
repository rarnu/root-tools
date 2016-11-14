unit customdrawnproc;

{$mode objfpc}{$H+}
{$include customdrawndefines.inc}

interface

uses
  // rtl+ftl
  Classes, SysUtils,
  fpimage, fpcanvas, Math,
  // LazUtils
  LazFileUtils,
  IntfGraphics, lazcanvas,
  // LCL
  GraphType, LCLType, LCLProc,
  Forms, Graphics,
  InterfaceBase, LCLIntf;

type
  { TCDBitmap }

  TCDBitmap = class
  public
    Image: TLazIntfImage;
    destructor Destroy; override;
  end;

  TCDTimer = class
  public
    NativeHandle: PtrInt; // The X11 timer uses this to store the current time which is summed up to the next interval
    NativeGlobalReference: PtrInt; // Utilized in Android to store the global JNI reference
    Interval: integer;
    TimerFunc: TWSTimerProc;
  end;

// Routines for form managing (both native and non-native)


// Routines for non-native form managing
procedure InitNonNativeForms();
function GetFormCount(): Integer;

// Routines for non-native wincontrol

procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TLazCanvasImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False;
  AFreeImageOnUpdate: Boolean = True; ADataOwner: Boolean = True);


// Other routines

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
function IsValidDC(ADC: HDC): Boolean;
function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;
function IsValidBitmap(ABitmap: HBITMAP): Boolean;
function RemoveAccelChars(AStr: string): string;

// Timers list management (for platforms that need it)

procedure InitTimersList();
procedure AddTimer(ATimer: TCDTimer);
function GetTimer(AIndex: Integer): TCDTimer;
function GetTimerCount(): Integer;
function GetSmallestTimerInterval(): Integer;
procedure RemoveTimer(ATimer: TCDTimer);
function FindTimerWithNativeHandle(ANativeHandle: PtrInt): TCDTimer;

implementation

var
  // List with the Z-order of non-native forms, index=0 is the bottom-most form
  NonNativeForms: TFPList = nil;

  // List of timers
  TimersList: TFPList = nil;


procedure InitNonNativeForms();
begin
  if NonNativeForms <> nil then Exit;
  NonNativeForms := TFPList.Create;
end;


function GetFormCount: Integer;
begin
  InitNonNativeForms();
  Result := NonNativeForms.Count;
end;


// If AForceUpdate=True then it will update even if the width and height remain the same
procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TLazCanvasImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False;
  AFreeImageOnUpdate: Boolean = True; ADataOwner: Boolean = True);
var
  lRawImage: TRawImage;
  lPixelSize: Byte;
begin
  {$IFDEF VerboseCDLazCanvas}
    DebugLn(Format(':>[UpdateControlLazImageAndCanvas] Input Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
  // Check if the image needs update
  if (AImage = nil) or (AWidth <> AImage.Width) or (AHeight <> AImage.Height)
    or AForceUpdate then
  begin
    if (AImage <> nil) and AFreeImageOnUpdate then AImage.Free;
    // Free the canvas and create a new one if it is a dummy Canvas created for text metrics reading by GetDC(control)
    if (ACanvas <> nil) and ACanvas.HasNoImage then
    begin
      ACanvas.Free;
      ACanvas := nil;
    end;

    lRawImage.Init;
    case AFormat of
    clfRGB16_R5G6B5:  lRawImage.Description.Init_BPP16_R5G6B5(AWidth, AHeight);
    clfRGB24:  lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight);
    clfRGB24UpsideDown: lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB_UpsideDown(AWidth, AHeight);
    clfBGR24:  lRawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
    clfBGRA32: lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    clfRGBA32: lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(AWidth, AHeight);
    clfARGB32: lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight);
    end;

    // Now connect the pixel buffer or create one
    if AData = nil then lRawImage.CreateData(True)
    else
    begin
      case AFormat of
      clfRGB16_R5G6B5:
        lPixelSize := 2;
      clfRGB24, clfRGB24UpsideDown, clfBGR24:
        lPixelSize := 3;
      clfBGRA32, clfRGBA32, clfARGB32:
        lPixelSize := 4;
      end;

      lRawImage.Data := AData;
      lRawImage.DataSize := AWidth * lPixelSize * AHeight;
    end;

    AImage := TLazIntfImage.Create(AWidth, AHeight);
    AImage.SetRawImage(lRawImage, ADataOwner);

    if (ACanvas <> nil) then ACanvas.Free;
    ACanvas := TLazCanvas.Create(AImage);
    ACanvas.ImageFormat := AFormat;
  end;
  {$IFDEF VerboseCDLazCanvas}
    DebugLn(Format(':<[UpdateControlLazImageAndCanvas] Output Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
end;


function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
end;

function IsValidDC(ADC: HDC): Boolean;
begin
  Result := ADC <> 0;
end;

function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;
begin
  Result := AGDIObj <> 0;
end;

function IsValidBitmap(ABitmap: HBITMAP): Boolean;
begin
  Result := ABitmap <> 0;
end;

function RemoveAccelChars(AStr: string): string;
begin
  // ToDo convert && to & and keep it
  Result := StringReplace(AStr, '&', '', [rfReplaceAll]);
end;

procedure InitTimersList;
begin
  if TimersList = nil then TimersList := TFPList.Create;
end;

procedure AddTimer(ATimer: TCDTimer);
begin
  InitTimersList();
  TimersList.Add(ATimer);
end;

function GetTimer(AIndex: Integer): TCDTimer;
begin
  InitTimersList();
  Result := TCDTimer(TimersList.Items[AIndex]);
end;

function GetTimerCount: Integer;
begin
  InitTimersList();
  Result := TimersList.Count;
end;

function GetSmallestTimerInterval: Integer;
var
  i: Integer;
  lTimer: TCDTimer;
begin
  Result := High(Integer);
  for i := 0 to GetTimerCount()-1 do
  begin
    lTimer := GetTimer(i);
    Result := Min(Result, lTimer.Interval);
  end;
  if Result = High(Integer) then Result := -1;
end;

procedure RemoveTimer(ATimer: TCDTimer);
begin
  InitTimersList();
  TimersList.Remove(ATimer);
end;

function FindTimerWithNativeHandle(ANativeHandle: PtrInt): TCDTimer;
var
  lTimer: TCDTimer;
  i: Integer;
begin
  Result := nil;
  InitTimersList();
  for i := 0 to TimersList.Count - 1 do
  begin
    lTimer := TCDTimer(TimersList.Items[i]);
    if lTimer.NativeHandle = ANativeHandle then
      Exit(lTimer);
  end;
end;

{ TCDBitmap }

destructor TCDBitmap.Destroy;
begin
  if Image <> nil then Image.Free;
  inherited Destroy;
end;

end.

