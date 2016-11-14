unit customdrawn_androidproc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, ctypes,
  // Android headers

  // Custom Drawn Canvas
  IntfGraphics, lazcanvas,
  //
  GraphType, Graphics, LCLType, LCLProc;

function FPColorToAndroidColor(AValue: TFPColor): Integer;

implementation

// Android color is in the format: Alpha-Red-Green-Blue
// It uses 8-bits per channel. $FF means alpha opaque
// See http://developer.android.com/reference/android/graphics/Color.html
function FPColorToAndroidColor(AValue: TFPColor): Integer;
begin
  Result:= $FF000000 or ((AValue.Blue shr 8) and $ff)
       or (AValue.Green and $ff00)
       or ((AValue.Red shl 8) and $ff0000);
end;

end.

