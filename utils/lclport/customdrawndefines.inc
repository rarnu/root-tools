
{$ifdef Darwin}
{$modeswitch objectivec1}{$H+}// modeswitch seams to deactivate H+
{$endif}

// For now default to Android for arm-linux,
// because LCL-CustomDrawn is our main Android widgetset.
// Remove this when Android gets it's own target
{$ifdef UNIX}{$ifndef Darwin}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}{$endif}


// Check if a backend which can be utilized in multiple-systems is already defined
{$if defined(CD_X11) or defined(CD_Android)}

{$else}
  // Choosing the default backend
  {$ifdef Windows}
    {$define CD_WINDOWS}
  {$endif}
  {$ifdef UNIX}
    {$ifdef Darwin}
      {$define CD_Cocoa}
    {$else}
      {$ifdef Android}
        {$define CD_Android}
      {$else}
        {$define CD_X11}
      {$endif}
    {$endif}
  {$endif}
{$endif}

// Default options for various backends
{$if defined(CD_Android)}

  {$define CD_HasNativeSelectItemDialog}
{$endif}
{$if defined(CD_WINDOWS)}

{$endif}

// ==================
// Other options
// ==================

// ==================
// Android options
// ==================
{.$define VerboseCDShowSelectItemDialog}

// ==================
// X11 options
// ==================
{$define CD_X11_NewNativePaint}
{$define CD_X11_UseLCL_MainLoop}
{$define CD_X11_UseNewTimer}
{$define CD_X11_SmartPaint}

// ==================
// X11 experimental options
// ==================
{$ifdef CD_X11}
// To be able to use TThreads, UseCThreads must be defined in the application!
{ $ifdef UseCTthreads}
 {$ifdef CD_X11_UseNewTimer}
   {.$define TimerUseCThreads}
 {$endif}
{ $endif}
{$endif}

// ==================
// Debug options
// ==================
{.$define VerboseCDPaintProfiler}
{.$define Verbose_CD_X11_Timer}
{.$define CD_Debug_TTF}
{.$define VerboseCDDrawing}
{.$define VerboseCDBitmap}
{.$define VerboseCDForms}
{.$define VerboseCDX11WinAPI}
{.$define VerboseCDEvents}
{.$define VerboseCDAccessibility}
{.$define VerboseCDClipboard}
{.$define VerboseCDFocus}
{.$define VerboseCDKeyInput}
{.$define VerboseCDInjectedControlNames}

