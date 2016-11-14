{                 ---------------------------------------------------
                  customdrawnthemes.pas  -  CustomDrawn Theme support
                  ---------------------------------------------------

  See Themes.pas for licencing and other further information.
}
unit customdrawnthemes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema, lazcanvas;
  
type
  { TCDThemeServices }

  TCDThemeServices = class(TThemeServices)
  private
  protected
(*
    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    procedure InternalDrawParentBackground({%H-}Window: HWND; {%H-}Target: HDC; {%H-}Bounds: PRect); override;
*)
(*    function DrawButtonElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; {%H-}ClipRect: PRect): TRect;
    function DrawComboBoxElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; {%H-}ClipRect: PRect): TRect;
    function DrawHeaderElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; {%H-}ClipRect: PRect): TRect;
    function DrawRebarElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; {%H-}ClipRect: PRect): TRect;*)
    function DrawTreeviewElement(DC: TLazCanvas; Details: TThemedElementDetails; R: TRect; {%H-}ClipRect: PRect): TRect;
(*    function DrawWindowElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; {%H-}ClipRect: PRect): TRect;
*)
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
(*
    procedure DrawEdge({%H-}DC: HDC; {%H-}Details: TThemedElementDetails; const {%H-}R: TRect; {%H-}Edge, {%H-}Flags: Cardinal; {%H-}AContentRect: PRect); override;
    procedure DrawIcon({%H-}DC: HDC; {%H-}Details: TThemedElementDetails; const {%H-}R: TRect; {%H-}himl: HIMAGELIST; {%H-}Index: Integer); override;
    procedure DrawText({%H-}DC: HDC; {%H-}Details: TThemedElementDetails; const {%H-}S: String; {%H-}R: TRect; {%H-}Flags, {%H-}Flags2: Cardinal); override;

    function ContentRect({%H-}DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts({%H-}Details: TThemedElementDetails): Boolean; override;
    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    function GetOption(AOption: TThemeOption): Integer; override;
*)
  end;

implementation

{ TCDThemeServices }

(*
{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawComboBoxElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a ComboBox element with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawComboBoxElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  BoundsRect: HIRect;
  NewHeight: Integer;
  BtnWidth: Integer;
begin
  ButtonDrawInfo.version := 0;
  ButtonDrawInfo.State := GetDrawState(Details);
  ButtonDrawInfo.value := kThemeButtonOn;
  ButtonDrawInfo.adornment := kThemeAdornmentNone;

  BoundsRect := RectToCGRect(R);

  NewHeight := GetCarbonThemeMetric(kThemeMetricPopupButtonHeight);
  BtnWidth := GetCarbonThemeMetric(kThemeMetricComboBoxLargeDisclosureWidth);
  ButtonDrawInfo.kind := kThemeComboBox;
  if BoundsRect.size.height < NewHeight then begin
    NewHeight := GetCarbonThemeMetric(kThemeMetricSmallPopupButtonHeight);
    BtnWidth := GetCarbonThemeMetric(kThemeMetricComboBoxSmallDisclosureWidth);
    ButtonDrawInfo.kind := kThemeComboBoxSmall;
    end;
  if BoundsRect.size.height < NewHeight then begin
    NewHeight := GetCarbonThemeMetric(kThemeMetricMiniPopupButtonHeight);
    BtnWidth := GetCarbonThemeMetric(kThemeMetricComboBoxMiniDisclosureWidth);
    ButtonDrawInfo.kind := kThemeComboBoxMini;
    end;

  OSError(
    HIThemeDrawButton(BoundsRect, ButtonDrawInfo, DC.CGContext,
      kHIThemeOrientationNormal, nil),
    Self, 'DrawComboBoxElement', 'HIThemeDrawButton');

  BoundsRect.size.height := NewHeight + 1;
  BoundsRect.size.width := BoundsRect.size.width - BtnWidth;
  Result := CGRectToRect(BoundsRect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawButtonElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a button element with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawButtonElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
const
  ButtonMap: array[BP_PUSHBUTTON..BP_USERBUTTON] of ThemeButtonKind =
  (
{BP_PUSHBUTTON } kThemeRoundedBevelButton,
{BP_RADIOBUTTON} kThemeRadioButton,
{BP_CHECKBOX   } kThemeCheckBox,
{BP_GROUPBOX   } kHIThemeGroupBoxKindPrimary, // ??
{BP_USERBUTTON } kThemeRoundedBevelButton
  );
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  // we can do so because GroupDrawIndo have common fields with ButtonDrawInfo
  GroupDrawInfo: HIThemeGroupBoxDrawInfo absolute ButtonDrawInfo; 
  LabelRect: HIRect;
begin
  ButtonDrawInfo.version := 0;
  ButtonDrawInfo.State := GetDrawState(Details);
  ButtonDrawInfo.kind := ButtonMap[Details.Part];
  if IsMixed(Details) then
    ButtonDrawInfo.value := kThemeButtonMixed
  else
  if IsChecked(Details) then
    ButtonDrawInfo.value := kThemeButtonOn
  else
    ButtonDrawInfo.value := kThemeButtonOff;
  ButtonDrawInfo.adornment := kThemeAdornmentNone;

  LabelRect := RectToCGRect(R);

  if Details.Part = BP_GROUPBOX then
    OSError(
      HIThemeDrawGroupBox(LabelRect, GroupDrawInfo, DC.CGContext,
          kHIThemeOrientationNormal),
      Self, 'DrawButtonElement', 'HIThemeDrawGroupBox')
  else
    OSError(
      HIThemeDrawButton(LabelRect, ButtonDrawInfo, DC.CGContext,
        kHIThemeOrientationNormal, @LabelRect),
      Self, 'DrawButtonElement', 'HIThemeDrawButton');
      
  Result := CGRectToRect(LabelRect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawHeaderElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a header (THeaderControl same as ListView header) element with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawHeaderElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  PaintRect: HIRect;
begin
  ButtonDrawInfo.version := 0;
  ButtonDrawInfo.State := GetDrawState(Details);
  ButtonDrawInfo.kind := kThemeBevelButtonSmall;//kThemeListHeaderButton;
  ButtonDrawInfo.adornment := kThemeAdornmentNone;

  PaintRect := RectToCGRect(R);

  OSError(
    HIThemeDrawButton(PaintRect, ButtonDrawInfo, DC.CGContext,
      kHIThemeOrientationNormal, @PaintRect),
    Self, 'DrawButtonElement', 'HIThemeDrawButton');

  Result := CGRectToRect(PaintRect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawRebarElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a rebar element (splitter) with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawRebarElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  SplitterInfo: HIThemeSplitterDrawInfo;
  PlacardInfo: HIThemePlacardDrawInfo;
  ARect: HIRect;
const
  SName = 'DrawRebarElement';
begin
  ARect := RectToCGRect(R);
  if Details.Part in [RP_GRIPPER, RP_GRIPPERVERT] then
  begin
    SplitterInfo.version := 0;
    SplitterInfo.State := kThemeStateActive;
    SplitterInfo.adornment := kHiThemeSplitterAdornmentNone;
    
    OSError(
      HIThemeDrawPaneSplitter(ARect, SplitterInfo, DC.CGContext, kHIThemeOrientationNormal),
      Self, SName, 'HIThemeDrawPaneSplitter');
  end
  else
  if Details.Part = RP_BAND then
  begin
    PlacardInfo.version := 0;
    PlacardInfo.State := GetDrawState(Details);

    OSError(
      HIThemeDrawPlacard(ARect, PlacardInfo, DC.CGContext, kHIThemeOrientationNormal),
      Self, SName, 'HIThemeDrawPlacard');
  end;
  
  Result := CGRectToRect(ARect);
end;*)

{------------------------------------------------------------------------------
  Method:  TCDThemeServices.DrawToolBarElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a tool bar element with native Carbon look
 ------------------------------------------------------------------------------}

function TCDThemeServices.DrawTreeviewElement(DC: TLazCanvas;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
{var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  LabelRect: HIRect;
  lBrush: TCocoaBrush;
  lOldBrush: HBRUSH;
  lPen: TCocoaPen;
  lOldPen: HGDIOBJ;
  lColor: NSColor;
  lPoints: array of TPoint;}
begin
 (* case Details.Part of
    TVP_TREEITEM:
    begin
      case Details.State of
        TREIS_NORMAL: lColor := ColorToNSColor(ColorToRGB(clWindow));
        TREIS_HOT: lColor := ColorToNSColor(ColorToRGB(clHotLight));
        TREIS_SELECTED: lColor := ColorToNSColor(ColorToRGB(clHighlight));
        TREIS_DISABLED: lColor := ColorToNSColor(ColorToRGB(clWindow));
        TREIS_SELECTEDNOTFOCUS: lColor := ColorToNSColor(ColorToRGB(clBtnFace));
        TREIS_HOTSELECTED: lColor := ColorToNSColor(ColorToRGB(clHighlight));
      end;
      lBrush := TCocoaBrush.Create(lColor, False);
      DC.Rectangle(R.Left, R.Top, R.Right, R.Bottom, True, lBrush);
      lBrush.Free;
    end;
    TVP_GLYPH, TVP_HOTGLYPH:
    begin
      // HIThemeDrawButton exists only in 32-bits and there is no Cocoa alternative =(
      {.$define CocoaUseHITheme}
      {$ifdef CocoaUseHITheme}
      {$ifdef CPU386}
      ButtonDrawInfo.version := 0;
      ButtonDrawInfo.State := GetDrawState(Details);
      ButtonDrawInfo.kind := kThemeDisclosureTriangle;
      if Details.State = GLPS_CLOSED then
        ButtonDrawInfo.value := kThemeDisclosureRight
      else
        ButtonDrawInfo.value := kThemeDisclosureDown;

      ButtonDrawInfo.adornment := kThemeAdornmentNone;
      LabelRect := RectToCGRect(R);
      // Felipe: Manual calibration to make TTreeView look right, strange that this is needed =/
      LabelRect.origin.x := LabelRect.origin.x - 2;
      LabelRect.origin.y := LabelRect.origin.y - 1;

      HIThemeDrawButton(LabelRect, ButtonDrawInfo, DC.CGContext(),
        kHIThemeOrientationNormal, @LabelRect);

      Result := CGRectToRect(LabelRect);
      {$endif}
      {$else}
      SetLength(lPoints, 3);

      // face right
      if Details.State = GLPS_CLOSED then
      begin
        lPoints[0] := Types.Point(R.Left+1, R.Top);
        lPoints[1] := Types.Point(R.Left+1, R.Bottom-2);
        lPoints[2] := Types.Point(R.Right-1, (R.Top + R.Bottom-2) div 2);
      end
      // face down
      else
      begin
        lPoints[0] := Types.Point(R.Left, R.Top);
        lPoints[1] := Types.Point(R.Right-2, R.Top);
        lPoints[2] := Types.Point((R.Left + R.Right-2) div 2, R.Bottom-2);
      end;

      // select the appropriate brush & pen
      lColor := ColorToNSColor(Graphics.RGBToColor(121, 121, 121));
      lBrush := TCocoaBrush.Create(lColor, False);
      lOldBrush := LCLIntf.SelectObject(HDC(DC), HGDIOBJ(lBrush));

      lPen := TCocoaPen.Create(Graphics.RGBToColor(121, 121, 121), False);
      lOldPen := LCLIntf.SelectObject(HDC(DC), HGDIOBJ(lPen));

      // Draw the triangle
      DC.Polygon(lPoints, 3, True);

      // restore the old brush and pen
      LCLIntf.SelectObject(HDC(DC), lOldBrush);
      LCLIntf.SelectObject(HDC(DC), lOldPen);
      lBrush.Free;
      lPen.Free;

      Result := R;
      {$endif}
    end;
  end;  *)
end;

(*function TCarbonThemeServices.DrawWindowElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  WindowDrawInfo: HIThemeWindowDrawInfo;
  WindowWidgetDrawInfo: HIThemeWindowWidgetDrawInfo;
  BtnRect: HIRect;
  WindowShape: HIShapeRef;
  WindowRegion: WindowRegionCode;
  Offset: TPoint;
begin
  WindowWidgetDrawInfo.version := 0;
  WindowWidgetDrawInfo.windowState := kThemeStateActive;
  WindowWidgetDrawInfo.windowType := kThemeDocumentWindow;
  WindowWidgetDrawInfo.widgetState := GetDrawState(Details);
  WindowWidgetDrawInfo.titleHeight := 0;
  WindowWidgetDrawInfo.titleWidth := 0;
  WindowWidgetDrawInfo.attributes := kThemeWindowHasFullZoom or kThemeWindowHasCloseBox or kThemeWindowHasCollapseBox;
  case Details.Part of
    WP_MINBUTTON,
    WP_MDIMINBUTTON:
      begin
        WindowWidgetDrawInfo.widgetType := kThemeWidgetCollapseBox;
        WindowRegion := kWindowCollapseBoxRgn;
      end;
    WP_MAXBUTTON:
      begin
        WindowWidgetDrawInfo.widgetType := kThemeWidgetZoomBox;
        WindowRegion := kWindowZoomBoxRgn;
      end;
    WP_CLOSEBUTTON,
    WP_SMALLCLOSEBUTTON,
    WP_MDICLOSEBUTTON:
      begin
        WindowWidgetDrawInfo.widgetType := kThemeWidgetCloseBox;
        WindowRegion := kWindowCloseBoxRgn;
      end;
    WP_RESTOREBUTTON,
    WP_MDIRESTOREBUTTON:
      begin
        WindowWidgetDrawInfo.widgetType := kThemeWidgetZoomBox;
        WindowRegion := kWindowZoomBoxRgn;
      end;
    else
      Exit;
  end;
  // We have a button rectanle but carbon expects from us a titlebar rectangle,
  // so we need to translate one coordinate to another
  BtnRect := RectToCGRect(Types.Rect(0, 0, 100, 100));
  WindowDrawInfo.version := 0;
  WindowDrawInfo.windowType := WindowWidgetDrawInfo.windowType;
  WindowDrawInfo.attributes := WindowWidgetDrawInfo.attributes;
  WindowDrawInfo.state := WindowWidgetDrawInfo.windowState;
  WindowDrawInfo.titleHeight := WindowWidgetDrawInfo.titleHeight;
  WindowDrawInfo.titleWidth := WindowWidgetDrawInfo.titleWidth;
  WindowShape:=nil;
  HIThemeGetWindowShape(BtnRect, WindowDrawInfo, WindowRegion, WindowShape);
  HIShapeGetBounds(WindowShape, BtnRect);
  Offset := CGRectToRect(BtnRect).TopLeft;
  OffsetRect(R, -Offset.X, -Offset.Y);
  BtnRect := RectToCGRect(R);
  OSError(
    HIThemeDrawTitleBarWidget(BtnRect, WindowWidgetDrawInfo, DC.CGContext,
      kHIThemeOrientationNormal),
    Self, 'DrawTreeviewElement', 'HIThemeDrawButton');

  Result := CGRectToRect(BtnRect);
  OffsetRect(Result, Offset.X, Offset.Y);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.InitThemes
  Returns: If the themes are initialized
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.InitThemes: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.InitThemes
  Returns: If the themes have to be used
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.ThemedControlsEnabled
  Returns: If the themed controls are enabled
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.ContentRect
  Params:  DC           - Carbon device context
           Details      - Details for themed element
           BoundingRect - Bounding rectangle
  Returns: Content rectangle of the passed themed element
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  case Details.Element of
    teComboBox: Result := DrawComboBoxElement(DefaultContext, Details, BoundingRect, nil);
    teHeader: Result := DrawHeaderElement(DefaultContext, Details, BoundingRect, nil);
    teButton: Result := DrawButtonElement(DefaultContext, Details, BoundingRect, nil);
    teRebar: Result := DrawRebarElement(DefaultContext, Details, BoundingRect, nil);
    teToolBar: Result := DrawToolBarElement(DefaultContext, Details, BoundingRect, nil);
    teWindow: Result := DrawWindowElement(DefaultContext, Details, BoundingRect, nil);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawEdge
  Params:  DC      - Carbon device context
           Details - Details for themed element
           R       - Bounding rectangle
           Edge    - Type of edge
           Flags   - Type of border

  Draws an edge with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawEdge(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  AContentRect: PRect);
begin

end;
*)
{------------------------------------------------------------------------------
  Method:  TCDThemeServices.DrawElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle

  Draws an element with native look
 ------------------------------------------------------------------------------}
procedure TCDThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  LazDC: TLazCanvas absolute DC;
begin
  if DC = 0 then Exit;

  case Details.Element of
  teTreeview: DrawTreeviewElement(LazDC, Details, R, ClipRect);
  else
  end;
end;
(*
{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawIcon
  Params:  DC      - Carbon device context
           Details - Details for themed element
           R       - Bounding rectangle
           himl    - Image list
           Index   - Icon index

  Draws an icon with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.HasTransparentParts
  Params:  Details - Details for themed element
  Returns: If the themed element has transparent parts
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True;
end;

function TCarbonThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
const
  DefaultPushButtonWidth = 70;
var
  BtnRect: CGRect;
  WindowDrawInfo: HIThemeWindowDrawInfo;
  WindowShape: HIShapeRef;
begin
  case Details.Element of
    teTreeView:
      if (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
      begin
        Result := Types.Size(
          GetCarbonThemeMetric(kThemeMetricDisclosureTriangleWidth),
          GetCarbonThemeMetric(kThemeMetricDisclosureTriangleHeight)
        );
      end
      else
        Result := inherited GetDetailSize(Details);
    teButton:
      if Details.Part = BP_PUSHBUTTON then
      begin
        Result := Types.Size(
          DefaultPushButtonWidth,
          GetCarbonThemeMetric(kThemeMetricPushButtonHeight)
        );
      end else
        Result := inherited GetDetailSize(Details);
    teWindow:
      if (Details.Part in [WP_MINBUTTON, WP_MDIMINBUTTON, WP_MAXBUTTON, WP_CLOSEBUTTON, WP_SMALLCLOSEBUTTON, WP_MDICLOSEBUTTON, WP_RESTOREBUTTON, WP_MDIRESTOREBUTTON]) then
      begin
        BtnRect := RectToCGRect(Types.Rect(0, 0, 100, 100));
        WindowDrawInfo.version := 0;
        WindowDrawInfo.windowType := kThemeDocumentWindow;
        WindowDrawInfo.attributes := kThemeWindowHasFullZoom or kThemeWindowHasCloseBox or kThemeWindowHasCollapseBox;
        WindowDrawInfo.state := kThemeStateActive;
        WindowDrawInfo.titleHeight := 0;
        WindowDrawInfo.titleWidth := 0;

        WindowShape:=nil;
        HIThemeGetWindowShape(BtnRect, WindowDrawInfo, kWindowCloseBoxRgn, WindowShape);
        HIShapeGetBounds(WindowShape, BtnRect);
        with BtnRect.size do
        begin
          Result.cx := Round(width);
          Result.cy := Round(height);
        end;
      end else
        Result := inherited GetDetailSize(Details);
  else
    Result := inherited GetDetailSize(Details);
  end;
end;

function TCarbonThemeServices.GetOption(AOption: TThemeOption): Integer;
begin
  case AOption of
    toShowButtonImages: Result := 0;
    toShowMenuImages: Result := 0;
  else
    Result := inherited GetOption(AOption);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.InternalDrawParentBackground
  Params:  Window - Handle to window
           Target - Carbon device context
           Bounds - Bounding rectangle

  Draws the parent background with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ?
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawText
  Params:  DC      - Carbon device context
           Details - Details for themed element
           S       - Text string to darw
           R       - Bounding rectangle
           Flags   - Draw flags
           Flags2  - Extra draw flags

  Draws the passed text with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
begin
  //
end;
*)
end.

