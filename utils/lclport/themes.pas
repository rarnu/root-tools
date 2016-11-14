// ------------------------------------------------------------------------------------//
//    TODO: write cool acknowledgement here                                            //
//                                                                                     //
// original file name ThemeSrv.pas changed to themes.pas to be compatible with delphi  //
// adapted for Lazarus by Lazarus Team                                                 //
// ------------------------------------------------------------------------------------//

//----------------------------------------------------------------------------------------------------------------------
// Windows XP Theme Manager is freeware. You may freely use it in any software, including commercial software, provided
// you accept the following conditions:
//
// 1) The software may not be included into component collections and similar compilations which are sold. If you want
//    to distribute this software for money then contact me first and ask for my permission.
// 2) My copyright notices in the source code may not be removed or modified.
// 3) If you modify and/or distribute the code to any third party then you must not veil the original author. It must
//    always be clearly identifiable that I, Mike Lischke, am the original author.
// Although it is not required it would be a nice move to recognize my work by adding a citation to the application's
// about box or a similar place.
//
// The original code is ThemeSrv.pas, released 03. February 2002.
//
// The initial developer of the original code is:
//   Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by Mike Lischke are
// (C) 2001-2005 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This unit contains the implementation of TThemeServices which is a designed to manage all necessary aspects of
// the Windows XP (and up) theme APIs. The component is responsible for loading and unloading of the theme DLL
// and offers properties and methods to test for various aspects of the theme APIs and to ease painting of themed
// window content.
//
// The TThemeServices class is designed to work for controls which need XP paint stuff. It does not manipulate
// any VCL class (for this task TThemeManager is responsible) and can be used standalone (also as general support
// tool in an application). Using the global ThemeServices function you can implicitely create and use a single
// instance of the class. Theme Manager uses the same approach.
//----------------------------------------------------------------------------------------------------------------------
// For version information and history see ThemeMgr.pas source file.
//----------------------------------------------------------------------------------------------------------------------

unit Themes;

{$mode objfpc}{$H+}

interface

uses
  // no Graphics or Controls can be used here to prevent circular references
  //
  SysUtils, Types, GraphType, Math, Classes, LCLProc, LCLType, Graphics,
  TmSchema;
  
type
  // These are all elements which can be themed.
  TThemedElement = (
    teButton,
    teClock,
    teComboBox,
    teEdit,
    teExplorerBar,
    teHeader,
    teListView,
    teMenu,
    tePage,
    teProgress,
    teRebar,
    teScrollBar,
    teSpin,
    teStartPanel,
    teStatus,
    teTab,
    teTaskBand,
    teTaskBar,
    teToolBar,
    teToolTip,
    teTrackBar,
    teTrayNotify,
    teTreeview,
    teWindow
  );

  // 'Button' theme data
  TThemedButton = (
    tbButtonDontCare,
    tbButtonRoot,       // The root part of each element is sometimes used for special painting and does not
                        // belong to a certain state.
    tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled, tbPushButtonDefaulted,
    tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled,
    tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled,
    tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled,
    tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled,
    tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled,
    tbGroupBoxNormal, tbGroupBoxDisabled,
    tbUserButton
  );

  // 'Clock' theme data
  TThemedClock = (
    tcClockDontCare,
    tcClockRoot,
    tcTimeNormal
  );

  // 'ComboBox' theme data
  TThemedComboBox = (
    tcComboBoxDontCare,
    tcComboBoxRoot,
    tcDropDownButtonNormal, tcDropDownButtonHot,  tcDropDownButtonPressed,  tcDropDownButtonDisabled
  );

  // 'Edit' theme data
  TThemedEdit = (
    teEditDontCare,
    teEditRoot,
    teEditTextNormal, teEditTextHot, teEditTextSelected, teEditTextDisabled, teEditTextFocused, teEditTextReadOnly, teEditTextAssist,
    teEditCaret
  );

  // 'ExplorerBar' theme data
  TThemedExplorerBar = (
    tebExplorerBarDontCare,
    tebExplorerBarRoot,
    tebHeaderBackgroundNormal, tebHeaderBackgroundHot,  tebHeaderBackgroundPressed,
    tebHeaderCloseNormal, tebHeaderCloseHot, tebHeaderClosePressed,
    tebHeaderPinNormal, tebHeaderPinHot, tebHeaderPinPressed,
    tebHeaderPinSelectedNormal, tebHeaderPinSelectedHot, tebHeaderPinSelectedPressed,
    tebIEBarMenuNormal, tebIEBarMenuHot, tebIEBarMenuPressed,
    tebNormalGroupBackground,
    tebNormalGroupCollapseNormal, tebNormalGroupCollapseHot,  tebNormalGroupCollapsePressed,
    tebNormalGroupExpandNormal, tebNormalGroupExpandHot,  tebNormalGroupExpandPressed,
    tebNormalGroupHead,
    tebSpecialGroupBackground,
    tebSpecialGroupCollapseSpecial, tebSpecialGroupCollapseHot,  tebSpecialGroupCollapsePressed,
    tebSpecialGroupExpandSpecial, tebSpecialGroupExpandHot,  tebSpecialGroupExpandPressed,
    tebSpecialGroupHead
  );

  // 'Header' theme data
  TThemedHeader = (
    thHeaderDontCare,
    thHeaderRoot,
    thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed,
    thHeaderItemLeftNormal, thHeaderItemLeftHot, thHeaderItemLeftPressed,
    thHeaderItemRightNormal, thHeaderItemRightHot, thHeaderItemRightPressed,
    thHeaderSortArrowSortedUp, thHeaderSortArrowSortedDown
  );

  // 'ListView' theme data
  TThemedListview = (
    tlListviewDontCare,
    tlListviewRoot,
    tlListItemNormal, tlListItemHot, tlListItemSelected, tlListItemDisabled, tlListItemSelectedNotFocus,
    tlListGroup,
    tlListDetail,
    tlListSortDetail,
    tlEmptyText
  );

  // 'Menu' theme data
  TThemedMenu = (
    tmMenuDontCare,
    tmMenuRoot,
    tmMenuItemNormal, tmMenuItemSelected, tmMenuItemDemoted,
    tmMenuDropDown,
    tmMenuBarItem,
    tmMenuBarDropDown,
    tmChevron,
    tmSeparator,
    tmBarBackgroundActive, tmBarBackgroundInactive,
    tmBarItemNormal, tmBarItemHot, tmBarItemPushed, tmBarItemDisabled, tmBarItemDisabledHot, tmBarItemDisabledPushed,
    tmPopupBackground,
    tmPopupBorders,
    tmPopupCheckMarkNormal, tmPopupCheckMarkDisabled, tmPopupBulletNormal, tmPopupBulletDisabled,
    tmPopupCheckBackgroundDisabled, tmPopupCheckBackgroundNormal, tmPopupCheckBackgroundBitmap,
    tmPopupGutter,
    tmPopupItemNormal, tmPopupItemHot, tmPopupItemDisabled, tmPopupItemDisabledHot,
    tmPopupSeparator,
    tmPopupSubmenuNormal, tmPopupSubmenuDisabled,
    tmSystemCloseNormal, tmSystemCloseDisabled,
    tmSystemMaximizeNormal, tmSystemMaximizeDisabled,
    tmSystemMinimizeNormal, tmSystemMinimizeDisabled,
    tmSystemRestoreNormal, tmSystemRestoreDisabled
  );

  // 'Page' theme data
  TThemedPage = (
    tpPageDontCare,
    tpPageRoot,
    tpUpNormal, tpUpHot, tpUpPressed, tpUpDisabled,
    tpDownNormal, tpDownHot, tpDownPressed, tpDownDisabled,
    tpUpHorzNormal, tpUpHorzHot, tpUpHorzPressed, tpUpHorzDisabled,
    tpDownHorzNormal, tpDownHorzHot, tpDownHorzPressed, tpDownHorzDisabled
  );

  // 'Progress' theme data
  TThemedProgress = (
    tpProgressDontCare,
    tpProgressRoot,
    tpBar,
    tpBarVert,
    tpChunk,
    tpChunkVert
  );

  // 'Rebar' theme data
  TThemedRebar = (
    trRebarDontCare,
    trRebarRoot,
    trGripper,
    trGripperVert,
    trBandNormal, trBandHot, trBandPressed, trBandDisabled, trBandChecked, trBandHotChecked,
    trChevronNormal, trChevronHot, trChevronPressed, trChevronDisabled,
    trChevronVertNormal, trChevronVertHot, trChevronVertPressed, trChevronVertDisabled
  );

  // 'ScrollBar' theme data
  TThemedScrollBar = (
    tsScrollBarDontCare,
    tsScrollBarRoot,
    tsArrowBtnUpNormal, tsArrowBtnUpHot, tsArrowBtnUpPressed, tsArrowBtnUpDisabled,
    tsArrowBtnDownNormal, tsArrowBtnDownHot, tsArrowBtnDownPressed, tsArrowBtnDownDisabled,
    tsArrowBtnLeftNormal, tsArrowBtnLeftHot, tsArrowBtnLeftPressed, tsArrowBtnLeftDisabled,
    tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled,
    tsThumbBtnHorzNormal, tsThumbBtnHorzHot, tsThumbBtnHorzPressed, tsThumbBtnHorzDisabled,
    tsThumbBtnVertNormal, tsThumbBtnVertHot, tsThumbBtnVertPressed, tsThumbBtnVertDisabled,
    tsLowerTrackHorzNormal, tsLowerTrackHorzHot, tsLowerTrackHorzPressed, tsLowerTrackHorzDisabled,
    tsUpperTrackHorzNormal, tsUpperTrackHorzHot, tsUpperTrackHorzPressed, tsUpperTrackHorzDisabled,
    tsLowerTrackVertNormal, tsLowerTrackVertHot, tsLowerTrackVertPressed, tsLowerTrackVertDisabled,
    tsUpperTrackVertNormal, tsUpperTrackVertHot, tsUpperTrackVertPressed, tsUpperTrackVertDisabled,
    tsGripperHorzNormal, tsGripperHorzHot, tsGripperHorzPressed, tsGripperHorzDisabled,
    tsGripperVertNormal, tsGripperVertHot, tsGripperVertPressed, tsGripperVertDisabled,
    tsSizeBoxRightAlign, tsSizeBoxLeftAlign
  );

  // 'Spin' theme data
  TThemedSpin = (
    tsSpinDontCare,
    tsSpinRoot,
    tsUpNormal, tsUpHot, tsUpPressed, tsUpDisabled,
    tsDownNormal, tsDownHot, tsDownPressed, tsDownDisabled,
    tsUpHorzNormal, tsUpHorzHot, tsUpHorzPressed, tsUpHorzDisabled,
    tsDownHorzNormal, tsDownHorzHot, tsDownHorzPressed, tsDownHorzDisabled
  );

  // 'StartPanel' theme data
  TThemedStartPanel = (
    tspStartPanelDontCare,
    tspStartPanelRoot,
    tspUserPane,
    tspMorePrograms,
    tspMoreProgramsArrowNormal, tspMoreProgramsArrowHot, tspMoreProgramsArrowPressed,
    tspProgList,
    tspProgListSeparator,
    tspPlacesList,
    tspPlacesListSeparator,
    tspLogOff,
    tspLogOffButtonsNormal, tspLogOffButtonsHot, tspLogOffButtonsPressed,
    tspUserPicture,
    tspPreview
  );

  // 'Status' theme data
  TThemedStatus = (
    tsStatusDontCare,
    tsStatusRoot,
    tsPane,
    tsGripperPane,
    tsGripper
  );

  // 'Tab' theme data
  TThemedTab = (
    ttTabDontCare,
    ttTabRoot,
    ttTabItemNormal, ttTabItemHot, ttTabItemSelected, ttTabItemDisabled, ttTabItemFocused,
    ttTabItemLeftEdgeNormal, ttTabItemLeftEdgeHot, ttTabItemLeftEdgeSelected, ttTabItemLeftEdgeDisabled, ttTabItemLeftEdgeFocused,
    ttTabItemRightEdgeNormal, ttTabItemRightEdgeHot, ttTabItemRightEdgeSelected, ttTabItemRightEdgeDisabled, ttTabItemRightEdgeFocused,
    ttTabItemBothEdgeNormal, ttTabItemBothEdgeHot, ttTabItemBothEdgeSelected, ttTabItemBothEdgeDisabled, ttTabItemBothEdgeFocused,
    ttTopTabItemNormal, ttTopTabItemHot, ttTopTabItemSelected, ttTopTabItemDisabled, ttTopTabItemFocused,
    ttTopTabItemLeftEdgeNormal, ttTopTabItemLeftEdgeHot, ttTopTabItemLeftEdgeSelected, ttTopTabItemLeftEdgeDisabled, ttTopTabItemLeftEdgeFocused,
    ttTopTabItemRightEdgeNormal, ttTopTabItemRightEdgeHot, ttTopTabItemRightEdgeSelected, ttTopTabItemRightEdgeDisabled, ttTopTabItemRightEdgeFocused,
    ttTopTabItemBothEdgeNormal, ttTopTabItemBothEdgeHot, ttTopTabItemBothEdgeSelected, ttTopTabItemBothEdgeDisabled, ttTopTabItemBothEdgeFocused,
    ttPane,
    ttBody
  );

  // 'TaskBand' theme data
  TThemedTaskBand = (
    ttbTaskBandDontCare,
    ttbTaskBandRoot,
    ttbGroupCount,
    ttbFlashButton,
    ttpFlashButtonGroupMenu
  );

  // 'TaskBar' theme data
  TThemedTaskBar = (
    ttTaskBarDontCare,
    ttTaskBarRoot,
    ttbTimeNormal
  );

  // 'ToolBar' theme data
  TThemedToolBar = (
    ttbToolBarDontCare,
    ttbToolBarRoot,
    ttbButtonNormal, ttbButtonHot, ttbButtonPressed, ttbButtonDisabled, ttbButtonChecked, ttbButtonCheckedHot,
    ttbDropDownButtonNormal, ttbDropDownButtonHot, ttbDropDownButtonPressed, ttbDropDownButtonDisabled, ttbDropDownButtonChecked, ttbDropDownButtonCheckedHot,
    ttbSplitButtonNormal, ttbSplitButtonHot, ttbSplitButtonPressed, ttbSplitButtonDisabled, ttbSplitButtonChecked, ttbSplitButtonCheckedHot,
    ttbSplitButtonDropDownNormal, ttbSplitButtonDropDownHot, ttbSplitButtonDropDownPressed, ttbSplitButtonDropDownDisabled, ttbSplitButtonDropDownChecked, ttbSplitButtonDropDownCheckedHot,
    ttbSeparatorNormal, ttbSeparatorHot, ttbSeparatorPressed, ttbSeparatorDisabled, ttbSeparatorChecked, ttbSeparatorCheckedHot,
    ttbSeparatorVertNormal, ttbSeparatorVertHot, ttbSeparatorVertPressed, ttbSeparatorVertDisabled, ttbSeparatorVertChecked, ttbSeparatorVertCheckedHot
  );

  // 'ToolTip' theme data
  TThemedToolTip = (
    tttToolTipDontCare,
    tttToolTipRoot,
    tttStandardNormal, tttStandardLink,
    tttStandardTitleNormal, tttStandardTitleLink,
    tttBaloonNormal, tttBaloonLink,
    tttBaloonTitleNormal, tttBaloonTitleLink,
    tttCloseNormal, tttCloseHot, tttClosePressed
  );

  // 'TrackBar' theme data
  TThemedTrackBar = (
    ttbTrackBarDontCare,
    ttbTrackBarRoot,
    ttbTrack,
    ttbTrackVert,
    ttbThumbNormal, ttbThumbHot, ttbThumbPressed, ttbThumbFocused, ttbThumbDisabled,
    ttbThumbBottomNormal, ttbThumbBottomHot, ttbThumbBottomPressed, ttbThumbBottomFocused, ttbThumbBottomDisabled,
    ttbThumbTopNormal, ttbThumbTopHot, ttbThumbTopPressed, ttbThumbTopFocused, ttbThumbTopDisabled,
    ttbThumbVertNormal, ttbThumbVertHot, ttbThumbVertPressed, ttbThumbVertFocused, ttbThumbVertDisabled,
    ttbThumbLeftNormal, ttbThumbLeftHot, ttbThumbLeftPressed, ttbThumbLeftFocused, ttbThumbLeftDisabled,
    ttbThumbRightNormal, ttbThumbRightHot, ttbThumbRightPressed, ttbThumbRightFocused, ttbThumbRightDisabled,
    ttbThumbTics, 
    ttbThumbTicsVert
  );

  // 'TrayNotify' theme data
  TThemedTrayNotify = (
    ttnTrayNotifyDontCare,
    ttnTrayNotifyRoot,
    ttnBackground,
    ttnAnimBackground
  );

  // 'Treeview' theme data
  TThemedTreeview = (
    ttTreeviewDontCare,
    ttTreeviewRoot,
    ttItemNormal, ttItemHot, ttItemSelected, ttItemDisabled, ttItemSelectedNotFocus,
    ttGlyphClosed, ttGlyphOpened,
    ttBranch,
    ttHotGlyphClosed, ttHotGlyphOpened
  );

  // 'Window' theme data
  TThemedWindow = (
    twWindowDontCare,
    twWindowRoot,
    twCaptionActive, twCaptionInactive, twCaptionDisabled,
    twSmallCaptionActive, twSmallCaptionInactive, twSmallCaptionDisabled,
    twMinCaptionActive, twMinCaptionInactive, twMinCaptionDisabled,
    twSmallMinCaptionActive, twSmallMinCaptionInactive, twSmallMinCaptionDisabled,
    twMaxCaptionActive, twMaxCaptionInactive, twMaxCaptionDisabled,
    twSmallMaxCaptionActive, twSmallMaxCaptionInactive, twSmallMaxCaptionDisabled,

    twFrameLeftActive, twFrameLeftInactive,
    twFrameRightActive, twFrameRightInactive,
    twFrameBottomActive, twFrameBottomInactive,
    twSmallFrameLeftActive, twSmallFrameLeftInactive,
    twSmallFrameRightActive, twSmallFrameRightInactive,
    twSmallFrameBottomActive, twSmallFrameBottomInactive,

    twSysButtonNormal, twSysButtonHot, twSysButtonPushed, twSysButtonDisabled, twSysButtonInactive,
    twMDISysButtonNormal, twMDISysButtonHot, twMDISysButtonPushed, twMDISysButtonDisabled, twMDISysButtonInactive,
    twMinButtonNormal, twMinButtonHot, twMinButtonPushed, twMinButtonDisabled, twMinButtonInactive,
    twMDIMinButtonNormal, twMDIMinButtonHot, twMDIMinButtonPushed, twMDIMinButtonDisabled, twMDIMinButtonInactive,
    twMaxButtonNormal, twMaxButtonHot, twMaxButtonPushed, twMaxButtonDisabled, twMaxButtonInactive,
    twCloseButtonNormal, twCloseButtonHot, twCloseButtonPushed, twCloseButtonDisabled, twCloseButtonInactive,
    twSmallCloseButtonNormal, twSmallCloseButtonHot, twSmallCloseButtonPushed, twSmallCloseButtonDisabled, twSmallCloseButtonInactive,
    twMDICloseButtonNormal, twMDICloseButtonHot, twMDICloseButtonPushed, twMDICloseButtonDisabled, twMDICloseButtonInactive,
    twRestoreButtonNormal, twRestoreButtonHot, twRestoreButtonPushed, twRestoreButtonDisabled, twRestoreButtonInactive,
    twMDIRestoreButtonNormal, twMDIRestoreButtonHot, twMDIRestoreButtonPushed, twMDIRestoreButtonDisabled, twMDIRestoreButtonInactive,
    twHelpButtonNormal, twHelpButtonHot, twHelpButtonPushed, twHelpButtonDisabled, twHelpButtonInactive,
    twMDIHelpButtonNormal, twMDIHelpButtonHot, twMDIHelpButtonPushed, twMDIHelpButtonDisabled, twMDIHelpButtonInactive,

    twHorzScrollNormal, twHorzScrollHot, twHorzScrollPushed, twHorzScrollDisabled,
    twHorzThumbNormal, twHorzThumbHot, twHorzThumbPushed, twHorzThumbDisabled,
    twVertScrollNormal, twVertScrollHot, twVertScrollPushed, twVertScrollDisabled,
    twVertThumbNormal, twVertThumbHot, twVertThumbPushed, twVertThumbDisabled,

    twDialog,
    twCaptionSizingTemplate,
    twSmallCaptionSizingTemplate,
    twFrameLeftSizingTemplate,
    twSmallFrameLeftSizingTemplate,
    twFrameRightSizingTemplate,
    twSmallFrameRightSizingTemplate,
    twFrameBottomSizingTemplate,
    twSmallFrameBottomSizingTemplate
  );

  PThemedElementDetails = ^TThemedElementDetails;
  TThemedElementDetails = record
    Element: TThemedElement;
    Part,
    State: Integer;
  end;

  TThemeOption = (
    toShowButtonImages, // show images on buttons
    toShowMenuImages,   // show images on menus
    toUseGlyphEffects   // use hot/down effects on (button) glyphs
  );

  // TThemeServices is a small foot print class to provide the user with pure
  // Windows XP theme related abilities like painting elements and text or
  // retrieving certain info.
  TThemeServices = class(TObject)
  private
    FThemesAvailable,
    FUseThemes,
    FThemedControlsEnabled: Boolean;
    FOnThemeChange: TNotifyEvent;      // Called when the current window theme has changed.
    FDottedBrush: HBrush;

    function GetThemesEnabled: Boolean;
    function GetDottedBrush: HBrush;
  protected
    function InitThemes: Boolean; virtual;
    procedure UnloadThemeData; virtual;
    function UseThemes: Boolean; virtual;
    function ThemedControlsEnabled: Boolean; virtual;

    function InternalColorToRGB(Details: TThemedElementDetails; Color: LongInt): COLORREF; virtual;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IntfDoOnThemeChange; virtual;

    // state helpers
    function IsDisabled(Details: TThemedElementDetails): Boolean;
    function IsPushed(Details: TThemedElementDetails): Boolean;
    function IsHot(Details: TThemedElementDetails): Boolean;
    function IsChecked(Details: TThemedElementDetails): Boolean;
    function IsMixed(Details: TThemedElementDetails): Boolean;

    function GetElementDetails(Detail: TThemedButton): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedClock): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedComboBox): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedEdit): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedExplorerBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedHeader): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedListView): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedMenu): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedPage): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedProgress): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedRebar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedScrollBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedSpin): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedStartPanel): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedStatus): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTab): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTaskBand): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTaskBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedToolBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedToolTip): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTrackBar): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTrayNotify): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedTreeview): TThemedElementDetails; overload;
    function GetElementDetails(Detail: TThemedWindow): TThemedElementDetails; overload;
    
    function GetDetailSize(Details: TThemedElementDetails): TSize; virtual;
    function GetDetailRegion(DC: HDC; Details: TThemedElementDetails; const R: TRect): HRGN; virtual;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; virtual;
    function GetOption(AOption: TThemeOption): Integer; virtual;
    function GetTextExtent(DC: HDC; Details: TThemedElementDetails; const S: String; Flags: Cardinal; BoundingRect: PRect): TRect; virtual;

    function ColorToRGB(Color: LongInt; Details: PThemedElementDetails = nil): COLORREF;
    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; virtual;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
      AContentRect: PRect = nil); virtual;
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil); virtual;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); virtual; overload;
    procedure DrawIcon(ACanvas: TPersistent; Details: TThemedElementDetails; const P: TPoint; AImageList: TPersistent; Index: Integer); virtual; overload;
    procedure DrawParentBackground(Window: HWND; Target: HDC; Details: PThemedElementDetails; OnlyIfTransparent: Boolean;
      Bounds: PRect = nil);
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); virtual; overload;
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); virtual; overload;

    function HasTransparentParts(Details: TThemedElementDetails): Boolean; virtual;
    procedure PaintBorder(Control: TObject; EraseLRCorner: Boolean); virtual;
    procedure UpdateThemes;

    property DottedBrush: HBRUSH read GetDottedBrush;
    property ThemesAvailable: Boolean read FThemesAvailable;
    property ThemesEnabled: Boolean read GetThemesEnabled;

    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

function ThemeServices: TThemeServices;
function ThemedElementDetailsEqual(const D1, D2: TThemedElementDetails): Boolean;
//----------------------------------------------------------------------------------------------------------------------

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  ThemeManagerCopyright: string = 'Theme manager © 2001-2005 Mike Lischke';
type
  TThemesImageDrawEvent = procedure(AImageList: TPersistent; ACanvas: TPersistent;
                     AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect);
var
  ThemesImageDrawEvent: TThemesImageDrawEvent = nil; // set by unit ImgList if used

implementation

uses
  InterfaceBase, LCLIntf;

//----------------------------------------------------------------------------------------------------------------------

function ThemeServices: TThemeServices;
begin
  Result := WidgetSet.ThemeServices;
end;

function ThemedElementDetailsEqual(const D1, D2: TThemedElementDetails): Boolean;
begin
  Result := CompareMem(@D1, @D2, SizeOf(TThemedElementDetails));
end;

//----------------- TThemeServices -------------------------------------------------------------------------------------

constructor TThemeServices.Create;
begin
  FDottedBrush := 0;

  FThemesAvailable := InitThemes;
  UpdateThemes;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TThemeServices.Destroy;
begin
  UnloadThemeData;

  if FDottedBrush <> 0 then
    DeleteObject(FDottedBrush);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetThemesEnabled: Boolean;
begin
  Result := FThemesAvailable and FUseThemes and FThemedControlsEnabled;
end;

function TThemeServices.GetDottedBrush: HBrush;
const
  LineBitsDotted: array[0..7] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA);
var
  DottedBitmap: HBitmap;
begin
  if FDottedBrush = 0 then
  begin
    DottedBitmap := CreateBitmap(8, 8, 1, 1, @LineBitsDotted);
    if DottedBitmap <> 0 then
    begin
      FDottedBrush := CreatePatternBrush(DottedBitmap);
      DeleteObject(DottedBitmap);
    end;
  end;
  Result := FDottedBrush;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.IntfDoOnThemeChange;
begin
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedButton): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teButton;
  with Result do
  begin
    case Detail of
      tbPushButtonNormal..tbPushButtonDefaulted:
        begin
          Part := BP_PUSHBUTTON;
          Base := Ord(tbPushButtonNormal);
        end;
      tbRadioButtonUncheckedNormal..tbRadioButtonCheckedDisabled:
        begin
          Part := BP_RADIOBUTTON;
          Base := Ord(tbRadioButtonUncheckedNormal);
        end;
      tbCheckBoxUncheckedNormal..tbCheckBoxMixedDisabled:
        begin
          Part := BP_CHECKBOX;
          Base := Ord(tbCheckBoxUncheckedNormal);
        end;
      tbGroupBoxNormal..tbGroupBoxDisabled:
        begin
          Part := BP_GROUPBOX;
          Base := Ord(tbGroupBoxNormal);
        end;
      tbUserButton:
        begin
          Part := BP_USERBUTTON;
          Base := Ord(tbUserButton);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedClock): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teClock;
  with Result do
  begin
    case Detail of
      tcTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(tcTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedComboBox): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teComboBox;
  with Result do
  begin
    case Detail of
      tcDropDownButtonNormal..tcDropDownButtonDisabled:
        begin
          Part := CP_DROPDOWNBUTTON;
          Base := Ord(tcDropDownButtonNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedEdit): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teEdit;
  with Result do
  begin
    case Detail of
      teEditTextNormal..teEditTextAssist:
        begin
          Part := EP_EDITTEXT;
          Base := Ord(teEditTextNormal);
        end;
      teEditCaret:
        begin
          Part := EP_CARET;
          Base := Ord(teEditCaret);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedExplorerBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teExplorerBar;
  with Result do
  begin
    case Detail of
      tebHeaderBackgroundNormal..tebHeaderBackgroundPressed:
        begin
          Part := EBP_HEADERBACKGROUND;
          Base := Ord(tebHeaderBackgroundNormal);
        end;
      tebHeaderCloseNormal..tebHeaderClosePressed:
        begin
          Part := EBP_HEADERCLOSE;
          Base := Ord(tebHeaderCloseNormal);
        end;
      tebHeaderPinNormal..tebHeaderPinSelectedPressed:
        begin
          Part := EBP_HEADERPIN;
          Base := Ord(tebHeaderPinSelectedNormal);
        end;
      tebIEBarMenuNormal..tebIEBarMenuPressed:
        begin
          Part := EBP_IEBARMENU;
          Base := Ord(tebIEBarMenuNormal);
        end;
      tebNormalGroupBackground:
        begin
          Part := EBP_NORMALGROUPBACKGROUND;
          Base := Ord(tebNormalGroupBackground);
        end;
      tebNormalGroupCollapseNormal..tebNormalGroupCollapsePressed:
        begin
          Part := EBP_NORMALGROUPCOLLAPSE;
          Base := Ord(tebNormalGroupCollapseNormal);
        end;
      tebNormalGroupExpandNormal..tebNormalGroupExpandPressed:
        begin
          Part := EBP_NORMALGROUPEXPAND;
          Base := Ord(tebNormalGroupExpandNormal);
        end;
      tebNormalGroupHead:
        begin
          Part := EBP_NORMALGROUPHEAD;
          Base := Ord(tebNormalGroupHead);
        end;
      tebSpecialGroupBackground:
        begin
          Part := EBP_SPECIALGROUPBACKGROUND;
          Base := Ord(tebSpecialGroupBackground);
        end;
      tebSpecialGroupCollapseSpecial..tebSpecialGroupCollapsePressed:
        begin
          Part := EBP_SPECIALGROUPCOLLAPSE;
          Base := Ord(tebSpecialGroupCollapseSpecial);
        end;
      tebSpecialGroupExpandSpecial..tebSpecialGroupExpandPressed:
        begin
          Part := EBP_SPECIALGROUPEXPAND;
          Base := Ord(tebSpecialGroupExpandSpecial);
        end;
      tebSpecialGroupHead:
        begin
          Part := EBP_SPECIALGROUPHEAD;
          Base := Ord(tebSpecialGroupHead);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedHeader): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teHeader;
  with Result do
  begin
    case Detail of
      thHeaderItemNormal..thHeaderItemPressed:
        begin
          Part := HP_HEADERITEM;
          Base := Ord(thHeaderItemNormal);
        end;
      thHeaderItemLeftNormal..thHeaderItemLeftPressed:
        begin
          Part := HP_HEADERITEMLEFT;
          Base := Ord(thHeaderItemLeftNormal);
        end;
      thHeaderItemRightNormal..thHeaderItemRightPressed:
        begin
          Part := HP_HEADERITEMRIGHT;
          Base := Ord(thHeaderItemRightNormal);
        end;
      thHeaderSortArrowSortedUp..thHeaderSortArrowSortedDown:
        begin
          Part := HP_HEADERSORTARROW;
          Base := Ord(thHeaderSortArrowSortedUp);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedListView
  ): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teListView;
  with Result do
  begin
    case Detail of
      tlListItemNormal..tlListItemSelectedNotFocus:
        begin
          Part := LVP_LISTITEM;
          Base := Ord(tlListItemNormal);
        end;
      tlListGroup:
        begin
          Part := LVP_LISTGROUP;
          Base := Ord(tlListGroup);
        end;
      tlListDetail:
        begin
          Part := LVP_LISTDETAIL;
          Base := Ord(tlListDetail);
        end;
      tlListSortDetail:
        begin
          Part := LVP_LISTSORTEDDETAIL;
          Base := Ord(tlListSortDetail);
        end;
      tlEmptyText:
        begin
          Part := LVP_EMPTYTEXT;
          Base := Ord(tlEmptyText);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedMenu): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teMenu;
  with Result do
  begin
    case Detail of
      tmMenuItemNormal..tmMenuItemDemoted:
        begin
          Part := MENU_MENUITEM_TMSCHEMA;
          Base := Ord(tmMenuItemNormal);
        end;
      tmMenuDropDown:
        begin
          Part := MENU_MENUDROPDOWN_TMSCHEMA;
          Base := Ord(tmMenuDropDown);
        end;
      tmMenuBarItem:
        begin
          Part := MENU_MENUBARITEM_TMSCHEMA;
          Base := Ord(tmMenuBarItem);
        end;
      tmMenuBarDropDown:
        begin
          Part := MENU_MENUBARDROPDOWN_TMSCHEMA;
          Base := Ord(tmMenuBarDropDown);
        end;
      tmChevron:
        begin
          Part := MENU_CHEVRON_TMSCHEMA;
          Base := Ord(tmChevron);
        end;
      tmSeparator:
        begin
          Part := MENU_SEPARATOR_TMSCHEMA;
          Base := Ord(tmSeparator);
        end;
      tmBarBackgroundActive..tmBarBackgroundInactive:
        begin
          Part := MENU_BARBACKGROUND;
          Base := Ord(tmBarBackgroundActive);
        end;
      tmBarItemNormal..tmBarItemDisabledPushed:
        begin
          Part := MENU_BARITEM;
          Base := Ord(tmBarItemNormal);
        end;
      tmPopupBackground:
        begin
          Part := MENU_POPUPBACKGROUND;
          Base := Ord(tmPopupBackground);
        end;
      tmPopupBorders:
        begin
          Part := MENU_POPUPBORDERS;
          Base := Ord(tmPopupBorders);
        end;
      tmPopupCheckMarkNormal..tmPopupBulletDisabled:
        begin
          Part := MENU_POPUPCHECK;
          Base := Ord(tmPopupCheckMarkNormal);
        end;
      tmPopupCheckBackgroundDisabled..tmPopupCheckBackgroundBitmap:
        begin
          Part := MENU_POPUPCHECKBACKGROUND;
          Base := Ord(tmPopupCheckBackgroundDisabled);
        end;
      tmPopupGutter:
        begin
          Part := MENU_POPUPGUTTER;
          Base := Ord(tmPopupGutter);
        end;
      tmPopupItemNormal..tmPopupItemDisabledHot:
        begin
          Part := MENU_POPUPITEM;
          Base := Ord(tmPopupItemNormal);
        end;
      tmPopupSeparator:
        begin
          Part := MENU_POPUPSEPARATOR;
          Base := Ord(tmPopupSeparator);
        end;
      tmPopupSubmenuNormal..tmPopupSubmenuDisabled:
        begin
          Part := MENU_POPUPSUBMENU;
          Base := Ord(tmPopupSubmenuNormal);
        end;
      tmSystemCloseNormal..tmSystemCloseDisabled:
        begin
          Part := MENU_SYSTEMCLOSE;
          Base := Ord(tmSystemCloseNormal);
        end;
      tmSystemMaximizeNormal..tmSystemMaximizeDisabled:
        begin
          Part := MENU_SYSTEMMAXIMIZE;
          Base := Ord(tmSystemMaximizeNormal);
        end;
      tmSystemMinimizeNormal..tmSystemMinimizeDisabled:
        begin
          Part := MENU_SYSTEMMINIMIZE;
          Base := Ord(tmSystemMinimizeNormal);
        end;
      tmSystemRestoreNormal..tmSystemRestoreDisabled:
        begin
          Part := MENU_SYSTEMRESTORE;
          Base := Ord(tmSystemRestoreNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedPage): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := tePage;
  with Result do
  begin
    case Detail of
      tpUpNormal..tpUpDisabled:
        begin
          Part := PGRP_UP;
          Base := Ord(tpUpNormal);
        end;
      tpDownNormal..tpDownDisabled:
        begin
          Part := PGRP_DOWN;
          Base := Ord(tpDownNormal);
        end;
      tpUpHorzNormal..tpUpHorzDisabled:
        begin
          Part := PGRP_UPHORZ;
          Base := Ord(tpUpHorzNormal);
        end;
      tpDownHorzNormal..tpDownHorzDisabled:
        begin
          Part := PGRP_DOWNHORZ;
          Base := Ord(tpDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedProgress): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teProgress;
  with Result do
  begin
    case Detail of
      tpBar:
        begin
          Part := PP_BAR;
          Base := Ord(tpBar);
        end;
      tpBarVert:
        begin
          Part := PP_BARVERT;
          Base := Ord(tpBarVert);
        end;
      tpChunk:
        begin
          Part := PP_CHUNK;
          Base := Ord(tpChunk);
        end;
      tpChunkVert:
        begin
          Part := PP_CHUNKVERT;
          Base := Ord(tpChunkVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedRebar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teRebar;
  with Result do
  begin
    case Detail of
      trGripper:
        begin
          Part := RP_GRIPPER;
          Base := Ord(trGripper);
        end;
      trGripperVert:
        begin
          Part := RP_GRIPPERVERT;
          Base := Ord(trGripperVert);
        end;
      trBandNormal..trBandHotChecked:
        begin
          Part := RP_BAND;
          Base := Ord(trBandNormal);
        end;
      trChevronNormal..trChevronDisabled:
        begin
          Part := RP_CHEVRON;
          Base := Ord(trChevronNormal);
        end;
      trChevronVertNormal..trChevronVertDisabled:
        begin
          Part := RP_CHEVRONVERT;
          Base := Ord(trChevronVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedScrollBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teScrollBar;
  with Result do
  begin
    case Detail of
      tsArrowBtnUpNormal..tsArrowBtnRightDisabled:
        begin
          Part := SBP_ARROWBTN;
          Base := Ord(tsArrowBtnUpNormal);
        end;
      tsThumbBtnHorzNormal..tsThumbBtnHorzDisabled:
        begin
          Part := SBP_THUMBBTNHORZ;
          Base := Ord(tsThumbBtnHorzNormal);
        end;
      tsThumbBtnVertNormal..tsThumbBtnVertDisabled:
        begin
          Part := SBP_THUMBBTNVERT;
          Base := Ord(tsThumbBtnVertNormal);
        end;
      tsLowerTrackHorzNormal..tsLowerTrackHorzDisabled:
        begin
          Part := SBP_LOWERTRACKHORZ;
          Base := Ord(tsLowerTrackHorzNormal);
        end;
      tsUpperTrackHorzNormal..tsUpperTrackHorzDisabled:
        begin
          Part := SBP_UPPERTRACKHORZ;
          Base := Ord(tsUpperTrackHorzNormal);
        end;
      tsLowerTrackVertNormal..tsLowerTrackVertDisabled:
        begin
          Part := SBP_LOWERTRACKVERT;
          Base := Ord(tsLowerTrackVertNormal);
        end;
      tsUpperTrackVertNormal..tsUpperTrackVertDisabled:
        begin
          Part := SBP_UPPERTRACKVERT;
          Base := Ord(tsUpperTrackVertNormal);
        end;
      tsGripperHorzNormal..tsGripperHorzDisabled:
        begin
          Part := SBP_GRIPPERHORZ;
          Base := Ord(tsGripperHorzNormal);
        end;
      tsGripperVertNormal..tsGripperVertDisabled:
        begin
          Part := SBP_GRIPPERVERT;
          Base := Ord(tsGripperVertNormal);
        end;
      tsSizeBoxRightAlign..tsSizeBoxLeftAlign:
        begin
          Part := SBP_SIZEBOX;
          Base := Ord(tsSizeBoxRightAlign);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedSpin): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teSpin;
  with Result do
  begin
    case Detail of
      tsUpNormal..tsUpDisabled:
        begin
          Part := SPNP_UP;
          Base := Ord(tsUpNormal);
        end;
      tsDownNormal..tsDownDisabled:
        begin
          Part := SPNP_DOWN;
          Base := Ord(tsDownNormal);
        end;
      tsUpHorzNormal..tsUpHorzDisabled:
        begin
          Part := SPNP_UPHORZ;
          Base := Ord(tsUpHorzNormal);
        end;
      tsDownHorzNormal..tsDownHorzDisabled:
        begin
          Part := SPNP_DOWNHORZ;
          Base := Ord(tsDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedStartPanel): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teStartPanel;
  with Result do
  begin
    case Detail of
      tspUserPane:
        begin
          Part := SPP_USERPANE;
          Base := Ord(tspUserPane);
        end;
      tspMorePrograms:
        begin
          Part := SPP_MOREPROGRAMS;
          Base := Ord(tspMorePrograms);
        end;
      tspMoreProgramsArrowNormal..tspMoreProgramsArrowPressed:
        begin
          Part := SPP_MOREPROGRAMSARROW;
          Base := Ord(tspMoreProgramsArrowNormal);
        end;
      tspProgList:
        begin
          Part := SPP_PROGLIST;
          Base := Ord(tspProgList);
        end;
      tspProgListSeparator:
        begin
          Part := SPP_PROGLISTSEPARATOR;
          Base := Ord(tspProgListSeparator);
        end;
      tspPlacesList:
        begin
          Part := SPP_PLACESLIST;
          Base := Ord(tspPlacesList);
        end;
      tspPlacesListSeparator:
        begin
          Part := SPP_PLACESLISTSEPARATOR;
          Base := Ord(tspPlacesListSeparator);
        end;
      tspLogOff:
        begin
          Part := SPP_LOGOFF;
          Base := Ord(tspLogOff);
        end;
      tspLogOffButtonsNormal..tspLogOffButtonsPressed:
        begin
          Part := SPP_LOGOFFBUTTONS;
          Base := Ord(tspLogOffButtonsNormal);
        end;
      tspUserPicture:
        begin
          Part := SPP_USERPICTURE;
          Base := Ord(tspUserPicture);
        end;
      tspPreview:
        begin
          Part := SPP_PREVIEW;
          Base := Ord(tspPreview);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedStatus): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teStatus;
  with Result do
  begin
    case Detail of
      tsPane:
        begin
          Part := SP_PANE;
          Base := Ord(tsPane);
        end;
      tsGripperPane:
        begin
          Part := SP_GRIPPERPANE;
          Base := Ord(tsGripperPane);
        end;
      tsGripper:
        begin
          Part := SP_GRIPPER;
          Base := Ord(tsGripper);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTab): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTab;
  with Result do
  begin
    case Detail of
      ttTabItemNormal..ttTabItemFocused:
        begin
          Part := TABP_TABITEM;
          Base := Ord(ttTabItemNormal);
        end;
      ttTabItemLeftEdgeNormal..ttTabItemLeftEdgeFocused:
        begin
          Part := TABP_TABITEMLEFTEDGE;
          Base := Ord(ttTabItemLeftEdgeNormal);
        end;
      ttTabItemRightEdgeNormal..ttTabItemRightEdgeFocused:
        begin
          Part := TABP_TABITEMRIGHTEDGE;
          Base := Ord(ttTabItemRightEdgeNormal);
        end;
      ttTabItemBothEdgeNormal..ttTabItemBothEdgeFocused:
        begin
          Part := TABP_TABITEMBOTHEDGE;
          Base := Ord(ttTabItemBothEdgeNormal);
        end;
      ttTopTabItemNormal..ttTopTabItemFocused:
        begin
          Part := TABP_TOPTABITEM;
          Base := Ord(ttTopTabItemNormal);
        end;
      ttTopTabItemLeftEdgeNormal..ttTopTabItemLeftEdgeFocused:
        begin
          Part := TABP_TOPTABITEMLEFTEDGE;
          Base := Ord(ttTopTabItemLeftEdgeNormal);
        end;
      ttTopTabItemRightEdgeNormal..ttTopTabItemRightEdgeFocused:
        begin
          Part := TABP_TOPTABITEMRIGHTEDGE;
          Base := Ord(ttTopTabItemRightEdgeNormal);
        end;
      ttTopTabItemBothEdgeNormal..ttTopTabItemBothEdgeFocused:
        begin
          Part := TABP_TOPTABITEMBOTHEDGE;
          Base := Ord(ttTopTabItemBothEdgeNormal);
        end;
      ttPane:
        begin
          Part := TABP_PANE;
          Base := Ord(ttPane);
        end;
      ttBody:
        begin
          Part := TABP_BODY;
          Base := Ord(ttBody);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTaskBand): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTaskBand;
  with Result do
  begin
    case Detail of
      ttbGroupCount:
        begin
          Part := TDP_GROUPCOUNT;
          Base := Ord(ttbGroupCount);
        end;
      ttbFlashButton:
        begin
          Part := TDP_FLASHBUTTON;
          Base := Ord(ttbFlashButton);
        end;
      ttpFlashButtonGroupMenu:
        begin
          Part := TDP_FLASHBUTTONGROUPMENU;
          Base := Ord(ttpFlashButtonGroupMenu);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTaskBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTaskBar;
  with Result do
  begin
    case Detail of
      ttbTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(ttbTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedToolBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teToolBar;
  with Result do
  begin
    case Detail of
      ttbButtonNormal..ttbButtonCheckedHot:
        begin
          Part := TP_BUTTON;
          Base := Ord(ttbButtonNormal);
        end;
      ttbDropDownButtonNormal..ttbDropDownButtonCheckedHot:
        begin
          Part := TP_DROPDOWNBUTTON;
          Base := Ord(ttbDropDownButtonNormal);
        end;
      ttbSplitButtonNormal..ttbSplitButtonCheckedHot:
        begin
          Part := TP_SPLITBUTTON;
          Base := Ord(ttbSplitButtonNormal);
        end;
      ttbSplitButtonDropDownNormal..ttbSplitButtonDropDownCheckedHot:
        begin
          Part := TP_SPLITBUTTONDROPDOWN;
          Base := Ord(ttbSplitButtonDropDownNormal);
        end;
      ttbSeparatorNormal..ttbSeparatorCheckedHot:
        begin
          Part := TP_SEPARATOR;
          Base := Ord(ttbSeparatorNormal);
        end;
      ttbSeparatorVertNormal..ttbSeparatorVertCheckedHot:
        begin
          Part := TP_SEPARATORVERT;
          Base := Ord(ttbSeparatorVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedToolTip): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teToolTip;
  with Result do
  begin
    case Detail of
      tttStandardNormal..tttStandardLink:
        begin
          Part := TTP_STANDARD;
          Base := Ord(tttStandardNormal);
        end;
      tttStandardTitleNormal..tttStandardTitleLink:
        begin
          Part := TTP_STANDARDTITLE;
          Base := Ord(tttStandardTitleNormal);
        end;
      tttBaloonNormal..tttBaloonLink:
        begin
          Part := TTP_BALLOON;
          Base := Ord(tttBaloonNormal);
        end;
      tttBaloonTitleNormal..tttBaloonTitleLink:
        begin
          Part := TTP_BALLOONTITLE;
          Base := Ord(tttBaloonTitleNormal);
        end;
      tttCloseNormal..tttClosePressed:
        begin
          Part := TTP_CLOSE;
          Base := Ord(tttCloseNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTrackBar): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTrackBar;
  with Result do
  begin
    case Detail of
      ttbTrack:
        begin
          Part := TKP_TRACK;
          Base := Ord(ttbTrack);
        end;
      ttbTrackVert:
        begin
          Part := TKP_TRACKVERT;
          Base := Ord(ttbTrackVert);
        end;
      ttbThumbNormal..ttbThumbDisabled:
        begin
          Part := TKP_THUMB;
          Base := Ord(ttbThumbNormal);
        end;
      ttbThumbBottomNormal..ttbThumbBottomDisabled:
        begin
          Part := TKP_THUMBBOTTOM;
          Base := Ord(ttbThumbBottomNormal);
        end;
      ttbThumbTopNormal..ttbThumbTopDisabled:
        begin
          Part := TKP_THUMBTOP;
          Base := Ord(ttbThumbTopNormal);
        end;
      ttbThumbVertNormal..ttbThumbVertDisabled:
        begin
          Part := TKP_THUMBVERT;
          Base := Ord(ttbThumbVertNormal);
        end;
      ttbThumbLeftNormal..ttbThumbLeftDisabled:
        begin
          Part := TKP_THUMBLEFT;
          Base := Ord(ttbThumbLeftNormal);
        end;
      ttbThumbRightNormal..ttbThumbRightDisabled:
        begin
          Part := TKP_THUMBRIGHT;
          Base := Ord(ttbThumbRightNormal);
        end;
      ttbThumbTics:
        begin
          Part := TKP_TICS;
          Base := Ord(ttbThumbTics);
        end;
      ttbThumbTicsVert:
        begin
          Part := TKP_TICSVERT;
          Base := Ord(ttbThumbTicsVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTrayNotify): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTrayNotify;
  with Result do
  begin
    case Detail of
      ttnBackground:
        begin
          Part := TNP_BACKGROUND;
          Base := Ord(ttnBackground);
        end;
      ttnAnimBackground:
        begin
          Part := TNP_ANIMBACKGROUND;
          Base := Ord(ttnAnimBackground);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedTreeview): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teTreeView;
  with Result do
  begin
    case Detail of
      ttItemNormal..ttItemSelectedNotFocus:
        begin
          Part := TVP_TREEITEM;
          Base := Ord(ttItemNormal);
        end;
      ttGlyphClosed..ttGlyphOpened:
        begin
          Part := TVP_GLYPH;
          Base := Ord(ttGlyphClosed);
        end;
      ttBranch:
        begin
          Part := TVP_BRANCH;
          Base := Ord(ttBranch);
        end;
      ttHotGlyphClosed..ttHotGlyphOpened:
        begin
          Part := TVP_HOTGLYPH;
          Base := Ord(ttHotGlyphClosed);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.GetElementDetails(Detail: TThemedWindow): TThemedElementDetails;
var
  Base: Integer;
begin
  Result.Element := teWindow;
  with Result do
  begin
    case Detail of
      twCaptionActive..twCaptionDisabled:
        begin
          Part := WP_CAPTION;
          Base := Ord(twCaptionActive);
        end;
      twSmallCaptionActive..twSmallCaptionDisabled:
        begin
          Part := WP_SMALLCAPTION;
          Base := Ord(twSmallCaptionActive);
        end;
      twMinCaptionActive..twMinCaptionDisabled:
        begin
          Part := WP_MINCAPTION;
          Base := Ord(twMinCaptionActive);
        end;
      twSmallMinCaptionActive..twSmallMinCaptionDisabled:
        begin
          Part := WP_SMALLMINCAPTION;
          Base := Ord(twSmallMinCaptionActive);
        end;
      twMaxCaptionActive..twMaxCaptionDisabled:
        begin
          Part := WP_MAXCAPTION;
          Base := Ord(twMaxCaptionActive);
        end;
      twSmallMaxCaptionActive..twSmallMaxCaptionDisabled:
        begin
          Part := WP_SMALLMAXCAPTION;
          Base := Ord(twSmallMaxCaptionActive);
        end;
      twFrameLeftActive..twFrameLeftInactive:
        begin
          Part := WP_FRAMELEFT;
          Base := Ord(twFrameLeftActive);
        end;
      twFrameRightActive..twFrameRightInactive:
        begin
          Part := WP_FRAMERIGHT;
          Base := Ord(twFrameRightActive);
        end;
      twFrameBottomActive..twFrameBottomInactive:
        begin
          Part := WP_FRAMEBOTTOM;
          Base := Ord(twFrameBottomActive);
        end;
      twSmallFrameLeftActive..twSmallFrameLeftInactive:
        begin
          Part := WP_SMALLFRAMELEFT;
          Base := Ord(twSmallFrameLeftActive);
        end;
      twSmallFrameRightActive..twSmallFrameRightInactive:
        begin
          Part := WP_SMALLFRAMERIGHT;
          Base := Ord(twSmallFrameRightActive);
        end;
      twSmallFrameBottomActive..twSmallFrameBottomInactive:
        begin
          Part := WP_SMALLFRAMEBOTTOM;
          Base := Ord(twSmallFrameBottomActive);
        end;
      twSysButtonNormal..twSysButtonInactive:
        begin
          Part := WP_SYSBUTTON;
          Base := Ord(twSysButtonNormal);
        end;
      twMDISysButtonNormal..twMDISysButtonInactive:
        begin
          Part := WP_MDISYSBUTTON;
          Base := Ord(twMDISysButtonNormal);
        end;
      twMinButtonNormal..twMinButtonInactive:
        begin
          Part := WP_MINBUTTON;
          Base := Ord(twMinButtonNormal);
        end;
      twMDIMinButtonNormal..twMDIMinButtonInactive:
        begin
          Part := WP_MDIMINBUTTON;
          Base := Ord(twMDIMinButtonNormal);
        end;
      twMaxButtonNormal..twMaxButtonInactive:
        begin
          Part := WP_MAXBUTTON;
          Base := Ord(twMaxButtonNormal);
        end;
      twCloseButtonNormal..twCloseButtonInactive:
        begin
          Part := WP_CLOSEBUTTON;
          Base := Ord(twCloseButtonNormal);
        end;
      twSmallCloseButtonNormal..twSmallCloseButtonInactive:
        begin
          Part := WP_SMALLCLOSEBUTTON;
          Base := Ord(twSmallCloseButtonNormal);
        end;
      twMDICloseButtonNormal..twMDICloseButtonInactive:
        begin
          Part := WP_MDICLOSEBUTTON;
          Base := Ord(twMDICloseButtonNormal);
        end;
      twRestoreButtonNormal..twRestoreButtonInactive:
        begin
          Part := WP_RESTOREBUTTON;
          Base := Ord(twRestoreButtonNormal);
        end;
      twMDIRestoreButtonNormal..twMDIRestoreButtonInactive:
        begin
          Part := WP_MDIRESTOREBUTTON;
          Base := Ord(twMDIRestoreButtonNormal);
        end;
      twHelpButtonNormal..twHelpButtonInactive:
        begin
          Part := WP_HELPBUTTON;
          Base := Ord(twHelpButtonNormal);
        end;
      twMDIHelpButtonNormal..twMDIHelpButtonInactive:
        begin
          Part := WP_MDIHELPBUTTON;
          Base := Ord(twMDIHelpButtonNormal);
        end;
      twHorzScrollNormal..twHorzScrollDisabled:
        begin
          Part := WP_HORZSCROLL;
          Base := Ord(twHorzScrollNormal);
        end;
      twHorzThumbNormal..twHorzThumbDisabled:
        begin
          Part := WP_HORZTHUMB;
          Base := Ord(twHorzThumbNormal);
        end;
      twVertScrollNormal..twVertScrollDisabled:
        begin
          Part := WP_VERTSCROLL;
          Base := Ord(twVertScrollNormal);
        end;
      twVertThumbNormal..twVertThumbDisabled:
        begin
          Part := WP_VERTTHUMB;
          Base := Ord(twVertThumbNormal);
        end;
      twDialog:
        begin
          Part := WP_DIALOG;
          Base := Ord(twDialog);
        end;
      twCaptionSizingTemplate:
        begin
          Part := WP_CAPTIONSIZINGTEMPLATE;
          Base := Ord(twCaptionSizingTemplate);
        end;
      twSmallCaptionSizingTemplate:
        begin
          Part := WP_SMALLCAPTIONSIZINGTEMPLATE;
          Base := Ord(twSmallCaptionSizingTemplate);
        end;
      twFrameLeftSizingTemplate:
        begin
          Part := WP_FRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twFrameLeftSizingTemplate);
        end;
      twSmallFrameLeftSizingTemplate:
        begin
          Part := WP_SMALLFRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameLeftSizingTemplate);
        end;
      twFrameRightSizingTemplate:
        begin
          Part := WP_FRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twFrameRightSizingTemplate);
        end;
      twSmallFrameRightSizingTemplate:
        begin
          Part := WP_SMALLFRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameRightSizingTemplate);
        end;
      twFrameBottomSizingTemplate:
        begin
          Part := WP_FRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twFrameBottomSizingTemplate);
        end;
      twSmallFrameBottomSizingTemplate:
        begin
          Part := WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twSmallFrameBottomSizingTemplate);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Detail) - Base + 1;
  end;
end;

function TThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
begin
  // default values here
  // -1 mean that we do not know size of detail
  Result := Size(-1, -1);
  case Details.Element of
    teButton:
      if Details.Part in [BP_RADIOBUTTON, BP_CHECKBOX] then
        Result := Size(13, 13)
      else
      if Details.Part = BP_PUSHBUTTON then
        Result := Size(75, 23);
    teRebar:
      if Details.Part = RP_GRIPPER then
        Result.cy := 30
      else
      if Details.Part = RP_GRIPPERVERT then
        Result.cx := 30;
    teToolBar:
      if Details.Part in [TP_SPLITBUTTONDROPDOWN, TP_DROPDOWNBUTTON] then
        Result.cx := 12;
    teTreeView:
      if Details.Part in [TVP_GLYPH, TVP_HOTGLYPH] then
        Result := Size(9, 9);
    teWindow:
      if Details.Part in [WP_SMALLCLOSEBUTTON, WP_MDICLOSEBUTTON, WP_MDIHELPBUTTON, WP_MDIMINBUTTON, WP_MDIRESTOREBUTTON, WP_MDISYSBUTTON] then
        Result := Size(9, 9);
  end;
end;

function TThemeServices.GetDetailRegion(DC: HDC;
  Details: TThemedElementDetails; const R: TRect): HRGN;
begin
  Result := 0;
end;

function TThemeServices.GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean;
begin
  Result := False;
end;

function TThemeServices.GetOption(AOption: TThemeOption): Integer;
begin
  case AOption of
    toShowButtonImages: Result := 1;
    toShowMenuImages: Result := 1;
    toUseGlyphEffects:
    begin
      // toUseGlyphEffects seems to be OS-dependent.
      //   Linux: yes
      //   Win, OSX: no
      {$IFDEF LINUX}
      Result := 1;
      {$ELSE}
      Result := 0;
      {$ENDIF}
    end;
  else
    Result := 0;
  end;
end;

function TThemeServices.GetTextExtent(DC: HDC; Details: TThemedElementDetails;
  const S: String; Flags: Cardinal; BoundingRect: PRect): TRect;
begin
  if Assigned(BoundingRect) then
    Result := BoundingRect^
  else
    Result := Rect(0, 0, 0, 0);
  LCLIntf.DrawText(DC, PChar(S), Length(S), Result, DT_CALCRECT or Flags);
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.ColorToRGB(Color: LongInt; Details: PThemedElementDetails = nil): COLORREF;
begin
  if (Color and $80000000 = 0) then
    Result := Color
  else
  if Details = nil then
    Result := Graphics.ColorToRGB(Color)
  else
    Result := InternalColorToRGB(Details^, Color);
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  Result := BoundingRect;
  if Details.Element in [teHeader, teButton] then
    InflateRect(Result, -2, -2)
  else
    InflateRect(Result, -1, -1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  AContentRect: PRect = nil);
var
  ARect: TRect;
begin
  // deafult painting
  ARect := R;
  WidgetSet.DrawEdge(DC, ARect, Edge, Flags);
  if (Flags and DFCS_ADJUSTRECT <> 0) and (AContentRect <> nil) then
    AContentRect^ := R;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil);

  procedure DrawDropDownArrow(const DropDownButtonRect: TRect);
  const
    cArrowWidth = 10;
  var
    ArrowRect: TRect;
    Points: array[1..3] of TPoint;
    OldBrush, Brush: HBrush;
  begin
    ArrowRect := DropDownButtonRect;
    ArrowRect.Left := (DropDownButtonRect.Left+DropDownButtonRect.Right-cArrowWidth-1) div 2;
    ArrowRect.Right := ArrowRect.Left+cArrowWidth;
    ArrowRect.Left := ArrowRect.Left + 3;
    ArrowRect.Right := Max(ArrowRect.Right - 3, ArrowRect.Left);
    ArrowRect.Top := (DropDownButtonRect.Top + DropDownButtonRect.Bottom +
                      ArrowRect.Left - ArrowRect.Right) div 2;
    ArrowRect.Bottom := ArrowRect.Top + Min(2, ArrowRect.Right - ArrowRect.Left);
    Points[1] := Point(ArrowRect.Left, ArrowRect.Top);
    Points[2] := Point((ArrowRect.Left + ArrowRect.Right) div 2, ArrowRect.Bottom);
    Points[3] := Point(ArrowRect.Right, ArrowRect.Top);
    Brush := CreateSolidBrush(clBlack);
    OldBrush := SelectObject(DC, Brush);
    Polygon(Dc, @Points[1], 3, False);
    DeleteObject(SelectObject(DC, OldBrush));
  end;
  
  procedure DrawVLine(ARect: TRect);
  begin
    ARect.Left := ((ARect.Left + ARect.Right) div 2) - 1;
    ARect.Right := ARect.Left + 2;
    inc(ARect.Top, 2);
    inc(ARect.Bottom, 2);
    LCLIntf.DrawEdge(DC, ARect, EDGE_ETCHED, BF_LEFT);
  end;

  procedure DrawHLine(ARect: TRect);
  begin
    ARect.Top := ((ARect.Top + ARect.Bottom) div 2) - 1;
    ARect.Bottom := ARect.Top + 2;
    inc(ARect.Left, 2);
    inc(ARect.Right, 2);
    LCLIntf.DrawEdge(DC, ARect, EDGE_ETCHED, BF_LEFT);
  end;
  
  procedure FillWithColor(ARect: TRect; AColor: TColor);
  var
    Brush: HBrush;
  begin
    Brush := CreateSolidBrush(ColorToRGB(AColor));
    FillRect(DC, ARect, Brush);
    DeleteObject(Brush);
  end;

  procedure FillWithDottedBrush(ARect: TRect; Color1, Color2: TColor);
  var
    OldColor1, OldColor2: TColorRef;
  begin
    OldColor1 := SetBkColor(DC, ColorToRGB(Color1));
    OldColor2 := SetTextColor(DC, ColorToRGB(Color2));
    FillRect(DC, ARect, DottedBrush);
    SetBkColor(DC, OldColor1);
    SetTextColor(DC, OldColor2);
  end;
  
var
  ADrawFlags: DWord;
  Bevel: TGraphicsBevelCut;
  ARect: TRect;
  Tmp: Integer;
begin
  // default painting
  ARect := R; // in order to pass by reference
  case Details.Element of
    teButton:
      begin
        ADrawFlags := DFCS_BUTTONPUSH;
        if Details.Element = teButton then
        begin
          case Details.Part of
            BP_RADIOBUTTON: ADrawFlags := DFCS_BUTTONRADIO;
            BP_CHECKBOX:
              if IsMixed(Details) then
                ADrawFlags := DFCS_BUTTON3STATE
              else
                ADrawFlags := DFCS_BUTTONCHECK;
          end;
        end;

        if IsDisabled(Details) then
          ADrawFlags := ADrawFlags or DFCS_INACTIVE else
        if IsPushed(Details) then
          ADrawFlags := ADrawFlags or DFCS_PUSHED else
        if IsHot(Details) then
          ADrawFlags := ADrawFlags or DFCS_HOT;
        if IsChecked(Details) or IsMixed(Details) then
          ADrawFlags := ADrawFlags or DFCS_CHECKED;

        WidgetSet.DrawFrameControl(DC, ARect, DFC_BUTTON, ADrawFlags);
      end;
    teHeader:
      begin
        ADrawFlags := DFCS_BUTTONPUSH;
        if IsDisabled(Details) then
          ADrawFlags := ADrawFlags or DFCS_INACTIVE else
        if IsPushed(Details) then
          ADrawFlags := ADrawFlags or DFCS_PUSHED else
        if IsHot(Details) then
          ADrawFlags := ADrawFlags or DFCS_HOT;

        WidgetSet.DrawFrameControl(DC, ARect, DFC_BUTTON, ADrawFlags);
      end;
    teToolBar:
      begin
        case Details.Part of
          TP_BUTTON,
          TP_DROPDOWNBUTTON,
          TP_SPLITBUTTON:
            begin
              if IsPushed(Details) or IsChecked(Details) then
                Bevel := bvLowered
              else
              if IsHot(Details) then
                Bevel := bvRaised
              else
                Bevel := bvNone;

              Frame3D(DC, ARect, 1, Bevel);

              if IsChecked(Details) and not IsHot(Details) then
              begin
                InflateRect(ARect, -2, -2);
                FillWithDottedBrush(ARect, clBtnHighlight, clBtnFace)
              end;
            end;
          TP_SPLITBUTTONDROPDOWN:
            begin
              if IsPushed(Details) or IsChecked(Details) then
                Bevel := bvLowered
              else
              if IsHot(Details) then
                Bevel := bvRaised
              else
                Bevel := bvNone;

              Frame3D(DC, ARect, 1, Bevel);
              ARect:=R;
              InflateRect(ARect, -1, -1);
              DrawDropDownArrow(ARect);
            end;
          TP_SEPARATOR:
            DrawVline(ARect);
          TP_SEPARATORVERT:
            DrawHline(ARect);
        end;
      end;
    teWindow:
      begin
        case Details.Part of
          WP_MINBUTTON,
          WP_MDIMINBUTTON    : ADrawFlags := DFCS_CAPTIONMIN;
          WP_MAXBUTTON       : ADrawFlags := DFCS_CAPTIONMAX;
          WP_CLOSEBUTTON,
          WP_SMALLCLOSEBUTTON,
          WP_MDICLOSEBUTTON  : ADrawFlags := DFCS_CAPTIONCLOSE;
          WP_RESTOREBUTTON,
          WP_MDIRESTOREBUTTON: ADrawFlags := DFCS_CAPTIONRESTORE;
          WP_HELPBUTTON,
          WP_MDIHELPBUTTON   : ADrawFlags := DFCS_CAPTIONHELP;
        else
          ADrawFlags := 0;
        end;
        
        if Details.Part in [WP_MDIMINBUTTON, WP_MDIRESTOREBUTTON, WP_MDICLOSEBUTTON] then
          ADrawFlags := ADrawFlags or DFCS_FLAT;

        if IsDisabled(Details) then
          ADrawFlags := ADrawFlags or DFCS_INACTIVE else
        if IsPushed(Details) then
          ADrawFlags := ADrawFlags or DFCS_PUSHED else
        if IsHot(Details) then
          ADrawFlags := ADrawFlags or DFCS_HOT;

        WidgetSet.DrawFrameControl(DC, ARect, DFC_CAPTION, ADrawFlags);
      end;
    teTab:
      begin
        if Details.Part in [TABP_PANE, TABP_BODY] then
          FillWithColor(ARect, clBackground);
      end;
    teTreeView:
      begin
        if Details.Part in [TVP_GLYPH, TVP_HOTGLYPH] then
        begin
          with ARect do
          begin
            if not odd(Right - Left) then
              Dec(Right);
            if not odd(Bottom - Top) then
              Dec(Bottom);
          end;
          Rectangle(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
          Tmp := Integer((ARect.Bottom + ARect.Top) shr 1);
          MoveToEx(DC, ARect.Left + 2, Tmp, nil);
          LineTo(DC, ARect.Right - 2, Tmp);
          if Details.State = GLPS_CLOSED then
          begin
            Tmp := (ARect.Left + ARect.Right) shr 1;
            MoveToEx(DC, Tmp, ARect.Top + 2, nil);
            LineTo(DC, Tmp, ARect.Bottom - 2);
          end;
        end
        else
        if Details.Part = TVP_TREEITEM then
        begin
          case Details.State of
            TREIS_NORMAL: FillWithColor(ARect, clWindow);
            TREIS_HOT: FillWithColor(ARect, clHotLight);
            TREIS_SELECTED: FillWithColor(ARect, clHighlight);
            TREIS_DISABLED: FillWithColor(ARect, clWindow);
            TREIS_SELECTEDNOTFOCUS: FillWithColor(ARect, clBtnFace);
            TREIS_HOTSELECTED: FillWithColor(ARect, clHighlight);
          end;
        end;
      end;
    teToolTip:
      begin
        if Details.Part = TTP_STANDARD then
        begin
          FillWithColor(ARect, clInfoBk);
          LCLIntf.DrawEdge(DC, ARect, BDR_RAISEDOUTER, BF_RECT);
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin
  // overrided in TWin32ThemeServices (for compatability with Delphi)
end;

procedure TThemeServices.DrawIcon(ACanvas: TPersistent;
  Details: TThemedElementDetails; const P: TPoint; AImageList: TPersistent;
  Index: Integer);
var
  AEffect: TGraphicsDrawEffect;
begin
  if not Assigned(ThemesImageDrawEvent) then exit;
  if IsDisabled(Details) then
    AEffect := gdeDisabled
  else
  if IsPushed(Details) then
    AEffect := gdeShadowed
  else
  if IsHot(Details) then
    AEffect := gdeHighlighted
  else
    AEffect := gdeNormal;
  ThemesImageDrawEvent(AImageList, ACanvas, P.X, P.Y, Index, AEffect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawParentBackground(Window: HWND; Target: HDC; Details: PThemedElementDetails;
  OnlyIfTransparent: Boolean; Bounds: PRect = nil);
var
  DoDraw: Boolean;
begin
  if OnlyIfTransparent and Assigned(Details) then
    DoDraw := HasTransparentParts(Details^)
  else
    DoDraw := True;
    
  if DoDraw then
    InternalDrawParentBackground(Window, Target, Bounds);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
begin
  // overrided in TWin32ThemeServices (for compatability with Delphi)
end;

function TThemeServices.IsDisabled(Details: TThemedElementDetails): Boolean;
begin
  // teHeader should not be here, it has no disabled state

  Result := False;
  if (Details.Element in [teButton, teToolBar, teComboBox]) or
     ((Details.Element = teRebar) and (Details.Part >= RP_BAND)) or
     ((Details.Element = teWindow) and (Details.Part >= WP_SYSBUTTON) and
     (Details.Part <= WP_MDIHELPBUTTON)) then
    Result := (Details.State mod 4) = 0; // usual disabled = 4 / 8 / 12
end;

function TThemeServices.IsPushed(Details: TThemedElementDetails): Boolean;
begin
  Result := False;
  if (Details.Element in [teButton, teToolBar, teHeader, teComboBox]) or
     ((Details.Element = teRebar) and (Details.Part >= RP_BAND)) or
     ((Details.Element = teWindow) and (Details.Part >= WP_SYSBUTTON) and
     (Details.Part <= WP_MDIHELPBUTTON)) then
    Result := Details.State in [3, 7, 11];
end;

function TThemeServices.IsHot(Details: TThemedElementDetails): Boolean;
begin
  Result := False;
  if (Details.Element in [teButton, teToolBar, teHeader, teComboBox]) or
     ((Details.Element = teRebar) and (Details.Part >= RP_BAND)) or
     ((Details.Element = teWindow) and (Details.Part >= WP_SYSBUTTON) and
     (Details.Part <= WP_MDIHELPBUTTON)) then
    Result := Details.State in [2, 6, 10]
  else
  if (Details.Element = teTreeview) and (Details.Part = TVP_HOTGLYPH) then
    Result := True;
end;

function TThemeServices.IsChecked(Details: TThemedElementDetails): Boolean;
begin
  Result := False;
  if (Details.Element in [teButton]) then
    Result := (Details.State > 4) and (Details.State < 9)
  else
  if (Details.Element in [teToolBar]) then
    Result := (Details.State > 4);
end;

function TThemeServices.IsMixed(Details: TThemedElementDetails): Boolean;
begin
  Result := False;
  if (Details.Element in [teButton]) then
    Result := Details.State > 8;
end;

function TThemeServices.InitThemes: Boolean;
begin
  Result := False;
end;

procedure TThemeServices.UnloadThemeData;
begin
end;

function TThemeServices.UseThemes: Boolean;
begin
  Result := False;
end;

function TThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := False;
end;

function TThemeServices.InternalColorToRGB(Details: TThemedElementDetails;
  Color: LongInt): COLORREF;
begin
  Result := Graphics.ColorToRGB(Color);
end;

procedure TThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // default painting
end;

procedure TThemeServices.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
var
  Canvas: TCanvas absolute ACanvas;
  TXTStyle: TTextStyle;
  OldColor: TColor;
begin
  TXTStyle := Canvas.TextStyle;
  TXTStyle.Opaque := False;
  TXTStyle.Clipping := (Flags and DT_NOCLIP) = 0;
  TXTStyle.ShowPrefix := (Flags and DT_NOPREFIX) = 0;
  TXTStyle.SingleLine := (Flags and DT_SINGLELINE) <> 0;

  if (Flags and DT_CENTER) <> 0 then
    TXTStyle.Alignment := taCenter
  else
  if (Flags and DT_RIGHT) <> 0 then
    TXTStyle.Alignment := taRightJustify
  else
    TXTStyle.Alignment := taLeftJustify;

  if (Flags and DT_VCENTER) <> 0 then
    TXTStyle.Layout := tlCenter
  else
  if (Flags and DT_BOTTOM) <> 0 then
    TXTStyle.Layout := tlBottom
  else
    TXTStyle.Layout := tlTop;
  TXTStyle.RightToLeft := (Flags and DT_RTLREADING) <> 0;
    // set color here, otherwise SystemFont is wrong if the button was disabled before
  TXTStyle.SystemFont := Canvas.Font.IsDefault;//Match System Default Style

  TXTStyle.Wordbreak := (Flags and DT_WORDBREAK) <> 0;
  if not TXTStyle.Wordbreak then
    TXTStyle.EndEllipsis := (Flags and DT_END_ELLIPSIS) <> 0
  else
    TXTStyle.EndEllipsis := False;

  OldColor := Canvas.Font.Color;
  if IsDisabled(Details) then
  begin
    Canvas.Font.Color := clBtnHighlight;
    OffsetRect(R, 1, 1);
    Canvas.TextRect(R, R.Left, R.Top, S, TXTStyle);
    Canvas.Font.Color := clBtnShadow;
    OffsetRect(R, -1, -1);
  end;
  if (Details.Element = teTreeview) and (Details.Part = TVP_TREEITEM) then
  begin
    case Details.State of
      TREIS_SELECTED,
      TREIS_HOTSELECTED: Canvas.Font.Color := clHighlightText;
      TREIS_SELECTEDNOTFOCUS: Canvas.Font.Color := clBtnText;
    else
      Canvas.Font.Color := clWindowText;
    end;
  end;
  Canvas.TextRect(R, R.Left, R.Top, S, TXTStyle);
  Canvas.Font.Color := OldColor;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.PaintBorder(Control: TObject; EraseLRCorner: Boolean);
begin
  // default painting
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeServices.UpdateThemes;
begin
  if FUseThemes then
    UnloadThemeData;
  FUseThemes := UseThemes;

  if FUseThemes then
    FThemedControlsEnabled := ThemedControlsEnabled
  else
    FThemedControlsEnabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
