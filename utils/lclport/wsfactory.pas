{
 *****************************************************************************
 *                               WSFactory.pas                               *
 *                               -------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSFactory;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface

{ These functions are implemented by the LCL backends.
  If you get a linker error for one of these functions you probably forgot
  to add the unit "interfaces" to your program.
  The unit interfaces must be added as one of the first units of the main program.

  Example for a linker error:
    /path/to/lazarus/lcl/units/x86_64-linux/wsimglist.o: In function `REGISTERCUSTOMIMAGELIST':
    /path/to/lazarus/lcl//widgetset/wsimglist.pp:266: undefined reference to `WSRegisterCustomImageList'
}

// imglist
function WSRegisterCustomImageList: Boolean; external name 'WSRegisterCustomImageList';
// controls
function WSRegisterDragImageList: Boolean;   external name 'WSRegisterDragImageList';
function WSRegisterLazAccessibleObject: Boolean; external name 'WSRegisterLazAccessibleObject';
function WSRegisterControl: Boolean;         external name 'WSRegisterControl';
function WSRegisterWinControl: Boolean;      external name 'WSRegisterWinControl';
function WSRegisterGraphicControl: Boolean;  external name 'WSRegisterGraphicControl';
function WSRegisterCustomControl: Boolean;   external name 'WSRegisterCustomControl';
// comctrls
function WSRegisterStatusBar: Boolean;         external name 'WSRegisterStatusBar';
function WSRegisterTabSheet: Boolean;          external name 'WSRegisterTabSheet';
function WSRegisterPageControl: Boolean;       external name 'WSRegisterPageControl';
function WSRegisterCustomListView: Boolean;    external name 'WSRegisterCustomListView';
function WSRegisterCustomProgressBar: Boolean; external name 'WSRegisterCustomProgressBar';
function WSRegisterCustomUpDown: Boolean;      external name 'WSRegisterCustomUpDown';
function WSRegisterCustomToolButton: Boolean;  external name 'WSRegisterCustomToolButton';
function WSRegisterToolBar: Boolean;           external name 'WSRegisterToolBar';
function WSRegisterCustomTrackBar: Boolean;    external name 'WSRegisterCustomTrackBar';
function WSRegisterCustomTreeView: Boolean;    external name 'WSRegisterCustomTreeView';
// calender
function WSRegisterCustomCalendar: Boolean;    external name 'WSRegisterCustomCalendar';
// dialogs
function WSRegisterCommonDialog: Boolean;      external name 'WSRegisterCommonDialog';
function WSRegisterFileDialog: Boolean;        external name 'WSRegisterFileDialog';
function WSRegisterOpenDialog: Boolean;        external name 'WSRegisterOpenDialog';
function WSRegisterSaveDialog: Boolean;        external name 'WSRegisterSaveDialog';
function WSRegisterSelectDirectoryDialog: Boolean; external name 'WSRegisterSelectDirectoryDialog';
function WSRegisterColorDialog: Boolean;       external name 'WSRegisterColorDialog';
function WSRegisterColorButton: Boolean;       external name 'WSRegisterColorButton';
function WSRegisterFontDialog: Boolean;        external name 'WSRegisterFontDialog';
// StdCtrls
function WSRegisterCustomScrollBar: Boolean;   external name 'WSRegisterCustomScrollBar';
function WSRegisterCustomGroupBox: Boolean;    external name 'WSRegisterCustomGroupBox';
function WSRegisterCustomComboBox: Boolean;    external name 'WSRegisterCustomComboBox';
function WSRegisterCustomListBox: Boolean;     external name 'WSRegisterCustomListBox';
function WSRegisterCustomEdit: Boolean;        external name 'WSRegisterCustomEdit';
function WSRegisterCustomMemo: Boolean;        external name 'WSRegisterCustomMemo';
function WSRegisterButtonControl: Boolean;     external name 'WSRegisterButtonControl';
function WSRegisterCustomButton: Boolean;      external name 'WSRegisterCustomButton';
function WSRegisterCustomCheckBox: Boolean;    external name 'WSRegisterCustomCheckBox';
function WSRegisterToggleBox: Boolean;         external name 'WSRegisterToggleBox';
function WSRegisterRadioButton: Boolean;       external name 'WSRegisterRadioButton';
function WSRegisterCustomStaticText: Boolean;  external name 'WSRegisterCustomStaticText';
function WSRegisterCustomLabel: Boolean;       external name 'WSRegisterCustomLabel';
// extctrls
function WSRegisterCustomPage: Boolean;        external name 'WSRegisterCustomPage';
function WSRegisterCustomNotebook: Boolean;    external name 'WSRegisterCustomNotebook';
function WSRegisterShape: Boolean;             external name 'WSRegisterShape';
function WSRegisterCustomSplitter: Boolean;    external name 'WSRegisterCustomSplitter';
function WSRegisterPaintBox: Boolean;          external name 'WSRegisterPaintBox';
function WSRegisterCustomImage: Boolean;       external name 'WSRegisterCustomImage';
function WSRegisterBevel: Boolean;             external name 'WSRegisterBevel';
function WSRegisterCustomRadioGroup: Boolean;  external name 'WSRegisterCustomRadioGroup';
function WSRegisterCustomCheckGroup: Boolean;  external name 'WSRegisterCustomCheckGroup';
function WSRegisterCustomLabeledEdit: Boolean; external name 'WSRegisterCustomLabeledEdit';
function WSRegisterCustomPanel: Boolean;       external name 'WSRegisterCustomPanel';
function WSRegisterCustomTrayIcon: Boolean;    external name 'WSRegisterCustomTrayIcon';
//ExtDlgs
function WSRegisterPreviewFileControl: Boolean; external name 'WSRegisterPreviewFileControl';
function WSRegisterPreviewFileDialog: Boolean; external name 'WSRegisterPreviewFileDialog';
function WSRegisterOpenPictureDialog: Boolean; external name 'WSRegisterOpenPictureDialog';
function WSRegisterSavePictureDialog: Boolean; external name 'WSRegisterSavePictureDialog';
function WSRegisterCalculatorDialog: Boolean;  external name 'WSRegisterCalculatorDialog';
function WSRegisterCalculatorForm: Boolean;    external name 'WSRegisterCalculatorForm';
//function WSRegisterCalendarDialogForm: Boolean; external name 'WSRegisterCalendarDialogForm';
function WSRegisterCalendarDialog: Boolean;    external name 'WSRegisterCalendarDialog';
// Buttons
function WSRegisterCustomBitBtn: Boolean;      external name 'WSRegisterCustomBitBtn';
function WSRegisterCustomSpeedButton: Boolean; external name 'WSRegisterCustomSpeedButton';
// CheckLst
function WSRegisterCustomCheckListBox: Boolean; external name 'WSRegisterCustomCheckListBox';
// Forms
function WSRegisterScrollingWinControl: Boolean;external name 'WSRegisterScrollingWinControl';
function WSRegisterScrollBox: Boolean;          external name 'WSRegisterScrollBox';
function WSRegisterCustomFrame: Boolean;        external name 'WSRegisterCustomFrame';
function WSRegisterCustomForm: Boolean;         external name 'WSRegisterCustomForm';
function WSRegisterHintWindow: Boolean;         external name 'WSRegisterHintWindow';
// Grids
function WSRegisterCustomGrid: Boolean;         external name 'WSRegisterCustomGrid';
// Menus
function WSRegisterMenuItem: Boolean;           external name 'WSRegisterMenuItem';
function WSRegisterMenu: Boolean;               external name 'WSRegisterMenu';
function WSRegisterMainMenu: Boolean;           external name 'WSRegisterMainMenu';
function WSRegisterPopupMenu: Boolean;          external name 'WSRegisterPopupMenu';
// PairSplitter
function WSRegisterPairSplitterSide: Boolean;   external name 'WSRegisterPairSplitterSide';
function WSRegisterCustomPairSplitter: Boolean; external name 'WSRegisterCustomPairSplitter';
// Spin
function WSRegisterCustomFloatSpinEdit: Boolean;external name 'WSRegisterCustomFloatSpinEdit';
// RubberBand
function WSRegisterCustomRubberBand: Boolean;   external name 'WSRegisterCustomRubberBand';
// LazDeviceAPIs

implementation

end.

