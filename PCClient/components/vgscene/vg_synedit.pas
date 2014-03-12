{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

This is port of SynEdit to VGScene (www.ksdev.com) by Eugene Kryukov
  Latest changes can be found: http://www.ksdev.com/mplfiles/
-------------------------------------------------------------------------------}

unit vg_synedit;

interface

uses
  Controls,
  Contnrs,
  Graphics,
  Windows,
  StdActns,
  Dialogs,
  SynEditTypes,
  SynEditKeyConst,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
  SynUnicode,
  Math,
  SysUtils,
  Classes,
  SynEdit,
  vg_scene,
  vg_memo,
  vg_layouts;

type
  TvgCustomSynEdit = class(TvgScrollBox)
  private
  private
    fAlwaysShowCaret: Boolean;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    fCaretX: Integer;
    fLastCaretX: integer;
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: single;
    FFont: TvgFont;
    FFontFill: TvgBrush;
    FSelectionFill: TvgBrush;
    fInserting: Boolean;
    fLines: TUnicodeStrings;
    fOrigLines: TUnicodeStrings;
    fOrigUndoList: TSynEditUndoList;
    fOrigRedoList: TSynEditUndoList;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fMaxScrollWidth: Integer;
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    fScrollHintColor: TColor;
    fScrollHintFormat: TScrollHintFormat;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    fActiveLineColor: TColor;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    fBookMarks: array[0..9] of TSynEditMark; // these are just references, fMarkList is the owner 
    fMouseDownX: single;
    fMouseDownY: single;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TSynBorderStyle;
    fHideSelection: Boolean;
    fMouseWheelAccumulator: Integer;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TvgPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fModified: Boolean;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: Integer;
    fSelectionMode: TSynSelectionMode;
    fActiveSelectionMode: TSynSelectionMode; //mode of the active selection
    fWantReturns: Boolean;
    fWantTabs: Boolean;
    fWordWrapPlugin: ISynEditBufferPlugin;
    fWordWrapGlyph: TSynGlyph;
    fCaretAtEOL: Boolean; // used by wordwrap

    fGutter: TSynGutter;
    fTabWidth: Integer;
    fInvalidateRect: TvgRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fSearchEngine: TSynEditSearchCustom;
    fHookedCommandHandlers: TObjectList;
    fKbdHandler: TSynEditKbdHandler;
    fFocusList: TList;
    fPlugins: TObjectList;
    fScrollDeltaX, fScrollDeltaY: Integer;
    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    FOnKeyPressW: TKeyPressWEvent;
    fOnMouseCursor: TMouseCursorEvent;
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnContextHelp: TContextHelpEvent;
    fOnPaintTransient: TPaintTransient;
    fOnGutterGetText: TGutterGetTextEvent;
    fOnGutterPaint: TGutterPaintEvent;

    fOnStatusChange: TStatusChangeEvent;
    fShowSpecChar: Boolean;
    FPaintTransientLock: Integer;
    FIsScrolling: Boolean;

    fChainListCleared: TNotifyEvent;
    fChainListDeleted: TStringListChangeEvent;
    fChainListInserted: TStringListChangeEvent;
    fChainListPutted: TStringListChangeEvent;
    fChainLinesChanging: TNotifyEvent;
    fChainLinesChanged: TNotifyEvent;
    fChainedEditor: TvgCustomSynEdit;
    fChainUndoAdded: TNotifyEvent;
    fChainRedoAdded: TNotifyEvent;

    FAdditionalWordBreakChars: TSysCharSet;
    FAdditionalIdentChars: TSysCharSet;
    fSearchNotFound: TCustomSynEditSearchNotFoundEvent;
    OnFindBeforeSearch: TNotifyEvent;
    OnReplaceBeforeSearch: TNotifyEvent;
    OnCloseBeforeSearch: TNotifyEvent;
    SelStartBeforeSearch: integer;
    SelLengthBeforeSearch: integer;
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure ComputeScroll(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(Selection:boolean);
    procedure DoEndKey(Selection: Boolean);
    procedure DoLinesDeleted(FirstLine, Count: integer);
    procedure DoLinesInserted(FirstLine, Count: integer);
    procedure DoShiftTabKey;
    procedure DoTabKey;
    procedure DoCaseChange(const Cmd : TSynEditorCommand);
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    procedure SynFontChanged(Sender: TObject);
    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TBufferCoord;
    function GetDisplayX: Integer;
    function GetDisplayY: Integer;
    function GetDisplayXY: TDisplayCoord;
    function GetDisplayLineCount: Integer;
    function GetHookedCommandHandlersCount: Integer;
    function GetLineText: WideString;
    function GetMaxUndo: Integer;
    function GetOptions: TSynEditorOptions;
    function GetSelAvail: Boolean;
    function GetSelTabBlock: Boolean;
    function GetSelTabLine: Boolean;
    function GetSelText: WideString;
    function SynGetText: WideString;
    function GetWordAtCursor: WideString;
    function GetWordAtMouse: WideString;
    function GetWordWrap: Boolean;
    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PWideChar; AddToUndoList: Boolean);
    function LeftSpaces(const Line: WideString): Integer;
    function LeftSpacesEx(const Line: WideString; WantTabs: Boolean): Integer;
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): WideString;
    procedure LinesChanging(Sender: TObject);
    procedure MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
      SelectionCommand: Boolean);
    procedure MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
    procedure PluginsAfterPaint(ACanvas: TvgCanvas; const AClip: TvgRect;
      FirstLine, LastLine: Integer);
    procedure ReadAddedKeystrokes(Reader: TReader);
    procedure ReadRemovedKeystrokes(Reader: TReader);
    function ScanFrom(Index: Integer): Integer;
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure InternalSetCaretX(Value: Integer);
    procedure InternalSetCaretY(Value: Integer);
    procedure SetInternalDisplayXY(const aPos: TDisplayCoord);
    procedure SetActiveLineColor(Value: TColor);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: Boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TUnicodeStrings);
    procedure SetLineText(Value: WideString);
    procedure SetMaxScrollWidth(Value: Integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelTextExternal(const Value: WideString);
    procedure SetTabWidth(Value: Integer);
    procedure SynSetText(const Value: WideString);
    procedure SetTopLine(Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetWordWrapGlyph(const Value: TSynGlyph);
    procedure WordWrapGlyphChange(Sender: TObject);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure ProperSetLine(ALine: Integer; const ALineText: WideString);
    procedure UpdateModifiedStatus;
    procedure UndoRedoAdded(Sender: TObject);
    procedure UpdateLastCaretX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);
    procedure SetAdditionalIdentChars(const Value: TSysCharSet);
    procedure SetAdditionalWordBreakChars(const Value: TSysCharSet);

    procedure DoSearchFindFirstExecute(Action: TSearchFindFirst);
    procedure DoSearchFindExecute(Action: TSearchFind);
    procedure DoSearchReplaceExecute(Action: TSearchReplace);
    procedure DoSearchFindNextExecute(Action: TSearchFindNext);
    procedure FindDialogFindFirst(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    function SearchByFindDialog(FindDialog: TFindDialog) : bool;
    procedure FindDialogClose(Sender: TObject);
  protected
    FIgnoreNextChar: Boolean;
    FCharCodeString: string;
    procedure InvalidateRect(aRect: TvgRect; aErase: Boolean); virtual;
    procedure DblClick; override;
    procedure DecPaintLock;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
    procedure DragCanceled; //override;
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    procedure KeyUp(var Key: Word; Shift: TShiftState); //override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyPress(var Key: WideChar); virtual;
    procedure LinesChanged(Sender: TObject); virtual;
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ListInserted(Sender: TObject; Index: Integer; aCount: Integer);
    procedure ListPutted(Sender: TObject; Index: Integer; aCount: Integer);
    //helper procs to chain list commands
    procedure ChainListCleared(Sender: TObject);
    procedure ChainListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListInserted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListPutted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainLinesChanging(Sender: TObject);
    procedure ChainLinesChanged(Sender: TObject);
    procedure ChainUndoRedoAdded(Sender: TObject);
    procedure ScanRanges;
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
    procedure EnterFocus; override;
    procedure KillFocus; override;
    procedure Realign; override;
    procedure ApplyStyle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: Boolean;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer); virtual;
    procedure VScrollChange(Sender: TObject); override;
    procedure HScrollChange(Sender: TObject); override;
    function GetContentBounds: TvgRect; override;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure PaintGutter(const AClip: TvgRect; const aFirstRow,
      aLastRow: Integer); virtual;
    procedure PaintTextLines(AClip: TvgRect; const aFirstRow, aLastRow,
      FirstCol, LastCol: Integer); virtual;
    procedure RecalcCharExtent;
    procedure RedoItem;
    procedure InternalSetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelTextPrimitive(const Value: WideString);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: PWideChar;
      AddToUndoList: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UndoItem;
  protected
    fGutterWidth: Integer;
    fInternalImage: TSynInternalImage;
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: Integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;

    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: WideChar; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: WideString;
      Line, Column: Integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: integer;
    function GetSelStart: integer;
    function GetSelLength: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    function ShrinkAtWideGlyphs(const S: WideString; First: Integer;
      var CharCount: Integer): WideString;
    procedure LinesHookChanged;
    property InternalCaretX: Integer write InternalSetCaretX;
    property InternalCaretY: Integer write InternalSetCaretY;
    property InternalCaretXY: TBufferCoord write InternalSetCaretXY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property AlwaysShowCaret: Boolean read FAlwaysShowCaret
                                      write SetAlwaysShowCaret;
    procedure UpdateCaret;
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word = 0; SS2: TShiftState = []);
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretInView: Boolean;
    function CharIndexToRowCol(Index: Integer): TBufferCoord;
    procedure Clear;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DoCopyToClipboard(const SText: WideString);
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean);
    procedure FindMatchingBracket; virtual;
    function GetMatchingBracket: TBufferCoord; virtual;
    function GetMatchingBracketEx(const APoint: TBufferCoord): TBufferCoord; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    function ExpandAtWideGlyphs(const S: UnicodeString): UnicodeString;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: WideString;
      var Attri: TSynHighlighterAttributes): Boolean;
    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: WideString;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetWordAtRowCol(XY: TBufferCoord): WideString;
    procedure GotoBookMark(BookMark: Integer); virtual;
    procedure GotoLineAndCenter(ALine: Integer); virtual;
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsWhiteChar(AChar: WideChar): Boolean; virtual;
    function IsWordBreakChar(AChar: WideChar): Boolean; virtual;
    procedure InvalidateGutter;
    procedure InvalidateGutterLine(aLine: integer);
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure InvalidateSelection;
    function IsBookmark(BookMark: Integer): Boolean;
    function IsPointInSelection(const Value: TBufferCoord): Boolean;
    procedure LockUndo;
    function BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;

    function NextWordPos: TBufferCoord; virtual;
    function NextWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordStart: TBufferCoord; virtual;
    function WordStartEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordEnd: TBufferCoord; virtual;
    function WordEndEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PrevWordPos: TBufferCoord; virtual;
    function PrevWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;

    function PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
    function PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
    procedure Redo;
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(const RowCol: TDisplayCoord): TvgPoint;
    function RowColToCharIndex(RowCol: TBufferCoord): Integer;
    function SearchReplace(const ASearch, AReplace: WideString;
      AOptions: TSynSearchOptions): Integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TBufferCoord);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetSelWord;
    procedure SetWordBlock(Value: TBufferCoord);
    procedure Undo;
    procedure UnlockUndo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure SetFocus; //override;
    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure AddFocusControl(aControl: TWinControl);
    procedure RemoveFocusControl(aControl: TWinControl);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure SetLinesPointer(ASynEdit: TvgCustomSynEdit);
    procedure RemoveLinesPointer;
    procedure HookTextBuffer(aBuffer: TSynEditStringList;
      aUndo, aRedo: TSynEditUndoList);
    procedure UnHookTextBuffer;
  public
    property AdditionalIdentChars: TSysCharSet read FAdditionalIdentChars write SetAdditionalIdentChars;
    property AdditionalWordBreakChars: TSysCharSet read FAdditionalWordBreakChars write SetAdditionalWordBreakChars;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property ActiveLineColor: TColor read fActiveLineColor
      write SetActiveLineColor default clNone;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
    property DisplayLineCount: Integer read GetDisplayLineCount;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: single read fCharWidth;
    property Font: TvgFont read FFont;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: Integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: WideString read GetLineText write SetLineText;
    property Lines: TUnicodeStrings read fLines write SetLines;
    property Marks: TSynEditMarkList read fMarkList;
    property MaxScrollWidth: Integer read fMaxScrollWidth write SetMaxScrollWidth
      default 1024;
    property Modified: Boolean read fModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchEngine: TSynEditSearchCustom read fSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelTabBlock: Boolean read GetSelTabBlock;
    property SelTabLine: Boolean read GetSelTabLine;
    property SelText: WideString read GetSelText write SetSelTextExternal;
    property StateFlags: TSynStateFlags read fStateFlags;
    property Text: WideString read SynGetText write SynSetText;
    property TopLine: Integer read fTopLine write SetTopLine;
    property WordAtCursor: WideString read GetWordAtCursor;
    property WordAtMouse: WideString read GetWordAtMouse;
    property UndoList: TSynEditUndoList read fUndoList;
    property RedoList: TSynEditUndoList read fRedoList;
  public
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;

    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: Boolean read fHideSelection write SetHideSelection
      default False;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property IsScrolling : Boolean read FIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read GetOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollHintColor: TColor read fScrollHintColor
      write fScrollHintColor default clInfoBk;
    property ScrollHintFormat: TScrollHintFormat read fScrollHintFormat
      write fScrollHintFormat default shfTopLineOnly;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property ActiveSelectionMode: TSynSelectionMode read fActiveSelectionMode
      write SetActiveSelectionMode stored False;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantReturns: boolean read fWantReturns write SetWantReturns default True;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default False;
    property WordWrapGlyph: TSynGlyph read fWordWrapGlyph write SetWordWrapGlyph;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnContextHelp: TContextHelpEvent
      read fOnContextHelp write fOnContextHelp;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read fOnGutterClick write fOnGutterClick;
    property OnGutterGetText: TGutterGetTextEvent read fOnGutterGetText
      write fOnGutterGetText;
    property OnGutterPaint: TGutterPaintEvent read fOnGutterPaint
      write fOnGutterPaint;
    property OnMouseCursor: TMouseCursorEvent read fOnMouseCursor
      write fOnMouseCursor;
    property OnKeyPress: TKeyPressWEvent read FOnKeyPressW write FOnKeyPressW;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
    property OnPaintTransient: TPaintTransient
      read fOnPaintTransient write fOnPaintTransient;
    property FontFill: TvgBrush read FFontFill;
  published
    property Cursor default crIBeam;
    property OnSearchNotFound: TCustomSynEditSearchNotFoundEvent
      read fSearchNotFound write fSearchNotFound;
  end;

  TvgSynEdit = class(TvgCustomSynEdit)
  published
    // inherited properties
    property Align;
    property ActiveLineColor;
    property Enabled;
    property DisableFocusEffect;
    property Font;
    property Height;
    property Name;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property CanFocused default True;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    // TvgCustomSynEdit properties
    property BookMarkOptions;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property Lines;
    property MaxScrollWidth;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollHintColor;
    property ScrollHintFormat;
    property SearchEngine;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property WordWrapGlyph;
    // TvgCustomSynEdit events
    property OnChange;
    property OnClearBookmark;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnDropFiles;
    property OnGutterClick;
    property OnGutterGetText;
    property OnGutterPaint;
    property OnMouseCursor;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
  end;

procedure Register;

implementation {===============================================================}

uses
  Consts,
  Clipbrd,
  ShellAPI,
  SynEditWordWrap,
  SynEditStrConst;

{$R *.res}

procedure Register;
begin
  RegisterNoIcon([TvgSynEdit]);
end;

type

  THackSynEditPlugin = class(TSynEditPlugin);

function TrimTrailingSpaces(const S: WideString): WideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and ((S[I] = #32) or (S[I] = #9)) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

{ THookedCommandHandlerEntry }

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
    function Equals(AEvent: THookedCommandEvent): Boolean; {$IFDEF UNICODE} reintroduce; {$ENDIF}
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): Boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TvgCustomSynEdit }

function TvgCustomSynEdit.PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
// Result is in display coordinates
var
  f: Single;
begin
  f := (aX - fGutterWidth - 2) / fCharWidth;
  // don't return a partially visible last line
  if aY >= fLinesInWindow * fTextHeight then
  begin
    aY := fLinesInWindow * fTextHeight - 1;
    if aY < 0 then
      aY := 0;
  end;
  Result.Column := Max(1, LeftChar + Round(f));
  Result.Row := Max(1, TopLine + (aY div fTextHeight));
end;

function TvgCustomSynEdit.PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
begin
  Result.Column := trunc(Max(1, LeftChar + ((aX - fGutterWidth - 2) / fCharWidth)));
  Result.Row := trunc(Max(1, TopLine + (aY / fTextHeight)));
end;

function TvgCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TvgPoint;
begin
  Result.X := (RowCol.Column-1) * fCharWidth + fTextOffset;
  Result.Y := (RowCol.Row - fTopLine) * fTextHeight;
end;

procedure TvgCustomSynEdit.ComputeCaret(X, Y: Integer);
//X,Y are pixel coordinates
var
  vCaretNearestPos : TDisplayCoord;
begin
  vCaretNearestPos := PixelsToNearestRowColumn(X, Y);
  vCaretNearestPos.Row := MinMax(vCaretNearestPos.Row, 1, DisplayLineCount);
  SetInternalDisplayXY(vCaretNearestPos);
end;

procedure TvgCustomSynEdit.ComputeScroll(X, Y: Integer);
//X,Y are pixel coordinates
var
  iScrollBounds: TvgRect; { relative to the client area }
begin
  { don't scroll if dragging text from other control }
(*  if (not MouseCapture) and (not Dragging) then
  begin
    fScrollTimer.Enabled := False;
    Exit;
  end;

  iScrollBounds := Bounds(fGutterWidth, 0, fCharsInWindow * fCharWidth,
    fLinesInWindow * fTextHeight);
  if BorderStyle = bsNone then
    InflateRect(iScrollBounds, -2, -2);

  if X < iScrollBounds.Left then
    fScrollDeltaX := (X - iScrollBounds.Left) div fCharWidth - 1
  else if X >= iScrollBounds.Right then
    fScrollDeltaX := (X - iScrollBounds.Right) div fCharWidth + 1
  else
    fScrollDeltaX := 0;

  if Y < iScrollBounds.Top then
    fScrollDeltaY := (Y - iScrollBounds.Top) div fTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div fTextHeight + 1
  else
    fScrollDeltaY := 0;

  fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0); *)
end;

procedure TvgCustomSynEdit.DoCopyToClipboard(const SText: WideString);
var
  Data: THandle;
  DataPtr: Pointer;
  Size: Cardinal;
begin
  if SText = '' then Exit;
  SetClipboardText(SText);
  Clipboard.Open;
  try
    Size := Length(SText);
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * Size + 2);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(PWideChar(SText)^, DataPtr^, 2 * Size + 2);
        Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TvgCustomSynEdit.CopyToClipboard;
var
  SText: WideString;
  ChangeTrim: Boolean;
begin
  if SelAvail then
  begin
    ChangeTrim := (fActiveSelectionMode = smColumn) and (eoTrimTrailingSpaces in Options);
    try
      if ChangeTrim then
        Exclude(fOptions, eoTrimTrailingSpaces);
      SText := SelText;
    finally
      if ChangeTrim then
        Include(fOptions, eoTrimTrailingSpaces);
    end;
    DoCopyToClipboard(SText);
  end;
end;

procedure TvgCustomSynEdit.CutToClipboard;
begin
  if not ReadOnly and SelAvail then
  begin
    BeginUndoBlock;
    try
      DoCopyToClipboard(SelText);
      SelText := '';
    finally
      EndUndoBlock;
    end;
  end;
end;

constructor TvgCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResource := 'memostyle';
  fLines := TSynEditStringList.Create(ExpandAtWideGlyphs);
  fOrigLines := fLines;
  with TSynEditStringList(fLines) do
  begin
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnPutted := ListPutted;
  end;
  FFont := TvgFont.Create;
  FFont.Family := 'Courier New';
  FFont.Size := 11;
  FFontFill := TvgBrush.Create(vgBrushSolid, $FF000000);
  FSelectionFill := TvgBrush.Create(vgBrushSolid, $40002070);
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := UndoRedoAdded;
  fOrigUndoList := fUndoList;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := UndoRedoAdded;
  fOrigRedoList := fRedoList;

  fActiveLineColor := clNone;
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create;
  fGutter.OnChange := GutterChanged;
  fGutterWidth := fGutter.Width;
  fWordWrapGlyph := TSynGlyph.Create(HINSTANCE, 'vgSynEditWrapped', clLime);
  fWordWrapGlyph.OnChange := WordWrapGlyphChange;
  fTextOffset := fGutterWidth + 2;
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Font.Assign(FFont);
  Font.OnChanged := SynFontChanged;
  CanFocused := True;
  fInserting := True;
  fMaxScrollWidth := 1024;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fActiveSelectionMode := smNormal;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create(nil);
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
  fWantReturns := True;
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  fLastCaretX := 1;
  fCaretY := 1;
  fBlockBegin.Char := 1;
  fBlockBegin.Line := 1;
  fBlockEnd := fBlockBegin;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;

  fScrollHintColor := clInfoBk;
  fScrollHintFormat := shfTopLineOnly;

  SynFontChanged(nil);
end;

procedure TvgCustomSynEdit.DecPaintLock;
var
  vAuxPos: TDisplayCoord;
begin
  Assert(fPaintLock > 0);
  Dec(fPaintLock);
  if (fPaintLock = 0) then
  begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    // Locks the caret inside the visible area
    if WordWrap and ([scCaretX,scCaretY] * fStatusChanges <> []) then
    begin
      vAuxPos := DisplayXY;
      // This may happen in the last row of a line or in rows which length is
      // greater than CharsInWindow (Tabs and Spaces are allowed beyond
      // CharsInWindow while wrapping the lines)
      if (vAuxPos.Column > CharsInWindow +1) and (CharsInWindow > 0) then
      begin
        if fCaretAtEOL then
          fCaretAtEOL := False
        else
        begin
          if scCaretY in fStatusChanges then
          begin
            vAuxPos.Column := CharsInWindow + 1;
            fCaretX := DisplayToBufferPos(vAuxPos).Char;
            Include(fStatusChanges,scCaretX);
            UpdateLastCaretX;
          end;
        end;
        Include(fStateFlags, sfCaretChanged);
      end;
    end;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TvgCustomSynEdit.Destroy;
begin
  Highlighter := nil;
  if (fChainedEditor <> nil) or (fLines <> fOrigLines) then
    RemoveLinesPointer;

  inherited Destroy;

  // free listeners while other fields are still valid

  // do not use FreeAndNil, it first nils and then freey causing problems with
  // code accessing fHookedCommandHandlers while destruction
  fHookedCommandHandlers.Free;
  fHookedCommandHandlers := nil;
  // do not use FreeAndNil, it first nils and then frees causing problems with
  // code accessing fPlugins while destruction
  fPlugins.Free;
  fPlugins := nil;

  fMarkList.Free;
  fBookMarkOpt.Free;
  fKeyStrokes.Free;
  fKbdHandler.Free;
  fFocusList.Free;
  fSelectedColor.Free;
  fOrigUndoList.Free;
  fOrigRedoList.Free;
  fGutter.Free;
  fWordWrapGlyph.Free;
  fInternalImage.Free;
  FFont.Free;
  FFontFill.Free;
  FSelectionFill.Free;
  fOrigLines.Free;
end;

function TvgCustomSynEdit.GetBlockBegin: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
  then
    Result := fBlockEnd
  else
    Result := fBlockBegin;
end;

function TvgCustomSynEdit.GetBlockEnd: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
  then
    Result := fBlockBegin
  else
    Result := fBlockEnd;
end;

procedure TvgCustomSynEdit.SynFontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(True);
end;

function TvgCustomSynEdit.GetLineText: WideString;
begin
  if (CaretY >= 1) and (CaretY <= Lines.Count) then
    Result := Lines[CaretY - 1]
  else
    Result := '';
end;

function TvgCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.Char <> fBlockEnd.Char) or
    ((fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn));
end;

function TvgCustomSynEdit.GetSelTabBlock: Boolean;
begin
  Result := (fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn);
end;

function TvgCustomSynEdit.GetSelTabLine: Boolean;
begin
  Result := (BlockBegin.Char <= 1) and (BlockEnd.Char > length(Lines[CaretY - 1])) and SelAvail;
end;

function WideTrim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function WStrLen(const Str: PWideChar): Cardinal;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;
function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,1
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,1
        REP     MOVSW
        POP     ESI
        POP     EDI
end;

function TvgCustomSynEdit.GetSelText: WideString;

  function CopyPadded(const S: WideString; Index, Count: Integer): WideString;
  var
    SrcLen: Integer;
    DstLen: Integer;
    i: Integer;
    P: PWideChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PWideChar(Result);
      WStrCopy(P, PWideChar(Copy(S, Index, Count)));
      Inc(P, Length(S));
      for i := 0 to DstLen - Srclen - 1 do
        P[i] := #32;
    end;
  end;

  procedure CopyAndForward(const S: WideString; Index, Count: Integer; var P:
    PWideChar);
  var
    pSrc: PWideChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then
    begin
      Dec(Index);
      pSrc := PWideChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen * sizeof(WideChar));
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  function CopyPaddedAndForward(const S: WideString; Index, Count: Integer;
    var P: PWideChar): Integer;
  var
    OldP: PWideChar;
    Len, i: Integer;
  begin
    Result := 0;
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    if not (eoTrimTrailingSpaces in Options) then
    begin
      for i := 0 to Len - 1 do
        P[i] := #32;
      Inc(P, Len);
    end
    else
      Result:= Len;
  end;

var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  l, r: Integer;
  s: WideString;
  P: PWideChar;
  cRow: Integer;
  vAuxLineChar: TBufferCoord;
  vAuxRowCol: TDisplayCoord;
  vTrimCount: Integer;
begin
  if not SelAvail then
    Result := ''
  else begin
    ColFrom := BlockBegin.Char;
    First := BlockBegin.Line - 1;
    //
    ColTo := BlockEnd.Char;
    Last := BlockEnd.Line - 1;
    //
    TotalLen := 0;
    case fActiveSelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calculate total length of result string
          TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(Lines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(SLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);

          CopyAndForward(SLineBreak, 1, MaxInt, P);

          for i := First + 1 to Last - 1 do
          begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
        end;
      smColumn:
        begin
          with BufferToDisplayPos(BlockBegin) do
          begin
            First := Row;
            ColFrom := Column;
          end;
          with BufferToDisplayPos(BlockEnd) do
          begin
            Last := Row;
            ColTo := Column;
          end;
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: pre-allocate string large enough for worst case
          TotalLen := ((ColTo - ColFrom) + Length(sLineBreak)) *
            (Last - First +1);
          SetLength(Result, TotalLen);
          P := PWideChar(Result);

          // step2: copy chunks to the pre-allocated string
          TotalLen := 0;
          for cRow := First to Last do
          begin
            vAuxRowCol.Row := cRow;
            vAuxRowCol.Column := ColFrom;
            vAuxLineChar := DisplayToBufferPos(vAuxRowCol);
            l := vAuxLineChar.Char;
            s := Lines[vAuxLineChar.Line - 1];
            vAuxRowCol.Column := ColTo;
            r := DisplayToBufferPos(vAuxRowCol).Char;

            vTrimCount := CopyPaddedAndForward(s, l, r - l, P);
            TotalLen := TotalLen + (r - l) - vTrimCount + Length(sLineBreak);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          SetLength(Result, TotalLen - Length(sLineBreak));
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calculate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(SLineBreak));
          if Last = Lines.Count then
            Dec(TotalLen, Length(SLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          for i := First to Last - 1 do
          begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(SLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

function TvgCustomSynEdit.SynGetText: WideString;
begin
  Result := Lines.Text;
end;

function TvgCustomSynEdit.GetWordAtCursor: WideString;
var
  bBegin: TBufferCoord;
  bEnd: TBufferCoord;
begin
  bBegin := GetBlockBegin;
  bEnd := GetBlockEnd;
  SetBlockBegin(WordStart);
  SetBlockEnd(WordEnd);
  Result := SelText;
  SetBlockBegin(bBegin);
  SetBlockEnd(bEnd);
end;

procedure TvgCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in fStateFlags then
  begin
    inherited HideCaret;
    Exclude(fStateFlags, sfCaretVisible);
  end;
end;

procedure TvgCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TvgCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TvgCustomSynEdit.InvalidateGutterLine(aLine: Integer);
begin
  if (aLine < 1) or (aLine > Lines.Count) then
    Exit;

  InvalidateGutterLines(aLine, aLine);
end;

procedure TvgCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TvgRect;
begin
  if Visible  then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := vgRect(0, 0, fGutterWidth, ClientHeight);
{$IFDEF SYN_CLX}
      with GetClientRect do
        OffsetRect(rcInval, Left, Top);
{$ENDIF}
      if sfLinesChanging in fStateFlags then
        fInvalidateRect := vgUnionRect(fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end
    else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);
      if WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        if LastLine <= Lines.Count then
          LastLine := LineToRow(LastLine)
        else
          LastLine := MaxInt;
      end;
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := vgRect(0, fTextHeight * (FirstLine - TopLine),
          fGutterWidth, fTextHeight * (LastLine - TopLine + 1));
{$IFDEF SYN_CLX}
        with GetClientRect do
          OffsetRect(rcInval, Left, Top);
{$ENDIF}
        if sfLinesChanging in fStateFlags then
          fInvalidateRect := vgUnionRect(fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TvgCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TvgRect;
begin
  if Visible then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := LocalRect;
      rcInval.Left := rcInval.Left + fGutterWidth;
      if sfLinesChanging in fStateFlags then
        fInvalidateRect := vgUnionRect(fInvalidateRect, rcInval)
      else
        InvalidateRect(rcInval, False);
    end
    else begin
      FirstLine := Max(FirstLine,1);
      LastLine := Max(LastLine,1);
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);

      if LastLine >= Lines.Count then
        LastLine := MaxInt; // paint empty space beyond last line

      if WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        // Could avoid this conversion if (First = Last) and
        // (Length < CharsInWindow) but the dependency isn't worth IMO.
        if LastLine < Lines.Count then
          LastLine := LineToRow(LastLine + 1) - 1;
      end;

      // TopLine is in display coordinates, so FirstLine and LastLine must be
      // converted previously.
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);

      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := vgRect(fGutterWidth, fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
{$IFDEF SYN_CLX}
        with GetClientRect do
          OffsetRect(rcInval, Left, Top);
{$ENDIF}
        if sfLinesChanging in fStateFlags then
          fInvalidateRect := vgUnionRect(fInvalidateRect, rcInval)
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TvgCustomSynEdit.InvalidateSelection;
begin
  InvalidateLines(BlockBegin.Line, BlockEnd.Line);
end;

{$IFDEF SYN_COMPILER_5}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

procedure TvgCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (ssAlt in Shift) and (Key >= VK_NUMPAD0) and (Key <= VK_NUMPAD9) then
    FCharCodeString := FCharCodeString + IntToStr(Key - VK_NUMPAD0);
{  inherited;
  DoKeyPress(CharCode);
  FIgnoreNextChar := True;}
  fKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TvgCustomSynEdit.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  Data: pointer;
  C: WideChar;
  Cmd: TSynEditorCommand;
begin
  inherited;
  if KeyChar <> #0 then
  begin
    KeyPress(KeyChar);
    KeyChar := #0;
  end;
  fKbdHandler.ExecuteKeyDown(Self, Key, Shift);
  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end
    else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

procedure TvgCustomSynEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
  UpdateScrollBars;
end;

type
  TAccessWinControl = class(TWinControl);

procedure TvgCustomSynEdit.KeyPress(var Key: WideChar);
begin
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then
  begin
    fKbdHandler.ExecuteKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
  end
  else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
end;

function TvgCustomSynEdit.LeftSpaces(const Line: WideString): Integer;
begin
  Result := LeftSpacesEx(Line, False);
end;

function TvgCustomSynEdit.LeftSpacesEx(const Line: WideString; WantTabs: Boolean): Integer;
var
  p: PWideChar;
begin
  p := PWideChar(Line);
  if Assigned(p) and (eoAutoIndent in fOptions) then
  begin
    Result := 0;
    while (p^ >= #1) and (p^ <= #32) do
    begin
      if (p^ = #9) and WantTabs then
        Inc(Result, TabWidth)
      else
        Inc(Result);
      Inc(p);
    end;
  end
  else
    Result := 0;
end;

function WideStringOfChar(C: WideChar; Count: Cardinal): WideString;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

function TvgCustomSynEdit.GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): WideString;
begin
  if WantTabs and not(eoTabsToSpaces in Options) and (CharCount >= TabWidth) then
    Result := WideStringOfChar(#9, CharCount div TabWidth) +
      WideStringOfChar(#32, CharCount mod TabWidth)
  else
    Result := WideStringOfChar(#32, CharCount);
end;

procedure TvgCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TvgCustomSynEdit.LinesChanged(Sender: TObject);
var
  vOldMode: TSynSelectionMode;
begin
  Exclude(fStateFlags, sfLinesChanging);
  begin
    Realign;
    vOldMode := fActiveSelectionMode;
    SetBlockBegin(CaretXY);
    fActiveSelectionMode := vOldMode;
    InvalidateRect(fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TvgRect), 0);
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TvgCustomSynEdit.EnterFocus;
begin
  inherited;
  ShowCaret;
  UpdateCaret;
end;

procedure TvgCustomSynEdit.Killfocus;
begin
  inherited;
  HideCaret;
end;

procedure TvgCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
var
  bWasSel: Boolean;
  bStartDrag: Boolean;
  TmpBegin, TmpEnd: TBufferCoord;
begin
  TmpBegin := FBlockBegin;
  TmpEnd := FBlockEnd;

  bWasSel := False;
  bStartDrag := False;
  if Button = mbLeft then
  begin
    if SelAvail then
    begin
      //remember selection state, as it will be cleared later
      bWasSel := True;
      fMouseDownX := X;
      fMouseDownY := Y;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and (ssDouble in Shift) then Exit;

  fKbdHandler.ExecuteMouseDown(Self, Button, Shift, trunc(X), trunc(Y));

  if (Button in [mbLeft, mbRight]) then
  begin
    if Button = mbRight then
    begin
      if (eoRightMouseMovesCursor in Options) and
         (SelAvail and not IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(trunc(X), trunc(Y))))
         or not SelAvail) then
      begin
        InvalidateSelection;
        FBlockEnd := FBlockBegin;
        ComputeCaret(trunc(X), trunc(Y));
      end
      else
        Exit;
    end
    else
      ComputeCaret(trunc(X), trunc(Y));
  end;

  if Button = mbLeft then
  begin
    //I couldn't track down why, but sometimes (and definately not all the time)
    //the block positioning is lost.  This makes sure that the block is
    //maintained in case they started a drag operation on the block
    FBlockBegin := TmpBegin;
    FBlockEnd := TmpEnd;

    Capture;
    //if mousedown occurred in selected block begin drag operation
    Exclude(fStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in fOptions) and (X >= fGutterWidth + 2)
      and (SelectionMode = smNormal) and IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(trunc(X), trunc(Y)))) then
    begin
      bStartDrag := True
    end;
  end;

  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, sfWaitForDragging)
  else
  begin
    if not (sfDblClicked in fStateFlags) then
    begin
      if ssShift in Shift then
        //BlockBegin and BlockEnd are restored to their original position in the
        //code from above and SetBlockEnd will take care of proper invalidation
        SetBlockEnd(CaretXY)
      else
      begin
        if (eoAltSetsColumnMode in Options) and (fActiveSelectionMode <> smLine) then
        begin
          if ssAlt in Shift then
            SelectionMode := smColumn
          else
            SelectionMode := smNormal;
        end;
        //Selection mode must be set before calling SetBlockBegin
        SetBlockBegin(CaretXY);
      end;
    end;
  end;

  if (X < fGutterWidth) then
    Include(fStateFlags, sfPossibleGutterClick);
  if (sfPossibleGutterClick in fStateFlags) and (Button = mbRight) then
  begin
    DoOnGutterClick(Button, trunc(X), trunc(Y))
  end;

  SetFocus;
end;

procedure TvgCustomSynEdit.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  P: TDisplayCoord;
begin
  inherited MouseMove(Shift, x, y, Dx, Dy);
  if (sfWaitForDragging in fStateFlags) then
  begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG)) then
    begin
      Exclude(fStateFlags, sfWaitForDragging);
//      BeginDrag(False);
    end;
  end
  else
  if (FPressed) then
  begin
    // should we begin scrolling?
    ComputeScroll(trunc(X), trunc(Y));
    { compute new caret }
    P := PixelsToNearestRowColumn(trunc(X), trunc(Y));
    P.Row := MinMax(P.Row, 1, DisplayLineCount);
    if fScrollDeltaX <> 0 then
      P.Column := DisplayX;
    if fScrollDeltaY <> 0 then
      P.Row := DisplayY;
    InternalCaretXY := DisplayToBufferPos(P);
    BlockEnd := CaretXY;
    if (sfPossibleGutterClick in fStateFlags) and (FBlockBegin.Line <> CaretXY.Line) then
      Include(fStateFlags, sfGutterDragging);
  end;
end;

procedure TvgCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  fKbdHandler.ExecuteMouseUp(Self, Button, Shift, trunc(X), trunc(Y));

  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  ReleaseCapture;
  if (sfPossibleGutterClick in fStateFlags) and (X < fGutterWidth) and (Button <> mbRight) then
  begin
    DoOnGutterClick(Button, trunc(X), trunc(Y))
  end
  else if fStateFlags * [sfDblClicked, sfWaitForDragging] = [sfWaitForDragging] then
  begin
    ComputeCaret(trunc(X), trunc(Y));
    if not(ssShift in Shift) then
      SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;
  Exclude(fStateFlags, sfDblClicked);
  Exclude(fStateFlags, sfPossibleGutterClick);
  Exclude(fStateFlags, sfGutterDragging);
end;

procedure TvgCustomSynEdit.DoOnGutterClick(Button: TMouseButton; X, Y: Integer);
var
  i     : Integer;
  offs  : Integer;
  line  : Integer;
  allmrk: TSynEditMarks;
  mark  : TSynEditMark;
begin
  if Assigned(fOnGutterClick) then
  begin
    line := DisplayToBufferPos(PixelsToRowColumn(X,Y)).Line;
    if line <= Lines.Count then
    begin
      Marks.GetMarksForLine(line, allmrk);
      offs := 0;
      mark := nil;
      for i := 1 to MAX_MARKS do
      begin
        if assigned(allmrk[i]) then
        begin
          Inc(offs, BookMarkOptions.XOffset);
          if X < offs then
          begin
            mark := allmrk[i];
            break;
          end;
        end;
      end; //for
      fOnGutterClick(Self, Button, X, Y, line, mark);
    end;
  end;
end;

procedure TvgCustomSynEdit.Realign;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  try
    SizeOrFontChanged(false);
    SetLeftChar(LeftChar);
  finally
    FDisableAlign := false;
  end;
end;

procedure TvgCustomSynEdit.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('content');
  if (T <> nil) and (T.IsVisual) then
  begin
    TvgVisualObject(T).OnPaint := DoContentPaint;
  end;
  T := FindResource('selection');
  if (T <> nil) and (T is TvgBrushObject) then
  begin
    FSelectionFill.Assign(TvgBrushObject(T).Brush);
  end;
  { from style }
  T := FindResource('foreground');
  if (T <> nil) and (T is TvgBrushObject) then
    FFontFill.Assign(TvgBrushObject(T).Brush);
end;

procedure TvgCustomSynEdit.UpdateScrollBars;
var
  nMaxScroll: Integer;
  iClientRect: TvgRect;
begin
  if VScrollBar <> nil then
    VScrollBar.Value := (TopLine - 1) * FTextHeight;
  if HScrollBar <> nil then
    HScrollBar.Value := (LeftChar - 1) * FCharWidth;
end;

procedure TvgCustomSynEdit.VScrollChange(Sender: TObject);
begin
  inherited ;
  TopLine := trunc(VScrollBar.Value / FTextHeight) + 1;
  UpdateCaret;
end;

procedure TvgCustomSynEdit.HScrollChange(Sender: TObject);
begin
  inherited ;
  LeftChar := trunc(HScrollBar.Value / FCharWidth) + 1;
  UpdateCaret;
end;

function TvgCustomSynEdit.GetContentBounds: TvgRect;
var
  nMaxScroll, vMaxScroll: single;
begin
  Result := LocalRect;
  if ContentLayout = nil then Exit;
  if eoScrollPastEol in Options then
    nMaxScroll := MaxScrollWidth * fCharWidth
  else
    nMaxScroll := Max(TSynEditStringList(Lines).LengthOfLongestLine, 1) * fCharWidth;
  vMaxScroll := (DisplayLineCount + 1) * FTextHeight;
  if vMaxScroll < ClientHeight then
    vMaxScroll := ClientHeight;
  if nMaxScroll < ClientWidth then
    nMaxScroll := ClientWidth;
  Result := vgRect(0, 0, nMaxScroll, vMaxScroll);
end;

procedure TvgCustomSynEdit.DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
var
  rcClip, rcDraw: TvgRect;
  nL1, nL2, nC1, nC2: single;
  SaveIndex: cardinal;
begin
  SaveIndex := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(ARect);
  try
    // Get the invalidated rect. Compute the invalid area in lines / columns.
    rcClip := ARect;
    // columns
    nC1 := LeftChar;
    if (rcClip.Left > fGutterWidth + 2) then
      nC1 := nC1 + (rcClip.Left - fGutterWidth - 2) / CharWidth;
    nC2 := nC1 +
      (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) / CharWidth;
    // lines
    nL1 := vgMaxFloat(TopLine + rcClip.Top / fTextHeight, TopLine);
    nL2 := vgMinFloat(TopLine + (rcClip.Bottom + fTextHeight - 1) / fTextHeight,
      Lines.Count);
    Canvas.Font.Assign(fFont);
    Canvas.Fill.Assign(FFontFill);
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, trunc(nL1), trunc(nL2));
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, trunc(nL1), trunc(nL2), trunc(nC1), trunc(nC2));
    end;
  //  PluginsAfterPaint(Canvas, rcClip, nL1, nL2);
    // If there is a custom paint handler call it.
    DoOnPaint;
    DoOnPaintTransient(ttAfter);
  finally
    Canvas.RestoreCanvas(SaveIndex);
  end;
end;

procedure TvgCustomSynEdit.PaintGutter(const AClip: TvgRect;
  const aFirstRow, aLastRow: Integer);

  procedure DrawMark(aMark: TSynEditMark; var aGutterOff: Integer;
    aMarkRow: Integer);
  begin
(*    if (not aMark.InternalImage) and Assigned(fBookMarkOpt.BookmarkImages) then
    begin
      if aMark.ImageIndex <= fBookMarkOpt.BookmarkImages.Count then
      begin
        if aMark.IsBookmark = BookMarkOptions.DrawBookmarksFirst then
          aGutterOff := 0
        else if aGutterOff = 0 then
          aGutterOff := fBookMarkOpt.XOffset;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight, aMark.ImageIndex);
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end
    else begin
      if aMark.ImageIndex in [0..9] then
      begin
        if not Assigned(fInternalImage) then
        begin
          fInternalImage := TSynInternalImage.Create(HINSTANCE,
            'SynEditInternalImages', 10);
        end;
        if aGutterOff = 0 then
        begin
          fInternalImage.Draw(Canvas, aMark.ImageIndex,
            fBookMarkOpt.LeftMargin + aGutterOff,
            (aMarkRow - TopLine) * fTextHeight, fTextHeight);
        end;
        Inc(aGutterOff, fBookMarkOpt.XOffset);
      end;
    end; *)
  end;

var
  cLine: Integer;
  cMark: Integer;
  rcLine: TvgRect;
  aGutterOffs: PIntArray;
  bHasOtherMarks: Boolean;
  s: UnicodeString;
  vFirstLine: Integer;
  vLastLine: Integer;
  vMarkRow: Integer;
  vGutterRow: Integer;
  vLineTop: Integer;
begin
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);
  //todo: Does the following comment still apply?
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
{  if fGutter.Gradient then
    SynDrawGradient(Canvas, fGutter.GradientStartColor, fGutter.GradientEndColor,
      fGutter.GradientSteps, vgRect(0, 0, fGutterWidth, ClientHeight), True);}
  if not fGutter.Gradient then
  begin
    Canvas.Fill.Style := vgBrushSolid;
    Canvas.Fill.SolidColor := vgRGBtoBGR(ColorToRGB(fGutter.Color)) or $FF000000;
    Canvas.FillRect(AClip, 0, 0, [], AbsoluteOpacity);
  end;
  if fGutter.ShowLineNumbers then
  begin
    Canvas.Fill.SolidColor := Gutter.Color;
    Canvas.Font.Assign(Font);
    try
      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := Max(rcLine.Right, fGutterWidth - 2);
      rcLine.Bottom := rcLine.Top;

      for cLine := vFirstLine to vLastLine do
      begin
        vLineTop := (LineToRow(cLine) - TopLine) * fTextHeight;
        if WordWrap and not fGutter.Gradient then
        begin
          // erase space between wrapped lines (from previous line to current one)
          rcLine.Top := rcLine.Bottom;
          rcLine.Bottom := vLineTop;
        end;
        // next line rect
        rcLine.Top := vLineTop;
        rcLine.Bottom := rcLine.Top + fTextHeight;

        s := fGutter.FormatLineNumber(cLine);
        if Assigned(OnGutterGetText) then
          OnGutterGetText(Self, cLine, s);

        Canvas.Fill.Assign(FFontFill);
        Canvas.FillText(rcLine, vgRect(-$FFFF, -$FFFF, $FFFF, $FFFF), s, false, AbsoluteOpacity, vgTextAlignNear, vgTextAlignNear);
      end;
    finally
    end;
  end;

  // paint wrapped line glyphs
{  if WordWrap and fWordWrapGlyph.Visible then
    for cLine := aFirstRow to aLastRow do
      if LineToRow(RowToLine(cLine)) <> cLine then
        fWordWrapGlyph.Draw(Canvas,
                            (fGutterWidth - fGutter.RightOffset - 2) - fWordWrapGlyph.Width,
                            (cLine - TopLine) * fTextHeight, fTextHeight);}

  // the gutter separator if visible
  if (fGutter.BorderStyle <> gbsNone) and (AClip.Right >= fGutterWidth - 2) then
    with Canvas do
    begin
      Canvas.Stroke.Style := vgBrushSolid;
      Canvas.Stroke.Assign(FFontFill);
      with AClip do
      begin
        Canvas.DrawLine(vgPoint(fGutterWidth - 1.5, Top), vgPoint(fGutterWidth - 1.5, Bottom), AbsoluteOpacity);
      end;
    end;

  // now the gutter marks
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0) and (vLastLine >= vFirstLine) then
  begin
    aGutterOffs := AllocMem((aLastRow - aFirstRow + 1) * SizeOf(Integer));
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := False;
      for cMark := 0 to Marks.Count - 1 do with Marks[cMark] do
        if Visible and (Line >= vFirstLine) and (Line <= vLastLine) then
        begin
          if IsBookmark <> BookMarkOptions.DrawBookmarksFirst then
            bHasOtherMarks := True
          else begin
            vMarkRow := LineToRow(Line);
            if vMarkRow >= aFirstRow then
              DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
          end
        end;
      if bHasOtherMarks then
        for cMark := 0 to Marks.Count - 1 do with Marks[cMark] do
        begin
          if Visible and (IsBookmark <> BookMarkOptions.DrawBookmarksFirst)
            and (Line >= vFirstLine) and (Line <= vLastLine) then
          begin
            vMarkRow := LineToRow(Line);
            if vMarkRow >= aFirstRow then
              DrawMark(Marks[cMark], aGutterOffs[vMarkRow - aFirstRow], vMarkRow);
          end;
        end;
      if Assigned(OnGutterPaint) then
        for cLine := vFirstLine to vLastLine do
        begin
          vGutterRow := LineToRow(cLine);
          OnGutterPaint(Self, cLine, aGutterOffs[vGutterRow - aFirstRow],
            (vGutterRow - TopLine) * LineHeight);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end
  else if Assigned(OnGutterPaint) then
  begin
    for cLine := vFirstLine to vLastLine do
    begin
      vGutterRow := LineToRow(cLine);
      OnGutterPaint(Self, cLine, 0, (vGutterRow - TopLine) * LineHeight);
    end;
  end;
end;

// Inserts filling chars into a string containing chars that display as glyphs
// wider than an average glyph. (This is often the case with Asian glyphs, which
// are usually wider than latin glpyhs)
// This is only to simplify paint-operations and has nothing to do with
// multi-byte chars.
function TvgCustomSynEdit.ExpandAtWideGlyphs(const S: UnicodeString): UnicodeString;
var
  i, j, CountOfAvgGlyphs: Integer;
begin
  Result := S;
  j := 0;
  SetLength(Result, Length(S) * 2); // speed improvement
  for i := 1 to Length(S) do
  begin
    inc(j);
    CountOfAvgGlyphs := {1; //}Ceil(Canvas.TextWidth(S[i]) / fCharWidth);

    if j + CountOfAvgGlyphs > Length(Result) then
      SetLength(Result, Length(Result) + 128);

    // insert CountOfAvgGlyphs filling chars
    while CountOfAvgGlyphs > 1 do
    begin
      Result[j] := FillerChar;
      inc(j);
      dec(CountOfAvgGlyphs);
    end;

    Result[j] := S[i];
  end;

  SetLength(Result, j);
end;

// does the opposite of ExpandAtWideGlyphs
function TvgCustomSynEdit.ShrinkAtWideGlyphs(const S: WideString; First: Integer;
  var CharCount: Integer): WideString;
var
  i, j: Integer;
begin
  SetLength(Result, Length(S));

  i := First;
  j := 0;
  while i < First + CharCount do
  begin
    inc(j);
    while S[i] = FillerChar do
      inc(i);
    Result[j] := S[i];
    inc(i);
  end;

  SetLength(Result, j);
  CharCount := j;
end;

procedure TvgCustomSynEdit.PaintTextLines(AClip: TvgRect; const aFirstRow, aLastRow,
  FirstCol, LastCol: Integer);
var
  bDoRightEdge: Boolean; // right edge
  nRightEdge: Integer;
    // selection info
  bAnySelection: Boolean; // any selection visible?
  vSelStart: TDisplayCoord; // start of selected area
  vSelEnd: TDisplayCoord; // end of selected area
    // info about normal and selected text and background colors
  bSpecialLine, bLineSelected, bCurrentLine: Boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
    // info about selection of the current line
  nLineSelStart, nLineSelEnd: Integer;
  bComplexLine: Boolean;
    // painting the background and the text
  rcLine, rcToken: TvgRect;
  TokenAccu: record
    // Note: s is not managed as a string, it will only grow!!!
    // Never use AppendStr or "+", use Len and MaxLen instead and
    // copy the string chars directly. This is for efficiency.
    Len, MaxLen, CharsBefore: Integer;
    s: WideString;
    TabString: WideString;
    FG, BG: TColor;
    Style: TvgFontStyle;
  end;
{$IFNDEF SYN_CLX}
  dc: HDC;
{$ENDIF}
  SynTabGlyphString: WideString;

  vFirstLine: Integer;
  vLastLine: Integer;

{ local procedures }

  function colEditorBG: TColor;
  var
    iAttri: TSynHighlighterAttributes;
  begin
    if (ActiveLineColor <> clNone) and (bCurrentLine) then
      Result := ActiveLineColor
    else
    begin
      Result := FFontFill.SolidColor;
      if Highlighter <> nil then
      begin
        iAttri := Highlighter.WhitespaceAttribute;
        if (iAttri <> nil) and (iAttri.Background <> clNone) then
          Result := iAttri.Background;
      end;
    end;
  end;

  procedure ComputeSelectionInfo;
  var
    vStart: TBufferCoord;
    vEnd: TBufferCoord;
  begin
    bAnySelection := False;
    // Only if selection is visible anyway.
    if not HideSelection or Self.IsFocused then
    begin
      bAnySelection := True;
      // Get the *real* start of the selected area.
      if fBlockBegin.Line < fBlockEnd.Line then
      begin
        vStart := fBlockBegin;
        vEnd := fBlockEnd;
      end
      else if fBlockBegin.Line > fBlockEnd.Line then
      begin
        vEnd := fBlockBegin;
        vStart := fBlockEnd;
      end
      else if fBlockBegin.Char <> fBlockEnd.Char then
      begin
        // No selection at all, or it is only on this line.
        vStart.Line := fBlockBegin.Line;
        vEnd.Line := vStart.Line;
        if fBlockBegin.Char < fBlockEnd.Char then
        begin
          vStart.Char := fBlockBegin.Char;
          vEnd.Char := fBlockEnd.Char;
        end
        else
        begin
          vStart.Char := fBlockEnd.Char;
          vEnd.Char := fBlockBegin.Char;
        end;
      end
      else
        bAnySelection := False;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bAnySelection then
      begin
        // Don't care if the selection is not visible.
        bAnySelection := (vEnd.Line >= vFirstLine) and (vStart.Line <= vLastLine);
        if bAnySelection then
        begin
          // Transform the selection from text space into screen space
          vSelStart := BufferToDisplayPos(vStart);
          vSelEnd := BufferToDisplayPos(vEnd);
          // In the column selection mode sort the begin and end of the selection,
          // this makes the painting code simpler.
          if (fActiveSelectionMode = smColumn) and (vSelStart.Column > vSelEnd.Column) then
            SwapInt(vSelStart.Column, vSelEnd.Column);
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: Boolean);
  begin
    Canvas.Fill.SolidColor := colFG or $FF000000;
  end;

  function ColumnToXValue(Col: Integer): Integer;
  begin
    Result := trunc(fTextOffset + Pred(Col) * fCharWidth);
  end;

  //todo: Review SpecialChars and HardTabs painting. Token parameter of PaintToken procedure could very probably be passed by reference.

  // Note: The PaintToken procedure will take care of invalid parameters
  // like empty token rect or invalid indices into TokenLen.
  // CharsBefore tells if Token starts at column one or not
  procedure PaintToken(Token: WideString;
    TokenLen, CharsBefore, First, Last: Integer);
  var
    Text: WideString;
    Counter, nX, nCharsToPaint: Integer;
    sTabbedToken: WideString;
    DoTabPainting: Boolean;
    i, TabStart, TabLen, CountOfAvgGlyphs, VisibleGlyphPart, FillerCount,
    NonFillerPos: Integer;
    rcTab: TvgRect;
  begin
    sTabbedToken := Token;
    DoTabPainting := False;

    Counter := Last - CharsBefore;
    while Counter > First - CharsBefore - 1 do
    begin
      if Length(Token) >= Counter then
      begin
        if fShowSpecChar and (Token[Counter] = #32) then
          Token[Counter] := SynSpaceGlyph
        else if Token[Counter] = #9 then
        begin
          Token[Counter] := #32;  //Tabs painted differently if necessary
          DoTabPainting := fShowSpecChar;
        end;
      end;
      Dec(Counter);
    end;

    if (Last >= First) and (rcToken.Right > rcToken.Left) then
    begin
      nX := ColumnToXValue(First);

      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);

      if (First > TokenLen) then
      begin
        nCharsToPaint := 0;
        Text := '';
      end
      else
      begin
        FillerCount := 0;
        NonFillerPos := First;
        while Token[NonFillerPos] = FillerChar do
        begin
          inc(FillerCount);
          inc(NonFillerPos);
        end;

        CountOfAvgGlyphs := {Length(Token[NonFillerPos]); //}Ceil(Canvas.TextWidth(Token[NonFillerPos]) / fCharWidth);

        // first visible part of the glyph (1-based)
        // (the glyph is visually sectioned in parts of size fCharWidth)
        VisibleGlyphPart := CountOfAvgGlyphs - FillerCount;

        // clip off invisible parts
        nX := nX - trunc(fCharWidth * (VisibleGlyphPart - 1));

        nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);

        // clip off partially visible glyphs at line end
        if WordWrap then
          while nX + fCharWidth * nCharsToPaint > ClientWidth do
          begin
            dec(nCharsToPaint);
            while (nCharsToPaint > 0) and (Token[First + nCharsToPaint - 1] = FillerChar) do
              dec(nCharsToPaint);
          end;

        // same as copy(Token, First, nCharsToPaint) and remove filler chars
        Text := ShrinkAtWideGlyphs(Token, First, nCharsToPaint);
      end;

      Canvas.FillText(vgRect(nX, rcToken.Top, rcToken.Right + 400, rcToken.Bottom),
        vgRect(0, 0, $FFFF, $FFFF),
        Copy(Text, 1, nCharsToPaint), false, AbsoluteOpacity, vgTextalignNear, vgTextAlignNear);

      if DoTabPainting then
      begin
        // fix everything before the FirstChar
        for i := 1 to First - 1 do               // wipe the text out so we don't
          if sTabbedToken[i] = #9 then           // count it out of the range
            sTabbedToken[i] := #32;              // we're looking for

        TabStart := pos(#9, sTabbedToken);
        rcTab.Top := rcToken.Top;
        rcTab.Bottom := rcToken.Bottom;
        while (TabStart > 0) and (TabStart >= First) and (TabStart <= Last) do
        begin
          TabLen := 1;
          while (TabStart + CharsBefore + TabLen - 1) mod FTabWidth <> 0 do inc(TabLen);
          Text := SynTabGlyphString;

          nX := ColumnToXValue(CharsBefore + TabStart + (TabLen div 2) - 1);
          if TabLen mod 2 = 0 then
            nX := nX + trunc(fCharWidth / 2)
          else nX := nX + trunc(fCharWidth);

          rcTab.Left := nX;
{!!}          rcTab.Right := nX + fCharWidth;

{          fTextDrawer.ExtTextOut(nX, rcTab.Top, ETOOptions, rcTab,
            PWideChar(Text), 1);}

          for i := 0 to TabLen - 1 do           //wipe the text out so we don't
            sTabbedToken[TabStart + i] := #32;  //count it again

          TabStart := pos(#9, sTabbedToken);
        end;
      end;
      rcToken.Left := rcToken.Right;
    end;
  end;

  procedure PaintHighlightToken(bFillToEOL: Boolean);
  var
    bComplexToken: Boolean;
    nC1, nC2, nC1Sel, nC2Sel: Integer;
    bU1, bSel, bU2: Boolean;
    nX1, nX2: Integer;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstCol, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastCol, TokenAccu.CharsBefore + TokenAccu.Len + 1);
    if bComplexLine then
    begin
      bU1 := (nC1 < nLineSelStart);
      bSel := (nC1 < nLineSelEnd) and (nC2 >= nLineSelStart);
      bU2 := (nC2 >= nLineSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end
    else
    begin
      bU1 := False; // to shut up Compiler warning Delphi 2
      bSel := bLineSelected;
      bU2 := False; // to shut up Compiler warning Delphi 2
      bComplexToken := False;
    end;
    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then
    begin
      // Initialize the colors and the font style.
      if not bSpecialLine then
      begin
        colBG := TokenAccu.BG;
        colFG := vgRGBtoBGR(colorToRGB(TokenAccu.FG));
      end;
      Canvas.Font.Style := TokenAccu.Style;
      if bSpecialLine and (eoSpecialLineDefaultFg in fOptions) then
        colFG := vgRGBtoBGR(colorToRGB(TokenAccu.FG));

      // Paint the chars
      if bComplexToken then
      begin
(*        // first unselected part of the token
        if bU1 then
        begin
          SetDrawingColors(False);
          rcToken.Right := ColumnToXValue(nLineSelStart);
          with TokenAccu do
            PaintToken(s, Len, CharsBefore, nC1, nLineSelStart);
        end;
        // selected part of the token
        SetDrawingColors(True);
        nC1Sel := Max(nLineSelStart, nC1);
        nC2Sel := Min(nLineSelEnd, nC2);
        rcToken.Right := ColumnToXValue(nC2Sel);
        with TokenAccu do
          PaintToken(s, Len, CharsBefore, nC1Sel, nC2Sel);
        // second unselected part of the token
        if bU2 then
        begin
          SetDrawingColors(False);
          rcToken.Right := ColumnToXValue(nC2);
          with TokenAccu do
            PaintToken(s, Len, CharsBefore, nLineSelEnd, nC2);
        end; *)
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);
        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nC2);
        // selection
        if bSel then
        begin
          Canvas.Fill.Assign(FSelectionFill);
          Canvas.FillRect(vgRect(vgMaxFloat(ColumnToXValue(nLineSelStart), ColumnToXValue(nC1)), rcLine.Top, vgMinFloat(ColumnToXValue(nC2), ColumnToXValue(nLineSelEnd)), rcLine.Bottom), 0, 0, [], AbsoluteOpacity);
        end;
      end
      else
      begin
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);
        with TokenAccu do
          PaintToken(s, Len, CharsBefore, nC1, nC2);
        // selection
        if bSel then
        begin
          Canvas.Fill.Assign(FSelectionFill);
          Canvas.FillRect(vgRect(ColumnToXValue(nC1), rcLine.Top, ColumnToXValue(nC2), rcLine.Bottom), 0, 0, [], AbsoluteOpacity);
        end;
      end;
    end;
  end;

  // Store the token chars with the attributes in the TokenAccu
  // record. This will paint any chars already stored if there is
  // a (visible) change in the attributes.
  procedure AddHighlightToken(const Token: WideString;
    CharsBefore, TokenLen: Integer;
    Foreground, Background: TColor;
    Style: TvgFontStyle);
  var
    bCanAppend: Boolean;
    bSpacesTest, bIsSpaces: Boolean;
    i: Integer;

    function TokenIsSpaces: Boolean;
    var
      pTok: PWideChar;
    begin
      if not bSpacesTest then
      begin
        bSpacesTest := True;
        pTok := PWideChar(Token);
        while pTok^ <> #0 do
        begin
          if pTok^ <> #32 then
            break;
          Inc(pTok);
        end;
        bIsSpaces := pTok^ = #0;
      end;
      Result := bIsSpaces;
    end;

  begin
    if (Background = clNone) or
      ((ActiveLineColor <> clNone) and (bCurrentLine)) then
    begin
      Background := colEditorBG;
    end;
    if Foreground = clNone then Foreground := FFontFill.SolidColor and not $FF000000;
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := False;
    bSpacesTest := False;
    if (TokenAccu.Len > 0) then
    begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style)
//        or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style) and TokenIsSpaces)
        then
      begin
        // either special colors or same colors
        if (bSpecialLine and not (eoSpecialLineDefaultFg in fOptions)) or bLineSelected or
          // background color must be the same and
          ((TokenAccu.BG = Background) and
          // foreground color must be the same or token is only spaces
          ((TokenAccu.FG = Foreground) or TokenIsSpaces)) then
        begin
          bCanAppend := True;
        end;
      end;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(False);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then
    begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then
      begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[TokenAccu.Len + i] := Token[i];
      Inc(TokenAccu.Len, TokenLen);
    end
    else
    begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then
      begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do
        TokenAccu.s[i] := Token[i];
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.Style := Style;
    end;
  end;

  function FontStyleToVG(Style: TFontStyles): TvgFontStyle;
  begin
    Result := vgFontRegular;
    if Style = [fsBold] then
      Result := vgFontBold;
    if Style = [fsItalic] then
      Result := vgFontItalic;
    if Style = [fsBold, fsItalic] then
      Result := vgFontBoldItalic;
  end;

  procedure PaintLines;
  var
    nLine: Integer; // line index for the loop
    cRow: Integer;
    sLine: WideString; // the current line (tab expanded)
    sLineExpandedAtWideGlyphs: WideString;
    sToken: WideString; // highlighter token info
    nTokenPos, nTokenLen: Integer;
    attr: TSynHighlighterAttributes;
    vAuxPos: TDisplayCoord;
    vFirstChar: Integer;
    vLastChar: Integer;
    vStartRow: Integer;
    vEndRow: Integer;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Left := fGutterWidth + 2;
    rcLine.Bottom := (aFirstRow - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then
    begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
    // Now loop through all the lines. The indices are valid for Lines.
    for nLine := vFirstLine to vLastLine do
    begin
      sLine := TSynEditStringList(Lines).ExpandedStrings[nLine - 1];
      sLineExpandedAtWideGlyphs := ExpandAtWideGlyphs(sLine);
      // determine whether will be painted with ActiveLineColor
      bCurrentLine := CaretY = nLine;
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      colFG := FFontFill.SolidColor and not $FF000000;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(nLine, colFG, colBG);
      if bSpecialLine then
      begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end
      else
      begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
      end;

      vStartRow := Max(LineToRow(nLine), aFirstRow);
      vEndRow := Min(LineToRow(nLine + 1) - 1, aLastRow);
      for cRow := vStartRow to vEndRow do
      begin
        if WordWrap then
        begin
          vAuxPos.Row := cRow;
          if Assigned(fHighlighter) then
            vAuxPos.Column := FirstCol
          else
            // When no highlighter is assigned, we must always start from the
            // first char in a row and PaintToken will do the actual clipping
            vAuxPos.Column := 1;
          vFirstChar := fWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;
          vAuxPos.Column := LastCol;
          vLastChar := fWordWrapPlugin.DisplayToBufferPos(vAuxPos).Char;
        end
        else
        begin
          vFirstChar := FirstCol;
          vLastChar := LastCol;
        end;
        // Get the information about the line selection. Three different parts
        // are possible (unselected before, selected, unselected after), only
        // unselected or only selected means bComplexLine will be False. Start
        // with no selection, compute based on the visible columns.
        bComplexLine := False;
        nLineSelStart := 0;
        nLineSelEnd := 0;
        // Does the selection intersect the visible area?
        if bAnySelection and (cRow >= vSelStart.Row) and (cRow <= vSelEnd.Row) then
        begin
          // Default to a fully selected line. This is correct for the smLine
          // selection mode and a good start for the smNormal mode.
          nLineSelStart := FirstCol;
          nLineSelEnd := LastCol + 1;
          if (fActiveSelectionMode = smColumn) or
            ((fActiveSelectionMode = smNormal) and (cRow = vSelStart.Row)) then
          begin
            if (vSelStart.Column > LastCol) then
            begin
              nLineSelStart := 0;
              nLineSelEnd := 0;
            end
            else if (vSelStart.Column > FirstCol) then
            begin
              nLineSelStart := vSelStart.Column;
              bComplexLine := True;
            end;
          end;
          if (fActiveSelectionMode = smColumn) or
            ((fActiveSelectionMode = smNormal) and (cRow = vSelEnd.Row)) then
          begin
            if (vSelEnd.Column < FirstCol) then
            begin
              nLineSelStart := 0;
              nLineSelEnd := 0;
            end
            else if (vSelEnd.Column < LastCol) then
            begin
              nLineSelEnd := vSelEnd.Column;
              bComplexLine := True;
            end;
          end;
        end; //endif bAnySelection

        // Update the rcLine rect to this line.
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := rcLine.Bottom + fTextHeight;

        bLineSelected := not bComplexLine and (nLineSelStart > 0);
        rcToken := rcLine;

        if not Assigned(fHighlighter) or not fHighlighter.Enabled then
        begin
          // Remove text already displayed (in previous rows)
          if (vFirstChar <> FirstCol) or (vLastChar <> LastCol) then
            sToken := Copy(sLineExpandedAtWideGlyphs, vFirstChar, vLastChar - vFirstChar)
          else
            sToken := Copy(sLineExpandedAtWideGlyphs, 1, vLastChar);
          if fShowSpecChar and (Length(sLineExpandedAtWideGlyphs) < vLastChar) then
            sToken := sToken + SynLineBreakGlyph;
          nTokenLen := Length(sToken);
          if bComplexLine then
          begin
            SetDrawingColors(True);
            PaintToken(sToken, nTokenLen, 0, FirstCol, LastCol);
            // selection
            rcToken.Left := vgMaxFloat(rcLine.Left, ColumnToXValue(nLineSelStart));
            rcToken.Right := vgMinFloat(rcLine.Right, ColumnToXValue(nLineSelEnd));
            Canvas.Fill.Assign(FSelectionFill);
            Canvas.FillRect(rcToken, 0, 0, [], AbsoluteOpacity);
          end
          else
          begin
            SetDrawingColors(bLineSelected);
            PaintToken(sToken, nTokenLen, 0, FirstCol, LastCol);
            // selection
            if bLineSelected then
            begin
              rcToken.Left := rcLine.Left;
              rcToken.Right := rcLine.Right;
              Canvas.Fill.Assign(FSelectionFill);
              Canvas.FillRect(rcToken, 0, 0, [], AbsoluteOpacity);
            end;
          end;
        end
        else
        begin
          // Initialize highlighter with line text and range info. It is
          // necessary because we probably did not scan to the end of the last
          // line - the internal highlighter range might be wrong.
          if nLine = 1 then
            fHighlighter.ResetRange
          else
            fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[nLine - 2]);
          fHighlighter.SetLineExpandedAtWideGlyphs(sLine, sLineExpandedAtWideGlyphs,
            nLine - 1);
          // Try to concatenate as many tokens as possible to minimize the count
          // of ExtTextOutW calls necessary. This depends on the selection state
          // or the line having special colors. For spaces the foreground color
          // is ignored as well.
          TokenAccu.Len := 0;
          nTokenPos := 0;
          nTokenLen := 0;
          attr := nil;
          // Test first whether anything of this token is visible.
          while not fHighlighter.GetEol do
          begin
            nTokenPos := fHighlighter.GetExpandedTokenPos;
            sToken := fHighlighter.GetExpandedToken;
            nTokenLen := Length(sToken);
            if nTokenPos + nTokenLen >= vFirstChar then
            begin
              if nTokenPos + nTokenLen > vLastChar then
              begin
                if nTokenPos > vLastChar then
                  break;
                if WordWrap then
                  nTokenLen := vLastChar - nTokenPos - 1
                else
                  nTokenLen := vLastChar - nTokenPos;
              end;
              // Remove offset generated by tokens already displayed (in previous rows)
              Dec(nTokenPos, vFirstChar - FirstCol);
              // It's at least partially visible. Get the token attributes now.
              attr := fHighlighter.GetTokenAttribute;
              if Assigned(attr) then
                AddHighlightToken(sToken, nTokenPos, nTokenLen, attr.Foreground,
                  attr.Background, FontStyleToVG(attr.Style))
              else
                AddHighlightToken(sToken, nTokenPos, nTokenLen, colFG, colBG,
                  FFont.Style);
            end;
            // Let the highlighter scan the next token.
            fHighlighter.Next;
          end;
          // Draw anything that's left in the TokenAccu record. Fill to the end
          // of the invalid area with the correct colors.
          if fShowSpecChar and fHighlighter.GetEol then
          begin
            if (attr = nil) or (attr <> fHighlighter.CommentAttribute) then
               attr := fHighlighter.WhitespaceAttribute;
            AddHighlightToken(SynLineBreakGlyph, nTokenPos + nTokenLen, 1,
              attr.Foreground, attr.Background, vgFontRegular);
          end;
          PaintHighlightToken(True);
        end;
        // Now paint the right edge if necessary. We do it line by line to reduce
        // the flicker. Should not cost very much anyway, compared to the many
        // calls to ExtTextOutW.
        if bDoRightEdge then
        begin
          Canvas.Stroke.Style := vgBrushSolid;
          Canvas.Stroke.Assign(FFontFill);
          Canvas.DrawLine(vgPoint(nRightEdge + 0.5, rcLine.Top), vgPoint(nRightEdge + 0.5, rcLine.Bottom), AbsoluteOpacity);
        end;
      end; //endfor cRow
      bCurrentLine := False;
    end; //endfor cLine
  end;

{ end local procedures }

begin
  vFirstLine := RowToLine(aFirstRow);
  vLastLine := RowToLine(aLastRow);

  bCurrentLine := False;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  SynTabGlyphString := SynTabGlyph;
  bDoRightEdge := False;
  if (fRightEdge > 0) then
  begin // column value
    nRightEdge := fTextOffset + trunc(fRightEdge * fCharWidth); // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
    begin
      bDoRightEdge := True;
{      Canvas.Pen.Color := fRightEdgeColor;
      Canvas.Pen.Width := 1;}
    end;
  end;
  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < fGutterWidth + 2) then
  begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, fGutterWidth);
    rcToken.Right := fGutterWidth + 2;
    // Paint whole left edge of the text with same color.
    // (value of WhiteAttribute can vary in e.g. MultiSyn)
    if Highlighter <> nil then
      Highlighter.ResetRange;
{    Canvas.Brush.Color := colEditorBG;
    Canvas.FillRect(rcToken);}
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
  // Paint the visible text lines. To make this easier, compute first the
  // necessary information about the selected area: is there any visible
  // selected area, and what are its lines / columns?
  if (vLastLine >= vFirstLine) then
  begin
    ComputeSelectionInfo;
    Canvas.Font.Style := Font.Style;
    PaintLines;
  end;
  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (aLastRow - TopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then
  begin
    if Highlighter <> nil then
      Highlighter.ResetRange;
    Canvas.Fill.SolidColor := colEditorBG;
//    Canvas.FillRect(rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge then
    begin
      Canvas.DrawLine(vgPoint(nRightEdge + 0.5, rcToken.Top), vgPoint(nRightEdge + 0.5, rcToken.Bottom + 1), AbsoluteOpacity);
    end;
  end;
end;

procedure TvgCustomSynEdit.PasteFromClipboard;
var
  AddPasteEndMarker: boolean;
  vStartOfBlock: TBufferCoord;
  vEndOfBlock: TBufferCoord;
  StoredPaintLock: Integer;
  PasteMode: TSynSelectionMode;
  Data: THandle;
  Insertion: WideString;
begin
  if not CanPaste then
    exit;
  DoOnPaintTransient(ttBefore);
  BeginUndoBlock;
  AddPasteEndMarker := False;
  PasteMode := SelectionMode;
  try
    if Clipbrd.Clipboard.HasFormat(CF_UNICODETEXT) then
    begin
      Data := Clipbrd.Clipboard.GetAsHandle(CF_UNICODETEXT);
      try
        if Data <> 0 then
          Insertion := PWideChar(GlobalLock(Data));
      finally
        if Data <> 0 then GlobalUnlock(Data);
      end;
    end
    else
      Insertion := Clipbrd.Clipboard.AsText;

    fUndoList.AddChange(crPasteBegin, BlockBegin, BlockEnd, '', smNormal);
    AddPasteEndMarker := True;
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        fActiveSelectionMode);
    end
    else
      ActiveSelectionMode := SelectionMode;

    if SelAvail then
    begin
      vStartOfBlock := BlockBegin;
      vEndOfBlock := BlockEnd;
      fBlockBegin := vStartOfBlock;
      fBlockEnd := vEndOfBlock;

      // Pasting always occurs at column 0 when current selection is
      // smLine type
      if fActiveSelectionMode = smLine then
        vStartOfBlock.Char := 1;
    end
    else
      vStartOfBlock := CaretXY;

    SetSelTextPrimitiveEx(PasteMode, PWideChar(Insertion), True);

    vEndOfBlock := BlockEnd;
    if PasteMode = smNormal then
      fUndoList.AddChange(crPaste, vStartOfBlock, vEndOfBlock, SelText,
        PasteMode)
    else if PasteMode = smColumn then
      // Do nothing. Moved to InsertColumn
    else if PasteMode = smLine then
      if CaretX = 1 then
        fUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
          BufferCoord(CharsInWindow, vEndOfBlock.Line - 1), SelText, smLine)
      else
        fUndoList.AddChange(crPaste, BufferCoord(1, vStartOfBlock.Line),
          vEndOfBlock, SelText, smNormal);
  finally
    if AddPasteEndMarker then
      fUndoList.AddChange(crPasteEnd, BlockBegin, BlockEnd, '', smNormal);
    EndUndoBlock;
  end;

  // ClientRect can be changed by UpdateScrollBars if eoHideShowScrollBars
  // is enabled
  if eoHideShowScrollBars in Options then
  begin
    StoredPaintLock := fPaintLock;
    try
      fPaintLock := 0;
      UpdateScrollBars;
    finally
      fPaintLock := StoredPaintLock;
    end;
  end;

  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]);
  DoOnPaintTransient(ttAfter);
end;

procedure TvgCustomSynEdit.SelectAll;
var
  LastPt: TBufferCoord;
begin
  LastPt.Char := 1;
  LastPt.Line := Lines.Count;
  if LastPt.Line > 0 then
    Inc(LastPt.Char, Length(Lines[LastPt.Line - 1]))
  else
    LastPt.Line  := 1;
  SetCaretAndSelection(LastPt, BufferCoord(1, 1), LastPt);
  // Selection should have changed...
  StatusChanged([scSelection]);
end;

procedure TvgCustomSynEdit.SetBlockBegin(Value: TBufferCoord);
var
  nInval1, nInval2: Integer;
  SelChanged: Boolean;
begin
  ActiveSelectionMode := SelectionMode;
  if (eoScrollPastEol in Options) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (fActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if SelAvail then
  begin
    if fBlockBegin.Line < fBlockEnd.Line then
    begin
      nInval1 := Min(Value.Line, fBlockBegin.Line);
      nInval2 := Max(Value.Line, fBlockEnd.Line);
    end
    else
    begin
      nInval1 := Min(Value.Line, fBlockEnd.Line);
      nInval2 := Max(Value.Line, fBlockBegin.Line);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := True;
  end
  else
  begin
    SelChanged :=
      (fBlockBegin.Char <> Value.Char) or (fBlockBegin.Line <> Value.Line) or
      (fBlockEnd.Char <> Value.Char) or (fBlockEnd.Line <> Value.Line);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TvgCustomSynEdit.SetBlockEnd(Value: TBufferCoord);
var
  nLine: Integer;
begin
  ActiveSelectionMode := SelectionMode;
  if not (eoNoSelection in Options) then
  begin
    if (eoScrollPastEol in Options) and not WordWrap then
      Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
    else
      Value.Char := Max(Value.Char, 1);
    Value.Line := MinMax(Value.Line, 1, Lines.Count);
    if (fActiveSelectionMode = smNormal) then
      if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
        Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
      else
        Value.Char := 1;
    if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
    begin
      if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
      begin
        if (fActiveSelectionMode = smColumn) and (Value.Char <> fBlockEnd.Char) then
        begin
          InvalidateLines(
            Min(fBlockBegin.Line, Min(fBlockEnd.Line, Value.Line)),
            Max(fBlockBegin.Line, Max(fBlockEnd.Line, Value.Line)));
          fBlockEnd := Value;
        end
        else begin
          nLine := fBlockEnd.Line;
          fBlockEnd := Value;
          if (fActiveSelectionMode <> smColumn) or (fBlockBegin.Char <> fBlockEnd.Char) then
            InvalidateLines(nLine, fBlockEnd.Line);
        end;
        StatusChanged([scSelection]);
      end;
    end;
  end;
end;

procedure TvgCustomSynEdit.SetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  SetCaretXY(vNewCaret);
end;

procedure TvgCustomSynEdit.SetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  SetCaretXY(vNewCaret);
end;

procedure TvgCustomSynEdit.InternalSetCaretX(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Char := Value;
  vNewCaret.Line := CaretY;
  InternalSetCaretXY(vNewCaret);
end;

procedure TvgCustomSynEdit.InternalSetCaretY(Value: Integer);
var
  vNewCaret: TBufferCoord;
begin
  vNewCaret.Line := Value;
  vNewCaret.Char := CaretX;
  InternalSetCaretXY(vNewCaret);
end;

function TvgCustomSynEdit.GetCaretXY: TBufferCoord;
begin
  Result.Char := CaretX;
  Result.Line := CaretY;
end;

function TvgCustomSynEdit.GetDisplayX: Integer;
begin
  Result := DisplayXY.Column;
end;

function TvgCustomSynEdit.GetDisplayY: Integer;
begin
  if not WordWrap then
    Result := CaretY
  else
    Result := DisplayXY.Row;
end;

Function TvgCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
  if WordWrap and fCaretAtEOL then
  begin
    if Result.Column = 1 then
    begin
      Dec(Result.Row);
      Result.Column := fWordWrapPlugin.GetRowLength(Result.Row) +1;
    end
    else begin
      // Work-around situations where fCaretAtEOL should have been updated because of
      //text change (it's only valid when Column = 1). Updating it in ProperSetLine()
      //would probably be the right thing, but...
      fCaretAtEOL := False;
    end;
  end;
end;

procedure TvgCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
//there are two setCaretXY methods.  One Internal, one External.  The published
//property CaretXY (re)sets the block as well
begin
  IncPaintLock;
  try
    Include(fStatusChanges, scSelection);
    SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  finally
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.InternalSetCaretXY(const Value: TBufferCoord);
begin
  SetCaretXYEx(True, Value);
end;

procedure TvgCustomSynEdit.UpdateLastCaretX;
begin
  fLastCaretX := DisplayX;
end;

procedure TvgCustomSynEdit.SetCaretXYEx(CallEnsureCursorPos: Boolean; Value: TBufferCoord);
var
  nMaxX: Integer;
  vTriggerPaint: boolean;
begin
  fCaretAtEOL := False;
  vTriggerPaint := true;
  if vTriggerPaint then
    DoOnPaintTransient(ttBefore);
  if WordWrap then
    nMaxX := MaxInt
  else
    nMaxX := MaxScrollWidth + 1;
  if Value.Line > Lines.Count then
    Value.Line := Lines.Count;
  if Value.Line < 1 then
  begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Line := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end
  else
  begin
    if not (eoScrollPastEol in fOptions) then
      nMaxX := Length(Lines[Value.Line - 1]) + 1;
  end;
  if (Value.Char > nMaxX) and (not(eoScrollPastEol in Options) or
    not(eoAutoSizeMaxScrollWidth in Options)) then
  begin
    Value.Char := nMaxX;
  end;
  if Value.Char < 1 then
    Value.Char := 1;
  if (Value.Char <> fCaretX) or (Value.Line <> fCaretY) then
  begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.Char then
      begin
        fCaretX := Value.Char;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Line then
      begin
        if ActiveLineColor <> clNone then
        begin
          InvalidateLine(Value.Line);
          InvalidateLine(fCaretY);
        end;
        fCaretY := Value.Line;
        Include(fStatusChanges, scCaretY);
      end;
      // Call UpdateLastCaretX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want fLastCaretX to be
      // left in an undefined state if that happens.
      UpdateLastCaretX;
      if CallEnsureCursorPos then
        EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
      Include(fStateFlags, sfScrollbarChanged);
    finally
      DecPaintLock;
    end;
  end
  else begin
    // Also call UpdateLastCaretX if the caret didn't move. Apps don't know
    // anything about fLastCaretX and they shouldn't need to. So, to avoid any
    // unwanted surprises, always update fLastCaretX whenever CaretXY is
    // assigned to.
    // Note to SynEdit developers: If this is undesirable in some obscure
    // case, just save the value of fLastCaretX before assigning to CaretXY and
    // restore it afterward as appropriate.
    UpdateLastCaretX;
  end;
  if vTriggerPaint then
    DoOnPaintTransient(ttAfter);
end;

function TvgCustomSynEdit.CaretInView: Boolean;
var
  vCaretRowCol: TDisplayCoord;
begin
  vCaretRowCol := DisplayXY;
  Result := (vCaretRowCol.Column >= LeftChar)
    and (vCaretRowCol.Column <= LeftChar + CharsInWindow)
    and (vCaretRowCol.Row >= TopLine)
    and (vCaretRowCol.Row <= TopLine + LinesInWindow);
end;

procedure TvgCustomSynEdit.SetActiveLineColor(Value: TColor);
begin
  if (fActiveLineColor<>Value) then
  begin
    fActiveLineColor:=Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TvgCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then
  begin
    fGutterWidth := Value;
    fTextOffset := fGutterWidth + 2 - trunc((LeftChar - 1) * fCharWidth);
    if true then
    begin
      fCharsInWindow := trunc(Max(ClientWidth - fGutterWidth - 2, 0) / fCharWidth);
      if WordWrap then
        fWordWrapPlugin.DisplayChanged;
      UpdateScrollBars;
      Repaint;
    end;
  end;
end;

procedure TvgCustomSynEdit.SetLeftChar(Value: Integer);
var
  MaxVal: Integer;
  iDelta: Integer;
  iTextArea: TvgRect;
begin
  if WordWrap then
    Value := 1;

  if eoScrollPastEol in Options then
  begin
    if eoAutoSizeMaxScrollWidth in Options then
      MaxVal := MaxInt - CharsInWindow
    else
      MaxVal := MaxScrollWidth - CharsInWindow + 1
  end
  else
  begin
    MaxVal := TSynEditStringList(Lines).LengthOfLongestLine;
    if MaxVal > CharsInWindow then
      MaxVal := MaxVal - CharsInWindow + 1
    else
      MaxVal := 1;
  end;
  Value := MinMax(Value, 1, MaxVal);
  if Value <> fLeftChar then
  begin
    iDelta := fLeftChar - Value;
    fLeftChar := Value;
    fTextOffset := fGutterWidth + 2 - trunc((LeftChar - 1) * fCharWidth);
    if Abs(iDelta) < CharsInWindow then
    begin
      iTextArea := LocalRect;
      iTextArea.Left := iTextArea.Left + fGutterWidth + 2;
      Repaint;
    end
    else
      InvalidateLines(-1, -1);
    if (Options >= [eoAutoSizeMaxScrollWidth, eoScrollPastEol]) and
      (MaxScrollWidth < LeftChar + CharsInWindow) then
    begin
      MaxScrollWidth := LeftChar + CharsInWindow
    end
    else
      UpdateScrollBars;
    StatusChanged([scLeftChar]);
  end;
end;

procedure TvgCustomSynEdit.SetLines(Value: TUnicodeStrings);
begin
  Lines.Assign(Value);
end;

procedure TvgCustomSynEdit.SetLineText(Value: WideString);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
end;

procedure TvgCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: Boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning])
    and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;

procedure TvgCustomSynEdit.SetSelTextPrimitive(const Value: WideString);
begin
  SetSelTextPrimitiveEx(fActiveSelectionMode, PWideChar(Value), True);
end;

// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if eoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if eoScrollPastEol is not set.
procedure TvgCustomSynEdit.SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode;
  Value: PWideChar; AddToUndoList: Boolean);
var
  BB, BE: TBufferCoord;
  TempString: WideString;

  procedure DeleteSelection;
  var
    x, MarkOffset: Integer;
    UpdateMarks: Boolean;
  begin
    UpdateMarks := False;
    MarkOffset := 0;
    case fActiveSelectionMode of
      smNormal:
        begin
          if Lines.Count > 0 then
          begin
              // Create a string that contains everything on the first line up
              // to the selection mark, and everything on the last line after
              // the selection mark.
            TempString := Copy(Lines[BB.Line - 1], 1, BB.Char - 1) +
              Copy(Lines[BE.Line - 1], BE.Char, MaxInt);
              // Delete all lines in the selection range.
            TSynEditStringList(Lines).DeleteLines(BB.Line, BE.Line - BB.Line);
              // Put the stuff that was outside of selection back in.
            if Options >= [eoScrollPastEol, eoTrimTrailingSpaces] then
              TempString := TrimTrailingSpaces(TempString);
            Lines[BB.Line - 1] := TempString;
          end;
          UpdateMarks := True;
          InternalCaretXY := BB;
        end;
      smColumn:
        begin
            // swap X if needed
          if BB.Char > BE.Char then
            SwapInt(Integer(BB.Char), Integer(BE.Char));

          for x := BB.Line - 1 to BE.Line - 1 do
          begin
            TempString := Lines[x];
            Delete(TempString, BB.Char, BE.Char - BB.Char);
            ProperSetLine(x, TempString);
          end;
          // Lines never get deleted completely, so keep caret at end.
          InternalCaretXY := BufferCoord(BB.Char, fBlockEnd.Line);
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
        end;
      smLine:
        begin
          if BE.Line = Lines.Count then
          begin
            Lines[BE.Line - 1] := '';
            for x := BE.Line - 2 downto BB.Line - 1 do
              Lines.Delete(x);
          end
          else begin
            for x := BE.Line - 1 downto BB.Line - 1 do
              Lines.Delete(x);
          end;
          // smLine deletion always resets to first column.
          InternalCaretXY := BufferCoord(1, BB.Line);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      DoLinesDeleted(BB.Line, BE.Line - BB.Line + MarkOffset);
  end;

  procedure InsertText;

    function CountLines(p: PWideChar): Integer;
    begin
      Result := 0;
      while p^ <> #0 do
      begin
        if p^ = #13 then
          Inc(p);
        if p^ = #10 then
          Inc(p);
        Inc(Result);
        p := GetEOL(p);
      end;
    end;

    function InsertNormal: Integer;
    var
      sLeftSide: WideString;
      sRightSide: WideString;
      Str: WideString;
      Start: PWideChar;
      P: PWideChar;
    begin
      Result := 0;
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if CaretX - 1 > Length(sLeftSide) then
      begin
        sLeftSide := sLeftSide + WideStringOfChar(#32,
          CaretX - 1 - Length(sLeftSide));
      end;
      sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));
      // step1: insert the first line of Value into current line
      Start := PWideChar(Value);
      P := GetEOL(Start);
      if P^ <> #0 then
      begin
        Str := sLeftSide + Copy(Value, 1, P - Start);
        ProperSetLine(CaretY - 1, Str);
        TSynEditStringList(Lines).InsertLines(CaretY, CountLines(P));
      end
      else begin
        Str := sLeftSide + Value + sRightSide;
        ProperSetLine(CaretY -1, Str);
      end;
      // step2: insert left lines of Value
      while P^ <> #0 do
      begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(fCaretY);
        Include(fStatusChanges, scCaretY);
        Start := P;
        P := GetEOL(Start);
        if P = Start then
        begin
          if p^ <> #0 then
            Str := ''
          else
            Str := sRightSide;
        end
        else begin
          SetString(Str, Start, P - Start);
          if p^ = #0 then
            Str := Str + sRightSide
        end;
        ProperSetLine(CaretY -1, Str);
        Inc(Result);
      end;
      if eoTrimTrailingSpaces in Options then
        if sRightSide = '' then
          fCaretX := GetExpandedLength(Str, TabWidth) + 1
        else
          fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(TrimTrailingSpaces(sRightSide))
      else fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
      StatusChanged([scCaretX]);
    end;

    function InsertColumn: Integer;
    var
      Str: WideString;
      Start: PWideChar;
      P: PWideChar;
      Len: Integer;
      InsertPos: Integer;
      LineBreakPos: TBufferCoord;
    begin
      Result := 0;
      // Insert string at current position
      InsertPos := CaretX;
      Start := PWideChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * sizeof(WideChar));
          if CaretY > Lines.Count then
          begin
            Inc(Result);
            TempString := WideStringOfChar(#32, InsertPos - 1) + Str;
            Lines.Add('');
            if AddToUndoList then
            begin
              LineBreakPos.Line := CaretY -1;
              LineBreakPos.Char := Length(Lines[CaretY - 2]) + 1;
              fUndoList.AddChange(crLineBreak, LineBreakPos, LineBreakPos, '', smNormal);
            end;
          end
          else begin
            TempString := Lines[CaretY - 1];
            Len := Length(TempString);
            if Len < InsertPos then
            begin
              TempString :=
                TempString + WideStringOfChar(#32, InsertPos - Len - 1) + Str
            end
            else
                Insert(Str, TempString, InsertPos);
          end;
          ProperSetLine(CaretY - 1, TempString);
          // Add undo change here from PasteFromClipboard
          if AddToUndoList then
          begin
            fUndoList.AddChange(crPaste, BufferCoord(InsertPos, CaretY),
               BufferCoord(InsertPos + (P - Start), CaretY), '', fActiveSelectionMode);
          end;
        end;
        if P^ = #13 then
        begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Include(fStatusChanges, scCaretX);
    end;

    function InsertLine: Integer;
    var
      Start: PWideChar;
      P: PWideChar;
      Str: WideString;
      n: Integer;
    begin
      Result := 0;
      fCaretX := 1;
      // Insert string before current line
      Start := PWideChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * sizeof(WideChar));
        end
        else
          Str := '';
        if (P^ = #0) then
        begin
          n := Lines.Count;
          if (n >= CaretY) then
            Lines[CaretY - 1] := Str + Lines[CaretY - 1]
          else
            Lines.Add(Str);
          if eoTrimTrailingSpaces in Options then
            Lines[CaretY - 1] := TrimTrailingSpaces(Lines[CaretY - 1]);
          fCaretX := 1 + Length(Str);
        end
        else begin
          ProperSetLine(CaretY - 1, Str);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
          Inc(Result);
          if P^ = #13 then
            Inc(P);
          if P^ = #10 then
            Inc(P);
          Start := P;
        end;
      until P^ = #0;
      StatusChanged([scCaretX]);
    end;

  var
    StartLine: Integer;
    StartCol: Integer;
    InsertedLines: Integer;
  begin
    if Value = '' then
      Exit;

    StartLine := CaretY;
    StartCol := CaretX;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;
    // We delete selected based on the current selection mode, but paste
    // what's on the clipboard according to what it was when copied.
    // Update marks
    if InsertedLines > 0 then
    begin
      if (PasteMode = smNormal) and (StartCol > 1) then
        Inc(StartLine);
      DoLinesInserted(StartLine, InsertedLines);
    end;
    // Force caret reset
    InternalCaretXY := CaretXY;
  end;

begin
  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    if SelAvail then
    begin
      DeleteSelection;
      InternalCaretXY := BB;
    end;
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    if CaretY < 1 then
      InternalCaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.SynSetText(const Value: WideString);
begin
  Lines.Text := Value;
end;

procedure TvgCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayLineCount)
  else
    Value := Min(Value, DisplayLineCount - fLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then
  begin
    Delta := TopLine - Value;
    fTopLine := Value;
    Repaint;
    UpdateScrollBars;
    UpdateCaret;
    StatusChanged([scTopLine]);
  end;
end;

procedure TvgCustomSynEdit.ShowCaret;
begin
  if not (eoNoCaret in Options) and not (sfCaretVisible in fStateFlags) then
  begin
    ShowCaretProc;
    Include(fStateFlags, sfCaretVisible);
  end;
end;

procedure TvgCustomSynEdit.UpdateCaret;
var
  CX, CY: single;
  iClientRect: TvgRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TvgPoint;
begin
  if (PaintLock <> 0) or not (IsFocused or FAlwaysShowCaret) then
    Include(fStateFlags, sfCaretChanged)
  else
  begin
    Exclude(fStateFlags, sfCaretChanged);
    vCaretDisplay := DisplayXY;
    if WordWrap and (vCaretDisplay.Column > CharsInWindow + 1) then
      vCaretDisplay.Column := CharsInWindow + 1;
    vCaretPix := RowColumnToPixels(vCaretDisplay);
    CX := vCaretPix.X + trunc(FCaretOffset.X + HScrollBarValue);
    CY := vCaretPix.Y + trunc(FCaretOffset.Y + VScrollBarValue);
    if ContentLayout <> nil then
    begin
      CX := CX + ContentLayout.Position.X;
      CY := CY + ContentLayout.Position.Y;
    end;
    if (CX >= fGutterWidth + HScrollBarValue) and (CX < ClientWidth + HScrollBarValue) and
      (CY >= VScrollBarValue) and (CY < VScrollBarValue + ClientHeight)
    then
    begin
      ShowCaret;
      SetCaretPos(vgPoint(CX, CY - 2));
      SetCaretSize(vgPoint(2, FTextHeight));
      SetCaretColor(FFontFill.SolidColor);
    end
    else
    begin
      HideCaret;
      SetCaretPos(vgPoint(CX, CY));
    end;
  end;
end;

function TvgCustomSynEdit.ScanFrom(Index: Integer): Integer;
var
  iRange: TSynEditRange;
begin
  Result := Index;
  if Result >= Lines.Count then Exit;

  if Result = 0 then
    fHighlighter.ResetRange
  else
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Result - 1]);

  repeat
    fHighlighter.SetLine(Lines[Result], Result);
    fHighlighter.NextToEol;
    iRange := fHighlighter.GetRange;
    if TSynEditStringList(Lines).Ranges[Result] = iRange then
      Exit; // avoid the final Decrement
    TSynEditStringList(Lines).Ranges[Result] := iRange;
    Inc(Result);
  until (Result = Lines.Count);
  Dec(Result);
end;

procedure TvgCustomSynEdit.ListCleared(Sender: TObject);
begin
  if WordWrap then
    fWordWrapPlugin.Reset;

  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TvgRect), 0);
  Repaint;
  // set caret and selected block to start of text
  CaretXY := BufferCoord(1, 1);
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TvgCustomSynEdit.ListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then
    ScanFrom(aIndex);

  if WordWrap then
    fWordWrapPlugin.LinesDeleted(aIndex, aCount);

  InvalidateLines(aIndex + 1, MaxInt);
  InvalidateGutterLines(aIndex + 1, MaxInt);
end;

procedure TvgCustomSynEdit.ListInserted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vLastScan: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then
  begin
    vLastScan := Index;
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until vLastScan >= Index + aCount;
  end;

  if WordWrap then
    fWordWrapPlugin.LinesInserted(Index, aCount);

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);

  if (eoAutoSizeMaxScrollWidth in fOptions) then
  begin
    L := TSynEditStringList(Lines).ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TvgCustomSynEdit.ListPutted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  L: Integer;
  vEndLine: Integer;
begin
  vEndLine := Index +1;
  if WordWrap then
  begin
    if fWordWrapPlugin.LinesPutted(Index, aCount) <> 0 then
      vEndLine := MaxInt;
    InvalidateGutterLines(Index + 1, vEndLine);
  end;
  if Assigned(fHighlighter) then
  begin
    vEndLine := Max(vEndLine, ScanFrom(Index) + 1);
    // If this editor is chained then the real owner of text buffer will probably
    // have already parsed the changes, so ScanFrom will return immediately.
    if fLines <> fOrigLines then
      vEndLine := MaxInt;
  end;
  InvalidateLines(Index + 1, vEndLine);

  if (eoAutoSizeMaxScrollWidth in fOptions) then
  begin
    L := TSynEditStringList(Lines).ExpandedStringLengths[Index];
    if L > MaxScrollWidth then
      MaxScrollWidth := L;
  end;
end;

procedure TvgCustomSynEdit.ScanRanges;
var
  i: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    fHighlighter.ResetRange;
    i := 0;
    repeat
      fHighlighter.SetLine(Lines[i], i);
      fHighlighter.NextToEol;
      TSynEditStringList(Lines).Ranges[i] := fHighlighter.GetRange;
      Inc(i);
    until i >= Lines.Count;
  end;
end;

procedure TvgCustomSynEdit.SetWordBlock(Value: TBufferCoord);
var
  vBlockBegin: TBufferCoord;
  vBlockEnd: TBufferCoord;
  TempString: WideString;

  procedure CharScan;
  var
    cRun: Integer;
  begin
    { search BlockEnd }
    vBlockEnd.Char := Length(TempString);
    for cRun := Value.Char to Length(TempString) do
      if not IsIdentChar(TempString[cRun]) then
      begin
        vBlockEnd.Char := cRun;
        Break;
      end;
    { search BlockBegin }
    vBlockBegin.Char := 1;
    for cRun := Value.Char - 1 downto 1 do
      if not IsIdentChar(TempString[cRun]) then
      begin
        vBlockBegin.Char := cRun + 1;
        Break;
      end;
  end;

begin
  if (eoScrollPastEol in Options) and not WordWrap then
    Value.Char := MinMax(Value.Char, 1, fMaxScrollWidth + 1)
  else
    Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  TempString := Lines[Value.Line - 1] + #0; //needed for CaretX = LineLength + 1
  if Value.Char > Length(TempString) then
  begin
    InternalCaretXY := BufferCoord(Length(TempString), Value.Line);
    exit;
  end;

  CharScan;

  vBlockBegin.Line := Value.Line;
  vBlockEnd.Line := Value.Line;
  SetCaretAndSelection(vBlockEnd, vBlockBegin, vBlockEnd);
  InvalidateLine(Value.Line);
  StatusChanged([scSelection]);
end;

procedure TvgCustomSynEdit.DblClick;
var
  ptMouse: TvgPoint;
begin
{  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then
  begin
    if not (eoNoSelection in fOptions) then
      SetWordBlock(CaretXY);
    inherited;
    Include(fStateFlags, sfDblClicked);
    MouseCapture := False;
  end
  else
    inherited;}
end;

function TvgCustomSynEdit.GetCanUndo: Boolean;
begin
  result := not ReadOnly and fUndoList.CanUndo;
end;

function TvgCustomSynEdit.GetCanRedo: Boolean;
begin
  result := not ReadOnly and fRedoList.CanUndo;
end;

function TvgCustomSynEdit.GetCanPaste;
begin
//  Result := not ReadOnly and ClipboardProvidesText;
end;

procedure TvgCustomSynEdit.InsertBlock(const BB, BE: TBufferCoord; ChangeStr: PWideChar;
  AddToUndoList: Boolean);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(BB, BB, BE);
  ActiveSelectionMode := smColumn;
  SetSelTextPrimitiveEx(smColumn, ChangeStr, AddToUndoList);
  StatusChanged([scSelection]);
end;

procedure TvgCustomSynEdit.Redo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if fRedoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := UndoList.BlockChangeNumber;
      Item := fRedoList.PopItem;
      try
        UndoList.BlockChangeNumber := Item.ChangeNumber;
        fUndoList.AddGroupBreak;
      finally
        UndoList.BlockChangeNumber := OldBlockNumber;
        Item.Free;
      end;
      UpdateModifiedStatus;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    exit;

  FLastChange := FRedoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteBegin;
  FPasteAction := FLastChange = crPasteBegin;
  FSpecial1 := FLastChange = crSpecial1Begin;
  FSpecial2 := FLastChange = crSpecial2Begin;

  Item := fRedoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.ChangeNumber;
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
             FKeepGoing:= (FRedoList.LastChangeReason <> crAutoCompleteEnd)
          else if FPasteAction then
             FKeepGoing:= (FRedoList.LastChangeReason <> crPasteEnd)
          else if FSpecial1 then
             FKeepGoing := (FRedoList.LastChangeReason <> crSpecial1End)
          else if FSpecial2 then
             FKeepGoing := (FRedoList.LastChangeReason <> crSpecial2End)
          else if Item.ChangeNumber = OldChangeNumber then
             FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not(FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not(FKeepGoing);

      //we need to eat the last command since it does nothing and also update modified status...
      if (FAutoComplete and (FRedoList.LastChangeReason = crAutoCompleteEnd)) or
         (FPasteAction and (FRedoList.LastChangeReason = crPasteEnd)) or
         (FSpecial1 and (FRedoList.LastChangeReason = crSpecial1End)) or
         (FSpecial2 and (FRedoList.LastChangeReason = crSpecial2End)) then
      begin
        RedoItem;
        UpdateModifiedStatus;
      end;

    finally
      fUndoList.BlockChangeNumber := SaveChangeNumber;
    end;
    RemoveGroupBreak;
  end;
end;

procedure TvgCustomSynEdit.RedoItem;
var
  Item: TSynEditUndoItem;
  Run, StrToDelete: PWideChar;
  Len: Integer;
  TempString: WideString;
  CaretPt: TBufferCoord;
  ChangeScrollPastEol: boolean;
  BeginX: integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fRedoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    fUndoList.InsideRedo := True;
    case Item.ChangeReason of
      crCaret:
        begin
          fUndoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          fUndoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', fActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeStartPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          InternalCaretXY := Item.ChangeEndPos;
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, SelText, Item.ChangeSelMode);
          if Item.ChangeReason = crDragDropInsert then begin
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
        end;
      crDeleteAfterCursor, crSilentDeleteAfterCursor:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TempString := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TempString, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeEndPos;
        end;
      crDelete, crSilentDelete:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TempString := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TempString, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crLineBreak:
        begin
          CaretPt := Item.ChangeStartPos;
          SetCaretAndSelection(CaretPt, CaretPt, CaretPt);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
       crUnindent :
         begin // re-delete the (raggered) column
           // Delete string
           StrToDelete := PWideChar(Item.ChangeStr);
           InternalCaretY := Item.ChangeStartPos.Line;
          if Item.ChangeSelMode = smColumn then
            BeginX := Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char)
          else
            BeginX := 1;
           repeat
             Run := GetEOL(StrToDelete);
             if Run <> StrToDelete then
             begin
               Len := Run - StrToDelete;
               if Len > 0 then
               begin
                 TempString := Lines[CaretY - 1];
                 Delete(TempString, BeginX, Len);
                 Lines[CaretY - 1] := TempString;
               end;
             end
             else
               Len := 0;
             if Run^ = #13 then
             begin
               Inc(Run);
               if Run^ = #10 then
                 Inc(Run);
               Inc(fCaretY);
             end;
             StrToDelete := Run;
           until Run^ = #0;
          if Item.ChangeSelMode = smColumn then
            SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
              Item.ChangeEndPos)
          else begin
            // restore selection
            CaretPt.Char := Item.ChangeStartPos.Char - fTabWidth;
            CaretPt.Line := Item.ChangeStartPos.Line;
            SetCaretAndSelection( CaretPt, CaretPt,
              BufferCoord(Item.ChangeEndPos.Char - Len, Item.ChangeEndPos.Line) );
          end;
           // add to undo list
           fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
         end;
      crWhiteSpaceAdd:
        begin
          fUndoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, '', Item.ChangeSelMode);
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeEndPos,
            Item.ChangeEndPos);
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), True);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    fUndoList.InsideRedo := False;
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.Undo;

  procedure RemoveGroupBreak;
  var
    Item: TSynEditUndoItem;
    OldBlockNumber: Integer;
  begin
    if fUndoList.LastChangeReason = crGroupBreak then
    begin
      OldBlockNumber := RedoList.BlockChangeNumber;
      try
        Item := fUndoList.PopItem;
        RedoList.BlockChangeNumber := Item.ChangeNumber;
        Item.Free;
        fRedoList.AddGroupBreak;
      finally
        RedoList.BlockChangeNumber := OldBlockNumber;
      end;
    end;
  end;

var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
  SaveChangeNumber: integer;
  FLastChange : TSynChangeReason;
  FAutoComplete: Boolean;
  FPasteAction: Boolean;
  FSpecial1: Boolean;
  FSpecial2: Boolean;
  FKeepGoing: Boolean;
begin
  if ReadOnly then
    exit;

  RemoveGroupBreak;

  FLastChange := FUndoList.LastChangeReason;
  FAutoComplete := FLastChange = crAutoCompleteEnd;
  FPasteAction := FLastChange = crPasteEnd;
  FSpecial1 := FLastChange = crSpecial1End;
  FSpecial2 := FLastChange = crSpecial2End;

  Item := fUndoList.PeekItem;
  if Item <> nil then
  begin
    OldChangeNumber := Item.ChangeNumber;
    SaveChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.ChangeNumber;

    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
        if Item = nil then
          FKeepGoing := False
        else begin
          if FAutoComplete then
             FKeepGoing := (FUndoList.LastChangeReason <> crAutoCompleteBegin)
          else if FPasteAction then
             FKeepGoing := (FUndoList.LastChangeReason <> crPasteBegin)
          else if FSpecial1 then
             FKeepGoing := (FUndoList.LastChangeReason <> crSpecial1Begin)
          else if FSpecial2 then
             FKeepGoing := (FUndoList.LastChangeReason <> crSpecial2Begin)
          else if Item.ChangeNumber = OldChangeNumber then
             FKeepGoing := True
          else begin
            FKeepGoing := ((eoGroupUndo in FOptions) and
              (FLastChange = Item.ChangeReason) and
              not(FLastChange in [crIndent, crUnindent]));
          end;
          FLastChange := Item.ChangeReason;
        end;
      until not(FKeepGoing);

      //we need to eat the last command since it does nothing and also update modified status...
      if (FAutoComplete and (FUndoList.LastChangeReason = crAutoCompleteBegin)) or
         (FPasteAction and (FUndoList.LastChangeReason = crPasteBegin)) or
         (FSpecial1 and (FUndoList.LastChangeReason = crSpecial1Begin)) or
         (FSpecial2 and (FUndoList.LastChangeReason = crSpecial2Begin)) then
      begin
        UndoItem;
        UpdateModifiedStatus;
       end;

    finally
      fRedoList.BlockChangeNumber := SaveChangeNumber;
    end;
  end;
end;

procedure TvgCustomSynEdit.UndoItem;
var
  Item: TSynEditUndoItem;
  TmpPos: TBufferCoord;
  TmpStr: WideString;
  ChangeScrollPastEol: Boolean;
  BeginX: Integer;
begin
  ChangeScrollPastEol := not (eoScrollPastEol in Options);
  Item := fUndoList.PopItem;
  if Assigned(Item) then
  try
    ActiveSelectionMode := Item.ChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);
    case Item.ChangeReason of
      crCaret:
        begin
          fRedoList.AddChange(Item.ChangeReason, CaretXY, CaretXY, '', fActiveSelectionMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crSelection:
        begin
          fRedoList.AddChange(Item.ChangeReason, BlockBegin, BlockEnd, '', fActiveSelectionMode);
          SetCaretAndSelection(CaretXY, Item.ChangeStartPos, Item.ChangeEndPos);
        end;
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
      crDeleteAfterCursor, crDelete,
      crSilentDelete, crSilentDeleteAfterCursor,
      crDeleteAll:
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          if Item.ChangeSelMode = smColumn then
            TmpPos := BufferCoord(
              Min(Item.ChangeStartPos.Char, Item.ChangeEndPos.Char),
              Min(Item.ChangeStartPos.Line, Item.ChangeEndPos.Line))
          else
            TmpPos := TBufferCoord(vgMinPoint(
              vgPoint(Item.ChangeStartPos.Char, Item.ChangeStartPos.Line), TvgPoint(Item.ChangeEndPos)));
          if (Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]) and (TmpPos.Line > Lines.Count) then
          begin
            InternalCaretXY := BufferCoord(1, Lines.Count);
            fLines.Add('');
          end;
          CaretXY := TmpPos;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr),
            False );
          if Item.ChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]
          then
            TmpPos := Item.ChangeStartPos
          else
            TmpPos := Item.ChangeEndPos;
          if Item.ChangeReason in [crSilentDelete, crSilentDeleteAfterCursor]
          then
            InternalCaretXY := TmpPos
          else begin
            SetCaretAndSelection(TmpPos, Item.ChangeStartPos,
              Item.ChangeEndPos);
          end;
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
          if Item.ChangeReason = crDeleteAll then begin
            InternalCaretXY := BufferCoord(1, 1);
            fBlockEnd := BufferCoord(1, 1);
          end;
          EnsureCursorPosVisible;
        end;
      crLineBreak:
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          InternalCaretXY := Item.ChangeStartPos;
          if CaretY > 0 then
          begin
            TmpStr := Lines.Strings[CaretY - 1];
            if (Length(TmpStr) < CaretX - 1)
              and (LeftSpaces(Item.ChangeStr) = 0)
            then
              TmpStr := TmpStr + WideStringOfChar(#32, CaretX - 1 - Length(TmpStr));
            ProperSetLine(CaretY - 1, TmpStr + Item.ChangeStr);
            Lines.Delete(Item.ChangeEndPos.Line);
          end
          else
            ProperSetLine(CaretY - 1, Item.ChangeStr);
          DoLinesDeleted(CaretY + 1, 1);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, '', Item.ChangeSelMode);
        end;
      crIndent:
        begin
          SetCaretAndSelection(Item.ChangeEndPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
       crUnindent: // reinsert the (raggered) column that was deleted
         begin
           // reinsert the string
          if Item.ChangeSelMode <> smColumn then
            InsertBlock(BufferCoord(1, Item.ChangeStartPos.Line),
              BufferCoord(1, Item.ChangeEndPos.Line),
              PWideChar(Item.ChangeStr), False)
          else
          begin
            BeginX := Min( Item.ChangeStartPos.Char, Item.ChangeEndPos.Char );
            InsertBlock(BufferCoord(BeginX, Item.ChangeStartPos.Line),
              BufferCoord(BeginX, Item.ChangeEndPos.Line),
              PWideChar(Item.ChangeStr), False);
          end;
           SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
             Item.ChangeEndPos);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
             Item.ChangeEndPos, Item.ChangeStr, Item.ChangeSelMode);
        end;
      crWhiteSpaceAdd:
        begin
          SetCaretAndSelection(Item.ChangeStartPos, Item.ChangeStartPos,
            Item.ChangeEndPos);
          TmpStr := SelText;
          SetSelTextPrimitiveEx(Item.ChangeSelMode, PWideChar(Item.ChangeStr), True);
          fRedoList.AddChange(Item.ChangeReason, Item.ChangeStartPos,
            Item.ChangeEndPos, TmpStr, Item.ChangeSelMode);
          InternalCaretXY := Item.ChangeStartPos;
        end;
    end;
  finally
    if ChangeScrollPastEol then
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then
  begin
    DoOnClearBookmark(fBookMarks[BookMark]);
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark] := nil;
  end
end;

procedure TvgCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  iNewPos: TBufferCoord;
begin
  if (BookMark in [0..9]) and
     assigned(fBookMarks[BookMark]) and
     (fBookMarks[BookMark].Line <= fLines.Count)
  then
  begin
    iNewPos.Char := fBookMarks[BookMark].Char;
    iNewPos.Line := fBookMarks[BookMark].Line;
    //call it this way instead to make sure that the caret ends up in the middle
    //if it is off screen (like Delphi does with bookmarks)
    SetCaretXYEx(False, iNewPos);
    EnsureCursorPosVisibleEx(True);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  end;
end;

procedure TvgCustomSynEdit.GotoLineAndCenter(ALine: Integer);
begin
  SetCaretXYEx( False, BufferCoord(1, ALine) );
  if SelAvail then
    InvalidateSelection;
  fBlockBegin.Char := fCaretX;
  fBlockBegin.Line := fCaretY;
  fBlockEnd := fBlockBegin;
  EnsureCursorPosVisibleEx(True);
end;

procedure TvgCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  mark: TSynEditMark;
begin
{  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    with mark do
    begin
      Line := Y;
      Char := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := True;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) then
    begin
      if assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
  end;}
end;


procedure TvgCustomSynEdit.ChainListCleared(Sender: TObject);
begin
  if Assigned(fChainListCleared) then
    fChainListCleared(Sender);
  TSynEditStringList(fOrigLines).OnCleared(Sender);
end;

procedure TvgCustomSynEdit.ChainListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListDeleted) then
    fChainListDeleted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnDeleted(Sender, aIndex, aCount);
end;

procedure TvgCustomSynEdit.ChainListInserted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListInserted) then
    fChainListInserted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnInserted(Sender, aIndex, aCount);
end;

procedure TvgCustomSynEdit.ChainListPutted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListPutted) then
    fChainListPutted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnPutted(Sender, aIndex, aCount);
end;

procedure TvgCustomSynEdit.ChainLinesChanging(Sender: TObject);
begin
  if Assigned(fChainLinesChanging) then
    fChainLinesChanging(Sender);
  TSynEditStringList(fOrigLines).OnChanging(Sender);
end;

procedure TvgCustomSynEdit.ChainLinesChanged(Sender: TObject);
begin
  if Assigned(fChainLinesChanged) then
    fChainLinesChanged(Sender);
  TSynEditStringList(fOrigLines).OnChange(Sender);
end;

procedure TvgCustomSynEdit.ChainUndoRedoAdded(Sender: TObject);
var
  iList: TSynEditUndoList;
  iHandler: TNotifyEvent;
begin
  if Sender = fUndoList then
  begin
    iList := fOrigUndoList;
    iHandler := fChainUndoAdded;
  end
  else { if Sender = fRedoList then }
  begin
    iList := fOrigRedoList;
    iHandler := fChainRedoAdded;
  end;
  if Assigned(iHandler) then
    iHandler(Sender);
  iList.OnAddedUndo(Sender);
end;

procedure TvgCustomSynEdit.UnHookTextBuffer;
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  if fLines = fOrigLines then
    Exit;

  vOldWrap := WordWrap;
  WordWrap := False;

  //first put back the real methods
  with TSynEditStringList(fLines) do
  begin
    OnCleared := fChainListCleared;
    OnDeleted := fChainListDeleted;
    OnInserted := fChainListInserted;
    OnPutted := fChainListPutted;
    OnChanging := fChainLinesChanging;
    OnChange := fChainLinesChanged;
  end;
  fUndoList.OnAddedUndo := fChainUndoAdded;
  fRedoList.OnAddedUndo := fChainRedoAdded;

  fChainListCleared := nil;
  fChainListDeleted := nil;
  fChainListInserted := nil;
  fChainListPutted := nil;
  fChainLinesChanging := nil;
  fChainLinesChanged := nil;
  fChainUndoAdded := nil;

  //make the switch
  fLines := fOrigLines;
  fUndoList := fOrigUndoList;
  fRedoList := fOrigRedoList;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TvgCustomSynEdit.HookTextBuffer(aBuffer: TSynEditStringList;
  aUndo, aRedo: TSynEditUndoList);
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  Assert(fLines = fOrigLines);

  vOldWrap := WordWrap;
  WordWrap := False;

  if fChainedEditor <> nil then
    RemoveLinesPointer
  else if fLines <> fOrigLines then
    UnHookTextBuffer;

  //store the current values and put in the chained methods
  fChainListCleared := aBuffer.OnCleared;
    aBuffer.OnCleared := ChainListCleared;
  fChainListDeleted := aBuffer.OnDeleted;
    aBuffer.OnDeleted := ChainListDeleted;
  fChainListInserted := aBuffer.OnInserted;
    aBuffer.OnInserted := ChainListInserted;
  fChainListPutted := aBuffer.OnPutted;
    aBuffer.OnPutted := ChainListPutted;
  fChainLinesChanging := aBuffer.OnChanging;
    aBuffer.OnChanging := ChainLinesChanging;
  fChainLinesChanged := aBuffer.OnChange;
    aBuffer.OnChange := ChainLinesChanged;

  fChainUndoAdded := aUndo.OnAddedUndo;
    aUndo.OnAddedUndo := ChainUndoRedoAdded;
  fChainRedoAdded := aRedo.OnAddedUndo;
    aRedo.OnAddedUndo := ChainUndoRedoAdded;

  //make the switch
  fLines := aBuffer;
  fUndoList := aUndo;
  fRedoList := aRedo;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TvgCustomSynEdit.LinesHookChanged;
var
  iLongestLineLength: Integer;
begin
  Repaint;
  if eoAutoSizeMaxScrollWidth in fOptions then
  begin
    iLongestLineLength := TSynEditStringList(Lines).LengthOfLongestLine;
    if iLongestLineLength > MaxScrollWidth then
      MaxScrollWidth := iLongestLineLength;
  end;
  UpdateScrollBars;
end;

procedure TvgCustomSynEdit.SetLinesPointer(ASynEdit: TvgCustomSynEdit);
begin
  HookTextBuffer(TSynEditStringList(ASynEdit.Lines),
    ASynEdit.UndoList, ASynEdit.RedoList);

  fChainedEditor := ASynEdit;
  ASynEdit.FreeNotification(Self);
end;

procedure TvgCustomSynEdit.RemoveLinesPointer;
begin
  {$IFDEF SYN_COMPILER_5_UP}
  if Assigned(fChainedEditor) then
    RemoveFreeNotification(fChainedEditor);
  {$ENDIF}
  fChainedEditor := nil;

  UnHookTextBuffer;
end;

{$IFDEF SYN_CLX}
function TvgCustomSynEdit.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  Result := inherited EventFilter(Sender, Event);
  case QEvent_type(Event) of
    QEventType_FocusIn:
      begin
        {$IFDEF SYN_LINUX}
        if not FDeadKeysFixed then
        begin
          FDeadKeysFixed := True;
          with TEdit.Create(Self) do
          begin
            Parent := Self;
            BorderStyle := bsNone;
            Color := Self.Color;
            ReadOnly := True;
            Top := ClientRect.Top;
            Left := ClientRect.Left + fGutterWidth + 2;
            Show;
            SetFocus;
            Free;
          end;
          SetFocus;
        end
        else
        {$ENDIF}
        begin
          InitializeCaret;
          if FHideSelection and SelAvail then
            InvalidateSelection;
        end;
      end;
    QEventType_FocusOut:
      begin
        HideCaret;
        kTextDrawer.DestroyCaret;
        if FHideSelection and SelAvail then
          InvalidateSelection;
        EndDrag(False);
      end;
  end;
end;
{$ENDIF}

procedure TvgCustomSynEdit.DragCanceled;
begin
  inherited;
end;

procedure TvgCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then
  begin
    fRightEdge := Value;
    Repaint;
  end;
end;

procedure TvgCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: Integer;
  rcInval: TvgRect;
begin
  if fRightEdgeColor <> Value then
  begin
    fRightEdgeColor := Value;
    if true then
    begin
      nX := fTextOffset + trunc(fRightEdge * fCharWidth);
      rcInval := vgRect(nX - 1, 0, nX + 1, Height);
      InvalidateRect(rcInval, False);
    end;
  end;
end;

function TvgCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndoActions;
end;

procedure TvgCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then
  begin
    fUndoList.MaxUndoActions := Value;
    fRedoList.MaxUndoActions := Value;
  end;
end;

procedure TvgCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = fSearchEngine then
    begin
      SearchEngine := nil;
    end;

    if AComponent = fHighlighter then
    begin
      Highlighter := nil;
    end;

    if AComponent = fChainedEditor then
    begin
      RemoveLinesPointer;
    end;

    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then
      begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

procedure TvgCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then
  begin
    if Assigned(fHighlighter) then
    begin
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
{$IFDEF SYN_COMPILER_5_UP}
      fHighlighter.RemoveFreeNotification(Self);
{$ENDIF}
    end;
    if Assigned(Value) then
    begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    if not(csDestroying in ComponentState) then
      HighlighterAttrChanged(fHighlighter);
  end;
end;

procedure TvgCustomSynEdit.SetHideSelection(const Value: Boolean);
begin
  if fHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateSelection;
  end;
end;

procedure TvgCustomSynEdit.SetInsertMode(const Value: Boolean);
begin
  if fInserting <> Value then
  begin
    fInserting := Value;
    if not (csDesigning in ComponentState) then
      // Reset the caret.
      InitializeCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TvgCustomSynEdit.InitializeCaret;
var
  ct: TSynEditCaretType;
  cw, ch: single;
begin
  // CreateCaret automatically destroys the previous one, so we don't have to
  // worry about cleaning up the old one here with DestroyCaret.
  // Ideally, we will have properties that control what these two carets look like.
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine:
      begin
        cw := fCharWidth;
        ch := 2;
        FCaretOffset := vgPoint(0, fTextHeight - 2);
      end;
    ctHalfBlock:
      begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := vgPoint(0, ch);
      end;
    ctBlock:
      begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := vgPoint(0, 0);
      end;
    else
    begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := vgPoint(-1, 0);
    end;
  end;
  Exclude(fStateFlags, sfCaretVisible);

  if IsFocused or FAlwaysShowCaret then
  begin
    UpdateCaret;
  end;
end;

procedure TvgCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then
  begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TvgCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then
  begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TvgCustomSynEdit.SetMaxScrollWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, MaxInt - 1);
  if MaxScrollWidth <> Value then
  begin
    fMaxScrollWidth := Value;
    if eoScrollPastEol in Options then
      UpdateScrollBars;
  end;
end;

procedure TvgCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

procedure TvgCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean);
var
  TmpMiddle: Integer;
  VisibleX: Integer;
  vCaretRow: Integer;
begin
  IncPaintLock;
  try
    // Make sure X is visible
    VisibleX := DisplayX;
    if VisibleX < LeftChar then
      LeftChar := VisibleX
    else if VisibleX >= CharsInWindow + LeftChar then
      LeftChar := VisibleX - CharsInWindow + 1
    else
      LeftChar := LeftChar;

    // Make sure Y is visible
    vCaretRow := DisplayY;
    if ForceToMiddle then
    begin
      if vCaretRow < (TopLine - 1) then
      begin
        TmpMiddle := LinesInWindow div 2;
        if vCaretRow - TmpMiddle < 0 then
          TopLine := 1
        else
          TopLine := vCaretRow - TmpMiddle + 1;
      end
      else if vCaretRow > (TopLine + (LinesInWindow - 2)) then
      begin
        TmpMiddle := LinesInWindow div 2;
        TopLine := vCaretRow - (LinesInWindow - 1) + TmpMiddle;
      end;
    end
    else begin
      if vCaretRow < TopLine then
        TopLine := vCaretRow
      else if vCaretRow > TopLine + Max(1, LinesInWindow) - 1 then
        TopLine := vCaretRow - (LinesInWindow - 1)
      else
        TopLine := TopLine;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TvgCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TvgCustomSynEdit.TranslateKeyCode(Code: word; Shift: TShiftState;
  var Data: pointer): TSynEditorCommand;
var
  i: Integer;
{$IFNDEF SYN_COMPILER_3_UP}
const
  VK_ACCEPT = $30;
{$ENDIF}
begin
  i := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if i >= 0 then
    Result := KeyStrokes[i].Command
  else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then
      Result := Keystrokes[i].Command
    else
      Result := ecNone;
  end;
{$IFDEF SYN_CLX}
  if Result = ecNone then
{$ELSE}
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
{$ENDIF}
  begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end
  else
  begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TvgCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then
  begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(False, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
      ExecuteCommand(Command, AChar, Data);
    // notify hooked command handlers after the command was executed inside of
    // the class
    if Command <> ecNone then
      NotifyHookedCommandHandlers(True, Command, AChar, Data);
  end;
  DoOnCommandProcessed(Command, AChar, Data);
end;

procedure TvgCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
  Data: pointer);

  procedure SetSelectedTextEmpty;
  var
    vSelText: WideString;
    vUndoBegin, vUndoEnd: TBufferCoord;
  begin
    vUndoBegin := fBlockBegin;
    vUndoEnd := fBlockEnd;
    vSelText := SelText;
    SetSelTextPrimitive('');
    if (vUndoBegin.Line < vUndoEnd.Line) or (
      (vUndoBegin.Line = vUndoEnd.Line) and (vUndoBegin.Char < vUndoEnd.Char)) then
    begin
      fUndoList.AddChange(crDelete, vUndoBegin, vUndoEnd, vSelText,
        fActiveSelectionMode);
    end
    else
    begin
      fUndoList.AddChange(crDeleteAfterCursor, vUndoBegin, vUndoEnd, vSelText,
        fActiveSelectionMode);
    end;
  end;

  procedure ForceCaretX(aCaretX: integer);
  var
    vRestoreScroll: boolean;
  begin
    vRestoreScroll := not (eoScrollPastEol in fOptions);
    Include(fOptions, eoScrollPastEol);
    try
      InternalCaretX := aCaretX;
    finally
      if vRestoreScroll then
        Exclude(fOptions, eoScrollPastEol);
    end;
  end;

var
  CX: Integer;
  Len: Integer;
  Temp: WideString;
  Temp2: WideString;
  Helper: WideString;
  TabBuffer: WideString;
  SpaceBuffer: WideString;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  BackCounter: Integer;
  StartOfBlock: TBufferCoord;
  EndOfBlock: TBufferCoord;
  bChangeScroll: Boolean;
  moveBkm: Boolean;
  WP: TBufferCoord;
  Caret: TBufferCoord;
  CaretNew: TBufferCoord;
  counter: Integer;
  InsDelta: Integer;
  iUndoBegin, iUndoEnd: TBufferCoord;
  vCaretRow: Integer;
  vTabTrim: integer;
  s: WideString;
  i: Integer;
begin
  IncPaintLock;
  try
    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft:
        MoveCaretHorz(-1, Command = ecSelLeft);
      ecRight, ecSelRight:
        MoveCaretHorz(1, Command = ecSelRight);
      ecPageLeft, ecSelPageLeft:
        MoveCaretHorz(-CharsInWindow, Command = ecSelPageLeft);
      ecPageRight, ecSelPageRight:
        MoveCaretHorz(CharsInWindow, Command = ecSelPageRight);
      ecLineStart, ecSelLineStart:
        begin
          DoHomeKey(Command = ecSelLineStart);
        end;
      ecLineEnd, ecSelLineEnd:
        DoEndKey(Command = ecSelLineEnd);
// vertical caret movement or selection
      ecUp, ecSelUp:
        begin
          MoveCaretVert(-1, Command = ecSelUp);
          Repaint;
        end;
      ecDown, ecSelDown:
        begin
          MoveCaretVert(1, Command = ecSelDown);
          Repaint;
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown:
        begin
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp]) then
            counter := -counter;
          TopLine := TopLine + counter;
          MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);
          Repaint;
        end;
      ecPageTop, ecSelPageTop:
        begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine) );
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageTop);
          Repaint;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine + LinesInWindow -1) );
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Repaint;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          CaretNew.Char := 1;
          CaretNew.Line := 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorTop);
          Repaint;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew.Char := 1;
          CaretNew.Line := Lines.Count;
          if (CaretNew.Line > 0) then
            CaretNew.Char := Length(Lines[CaretNew.Line - 1]) + 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          Repaint;
        end;
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then
        begin
          MoveCaretAndSelection(CaretXY, TBufferCoord(Data^), Command = ecSelGotoXY);
          Repaint;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          CaretNew := NextWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordRight);
        end;
      ecSelWord:
      	begin
      	  SetSelWord;
      	end;
      ecSelectAll:
        begin
          SelectAll;
        end;
      ecDeleteLastChar:
        if not ReadOnly then begin
          DoOnPaintTransientEx(ttBefore,true);
          try
            if SelAvail then
              SetSelectedTextEmpty
            else begin
              Temp := LineText;
              TabBuffer := TSynEditStringList(Lines).ExpandedStrings[CaretY - 1];
              Len := Length(Temp);
              Caret := CaretXY;
              vTabTrim := 0;
              if CaretX > Len + 1 then
              begin
                Helper := '';
                if eoSmartTabDelete in fOptions then
                begin
                  //It's at the end of the line, move it to the length
                  if Len > 0 then
                    InternalCaretX := Len + 1
                  else begin
                    //move it as if there were normal spaces there
                    SpaceCount1 := CaretX - 1;
                    SpaceCount2 := 0;
                    // unindent
                    if SpaceCount1 > 0 then
                    begin
                      BackCounter := CaretY - 2;
                      //It's better not to have if statement inside loop
                      if (eoTrimTrailingSpaces in Options) and (Len = 0) then
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpacesEx(Lines[BackCounter], True);
                          if SpaceCount2 < SpaceCount1 then
                            break;
                          Dec(BackCounter);
                        end
                      else
                        while BackCounter >= 0 do
                        begin
                          SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                          if SpaceCount2 < SpaceCount1 then
                            break;
                          Dec(BackCounter);
                        end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                    UpdateLastCaretX;
                    fStateFlags := fStateFlags + [sfCaretChanged];
                    StatusChanged([scCaretX]);
                  end;
                end
                else begin
                  // only move caret one column
                  InternalCaretX := CaretX - 1;
                end;
              end else if CaretX = 1 then begin
                // join this line with the last line if possible
                if CaretY > 1 then
                begin
                  InternalCaretY := CaretY - 1;
                  InternalCaretX := Length(Lines[CaretY - 1]) + 1;
                  Lines.Delete(CaretY);
                  DoLinesDeleted(CaretY+1, 1);
                  if eoTrimTrailingSpaces in Options then
                    Temp := TrimTrailingSpaces(Temp);

                  LineText := LineText + Temp;
                  Helper := #13#10;
                end;
              end
              else begin
                // delete text before the caret
                SpaceCount1 := LeftSpaces(Temp);
                SpaceCount2 := 0;
                if (Temp[CaretX - 1] <= #32) and (SpaceCount1 = CaretX - 1) then
                begin
                  if eoSmartTabDelete in fOptions then
                  begin
                    // unindent
                    if SpaceCount1 > 0 then
                    begin
                      BackCounter := CaretY - 2;
                      while BackCounter >= 0 do
                      begin
                        SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                        if SpaceCount2 < SpaceCount1 then
                          break;
                        Dec(BackCounter);
                      end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    Helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                    Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                  end
                  else begin
                    SpaceCount2 := SpaceCount1;
                    //how much till the next tab column
                    BackCounter  := (DisplayX - 1) mod FTabWidth;
                    if BackCounter = 0 then BackCounter := FTabWidth;

                    SpaceCount1 := 0;
                    CX := DisplayX - BackCounter;
                    while (SpaceCount1 < FTabWidth) and
                          (SpaceCount1 < BackCounter) and
                          (TabBuffer[CX] <> #9) do
                    begin
                      Inc(SpaceCount1);
                      Inc(CX);
                    end;
                    {$IFOPT R+}
                    // Avoids an exception when compiled with $R+.
                    // 'CX' can be 'Length(TabBuffer)+1', which isn't an AV and evaluates
                    //to #0. But when compiled with $R+, Delphi raises an Exception.
                    if CX <= Length(TabBuffer) then
                    {$ENDIF}
                    if TabBuffer[CX] = #9 then
                      SpaceCount1 := SpaceCount1 + 1;

                    if SpaceCount2 = SpaceCount1 then
                    begin
                      Helper := Copy(Temp, 1, SpaceCount1);
                      Delete(Temp, 1, SpaceCount1);
                    end
                    else begin
                      Helper := Copy(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                      Delete(Temp, SpaceCount2 - SpaceCount1 + 1, SpaceCount1);
                    end;
                    SpaceCount2 := 0;
                  end;
                  ProperSetLine(CaretY - 1, Temp);
                  fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                  UpdateLastCaretX;
                  fStateFlags := fStateFlags + [sfCaretChanged];
                  StatusChanged([scCaretX]);
                end
                else begin
                  // delete char
                  counter := 1;
                  InternalCaretX := CaretX - counter;
                  // Stores the previous "expanded" CaretX if the line contains tabs.
                  if (eoTrimTrailingSpaces in Options) and (Len <> Length(TabBuffer)) then
                    vTabTrim := CharIndex2CaretPos(CaretX, TabWidth, Temp);
                  Helper := Copy(Temp, CaretX, counter);
                  Delete(Temp, CaretX, counter);
                  ProperSetLine(CaretY - 1, Temp);
                  // Calculates a delta to CaretX to compensate for trimmed tabs.
                  if vTabTrim <> 0 then
                    if Length(Temp) <> Length(LineText) then
                      Dec(vTabTrim, CharIndex2CaretPos(CaretX, TabWidth, LineText))
                    else
                      vTabTrim := 0;
                end;
              end;
              if (Caret.Char <> CaretX) or (Caret.Line <> CaretY) then
              begin
                fUndoList.AddChange(crSilentDelete, CaretXY, Caret, Helper,
                  smNormal);
                if vTabTrim <> 0 then
                  ForceCaretX(CaretX + vTabTrim);
              end;
            end;
            EnsureCursorPosVisible;
          finally
            DoOnPaintTransientEx(ttAfter,true);
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);

          if SelAvail then
            SetSelectedTextEmpty
          else begin
            // Call UpdateLastCaretX. Even though the caret doesn't move, the
            // current caret position should "stick" whenever text is modified.
            UpdateLastCaretX;
            Temp := LineText;
            Len := Length(Temp);
            if CaretX <= Len then
            begin
              // delete char
              counter := 1;
              Helper := Copy(Temp, CaretX, counter);
              Caret.Char := CaretX + counter;
              Caret.Line := CaretY;
              Delete(Temp, CaretX, counter);
              ProperSetLine(CaretY - 1, Temp);
            end
            else begin
              // join line with the line after
              if CaretY < Lines.Count then
              begin
                Helper := WideStringOfChar(#32, CaretX - 1 - Len);
                ProperSetLine(CaretY - 1, Temp + Helper + Lines[CaretY]);
                Caret.Char := 1;
                Caret.Line := CaretY + 1;
                Helper := #13#10;
                Lines.Delete(CaretY);
                DoLinesDeleted(CaretY +1, 1);
              end;
            end;
            if (Caret.Char <> CaretX) or (Caret.Line <> CaretY) then
            begin
              fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, Caret,
                Helper, smNormal);
            end;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);
          Len := Length(LineText);
          if Command = ecDeleteWord then
          begin
            WP := WordEnd;
            Temp := LineText;
            if (WP.Char < CaretX) or ((WP.Char = CaretX) and (WP.Line < fLines.Count)) then
            begin
              if WP.Char > Len then
              begin
                Inc(WP.Line);
                WP.Char := 1;
                Temp := Lines[WP.Line - 1];
              end
              else if Temp[WP.Char] <> #32 then
                Inc(WP.Char);
            end;
            {$IFOPT R+}
            Temp := Temp + #0;
            {$ENDIF}
            if Temp <> '' then
              while Temp[WP.Char] = #32 do
                Inc(WP.Char);
          end
          else begin
            WP.Char := Len + 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then
          begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            Helper := SelText;
            SetSelTextPrimitive(WideStringOfChar(' ', CaretX - BlockBegin.Char));
            fUndoList.AddChange(crSilentDeleteAfterCursor, CaretXY, WP,
              Helper, smNormal);
            InternalCaretXY := CaretXY;
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);
          if Command = ecDeleteLastWord then
            WP := PrevWordPos
          else begin
            WP.Char := 1;
            WP.Line := CaretY;
          end;
          if (WP.Char <> CaretX) or (WP.Line <> CaretY) then
          begin
            SetBlockBegin(CaretXY);
            SetBlockEnd(WP);
            ActiveSelectionMode := smNormal;
            Helper := SelText;
            SetSelTextPrimitive('');
            fUndoList.AddChange(crSilentDelete, WP, CaretXY, Helper,
              smNormal);
            InternalCaretXY := WP;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecDeleteLine:
        if not ReadOnly and (Lines.Count > 0) and not ((CaretY = Lines.Count) and (Length(Lines[CaretY - 1]) = 0))
        then begin
          DoOnPaintTransient(ttBefore);
          if SelAvail then
            SetBlockBegin(CaretXY);
          Helper := LineText;
          if CaretY = Lines.Count then
          begin
            Lines[CaretY - 1] := '';
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(Length(Helper) + 1, CaretY), Helper, smNormal);
          end
          else begin
            Lines.Delete(CaretY - 1);
            Helper := Helper + #13#10;
            fUndoList.AddChange(crSilentDeleteAfterCursor, BufferCoord(1, CaretY),
              BufferCoord(1, CaretY + 1), Helper, smNormal);
            DoLinesDeleted(CaretY - 1, 1);
          end;
          InternalCaretXY := BufferCoord(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          UndoList.BeginBlock;
          try
          if SelAvail then
          begin
            Helper := SelText;
            iUndoBegin := fBlockBegin;
            iUndoEnd := fBlockEnd;
            SetSelTextPrimitive('');
            fUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, Helper,
              fActiveSelectionMode);
          end;
          Temp := LineText;
          Temp2 := Temp;
// This is sloppy, but the Right Thing would be to track the column of markers
// too, so they could be moved depending on whether they are after the caret...
          InsDelta := Ord(CaretX = 1);
          Len := Length(Temp);
          if Len > 0 then
          begin
            if Len >= CaretX then
            begin
              if CaretX > 1 then
              begin
                Temp := Copy(LineText, 1, CaretX - 1);
                SpaceCount1 := LeftSpacesEx(Temp,true);
                Delete(Temp2, 1, CaretX - 1);
                Lines.Insert(CaretY, GetLeftSpacing(SpaceCount1, True) + Temp2);
                ProperSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  InternalCaretXY := BufferCoord(
                    Length(GetLeftSpacing(SpaceCount1,true)) + 1,
                    CaretY + 1);
              end
              else begin
                Lines.Insert(CaretY - 1, '');
                fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, Temp2,
                  smNormal);
                if Command = ecLineBreak then
                  InternalCaretY := CaretY + 1;
              end;
            end
            else begin
              SpaceCount2 := 0;
              BackCounter := CaretY;
              if eoAutoIndent in Options then
              begin
                repeat
                  Dec(BackCounter);
                  Temp := Lines[BackCounter];
                  SpaceCount2 := LeftSpaces(Temp);
                until (BackCounter = 0) or (Temp <> '');
              end;
              Lines.Insert(CaretY, '');
              Caret := CaretXY;
              if Command = ecLineBreak then
              begin
                if SpaceCount2 > 0 then
                begin
                  SpaceBuffer := Copy(Lines[BackCounter], 1, SpaceCount2);
                  InternalCaretXY := BufferCoord(1, CaretY +1);
                  for i := 1 to Length(SpaceBuffer) do
                    if SpaceBuffer[i] = #9 then
                      CommandProcessor(ecTab, #0, nil)
                    else
                      CommandProcessor(ecChar, SpaceBuffer[i], nil);
                end;
                CaretY := CaretY + 1;
              end;
              fUndoList.AddChange(crLineBreak, Caret, Caret, '', smNormal);
            end;
          end
          else begin
            if fLines.Count = 0 then
              fLines.Add('');
            SpaceCount2 := 0;
            if eoAutoIndent in Options then
            begin
              BackCounter := CaretY - 1;
              while BackCounter >= 0 do
              begin
                SpaceCount2 := LeftSpacesEx(Lines[BackCounter],True);
                if Length(Lines[BackCounter]) > 0 then break;
                dec(BackCounter);
              end;
            end;
            Lines.Insert(CaretY - 1, '');
            fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
            if Command = ecLineBreak then
              InternalCaretX := SpaceCount2 + 1;
            if Command = ecLineBreak then
              InternalCaretY := CaretY + 1;
          end;
          DoLinesInserted(CaretY - InsDelta, 1);
          BlockBegin := CaretXY;
          BlockEnd   := CaretXY;
          EnsureCursorPosVisible;
          UpdateLastCaretX;
          finally
            UndoList.EndBlock;
          end;
        end;
      ecTab:
        if not ReadOnly then DoTabKey;
      ecShiftTab:
        if not ReadOnly then DoShiftTabKey;
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
      // #127 is Ctrl + Backspace, #32 is space
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then
        begin
          if SelAvail then
          begin
            BeginUndoBlock;
            try
              Helper := SelText;
              iUndoBegin := fBlockBegin;
              iUndoEnd := fBlockEnd;
              StartOfBlock := BlockBegin;
              if fActiveSelectionMode = smLine then
                StartOfBlock.Char := 1;
              fUndoList.AddChange(crDelete, iUndoBegin, iUndoEnd, Helper,
                fActiveSelectionMode);
              SetSelTextPrimitive(AChar);
              if fActiveSelectionMode <> smColumn then
              begin
                fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '',
                  smNormal);
              end;
            finally
              EndUndoBlock;
            end;
          end
          else
          begin
            SpaceCount2 := 0;
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
            begin
              if (Len > 0) then
                SpaceBuffer := WideStringOfChar(#32, CaretX - Len - Ord(fInserting))
              else
                SpaceBuffer := GetLeftSpacing(CaretX - Len - Ord(fInserting), True);
              SpaceCount2 := Length(SpaceBuffer);

              Temp := Temp + SpaceBuffer;
            end;
            // Added the check for whether or not we're in insert mode.
            // If we are, we append one less space than we would in overwrite mode.
            // This is because in overwrite mode we have to put in a final space
            // character which will be overwritten with the typed character.  If we put the
            // extra space in in insert mode, it would be left at the end of the line and
            // cause problems unless eoTrimTrailingSpaces is set.
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;

              if fInserting then
              begin
                if not WordWrap and not (eoAutoSizeMaxScrollWidth in Options)
                   and (CaretX > MaxScrollWidth) then
                begin
                  Exit;
                end;
                Insert(AChar, Temp, CaretX);
                if (eoTrimTrailingSpaces in Options) and ((AChar = #9) or (AChar = #32)) and (Length(TrimTrailingSpaces(LineText)) = 0) then
                  InternalCaretX := GetExpandedLength(Temp, TabWidth) + 1
                else
                begin
                  if Len = 0 then
                    InternalCaretX := Length(Temp) + 1
                  else
                    InternalCaretX := CaretX + 1;
                end;
                ProperSetLine(CaretY - 1, Temp);
                if SpaceCount2 > 0 then
                begin
                  BeginUndoBlock;
                  try
                    //if we inserted spaces with this char, we need to account for those
                    //in the X Position
                    StartOfBlock.Char := StartOfBlock.Char - SpaceCount2;
                    EndOfBlock := CaretXY;
                    EndOfBlock.Char := EndOfBlock.Char - 1;
                    //The added whitespace
                    fUndoList.AddChange(crWhiteSpaceAdd, EndOfBlock, StartOfBlock, '',
                      smNormal);
                    StartOfBlock.Char := StartOfBlock.Char + SpaceCount2;

                    fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                      smNormal);
                  finally
                    EndUndoBlock;
                  end;
                end
                else begin
                  fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, '',
                    smNormal);
                end;
              end
              else begin
// Processing of case character covers on LeadByte.
                counter := 1;
                Helper := Copy(Temp, CaretX, counter);
                Temp[CaretX] := AChar;
                CaretNew.Char := CaretX + counter;
                CaretNew.Line := CaretY;
                ProperSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crInsert, StartOfBlock, CaretNew, Helper,
                  smNormal);
                InternalCaretX := CaretX + 1;
              end;
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + Min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
          DoOnPaintTransient(ttAfter);
        end;
      ecUpperCase,
      ecLowerCase,
      ecToggleCase,
      ecTitleCase,
      ecUpperCaseBlock,
      ecLowerCaseBlock,
      ecToggleCaseBlock:
        if not ReadOnly then DoCaseChange(Command);
      ecUndo:
        begin
          if not ReadOnly then Undo;
        end;
      ecRedo:
        begin
          if not ReadOnly then Redo;
        end;
      ecGotoMarker0..ecGotoMarker9:
        begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0..ecSetMarker9:
        begin
          if BookMarkOptions.EnableKeys then
          begin
            CX := Command - ecSetMarker0;
            if Assigned(Data) then
              Caret := TBufferCoord(Data^)
            else
              Caret := CaretXY;
            if assigned(fBookMarks[CX]) then
            begin
              moveBkm := (fBookMarks[CX].Line <> Caret.Line);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, Caret.Char, Caret.Line);
            end
            else
              SetBookMark(CX, Caret.Char, Caret.Line);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut:
        begin
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if not ReadOnly then PasteFromClipboard;
        end;
      ecScrollUp, ecScrollDown:
        begin
          vCaretRow := DisplayY;
          if (vCaretRow < TopLine) or (vCaretRow >= TopLine + LinesInWindow) then
            // If the caret is not in view then, like the Delphi editor, move
            // it in view and do nothing else
            EnsureCursorPosVisible
          else begin
            if Command = ecScrollUp then
            begin
              TopLine := TopLine - 1;
              if vCaretRow > TopLine + LinesInWindow - 1 then
                MoveCaretVert((TopLine + LinesInWindow - 1) - vCaretRow, False);
            end
            else begin
              TopLine := TopLine + 1;
              if vCaretRow < TopLine then
                MoveCaretVert(TopLine - vCaretRow, False);
            end;
            EnsureCursorPosVisible;
            Repaint;
          end;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX > LeftChar + CharsInWindow then
          //  InternalCaretX := LeftChar + CharsInWindow;
          Repaint;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX < LeftChar then
          //  InternalCaretX := LeftChar;
          Repaint;
        end;
      ecInsertMode:
        begin
          InsertMode := True;
        end;
      ecOverwriteMode:
        begin
          InsertMode := False;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;
      ecNormalSelect:
        SelectionMode := smNormal;
      ecColumnSelect:
        SelectionMode := smColumn;
      ecLineSelect:
        SelectionMode := smLine;
      ecContextHelp:
        begin
          if Assigned (fOnContextHelp) then
            fOnContextHelp (self,WordAtCursor);
        end;
      ecImeStr:
        if not ReadOnly then
        begin
          SetString(S, PWideChar(Data), WStrLen(Data));
          if SelAvail then
          begin
            BeginUndoBlock;
            try
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, Helper,
                smNormal);
              StartOfBlock := fBlockBegin;
              SetSelTextPrimitive(s);
              fUndoList.AddChange(crInsert, fBlockBegin, fBlockEnd, Helper,
                smNormal);
            finally
              EndUndoBlock;
            end;
            InvalidateGutterLines(-1, -1);
          end
          else
          begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + WideStringOfChar(#32, CaretX - Len - 1);
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              Len := Length(s);
              if not fInserting then
              begin
                Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              InternalCaretX := (CaretX + Len);
              ProperSetLine(CaretY - 1, Temp);
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, Helper,
                smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
        end;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TvgCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: WideChar; Data: pointer);
begin
  if Command < ecUserFirst then
  begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end
  else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TvgCustomSynEdit.ClearAll;
begin
  Lines.Clear;
  fMarkList.Clear; // fMarkList.Clear also frees all bookmarks,
  FillChar(fBookMarks, sizeof(fBookMarks), 0); // so fBookMarks should be cleared too
  fUndoList.Clear;
  fRedoList.Clear;
  Modified := False;
end;

procedure TvgCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TvgCustomSynEdit.NextWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, LineLen: Integer;
  Line: WideString;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    LineLen := Length(Line);
    if CX >= LineLen then
    begin
      // find first IdentChar or multibyte char in the next line
      if CY < Lines.Count then
      begin
        Line := Lines[CY];
        Inc(CY);
        CX := StrScanForCharInCategory(Line, 1, IsIdentChar);
        if CX = 0 then
          Inc(CX);
      end;
    end
    else
    begin
      // find next word-break-char if current char is an IdentChar
      if IsIdentChar(Line[CX]) then
        CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
      // if word-break-char found, find the next IdentChar
      if CX > 0 then
        CX := StrScanForCharInCategory(Line, CX, IsIdentChar);
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TvgCustomSynEdit.WordStartEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: WideString;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX > 1 then
    begin  // only find previous char, if not already on start of line
      // if previous char isn't a word-break-char search for the last IdentChar
      if not IsWordBreakChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, IsWordBreakChar) + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TvgCustomSynEdit.WordEndEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: WideString;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
    // if no word-break-char is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TvgCustomSynEdit.PrevWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: WideString;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX <= 1 then
    begin
      // find last IdentChar in the previous line
      if CY > 1 then
      begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end
    else
    begin
      // if previous char is a word-break-char search for the last IdentChar
      if IsWordBreakChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, IsIdentChar);
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInCategory(Line, CX - 1, IsWordBreakChar) + 1;
      if CX = 0 then
      begin
        // else just position at the end of the previous line
        if CY > 1 then
        begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end
        else
          CX := 1;
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure TvgCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if FSelectionMode <> Value then
  begin
    fSelectionMode := Value;
    ActiveSelectionMode := Value;
  end;
end;

procedure TvgCustomSynEdit.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if fActiveSelectionMode <> Value then
  begin
    if SelAvail then
      InvalidateSelection;
    fActiveSelectionMode := Value;
    if SelAvail then
      InvalidateSelection;
    StatusChanged([scSelection]);
  end;
end;

procedure TvgCustomSynEdit.SetAdditionalIdentChars(const Value: TSysCharSet);
begin
  FAdditionalIdentChars := Value;
end;

procedure TvgCustomSynEdit.SetAdditionalWordBreakChars(const Value: TSysCharSet);
begin
  FAdditionalWordBreakChars := Value;
end;

procedure TvgCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
end;

procedure TvgCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TvgCustomSynEdit.EndUndoBlock;
begin
  fUndoList.EndBlock;
end;

procedure TvgCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TvgCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

{ Called by FMarkList if change }
procedure TvgCustomSynEdit.MarkListChange(Sender: TObject);
begin
  InvalidateGutter;
end;

function TvgCustomSynEdit.GetSelStart: integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(BlockBegin)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TvgCustomSynEdit.SetAlwaysShowCaret(const Value: Boolean);
begin
  if FAlwaysShowCaret <> Value then
  begin
    FAlwaysShowCaret := Value;
    if not(csDestroying in ComponentState) and not (IsFocused) then
    begin
      if Value then
      begin
        InitializeCaret;
      end
      else
      begin
        HideCaret;
      end;
    end;
  end;
end;

procedure TvgCustomSynEdit.SetSelStart(const Value: Integer);
begin
  { if we don't call HandleNeeded, CharsInWindow may be 0 and LeftChar will
  be set to CaretX }
  InternalCaretXY := CharIndexToRowCol(Value);
  BlockBegin := CaretXY;
end;

function TvgCustomSynEdit.GetSelEnd: Integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(Blockend)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TvgCustomSynEdit.SetSelEnd(const Value: Integer);
begin
  BlockEnd := CharIndexToRowCol( Value );
end;

procedure TvgCustomSynEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TvgCustomSynEdit.SetExtraLineSpacing(const Value: Integer);
begin
  fExtraLineSpacing := Value;
  SynFontChanged(self);
end;

function TvgCustomSynEdit.GetBookMark(BookMark: Integer; var X, Y: Integer):
  Boolean;
var
  i: Integer;
begin
  Result := False;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then
      begin
        X := Marks[i].Char;
        Y := Marks[i].Line;
        Result := True;
        Exit;
      end;
end;

function TvgCustomSynEdit.IsBookmark(BookMark: Integer): Boolean;
var
  x, y: Integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TvgCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TvgCustomSynEdit.SetSelTextExternal(const Value: WideString);
var
  StartOfBlock, EndOfBlock: TBufferCoord;
begin
  BeginUndoBlock;
  try
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,
        SelText, fActiveSelectionMode);
    end
    else
      ActiveSelectionMode := SelectionMode;
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    SetSelTextPrimitive(Value);
    if (Value <> '') and (fActiveSelectionMode <> smColumn) then
      fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd, '', fActiveSelectionMode);
  finally
    EndUndoBlock;
  end;
end;

procedure TvgCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TvgCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if fGutter.UseFontStyle then
    begin
//      fTextDrawer.SetBaseFont(fGutter.Font);
      nW := fGutter.RealGutterWidth(trunc(fCharWidth));
//      fTextDrawer.SetBaseFont(Font);}
    end
    else
      nW := fGutter.RealGutterWidth(trunc(fCharWidth));
    if nW = fGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;
end;

procedure TvgCustomSynEdit.LockUndo;
begin
  fUndoList.Lock;
  fRedoList.Lock;
end;

procedure TvgCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

procedure TvgCustomSynEdit.SetTabWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, 256);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    TSynEditStringList(Lines).TabWidth := Value;
    Repaint; // to redraw text containing tab chars
    if WordWrap then
    begin
      fWordWrapPlugin.Reset;
      InvalidateGutter;
    end;
  end;
end;

procedure TvgCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  InvalidateSelection;
end;

// find / replace

function TvgCustomSynEdit.SearchReplace(const ASearch, AReplace: WideString;
  AOptions: TSynSearchOptions): Integer;
var
  ptStart, ptEnd: TBufferCoord; // start and end of the search range
  ptCurrent: TBufferCoord; // current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;
  nAction: TSynReplaceAction;
  iResultOffset: Integer;

  function InValidSearchRange(First, Last: Integer): Boolean;
  begin
    Result := True;
    if (fActiveSelectionMode = smNormal) or not (ssoSelectedOnly in AOptions) then
    begin
      if ((ptCurrent.Line = ptStart.Line) and (First < ptStart.Char)) or
        ((ptCurrent.Line = ptEnd.Line) and (Last > ptEnd.Char))
      then
        Result := False;
    end
    else
    if (fActiveSelectionMode = smColumn) then
      // solves bug in search/replace when smColumn mode active and no selection
      Result := (First >= ptStart.Char) and (Last <= ptEnd.Char) or (ptEnd.Char - ptStart.Char < 1);
  end;

begin
  if not Assigned(fSearchEngine) then
    raise ESynEditError.Create('No search engine has been assigned');

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not (ssoEntireScope in AOptions);
  if not SelAvail then Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (fActiveSelectionMode = smLine) then
    begin
      ptStart.Char := 1;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    end
    else if (fActiveSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.Char > ptEnd.Char) then
        SwapInt(Integer(ptStart.Char), Integer(ptEnd.Char));
    // ignore the cursor position when searching in the selection
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end
  else
  begin
    ptStart.Char := 1;
    ptStart.Line := 1;
    ptEnd.Line := Lines.Count;
    ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    if bFromCursor then
      if bBackward then ptEnd := CaretXY else ptStart := CaretXY;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  // initialize the search engine
  fSearchEngine.Options := AOptions;
  fSearchEngine.Pattern := ASearch;
  // search while the current search position is inside of the search range
  nReplaceLen := 0;
  DoOnPaintTransient(ttBefore);
  if bReplaceAll and not bPrompt then
  begin
    IncPaintLock;
    BeginUndoBlock;
    bEndUndoBlock := True;
  end
  else
    bEndUndoBlock := False;
  try
    while (ptCurrent.Line >= ptStart.Line) and (ptCurrent.Line <= ptEnd.Line) do
    begin
      nInLine := fSearchEngine.FindAll(Lines[ptCurrent.Line - 1]);
      iResultOffset := 0;
      if bBackward then
        n := Pred(fSearchEngine.ResultCount)
      else
        n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nFound := fSearchEngine.Results[n] + iResultOffset;
        nSearchLen := fSearchEngine.Lengths[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.

        ptCurrent.Char := nFound;
        BlockBegin := ptCurrent;
        // Be sure to use the Ex version of CursorPos so that it appears in the middle if necessary
        SetCaretXYEx(False, BufferCoord(1, ptCurrent.Line));
        EnsureCursorPosVisibleEx(True);
        Inc(ptCurrent.Char, nSearchLen);
        BlockEnd := ptCurrent;
        InternalCaretXY := ptCurrent;
        if bBackward then InternalCaretXY := BlockBegin else InternalCaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then
        begin
          nAction := DoOnReplaceText(ASearch, AReplace, ptCurrent.Line, nFound);
          if nAction = raCancel then
            exit;
        end
        else
          nAction := raReplace;
        if nAction <> raSkip then
        begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll or bPrompt then
            begin
              bReplaceAll := True;
              IncPaintLock;
            end;
            bPrompt := False;
            if bEndUndoBlock = false then
              BeginUndoBlock;
            bEndUndoBlock:= true;
          end;
          // Allow advanced substition in the search engine
          SelText := fSearchEngine.Replace(SelText, AReplace);
          nReplaceLen := CaretX - nFound;
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          InternalCaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
            if (fActiveSelectionMode <> smColumn) and (CaretY = ptEnd.Line) then
            begin
              Inc(ptEnd.Char, nReplaceLen - nSearchLen);
              BlockEnd := ptEnd;
            end;
          end;
        end;
        if not bReplaceAll then
          exit;
      end;
      // search next / previous line
      if bBackward then
        Dec(ptCurrent.Line)
      else
        Inc(ptCurrent.Line);
    end;
  finally
    if bReplaceAll and not bPrompt then DecPaintLock;
    if bEndUndoBlock then EndUndoBlock;
    DoOnPaintTransient( ttAfter );
  end;
end;

function TvgCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): boolean;
var
  ptBegin, ptEnd: TBufferCoord;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Line >= ptBegin.Line) and (Value.Line <= ptEnd.Line) and
    ((ptBegin.Line <> ptEnd.Line) or (ptBegin.Char <> ptEnd.Char)) then
  begin
    if fActiveSelectionMode = smLine then
      Result := True
    else if (fActiveSelectionMode = smColumn) then
    begin
      if (ptBegin.Char > ptEnd.Char) then
        Result := (Value.Char >= ptEnd.Char) and (Value.Char < ptBegin.Char)
      else if (ptBegin.Char < ptEnd.Char) then
        Result := (Value.Char >= ptBegin.Char) and (Value.Char < ptEnd.Char)
      else
        Result := False;
    end
    else
      Result := ((Value.Line > ptBegin.Line) or (Value.Char >= ptBegin.Char)) and
        ((Value.Line < ptEnd.Line) or (Value.Char < ptEnd.Char));
  end
  else
    Result := False;
end;

procedure TvgCustomSynEdit.SetFocus;
begin
  if (fFocusList.Count > 0) then
  begin
    TWinControl (fFocusList.Last).SetFocus;
    exit;
  end;
  inherited;
end;

procedure TvgCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

function TvgCustomSynEdit.GetOptions: TSynEditorOptions;
begin
  Result := fOptions;
end;

procedure TvgCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoDisableScrollArrows,eoHideShowScrollbars,
    eoScrollPastEof,eoScrollPastEol];
var
{$IFNDEF SYN_CLX}
  bSetDrag: Boolean;
{$ENDIF}
  TmpBool: Boolean;
  bUpdateScroll: Boolean;
  vTempBlockBegin, vTempBlockEnd : TBufferCoord;
begin
  if (Value <> fOptions) then
  begin
{$IFNDEF SYN_CLX}
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
{$ENDIF}

    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;

    bUpdateScroll := (Options * ScrollOptions) <> (Value * ScrollOptions);

    fOptions := Value;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    InternalCaretXY := CaretXY;
    if (eoScrollPastEol in Options) then
    begin
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;

    TmpBool := eoShowSpecialChars in Value;
    if TmpBool <> fShowSpecChar then
    begin
      fShowSpecChar := TmpBool;
      Repaint;
    end;
    if bUpdateScroll then
      UpdateScrollBars;
  end;
end;

procedure TvgCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if (fCharWidth <> 0) then
  begin
    fCharsInWindow := trunc(Max(ClientWidth - fGutterWidth - 2, 0) / fCharWidth);
    fLinesInWindow := trunc(ClientHeight / fTextHeight);
    if WordWrap then
    begin
      fWordWrapPlugin.DisplayChanged;
      Repaint;
    end;
    if bFont then
    begin
      if Gutter.ShowLineNumbers then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(fStateFlags, sfCaretChanged);
      Repaint;
    end
    else
      UpdateScrollbars;
    Exclude(fStateFlags, sfScrollbarChanged);
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TvgCustomSynEdit.MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst: TBufferCoord;
  s: WideString;
  nLineLen: Integer;
  bChangeY: Boolean;
  vCaretRowCol: TDisplayCoord;
begin
  if WordWrap then
  begin
    if DX > 0 then
    begin
      if fCaretAtEOL then
      begin
        fCaretAtEOL := False;
        UpdateLastCaretX;
        IncPaintLock;
        Include(fStateFlags, sfCaretChanged);
        DecPaintLock;
        Exit;
      end;
    end
    else
    begin // DX < 0. Handle ecLeft/ecPageLeft at BOL.
      if (not fCaretAtEOL) and (CaretX > 1) and (DisplayX = 1) then
      begin
        fCaretAtEOL := True;
        UpdateLastCaretX;
        if DisplayX > CharsInWindow +1 then
          SetInternalDisplayXY( DisplayCoord(CharsInWindow +1, DisplayY) )
        else begin
          IncPaintLock;
          Include(fStateFlags, sfCaretChanged);
          DecPaintLock;
        end;
        Exit;
      end;
    end;
  end;
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (eoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.Char = 1) and (ptO.Line > 1) then
  begin
    // end of previous line
    Dec(ptDst.Line);
    ptDst.Char := Length(Lines[ptDst.Line - 1]) + 1;
  end
  else if bChangeY and (DX = 1) and (ptO.Char > nLineLen) and (ptO.Line < Lines.Count) then
  begin
    // start of next line
    Inc(ptDst.Line);
    ptDst.Char := 1;
  end
  else begin
    ptDst.Char := Max(1, ptDst.Char + DX);
    // don't go past last char when ScrollPastEol option not set
    if (DX > 0) and bChangeY then
      ptDst.Char := Min(ptDst.Char, nLineLen + 1);
  end;
  // set caret and block begin / end
  MoveCaretAndSelection(fBlockBegin, ptDst, SelectionCommand);
  // if caret is beyond CharsInWindow move to next row (this means there are
  // spaces/tabs at the end of the row)
  if WordWrap and (DX > 0) and (CaretX < Length(LineText)) then
  begin
    vCaretRowCol := DisplayXY;
    if (vCaretRowCol.Column = 1) and (LineToRow(CaretY) <> vCaretRowCol.Row) then
    begin
      fCaretAtEOL := True;
      UpdateLastCaretX;
    end
    else if vCaretRowCol.Column > CharsInWindow +1 then
    begin
      Inc(vCaretRowCol.Row);
      vCaretRowCol.Column := 1;
      InternalCaretXY := DisplayToBufferPos(vCaretRowCol);
    end;
  end;
end;

procedure TvgCustomSynEdit.MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst, vEOLTestPos: TDisplayCoord;
  vDstLineChar: TBufferCoord;
  SaveLastCaretX: Integer;
begin
  ptO := DisplayXY;
  ptDst := ptO;

  Inc(ptDst.Row, DY);
  if DY >= 0 then
  begin
    if RowToLine(ptDst.Row) > Lines.Count then
      ptDst.Row := Max(1, DisplayLineCount);
  end
  else begin
    if ptDst.Row < 1 then
      ptDst.Row := 1;
  end;

  if (ptO.Row <> ptDst.Row) then
  begin
    if eoKeepCaretX in Options then
      ptDst.Column := fLastCaretX;
  end;
  vDstLineChar := DisplayToBufferPos(ptDst);
  SaveLastCaretX := fLastCaretX;

  // set caret and block begin / end
  IncPaintLock;
  MoveCaretAndSelection(fBlockBegin, vDstLineChar, SelectionCommand);
  if WordWrap then
  begin
    vEOLTestPos := BufferToDisplayPos(vDstLineChar);
    fCaretAtEOL := (vEOLTestPos.Column = 1) and (vEOLTestPos.Row <> ptDst.Row);
  end;
  DecPaintLock;

  // Restore fLastCaretX after moving caret, since UpdateLastCaretX, called by
  // SetCaretXYEx, changes them. This is the one case where we don't want that.
  fLastCaretX := SaveLastCaretX;
end;

procedure TvgCustomSynEdit.MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
  SelectionCommand: Boolean);
begin
  if (eoGroupUndo in FOptions) and UndoList.CanUndo then
    fUndoList.AddGroupBreak;

  IncPaintLock;
  if SelectionCommand then
  begin
    if not SelAvail then
      SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end
  else
    SetBlockBegin(ptAfter);
  InternalCaretXY := ptAfter;
  DecPaintLock;
end;

procedure TvgCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TBufferCoord);
var
  vOldMode: TSynSelectionMode;
begin
  vOldMode := fActiveSelectionMode;
  IncPaintLock;
  try
    InternalCaretXY := ptCaret;
    SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  finally
    ActiveSelectionMode := vOldMode;
    DecPaintLock;
  end;
end;

procedure TvgCustomSynEdit.RecalcCharExtent;
{const
  iFontStyles: array[0..3] of TvgFontStyle = ([], vgFontItalic, vgFontBold,
    [fsItalic, fsBold]);
var
  iHasStyle: array[0..3] of Boolean;
  cAttr: Integer;
  cStyle: Integer;
  iCurr: TvgFontStyle;
begin
  FillChar(iHasStyle, SizeOf(iHasStyle), 0);
  if Assigned(fHighlighter) and (fHighlighter.AttrCount > 0) then begin
    for cAttr := 0 to fHighlighter.AttrCount - 1 do
    begin
      iCurr := fHighlighter.Attribute[cAttr].Style * [fsItalic, fsBold];
      for cStyle := 0 to 3 do
        if iCurr = iFontStyles[cStyle] then
        begin
          iHasStyle[cStyle] := True;
          break;
        end;
    end;
  end
  else begin
    iCurr := Font.Style * [fsItalic, fsBold];
    for cStyle := 0 to 3 do
      if iCurr = iFontStyles[cStyle] then
      begin
        iHasStyle[cStyle] := True;
        break;
      end;
  end;

  fTextHeight := 0;
  fCharWidth := 0;
  fTextDrawer.BaseFont := Self.Font;
  for cStyle := 0 to 3 do
    if iHasStyle[cStyle] then
    begin
      fTextDrawer.BaseStyle := iFontStyles[cStyle];
      fTextHeight := Max(fTextHeight, fTextDrawer.CharHeight);
      fCharWidth := Max(fCharWidth, fTextDrawer.CharWidth);
    end;
  Inc(fTextHeight, fExtraLineSpacing);}
var
  B: TvgBitmap;
begin
  fTextHeight := Round(FFont.Size * 1.25);
  if Canvas = nil then
  begin
    B := TvgBitmap.Create(1, 1);
    B.Canvas.Font.Assign(FFont);
    fCharWidth := B.Canvas.TextWidth('a');
    B.Free;
  end
  else
    fCharWidth := Canvas.TextWidth('a');
end;

procedure TvgCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  if Sender is TSynCustomHighlighter then
  begin
    Lines.BeginUpdate;
    try
      ScanRanges;
    finally
      Lines.EndUpdate;
    end;
  end
  else
    Repaint;
  SizeOrFontChanged(True);
end;

procedure TvgCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TvgCustomSynEdit.DoCaseChange(const Cmd: TSynEditorCommand);

  function ToggleCase(const aStr: WideString): WideString;
  var
    i: Integer;
    sLower: WideString;
  begin
    Result := WideUpperCase(aStr);
    sLower := WideLowerCase(aStr);
    for i := 1 to Length(aStr) do
    begin
      if Result[i] = aStr[i] then
        Result[i] := sLower[i];
    end;
  end;

var
  w: WideString;
  oldCaret, oldBlockBegin, oldBlockEnd: TBufferCoord;
  bHadSel : Boolean;
begin
  Assert((Cmd >= ecUpperCase) and (Cmd <= ecToggleCaseBlock));
  if SelAvail then
  begin
    bHadSel := True;
    oldBlockBegin := BlockBegin;
    oldBlockEnd := BlockEnd;
  end
  else begin
    bHadSel := False;
  end;
  oldCaret := CaretXY;
  try
    if Cmd < ecUpperCaseBlock then
    begin
      { word commands }
      SetSelWord;
      if SelText = '' then
      begin
        { searches a previous word }
        InternalCaretXY := PrevWordPos;
        SetSelWord;
        if SelText = '' then
        begin
          { try once more since PrevWordPos may have failed last time.
          (PrevWordPos "points" to the end of the previous line instead of the
          beggining of the previous word if invoked (e.g.) when CaretX = 1) }
          InternalCaretXY := PrevWordPos;
          SetSelWord;
        end;
      end;
    end
    else begin
      { block commands }
      if not SelAvail then
      begin
        if CaretX <= Length(LineText) then
          MoveCaretHorz(1, True)
        else if CaretY < Lines.Count then
          InternalCaretXY := BufferCoord(1, CaretY +1);
      end;
    end;

    w := SelText;
    if w <> '' then
    begin
      case Cmd of
        ecUpperCase, ecUpperCaseBlock:
          w := WideUpperCase(w);
        ecLowerCase, ecLowerCaseBlock:
          w := WideLowerCase(w);
        ecToggleCase, ecToggleCaseBlock:
          w := ToggleCase(w);
        ecTitleCase:
          w := WideUpperCase(w[1]) + WideLowerCase(Copy(w, 2, Length(w)));
      end;
      BeginUndoBlock;
      try
        if bHadSel then
          fUndoList.AddChange(crSelection, oldBlockBegin, oldBlockEnd, '', fActiveSelectionMode)
        else
          fUndoList.AddChange(crSelection, oldCaret, oldCaret, '', fActiveSelectionMode);
        fUndoList.AddChange(crCaret, oldCaret, oldCaret, '', fActiveSelectionMode);
        SelText := w;
      finally
        EndUndoBlock;
      end;
    end;
  finally
    { "word" commands do not restore Selection }
    if bHadSel and (Cmd >= ecUpperCaseBlock) then
    begin
      BlockBegin := oldBlockBegin;
      BlockEnd := oldBlockEnd;
    end;
    { "block" commands with empty Selection move the Caret }
    if bHadSel or (Cmd < ecUpperCaseBlock) then
      CaretXY := oldCaret;
  end;
end;

procedure TvgCustomSynEdit.DoTabKey;
var
  StartOfBlock: TBufferCoord;
  i, MinLen, iLine: integer;
  PrevLine, Spaces: WideString;
  p: PWideChar;
  NewCaretX: integer;
  ChangeScroll: Boolean;
  nPhysX, nDistanceToTab, nSpacesToNextTabStop : Integer;
  OldSelTabLine, vIgnoreSmartTabs: Boolean;
begin
  // Provide Visual Studio like block indenting
  OldSelTabLine := SelTabLine;
  if (eoTabIndent in Options) and ((SelTabBlock) or (OldSelTabLine)) then
  begin
    DoBlockIndent;
    if OldSelTabLine then
    begin
      if fBlockBegin.Char < fBlockEnd.Char then
        FBlockBegin.Char := 1
      else
        fBlockEnd.Char := 1;
    end;
    exit;
  end;
  i := 0;
  iLine := 0;
  MinLen := 0;
  vIgnoreSmartTabs := False;
  if eoSmartTabs in fOptions then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      Dec(iLine);
      repeat
        //todo: rethink it
        MinLen := DisplayToBufferPos(DisplayCoord(
          BufferToDisplayPos(CaretXY).Column, LineToRow(iLine + 1))).Char;
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if (p^ = #9) or (p^ = #32) then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if (p^ <> #9) and (p^ <> #32) then break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end
    else
      vIgnoreSmartTabs := True;
  end;
  fUndoList.BeginBlock;
  try
    if SelAvail then
    begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        fActiveSelectionMode);
      SetSelTextPrimitive('');
    end;
    StartOfBlock := CaretXY;

    if i = 0 then
    begin
      if (eoTabsToSpaces in fOptions) then
      begin
        i := TabWidth - (StartOfBlock.Char - 1) mod TabWidth;
        if i = 0 then
          i := TabWidth;
      end
      else
        i := TabWidth;
    end;

    if eoTabsToSpaces in fOptions then
    begin
      Spaces := WideStringOfChar(#32, i);
      NewCaretX := StartOfBlock.Char + i;
    end
    else if (eoTrimTrailingSpaces in Options) and (StartOfBlock.Char > Length(LineText)) then
    begin
      // work-around for trimming Tabs
      nPhysX := BufferToDisplayPos(CaretXY).Column;
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        i := BufferToDisplayPos( BufferCoord(MinLen+i, iLine+1) ).Column;
        nDistanceToTab := i - nPhysX;
      end
      else
        nDistanceToTab := TabWidth - ((nPhysX - 1) mod TabWidth);
      NewCaretX := StartOfBlock.Char + nDistanceToTab;
    end
    else begin
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        Spaces := Copy(fLines[CaretXY.Line - 1], 1, CaretXY.Char - 1);
        while Pos(#9, Spaces) > 0 do
          Delete(Spaces, Pos(#9, Spaces), 1);
        Spaces := WideTrim(Spaces);

        //smart tabs are only in the front of the line *NOT IN THE MIDDLE*
        if Spaces = '' then
        begin
          i := BufferToDisplayPos( BufferCoord(MinLen+i, iLine+1) ).Column;

          nPhysX := DisplayX;
          nDistanceToTab := i - nPhysX;
          nSpacesToNextTabStop := TabWidth - ((nPhysX - 1) mod TabWidth);
          if nSpacesToNextTabStop <= nDistanceToTab then begin
            Spaces := #9;
            Dec(nDistanceToTab, nSpacesToNextTabStop);
          end;
          while nDistanceToTab >= TabWidth do begin
            Spaces := Spaces + #9;
            Dec(nDistanceToTab, TabWidth);
          end;
          if nDistanceToTab > 0 then
            Spaces := Spaces + WideStringOfChar(#32, nDistanceToTab);
        end else
          Spaces := #9;
      end
      else begin
        Spaces := #9;
      end;
      if (eoTrimTrailingSpaces in Options) and (Length(TrimTrailingSpaces(LineText)) = 0) then
        NewCaretX := StartOfBlock.Char + GetExpandedLength(Spaces, TabWidth)
      else
        NewCaretX := StartOfBlock.Char + Length(Spaces);
    end;

    SetSelTextPrimitive(Spaces);
    // Undo is already handled in SetSelText when SelectionMode is Column
    if fActiveSelectionMode <> smColumn then
    begin
      fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, SelText,
        fActiveSelectionMode);
    end;
  finally
    fUndoList.EndBlock;
  end;

  ChangeScroll := not(eoScrollPastEol in fOptions);
  try
    Include(fOptions, eoScrollPastEol);
    InternalCaretX := NewCaretX;
  finally
    if ChangeScroll then
      Exclude(fOptions, eoScrollPastEol);
  end;

  EnsureCursorPosVisible;
end;

procedure TvgCustomSynEdit.DoShiftTabKey;
// shift-tab key handling
var
  NewX: Integer;
  Line: WideString;
  LineLen: Integer;
  DestX: Integer;

  MaxLen, iLine: Integer;
  PrevLine, OldSelText: WideString;
  p: PWideChar;
  OldCaretXY: TBufferCoord;
begin
  // Provide Visual Studio like block indenting
  if (eoTabIndent in Options) and ((SelTabBlock) or (SelTabLine)) then
  begin
    DoBlockUnIndent;
    exit;
  end;

  NewX := CaretX;

  if (NewX <> 1) and (eoSmartTabs in fOptions) then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      Dec(iLine);
      MaxLen := CaretX - 1;
      repeat
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MaxLen) then
        begin
          p := @PrevLine[MaxLen];
          // scan over whitespaces
          repeat
            if p^ <> #32 then break;
            Dec(NewX);
            Dec(p);
          until NewX = 1;
          // scan over non-whitespaces
          if NewX <> 1 then
            repeat
              if p^ = #32 then break;
              Dec(NewX);
              Dec(p);
            until NewX = 1;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end;
  end;

  if NewX = CaretX then
  begin
    Line := LineText;
    LineLen := Length(Line);

    // find real un-tab position

    DestX := ((CaretX - 2) div TabWidth) * TabWidth + 1;
    if NewX > LineLen then
      NewX := DestX
    else if (NewX > DestX) and (Line[NewX - 1] = #9) then
      dec(NewX)
    else begin
      while (NewX > DestX) and ((NewX - 1 > LineLen) or (Line[NewX - 1] = #32)) do
        dec(NewX);
    end;
  end;

  // perform un-tab
  if (NewX <> CaretX) then
  begin
    SetBlockBegin(BufferCoord(NewX, CaretY));
    SetBlockEnd(CaretXY);
    OldCaretXY := CaretXY;

    OldSelText := SelText;
    SetSelTextPrimitive('');

    fUndoList.AddChange(crSilentDelete, BufferCoord(NewX, CaretY),
      OldCaretXY, OldSelText, smNormal);

    InternalCaretX := NewX;
  end;
end;

procedure TvgCustomSynEdit.DoHomeKey(Selection: Boolean);

  function LastCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    if fLines.Count = 0 then
      Result := 1
    else
    begin
      vPos := DisplayXY;
      vPos.Column := Min(CharsInWindow, fWordWrapPlugin.GetRowLength(vPos.Row) + 1);
      Result := DisplayToBufferPos(vPos).Char;
    end;
  end;

var
  newX: Integer;
  first_nonblank: Integer;
  s: WideString;
  vNewPos: TDisplayCoord;
  vMaxX: Integer;
begin
  // home key enhancement
  if (eoEnhanceHomeKey in fOptions) and (LineToRow(CaretY) = DisplayY) then
  begin
    s := fLines[CaretXY.Line - 1];

    first_nonblank := 1;
    if WordWrap then
      vMaxX := LastCharInRow() -1
    else
      vMaxX := Length(s);
    while (first_nonblank <= vMaxX) and
      CharInSet(s[first_nonblank], [#32, #9])
    do
      inc(first_nonblank);
    dec(first_nonblank);

    newX := CaretXY.Char - 1;

    if (newX > first_nonblank) or (newX = 0) then
      newX := first_nonblank + 1
    else
      newX := 1;
  end
  else
    newX := 1;

  if WordWrap then
  begin
    vNewPos.Row := DisplayY;
    vNewPos.Column := BufferToDisplayPos(BufferCoord(newX, CaretY)).Column;
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewPos), Selection);
  end
  else
    MoveCaretAndSelection(CaretXY, BufferCoord(newX, CaretY), Selection);
end;

procedure TvgCustomSynEdit.DoEndKey(Selection: Boolean);

  function CaretInLastRow: Boolean;
  var
    vLastRow: Integer;
  begin
    if not WordWrap then
      Result := True
    else
    begin
      vLastRow := LineToRow(CaretY + 1) - 1;
      // This check allows good behaviour with empty rows (this can be useful in a diff app ;-)
      while (vLastRow > 1)
        and (fWordWrapPlugin.GetRowLength(vLastRow) = 0)
        and (RowToLine(vLastRow) = CaretY) do
      begin
        Dec(vLastRow);
      end;
      Result := DisplayY = vLastRow;
    end;
  end;

  function FirstCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    vPos.Row := DisplayY;
    vPos.Column := 1;
    Result := DisplayToBufferPos(vPos).Char;
  end;

var
  vText: WideString;
  vLastNonBlank: Integer;
  vNewX: Integer;
  vNewCaret: TDisplayCoord;
  vMinX: Integer;
  vEnhance: Boolean;
begin
  if (eoEnhanceEndKey in fOptions) and CaretInLastRow then
  begin
    vEnhance := True;
    vText := LineText;
    vLastNonBlank := Length(vText);
    if WordWrap then
      vMinX := FirstCharInRow() - 1
    else
      vMinX := 0;
    while (vLastNonBlank > vMinX) and CharInSet(vText[vLastNonBlank], [#32, #9]) do
      Dec(vLastNonBlank);

    vNewX := CaretX - 1;
    if vNewX = vLastNonBlank then
      vNewX := Length(LineText) + 1
    else
      vNewX := vLastNonBlank + 1;
  end
  else
  begin
    vNewX := Length(LineText) + 1;
    vEnhance := False;
  end;

  if WordWrap then
  begin
    vNewCaret.Row := DisplayY;
    if vEnhance then
      vNewCaret.Column := BufferToDisplayPos(BufferCoord(vNewX, CaretY)).Column
    else
      vNewCaret.Column := fWordWrapPlugin.GetRowLength(vNewCaret.Row) + 1;
    vNewCaret.Column := Min(CharsInWindow + 1, vNewCaret.Column);
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewCaret), Selection);
    // Updates fCaretAtEOL flag.
    SetInternalDisplayXY(vNewCaret);
  end
  else
    MoveCaretAndSelection(CaretXY,
      BufferCoord(vNewX, CaretY), Selection);
end;

procedure TvgCustomSynEdit.InvalidateRect(aRect: TvgRect; aErase: Boolean);
begin
  vgInflateRect(aRect, 1, 1);
  with aRect do
    aRect := vgNormalizeRect([LocaltoAbsolute(vgPoint(Left, Top)), LocaltoAbsolute(vgPoint(Right, Top)),
      LocaltoAbsolute(vgPoint(Right, Bottom)), LocaltoAbsolute(vgPoint(Left, Bottom))]);
  Scene.AddUpdateRect(aRect);
end;

procedure TvgCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos: TBufferCoord;
  BB, BE: TBufferCoord;
  Run, StrToInsert: PWideChar;
  e, x, i, InsertStrLen: Integer;
  Spaces: WideString;
  OrgSelectionMode: TSynSelectionMode;
  InsertionPos: TBufferCoord;
begin
  OrgSelectionMode := fActiveSelectionMode;
  OrgCaretPos := CaretXY;

  StrToInsert := nil;
  if SelAvail then
  try
    // keep current selection detail
    BB := BlockBegin;
    BE := BlockEnd;

    // build text to insert
    if (BE.Char = 1) then
    begin
      e := BE.Line - 1;
      x := 1;
    end
    else
    begin
      e := BE.Line;
      if eoTabsToSpaces in Options then
        x := CaretX + FTabWidth
      else x := CaretX + 1;
    end;
    if (eoTabsToSpaces in Options) then
    begin
      InsertStrLen := (FTabWidth + 2) * (e - BB.Line) + FTabWidth + 1;
      //               chars per line * lines-1    + last line + null char
      StrToInsert := {$IFDEF UNICODE}StrAlloc{$ELSE}WStrAlloc{$ENDIF}(InsertStrLen);
      Run := StrToInsert;
      Spaces := WideStringOfChar(#32, FTabWidth);
    end
    else begin
      InsertStrLen:= 3 * (e - BB.Line) + 2;
      //         #9#13#10 * lines-1 + (last line's #9 + null char)
      StrToInsert := {$IFDEF UNICODE}StrAlloc{$ELSE}WStrAlloc{$ENDIF}(InsertStrLen);
      Run := StrToInsert;
      Spaces := #9;
    end;
    for i := BB.Line to e-1 do
    begin
      WStrCopy(Run, PWideChar(Spaces + #13#10));
      Inc(Run, Length(spaces) + 2);
    end;
    WStrCopy(Run, PWideChar(Spaces));

    fUndoList.BeginBlock;
    try
      InsertionPos.Line := BB.Line;
      if fActiveSelectionMode = smColumn then
        InsertionPos.Char := Min(BB.Char, BE.Char)
      else
        InsertionPos.Char := 1;
      InsertBlock(InsertionPos, InsertionPos, StrToInsert, True);
      fUndoList.AddChange(crIndent, BB, BE, '', smColumn);
      //We need to save the position of the end block for redo
      fUndoList.AddChange(crIndent,
        BufferCoord(BB.Char + length(Spaces), BB.Line),
        BufferCoord(BE.Char + length(Spaces), BE.Line),
        '', smColumn);
    finally
      fUndoList.EndBlock;
    end;

    //adjust the x position of orgcaretpos appropriately
    OrgCaretPos.Char := X;
  finally
    if BE.Char > 1 then
      Inc(BE.Char, Length(Spaces));
    {$IFDEF UNICODE}StrDispose{$ELSE}WStrDispose{$ENDIF}(StrToInsert);
    SetCaretAndSelection(OrgCaretPos,
      BufferCoord(BB.Char + Length(Spaces), BB.Line), BE);
    ActiveSelectionMode := OrgSelectionMode;
  end;
end;

procedure TvgCustomSynEdit.DoBlockUnindent;
var
  OrgCaretPos,
  BB, BE: TBufferCoord;
  Line, Run,
  FullStrToDelete,
  StrToDelete: PWideChar;
  Len, x, StrToDeleteLen, i, TmpDelLen, FirstIndent, LastIndent, e: Integer;
  TempString: WideString;
  OrgSelectionMode: TSynSelectionMode;
  SomethingToDelete: Boolean;

  function GetDelLen: Integer;
  var
    Run: PWideChar;
  begin
    Result := 0;
    Run := Line;
    //Take care of tab character
    if Run[0] = #9 then
    begin
      Result := 1;
      SomethingToDelete := True;
      exit;
    end;
    //Deal with compound tabwidths  Sometimes they have TabChars after a few
    //spaces, yet we need to delete the whole tab width even though the char
    //count might not be FTabWidth because of the TabChar
    while (Run[0] = #32) and (Result < FTabWidth) do
    begin
      Inc(Result);
      Inc(Run);
      SomethingToDelete := True;
    end;
    if (Run[0] = #9) and (Result < FTabWidth) then
      Inc(Result);
  end;

begin
  OrgSelectionMode := fActiveSelectionMode;
  Len := 0;
  LastIndent := 0;
  if SelAvail then
  begin
    // store current selection detail
    BB := BlockBegin;
    BE := BlockEnd;
    OrgCaretPos := CaretXY;
    x := fCaretX;

    // convert selection to complete lines
    if BE.Char = 1 then
      e := BE.Line - 1
    else
      e := BE.Line;

    // build string to delete
    StrToDeleteLen := (FTabWidth + 2) * (e - BB.Line) + FTabWidth + 1;
    //                chars per line * lines-1    + last line + null char
    StrToDelete := {$IFDEF UNICODE}StrAlloc{$ELSE}WStrAlloc{$ENDIF}(StrToDeleteLen);
    StrToDelete[0] := #0;
    SomethingToDelete := False;
    for i := BB.Line to e-1 do
    begin
       Line := PWideChar(Lines[i - 1]);
       //'Line' is 0-based, 'BB.x' is 1-based, so the '-1'
       //And must not increment 'Line' pointer by more than its 'Length'
       if fActiveSelectionMode = smColumn then
         Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[i - 1])]));
       //Instead of doing a WideStringOfChar, we need to get *exactly* what was
       //being deleted incase there is a TabChar
       TmpDelLen := GetDelLen;
       {$IFDEF UNICODE}StrCat{$ELSE}WStrCat{$ENDIF}(StrToDelete, PWideChar(Copy(Line, 1, TmpDelLen)));
       {$IFDEF UNICODE}StrCat{$ELSE}WStrCat{$ENDIF}(StrToDelete, PWideChar(WideString(#13#10)));
       if (fCaretY = i) and (x <> 1) then
         x := x - TmpDelLen;
    end;
    Line := PWideChar(Lines[e - 1]);
    if fActiveSelectionMode = smColumn then
      Inc(Line, MinIntValue([BB.Char - 1, BE.Char - 1, Length(Lines[e - 1])]));
    TmpDelLen := GetDelLen;
    {$IFDEF UNICODE}StrCat{$ELSE}WStrCat{$ENDIF}(StrToDelete, PWideChar(Copy(Line, 1, TmpDelLen)));
    if (fCaretY = e) and (x <> 1) then
      x := x - TmpDelLen;

    FirstIndent := -1;
    FullStrToDelete := nil;
    // Delete string
    if SomethingToDelete then
    begin
      FullStrToDelete := StrToDelete;
      InternalCaretY := BB.Line;
      if fActiveSelectionMode <> smColumn then
        i := 1
      else
        i := Min(BB.Char, BE.Char);
      repeat
        Run := GetEOL(StrToDelete);
        if Run <> StrToDelete then
        begin
          Len := Run - StrToDelete;
          if FirstIndent = -1 then
            FirstIndent := Len;
          if Len > 0 then
          begin
            TempString := Lines[CaretY - 1];
            Delete(TempString, i, Len);
            Lines[CaretY - 1] := TempString;
          end;
        end;
        if Run^ = #13 then
        begin
          Inc(Run);
          if Run^ = #10 then
            Inc(Run);
          Inc(fCaretY);
        end;
        StrToDelete := Run;
      until Run^ = #0;
      LastIndent := Len;
      fUndoList.AddChange(crUnindent, BB, BE, FullStrToDelete, fActiveSelectionMode);
    end;
    // restore selection
    if FirstIndent = -1 then
      FirstIndent := 0;
    //adjust the x position of orgcaretpos appropriately
    if fActiveSelectionMode = smColumn then
      SetCaretAndSelection(OrgCaretPos, BB, BE)
    else
    begin
      OrgCaretPos.Char := X;
      Dec(BB.Char, FirstIndent);
      Dec(BE.Char, LastIndent);
      SetCaretAndSelection(OrgCaretPos, BB, BE);
    end;
    ActiveSelectionMode := OrgSelectionMode;
    if FullStrToDelete <> nil then
      {$IFDEF UNICODE}StrDispose{$ELSE}WStrDispose{$ENDIF}(FullStrToDelete)
    else
      {$IFDEF UNICODE}StrDispose{$ELSE}WStrDispose{$ENDIF}(StrToDelete);
  end;
end;

function TvgCustomSynEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TEditAction then
  begin
    Result := IsFocused;
    if Result then
    begin
      if Action is TEditCut then
        CutToClipboard
      else if Action is TEditCopy then
        CopyToClipboard
      else if Action is TEditPaste then
        PasteFromClipboard
      else if Action is TEditDelete then
      begin
        if SelAvail then
          ClearSelection
        else
          CommandProcessor(ecDeleteChar, ' ', nil)
      end
      else if Action is TEditUndo then
        Undo
      else if Action is TEditSelectAll then
        SelectAll;
    end
  end
  else if Action is TSearchAction then
  begin
    Result := IsFocused;
    if Action is TSearchFindFirst then
      DoSearchFindFirstExecute(TSearchFindFirst(Action))
    else if Action is TSearchFind then
      DoSearchFindExecute(TSearchFind(Action))
    else if Action is TSearchReplace then
      DoSearchReplaceExecute(TSearchReplace(Action));
  end
  else if Action is TSearchFindNext then
  begin
    Result := IsFocused;
    DoSearchFindNextExecute(TSearchFindNext(Action))
  end
  else
    Result := inherited ExecuteAction(Action);
end;

function TvgCustomSynEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  if Action is TEditAction then
  begin
    Result := IsFocused;
    if Result then
    begin
      if Action is TEditCut then
        TEditAction(Action).Enabled := SelAvail and not ReadOnly
      else if Action is TEditCopy then
        TEditAction(Action).Enabled := SelAvail
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := CanPaste
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := not ReadOnly
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := True;
    end;
  end else if Action is TSearchAction then
  begin                                                                         
    Result := IsFocused;
    if Result then
    begin                                                                       
      if Action is TSearchFindFirst then                                        
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine) 
      else if Action is TSearchFind then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine)
      else if Action is TSearchReplace then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine);
    end;
  end else if Action is TSearchFindNext then
  begin
    Result := IsFocused;
    if Result then
      TSearchAction(Action).Enabled := (Text<>'')
        and (TSearchFindNext(Action).SearchFind <> nil)
        and (TSearchFindNext(Action).SearchFind.Dialog.FindText <> '');
  end
  else
    Result := inherited UpdateAction(Action);
end;

procedure TvgCustomSynEdit.SetModified(Value: Boolean);
begin
  if Value <> fModified then begin
    fModified := Value;
    if (eoGroupUndo in Options) and (not Value) and UndoList.CanUndo then
      UndoList.AddGroupBreak;
    UndoList.InitialState := not Value;
    StatusChanged([scModified]);
  end;
end;

function TvgCustomSynEdit.DoOnSpecialLineColors(Line: Integer; var Foreground,
  Background: TColor): Boolean;
begin
  Result := False;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TvgCustomSynEdit.InvalidateLine(Line: Integer);
var
  rcInval: TvgRect;
begin
  if (Line < 1) or (Line > Lines.Count) or (not Visible) then
    Exit;

  if WordWrap then
  begin
    InvalidateLines(Line, Line);
    Exit;
  end;

  if (Line >= TopLine) and (Line <= TopLine + LinesInWindow) then
  begin
    // invalidate text area of this line
    rcInval := vgRect(fGutterWidth, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
{$IFDEF SYN_CLX}
    with GetClientRect do
      OffsetRect(rcInval, Left, Top);
{$ENDIF}
    if sfLinesChanging in fStateFlags then
      fInvalidateRect := vgUnionRect(fInvalidateRect, rcInval)
    else
      InvalidateRect(rcInval, False);
  end;
end;

function TvgCustomSynEdit.GetReadOnly: Boolean;
begin
  Result := fReadOnly;
end;

procedure TvgCustomSynEdit.SetReadOnly(Value: Boolean);
begin
  if fReadOnly <> Value then
  begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TvgCustomSynEdit.FindMatchingBracket;
begin
  InternalCaretXY := GetMatchingBracket;
end;

function TvgCustomSynEdit.GetMatchingBracket: TBufferCoord;
begin
  Result := GetMatchingBracketEx(CaretXY);
end;

function TvgCustomSynEdit.GetMatchingBracketEx(const APoint: TBufferCoord): TBufferCoord;
const
  Brackets: array[0..7] of WideChar = ('(', ')', '[', ']', '{', '}', '<', '>');
var
  Line: WideString;
  i, PosX, PosY, Len: Integer;
  Test, BracketInc, BracketDec: WideChar;
  NumBrackets: Integer;
  vDummy: WideString;
  attr: TSynHighlighterAttributes;
  p: TBufferCoord;
  isCommentOrString: Boolean;
begin
  Result.Char := 0;
  Result.Line := 0;
  // get char at caret
  PosX := APoint.Char;
  PosY := APoint.Line;
  Line := Lines[APoint.Line - 1];
  if Length(Line) >= PosX then
  begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    for i := Low(Brackets) to High(Brackets) do
      if Test = Brackets[i] then
      begin
        // this is the bracket, get the matching one and the direction
        BracketInc := Brackets[i];
        BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
        // search for the matching bracket (that is until NumBrackets = 0)
        NumBrackets := 1;
        if Odd(i) then
        begin
          repeat
            // search until start of line
            while PosX > 1 do
            begin
              Dec(PosX);
              Test := Line[PosX];
              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                  isCommentOrString := (attr = Highlighter.StringAttribute) or
                    (attr = Highlighter.CommentAttribute)
                else
                  isCommentOrString := False;
                if (Test = BracketInc) and (not isCommentOrString) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) and (not isCommentOrString) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get previous line if possible
            if PosY = 1 then break;
            Dec(PosY);
            Line := Lines[PosY - 1];
            PosX := Length(Line) + 1;
          until False;
        end
        else begin
          repeat
            // search until end of line
            Len := Length(Line);
            while PosX < Len do
            begin
              Inc(PosX);
              Test := Line[PosX];
              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                  isCommentOrString := (attr = Highlighter.StringAttribute) or
                    (attr = Highlighter.CommentAttribute)
                else
                  isCommentOrString := False;
                if (Test = BracketInc) and (not isCommentOrString) then
                  Inc(NumBrackets)
                else if (Test = BracketDec)and (not isCommentOrString) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get next line if possible
            if PosY = Lines.Count then
              Break;
            Inc(PosY);
            Line := Lines[PosY - 1];
            PosX := 0;
          until False;
        end;
        // don't test the other brackets, we're done
        break;
      end;
  end;
end;

function TvgCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: WideString; var Attri: TSynHighlighterAttributes): Boolean;
var
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;

function TvgCustomSynEdit.GetHighlighterAttriAtRowColEx(const XY: TBufferCoord;
  var Token: WideString; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: Integer;
  Line: WideString;
begin
  PosY := XY.Line - 1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
  begin
    Line := Lines[PosY];
    if PosY = 0 then
      Highlighter.ResetRange
    else
      Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY - 1]);
    Highlighter.SetLine(Line, PosY);
    PosX := XY.Char;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do
      begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then
        begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := True;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := False;
end;

function TvgCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TvgCustomSynEdit.GetHookedCommandHandlersCount: Integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TvgCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TObjectList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TvgCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: Integer;
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
    fHookedCommandHandlers.Delete(i)
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TvgCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer);
var
  Handled: Boolean;
  i: Integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := False;
  for i := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.fEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.fData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TvgCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(fOnClearMark) then
    fOnClearMark(Self, Mark);
end;

procedure TvgCustomSynEdit.DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean);
var
  DoTransient: Boolean;
begin
  DoTransient:=(FPaintTransientLock=0);
  if Lock then
    begin
    if (TransientType=ttBefore) then inc(FPaintTransientLock)
    else
      begin
      dec(FPaintTransientLock);
      DoTransient:=(FPaintTransientLock=0);
      end;
    end;

  if DoTransient and Assigned(fOnPaintTransient) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Fill.SolidColor := FFontFill.SolidColor;
    HideCaret;
    try
//      fOnPaintTransient(Self, Canvas, TransientType);
    finally
      ShowCaret;
    end;
  end;
end;

procedure TvgCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);
begin
  DoOnPaintTransientEx(TransientType, False);
end;

procedure TvgCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Fill.Assign(FFontFill);
//    fOnPaint(Self, Canvas);
  end;
end;

procedure TvgCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(fOnPlaceMark) then
    fOnPlaceMark(Self, Mark);
end;

function TvgCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: WideString;
  Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TvgCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Assigned(fOnStatusChange) then
  begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TvgCustomSynEdit.UpdateModifiedStatus;
begin
  Modified := not UndoList.InitialState;
end;

procedure TvgCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  UpdateModifiedStatus;

  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not fUndoList.InsideRedo and
     (fUndoList.PeekItem<>nil) and (fUndoList.PeekItem.ChangeReason<>crGroupBreak) then
    fRedoList.Clear;
  if TSynEditUndoList(Sender).BlockCount = 0 then
    DoChange;
end;

function TvgCustomSynEdit.GetWordAtRowCol(XY: TBufferCoord): WideString;
var
  Line: WideString;
  Len, Stop: Integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then
  begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if Len = 0 then Exit;
    if (XY.Char >= 1) and (XY.Char <= Len + 1) and IsIdentChar(Line[XY.Char]) then
    begin
      Stop := XY.Char;
      while (Stop <= Len) and IsIdentChar(Line[Stop]) do
        Inc(Stop);
      while (XY.Char > 1) and IsIdentChar(Line[XY.Char - 1]) do
        Dec(XY.Char);
      if Stop > XY.Char then
        Result := Copy(Line, XY.Char, Stop - XY.Char);
    end;
  end;
end;

function TvgCustomSynEdit.BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
var
  s: WideString;
  i, L: Integer;
  x, CountOfAvgGlyphs: Integer;
begin
  Canvas.Font.Assign(Font);

  Result := TDisplayCoord(p);
  if p.Line - 1 < Lines.Count then
  begin
    s := Lines[p.Line - 1];
    l := Length(s);
    x := 0;
    for i := 1 to p.Char - 1 do begin
      if (i <= l) and (s[i] = #9) then
        inc(x, TabWidth - (x mod TabWidth))
      else if i <= l then
      begin
        CountOfAvgGlyphs := {1; // }Ceil(Canvas.TextWidth(s[i]) / fCharWidth);
        inc(x, CountOfAvgGlyphs);
      end
      else
        inc(x);
    end;
    Result.Column := x + 1;
  end;
  if WordWrap then
    Result := fWordWrapPlugin.BufferToDisplayPos(TBufferCoord(Result));
end;

function TvgCustomSynEdit.DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
// DisplayToBufferPos takes a position on screen and transfrom it
// into position of text
var
  s: WideString;
  i, L: Integer;
  x, CountOfAvgGlyphs: Integer;
begin
  Canvas.Font.Assign(Font);

  if WordWrap then
    Result := fWordWrapPlugin.DisplayToBufferPos(p)
  else
    Result := TBufferCoord(p);
  if Result.Line <= lines.Count then
  begin
    s := Lines[Result.Line -1];
    l := Length(s);
    x := 0;
    i := 0;

    while x < Result.Char  do
    begin
      inc(i);
      if (i <= l) and (s[i] = #9) then
        inc(x, TabWidth - (x mod TabWidth))
      else if i <= l then
      begin
        CountOfAvgGlyphs := {1; //}Ceil(Canvas.TextWidth(s[i]) / fCharWidth);
        inc(x, CountOfAvgGlyphs);
      end
      else
        inc(x);
    end;
    Result.Char := i;
  end;
end;

procedure TvgCustomSynEdit.DoLinesDeleted(FirstLine, Count: Integer);
var
  i: Integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;
      
  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      THackSynEditPlugin(fPlugins[i]).LinesDeleted(FirstLine, Count);
end;

procedure TvgCustomSynEdit.DoLinesInserted(FirstLine, Count: Integer);
var
  i: Integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;

  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      THackSynEditPlugin(fPlugins[i]).LinesInserted(FirstLine, Count);
end;

procedure TvgCustomSynEdit.PluginsAfterPaint(ACanvas: TvgCanvas; const AClip: TvgRect;
  FirstLine, LastLine: Integer);
var
  i: Integer;
begin
{  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
      THackSynEditPlugin(fPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine, LastLine);}
end;

procedure TvgCustomSynEdit.ProperSetLine(ALine: Integer; const ALineText: WideString);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := TrimTrailingSpaces(ALineText)
  else
    Lines[ALine] := ALineText;
end;

procedure TvgCustomSynEdit.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TvgCustomSynEdit.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TvgCustomSynEdit.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TvgCustomSynEdit.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TvgCustomSynEdit.AddKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  fKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TvgCustomSynEdit.RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  fKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TvgCustomSynEdit.AddFocusControl(aControl: TWinControl);
begin
  fFocusList.Add(aControl);
end;

procedure TvgCustomSynEdit.RemoveFocusControl(aControl: TWinControl);
begin
  fFocusList.Remove(aControl);
end;

function TvgCustomSynEdit.IsIdentChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsIdentChar(AChar)
  else
    Result := AChar >= #33;

  if Assigned(Highlighter) then
    Result := Result or CharInSet(AChar, Highlighter.AdditionalIdentChars)
  else
    Result := Result or CharInSet(AChar, Self.AdditionalIdentChars);

  Result := Result and not IsWordBreakChar(AChar);
end;

function TvgCustomSynEdit.IsWhiteChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWhiteChar(AChar)
  else
    case AChar of
    #0..#32:
      Result := True;
    else
      Result := not (IsIdentChar(AChar) or IsWordBreakChar(AChar))
    end
end;

function TvgCustomSynEdit.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWordBreakChar(AChar)
  else
    case AChar of
      #0..#32, '.', ',', ';', ':', '"', '''', '´', '`', '°', '^', '!', '?', '&',
      '$', '@', '§', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
      '-', '=', '+', '*', '/', '\', '|':
        Result := True;
      else
        Result := False;
    end;

  if Assigned(Highlighter) then
  begin
    Result := Result or CharInSet(AChar, Highlighter.AdditionalWordBreakChars);
    Result := Result and not CharInSet(AChar, Highlighter.AdditionalIdentChars);
  end
  else
  begin
    Result := Result or CharInSet(AChar, Self.AdditionalWordBreakChars);
    Result := Result and not CharInSet(AChar, Self.AdditionalIdentChars);
  end;
end;

procedure TvgCustomSynEdit.SetSearchEngine(Value: TSynEditSearchCustom);
begin
  if (fSearchEngine <> Value) then
  begin
    fSearchEngine := Value;
    if Assigned(fSearchEngine) then
      fSearchEngine.FreeNotification(Self);
  end;
end;

function TvgCustomSynEdit.NextWordPos: TBufferCoord;
begin
  Result := NextWordPosEx(CaretXY);
end;

function TvgCustomSynEdit.WordStart: TBufferCoord;
begin
  Result := WordStartEx(CaretXY);
end;

function TvgCustomSynEdit.WordEnd: TBufferCoord;
begin
  Result := WordEndEx(CaretXY);
end;

function TvgCustomSynEdit.PrevWordPos: TBufferCoord;
begin
  Result := PrevWordPosEx(CaretXY);
end;

function TvgCustomSynEdit.GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
// Get XY caret position of mouse. Returns False if point is outside the
// region of the SynEdit control.
var
  Point: TvgPoint;
begin
(*  GetCursorPos(Point);                    // mouse position (on screen)
  Point := Self.ScreenToClient(Point);    // convert to SynEdit coordinates
  { Make sure it fits within the SynEdit bounds }
  if (Point.X < 0) or (Point.Y < 0) or (Point.X > Self.Width) or (Point.Y> Self.Height) then
  begin
    Result := False;
    Exit;
  end;

  { inside the editor, get the word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  Result := True; *)
end;

function TvgCustomSynEdit.GetWordAtMouse: WideString;
var
  Point: TBufferCoord;
begin
  { Return the word under the mouse }
  if GetPositionOfMouse(Point) then        // if point is valid
    Result := Self.GetWordAtRowCol(Point); // return the point at the mouse position
end;

function TvgCustomSynEdit.CharIndexToRowCol(Index: Integer): TBufferCoord;
{ Index is 0-based; Result.x and Result.y are 1-based }
var
  x, y, Chars: Integer;
begin
  x := 0;
  y := 0;
  Chars := 0;
  while y < Lines.Count do
  begin
    x := Length(Lines[y]);
    if Chars + x + 2 > Index then
    begin
      x := Index - Chars;
      break;
    end;
    Inc(Chars, x + 2);
    x := 0;
    Inc(y);
  end;
  Result.Char := x + 1;
  Result.Line := y + 1;
end;

function TvgCustomSynEdit.RowColToCharIndex(RowCol: TBufferCoord): Integer;
{ Row and Col are 1-based; Result is 0-based }
var
  i: Integer;
begin
  Result := 0;
  RowCol.Line := Min(Lines.Count, RowCol.Line) - 1;
  for i := 0 to RowCol.Line - 1 do
    Result := Result + Length(Lines[i]) + 2;
  Result := Result + (RowCol.Char -1);
end;

procedure TvgCustomSynEdit.Clear;
{ just to attain interface compatibility with TMemo }
begin
  ClearAll;
end;

function TvgCustomSynEdit.GetSelLength: Integer;
begin
  if SelAvail then
    Result := RowColToCharIndex(BlockEnd) - RowColToCharIndex(BlockBegin)
  else
    Result := 0;
end;

procedure TvgCustomSynEdit.SetSelLength(const Value: Integer);
var
  iNewCharIndex: Integer;
  iNewBegin: TBufferCoord;
  iNewEnd: TBufferCoord;
begin
  iNewCharIndex := RowColToCharIndex(BlockBegin) + Value;
  if (Value >= 0) or (iNewCharIndex < 0) then
  begin
    if iNewCharIndex < 0 then
    begin
      iNewEnd.Char := Length(Lines[Lines.Count - 1]) + 1;
      iNewEnd.Line := Lines.Count;
    end
    else
      iNewEnd := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewEnd, BlockBegin, iNewEnd);
  end
  else begin
    iNewBegin := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewBegin, iNewBegin, BlockBegin);
  end;
end;

procedure TvgCustomSynEdit.DefineProperties(Filer: TFiler);

  function CollectionsEqual(C1, C2: TCollection): Boolean;
  begin
    Result := Classes.CollectionsEqual(C1, C2, nil, nil);
  end;

  function HasKeyData: Boolean;
  var
    iDefKeys: TSynEditKeyStrokes;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := not CollectionsEqual(Keystrokes,
        TvgCustomSynEdit(Filer.Ancestor).Keystrokes);
    end
    else begin
      iDefKeys := TSynEditKeyStrokes.Create(nil);
      try
        iDefKeys.ResetDefaults;
        Result := not CollectionsEqual(Keystrokes, iDefKeys);
      finally
        iDefKeys.Free;
      end;
    end;
  end;

var
  iSaveKeyData: Boolean;
begin
  inherited;
{$IFNDEF UNICODE}
  UnicodeDefineProperties(Filer, Self);
{$ENDIF}
  iSaveKeyData := HasKeyData;
  Filer.DefineProperty('RemovedKeystrokes', ReadRemovedKeystrokes,
    WriteRemovedKeystrokes, iSaveKeyData);
  Filer.DefineProperty('AddedKeystrokes', ReadAddedKeystrokes, WriteAddedKeystrokes,
    iSaveKeyData);
end;

procedure TvgCustomSynEdit.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TvgCustomSynEdit.ReadAddedKeystrokes(Reader: TReader);
var
  iAddKeys: TSynEditKeyStrokes;
  cKey: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iAddKeys := TSynEditKeyStrokes.Create(Self);
  try
    Reader.ReadCollection(iAddKeys);
    for cKey := 0 to iAddKeys.Count -1 do
      Keystrokes.Add.Assign(iAddKeys[cKey]);
  finally
    iAddKeys.Free;
  end;
end;

procedure TvgCustomSynEdit.ReadRemovedKeystrokes(Reader: TReader);
var
  iDelKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iToDelete: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iDelKeys := TSynEditKeyStrokes.Create(nil);
  try
    Reader.ReadCollection(iDelKeys);
    for cKey := 0 to iDelKeys.Count -1 do
    begin
      iKey := iDelKeys[cKey];
      iToDelete := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iToDelete >= 0) and (Keystrokes[iToDelete].Command = iKey.Command) then
        Keystrokes[iToDelete].Free;
    end;
  finally
    iDelKeys.Free;
  end;
end;

procedure TvgCustomSynEdit.WriteAddedKeystrokes(Writer: TWriter);
var
  iDefaultKeys: TSynEditKeyStrokes;
  iAddedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iDelIndex: Integer;
begin
  iDefaultKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iDefaultKeys.Assign(TvgSynEdit(Writer.Ancestor).Keystrokes)
    else
      iDefaultKeys.ResetDefaults;
    iAddedKeys := TSynEditKeyStrokes.Create(nil);
    try
      for cKey := 0 to Keystrokes.Count -1 do
      begin
        iKey := Keystrokes[cKey];
        iDelIndex := iDefaultKeys.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        //if it's not a default keystroke, add it
        if (iDelIndex < 0) or (iDefaultKeys[iDelIndex].Command <> iKey.Command) then
          iAddedKeys.Add.Assign(iKey);
      end;
      Writer.WriteCollection(iAddedKeys);
    finally
      iAddedKeys.Free;
    end;
  finally
    iDefaultKeys.Free;
  end;
end;

procedure TvgCustomSynEdit.WriteRemovedKeystrokes(Writer: TWriter);
var
  iRemovedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iFoundAt: Integer;
begin
  iRemovedKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iRemovedKeys.Assign(TvgSynEdit(Writer.Ancestor).Keystrokes)
    else
      iRemovedKeys.ResetDefaults;
    cKey := 0;
    while cKey < iRemovedKeys.Count do
    begin
      iKey := iRemovedKeys[cKey];
      iFoundAt := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iFoundAt >= 0) and (Keystrokes[iFoundAt].Command = iKey.Command) then
        iKey.Free //if exists in Keystrokes, then shouldn't be in "removed" list
      else
        Inc(cKey);
    end;
    Writer.WriteCollection(iRemovedKeys);
  finally
    iRemovedKeys.Free;
  end;
end;

procedure TvgCustomSynEdit.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseDownHandler(aHandler);
end;

procedure TvgCustomSynEdit.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseDownHandler(aHandler);
end;

procedure TvgCustomSynEdit.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseUpHandler(aHandler);
end;

procedure TvgCustomSynEdit.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseUpHandler(aHandler);
end;

procedure TvgCustomSynEdit.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TvgCustomSynEdit.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

procedure TvgCustomSynEdit.DoSearchFindFirstExecute(Action: TSearchFindFirst);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnCloseBeforeSearch := Action.Dialog.OnClose;
  SelStartBeforeSearch := SelStart; SelLengthBeforeSearch := SelLength;

  Action.Dialog.OnFind := FindDialogFindFirst;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TvgCustomSynEdit.DoSearchFindExecute(Action: TSearchFind);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TvgCustomSynEdit.DoSearchReplaceExecute(Action: TSearchReplace);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnReplaceBeforeSearch := Action.Dialog.OnReplace;
  OnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnReplace := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TvgCustomSynEdit.DoSearchFindNextExecute(Action: TSearchFindNext);
begin
  SearchByFindDialog(Action.SearchFind.Dialog);
end;

procedure TvgCustomSynEdit.FindDialogFindFirst(Sender: TObject);
begin
  TFindDialog(Sender).CloseDialog;

  if (SelStart = SelStartBeforeSearch) and (SelLength = SelLengthBeforeSearch) then
  begin
    SelStart := 0;
    SelLength := 0;
  end;

  if Sender is TFindDialog then
    if not SearchByFindDialog(TFindDialog(Sender)) and (SelStart = 0) and (SelLength = 0) then
    begin
      SelStart := SelStartBeforeSearch;
      SelLength := SelLengthBeforeSearch;
    end;
end;

procedure TvgCustomSynEdit.FindDialogFind(Sender: TObject);
begin
  if Sender is TFindDialog then
    SearchByFindDialog(TFindDialog(Sender));
end;

function TvgCustomSynEdit.SearchByFindDialog(FindDialog: TFindDialog) : bool;
var
  Options :TSynSearchOptions;
  ReplaceText, MessageText :String;
  OldSelStart, OldSelLength: integer;
begin
  if (frReplaceAll in FindDialog.Options) then Options := [ssoReplaceAll]
  else if (frReplace in FindDialog.Options) then Options := [ssoReplace]
  else Options := [ssoSelectedOnly];

  if (frMatchCase in FindDialog.Options) then Options := Options + [ssoMatchCase];
  if (frWholeWord in FindDialog.Options) then Options := Options + [ssoWholeWord];
  if (not (frDown in FindDialog.Options)) then Options := Options + [ssoBackwards];

  if (ssoSelectedOnly in Options)
    then ReplaceText := ''
    else ReplaceText := TReplaceDialog(FindDialog).ReplaceText;

  OldSelStart := SelStart; OldSelLength := SelLength;
  if (UpperCase(SelText) = UpperCase(FindDialog.FindText)) and not (frReplace in FindDialog.Options) then
    SelStart := SelStart + SelLength
  else
    SelLength := 0;

  Result := SearchReplace(FindDialog.FindText, ReplaceText, Options) > 0;
  if not Result then
  begin
    SelStart := OldSelStart; SelLength := OldSelLength;
    if Assigned(OnSearchNotFound) then
      OnSearchNotFound(self, FindDialog.FindText)
    else
    begin
      MessageText := Format(STextNotFound, [FindDialog.FindText]);
      ShowMessage(MessageText);
    end;
  end
  else if (frReplace in FindDialog.Options) then
  begin
    SelStart := SelStart - Length(FindDialog.FindText) - 1;
    SelLength := Length(FindDialog.FindText) + 1;
  end;
end;

procedure TvgCustomSynEdit.FindDialogClose(Sender: TObject);
begin
  TFindDialog(Sender).OnFind := OnFindBeforeSearch;
  if Sender is TReplaceDialog then
    TReplaceDialog(Sender).OnReplace := OnReplaceBeforeSearch;
  TFindDialog(Sender).OnClose := OnCloseBeforeSearch;
end;

function TvgCustomSynEdit.GetWordWrap: Boolean;
begin
  Result := fWordWrapPlugin <> nil;
end;

procedure TvgCustomSynEdit.SetWordWrap(const Value: Boolean);
var
  vTempBlockBegin, vTempBlockEnd : TBufferCoord;
  vOldTopLine: Integer;
  vShowCaret: Boolean;
begin
  if WordWrap <> Value then
  begin
    Repaint; // better Invalidate before changing LeftChar and TopLine
    vShowCaret := CaretInView;
    vOldTopLine := RowToLine(TopLine);
    if Value then
    begin
//      fWordWrapPlugin := TSynWordWrapPlugin.Create(Self);
      LeftChar := 1;
    end
    else
      fWordWrapPlugin := nil;
    TopLine := LineToRow(vOldTopLine);
    UpdateScrollBars;

    // constrain caret position to MaxScrollWidth if eoScrollPastEol is enabled
    if (eoScrollPastEol in Options) then
    begin
      InternalCaretXY := CaretXY;
      vTempBlockBegin := BlockBegin;
      vTempBlockEnd := BlockEnd;
      SetBlockBegin(vTempBlockBegin);
      SetBlockEnd(vTempBlockEnd);
    end;
    if vShowCaret then
      EnsureCursorPosVisible;
  end;
end;

function TvgCustomSynEdit.GetDisplayLineCount: Integer;
begin
  if fWordWrapPlugin = nil then
    Result := Lines.Count
  else if Lines.Count = 0 then
    Result := 0
  else begin
    Result := fWordWrapPlugin.RowCount;
  end;
end;

function TvgCustomSynEdit.LineToRow(aLine: Integer): Integer;
var
  vBufferPos: TBufferCoord;
begin
  if not WordWrap then
    Result := aLine
  else begin
    vBufferPos.Char := 1;
    vBufferPos.Line := aLine;
    Result := BufferToDisplayPos(vBufferPos).Row;
  end;
end;

function TvgCustomSynEdit.RowToLine(aRow: Integer): Integer;
var
  vDisplayPos: TDisplayCoord;
begin
  if not WordWrap then
    Result := aRow
  else begin
    vDisplayPos.Column := 1;
    vDisplayPos.Row := aRow;
    Result := DisplayToBufferPos(vDisplayPos).Line;
  end;
end;

procedure TvgCustomSynEdit.SetInternalDisplayXY(const aPos: TDisplayCoord);
begin
  IncPaintLock;
  InternalCaretXY := DisplayToBufferPos(aPos);
  fCaretAtEOL := WordWrap and (aPos.Row <= fWordWrapPlugin.RowCount) and
    (aPos.Column > fWordWrapPlugin.GetRowLength(aPos.Row)) and
    (DisplayY <> aPos.Row);
  DecPaintLock;
  UpdateLastCaretX;
end;

procedure TvgCustomSynEdit.SetWantReturns(Value: Boolean);
begin
  fWantReturns := Value;
  {$IFDEF SYN_CLX}
  if fWantReturns then
    InputKeys := InputKeys + [ikReturns]
  else
    InputKeys := InputKeys - [ikReturns];
  {$ENDIF}
end;

procedure TvgCustomSynEdit.SetWantTabs(Value: Boolean);
begin
  fWantTabs := Value;
  {$IFDEF SYN_CLX}
  if fWantTabs then
    InputKeys := InputKeys + [ikTabs]
  else
    InputKeys := InputKeys - [ikTabs];
  {$ENDIF}
end;

procedure TvgCustomSynEdit.SetWordWrapGlyph(const Value: TSynGlyph);
begin
  fWordWrapGlyph.Assign(Value);
end;

procedure TvgCustomSynEdit.WordWrapGlyphChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    InvalidateGutter;
end;


initialization
  RegisterVGObjects('SynEdit', [TvgSynEdit]);
end.
