unit vg_memo;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFDEF KS_COMPILER6_UP} Types, StrUtils, {$ENDIF}
  {$IFNDEF NOVCL} Forms, Controls, Menus, Clipbrd, {$ENDIF}
  {$IFDEF LCL} LCLType, {$ENDIF}
  SysUtils, Classes, Contnrs, vg_scene, vg_controls, vg_layouts;

type

  TEditCharCase = (vgecNormal, vgecUpperCase, vgecLowerCase);

  TInsertOption = (ioSelected, ioMoveCaret, ioCanUndo, ioUnDoPairedWithPriv);
  TInsertOptions = set of TInsertOption;
  TDeleteOption = (doMoveCaret, doCanUndo);
  TDeleteOptions = set of TDeleteOption;

  TActionType = (atDelete, atInsert);

  TLinesBegs = array of integer;
  PLinesBegs = ^TLinesBegs;

  TCaretPosition = record
    Line, Pos: integer;
  end;

  PEdtAction = ^TEdtAction;

  TEdtAction = record
    ActionType : TActionType;
    PairedWithPriv : boolean;
    StartPosition : integer;
    DeletedFragment : WideString; {For atDelete}
    Length : integer; {For atInsert}
  end;

  TvgMemo = class;

  TEdtActionStack = class(TStack)
  private
    FOwner : TvgMemo;
  public
    constructor Create(AOwner : TvgMemo);
    destructor Destroy; override;

    procedure FragmentInserted(StartPos, FragmentLength : integer; IsPairedWithPriv : boolean);
    procedure FragmentDeleted(StartPos : integer; Fragment : WideString);
    procedure CaretMovedBy(Shift : integer);

    function RollBackAction : boolean;
  end;

  TSelArea = array of TvgRect;

  TvgMemo = class(TvgScrollBox)
  private
    FNeedChange: boolean;
    FText: WideString;
    FFontFill: TvgBrush;
    FFont: TvgFont;
    FTextAlign: TvgTextAlign;
    FInternalMustUpdateLines : boolean;
    FLMouseSelecting: boolean;
    FOldMPt : TvgPoint;
    FCaretPosition: TCaretPosition;
    FFirstVisibleChar: integer;
    FUnwrapLines: TvgWideStrings;
    {$IFNDEF NOVCL}
    FPopupMenu: TPopupMenu;
    {$ENDIF}
    FAutoSelect: boolean;
    FCharCase: TEditCharCase;
    FHideSelection: Boolean;
    FMaxLength: Integer;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FTextAlignment: TAlignment;
    FActionStack : TEdtActionStack;
    FLines: TvgWideStrings;
    FWordWrap: boolean;
    FLinesBegs : array of integer;
    FSelStart: TCaretPosition;
    FSelEnd: TCaretPosition;
    FSelected : boolean;
    FOldSelStartPos, FOldSelEndPos, FOldCaretPos : integer;
    FSelectionFill: TvgBrush;
    FOnChangeTracking: TNotifyEvent;
    function GetSelBeg : TCaretPosition;
    function GetSelEnd : TCaretPosition;
    procedure StorePositions;
    procedure RestorePositions;
    procedure SelectAtPos(APos : TCaretPosition);
    procedure SetCaretPosition(const Value: TCaretPosition);
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelStart: integer;
    function GetSelLength: integer;
    procedure UpdateHScrlBarByCaretPos;
    procedure UpdateVScrlBarByCaretPos;
    function GetSelText: WideString;
    procedure SetAutoSelect(const Value: boolean);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMaxLength(const Value: Integer);
    procedure SelectAtMousePoint;
    function GetNextWordBeging(StartPosition: TCaretPosition): TCaretPosition;
    function GetPrivWordBeging(StartPosition: TCaretPosition): TCaretPosition;
    function GetPositionShift(APos : TCaretPosition; Delta: integer {char count}):TCaretPosition;
    procedure MoveCareteBy(Delta : integer);
    procedure MoveCaretLeft;
    procedure MoveCaretRight;
    procedure MoveCaretVertical(LineDelta : integer);
    procedure MoveCaretDown;
    procedure MoveCaretUp;
    procedure MoveCaretPageUp;
    procedure MoveCaretPageDown;
    procedure UpdateCaretPosition(UpdateScrllBars : boolean);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetLines(const Value: TvgWideStrings);
    procedure GetLineBounds(LineIndex : integer; var LineBeg, LineLength : integer);
    function GetLineCount : integer;
    function GetLine(Index : integer) : WideString; //Returns Line without special symbols at the end.
    function GetLineInternal(Index : integer) : WideString; //Returns Line with special symbols at the end.
    procedure InsertLine(Index: Integer; const S: WideString);
    procedure DeleteLine(Index: Integer);
    procedure ClearLines;
    procedure SetWordWrap(const Value: boolean);
    function GetPageSize : single;
    function GetLineWidth(LineNum: Integer): single;
    function GetWidestLine : integer;
    function FillLocalLinesBegs(PText: PWideString; ABegChar, AEndChar: integer;
      TmpLinesBegs: PLinesBegs) : integer;
    procedure UpdateRngLinesBegs(PText : PWideString;AUpdBegLine, AUpdEndLine,
      AUpdBegChar, AUpdEndChar, ACharDelta, AOldWideslLineWidth : integer);
    function GetShowSelection : boolean;
    function GetLineRealEnd(AStartPos: TCaretPosition; PText: PWideString): TCaretPosition;
    procedure SetFont(const Value: TvgFont);
    procedure SetTextAlign(const Value: TvgTextAlign);
    function TextWidth(const Str: WideString): single;
    procedure HScrlBarChange(Sender: TObject);
    procedure SetUpdateState(Updating: Boolean);
    function GetUnwrapLines: TvgWideStrings;
  protected
    FUpdating: boolean;
    FWidesLineIndex : integer;
    FTextWidth : array of integer;
    function GetLineHeight : single;
    function GetPointPosition(Pt : TvgPoint): TCaretPosition;
    procedure SetText(const Value: WideString); virtual;
    function GetSelArea: TSelArea; virtual;
    procedure DrawPasswordChar(SymbolRect: TvgRect; Selected: boolean); virtual;
    procedure CreatePopupMenu; virtual;
    procedure UpdatePopupMenuItems; virtual;
    procedure ApplyStyle; override;
    function ContentPos: TvgPoint;
    procedure Change; virtual;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    function ValidText(NewText: WideString): boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; //override;
    procedure ContextMenu(const ScreenPosition: TvgPoint); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y, dx, dy: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure SelectWord;
    procedure FontChanged(Sender: TObject);
    procedure DoUndo(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoCopy(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoSelectAll(Sender: TObject);
    procedure UpdateLines;
    { inherited }
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
    procedure VScrollChange(Sender: TObject);
    function GetContentBounds: TvgRect; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure CutToClipboard;
    procedure ClearSelection;
    procedure SelectAll;
    procedure GoToTextEnd;
    procedure GoToTextBegin;
    procedure GotoLineEnd;
    procedure GoToLineBegin;
    function GetPositionPoint(ACaretPos : TCaretPosition): TvgPoint;
    procedure UnDo;
    procedure InsertAfter(Position: TCaretPosition; S: WideString; Options : TInsertOptions);
    procedure DeleteFrom(Position: TCaretPosition; ALength : integer; Options : TDeleteOptions);
    function TextPosToPos(APos: integer): TCaretPosition;
    function PosToTextPos(APostion: TCaretPosition): integer;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText;
    property CaretPosition: TCaretPosition read FCaretPosition write SetCaretPosition;
    property LineWidth[LineNum: Integer]: single read GetLineWidth;
    { return unwrapped lines }
    property UnwrapLines: TvgWideStrings read GetUnwrapLines;
    { custom colors - only work when style was loaded }
    property FontFill: TvgBrush read FFontFill;
    property SelectionFill: TvgBrush read FSelectionFill;
  published
    property Cursor default crIBeam;
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property AutoSelect: boolean read FAutoSelect write SetAutoSelect default true;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default vgecNormal;
    property Enabled;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property Lines: TvgWideStrings read FLines write SetLines stored false;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeTracking: TNotifyEvent read FOnChangeTracking write FOnChangeTracking;
    property WordWrap: boolean read FWordWrap write SetWordWrap;
    property Font: TvgFont read FFont write SetFont;
    property Text: WideString read FText write SetText stored true;
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign default vgTextAlignNear;
    property Resource;
  end;

  TvgHudMemo = class(TvgMemo)
  private
  protected
  public
  published
  end;

function ComposeCaretPos(ALine, APos : integer) : TCaretPosition;

implementation

type

  TvgMemoLines = class(TvgWideStrings)
  private
    FMemo: TvgMemo;
  protected
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

{ TvgMemoLines }

procedure TvgMemoLines.Clear;
begin
  FMemo.ClearLines;
  FMemo.Change;
end;

procedure TvgMemoLines.Delete(Index: Integer);
begin
  FMemo.DeleteLine(Index);
  FMemo.Change;
end;

procedure TvgMemoLines.Insert(Index: Integer; const S: WideString);
begin
  FMemo.InsertLine(Index, S);
  FMemo.Change;
end;

function TvgMemoLines.Get(Index: Integer): WideString;
begin
  Result := FMemo.GetLine(Index);
end;

function TvgMemoLines.GetCount: Integer;
begin
  Result := FMemo.GetLineCount;
end;

procedure TvgMemoLines.SetUpdateState(Updating: Boolean);
begin
  inherited;
  FMemo.SetUpdateState(Updating);
end;

function ComposeCaretPos(ALine, APos : integer) : TCaretPosition;
begin
  with Result do
  begin
    Line := ALine;
    Pos := APos;
  end;
end;

{ TvgMemo }

constructor TvgMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextAlign := vgTextAlignNear;
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
  FFontFill := TvgBrush.Create(vgBrushSolid, $FF000000);
  FSelectionFill := TvgBrush.Create(vgBrushSolid, $802A8ADF);
  FLines := TvgMemoLines.Create;
  (FLines as TvgMemoLines).FMemo := Self;
  CanFocused := true;
  Cursor := crIBeam;
  FInternalMustUpdateLines := true;

  CreatePopupMenu;

  FActionStack := TEdtActionStack.Create(Self);

  FTextAlignment := taLeftJustify;
  FAutoSelect := true;
  FCharCase := vgecNormal;
  FHideSelection := true;
  FMaxLength := 0;
  FReadOnly := false;

  FLMouseSelecting := false;
  FOldMPt := vgPoint(0,0);

  FUpdating := false;


  with FCaretPosition do
  begin
    Line := 0;
    Pos := 0;
  end;

  FSelStart := ComposeCaretPos(0,0);
  FSelEnd := ComposeCaretPos(0,0);
  FSelected := false;

  FOldSelStartPos := -1;
  FOldSelEndPos := -1;
  FOldCaretPos := -1;

  AutoCapture := true;

  FWidesLineIndex := 0;
  SetLength(FTextWidth,0);

  Width := 100;
end;

destructor TvgMemo.Destroy;
begin
  if FUnwrapLines <> nil then
    FUnwrapLines.Free;
  FSelectionFill.Free;
  FFontFill.Free;
  FFont.Free;
  FActionStack.Free;
  {$IFNDEF NOVCL}
  FPopupMenu.Free;
  {$ENDIF}
  FLines.Free;
  inherited;
end;

procedure TvgMemo.EnterFocus;
begin
  inherited;
  FNeedChange := false;
  UpdateCaretPosition(false);
  ShowCaretProc;
  if FScene.ShowKeyboardForControl(Self) then
  begin
    CaretPosition := TextPosToPos(Length(Text));
  end
  else
  begin
    with FCaretPosition do
    begin
      Line := 0;
      Pos := 0;
    end;
    if AutoSelect then
      SelectAll;
  end;
end;

procedure TvgMemo.Killfocus;
begin
  FScene.HideKeyboardForControl(Self);
  inherited;
  HideCaret;
  Change;
end;

function TvgMemo.TextWidth(const Str: WideString): single;
var
  R: TvgRect;
begin
  R := ContentRect;
  R.Right := 10000;
  Canvas.Font.Assign(Font);
  Canvas.MeasureText(R, R, Str, false, TextAlign, vgTextAlignCenter);
  Result := vgRectWidth(R);
end;

function TvgMemo.GetPositionPoint(ACaretPos : TCaretPosition): TvgPoint;
var
  WholeTextWidth : single;
  EditRectWidth : single;
  LineText : WideString;
begin
  Result.X := ContentRect.Left;
  Result.Y := ContentRect.Top + (GetLineHeight * ACaretPos.Line) - VScrollBarValue;
  WholeTextWidth := 0;
  if Canvas = nil then Exit;

  if (ACaretPos.Line < Lines.Count) and (Lines.Count > 0) then
  begin
    LineText := Lines[ACaretPos.Line];

    WholeTextWidth := TextWidth(LineText);

    if ACaretPos.Pos > 0 then
    begin
      if ACaretPos.Pos <= Length(LineText) then
        Result.X := Result.X + TextWidth(Copy(LineText, 1, ACaretPos.Pos))
      else
        Result.X := Result.X + TextWidth(LineText);
    end;
  end;
  EditRectWidth := ContentRect.Right - ContentRect.Left;
  if WholeTextWidth < EditRectWidth then
    case FTextAlign of
      vgTextAlignFar: Result.X := Result.X + (EditRectWidth-WholeTextWidth);
      vgTextAlignCenter: Result.X := Result.X + ((EditRectWidth-WholeTextWidth) / 2);
    end;
  Result.X := Result.X - HScrollBarValue;
end;

function TvgMemo.GetPointPosition(Pt : TvgPoint): TCaretPosition;
var
  CurX: double;
  TmpX,
  WholeTextWidth,
  EdiTvgRectWidth : single;
  LineText : WideString;
  LLine : integer;
  LPos : integer;
  TmpPt : TvgPoint;
  LEdiTvgRect : TvgRect;
begin
  with Result do
  begin
    Line := 0;
    Pos := 0;
  end;

  if Lines.Count <= 0 then
    Exit;

  LEdiTvgRect := ContentRect;

  with LEdiTvgRect, Pt do begin
    if x < Left then
      TmpPt.x := Left
    else
      if x > Right then
        TmpPt.x := Right
      else
        TmpPt.x := x;

    if y < Top then
      TmpPt.y := Top
    else
      if y > Bottom then
        TmpPt.y := Bottom
      else
        TmpPt.y := y;
  end;

  LLine := trunc((TmpPt.Y - ContentRect.Top) / GetLineHeight + trunc(VScrollBarValue / GetLineHeight));

  LPos := 0;

  if LLine > Lines.Count-1 then
    LLine := Lines.Count-1;

  LineText := Lines[LLine];

  if Length(LineText) > 0 then
  begin
    WholeTextWidth := TextWidth(LineText);

    EdiTvgRectWidth := ContentRect.Right - ContentRect.Left;
    TmpX := TmpPt.x;
    if WholeTextWidth < EdiTvgRectWidth then
      case TextAlign of
        vgTextAlignFar: TmpX := TmpPt.x - (EdiTvgRectWidth-WholeTextWidth);
        vgTextAlignCenter: TmpX := TmpPt.x - ((EdiTvgRectWidth-WholeTextWidth) / 2);
      end;

    TmpX := TmpX + HScrollBarValue;

    CurX := ContentRect.Left + TextWidth(LineText[1]) / 2;
    while (CurX < TmpX) and (LPos + 1 <= Length(LineText)) and (CurX < ContentRect.Right + HScrollBarValue) do
    begin
      CurX := ContentRect.Left + TextWidth(Copy(LineText, 1, LPos + 1)) + (Font.Size / 4);
      Inc(LPos);
    end;
  end;
  with Result do
  begin
    Line := LLine;
    Pos := LPos;
  end;
end;

procedure TvgMemo.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  TmpS: WideString;
  OldCaretPosition: TCaretPosition;
  WasSelection : boolean;
  LTmpOptions : TInsertOptions;
begin
  inherited KeyDown(Key, KeyChar, Shift);
  OldCaretPosition := CaretPosition;
  if (Key = VK_RETURN) then
  begin
    WasSelection := SelLength > 0;
    if WasSelection then
      DeleteFrom(GetSelBeg,SelLength,[doMoveCaret, doCanUndo]{true,true, false});
    if WasSelection then
      LTmpOptions := [ioUnDoPairedWithPriv]
    else
      LTmpOptions := [];
    TmpS := #13#10;
    InsertAfter(CaretPosition, TmpS, LTmpOptions+[ioMoveCaret, ioCanUndo]{false, true, true, WasSelection});
    SelLength := 0;
    Key := 0;
  end;
  case Key of
    VK_END: if ssCtrl in Shift then
              GoToTextEnd
            else
              GoToLineEnd;
    VK_HOME: if ssCtrl in Shift then
               GoToTextBegin
             else
               GoToLineBegin;
    VK_LEFT:
      if ssCtrl in Shift then
        CaretPosition := GetPrivWordBeging(CaretPosition)
      else
        MoveCaretLeft;
    VK_RIGHT:
      if ssCtrl in Shift then
        CaretPosition := GetNextWordBeging(CaretPosition)
      else
        MoveCaretRight;
    VK_UP:
      MoveCaretUp;
    VK_DOWN:
      MoveCaretDown;
    VK_PRIOR:
      MoveCaretPageUp;
    VK_NEXT:
      MoveCaretPageDown;
    VK_DELETE, 8: {Delete or BackSpace key was pressed}
      if not ReadOnly then
      begin
        if SelLength <> 0 then
        begin
          if ssShift in Shift then
            CutToClipboard
          else
            ClearSelection;
        end
        else
        begin
          TmpS := Text;
          if Key = VK_DELETE then
            DeleteFrom(CaretPosition,1, [doMoveCaret, doCanUndo])
          else {BackSpace key was pressed}
            if PosToTextPos(CaretPosition) > 0 then
              DeleteFrom(GetPositionShift(CaretPosition,-1),1, [doMoveCaret, doCanUndo]);
        end;
      end;
    VK_INSERT:
      if ssCtrl in Shift then
        CopyToClipboard
      else
        if ssShift in Shift then
          PasteFromClipboard;
  end;

  case KeyChar of
    'a','A':
      if Shift = [ssCtrl] then
      begin
        SelectAll;
        KeyChar := #0;
      end;
    'c','C':
      if Shift = [ssCtrl] then
      begin
        CopyToClipboard;
        KeyChar := #0;
      end;
    'v', 'V':
      if Shift = [ssCtrl] then
      begin
        PasteFromClipboard;
        KeyChar := #0;
      end;
    'x', 'X':
      if Shift = [ssCtrl] then
      begin
        CutToClipboard;
        KeyChar := #0;
      end;
    'z', 'Z':
      if Shift = [ssCtrl] then
      begin
        {UnDo};
        KeyChar := #0;
      end;
  end;

  if ((Ord(Keychar) >= 32) or (Keychar = #13)) and not ReadOnly then
  begin
    WasSelection := SelLength > 0;
    if WasSelection then
      DeleteFrom(GetSelBeg,SelLength,[doMoveCaret, doCanUndo]{true,true, false});
    if WasSelection then
      LTmpOptions := [ioUnDoPairedWithPriv]
    else
      LTmpOptions := [];
    if Keychar <> #13 then
    begin
      InsertAfter(CaretPosition, KeyChar, LTmpOptions+[ioMoveCaret, ioCanUndo]{false, true, true, WasSelection});
    end
    else
    begin
      TmpS := #13#10;
      InsertAfter(CaretPosition, TmpS, LTmpOptions+[ioMoveCaret, ioCanUndo]{false, true, true, WasSelection});
    end;
    SelLength := 0;
    Keychar := #0;
  end;
  
  if Key in [VK_END, VK_HOME, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT] then
  begin
    if ssShift in Shift then
    begin
      if not FSelected then
        SelectAtPos(OldCaretPosition);
      SelectAtPos(CaretPosition);
      Repaint;
    end else
      if FSelected then begin
        FSelected := false;
        Repaint;
      end;
  end;
  UpdateCaretPosition(true);
end;

procedure TvgMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single);
begin
  inherited;
  if (Button = mbLeft) and  (ssDouble in Shift) then
  begin
    if vgPtInRect(vgPoint(x,y), ContentRect) then
    begin
      FLMouseSelecting := false;
      SelectWord;
    end;
  end;
  if (Button = mbLeft) and  vgPtInRect(vgPoint(x,y), ContentRect) then
  begin
    FLMouseSelecting := true;
    CaretPosition := GetPointPosition(vgPoint(x,y));
    FSelected := false;
    SelectAtPos(CaretPosition);
    Repaint;
  end;
end;

function TvgMemo.ContentPos: TvgPoint;
var
  T: TvgObject;
begin
  T := FindResource('content');
  if (T <> nil) and (T.IsVisual) then
  begin
    Result := TvgVisualObject(T).Position.Point;
  end;
end;

procedure TvgMemo.DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
var
  R, TmpRect: TvgRect;
  LSelArea : TSelArea;
  CurSelRect : integer;
  SaveIndex: integer;
  CurChar, CurLine, LEndLine: integer;
  TmpPt : TvgPoint;
  LPageSize : single;
  LLeftTopCharPt : TvgPoint;
begin
  with Canvas do
  begin
    SaveIndex := Canvas.SaveCanvas;
    Canvas.IntersectClipRect(ARect);

    Font.Assign(Font);
    Fill.Assign(FFontFill);

    // text
    R := ContentRect;
    TmpRect := ARect;
    Canvas.Font.Assign(Font);
    LPageSize := GetPageSize;

    LLeftTopCharPt.X := TmpRect.Left;
    LLeftTopCharPt.Y := TmpRect.Top;

    CurLine := trunc(VScrollBarValue / GetLineHeight);
    if round(VScrollBarValue / GetLineHeight) + LPageSize - 1 < Lines.Count-1 then
      LEndLine := Round(round(VScrollBarValue / GetLineHeight) + LPageSize-1)
    else
      LEndLine := Lines.Count-1;
    while CurLine <= LEndLine do
    begin
      TmpPt := GetPositionPoint(ComposeCaretPos(CurLine, 0));
      Canvas.FillText(vgRect(TmpPt.X - R.Left, TmpPt.Y - R.Top, $FFFF, TmpPt.Y - R.Top + (GetLineHeight * 1.25)), LocalRect, Lines[CurLine], false, 1, vgTextAlignNear, vgTextAlignNear);
      Inc(CurLine);
    end;

    // selection
    if IsFocused then
    begin
      LSelArea := GetSelArea;
      if GetShowSelection then
      begin
        Fill.Assign(FSelectionFill);
        for CurSelRect := Low(LSelArea) to High(LSelArea) do
        begin
          vgIntersectRect(TmpRect, LSelArea[CurSelRect], vgRect(0, 0, 1000, 1000));
          vgOffsetRect(TmpRect, -R.Left, -R.Top);
          FillRect(TmpRect, 0, 0, [], 1);
        end;
      end;
    end;
    Canvas.RestoreCanvas(SaveIndex);
  end;
end;

procedure TvgMemo.ApplyStyle;
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

procedure TvgMemo.UpdateHScrlBarByCaretPos;
var
  LEdiTvgRect: TvgRect;
  LCaretLinePos : integer;
  LCaretLine : integer;
  CurCaretX : integer;
begin
  if Lines.Count <= 0 then
    Exit;
  if Canvas = nil then Exit;

  LEdiTvgRect := ContentRect;
  CurCaretX := Round(GetPositionPoint(CaretPosition).X);

  if not ((CurCaretX < LEdiTvgRect.Left) or
          (CurCaretX > LEdiTvgRect.Right)) then
    Exit;

  LCaretLinePos := CaretPosition.Pos;
  LCaretLine := CaretPosition.Line;

  if FFirstVisibleChar >= (LCaretLinePos + 1) then
  begin
    FFirstVisibleChar := LCaretLinePos;
    if FFirstVisibleChar < 1 then
      FFirstVisibleChar := 1;
  end
  else
  begin                                                                                                                                          // caret
    while (TextWidth(Copy(Lines[LCaretLine], FFirstVisibleChar, LCaretLinePos - FFirstVisibleChar + 1)) > LEdiTvgRect.Right - LEdiTvgRect.Left - 5) and (FFirstVisibleChar < Length(Lines[LCaretLine])) do
      Inc(FFirstVisibleChar);
  end;
  if (HScrollBar <> nil) and (HScrollBar.Visible) then
    HScrollBar.Value := TextWidth(Copy(Lines[LCaretLine],1,FFirstVisibleChar-1));
end;

procedure TvgMemo.MouseMove(Shift: TShiftState; x, y, dx, dy: single);
var
  LEdiTvgRect : TvgRect;
begin
  inherited;
  FOldMPt := vgPoint(x,y);

  if FLMouseSelecting then
  begin
    LEdiTvgRect := ContentRect;

{    if y < LEdiTvgRect.Top then
      VScrollBar.AutoScrollUp := true
    else
      if y > LEdiTvgRect.Bottom then
        VScrollBar.AutoScrollDown := true
      else begin
        VScrollBar.AutoScrollDown := false;
        VScrollBar.AutoScrollUp := false;
      end;}

    SelectAtMousePoint;
  end;
end;

procedure TvgMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
begin
  inherited;
  FLMouseSelecting := false;
  if SelLength = 0 then
    FSelected := false;
end;

procedure TvgMemo.CopyToClipboard;
var
  Data: THandle;
  DataPtr: Pointer;
  Size: Cardinal;
  S: WideString;
begin
  {$IFNDEF NOVCL}
  {$IFNDEF FPC}
  if Length(SelText) > 0 then
  begin
    S := SelText;
      begin
        Size := Length(S);
        Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * Size + 2);
        try
          DataPtr := GlobalLock(Data);
          try
            Move(PWideChar(S)^, DataPtr^, 2 * Size + 2);
            Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
          finally
            GlobalUnlock(Data);
          end;
        except
          GlobalFree(Data);
          raise;
        end;
      end;
  end;
  {$ELSE}
  if SelText <> '' then
    Clipbrd.Clipboard.AsText := UTF8Encode(SelText);
  {$ENDIF}
  {$ENDIF}
end;

procedure TvgMemo.PasteFromClipboard;
var
  WasSelection : boolean;
  Data: THandle;
  Insertion: WideString;
begin
  if ReadOnly then Exit;
  try
    {$IFNDEF NOVCL}
    {$IFNDEF FPC}
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

    WasSelection := SelLength >0;
    if WasSelection then
    begin
      DeleteFrom(GetSelBeg,SelLength, [doMoveCaret, doCanUndo]);
      InsertAfter(GetSelBeg, Insertion, [ioMoveCaret, ioCanUndo, ioUndoPairedWithPriv]);
    end
    else
      InsertAfter(CaretPosition, Insertion, [ioMoveCaret, ioCanUndo{, ioUndoPairedWithPriv}]);
    {$ELSE}
    WasSelection := SelLength >0;
    if WasSelection then
    begin
      DeleteFrom(GetSelBeg, SelLength, [doMoveCaret, doCanUndo]);
      InsertAfter(GetSelBeg, {$IFDEF FPC}UTF8Decode{$ENDIF}(ClipBoard.AsText), [ioMoveCaret, ioCanUndo, ioUndoPairedWithPriv]);
    end
    else
      InsertAfter(CaretPosition, {$IFDEF FPC}UTF8Decode{$ENDIF}(ClipBoard.AsText), [ioMoveCaret, ioCanUndo{, ioUndoPairedWithPriv}]);
    {$ENDIF}
    {$ENDIF}
    Change;
  finally
  end;
end;

procedure TvgMemo.CreatePopupMenu;
{$IFNDEF NOVCL}
var
  TmpItem: TMenuItem;
{$ENDIF}
begin
  {$IFNDEF NOVCL}
  FPopupMenu := TPopupMenu.Create(Self);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := 'Undo';
    OnClick := DoUndo;
  end;
  FPopupMenu.Items.Add(TmpItem);

//  FPopupMenu.Items.NewBottomLine;

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := 'Cut';
    OnClick := DoCut;
  end;
  FPopupMenu.Items.Add(TmpItem);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := 'Copy';
    OnClick := DoCopy;
  end;
  FPopupMenu.Items.Add(TmpItem);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := 'Paste';
    OnClick := DoPaste;
  end;
  FPopupMenu.Items.Add(TmpItem);

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := 'Delete';
    OnClick := DoDelete;
  end;
  FPopupMenu.Items.Add(TmpItem);

//  FPopupMenu.Items.NewBottomLine;

  TmpItem := TMenuItem.Create(FPopupMenu);
  with TmpItem do
  begin
    Caption := 'Select All';
    OnClick := DoSelectAll;
  end;
  FPopupMenu.Items.Add(TmpItem);
  {$ENDIF}
end;

procedure TvgMemo.DoCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TvgMemo.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TvgMemo.DoDelete(Sender: TObject);
begin
  ClearSelection;
end;

procedure TvgMemo.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TvgMemo.UpdatePopupMenuItems;
var
  SelTextEmpty: boolean;
begin
  SelTextEmpty := SelText <> '';
  {$IFNDEF NOVCL}
  FPopupMenu.Items.Find('Undo').Enabled := FActionStack.AtLeast(1) and not ReadOnly;
  FPopupMenu.Items.Find('Cut').Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find('Copy').Enabled := SelTextEmpty;
  FPopupMenu.Items.Find('Paste').Enabled := (ClipBoard.AsText <> '') and not ReadOnly;
  FPopupMenu.Items.Find('Delete').Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find('Select All').Enabled := SelText <> Text;
  {$ENDIF}
end;

function TvgMemo.GetNextWordBeging(StartPosition: TCaretPosition): TCaretPosition;
var
  SpaceFound,
  WordFound: boolean;
  LLineText : WideString;
  CurPos : integer;
  CurLine : integer;
begin
  CurPos := StartPosition.Pos;
  CurLine := StartPosition.Line;

  if StartPosition.Pos < Length(GetLine(StartPosition.Line)) then begin
    LLineText := GetLine(StartPosition.Line);

    SpaceFound := false;
    WordFound := false;
    while (CurPos + 2 <= Length(LLineText)) and
      ((not ((LLineText[CurPos + 1] <> ' ') and SpaceFound))
      or not WordFound) do
    begin
      if LLineText[CurPos + 1] = ' ' then
        SpaceFound := true;
      if LLineText[CurPos + 1] <> ' ' then begin
        WordFound := true;
        SpaceFound := false;
      end;

      CurPos := CurPos + 1;
    end;
    if not SpaceFound then
      CurPos := CurPos + 1;
  end else
    if StartPosition.Line < Lines.Count-1 then begin
      CurLine := CurLine+1;
      CurPos := 0;
    end;

  with Result do begin
    Line := CurLine;
    Pos := CurPos;
  end
end;

function TvgMemo.GetPrivWordBeging(StartPosition: TCaretPosition): TCaretPosition;
var
  WordFound: boolean;
  LLineText : WideString;
  CurPos : integer;
  CurLine : integer;
begin
  Result := StartPosition;

  CurPos := StartPosition.Pos;
  CurLine := StartPosition.Line;

  if StartPosition.Pos > 0 then begin
    LLineText := GetLine(StartPosition.Line);

    WordFound := false;
    while (CurPos > 0) and
      ((LLineText[CurPos] <> ' ') or not WordFound) do
    begin
      if LLineText[CurPos] <> ' ' then
        WordFound := true;
      CurPos := CurPos - 1;
    end;
  end else
    if (StartPosition.Line-1 >= 0) and (StartPosition.Line-1<=Lines.Count-1) then begin
      CurLine := CurLine-1;
      CurPos := Length(GetLine(CurLine));
    end;

  with Result do begin
    Line := CurLine;
    Pos := CurPos;
  end
end;


procedure TvgMemo.ClearSelection;
begin
  if not ReadOnly then
    DeleteFrom(GetSelBeg, SelLength, [doMoveCaret, doCanUndo]);
end;

procedure TvgMemo.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TvgMemo.SelectAll;
begin
  FSelStart := TextPosToPos(Length(FText));
  FSelEnd := ComposeCaretPos(0,0);
  FSelected := true;
  GoToTextEnd;
  Repaint;
end;

procedure TvgMemo.DoSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TvgMemo.DrawPasswordChar(SymbolRect: TvgRect; Selected: boolean);
var
  LRect : TvgRect;
begin
  { !!! Don't forget include clipping rountines
        Char symbol image must not extend out of EdiTvgRect}

  vgIntersectRect(LRect, SymbolRect, ContentRect);

  Canvas.Font.Assign(Font);
//  if Selected then
//    Canvas.Font.Color := clHighlightText;
//  Canvas.Brush.Style := bsClear;
//  Canvas.TexTvgRect(LRect, SymbolRect.Left, SymbolRect.Top, PasswordChar);
end;

function TvgMemo.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := Round(GetLineHeight + ContentRect.Top*2);
end;

procedure TvgMemo.SelectWord;
begin
  FSelStart := GetPrivWordBeging(CaretPosition);
  FSelEnd := GetNextWordBeging(CaretPosition);
  FSelected := true;
  Repaint;
end;

procedure TvgMemo.Change;
begin
  if FNeedChange then
  begin
    if Assigned(FBindingObjects) then
      ToBindingObjects;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgMemo.ContextMenu(const ScreenPosition: TvgPoint);
begin
  inherited;
  if csDesigning in ComponentState then Exit;

  UpdatePopupMenuItems;
  {$IFNDEF NOVCL}
  FPopupMenu.PopupComponent := Self;
  FPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
  {$ENDIF}
end;

procedure TvgMemo.FontChanged(Sender: TObject);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    UpdateLines;
    UpdateCaretPosition(false);
    if not FUpdating then
      Realign;
  end;
end;

procedure TvgMemo.SetText(const Value: WideString);
begin
  if not ValidText(Value) then Exit;

  if Value <> Text then
  begin
    if (Value <> '') and (CharCase <> vgecNormal) then
      case CharCase of
        {$IFDEF KS_COMPILER5}
        vgecUpperCase: FText := UpperCase(Value);
        vgecLowerCase: FText := UpperCase(Value);
        {$ELSE}
        vgecUpperCase: FText := WideUpperCase(Value);
        vgecLowerCase: FText := WideUpperCase(Value);
        {$ENDIF}
      end
    else
      FText := Value;

    if FInternalMustUpdateLines then
    begin
      UpdateLines;
      if not FUpdating then
        Realign;
    end;

    if not (csLoading in ComponentState) and Assigned(OnChangeTracking) then
      OnChangeTracking(Self);
    FNeedChange := true;
  end;
end;

procedure TvgMemo.SetCaretPosition(const Value: TCaretPosition);
begin
  if Value.Line > Lines.Count-1 then
    FCaretPosition.Line := Lines.Count-1
  else
    FCaretPosition.Line := Value.Line;

  if FCaretPosition.Line < 0 then
    FCaretPosition.Line := 0;

  if Value.Pos < 0 then
    FCaretPosition.Pos := 0
  else
    if Value.Pos > Length(Lines[FCaretPosition.Line]) then
      FCaretPosition.Pos := Length(Lines[FCaretPosition.Line])
    else
      FCaretPosition.Pos := Value.Pos;

  UpdateCaretPosition(true);
end;

procedure TvgMemo.SetSelLength(const Value: integer);
begin
  FSelEnd:=TextPosToPos(PosToTextPos(FSelStart)+value);
end;

procedure TvgMemo.SetSelStart(const Value: integer);
begin
  FSelStart := TextPosToPos(value);
end;

procedure TvgMemo.SetAutoSelect(const Value: boolean);
begin
  if FAutoSelect <> Value then
    FAutoSelect := Value;
end;

function TvgMemo.GetSelStart: integer;
begin
  if FSelected then
    Result := PosToTextPos(GetSelBeg)
  else
    Result := PosToTextPos(CaretPosition);
end;

function TvgMemo.GetSelArea: TSelArea;
var
  BegLine, EndLine, CurLine : integer;
  LPageSize : single;
  SelBegLineVisible, SelEndLineVisible : boolean;
begin
  if not FSelected then begin
    SetLength(Result,0);
    Exit;
  end;

  SelBegLineVisible := true;
  SelEndLineVisible := true;

  BegLine := GetSelBeg.Line;

  if BegLine < trunc(VScrollBarValue / GetLineHeight) then
  begin
    BegLine := trunc(VScrollBarValue / GetLineHeight);
    SelBegLineVisible := false;
  end;

  EndLine := GetSelEnd.Line;
  LPageSize := GetPageSize;

  if EndLine > round(VScrollBarValue / GetLineHeight) + LPageSize-1 then
  begin
    EndLine := Round(round(VScrollBarValue / GetLineHeight) + LPageSize-1);
    SelEndLineVisible := false;
  end;

  if EndLine < BegLine then
    EndLine := BegLine;

  SetLength(Result,EndLine-BegLine+1);

  CurLine := BegLine;
  while (CurLine <= EndLine) and (CurLine < Lines.Count) do
  begin
    with Result[CurLine-BegLine] do
    begin
      Left := GetPositionPoint(ComposeCaretPos(CurLine,0)).x;
      Right := GetPositionPoint(ComposeCaretPos(CurLine,Length(Lines[CurLine]))).x;
      Top := GetPositionPoint(ComposeCaretPos(CurLine,0)).y;
      Bottom := GetPositionPoint(ComposeCaretPos(CurLine,0)).y+GetLineHeight;
    end;
    Inc(CurLine);
  end;

  if EndLine-BegLine >= 0 then begin
    if SelBegLineVisible then
      Result[0].Left := GetPositionPoint(ComposeCaretPos(BegLine,GetSelBeg.Pos)).x;
    if SelEndLineVisible then
      Result[EndLine-BegLine].Right := GetPositionPoint(ComposeCaretPos(EndLine,GetSelEnd.Pos)).x;
  end;
end;

function TvgMemo.GetSelLength: integer;
begin
  if FSelected then
    Result := PosToTextPos(GetSelEnd)-PosToTextPos(GetSelBeg)
  else
    Result := 0;
end;

function TvgMemo.GetSelText: WideString;
var
  LSelStart,
  LSelLength : Integer;
begin
  if FSelected then begin
    LSelStart := SelStart;
    LSelLength := SelLength;
    Result := Copy(Text, LSelStart + 1, LSelLength);
  end else
    Result := '';
end;

procedure TvgMemo.SetCharCase(const Value: TEditCharCase);
var
  TmpS: WideString;
begin
  if FCharCase <> Value then
  begin
    FCharCase := Value;
    if Text <> '' then
    begin
      TmpS := Text;
{      case Value of
        vgecUpperCase: Text := WideString(CharUpper(PChar(TmpS)));
        vgecLowerCase: Text := WideString(CharLower(PChar(TmpS)));
      end;}
    end;
  end;
end;

procedure TvgMemo.SetHideSelection(const Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    Repaint;
  end;
end;

procedure TvgMemo.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
  end;
end;

function TvgMemo.ValidText(NewText: WideString): boolean;
begin
  Result := true;
end;

procedure TvgMemo.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    Repaint;
  end;
end;

procedure TvgMemo.UpdateCaretPosition(UpdateScrllBars : boolean);
var
  TmpPt : TvgPoint;
  TmpRect : TvgRect;
begin
  if UpdateScrllBars then
  begin
    UpdateVScrlBarByCaretPos;
    UpdateHScrlBarByCaretPos;
  end;
  TmpRect := ContentRect;
  TmpPt := GetPositionPoint(CaretPosition);
  TmpPt.X := TmpPt.X + HScrollBarValue - TmpRect.Left;
  TmpPt.Y := TmpPt.Y + VScrollBarValue - TmpRect.Top;
  SetCaretSize(vgPoint(2, GetLineHeight));
  SetCaretPos(TmpPt);
  SetCaretColor(FFontFill.SolidColor);
end;

function TvgMemo.GetLineRealEnd(AStartPos : TCaretPosition; PText : PWideString) : TCaretPosition;
begin
  Result.Line := AStartPos.Line;
  while (Result.Line+1 <= Lines.Count-1) and
    (GetLineInternal(Result.Line) = GetLine(Result.Line)) do
    Result.Line := Result.Line + 1;

  if (Result.Line <= Lines.Count-1) and (Lines.Count > 0) then begin
    Result.Pos := Length(GetLine(Result.Line)) + FLinesBegs[Result.Line]-1
  end else
    Result.Pos := 0;
end;

function TvgMemo.FillLocalLinesBegs(PText: PWideString; ABegChar, AEndChar : integer; TmpLinesBegs : PLinesBegs): integer;
var
  WStartChar, WSaveChar, WCurChar: integer;
  LCurChar, LSaveChar: integer;
  TmpS : WideString;
  TmpSWidth, WWidth: single;
  LTmpWidth: single;
  LEditRectWidth : single;
  Tok: WideString;
  LLongestLineWidth : single;
  CurLineIsEmpty : boolean;
  LWidth: single;
  i: integer;
  LLocalWidesLineWidth: integer;
begin
  Result := 0;
  SetLength(TmpLinesBegs^, 0);

  if PText^ = '' then
    Exit;

  LCurChar := ABegChar;
  TmpS := '';
  LTmpWidth := 0;
  CurLineIsEmpty := true;

  with ContentRect do
    LEditRectWidth := Right-Left;

  Result := -1;
  LLocalWidesLineWidth := -1;
  while LCurChar <= AEndChar do
  begin
    if (PText^[LCurChar] = #13) or (PText^[LCurChar] = #10) then
    begin
      LSaveChar := LCurChar - 1; // before #13
      if (PText^[LCurChar] = #13) and (LCurChar + 1 <= Length(PText^)) then
        if PText^[LCurChar + 1] = #10 then
          Inc(LCurChar);

      TmpSWidth := TextWidth(TmpS);
      if LLongestLineWidth < TmpSWidth then
      begin
        LLongestLineWidth := TmpSWidth;
        Result := Length(TmpLinesBegs^) - 1;
      end;

      if FWordWrap and (TextWidth(TmpS) > LEditRectWidth) then
      begin
        WCurChar := 1;
        WStartChar := 1;
        WSaveChar := 1;
        Tok := vgWideGetToken(WCurChar, TmpS, ' ,-');
        while Tok <> '' do
        begin
          WWidth := TextWidth(Copy(TmpS, WStartChar, WCurChar - WStartChar));
          if WWidth > LEditRectWidth then
          begin
            WStartChar := WSaveChar;
            SetLength(TmpLinesBegs^,Length(TmpLinesBegs^) + 1);
            TmpLinesBegs^[Length(TmpLinesBegs^)-1] := LCurChar - Length(TmpS) + WSaveChar - 2;
          end;
          WSaveChar := WCurChar;
          Tok := vgWideGetToken(WCurChar, TmpS, ' ,-');
          if WSaveChar = WCurChar then
            Break; {!!! - error }
        end;
      end;

      SetLength(TmpLinesBegs^,Length(TmpLinesBegs^) + 1);
      TmpLinesBegs^[Length(TmpLinesBegs^)-1] := LCurChar + 1;

      TmpS := '';
      LTmpWidth := 0;
      CurLineIsEmpty := true;
    end
    else
    begin
      CurLineIsEmpty := false;
      TmpS := TmpS + PText^[LCurChar];
    end;
    Inc(LCurChar);
  end;
  if FWordWrap and (TextWidth(TmpS) > LEditRectWidth) then
  begin
    WCurChar := 1;
    WStartChar := 1;
    WSaveChar := 1;
    Tok := vgWideGetToken(WCurChar, TmpS, ' ,-');
    while Tok <> '' do
    begin
      WWidth := TextWidth(Copy(TmpS, WStartChar, WCurChar - WStartChar));
      if WWidth > LEditRectWidth then
      begin
        WStartChar := WSaveChar;
        SetLength(TmpLinesBegs^,Length(TmpLinesBegs^) + 1);
        TmpLinesBegs^[Length(TmpLinesBegs^)-1] := LCurChar - Length(TmpS) + WSaveChar - 1;
      end;
      WSaveChar := WCurChar;
      Tok := vgWideGetToken(WCurChar, TmpS, ' ,-');
      if WSaveChar = WCurChar then
        Break; {!!! - error }
    end;
  end
  else
  begin
    LWidth := Canvas.TextWidth(TmpS);
    if LWidth > LLocalWidesLineWidth then
      Result := Length(TmpLinesBegs^) - 1;
  end;
  if Length(TmpLinesBegs^) = 0 then
    Result := 0;
end;

procedure TvgMemo.UpdateLines;
const
  Sep: WideString = ' ,-.;:'#65292;
var
  WStartChar, WSaveChar, WCurChar: integer;
  LCurChar, LSaveChar: integer;
  TmpS : WideString;
  TmpSWidth, WWidth: single;
  LTmpWidth: single;
  LEditRectWidth : single;
  Tok, LText: WideString;
  LLongestLineWidth : single;
  CurLineIsEmpty : boolean;
  i: integer;
begin
  FWidesLineIndex := 0;
  SetLength(FLinesBegs,0);
  if Text = '' then
    Exit;

  SetLength(FLinesBegs, 0);
  SetLength(FLinesBegs, 1);
  with ContentRect do
    LEditRectWidth := Right - Left;
  if Canvas = nil then Exit;

  // first check linecreaks
  LText := Text;
  TmpS := '';
  LCurChar := 1;
  LTmpWidth := 0;
  CurLineIsEmpty := true;
  FLinesBegs[0] := 1;
  LLongestLineWidth := 0;
  while LCurChar <= Length(LText) do
  begin
    if (LText[LCurChar] = #13) or (LText[LCurChar] = #10) then
    begin
      LSaveChar := LCurChar - 1; // before #13
      if (LText[LCurChar] = #13) and (LCurChar + 1 <= Length(LText)) then
        if LText[LCurChar + 1] = #10 then
          Inc(LCurChar);

      TmpSWidth := TextWidth(TmpS);
      if LLongestLineWidth < TmpSWidth then
      begin
        LLongestLineWidth := TmpSWidth;
        FWidesLineIndex := Length(FLinesBegs) - 1;
      end;

      if FWordWrap and (TextWidth(TmpS) > LEditRectWidth) then
      begin
        WCurChar := 1;
        WStartChar := 1;
        WSaveChar := 1;
        Tok := vgWideGetToken(WCurChar, TmpS, Sep);
        while Tok <> '' do
        begin
          WWidth := TextWidth(Copy(TmpS, WStartChar, WCurChar - WStartChar));
          if WWidth > LEditRectWidth then
          begin
            WStartChar := WSaveChar;
            SetLength(FLinesBegs, Length(FLinesBegs) + 1);
            FLinesBegs[Length(FLinesBegs) - 1] := LCurChar - Length(TmpS) + WSaveChar - 2;
          end;
          WSaveChar := WCurChar;
          Tok := vgWideGetToken(WCurChar, TmpS, Sep);
          if WSaveChar = WCurChar then
            Break; {!!! - error }
        end;
      end;

      SetLength(FLinesBegs,Length(FLinesBegs) + 1);
      FLinesBegs[Length(FLinesBegs) - 1] := LCurChar + 1;

      TmpS := '';
      LTmpWidth := 0;
      CurLineIsEmpty := true;
    end
    else
    begin
      CurLineIsEmpty := false;
      TmpS := TmpS + LText[LCurChar];
    end;
    Inc(LCurChar);
  end;
  // last line
  if FWordWrap and (TextWidth(TmpS) > LEditRectWidth) then
  begin
    WCurChar := 1;
    WStartChar := 1;
    WSaveChar := 1;
    Tok := vgWideGetToken(WCurChar, TmpS, Sep);
    while Tok <> '' do
    begin
      WWidth := TextWidth(Copy(TmpS, WStartChar, WCurChar - WStartChar));
      if WWidth > LEditRectWidth then
      begin
        WStartChar := WSaveChar;
        SetLength(FLinesBegs, Length(FLinesBegs) + 1);
        FLinesBegs[Length(FLinesBegs) - 1] := LCurChar - Length(TmpS) + WSaveChar - 1;
      end;
      WSaveChar := WCurChar;
      Tok := vgWideGetToken(WCurChar, TmpS, Sep);
      if WSaveChar = WCurChar then
        Break; {!!! - error }
    end;
  end
  else
    if LLongestLineWidth < Canvas.TextWidth(TmpS) then
      FWidesLineIndex := Length(FLinesBegs) - 1;
end;

procedure TvgMemo.UpdateRngLinesBegs(PText: PWideString; AUpdBegLine, AUpdEndLine, AUpdBegChar, AUpdEndChar, ACharDelta, AOldWideslLineWidth: integer);
var
  LUpdEndChar,
  LNewWidesLineIndex,
  LLineDelta, i : integer;
  LTmpLinesBegs : TLinesBegs;
begin
  if (Length(FLinesBegs) = 0) and (PText^ <> '') then
  begin
    SetLength(FLinesBegs, 1);
    FLinesBegs[0] := 1;
  end;

  LUpdEndChar := AUpdEndChar + ACharDelta;
  LNewWidesLineIndex := FillLocalLinesBegs(PText, AUpdBegChar, LUpdEndChar, @LTmpLinesBegs) + AUpdBegLine;

  LLineDelta := Length(LTmpLinesBegs) - (AUpdEndLine-AUpdBegLine);

  if LLineDelta > 0 then
  begin
    SetLength(FLinesBegs, Length(FLinesBegs) + LLineDelta);
    for i := Length(FLinesBegs)-1 downto AUpdEndLine+1+LLineDelta do
      FLinesBegs[i] := FLinesBegs[i-LLineDelta] + ACharDelta;
  end
  else
  begin
    for i := AUpdBegLine+1 to Length(FLinesBegs)-1+LLineDelta do
      FLinesBegs[i] := FLinesBegs[i-LLineDelta] + ACharDelta;
    SetLength(FLinesBegs, Length(FLinesBegs) + LLineDelta);
  end;

  for i := 0 to Length(LTmpLinesBegs) - 1 do
    if AUpdBegLine+i+1 <= Length(FLinesBegs)-1 then
      FLinesBegs[AUpdBegLine+i+1] := LTmpLinesBegs[i];

  if FWidesLineIndex > Length(FLinesBegs)-1 then
    FWidesLineIndex := Round(GetWidestLine)
  else
  if LineWidth[LNewWidesLineIndex] >= AOldWideslLineWidth then
    FWidesLineIndex := LNewWidesLineIndex
  else
    if not ((FWidesLineIndex < AUpdBegLine) or (FWidesLineIndex > AUpdEndLine)) then
      FWidesLineIndex := GetWidestLine;

  if not FUpdating then
    Realign;
end;

procedure TvgMemo.InsertAfter(Position: TCaretPosition; S: WideString; Options : TInsertOptions);
var
  LText : WideString;
  Insertion : WideString;
  LUpdBegLine, LUpdBegChar, LUpdEndLine, LUpdEndChar : integer;
  R: integer;
  LInsertionLength : integer;
  LOldWideslLineWidth : single;
begin
  R := PosToTextPos(CaretPosition);
  LText := Text;
  Insertion := S;
  if MaxLength > 0 then
    Insertion := Copy(Insertion, 1, MaxLength - Length(LText));

  if ioCanUndo in Options then
    FActionStack.FragmentInserted(PosToTextPos(Position), Length(S), ioUnDoPairedWithPriv in Options);

  LUpdBegLine := Position.Line;
  if (Length(FLinesBegs) > 0) and (Position.Line <= Length(FLinesBegs) - 1) then
    LUpdBegChar := FLinesBegs[Position.Line]
  else
    LUpdBegChar := 1;

  with GetLineRealEnd(Position, @LText) do
  begin
    LUpdEndLine := Line;
    LUpdEndChar := Pos;
  end;

  LInsertionLength := Length(Insertion);
  LOldWideslLineWidth := LineWidth[FWidesLineIndex];

  Insert(Insertion, LText, PosToTextPos(Position)+1);
  try
    FInternalMustUpdateLines := false;
    Text := LText;
  finally
    FInternalMustUpdateLines := true;
  end;

  UpdateRngLinesBegs(@LText, LUpdBegLine, LUpdEndLine,
    LUpdBegChar, LUpdEndChar, LInsertionLength, Round(LOldWideslLineWidth));

  if ioSelected in Options then
  begin
    FSelStart := Position;
    FSelEnd := GetPositionShift(Position, Length(Insertion));
    FSelected := true;
    CaretPosition := FSelEnd;
  end
  else
  begin
    if not (csLoading in ComponentState) then
    begin
      CaretPosition := TextPosToPos(R + Length(Insertion));
      UpdateCaretPosition(false);
    end;
  end;

  if not FUpdating then
    Realign;
end;

procedure TvgMemo.DeleteFrom(Position : TCaretPosition; ALength: integer; Options : TDeleteOptions);
var
  LUpdBegLine, LUpdEndLine,
  LUpdBegChar, LUpdEndChar : integer;
  LText : WideString;
  LTmpPos, LTmpLength : integer;
  LOldWideslLineWidth : integer;
begin
  LText := Text;

  LTmpLength := ALength;
  LTmpPos :=  PosToTextPos(Position)+1;

  if (LTmpPos+ALength-1+1 <= System.Length(LText)) and
     (LTmpPos+ALength-1 >= 1) and
     (LText[LTmpPos+ALength-1]=#13) and
     (LText[LTmpPos+ALength-1+1]=#10) then
    LTmpLength := LTmpLength + 1;

  if (LTmpPos-1 >= 0) and
     (LTmpPos <= System.Length(LText)) and
     (LText[LTmpPos]=#10) and
     (LText[LTmpPos-1]=#13) then begin
    LTmpLength := LTmpLength + 1;
    LTmpPos := LTmpPos-1;
  end;

  if (doCanUndo in Options) and (LTmpLength > 0) then
    FActionStack.FragmentDeleted(LTmpPos, Copy(LText,LTmpPos,LTmpLength));

  LUpdBegLine := Position.Line;
  if Position.Line <= Length(FLinesBegs)-1 then
    LUpdBegChar := FLinesBegs[Position.Line]
  else
    LUpdBegChar := 1;

  with GetLineRealEnd(GetPositionShift(Position, LTmpLength-1),@LText) do begin
    LUpdEndLine := Line;
    LUpdEndChar := Pos;
  end;

  LOldWideslLineWidth := Round(LineWidth[FWidesLineIndex]);

  Delete(LText,LTmpPos,LTmpLength);

  try
    FInternalMustUpdateLines := false;
    Text := LText;
  finally
    FInternalMustUpdateLines := true;
  end;

  UpdateRngLinesBegs(@LText, LUpdBegLine, LUpdEndLine,
    LUpdBegChar, LUpdEndChar, -LTmpLength, LOldWideslLineWidth);

  if (doMoveCaret in Options) or (SelLength <> 0) then begin
    FSelected := false;
    CaretPosition := Position;
  end;

  if not FUpdating then
    Realign;
end;

procedure TvgMemo.DoUndo(Sender: TObject);
begin
  UnDo;
end;

procedure TvgMemo.UnDo;
begin
  FActionStack.RollBackAction;
end;

function TvgMemo.GetContentBounds: TvgRect;
begin
  Result := inherited GetContentBounds;
  if FWordWrap then
  begin
    StorePositions;
    UpdateLines;
    RestorePositions;
  end;
  if Lines.Count > 0 then
    Result.Bottom := Result.Top + (Lines.Count * GetLineHeight);
  // Updating Horizontal scrollbar params
  if not FWordWrap and (TextWidth(Lines[FWidesLineIndex]) > (Result.Right - Result.Left)) then
    Result.Right := Result.Left + TextWidth(Lines[FWidesLineIndex]) + 10;// for caret
  UpdateHScrlBarByCaretPos;
end;

procedure TvgMemo.SetLines(const Value: TvgWideStrings);
begin
  FLines.Assign(Value);
end;

function TvgMemo.TextPosToPos(APos : integer) : TCaretPosition;
var
  CurRangeBeg, CurRangeEnd : integer;
  TmpI : integer;
begin
  with Result do begin
    Line := 0;
    Pos := 0;
  end;

  if Lines.Count <= 0 then
    Exit;

  CurRangeBeg := 0;
  CurRangeEnd := Length(FLinesBegs)-1;
  repeat
    if ((CurRangeBeg < Length(FLinesBegs)-1) and
        (APos+1>=FLinesBegs[CurRangeBeg]) and
        (APos+1<FLinesBegs[CurRangeBeg+1]))
    or ((CurRangeBeg = Length(FLinesBegs)-1) and
        (APos+1>=FLinesBegs[CurRangeBeg]))
    then
      CurRangeEnd := CurRangeBeg
    else
    begin
      if APos+1 < FLinesBegs[CurRangeBeg] then begin
        TmpI := CurRangeEnd - CurRangeBeg+1;
        CurRangeEnd := CurRangeBeg;
        CurRangeBeg := CurRangeBeg - TmpI div 2;
      end else
        if APos+1 >= FLinesBegs[CurRangeEnd] then begin
          TmpI := CurRangeEnd - CurRangeBeg+1;
          CurRangeBeg := CurRangeEnd;
          CurRangeEnd := CurRangeEnd + TmpI div 2;
        end else
          CurRangeEnd := (CurRangeBeg + CurRangeEnd) div 2;

      if CurRangeBeg < 0 then
        CurRangeBeg := 0;

      if CurRangeEnd < 0 then
        CurRangeEnd := 0;

      if CurRangeEnd > Length(FLinesBegs)-1 then
        CurRangeEnd := Length(FLinesBegs)-1;

      if CurRangeBeg > Length(FLinesBegs)-1 then
        CurRangeBeg := Length(FLinesBegs)-1;
    end;

  until CurRangeBeg = CurRangeEnd;
  Result.Line := CurRangeBeg;

  if Result.Line <= Length(FLinesBegs)-1 then
    Result.Pos := APos-FLinesBegs[Result.Line]+1;
end;

procedure TvgMemo.MoveCaretLeft;
begin
  MoveCareteBy(-1);
end;

procedure TvgMemo.MoveCaretRight;
begin
  MoveCareteBy(1);
end;

procedure TvgMemo.MoveCareteBy(Delta : integer);
begin
  CaretPosition := GetPositionShift(CaretPosition, Delta);
end;

procedure TvgMemo.VScrollChange(Sender: TObject);
begin
  inherited ;
  UpdateCaretPosition(false);
end;

function TvgMemo.GetLineHeight: single;
begin
  Result := round(FFont.Size * (1.25));
end;

procedure TvgMemo.HScrlBarChange(Sender: TObject);
begin
  Repaint;
  UpdateCaretPosition(false);
end;

procedure TvgMemo.UpdateVScrlBarByCaretPos;
var
  LCaretPosLine : integer;
  LPageSize : single;
begin
  LCaretPosLine := CaretPosition.Line;
  LPageSize := GetPageSize;

  if (VScrollBar <> nil) and (LCaretPosLine * GetLineHeight < VScrollBarValue) then
    VScrollBar.Value := LCaretPosLine * GetLineHeight;

  if (VScrollBar <> nil) and ((LCaretPosLine + 1) * GetLineHeight > VScrollBarValue + ContentLayout.Height) then
    VScrollBar.Value := (LCaretPosLine - LPageSize + 1) * GetLineHeight;
end;

procedure TvgMemo.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    UpdateLines;
    if not FUpdating then
      Realign;
    UpdateCaretPosition(false);
  end;
end;

procedure TvgMemo.GetLineBounds(LineIndex: integer; var LineBeg,
  LineLength: integer);
begin
  if Length(FLinesBegs) = 0 then
  begin
    LineBeg := 1;
    LineLength := 0;
    Exit;
  end;

  if (LineIndex <= Length(FLinesBegs)-1) and (LineIndex >= 0) then
  begin
    LineBeg := FLinesBegs[LineIndex];
    if (LineIndex+1 < Length(FLinesBegs))then
      LineLength := FLinesBegs[LineIndex+1]-LineBeg
    else
      LineLength := Length(Text)-LineBeg+1;
  end
  else
  begin
    LineBeg := 0;
    LineLength := 0;
  end;
end;

function TvgMemo.GetLineCount: integer;
begin
  if Text <> '' then
    Result := Length(FLinesBegs)
  else
    Result := 0;
end;

function TvgMemo.GetLine(Index: integer): WideString;
begin
  Result := GetLineInternal(Index);
  if Length(Result) > 0 then
    if Result[Length(Result)] = #10 then
      Delete(Result,Length(Result),1);
  if Length(Result) > 0 then
    if Result[Length(Result)] = #13 then
      Delete(Result,Length(Result),1);
end;

procedure TvgMemo.InsertLine(Index: Integer; const S: WideString);
begin
  if Index < GetLineCount then
    InsertAfter(ComposeCaretPos(Index, 0),S+#13+#10,[])
  else
    if (Index > 0) and (GetLineCount > 0) then
    begin
      InsertAfter(ComposeCaretPos(Index - 1, Length(GetLineInternal(Index - 1))),#13+#10+S,[])
    end
    else
      InsertAfter(ComposeCaretPos(Index, 0),S,[]);
end;

procedure TvgMemo.DeleteLine(Index: Integer);
begin
  if Index = GetLineCount - 1 then
    DeleteFrom(ComposeCaretPos(Index-1,Length(GetLineInternal(Index-1))){LLineBeg-1} ,Length(GetLineInternal(Index))+1, [])
  else
    DeleteFrom(ComposeCaretPos(Index,0){LLineBeg}, Length(GetLineInternal(Index))+1, []);
end;

procedure TvgMemo.ClearLines;
begin
  Text := '';
end;

procedure TvgMemo.SelectAtMousePoint;
var
  TmpPt : TvgPoint;
  LEdiTvgRect : TvgRect;
begin
  LEdiTvgRect := ContentRect;
  TmpPt := FOldMPt;
  with TmpPt, LEdiTvgRect do begin
    if y < Top then
      y := Top
    else
      if y > Bottom then
        y := Bottom;

    if x < Left then
      x := Left
    else
      if x > Right then
        x := Right;
  end;

  CaretPosition :=  GetPointPosition(TmpPt);
  SelectAtPos(CaretPosition);
  Repaint;
end;

function TvgMemo.GetPageSize: single;
begin
  with ContentRect do
    Result := (Bottom-Top) / GetLineHeight;
end;

function TvgMemo.GetLineWidth(LineNum: Integer): single;
begin
  if (LineNum >= 0) and (LineNum <= Lines.Count-1) then
  begin
    Result := Canvas.TextWidth(Lines[LineNum]);
  end
  else
    Result := 0;
end;

function TvgMemo.PosToTextPos(APostion: TCaretPosition): integer;
var
  LTmpLine : integer;
begin
  Result := 0;
  if Text = '' then
    Exit;

  with APostion do begin
    if Line <= Length(FLinesBegs)-1 then
      LTmpLine := Line
    else
      LTmpLine := Length(FLinesBegs)-1;

    if LTmpLine < 0 then
      exit;

    Result := FLinesBegs[LTmpLine];

    if Pos <= Length(GetLineInternal(LTmpLine)) then
      Result := Result + Pos -1
    else
      Result := Result + Length(GetLineInternal(LTmpLine)) -1;

  end;
end;

function TvgMemo.GetLineInternal(Index: integer): WideString;
var
  LLineBeg, LLineLength : integer;
begin
  GetLineBounds(Index, LLineBeg, LLineLength);
  Result := Copy(Text, LLineBeg, LLineLength);
end;

procedure TvgMemo.GoToTextBegin;
begin
  with FCaretPosition do
  begin
    Line := 0;
    Pos := 0;
  end;
end;

procedure TvgMemo.GoToTextEnd;
begin
  with FCaretPosition do
  begin
    Line := Lines.Count - 1;
    if Line >= 0 then
      Pos := Length(Lines[Line])
    else
      Pos := 0;
  end;
end;

procedure TvgMemo.GoToLineEnd;
begin
  with FCaretPosition do
  begin
    if Line <= Lines.Count-1 then
      Pos := Length(GetLine(CaretPosition.Line));
  end;
end;

procedure TvgMemo.GoToLineBegin;
begin
  with FCaretPosition do
  begin
    Pos := 0;
  end;
end;

function TvgMemo.GetSelBeg: TCaretPosition;
begin
  if FSelStart.Line < FSelEnd.Line then
    Result := FSelStart
  else
    if FSelEnd.Line < FSelStart.Line then
      Result := FSelEnd
    else
      if FSelStart.Pos < FSelEnd.Pos then
        Result := FSelStart
      else
        Result := FSelEnd;
end;

function TvgMemo.GetSelEnd: TCaretPosition;
begin
  if FSelStart.Line > FSelEnd.Line then
    Result := FSelStart
  else
    if FSelEnd.Line > FSelStart.Line then
      Result := FSelEnd
    else
      if FSelStart.Pos > FSelEnd.Pos then
        Result := FSelStart
      else
        Result := FSelEnd;
end;

procedure TvgMemo.SelectAtPos(APos: TCaretPosition);
begin
  if not FSelected then begin
    FSelStart := APos;
    FSelEnd := APos;
    FSelected := true;
  end else begin
    FSelEnd := APos;
  end;
end;

function TvgMemo.GetPositionShift(APos: TCaretPosition;
  Delta: integer): TCaretPosition;
var
  LNewPos : TCaretPosition;
  LNewTextPos : integer;
  i : integer;
  CurLineText : WideString;
begin
  LNewPos := APos;
  with LNewPos do
    if Delta >= 14 then begin
      LNewTextPos := PosToTextPos(CaretPosition)+Delta;

      if Delta > 0 then begin
        if (LNewTextPos+1 <= Length(Text)) and
           (Text[LNewTextPos+1] = #10) then
          Inc(LNewTextPos);
      end else
        if Delta < 0 then begin
          if (LNewTextPos+1-1 >= Length(Text)) and
             (Text[LNewTextPos+1-1] = #10) then
          Dec(LNewTextPos);
        end;

      LNewPos := TextPosToPos(LNewTextPos);
    end else begin
      CurLineText := GetLineInternal(Line);
      if Delta > 0 then begin
        i := 1;
        while i <= Delta do begin
          Pos := Pos+1;
          if (Pos+1 <= Length(CurLineText)) and (CurLineText[Pos+1] = #10) then begin
            Inc(Pos); Inc(i);
          end;
          if Pos+1 > Length(CurLineText) then begin
            if Line+1 <= Lines.Count-1 then begin
              Line := Line+1;
              CurLineText := GetLineInternal(Line);
              Pos := 0;
            end else
              Pos := Length(CurLineText);
          end;
          Inc(i);
        end;
      end else begin {Delta < 0}
        i := 1;
        while i <= Abs(Delta) do begin
          if Pos-1 >= 0 then
            Pos := Pos-1
          else begin
            if Line -1 >= 0 then begin
              Line := Line-1;
              CurLineText := GetLineInternal(Line);
              if CurLineText[Length(CurLineText)] = #10 then
                Pos := Length(CurLineText)-2
              else
                Pos := Length(CurLineText)-1;
            end;
          end;
          Inc(i);
        end;
      end;
    end;
  Result := LNewPos;
end;

procedure TvgMemo.RestorePositions;
begin
  if FOldCaretPos >= 0 then
    CaretPosition := TextPosToPos(FOldCaretPos);
  if FSelected and (FOldSelStartPos >= 0) then begin
    FSelStart := TextPosToPos(FOldSelStartPos);
    FSelEnd := TextPosToPos(FOldSelEndPos);
    FOldSelStartPos := -1;
  end;
end;

procedure TvgMemo.StorePositions;
begin
  FOldCaretPos := PosToTextPos(CaretPosition);
  if FSelected then begin
    FOldSelStartPos := PosToTextPos(FSelStart);
    FOldSelEndPos := PosToTextPos(FSelEnd);
  end;
end;

procedure TvgMemo.MoveCaretVertical(LineDelta: integer);
var
  NewLine, NewY, OldX : integer;
begin
  with FCaretPosition do
  begin
    NewLine := Line+LineDelta;
    if NewLine < 0 then
      NewLine := 0
    else
      if NewLine > Lines.Count -1 then
        NewLine := Lines.Count -1;

    NewY := Round(GetPositionPoint(ComposeCaretPos(NewLine,Pos)).Y);
    OldX := Round(GetPositionPoint(CaretPosition).X);
    Line := NewLine;
    Pos := Round(GetPointPosition(vgPoint(OldX,NewY)).Pos);
  end;
end;

procedure TvgMemo.MoveCaretDown;
begin
  MoveCaretVertical(1);
end;

procedure TvgMemo.MoveCaretUp;
begin
  MoveCaretVertical(-1);
end;

procedure TvgMemo.MoveCaretPageDown;
begin
  MoveCaretVertical(round(GetPageSize));
end;

procedure TvgMemo.MoveCaretPageUp;
begin
  MoveCaretVertical(-round(GetPageSize));
end;

function TvgMemo.GetWidestLine: integer;
var
  i : integer;
  LWidth, LMaxWidth : single;
begin
  Result := -1;
  LMaxWidth := -1;
  for i := 0 to Lines.Count-1 do
  begin
    LWidth := LineWidth[i];
    if LWidth > LMaxWidth then
    begin
      Result := i;
      LMaxWidth := LWidth;
    end;
  end;
end;

function TvgMemo.GetShowSelection: boolean;
begin
  Result := IsFocused or not HideSelection;
end;

procedure TvgMemo.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: boolean);
begin
  inherited;
  if (VScrollBar <> nil) and (VScrollBar.Visible) then
  begin
    VScrollBar.Value := VScrollBar.Value - (WheelDelta / 30);
  end;
end;

function TvgMemo.GetData: Variant;
begin
  Result := Text;
end;

procedure TvgMemo.SetData(const Value: Variant);
begin
  Text := Value;
end;

procedure TvgMemo.SetFont(const Value: TvgFont);
begin
  FFont.Assign(Value);
end;

procedure TvgMemo.SetTextAlign(const Value: TvgTextAlign);
begin
  FTextAlign := Value;
end;

procedure TvgMemo.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating then
    Realign;
end;

function TvgMemo.GetUnwrapLines: TvgWideStrings;
begin
  if FUnwrapLines = nil then
    FUnwrapLines := TvgWideStringList.Create;
  FUnwrapLines.Text := FText;
  Result := FUnwrapLines;
end;

{ TEdtActionStack }

constructor TEdtActionStack.Create(AOwner: TvgMemo);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TEdtActionStack.Destroy;
var
  TmpItem : PEdtAction;
begin
  while AtLeast(1) do begin
    TmpItem := Pop;
    Finalize(TmpItem^);
    FreeMem(TmpItem);
  end;
  inherited;
end;

procedure TEdtActionStack.FragmentDeleted(StartPos: integer;
  Fragment: WideString);
var
  TmpItem : PEdtAction;
begin
  if Fragment = '' then Exit;

  if (not AtLeast(1)) or
      not ((PEdtAction(Peek)^.ActionType=atDelete) and
           (PEdtAction(Peek)^.StartPosition-StartPos-Length(Fragment) <= 1) and
           (PEdtAction(Peek)^.StartPosition-StartPos >= 0)) then
  begin
    New(TmpItem);
    Initialize(TmpItem^);
    Push(TmpItem);

    with TmpItem^ do begin
      ActionType := atDelete;
      StartPosition := StartPos;
      DeletedFragment := Fragment;
      PairedWithPriv := false;
    end;
  end
  else
    case PEdtAction(Peek)^.ActionType of
      atDelete : begin
        if StartPos > 0 then begin
          if StartPos < PEdtAction(Peek)^.StartPosition then
            PEdtAction(Peek)^.DeletedFragment := Fragment+PEdtAction(Peek)^.DeletedFragment
          else
            PEdtAction(Peek)^.DeletedFragment := PEdtAction(Peek)^.DeletedFragment+Fragment;
          PEdtAction(Peek)^.StartPosition := StartPos;
        end;
      end;
    end;
end;

procedure TEdtActionStack.FragmentInserted(StartPos, FragmentLength: integer; IsPairedWithPriv : boolean);
var
  TmpItem : PEdtAction;
begin
  if FragmentLength = 0 then Exit;

  if (not AtLeast(1)) or
      not ((PEdtAction(Peek)^.ActionType=atInsert) and
           (PEdtAction(Peek)^.StartPosition+PEdtAction(Peek)^.Length =StartPos)) then
  begin
    New(TmpItem);
    Initialize(TmpItem^);
    Push(TmpItem);
    with TmpItem^ do begin
      ActionType := atInsert;
      StartPosition := StartPos;
      Length := FragmentLength;
      PairedWithPriv := IsPairedWithPriv;
    end;
  end else
    case PEdtAction(Peek)^.ActionType of
      atInsert : PEdtAction(Peek)^.Length := PEdtAction(Peek)^.Length+FragmentLength;
    end;
end;

procedure TEdtActionStack.CaretMovedBy(Shift: integer);
begin

end;

function TEdtActionStack.RollBackAction: boolean;
var
  TmpItem : PEdtAction;
  WasPaired : boolean;
  LTmpOptions : TInsertOptions;
begin
  Result := AtLeast(1);
  if not(Result and Assigned(FOwner)) then Exit;

  repeat
    TmpItem := Pop;

    with TmpItem^, FOwner do begin
      if DeletedFragment<>#13+#10 then
        LTmpOptions := [ioSelected]
      else
        LTmpOptions := [];

      case ActionType of
        atDelete : InsertAfter(TextPosToPos(StartPosition-1),DeletedFragment,LTmpOptions+[ioMoveCaret]{DeletedFragment<>#13+#10, true, false, false});
        atInsert : DeleteFrom(TextPosToPos(StartPosition),Length,[doMoveCaret]);
      end;
    end;

    WasPaired := TmpItem^.PairedWithPriv;
    Finalize(TmpItem^);
    Dispose(TmpItem);
  until (not AtLeast(1)) or (not WasPaired);
end;

initialization
  RegisterVGObjects('Text Edits', [TvgMemo]);
  RegisterVGObjects('HUD', [TvgHudMemo]);
end.
