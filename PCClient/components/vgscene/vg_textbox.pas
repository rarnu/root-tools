unit vg_textbox;

{$I vg_define.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFNDEF NOVCL} Controls, Menus, Clipbrd, {$ENDIF}
  {$IFDEF LCL} LCLType, {$ENDIF}
  Variants,
  Classes, SysUtils, StrUtils, vg_scene, vg_objects, vg_controls, vg_listbox;

type

  TvgCustomTextBox = class(TvgControl)
  private
    FText: WideString;
    FFontFill: TvgBrush;
    FFont: TvgFont;
    FTextAlign: TvgTextAlign;
    FOnChange: TNotifyEvent;
    FReadOnly: boolean;
    FSelStart: integer;
    FSelLength: integer;
    FCaretPosition: integer;
    FMaxLength: integer;
    FFirstVisibleChar: integer;
    FLMouseSelecting: boolean;
    FDisableCaret: boolean;
    FPassword: boolean;
    {$IFNDEF NOVCL}
    FPopupMenu: TPopupMenu;
    {$ENDIF}
    FOnTyping: TNotifyEvent;
    FSelectionFill: TvgBrush;
    FOnChangeTracking: TNotifyEvent;
    procedure InsertText(const AText: WideString);
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: WideString;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelRect: TvgRect;
    procedure SetCaretPosition(const Value: integer);
    function GetCoordinatePosition(x: single): integer;
    procedure SetMaxLength(const Value: Integer);
    function GetNextWordBeging(StartPosition: integer): integer;
    function GetPrivWordBeging(StartPosition: integer): integer;
    procedure UpdateFirstVisibleChar;
    procedure UpdateCaretePosition;
    procedure SetPassword(const Value: boolean);
    procedure CreatePopupMenu;
    procedure DoCopy(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure UpdatePopupMenuItems;
    procedure DoSelectAll(Sender: TObject);
    procedure SetFont(const Value: TvgFont);
    procedure SetTextAlign(const Value: TvgTextAlign);
  protected
    FNeedChange: boolean;
    FFilterChar: WideString;
    FShowCaret: boolean;
    FLastKey: Word;
    FLastChar: System.WideChar;
    procedure ApplyStyle; override;
    procedure Change; virtual;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    function GetPasswordCharWidth: single;
    function TextWidth(const Str: WideString): single;
    procedure SetText(const Value: WideString); virtual;
    procedure FontChanged(Sender: TObject); virtual;
    procedure DoContentPaint(Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure EnterFocus; override;
    procedure KillFocus; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure ContextMenu(const ScreenPosition: TvgPoint); override;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    function GetCharX(a: integer): single;
    function ContentRect: TvgRect;
    property CaretPosition: integer read FCaretPosition write SetCaretPosition;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ShowCaret: boolean read FShowCaret write FShowCaret default true;
    property FontFill: TvgBrush read FFontFill;
    property SelectionFill: TvgBrush read FSelectionFill;
    property Password: boolean read FPassword write SetPassword;
    property Text: WideString read FText write SetText;
    property FilterChar: WideString read FFilterChar write FFilterChar;
  published
    property Cursor default crIBeam;
    property CanFocused default true;
    property DisableFocusEffect;
    property TabOrder;
    property Font: TvgFont read FFont write SetFont;
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign default vgTextAlignNear;
    property Resource;
    property BindingSource;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeTracking: TNotifyEvent read FOnChangeTracking write FOnChangeTracking;
    property OnTyping: TNotifyEvent read FOnTyping write FOnTyping;
  end;

  TvgTextBox = class(TvgCustomTextBox)
  private
  protected
  public
  published
    property Password;
    property Text;
  end;

  TvgValueType = (
    vgValueInteger,
    vgValueFloat
  );

  TvgNumberBox = class(TvgCustomTextBox)
  private
    FValue: single;
    FMin: single;
    FMax: single;
    FPressed: boolean;
    FPressedPos: TvgPoint;
    FPressedVert: boolean;
    FPressedInc: single;
    FValueType: TvgValueType;
    FHorzIncrement: single;
    FVertIncrement: single;
    FDecimalDigits: integer;
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetValue(const AValue: single);
    procedure SetValueType(const Value: TvgValueType);
    procedure SetDecimalDigits(const Value: integer);
  protected
    procedure Change; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure SetText(const Value: WideString); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure PaintChildren; override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    property DecimalDigits: integer read FDecimalDigits write SetDecimalDigits default 2;
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property ShowCaret default true;
    property Value: single read FValue write SetValue;
    property ValueType: TvgValueType read FValueType write SetValueType;
    property HorzIncrement: single read FHorzIncrement write FHorzIncrement;
    property VertIncrement: single read FVertIncrement write FVertIncrement;
  end;

  TvgSpinBox = class(TvgCustomTextBox)
  private
    FValue: single;
    FMin: single;
    FMax: single;
    FValueType: TvgValueType;
    FMinus, FPlus: TvgCustomButton;
    FIncrement: single;
    FDecimalDigits: integer;
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetValue(const AValue: single);
    procedure SetValueType(const Value: TvgValueType);
    procedure SetDecimalDigits(const Value: integer);
  protected
    procedure SetText(const Value: WideString); override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Change; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure DoMinusClick(Sender: TObject);
    procedure DoPlusClick(Sender: TObject);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DecimalDigits: integer read FDecimalDigits write SetDecimalDigits default 2;
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Increment: single read FIncrement write FIncrement;
    property ShowCaret default true;
    property Value: single read FValue write SetValue;
    property ValueType: TvgValueType read FValueType write SetValueType;
    property TextAlign default vgTextAlignCenter;
  end;

  TvgComboTextBox = class(TvgCustomTextBox)
  private
    FDropDownCount: integer;
    FPopup: TvgPopup;
    FListBox: TvgComboListBox;
    FPlacement: TvgPlacement;
    FItems: TvgWideStrings;
    FItemHeight: single;
    procedure DoItemsChanged(Sender: TObject);
    procedure RebuildList;
    procedure SetItemHeight(const Value: single);
    procedure SetItems(const Value: TvgWideStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
    function GetCount: integer;
    function GetListBoxResource: string;
    procedure SetListBoxResource(const Value: string);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure ChangeParent; override;
    procedure DoTyping(Sender: TObject);
    procedure DoClosePopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
    procedure DropDown;
    property ListBox: TvgComboListBox read FListBox write FListBox;
    property Count: integer read GetCount;
  published
    property Cursor default crDefault;
    property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Items: TvgWideStrings read FItems write SetItems;
    property ListBoxResource: string read GetListBoxResource write SetListBoxResource;
    property Text;
  end;

  TvgComboTrackBar = class(TvgCustomTextBox)
  private
    FPopup: TvgPopup;
    FTrackBar: TvgTrackBar;
    FPlacement: TvgPlacement;
    function GetFrequency: single;
    function GetMax: single;
    function GetMin: single;
    procedure SetFrequency(const Value: single);
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetValue(const AValue: single);
    function GetValue: single;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure ChangeParent; override;
    procedure DoTrackChange(Sender: TObject); virtual;
    procedure DropDown;
    procedure DoClosePopup(Sender: TObject);
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TrackBar: TvgTrackBar read FTrackBar write FTrackBar;
  published
    property Min: single read GetMin write SetMin;
    property Max: single read GetMax write SetMax;
    property Value: single read GetValue write SetValue;
    property Frequency: single read GetFrequency write SetFrequency;
    property Text stored false;
  end;

  TvgTextBoxClearBtn = class(TvgCustomTextBox)
  private
    FClearBtn: TvgCustomButton;
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoClearBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Password;
    property Text;
  end;

  TvgRoundTextBox = class(TvgTextBox)
  private
  protected
  public
  published
  end;

  TvgHudTextBox = class(TvgTextBox)
  private
  protected
  public
  published
  end;

  TvgHudRoundTextBox = class(TvgTextBox)
  private
  protected
  public
  published
  end;

  TvgHudNumberBox = class(TvgNumberBox)
  private
  protected
  public
  published
  end;

  TvgHudSpinBox = class(TvgSpinBox)
  private
  protected
  public
  published
  end;

  TvgHudComboTextBox = class(TvgComboTextBox)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TvgHudComboTrackBar = class(TvgComboTrackBar)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation {===============================================================}

uses vg_ani;

{ TvgCustomTextBox ==================================================================}

constructor TvgCustomTextBox.Create(AOwner: TComponent);
begin
  inherited;
  FShowCaret := true;
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
  FFontFill := TvgBrush.Create(vgBrushSolid, $FF000000);
  FSelectionFill := TvgBrush.Create(vgBrushSolid, $802A8ADF);
  CanFocused := true;
  Cursor := crIBeam;
  TextAlign := vgTextAlignNear;
  AutoCapture := true;
  Width := 100;
  Height := 22;
  FCaretPosition := 0;
  FSelStart := 0;
  FSelLength := 0;
  FFirstVisibleChar := 1;
  CreatePopupMenu;
end;

destructor TvgCustomTextBox.Destroy;
begin
  FSelectionFill.Free;
  FFontFill.Free;
  FFont.Free;
  {$IFNDEF NOVCL}
  FPopupMenu.Free;
  {$ENDIF}
  inherited;
end;

procedure TvgCustomTextBox.CreatePopupMenu;
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

procedure TvgCustomTextBox.DoSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TvgCustomTextBox.DoCut(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TvgCustomTextBox.DoCopy(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TvgCustomTextBox.DoDelete(Sender: TObject);
begin
  ClearSelection;
end;

procedure TvgCustomTextBox.DoPaste(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TvgCustomTextBox.UpdatePopupMenuItems;
var
  SelTextEmpty: boolean;
begin
  SelTextEmpty := SelText <> '';
  {$IFNDEF NOVCL}
  FPopupMenu.Items.Find('Cut').Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find('Copy').Enabled := SelTextEmpty;
  FPopupMenu.Items.Find('Paste').Enabled := (Clipboard.AsText <> '') and not ReadOnly;
  FPopupMenu.Items.Find('Delete').Enabled := SelTextEmpty and not ReadOnly;
  FPopupMenu.Items.Find('Select All').Enabled := SelText <> Text;
  {$ENDIF}
end;

function TvgCustomTextBox.GetData: Variant;
begin
  Result := FText;
end;

procedure TvgCustomTextBox.SetData(const Value: Variant);
begin
  if VarIsNull(Value) then
    Text := ''
  else
  if VarIsType(Value, varDate) then
    Text := DateTimeToStr(VarToDateTime(Value))
  else
  if VarIsEvent(Value) then
    OnChange := VariantToEvent(Value)
  else
    Text := VarToWideStr(Value);
end;

procedure TvgCustomTextBox.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('content');
  if (T <> nil) and (T is TvgContent) then
  begin
    TvgContent(T).OnPaint := DoContentPaint;
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

function TvgCustomTextBox.ContentRect: TvgRect;
var
  T: TvgObject;
begin
  T := FindResource('content');
  if (T <> nil) and (T.IsVisual) then
  begin
    Result := TvgVisualObject(T).ParentedRect;
  end
  else
  begin
    Result := LocalRect;
  end;
end;

procedure TvgCustomTextBox.Paint;
begin
  inherited;
end;

procedure TvgCustomTextBox.DoContentPaint(Sender: TObject; const Canvas: TvgCanvas;
  const ARect: TvgRect);
var
  i: integer;
  R: TvgRect;
  SaveIdx: cardinal;
begin
  { draw text }
  if FText = '' then Exit;

  SaveIdx := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(ARect);

  Canvas.Font.Assign(Font);
  Canvas.Fill.Assign(FFontFill);
  if FPassword then
  begin
    R := ARect;
    R.Right := R.Left + GetPasswordCharWidth - 1;
    R.Top := (vgRectHeight(ARect) - vgRectWidth(R)) / 2;
    R.Bottom := R.Top + vgRectWidth(R);
    for i := FFirstVisibleChar to Length(Text) do
    begin
      Canvas.FillEllipse(R, AbsoluteOpacity);
      vgOffsetRect(R, vgRectWidth(R) + 1, 0);
    end;
  end
  else
  begin
    R := ARect;
    if Textalign = vgTextAlignNear then
      R.Right := R.Right + (Font.Size / 2); // disable GDI+ clippping
    Canvas.FillText(R, R, Copy(Text, FFirstVisibleChar, Length(Text) - FFirstVisibleChar + 1),
      false, AbsoluteOpacity, TextAlign, vgTextAlignCenter);
  end;
  { carret }
  if IsFocused then
  begin
    { selection }
    if SelLength > 0 then
    begin
      Canvas.Fill.Assign(FSelectionFill);
      R := GetSelRect;
      with ContentRect do
        vgOffsetRect(R, -Left, -Top);
      Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    end;
  end;

  Canvas.RestoreCanvas(SaveIdx);
end;

procedure TvgCustomTextBox.InsertText(const AText: WideString);
var
  TmpS: WideString;
begin
  if ReadOnly then Exit;

  TmpS := Text;
//  FActionStack.FragmentDeleted(SelStart + 1, Copy(TmpS, SelStart+1, SelLength));
  Delete(TmpS, SelStart + 1, SelLength);
//  FActionStack.FragmentInserted(SelStart + 1, Length(AText), SelLength <> 0);
  Insert(AText, TmpS, SelStart + 1);
  if (MaxLength <= 0) or (Length(TmpS) <= MaxLength) then
  begin
    Text := TmpS;
    CaretPosition := SelStart + Length(AText);
  end;
  SelLength := 0;
end;

procedure TvgCustomTextBox.UpdateFirstVisibleChar;
var
  LEditRect: TvgRect;
begin
  if FFirstVisibleChar >= (FCaretPosition + 1) then
  begin
    FFirstVisibleChar := FCaretPosition;
    if FFirstVisibleChar < 1 then
      FFirstVisibleChar := 1;
  end
  else
  begin
    LEditRect := ContentRect;

    if Password then
      while ((FCaretPosition - FFirstVisibleChar + 1) * GetPasswordCharWidth > LEditRect.Right - LEditRect.Left) and (FFirstVisibleChar < Length(Text)) do
        Inc(FFirstVisibleChar)
    else
    begin
      while (TextWidth(Copy(Text, FFirstVisibleChar, FCaretPosition - FFirstVisibleChar + 1)) > LEditRect.Right - LEditRect.Left) and (FFirstVisibleChar < Length(Text)) do
      begin
        if TextWidth(Copy(Text, FFirstVisibleChar + 500, (FCaretPosition - FFirstVisibleChar + 500) + 1)) > LEditRect.Right - LEditRect.Left then
          Inc(FFirstVisibleChar, 500)
        else
          if TextWidth(Copy(Text, FFirstVisibleChar + 100, (FCaretPosition - FFirstVisibleChar + 100) + 1)) > LEditRect.Right - LEditRect.Left then
            Inc(FFirstVisibleChar, 100)
          else
            if TextWidth(Copy(Text, FFirstVisibleChar + 50, (FCaretPosition - FFirstVisibleChar + 100) + 1)) > LEditRect.Right - LEditRect.Left then
              Inc(FFirstVisibleChar, 50)
            else
              if TextWidth(Copy(Text, FFirstVisibleChar + 10, (FCaretPosition - FFirstVisibleChar + 10) + 1)) > LEditRect.Right - LEditRect.Left then
                Inc(FFirstVisibleChar, 10)
              else
                Inc(FFirstVisibleChar);
      end;
    end;
  end;
  Repaint;
end;

procedure TvgCustomTextBox.UpdateCaretePosition;
begin
  SetCaretPosition(CaretPosition);
end;

function TvgCustomTextBox.GetPasswordCharWidth: single;
begin
  Result := Font.Size / 2;
end;

function TvgCustomTextBox.TextWidth(const Str: WideString): single;
var
  R: TvgRect;
begin
  R := ContentRect;
  R.Right := 10000;
  Canvas.Font.Assign(Font);
  if FPassword then
  begin
    R.Right := R.Left + GetPasswordCharWidth * Length(Str);
  end
  else
    Canvas.MeasureText(R, R, Str, false, vgTextAlignNear, vgTextAlignCenter);
  Result := vgRectWidth(R);
end;

function TvgCustomTextBox.GetCoordinatePosition(x: single): integer;
var
  CurX: double;
  TmpX,
  WholeTextWidth,
  EditRectWidth : single;
  Str, StrA: WideString;
begin
  Result := FFirstVisibleChar - 1;
  if Length(Text) = 0 then
    Exit;

  if FPassword then
    WholeTextWidth := Length(Text) * GetPasswordCharWidth
  else
    WholeTextWidth := TextWidth(Copy(Text, 1, Length(Text)));

  EditRectWidth := ContentRect.Right - ContentRect.Left;
  TmpX := x;
  if WholeTextWidth < EditRectWidth then
    case TextAlign of
      vgTextAlignFar: TmpX := x - (EditRectWidth - WholeTextWidth);
      vgTextAlignCenter: TmpX := x - ((EditRectWidth - WholeTextWidth) / 2);
    end;

  if FPassword then
  begin
    Result := Result + Trunc((TmpX - ContentRect.Left) / GetPasswordCharWidth);
    if Result < 0 then
      Result := 0
    else
      if Result > Length(Text) then
        Result := Length(Text);
  end
  else
  begin
    TmpX := TmpX - ContentRect.Left;
    StrA := System.Copy(Text, FFirstVisibleChar, Result - FFirstVisibleChar + 1);
    Str := System.Copy(Text, FFirstVisibleChar, Result - FFirstVisibleChar + 2);
    while (TextWidth(StrA) < TmpX) and (Result < Length(Text)) do
    begin
      if (TmpX > TextWidth(StrA) + ((TextWidth(Str) - TextWidth(StrA)) / 2)) and (TmpX < TextWidth(Str)) then
      begin
        Result := Result + 1;
        Break;
      end;
      if TmpX < TextWidth(Str) then Break;
      Result := Result + 1;
      StrA := Str;
      Str := Copy(Text, FFirstVisibleChar, Result - FFirstVisibleChar + 2);
    end;
  end;
end;

function TvgCustomTextBox.GetCharX(a: integer): single;
var
  WholeTextWidth: single;
  EditRectWidth: single;
  R: TvgRect;
  T: WideString;
begin
  if FPassword then
  begin
    WholeTextWidth := Length(Text) * GetPasswordCharWidth;
    Result := ContentRect.Left;
    if a > 0 then
    begin
      if FPassword then
      begin
        if a <= Length(Text) then
          Result := Result + (a - FFirstVisibleChar + 1) * GetPasswordCharWidth
        else
          Result := Result + (Length(Text) - FFirstVisibleChar + 1) * GetPasswordCharWidth;
      end
    end;
    EditRectWidth := ContentRect.Right - ContentRect.Left;
    if WholeTextWidth < EditRectWidth then
      case TextAlign of
        vgTextAlignFar : Result := Result + (EditRectWidth - WholeTextWidth);
        vgTextAlignCenter: Result := Result + ((EditRectWidth - WholeTextWidth) / 2);
      end;
    Exit;
  end;

  R := ContentRect;
  Canvas.Font.Assign(Font);
  T := Text;
  if Text = '' then
    T := 'a';
  Canvas.MeasureText(R, R, T, false, vgTextAlignNear, vgTextAlignCenter);
  WholeTextWidth := R.Right - ContentRect.Left;
  Result := ContentRect.Left;

  if a > 0 then
  begin
    if a <= Length(Text) then
    begin
      R := ContentRect;
      Canvas.MeasureText(R, R, Copy(Text, FFirstVisibleChar, a - FFirstVisibleChar + 1), false, vgTextAlignNear, vgTextAlignCenter);
      Result := R.Right;
    end
    else
    begin
      R := ContentRect;
    end;
  end;

  EditRectWidth := ContentRect.Right - ContentRect.Left;
  if WholeTextWidth < EditRectWidth then
    case TextAlign of
      vgTextAlignFar: Result := Result + (EditRectWidth - WholeTextWidth);
      vgTextAlignCenter: Result := Result + ((EditRectWidth - WholeTextWidth) / 2);
    end;
end;

function TvgCustomTextBox.GetNextWordBeging(StartPosition: integer): integer;
var
  SpaceFound, WordFound: boolean;
begin
  Result := StartPosition;
  SpaceFound := false;
  WordFound := false;
  while (Result + 2 <= Length(Text)) and
    ((not ((Text[Result + 1] <> vgWideSpace) and SpaceFound))
    or not WordFound) do
  begin
    if Text[Result + 1] = vgWideSpace then
      SpaceFound := true;
    if Text[Result + 1] <> vgWideSpace then
    begin
      WordFound := true;
      SpaceFound := false;
    end;

    Result := Result + 1;
  end;
  if not SpaceFound then
    Result := Result + 1;
end;

function TvgCustomTextBox.GetPrivWordBeging(StartPosition: integer): integer;
var
  WordFound: boolean;
begin
  Result := StartPosition;
  WordFound := false;
  while (Result > 0) and
    ((Text[Result] <> vgWideSpace) or not WordFound) do
  begin
    if Text[Result] <> vgWideSpace then
      WordFound := true;
    Result := Result - 1;
  end;
end;

function TvgCustomTextBox.GetSelStart: integer;
begin
  if FSelLength > 0 then
    Result := FSelStart
  else
    if FSelLength < 0 then
      Result := FSelStart + FSelLength
    else
      Result := CaretPosition;
end;

function TvgCustomTextBox.GetSelRect: TvgRect;
begin
  Result := ContentRect;
  Result.Left := GetCharX(SelStart);
  Result.Right := GetCharX(SelStart + SelLength) + 1;
end;

function TvgCustomTextBox.GetSelLength: integer;
begin
  Result := Abs(FSelLength);
end;

function TvgCustomTextBox.GetSelText: WideString;
begin
  Result := Copy(Text, SelStart + 1, SelLength);
end;

procedure TvgCustomTextBox.SetSelLength(const Value: integer);
begin
  if FSelLength <> Value then
  begin
    FSelLength := Value;
    Repaint;
  end;
end;

procedure TvgCustomTextBox.SetSelStart(const Value: integer);
begin
  if FSelStart <> Value then
  begin
    SelLength := 0;
    FSelStart := Value;
    CaretPosition := FSelStart;
    Repaint;
  end;
end;

procedure TvgCustomTextBox.SetCaretPosition(const Value: integer);
begin
  if Value < 0 then
    FCaretPosition := 0
  else
    if Value > Length(Text) then
      FCaretPosition := Length(Text)
    else
      FCaretPosition := Value;

  UpdateFirstVisibleChar;

  if SelLength <= 0 then
    FSelStart := Value;

  Repaint;
  if IsFocused and FShowCaret then
  begin
    SetCaretSize(vgPoint(2, (Font.Size * 1.25)));
    SetCaretPos(vgPoint(GetCharX(FCaretPosition) - 1, (ContentRect.Top + ContentRect.Bottom - (Font.Size * 1.25)) / 2));
    SetCaretColor(FFontFill.SolidColor);
  end;
end;

procedure TvgCustomTextBox.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
  end;
end;

procedure TvgCustomTextBox.CopyToClipboard;
var
  Data: THandle;
  DataPtr: Pointer;
  Size: Cardinal;
  S: WideString;
begin
  {$IFNDEF NOVCL}
  {$IFNDEF FPC}
//  if PasswordKind = pkNone then
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

procedure TvgCustomTextBox.PasteFromClipboard;
var
  Data: THandle;
  Insertion: WideString;
begin
  if ReadOnly then Exit;
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

  InsertText(Insertion);
  {$ELSE}
  InsertText(UTF8Decode(Clipbrd.Clipboard.AsText));
  {$ENDIF}
  {$ENDIF}
end;

procedure TvgCustomTextBox.ClearSelection;
var
  TmpS: WideString;
begin
  if ReadOnly then Exit;

  TmpS := Text;
//  FActionStack.FragmentDeleted(SelStart+1, Copy(TmpS,SelStart+1,SelLength));
  Delete(TmpS, SelStart + 1, SelLength);
  CaretPosition := SelStart;
  Text := TmpS;
  SelLength := 0;
end;

procedure TvgCustomTextBox.CutToClipboard;
begin
//  if PasswordKind = pkNone then
    CopyToClipboard;
  ClearSelection;
end;

procedure TvgCustomTextBox.SelectAll;
begin
  SelStart := 0;
  SelLength := Length(Text);
  SetCaretPosition(Length(Text));
  Repaint;
end;

procedure TvgCustomTextBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  S: wideString;
  TmpS: WideString;
  OldCaretPosition: integer;
begin
  inherited ;
  OldCaretPosition := CaretPosition;
  FLastChar := KeyChar;
  FLastKey := Key;
  case Key of
    VK_RETURN: Change;
    VK_END: CaretPosition := Length(Text);
    VK_HOME: CaretPosition := 0;
    VK_LEFT:
      if ssCtrl in Shift then
        CaretPosition := GetPrivWordBeging(CaretPosition)
      else
        CaretPosition := CaretPosition - 1;
    VK_RIGHT:
      if ssCtrl in Shift then
        CaretPosition := GetNextWordBeging(CaretPosition)
      else
        CaretPosition := CaretPosition + 1;
    VK_DELETE, 8: {Delete or BackSpace key was pressed}
      if not ReadOnly then
      begin
        if SelLength <> 0 then
        begin
          if Shift = [ssShift] then
            CutToClipboard
          else
            ClearSelection;
        end
        else
        begin
          TmpS := Text;
          if TmpS <> '' then
            if Key = VK_DELETE then
            begin
//              FActionStack.FragmentDeleted(CaretPosition + 1,TmpS[CaretPosition + 1]);
              Delete(TmpS, CaretPosition + 1, 1);
            end
            else
            begin {BackSpace key was pressed}
{              if CaretPosition > 0 then
                FActionStack.FragmentDeleted(CaretPosition,TmpS[CaretPosition]);}
              Delete(TmpS, CaretPosition, 1);
              CaretPosition := CaretPosition - 1;
            end;
          Text := TmpS;
          if Assigned(FOnTyping) then FOnTyping(Self);
        end;
      end;
    VK_INSERT:
      if Shift = [ssCtrl] then
      begin
        CopyToClipboard;
      end
      else
        if Shift = [ssShift] then
        begin
          PasteFromClipboard;
          if Assigned(FOnTyping) then FOnTyping(Self);
        end;
  end;

  if (KeyChar <> #0) and (FFilterChar <> '') and (Pos(KeyChar, FFilterChar) = 0) then
  begin
    KeyChar := #0;
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
        if Assigned(FOnTyping) then FOnTyping(Self);
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

  if Key in [VK_END, VK_HOME, VK_LEFT, VK_RIGHT] then
  begin
    if ssShift in Shift then
    begin
      if SelLength = 0 then
        FSelStart := OldCaretPosition;
      FSelStart := CaretPosition;
      FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
    end
    else
      FSelLength := 0;
    Repaint;
  end;

  if (Ord(KeyChar) >= 32) and not ReadOnly then
  begin
    S := KeyChar;
    InsertText(S);
    if Assigned(FOnTyping) then FOnTyping(Self);
  end;
  if FResourceLink <> nil then
    FResourceLink.Visual.UpdateEffects;
  UpdateCaretePosition;
end;

procedure TvgCustomTextBox.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  inherited ;
end;

procedure TvgCustomTextBox.DblClick;
begin
  inherited;
  SelectAll;
end;

procedure TvgCustomTextBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  OldFocused: boolean;
begin
  OldFocused := IsFocused;
  inherited;
  if Button = mbLeft then
    FLMouseSelecting := true;

  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    if OldFocused then // clear only already focused
    begin
      CaretPosition := GetCoordinatePosition(x);
      SelLength := 0;
    end;
  end;
end;

procedure TvgCustomTextBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  OldCaretPosition: integer;
  TmpNewPosition : integer;
begin
  inherited;
  if FLMouseSelecting then
  begin
    TmpNewPosition := GetCoordinatePosition(x);
    OldCaretPosition := CaretPosition;
    if (x > ContentRect.Right) then
      CaretPosition := TmpNewPosition +1
    else
      CaretPosition := TmpNewPosition;
    if SelLength = 0 then
      FSelStart := OldCaretPosition;
    FSelStart := CaretPosition;
    FSelLength := FSelLength - (CaretPosition - OldCaretPosition);
  end;
end;

procedure TvgCustomTextBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  FLMouseSelecting := false;
end;

procedure TvgCustomTextBox.Change;
begin
  if FNeedChange then
  begin
    if Assigned(FBindingObjects) then
      ToBindingObjects;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgCustomTextBox.ContextMenu(const ScreenPosition: TvgPoint);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if Popup <> nil then Exit;

  UpdatePopupMenuItems;
  {$IFNDEF NOVCL}
  FPopupMenu.PopupComponent := Self;
  FPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
  {$ENDIF}
end;

procedure TvgCustomTextBox.EnterFocus;
begin
  inherited;
  FNeedChange := false;
  if FShowCaret then
    ShowCaretProc;
  if FScene.ShowKeyboardForControl(Self) then
    CaretPosition := Length(Text)
  else
    SelectAll;
end;

procedure TvgCustomTextBox.KillFocus;
begin
  if not Assigned(FScene) then
  begin
    inherited ;
    Exit;
  end;
  FScene.HideKeyboardForControl(Self);
  if FShowCaret then
    HideCaret;
  inherited ;
  Change;
end;

procedure TvgCustomTextBox.SetText(const Value: WideString);
begin
  if FText <> Value then
  begin
    FText := Value;
    if FCaretPosition > Length(Text) then
      SetCaretPosition(Length(Text));
    if not (csLoading in ComponentState) and Assigned(OnChangeTracking) then
      OnChangeTracking(Self);
    FNeedChange := true;
    Repaint;
  end;
end;

procedure TvgCustomTextBox.SetPassword(const Value: boolean);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    Repaint;
  end;
end;

procedure TvgCustomTextBox.SetFont(const Value: TvgFont);
begin
  FFont.Assign(Value);
end;

procedure TvgCustomTextBox.SetTextAlign(const Value: TvgTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    Repaint;
  end;
end;

procedure TvgCustomTextBox.FontChanged(Sender: TObject);
begin
  Repaint;
end;

{ TvgNumberBox ================================================================}

constructor TvgNumberBox.Create(AOwner: TComponent);
begin
  inherited;
  FDecimalDigits := 2;
  FFilterChar := '0123456789.,-+';
  Max := 10;
  VertIncrement := 5;
  HorzIncrement := 1;
  Text := '0';
  Value := 0;
  AutoCapture := true;
end;

destructor TvgNumberBox.Destroy;
begin
  inherited;
end;

function TvgNumberBox.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgNumberBox.SetData(const Value: Variant);
begin
  if VarIsEvent(Value) then
    OnChange := VariantToEvent(Value)
  else
    Self.Value := Value;
end;

procedure TvgNumberBox.Change;
begin
  try
    FValue := StrToFloat(Text);
    if FValue > FMax then FValue := FMax;
    if FValue < FMin then FValue := FMin;
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
  except
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
  end;
  Repaint;
  inherited;
end;

procedure TvgNumberBox.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  case Key of
    VK_UP: Value := Value + HorzIncrement;
    VK_DOWN: Value := Value - HorzIncrement;
  else
    inherited;
    Exit;
  end;
  Key := 0;
end;

procedure TvgNumberBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressed := true;
    FPressedPos := vgPoint(X, Y);
    FPressedVert := false;
    FPressedInc := 0;
  end;
end;

procedure TvgNumberBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if FPressed then
  begin
    if Abs(X - FPressedPos.X) >= Abs(Y - FPressedPos.Y) then
    begin
      { horz }
      if X > FPressedPos.X then
        Value := Value + HorzIncrement
      else
        Value := Value - HorzIncrement;
      FPressedInc := X - FPressedPos.X;
      FPressedVert := false;
    end
    else
    begin
      { vert }
      if Y < FPressedPos.Y then
        Value := Value + VertIncrement
      else
        Value := Value - VertIncrement;
      FPressedInc := X - FPressedPos.X;
      FPressedVert := true;
    end;
    FNeedChange := true;
    FPressedPos := vgPoint(X, Y);
  end;
end;

procedure TvgNumberBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FPressed then
  begin
    FPressed := false;
    Change;
    Repaint;
  end;
end;

procedure TvgNumberBox.Paint;
begin
  inherited ;
end;

procedure TvgNumberBox.PaintChildren;
var
  R: TvgRect;
begin
  if FPressed then
    FDisableCaret := true;
  inherited;
  if FPressed then
  begin
    Canvas.SetMatrix(AbsoluteMatrix);
    Canvas.Fill.Style := vgBrushSolid;
    Canvas.Fill.SolidColor := $AA505050;
    R := LocalRect;
    if FPressedVert then
    begin
      vgInflateRect(R, -1, -1);
      R.Left := R.Right - 5;
      Canvas.FillRect(R, 1, 1, AllCorners, AbsoluteOpacity);
      vgInflateRect(R, -1, -1);
{      if FPressedInc > 0 then
      begin
        Canvas.Fill.SolidColor := $AA202020;
        R.Top := (vgRectHeight(R) / 2);
        R.Bottom := R.Top + (Height / 2.1);
        Canvas.FillRect(R, 1, 1, AbsoluteOpacity);
      end;
      if FPressedInc < 0 then
      begin
        Canvas.Fill.SolidColor := $AA202020;
        R.Bottom := (vgRectHeight(R) / 2);
        R.Top := R.Bottom - (Height / 2.1);
        Canvas.FillRect(R, 1, 1, AbsoluteOpacity);
      end;}
    end
    else
    begin
      vgInflateRect(R, -1, -1);
      R.Top := R.Bottom - 5;
      Canvas.FillRect(R, 1, 1, AllCorners, AbsoluteOpacity);
      vgInflateRect(R, -1, -1);
{      if FPressedInc > 0 then
      begin
        Canvas.Fill.SolidColor := $AA202020;
        R.Left := (vgRectWidth(R) / 2);
        R.Right := R.Left + (Width / 2.1);
        Canvas.FillRect(R, 1, 1, AbsoluteOpacity);
      end;
      if FPressedInc < 0 then
      begin
        Canvas.Fill.SolidColor := $AA202020;
        R.Right := (vgRectWidth(R) / 2);
        R.Left := R.Right - (Width / 2.1);
        Canvas.FillRect(R, 1, 1, AbsoluteOpacity);
      end;}
    end;
  end;
  if FPressed then
    FDisableCaret := false;
end;

procedure TvgNumberBox.SetMax(const Value: single);
begin
  FMax := Value;
end;

procedure TvgNumberBox.SetMin(const Value: single);
begin
  FMin := Value;
end;

procedure TvgNumberBox.SetDecimalDigits(const Value: integer);
begin
  FDecimalDigits := Value;
end;

procedure TvgNumberBox.SetValue(const AValue: single);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    if FValue > FMax then FValue := FMax;
    if FValue < FMin then FValue := FMin;
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := Format('%.' + IntToStr(FDecimalDigits) + 'f', [FValue]);
    SelLength := 0;
    Repaint;
  end;
end;

procedure TvgNumberBox.SetValueType(const Value: TvgValueType);
begin
  FValueType := Value;
end;

procedure TvgNumberBox.SetText(const Value: WideString);
begin
  inherited;
end;

{ TvgSpinBox ==================================================================}

constructor TvgSpinBox.Create(AOwner: TComponent);
begin
  inherited;
  FDecimalDigits := 2;
  FFilterChar := '0123456789.,-+';
  TextAlign := vgTextAlignCenter;
  Increment := 1;
  Max := 10;
  Text := '0';
  Value := 0;
end;

destructor TvgSpinBox.Destroy;
begin
  inherited;
end;

procedure TvgSpinBox.ApplyStyle;
var
  B: TvgObject;
begin
  inherited;
  B := FindResource('minusbutton');
  if (B <> nil) and (B is TvgCustomButton) then
  begin
    FMinus := TvgCustomButton(B);
    FMinus.OnClick := DoMinusClick;
  end;
  B := FindResource('plusbutton');
  if (B <> nil) and (B is TvgCustomButton) then
  begin
    FPlus := TvgCustomButton(B);
    FPlus.OnClick := DoPlusClick;
  end;
end;

procedure TvgSpinBox.FreeStyle;
begin
  inherited;
  FMinus := nil;
  FPlus := nil;
end;

procedure TvgSpinBox.DoMinusClick(Sender: TObject);
begin
  SetFocus;
  Value := Value - Increment;
  FNeedChange := true;
  Change;
end;

procedure TvgSpinBox.DoPlusClick(Sender: TObject);
begin
  SetFocus;
  Value := Value + Increment;
  FNeedChange := true;
  Change;
end;

function TvgSpinBox.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgSpinBox.SetData(const Value: Variant);
begin
  if VarIsEvent(Value) then
    OnChange := VariantToEvent(Value)
  else
    Self.Value := Value;
end;

procedure TvgSpinBox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  case Key of
    VK_UP: Value := Value + Increment;
    VK_DOWN: Value := Value - Increment;
  else
    inherited;
    Exit;
  end;
  Key := 0;
end;

procedure TvgSpinBox.Change;
begin
  try
    FValue := StrToFloat(Text);
    if FValue > FMax then FValue := FMax;
    if FValue < FMin then FValue := FMin;
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
  except
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := FloattoStr(FValue);
  end;
  Repaint;
  inherited;
end;

procedure TvgSpinBox.SetMax(const Value: single);
begin
  FMax := Value;
end;

procedure TvgSpinBox.SetMin(const Value: single);
begin
  FMin := Value;
end;

procedure TvgSpinBox.SetValue(const AValue: single);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    if FValue > FMax then FValue := FMax;
    if FValue < FMin then FValue := FMin;
    if (frac(FValue) = 0) or (FValueType = vgValueInteger) then
      FText := IntToStr(Trunc(FValue))
    else
      FText := Format('%' + DecimalSeparator + IntToStr(FDecimalDigits) + 'f', [FValue]);
    SelLength := 0;
    Repaint;
  end;
end;

procedure TvgSpinBox.SetValueType(const Value: TvgValueType);
begin
  if FValueType <> Value then
  begin
    FValueType := Value;
    if (FValueType = vgValueInteger) and (Frac(Increment) <> 0) then
      Increment := 1;
  end;
end;

procedure TvgSpinBox.SetDecimalDigits(const Value: integer);
begin
  FDecimalDigits := Value;
end;

procedure TvgSpinBox.SetText(const Value: WideString);
begin
  inherited;
end;

{ TvgComboTextBox }

type
  TvgHackComboListBox = class(TvgComboListBox);

constructor TvgComboTextBox.Create(AOwner: TComponent);
begin
  inherited;
  DropDownCount := 8;
  Cursor := crDefault;
  FItemHeight := 19;
  FItems := TvgWideStringList.Create;
  TvgWideStringList(FItems).OnChange := DoItemsChanged;
  FPopup := TvgPopup.Create(Self);
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Self;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FPopup.OnClosePopup := DoClosePopup;
  FListBox := TvgComboListBox.Create(Self);
  FListBox.Parent := FPopup;
  TvgHackComboListBox(FListBox).FComboBox := Self;
  FListBox.ItemHeight := ItemHeight;
  FListBox.Stored := false;
  FListBox.Align := vaClient;
  FListBox.ShowCheckboxes := false;
  FListBox.ItemIndex := -1;
  OnTyping := DoTyping;
end;

destructor TvgComboTextBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TvgComboTextBox.DoItemsChanged(Sender: TObject);
begin
  RebuildList;
end;

procedure TvgComboTextBox.RebuildList;
var
  SaveI, i: integer;
  Item: TvgListBoxItem;
begin
  if csDestroying in ComponentState then Exit;
  if Items = nil then Exit;
  BeginUpdate;
  SaveI := FListbox.ItemIndex;
  TvgHackComboListBox(FListBox).FItemIndex := -1;
  FListBox.Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := TvgListBoxItem.Create(Self);
    Item.AutoTranslate := FAutoTranslate;
    Item.Parent := FListBox;
    Item.Height := FItemHeight;
    Item.Stored := false;
    Item.Locked := true;
    Item.Text := FItems[i];
  end;
  EndUpdate;
  TvgHackComboListBox(FListbox).FItemIndex := SaveI;
  if TvgHackComboListBox(FListbox).FItemIndex >= FListbox.Count then
    TvgHackComboListBox(FListbox).FItemIndex := FListbox.Count - 1;
end;

procedure TvgComboTextBox.KeyDown(var Key: Word;
  var KeyChar: System.WideChar; Shift: TShiftState);
var
  i: integer;
begin
  inherited ;
  if Count > 0 then
  begin
    if ReadOnly and (KeyChar <> #0) then
    begin
      for i := 0 to Count - 1 do
        if (FListBox.Items[i].Text <> '') and (WideLowerCase(FListBox.Items[i].Text[1]) = WideLowerCase(KeyChar)) then
        begin
          ItemIndex := i;
          Break;
        end;
      KeyChar := #0;
    end;
    case Key of
      VK_UP: If ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex - 1;
          if ItemIndex < 0 then ItemIndex := 0;
        end;
      VK_DOWN:
        begin
          If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
          if ItemIndex > Count - 1 then ItemIndex := Count - 1;
        end;
    else
      Exit;
    end;
    Key := 0;
  end;
end;

procedure TvgComboTextBox.Realign;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { FContent }
  if FPopup <> nil then
    FPopup.Width := Width;
  if FListBox <> nil then
    FListBox.Width := Width;
  FDisableAlign := false;
end;

procedure TvgComboTextBox.DoClosePopup(Sender: TObject);
begin
  if ShowCaret and IsFocused then
    ShowCaretProc;
end;

procedure TvgComboTextBox.DropDown;
var
  Count, i: integer;
begin
  if not FPopup.IsOpen then
  begin
    if ShowCaret then
      HideCaret;
    FPopup.Placement := FPlacement;
    FPopup.Width := Width;
    Count := DropDownCount;
    if FListBox.Count < Count then
    Count := FListBox.Count;
    if FListbox.ItemHeight > 0 then
      FPopup.Height := Count * (FListbox.ItemHeight) + 4
    else
      FPopup.Height := Count * (Height - 4);
    TvgHackComboListBox(FListBox).FNeedResource := true;
    FListBox.ApplyResource;
    FPopup.IsOpen := true;
    FListBox.SetFocus;
    TvgHackComboListBox(FListbox).UpdateSelection;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

procedure TvgComboTextBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if (Button = mbLeft) and not vgPtInRect(vgPoint(X, Y), ContentRect) then
  begin
    DropDown;
  end;
end;

procedure TvgComboTextBox.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    FListBox.ItemHeight := FItemHeight;
  end;
end;

procedure TvgComboTextBox.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

procedure TvgComboTextBox.ChangeParent;
begin
  inherited;
end;

function TvgComboTextBox.GetItemIndex: integer;
begin
  if FListBox <> nil then
    Result := FListBox.ItemIndex
  else
    Result := -1;
end;

procedure TvgComboTextBox.SetItemIndex(const Value: integer);
begin
  if FListBox <> nil then
  begin
    FListBox.ItemIndex := Value;
    if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    begin
      Text := Items[ItemIndex];
      FNeedChange := false;
      if not Scene.ShowKeyboardForControl(Self) then
        SelectAll;
      if Assigned(FBindingObjects) then
        ToBindingObjects;
      if Assigned(FOnChange) then
        FOnChange(Self);
      if (FResourceLink <> nil) then
        FResourceLink.Visual.UpdateEffects;
      Repaint;
    end;
  end;
end;

function TvgComboTextBox.GetCount: integer;
begin
  if FListBox <> nil then
    Result := FListBox.Count
  else
    Result := 0;
end;

procedure TvgComboTextBox.DoTyping(Sender: TObject);
var
  i, l: integer;
  UT: WideString;
begin
  if (FLastChar = #0) and ((FLastKey = VK_DELETE) or (FLastKey = 8)) then Exit;
  {$IFDEF NOVCL}
  {$IFDEF DARWIN}
  Exit;
  {$ENDIF}
  {$ENDIF}
  UT := WideUpperCase(Text);
  exit;
  for i := 0 to Items.Count - 1 do
    if pos(UT, WideUpperCase(Items.Strings[i])) = 1 then
    begin
      l := length(Text);
      Text := Items.Strings[i];
      SelStart := l;
      SelLength := length(Items.Strings[i]) - l;
      Exit;
    end;
end;

function TvgComboTextBox.GetListBoxResource: string;
begin
  Result := FListBox.Resource;
end;

procedure TvgComboTextBox.SetListBoxResource(const Value: string);
begin
  FListBox.Resource := Value;
end;

{ TvgComboTrackBar }

constructor TvgComboTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FFilterChar := '0123456789.,-+';
  Text := '0';
  Width := 100;
  Height := 22;
  FResource := 'combotextboxstyle';
  FPopup := TvgPopup.Create(Self);
  FPopup.FResource := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.StaysOpen := false;
  FPopup.Stored := false;
  FPopup.Parent := Self;
  FPopup.Locked := true;
  FPopup.DesignHide := true;
  FPopup.Margins.Rect := vgRect(5, 2, 5, 2);
  FPopup.OnClosePopup := DoClosePopup;
  FTrackBar := TvgTrackBar.Create(Self);
  FTrackBar.Parent := FPopup;
  FTrackBar.Stored := false;
  FTrackBar.DisableFocusEffect := true;
  FTrackBar.Align := vaVertCenter;
  FTrackBar.OnChange := DoTrackChange;
end;

destructor TvgComboTrackBar.Destroy;
begin
  inherited;
end;

procedure TvgComboTrackBar.DoTrackChange(Sender: TObject);
begin
  Text := vgFloatToStr(FTrackBar.Value);
  SelectAll;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TvgComboTrackBar.KeyDown(var Key: Word;
  var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited ;
{  if Count > 0 then
  begin
    case Key of
      VK_UP: If ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex - 1;
          if ItemIndex < 0 then ItemIndex := 0;
        end;
      VK_DOWN:
        begin
          If ItemIndex < Count - 1 then ItemIndex := ItemIndex + 1;
          if ItemIndex > Count - 1 then ItemIndex := Count - 1;
        end;
    else
      Exit;
    end;
    Key := 0;
  end; }
end;

procedure TvgComboTrackBar.DoClosePopup(Sender: TObject);
begin
  if ShowCaret and IsFocused then
    ShowCaretProc;
end;

procedure TvgComboTrackBar.DropDown;
var
  i: integer;
begin
  if not FPopup.IsOpen then
  begin
    if ShowCaret then
      HideCaret;
    FPopup.Placement := FPlacement;
    if Width < 100 then
      FPopup.Width := 100
    else
      FPopup.Width := Width;
    FPopup.Height := 30;
    FTrackBar.ApplyResource;
    FPopup.IsOpen := true;
  end
  else
  begin
    FPopup.IsOpen := false;
  end;
end;

procedure TvgComboTrackBar.Change;
begin
  try
    Value := StrToFloat(Text);
  except
    FText := FloatToStr(Value);
  end;
  Repaint;
  inherited;
end;

procedure TvgComboTrackBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if (Button = mbLeft) and not vgPtInRect(vgPoint(X, Y), ContentRect) then
  begin
    DropDown;
  end;
end;

procedure TvgComboTrackBar.ChangeParent;
begin
  inherited;
  FPopup.Parent := Parent;
end;

function TvgComboTrackBar.GetFrequency: single;
begin
  Result := FTrackBar.Frequency;
end;

function TvgComboTrackBar.GetMax: single;
begin
  Result := FTrackBar.Max;
end;

function TvgComboTrackBar.GetMin: single;
begin
  Result := FTrackBar.Min;
end;

procedure TvgComboTrackBar.SetFrequency(const Value: single);
begin
  FTrackBar.Frequency := Value;
end;

procedure TvgComboTrackBar.SetMax(const Value: single);
begin
  FTrackBar.Max := Value;
end;

procedure TvgComboTrackBar.SetMin(const Value: single);
begin
  FTrackBar.Min := Value;
end;

procedure TvgComboTrackBar.SetValue(const AValue: single);
begin
  FTrackBar.Value := AValue;
end;

function TvgComboTrackBar.GetValue: single;
begin
  Result := FTrackBar.Value;
end;

{ TvgTextBoxClearBtn }

constructor TvgTextBoxClearBtn.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgTextBoxClearBtn.Destroy;
begin
  inherited;
end;

procedure TvgTextBoxClearBtn.ApplyStyle;
var
  B: TvgObject;
begin
  inherited;
  B := FindResource('clearbutton');
  if (B <> nil) and (B is TvgCustomButton) then
  begin
    FClearBtn := TvgCustomButton(B);
    FClearBtn.OnClick := DoClearBtnClick;
  end;
end;

procedure TvgTextBoxClearBtn.DoClearBtnClick(Sender: TObject);
begin
  SetFocus;
  Text := '';
  Change;
end;

procedure TvgTextBoxClearBtn.FreeStyle;
begin
  FClearBtn := nil;
  inherited;
end;

{ TvgHudComboTextBox }

constructor TvgHudComboTextBox.Create(AOwner: TComponent);
begin
  inherited;
  FListBox.FResource := 'hudcombolistboxstyle';
  FResource := 'hudcombotextboxstyle';
end;

{ TvgHudComboTrackBar }

constructor TvgHudComboTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FResource := 'hudcombotextboxstyle';
  FPopup.FResource := 'hudcombopopupstyle';
  FTrackBar.FResource := 'hudtrackbarstyle';
end;

initialization
  RegisterVGObjects('Text Edits', [TvgTextBox, TvgRoundTextBox, TvgNumberBox, TvgSpinBox, TvgComboTextBox, TvgComboTrackBar, TvgTextBoxClearBtn]);
  RegisterVGObjects('HUD', [TvgHudTextBox, TvgHudNumberBox, TvgHudRoundTextBox, TvgHudSpinBox, TvgHudComboTextBox, TvgHudComboTrackBar]);
end.

