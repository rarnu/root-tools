{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TFONT
 ******************************************************************************
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


{ TFontHandleCache }

type
  TLogFontAndName = record
    LogFont: TLogFont;
    LongFontName: string;
  end;
  PLogFontAndName = ^TLogFontAndName;

function CompareLogFontAndNameWithResDesc(Key: PLogFontAndName; Desc: TFontHandleCacheDescriptor): integer;
begin
  Result := CompareStr(Key^.LongFontName, Desc.LongFontName);
  //debugln('CompareLogFontAndNameWithResDesc A ',Key^.LongFontName,' ',Desc.LongFontName,' ',DbgS(Desc),' Result=',Result);
  if Result = 0 then
    Result := CompareMemRange(@Key^.LogFont, @Desc.LogFont, SizeOf(Desc.LogFont));
  //debugln('CompareLogFontAndNameWithResDesc END Result=',Result);
end;

procedure TFontHandleCache.RemoveItem(Item: TResourceCacheItem);
begin
  DeleteObject(HGDIOBJ(Item.Handle));
  inherited RemoveItem(Item);
end;

constructor TFontHandleCache.Create;
begin
  inherited Create;
  FResourceCacheDescriptorClass := TFontHandleCacheDescriptor;
end;

function TFontHandleCache.CompareDescriptors(Tree: TAvgLvlTree; Desc1,
  Desc2: Pointer): integer;
var
  Descriptor1: TFontHandleCacheDescriptor absolute Desc1;
  Descriptor2: TFontHandleCacheDescriptor absolute Desc2;
begin
  Result := CompareStr(Descriptor1.LongFontName, Descriptor2.LongFontName);
  if Result <> 0 then
    Exit;
  Result := CompareMemRange(@Descriptor1.LogFont, @Descriptor2.LogFont,
                          SizeOf(Descriptor1.LogFont));
end;

function TFontHandleCache.FindFont(TheFont: TLCLHandle): TResourceCacheItem;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode := FItems.FindKey(@TheFont,
                          TListSortCompare(@ComparePHandleWithResourceCacheItem));
  if ANode <> nil then
    Result := TResourceCacheItem(ANode.Data)
  else
    Result := nil;
end;

function TFontHandleCache.FindFontDesc(const LogFont: TLogFont;
  const LongFontName: string): TFontHandleCacheDescriptor;
var
  LogFontAndName: TLogFontAndName;
  ANode: TAvgLvlTreeNode;
begin
  LogFontAndName.LogFont := LogFont;
  LogFontAndName.LongFontName := LongFontName;
  ANode := FDescriptors.Findkey(@LogFontAndName,
                           TListSortCompare(@CompareLogFontAndNameWithResDesc));
  if ANode <> nil then
    Result := TFontHandleCacheDescriptor(ANode.Data)
  else
    Result := nil;
end;

function TFontHandleCache.Add(TheFont: TLCLHandle; const LogFont: TLogFont;
  const LongFontName: string): TFontHandleCacheDescriptor;
var
  Item: TResourceCacheItem;
begin

  // find cache item with TheFont
  Item := FindFont(TheFont);
  if Item = nil then
  begin
    // create new item
    Item := TResourceCacheItem.Create(Self, TheFont);
    FItems.Add(Item);
  end;

  // create descriptor
  Result := TFontHandleCacheDescriptor.Create(Self, Item);
  Result.LongFontName := LongFontName;
  Result.LogFont := LogFont;
  FDescriptors.Add(Result);
  if FindFontDesc(LogFont, LongFontName) = nil then
  begin

  end;
end;

{ TFont }

procedure GetCharsetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(FontCharsets) to High(FontCharsets) do
    Proc(FontCharsets[I].Name);
end;

function CharsetToIdent(Charset: Longint; out Ident: string): Boolean;
begin
  Result := IntToIdent(Charset, Ident, FontCharsets);
end;

function IdentToCharset(const Ident: string; out Charset: Longint): Boolean;
begin
  Result := IdentToInt(Ident, CharSet, FontCharsets);
end;

function GetFontData(Font: HFont): TFontData;
var
  ALogFont: TLogFont;
begin
  Result := DefFontData;
  if Font <> 0 then
  begin
    if GetObject(Font, SizeOf(ALogFont), @ALogFont) <> 0 then
      with Result, ALogFont do
      begin
        Height := lfHeight;
        if lfWeight >= FW_BOLD then
          Include(Style, fsBold);
        if lfItalic > 0 then
          Include(Style, fsItalic);
        if lfUnderline > 0 then
          Include(Style, fsUnderline);
        if lfStrikeOut > 0 then
          Include(Style, fsStrikeOut);
        Charset := TFontCharset(lfCharSet);
        Name := lfFaceName;
        case lfPitchAndFamily and $F of
          VARIABLE_PITCH: Pitch := fpVariable;
          FIXED_PITCH: Pitch := fpFixed;
        else
          Pitch := fpDefault;
        end;
        Orientation := lfOrientation;
        Handle := Font;
      end;
  end;
end;

function GetDefFontCharSet: TFontCharSet;
begin
  Result := DEFAULT_CHARSET;
end;

{------------------------------------------------------------------------------
  function:  FindXLFDItem
  Params:  const XLFDName: string; Index: integer;
           var ItemStart, ItemEnd: integer
  Returns: boolean

  Searches the XLFD item on position Index. Index starts from 0.
  Returns true on sucess.
  ItemStart will be on the first character and ItemEnd after the last character.
 ------------------------------------------------------------------------------}
function FindXLFDItem(const XLFDName: string; Index: integer;
  var ItemStart, ItemEnd: integer): boolean;
begin
  if Index<0 then
  begin
    Result := False;
    exit;
  end;
  ItemStart := 1;
  ItemEnd := ItemStart;
  while true do
  begin
    if (ItemEnd>length(XLFDName)) then
    begin
      dec(Index);
      break;
    end;
    if XLFDName[ItemEnd] = '-' then
    begin
      dec(Index);
      if Index < 0 then break;
      ItemStart := ItemEnd + 1;
    end;
    inc(ItemEnd);
  end;
  Result := (Index = -1);
end;

{------------------------------------------------------------------------------
  function:  ExtractXLFDItem
  Params:  const XLFDName: string; Index: integer
  Returns: string

  Parses a font name in XLFD format and extracts one item.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding

 ------------------------------------------------------------------------------}
function ExtractXLFDItem(const XLFDName: string; Index: integer): string;
var StartPos, EndPos: integer;
begin
  if FindXLFDItem(XLFDName, Index, StartPos, EndPos) then
    Result := copy(XLFDName, StartPos, EndPos - StartPos)
  else
    Result := '';
end;

{------------------------------------------------------------------------------
  function:  ExtractFamilyFromXLFDName
  Params:  const XLFDName: string
  Returns: string

  Parses a font name in XLFD format and extracts the FamilyName.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding

 ------------------------------------------------------------------------------}
function ExtractFamilyFromXLFDName(const XLFDName: string): string;
var StartPos, EndPos: integer;
begin
  if FindXLFDItem(XLFDName, 2, StartPos, EndPos) then
    Result:=copy(XLFDName, StartPos, EndPos - StartPos)
  else
    Result := '';
end;

{------------------------------------------------------------------------------
  Method:  XLFDNameToLogFont
  Params:  const XLFDName: string
  Returns: TLogFont

  Parses a font name in XLFD format and creates a TLogFont record from it.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding

 ------------------------------------------------------------------------------}
function XLFDNameToLogFont(const XLFDName: string): TLogFont;
type
  TWeightMapEntry = record
    Name: string;
    Weight: integer;
  end;
const
  WeightMap: array[1..15] of TWeightMapEntry = (
    (Name: 'DONTCARE'; Weight: FW_DONTCARE),
    (Name: 'THIN'; Weight: FW_THIN),
    (Name: 'EXTRALIGHT'; Weight: FW_EXTRALIGHT),
    (Name: 'LIGHT'; Weight: FW_LIGHT),
    (Name: 'NORMAL'; Weight: FW_NORMAL),
    (Name: 'MEDIUM'; Weight: FW_MEDIUM),
    (Name: 'SEMIBOLD'; Weight: FW_SEMIBOLD),
    (Name: 'BOLD'; Weight: FW_BOLD),
    (Name: 'EXTRABOLD'; Weight: FW_EXTRABOLD),
    (Name: 'HEAVY'; Weight: FW_HEAVY),
    (Name: 'ULTRALIGHT'; Weight: FW_ULTRALIGHT),
    (Name: 'REGULAR'; Weight: FW_REGULAR),
    (Name: 'DEMIBOLD'; Weight: FW_DEMIBOLD),
    (Name: 'ULTRABOLD'; Weight: FW_ULTRABOLD),
    (Name: 'BLACK'; Weight: FW_BLACK)
    );
var
  ItemStart, ItemEnd: integer;
  Item: string;

  procedure GetNextItem;
  begin
    ItemStart:=ItemEnd+1;
    ItemEnd:=ItemStart;
    while (ItemEnd<=length(XLFDName)) and (XLFDName[ItemEnd]<>'-') do
      inc(ItemEnd);
    Item:=copy(XLFDName,ItemStart,ItemEnd-ItemStart);
  end;
  
  function WeightNameToWeightID(const WeightName: string): integer;
  var i: integer;
  begin
    for i:=Low(WeightMap) to High(WeightMap) do begin
      if AnsiCompareText(WeightMap[i].Name,WeightName)=0 then begin
        Result:=WeightMap[i].Weight;
        exit;
      end;
    end;
    Result:=FW_DONTCARE;
  end;

var l, FaceNameMax, PixelSize, PointSize, Resolution, AverageWidth: integer;
begin
  FillChar(Result,SizeOf(TLogFont),0);
  ItemEnd:=0;
  GetNextItem; // 1. read FontNameRegistry
  // ToDo
  
  GetNextItem; // 2. read Foundry
  // ToDo

  GetNextItem; // 3. read FamilyName
  l:=length(Item);
  FaceNameMax:=High(Result.lfFaceName)-Low(Result.lfFaceName); // max without #0
  if l>FaceNameMax then l:=FaceNameMax;
  if l>0 then Move(Item[1],Result.lfFaceName[Low(Result.lfFaceName)],l);
  Result.lfFaceName[Low(Result.lfFaceName)+l]:=#0;

  GetNextItem; // 4. read WeightName
  Result.lfWeight:=WeightNameToWeightID(Item);

  GetNextItem; // 5. read Slant
  if (AnsiCompareText(Item,'I')=0) or (AnsiCompareText(Item,'RI')=0)
  or (AnsiCompareText(Item,'O')=0) then
    // I = italic, RI = reverse italic, O = oblique
    Result.lfItalic:=1
  else
    Result.lfItalic:=0;

  GetNextItem; // 6. read SetwidthName
  // ToDO: NORMAL, CONDENSED, NARROW, WIDE, EXPANDED

  GetNextItem; // 7. read AddStyleName
  // calculate Style name extensions (=rotation)
  //        API                 XLFD
  // --------------------- --------------
  // Orientation 1/10 deg  1/64 deg
  Result.lfOrientation:=(StrToIntDef(Item,0)*10) div 64;
      
  GetNextItem; // 8. read PixelSize
  PixelSize:=StrToIntDef(Item,0);
  GetNextItem; // 9. read PointSize
  PointSize:=StrToIntDef(Item,0) div 10;
  GetNextItem; // 10. read ResolutionX
  Resolution:=StrToIntDef(Item,0);
  if Resolution<=0 then Resolution:=72;

  if PixelSize=0 then begin
    if PointSize<=0 then
      Result.lfHeight:=(12*Resolution) div 72
    else
      Result.lfHeight:=(PointSize*Resolution) div 72;
  end else begin
    Result.lfHeight:=PixelSize;
  end;

  GetNextItem; // 11. read ResolutionY
  Resolution:=StrToIntDef(Item,0);
  if Resolution<=0 then Resolution:=72;

  GetNextItem; // 12. read Spacing
  {M       Monospaced (fixed pitch)
   P       Proportional spaced (variable pitch)
   C       Character cell.  The glyphs of the font can be thought of as
           "boxes" of the same width and height that are stacked side by
           side or top to bottom.}
  if AnsiCompareText(Item,'M')=0 then
    Result.lfPitchAndFamily:=FIXED_PITCH
  else if AnsiCompareText(Item,'P')=0 then
    Result.lfPitchAndFamily:=VARIABLE_PITCH
  else if AnsiCompareText(Item,'C')=0 then
    Result.lfPitchAndFamily:=FIXED_PITCH;

  GetNextItem; // 13. read AverageWidth
  AverageWidth := StrToIntDef(Item,0);
  Result.lfWidth := AverageWidth div 10;

  GetNextItem; // 14. read CharSetRegistry
  // ToDo

  GetNextItem; // 15. read CharSetCoding
  // ToDo

end;

{------------------------------------------------------------------------------
  function: ClearXLFDItem
  Params:   const LongFontName: string; Index: integer
  Returns:  string

  Replaces an item of a font name in XLFD format with a '*'.
 ------------------------------------------------------------------------------}
function ClearXLFDItem(const LongFontName: string; Index: integer): string;
var ItemStart, ItemEnd: integer;
begin
  if FindXLFDItem(LongFontName,Index,ItemStart,ItemEnd)
  and ((ItemEnd-ItemStart<>1) or (LongFontName[ItemStart]<>'*')) then
    Result:=LeftStr(LongFontName,ItemStart-1)+'*'
            +RightStr(LongFontName,length(LongFontName)-ItemEnd+1)
  else
    Result:=LongFontName;
end;

{------------------------------------------------------------------------------
  function: ClearXLFDHeight
  Params:   const LongFontName: string
  Returns:  string

  Replaces the PixelSize, PointSize, ResolutionX, ResolutionY and AverageWidth
  of a font name in XLFD format with '*'.
  
  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding
 ------------------------------------------------------------------------------}
function ClearXLFDHeight(const LongFontName: string): string;
begin
  Result:=ClearXLFDItem(LongFontName,7); // PixelSize
  Result:=ClearXLFDItem(Result,8);       // PointSize
  Result:=ClearXLFDItem(Result,9);       // ResolutionX
  Result:=ClearXLFDItem(Result,10);      // ResolutionY
  Result:=ClearXLFDItem(Result,12);      // AverageWidth
end;

{------------------------------------------------------------------------------
  function: ClearXLFDPitch
  Params:   const LongFontName: string
  Returns:  string

  Replaces the spacing a font name in XLFD format with a '*'.
 ------------------------------------------------------------------------------}
function ClearXLFDPitch(const LongFontName: string): string;
begin
  Result:=ClearXLFDItem(LongFontName,11);
end;

{------------------------------------------------------------------------------
  function: ClearXLFDStyle
  Params:   const LongFontName: string
  Returns:  string

  Replaces the WeightName, Slant and SetwidthName of a font name in XLFD format
  with '*'.
 ------------------------------------------------------------------------------}
function ClearXLFDStyle(const LongFontName: string): string;
begin
  Result:=ClearXLFDItem(ClearXLFDItem(ClearXLFDItem(LongFontName,3),4),5);
end;

function XLFDHeightIsSet(const LongFontName: string): boolean;
begin
  Result:=(ExtractXLFDItem(LongFontName,7)<>'')
       or (ExtractXLFDItem(LongFontName,8)<>'')
       or (ExtractXLFDItem(LongFontName,9)<>'')
       or (ExtractXLFDItem(LongFontName,10)<>'');
end;

{------------------------------------------------------------------------------
  function: IsFontNameXLogicalFontDesc
  Params:   const LongFontName: string
  Returns:  boolean

  Checks if font name is in X Logical Font Description format.
  (see http://wwwinfo.cern.ch/umtf/working-groups/X11/fonts/hp_xlfd.html)

  An XLFD name is
  FontNameRegistry-Foundry-FamilyName-WeightName-Slant-SetwidthName
  -AddStyleName-PixelSize-PointSize-ResolutionX-ResolutionY-Spacing
  -AverageWidth-CharSetRegistry-CharSetCoding
 ------------------------------------------------------------------------------}
function IsFontNameXLogicalFontDesc(const LongFontName: string): boolean;
// Quick test: check if LongFontName contains 14 times the char '-'
var MinusCnt, p: integer;
begin
  MinusCnt:=0;
  for p:=1 to length(LongFontName) do
    if LongFontName[p]='-' then inc(MinusCnt);
  Result:=(MinusCnt=14);
end;

// split a given fontName into Pango Font description components
// font name is supposed to follow this layout:
// [FAMILY-LIST][STYLE-LIST][SIZE]
// where:
// [FAMILY-LIST]  is a comma separated list of families optionally
//                ended by a comma
// [STYLE-LIST]   is white space separated list of words where each word
//                describe one of style, variant, slant, weight or stretch
// [SIZE]         is a decimal number (size in points) (... and points in PANGO_UNITS)
// any of these options may be absent.
procedure FontNameToPangoFontDescStr(const LongFontName: string;
  out aFamily,aStyle: string; out aSize: Integer; out aSizeInPixels: Boolean);

var
  ParsePos: Integer;
  
  procedure addStyle(const s: string);
  begin
    if (s<>'') and (s<>'*') and (s<>'r') then begin
      // 'r' is regular
      if aStyle<>'' then
        aStyle := aStyle + ' ' + s
      else
        aStyle := s;
    end;
  end;

  function GetSize: string;
  var
    c: char;
    ValidBlank, CheckPixelsNeeded: boolean;
    InitPos: Integer;
    
    function IsBlank: boolean;
    begin
      result := c in [#0..' '];
    end;
    
    function IsDigit: boolean;
    begin
      result := c in ['0'..'9'];
    end;
    
  begin
    Result := '';
    ValidBlank := True;
    CheckPixelsNeeded := True;
    ParsePos := Length(LongFontname);
    InitPos := ParsePos;
    while ParsePos>0 do begin
      c := longFontName[ParsePos];
      if IsBlank then
        if ValidBlank then begin
          dec(ParsePos);
          dec(InitPos);
          continue
        end else
          break;
      ValidBlank := False;
      if CheckPixelsNeeded then
      begin
        CheckPixelsNeeded := False;
        aSizeInPixels := (ParsePos > 2) and (longFontName[ParsePos - 1] = 'p')
          and (longFontName[ParsePos] = 'x');
        if aSizeInPixels then
        begin
          dec(ParsePos, 2);
          Continue;
        end;
      end;
      if IsDigit then begin
        Result := C + Result;
        dec(ParsePos);
      end else begin
        if not IsBlank and (C <> ',')then
        begin
          Result := '';
          ParsePos := InitPos;
        end;
        if C = ',' then
          dec(ParsePos);
        break;
      end;
    end;
  end;
  
begin
  aStyle := '';
  aFamily := '';
  aSize := 0;
  aSizeInPixels := False;
  if IsFontNameXLogicalFontDesc(LongFontName) then begin
    aFamily := ExtractXLFDItem(LongFontName, XLFD_FAMILY);
    if aFamily='*' then
      aFamily:='';
    aSize := StrToIntDef(ExtractXLFDItem(LongFontName, XLFD_POINTSIZE),0) div 10;
    addStyle( ExtractXLFDItem(LongFontName, XLFD_STYLENAME ));
    addStyle( ExtractXLFDItem(LongFontname, XLFD_WEIGHTNAME));
    addStyle( ExtractXLFDItem(LongFontname, XLFD_SLANT));
    addStyle( ExtractXLFDItem(LongFontname, XLFD_WidthName));
  end else begin
    // this could go through, but we want to know at least the pointSize from
    // the fontname
    aSize := StrToIntDef(GetSize,0);
    aFamily := Copy(LongFontName, 1, ParsePos);
    // todo: parse aFamily to separate Family and Style
  end;
end;

{ TFont }

{------------------------------------------------------------------------------
  Method:  TFont.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TFont.Create;
begin
  inherited Create;
  FColor := {$ifdef UseCLDefault}clDefault{$else}clWindowText{$endif};
  FPixelsPerInch := ScreenInfo.PixelsPerInchY;
  FPitch := DefFontData.Pitch;
  FCharSet := DefFontData.CharSet;
  FQuality := DefFontData.Quality;
  FHeight := DefFontData.Height;
  inherited SetSize(-MulDiv(FHeight, 72, FPixelsPerInch));
  DelayAllocate := True;
  inherited SetName(DefFontData.Name);
  inherited SetFPColor(colBlack);
end;

{------------------------------------------------------------------------------
  Method: TFont.Assign
  Params: Source: Another font
  Returns:  nothing

  Copies the Source font to itself
 ------------------------------------------------------------------------------}
procedure TFont.Assign(Source: TPersistent);
begin
  if Source is TFont then
  begin
    //TODO:lock;
    try
      //TODO: TFont(Source).Lock;
      try
        BeginUpdate;
        try
          CharSet := TFont(Source).CharSet;
          SetColor(TFont(Source).Color, TFPCanvasHelper(Source).FPColor);
          if TFont(Source).PixelsPerInch <> FPixelsPerInch then
            // use size to convert source height pixels to current resolution
            Size := TFont(Source).Size
          else
            // use height which users could have changed directly
            Height := TFont(Source).Height;
          Name := TFont(Source).Name;
          Orientation := TFont(Source).Orientation;
          Pitch := TFont(Source).Pitch;
          Style := TFont(Source).Style;
          Quality := TFont(Source).Quality;
        finally
          EndUpdate;
        end;
      finally
        //TODO: TFont(Source).UnLock;
      end;
    finally
      //TODO: UnLock;
    end;
    Exit;
  end;

  inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TFont.Assign
  Params: ALogFont: TLogFont
  Returns:  nothing

  Copies the logfont settings to itself
 ------------------------------------------------------------------------------}
procedure TFont.Assign(const ALogFont: TLogFont);
var
  AStyle: TFontStyles;
begin
  BeginUpdate;
  try
    with ALogFont do
    begin
      Height := ALogFont.lfHeight;
      Charset := TFontCharset(ALogFont.lfCharSet);
      AStyle := [];
      with ALogFont do
      begin
        if lfWeight >= FW_SEMIBOLD then Include(AStyle, fsBold);
        if lfItalic <> 0 then Include(AStyle, fsItalic);
        if lfUnderline <> 0 then Include(AStyle, fsUnderline);
        if lfStrikeOut <> 0 then Include(AStyle, fsStrikeOut);
      end;
      if (FIXED_PITCH and lfPitchAndFamily) <> 0 then
        Pitch := fpFixed
      else if (VARIABLE_PITCH and lfPitchAndFamily) <> 0 then
        Pitch := fpVariable
      else
        Pitch := fpDefault;
      Style := AStyle;
      Quality := TFontQuality(ALogFont.lfQuality);
      Name := ALogFont.lfFaceName;
    end;
  finally
    EndUpdate;
  end;
end;

function TFont.IsEqual(AFont: TFont): boolean;
begin
  if (AFont = Self) then Exit(true);
  if (AFont=nil)
  or (CharSet<>AFont.CharSet)
  or (Color<>AFont.Color)
  or (PixelsPerInch<>AFont.PixelsPerInch)
  or (Size<>AFont.Size)
  or (Height<>AFont.Height)
  or (Name<>AFont.Name)
  or (Pitch<>AFont.Pitch)
  or (Quality<>AFont.Quality)
  or (Style<>AFont.Style) then
    Result := False
  else
    Result := True;
end;

procedure TFont.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TFont.EndUpdate;
begin
  if FUpdateCount=0 then exit;
  dec(FUpdateCount);
  if (FUpdateCount=0) and FChanged then Changed;
end;

{------------------------------------------------------------------------------
  Method: TFont.HandleAllocated
  Params: none
  Returns: boolean

  Resturns True on handle allocated.
 ------------------------------------------------------------------------------}
function TFont.HandleAllocated: boolean;
begin
  Result := FReference.Allocated;
end;

{------------------------------------------------------------------------------
  function TFont.IsDefault: boolean;
 ------------------------------------------------------------------------------}
function TFont.IsDefault: boolean;
begin
  Result:=(CharSet=DEFAULT_CHARSET)
         and (Color={$ifdef UseCLDefault}clDefault{$else}clWindowText{$endif})
         and (Height=0)
         and (not IsNameStored)
         and (Pitch=fpDefault)
         and (Size=0)
         and (Quality=fqDefault)
         and (Style=[]);
end;

{------------------------------------------------------------------------------
  procedure TFont.SetDefault;

  Set Font properties to default.
 ------------------------------------------------------------------------------}
procedure TFont.SetDefault;
begin
  BeginUpdate;
  try
    Name := DefFontData.Name;
    Charset := DefFontData.CharSet;
    Height := DefFontData.Height;
    Pitch := DefFontData.Pitch;
    Quality := DefFontData.Quality;
    Style := DefFontData.Style;
    Color := {$ifdef UseCLDefault}clDefault{$else}clWindowText{$endif};
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetSize
  Params: AValue: the new value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TFont.SetSize(AValue: Integer);
begin
  if Size <> AValue then
  begin
    BeginUpdate;
    try
      FreeReference;
      inherited SetSize(AValue);
      FHeight := -MulDiv(AValue, FPixelsPerInch, 72);
      if IsFontNameXLogicalFontDesc(Name) then
        Name := ClearXLFDHeight(Name);
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetSize
  Params: none
  Returns:  The font size

  Calculates the size based on height
 ------------------------------------------------------------------------------}
function TFont.GetSize: Integer;
begin
  Result := inherited Size;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetPitch
  Params: Value: the new value
  Returns:  nothing

  Sets the pitch of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetPitch(Value : TFontPitch);
Begin
  if FPitch <> Value then
  begin
    BeginUpdate;
    FreeReference;
    FPitch := Value;
    if IsFontNameXLogicalFontDesc(Name) then
      Name := ClearXLFDPitch(Name);
    Changed;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetHeight
  Params: Value: the new value
  Returns:  nothing

  Sets the height of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetHeight(AValue: Integer);
begin
  // Don't update Size only. The LogFont contains a lfHeight value and on Windows,
  // Qt and Carbon it is the main parameter which determins the font height.
  if Height <> AValue then
  begin
    BeginUpdate;
    try
      FreeReference;
      FHeight := AValue;
      // update size to equivalent value
      inherited SetSize(-MulDiv(AValue, 72, FPixelsPerInch));
      if IsFontNameXLogicalFontDesc(Name) then
        Name := ClearXLFDHeight(Name);
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetStyle
  Params: Value: the new value
  Returns:  nothing

  Sets the style of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetStyle(value : TFontStyles);
begin
  if FStyle <> Value then
  begin
    BeginUpdate;
    FreeReference;
    FStyle := Value;
    inherited SetFlags(5, fsBold in FStyle);
    inherited SetFlags(6, fsItalic in FStyle);
    inherited SetFlags(7, fsUnderline in FStyle);
    inherited SetFlags(8, fsStrikeOut in FStyle);
    if IsFontNameXLogicalFontDesc(Name) then
      Name := ClearXLFDStyle(Name);
    Changed;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetColor
  Params: Value: the new value
  Returns:  nothing

  Sets the pencolor of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetColor(Value : TColor);
begin
  if FColor <> Value then
    SetColor(Value, TColorToFPColor(Value));
end;

function TFont.GetColor: TColor;
begin
  Result := Color;
  if (Result = clDefault) and Assigned(Canvas) and (Canvas is TCanvas) then
    Result := TCanvas(Canvas).GetDefaultColor(dctFont);
end;

{------------------------------------------------------------------------------
  Function: TFont.GetName
  Params: none
  Returns:  The font name

  Returns the name of the font
 ------------------------------------------------------------------------------}
function TFont.GetName: string;
begin
  Result := inherited Name;
end;

{------------------------------------------------------------------------------
  Returns the orientation of the font

  The orientation is defined as the angle, in tenths of degrees,
  between the X axis of the Canvas and the baseline of the font.

  The property and it's setter/getter pair are compatible with Delphi
 ------------------------------------------------------------------------------}
function TFont.GetOrientation: Integer;
begin
  Result := FOrientation;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetName
  Params: Value: the new value
  Returns:  nothing

  Sets the name of a font
 ------------------------------------------------------------------------------}
procedure TFont.SetName(AValue: string);
begin
  if Name <> AValue then
  begin
    FreeReference;
    inherited SetName(AValue);
    Changed;
  end;
end;

{------------------------------------------------------------------------------
  Changes the orientation of the font

  The orientation is defined as the angle, in tenths of degrees,
  between the X axis of the Canvas and the baseline of the font.

  The property and it's setter/getter pair are compatible with Delphi
 ------------------------------------------------------------------------------}
procedure TFont.SetOrientation(AValue: Integer);
begin
  if FOrientation <> AValue then
  begin
    FreeReference;
    FOrientation := AValue;
    Changed;
  end;
end;

procedure TFont.DoAllocateResources;
begin
  inherited DoAllocateResources;
  GetReference;
end;

procedure TFont.DoDeAllocateResources;
begin
  FreeReference;
  inherited DoDeAllocateResources;
end;

procedure TFont.DoCopyProps(From: TFPCanvasHelper);
var
  SrcFont: TFont;
begin
  BeginUpdate;
  try
    inherited DoCopyProps(From);
    if From is TFont then
    begin
      SrcFont := TFont(From);
      Pitch := SrcFont.Pitch;
      CharSet := SrcFont.CharSet;
      Quality := SrcFont.Quality;
      Style := SrcFont.Style;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TFont.SetFlags(Index: integer; AValue: boolean);

  procedure SetStyleFlag(Flag: TFontStyle; NewValue: boolean);
  begin
    BeginUpdate;
    FreeReference;
    if NewValue then
      Include(FStyle, Flag)
    else
      Exclude(FStyle, Flag);
    if IsFontNameXLogicalFontDesc(Name) then
      Name := ClearXLFDStyle(Name);
    Changed;
    EndUpdate;
  end;

begin
  if GetFlags(Index) = AValue then Exit;
  inherited SetFlags(Index, AValue);
  case Index of
    5: SetStyleFlag(fsBold, AValue);
    6: SetStyleFlag(fsItalic, AValue);
    7: SetStyleFlag(fsUnderline, AValue);
    8: SetStyleFlag(fsStrikeOut, AValue);
  end;
end;

{------------------------------------------------------------------------------
  procedure TFont.SetFPColor(const AValue: TFPColor);

  Set FPColor and Color
 ------------------------------------------------------------------------------}
procedure TFont.SetFPColor(const AValue: TFPColor);
begin
  if FPColor <> AValue then
    SetColor(FPColorToTColor(AValue), AValue);
end;

procedure TFont.SetColor(const NewColor: TColor; const NewFPColor: TFPColor);
begin
  if (NewColor = Color) and (NewFPColor = FPColor) then Exit;
  FColor := NewColor;
  inherited SetFPColor(NewFPColor);
  Changed;
end;

{------------------------------------------------------------------------------
  Method: TFont.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TFont.Destroy;
begin
  FreeReference;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFont.SetHandle
  Params:   a font handle
  Returns:  nothing

  sets the font to an external created font 
 ------------------------------------------------------------------------------}
procedure TFont.SetHandle(const Value: HFONT);
begin
  SetData(GetFontData(Value));
end;

procedure TFont.ReferenceNeeded;
const
  LF_BOOL: array[Boolean] of Byte = (0, 255);
  LF_WEIGHT: array[Boolean] of Integer = (FW_NORMAL, FW_BOLD);
  LF_QUALITY: array[TFontQuality] of Integer = (DEFAULT_QUALITY,
    DRAFT_QUALITY, PROOF_QUALITY, NONANTIALIASED_QUALITY, ANTIALIASED_QUALITY,
    CLEARTYPE_QUALITY, CLEARTYPE_NATURAL_QUALITY);
var
  ALogFont: TLogFont;
  CachedFont: TFontHandleCacheDescriptor;

  procedure SetLogFontName(const NewName: string);
  var
    l: integer;
    aName: string;
  begin
    if IsFontNameXLogicalFontDesc(NewName) then
      aName := ExtractFamilyFromXLFDName(NewName)
    else
      aName := NewName;
    l := High(ALogFont.lfFaceName) - Low(ALogFont.lfFaceName);
    if l > length(aName) then
      l := length(aName);
    if l > 0 then
      Move(aName[1], ALogFont.lfFaceName[Low(ALogFont.lfFaceName)], l);
    ALogFont.lfFaceName[Low(ALogFont.lfFaceName) + l] := #0;
  end;

begin
  if FReference.Allocated then Exit;

  FillChar(ALogFont, SizeOf(ALogFont), 0);
  with ALogFont do
  begin
    lfHeight := Height;
    lfWidth := 0;
    lfEscapement := FOrientation;
    lfOrientation := FOrientation;
    lfWeight := LF_WEIGHT[fsBold in Style];
    lfItalic := LF_BOOL[fsItalic in Style];
    lfUnderline := LF_BOOL[fsUnderline in Style];
    lfStrikeOut := LF_BOOL[fsStrikeOut in Style];
    lfCharSet := Byte(FCharset);
    SetLogFontName(Name);

    lfQuality := LF_QUALITY[FQuality];
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  FontResourceCache.Lock;
  try
    // ask the font cache for the nearest font
    CachedFont := FontResourceCache.FindFontDesc(ALogFont, Name);
    //DebugLn(['TFont.GetHandle in cache: ',CachedFont<>nil]);
    if CachedFont <> nil then
    begin
      CachedFont.Item.IncreaseRefCount;
      FReference._lclHandle := CachedFont.Item.Handle;
    end else
    begin
      // ask the interface for the nearest font
      FReference._lclHandle := TLCLHandle(CreateFontIndirectEx(ALogFont, Name));
      FontResourceCache.Add(FReference.Handle, ALogFont, Name);
    end;
    FFontHandleCached := True;
  finally
    FontResourceCache.Unlock;
  end;
  FCanUTF8Valid := False;
  FIsMonoSpaceValid := False;
end;

procedure TFont.SetQuality(const AValue: TFontQuality);
begin
  if FQuality <> AValue then
  begin
    BeginUpdate;
    FreeReference;
    FQuality := AValue;
    if IsFontNameXLogicalFontDesc(Name) then
      Name := ClearXLFDStyle(Name);
    Changed;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Function: TFont.GetHandle
  Params:   none
  Returns:  a handle to a font gdiobject

  Creates a font if needed
 ------------------------------------------------------------------------------}
function TFont.GetHandle: HFONT;
begin
  Result := HFONT(Reference.Handle);
end;

{------------------------------------------------------------------------------
  Method:  TFont.FreeReference
  Params:  none
  Returns: Nothing

  Frees a font handle if needed
 ------------------------------------------------------------------------------}

procedure TFont.FreeReference;
begin
  if not FReference.Allocated then Exit;

  // Changing triggers deselecting the current handle
  Changing;
  if FFontHandleCached then
  begin
    if Assigned(FontResourceCache) then
    begin
      FontResourceCache.Lock;
      try
        FontResourceCache.FindFont(FReference.Handle).DecreaseRefCount;
        FFontHandleCached := False;
      finally
        FontResourceCache.Unlock;
      end;
    end;
  end else
    DeleteObject(HGDIOBJ(FReference.Handle));
  FReference._lclHandle := 0;
end;

function TFont.GetCanUTF8: boolean;
begin
  if not FCanUTF8Valid then
  begin
    FCanUTF8 := {%H-}FontCanUTF8(HFONT(Reference.Handle));
    FCanUTF8Valid := True;
  end;
  Result := FCanUTF8;
end;

function TFont.GetCharSet: TFontCharSet;
begin
  Result := FCharSet;
end;

procedure TFont.SetCharSet(const AValue: TFontCharSet);
begin
  if FCharSet <> AValue then
  begin
    FreeReference;
    FCharSet := AValue;
    Changed;
  end;
end;

function TFont.GetData: TFontData;
begin
  Result := DefFontData;
  if HandleAllocated then
    Result.Handle := Reference.Handle
  else
    Result.Handle := 0;
  Result.Height := Height;
  Result.Pitch := Pitch;
  Result.Style := Style;
  Result.CharSet := CharSet;
  Result.Quality := Quality;
  Result.Name := LeftStr(Name, SizeOf(Result.Name) - 1);
  Result.Orientation := Orientation;
end;

function TFont.GetIsMonoSpace: boolean;
begin
  if not FIsMonoSpaceValid then
  begin
    FIsMonoSpace := FontIsMonoSpace(HFONT(Reference.Handle));
    FIsMonoSpaceValid := True;
  end;
  Result := FIsMonoSpace;
end;

function TFont.GetReference: TWSFontReference;
begin
  ReferenceNeeded;
  Result := FReference;
end;

function TFont.IsHeightStored: boolean;
begin
  Result := DefFontData.Height <> Height;
end;

function TFont.IsNameStored: boolean;
begin
  Result := DefFontData.Name <> Name;
end;

procedure TFont.SetData(const FontData: TFontData);
var
  OldStyle: TFontStylesbase;
begin
  if (HFONT(FReference.Handle) <> FontData.Handle) or not FReference.Allocated then
  begin
    OldStyle := FStyle;
    FreeReference;
    FReference._lclHandle := TLCLHandle(FontData.Handle);
    inherited SetSize(-MulDiv(FontData.Height, 72, FPixelsPerInch));
    FHeight := FontData.Height;
    FPitch := FontData.Pitch;
    FStyle := FontData.Style;
    FCharSet := FontData.CharSet;
    FQuality := FontData.Quality;
    inherited SetName(FontData.Name);
    Bold; // it calls GetFlags
    if (fsBold in OldStyle)<>(fsBold in FStyle) then
      inherited SetFlags(5, fsBold in FStyle);
    if (fsItalic in OldStyle)<>(fsItalic in FStyle) then
      inherited SetFlags(6, fsItalic in FStyle);
    if (fsUnderline in OldStyle)<>(fsUnderline in FStyle) then
      inherited SetFlags(7, fsUnderline in FStyle);
    if (fsStrikeOut in OldStyle)<>(fsStrikeOut in FStyle) then
      inherited SetFlags(8, fsStrikeOut in FStyle);
    FOrientation := FontData.Orientation;
    Changed;
  end;
end;

function TFont.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TFont.GetPitch: TFontPitch;
begin
  Result := FPitch;
end;

function TFont.GetStyle: TFontStyles;
begin
  Result := FStyle;
end;

procedure TFont.Changed;
begin
  if FUpdateCount > 0 then
  begin
    FChanged := True;
    exit;
  end;
  FChanged := False;
  inherited Changed;
  // ToDo: we need interfaces:
  // if FNotify <> nil then FNotify.Changed;
end;

// included by graphics.pp
