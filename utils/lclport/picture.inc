{%MainUnit ../graphics.pp}

{ TPicture and help classes TPictureFileFormatList

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  { TPicFileFormatsList }

  PPicFileFormat = ^TPicFileFormat;
  TPicFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string; // ; low case separated list, first is default
    Description: string;
  end;

  TPicFileFormatsList = class(TList)
    // list of TPicFileFormat
  public
    constructor Create;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Add(const Ext, Desc: String; AClass: TGraphicClass);
    function GetFormats(Index: integer): PPicFileFormat;
    function GetFormatExt(Index: integer): String;
    function GetFormatFilter(Index: integer): String;
    function FindExt(const Ext: string): TGraphicClass;
    function FindClassName(const AClassname: string): TGraphicClass;
    function FindByStreamFormat(Stream: TStream): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    procedure BuildFilterStrings(GraphicClass: TGraphicClass;
                                 var Descriptions, Filters: string);
    property Formats[Index: integer]: PPicFileFormat read GetFormats; default;
  end;

constructor TPicFileFormatsList.Create;
begin
  inherited Create;
  // add by priority of use in LCL/IDE
  Add(TPortableNetworkGraphic.GetFileExtensions, rsPortableNetworkGraphic, TPortableNetworkGraphic);
  Add(TPixmap.GetFileExtensions, rsPixmap, TPixmap);
  Add(TBitmap.GetFileExtensions, rsBitmaps, TBitmap);
  Add(TCursorImage.GetFileExtensions, rsCursor, TCursorImage);
  Add(TIcon.GetFileExtensions, rsIcon, TIcon);
  Add(TIcnsIcon.GetFileExtensions, rsIcns, TIcnsIcon);
  Add(TJpegImage.GetFileExtensions, rsJpeg, TJpegImage);
  Add(TTiffImage.GetFileExtensions, rsTiff, TTiffImage);
  Add(TGIFImage.GetFileExtensions, rsGIF, TGIFImage);
  Add(TPortableAnyMapGraphic.GetFileExtensions, rsPortablePixmap, TPortableAnyMapGraphic);
end;

procedure TPicFileFormatsList.Clear;
var
  i: integer;
  P: PPicFileFormat;
begin
  for i:=0 to Count - 1 do
  begin
    P := GetFormats(i);
    Dispose(P);
  end;
  inherited Clear;
end;

procedure TPicFileFormatsList.Delete(Index: Integer);
var
  P: PPicFileFormat;
begin
  P:=GetFormats(Index);
  Dispose(P);
  inherited Delete(Index);
end;

procedure TPicFileFormatsList.Add(const Ext, Desc: String;
  AClass: TGraphicClass);
var
  NewFormat: PPicFileFormat;
begin
  New(NewFormat);
  with NewFormat^ do
  begin
    Extension := AnsiLowerCase(Ext);
    GraphicClass := AClass;
    Description := Desc;
  end;
  inherited Add(NewFormat);
end;

function TPicFileFormatsList.GetFormats(Index: integer): PPicFileFormat;
begin
  Result:=PPicFileFormat(Items[Index]);
end;

function TPicFileFormatsList.GetFormatExt(Index: integer): String;
begin
  Result := PPicFileFormat(Items[Index])^.Extension;
  if Pos(';', Result) > 0 then
    System.Delete(Result, Pos(';', Result), MaxInt);
end;

function TPicFileFormatsList.GetFormatFilter(Index: integer): String;
begin
  Result := StringReplace('*.' + PPicFileFormat(Items[Index])^.Extension, ';', ';*.', [rfReplaceAll]);
end;

function TPicFileFormatsList.FindExt(const Ext: string): TGraphicClass;
var
  I, P: Integer;
  E, ExtList: String;
begin
  if Ext<>'' then
  begin
    E := AnsiLowerCase(Ext);
    if E[1] = '.' then System.Delete(E, 1, 1);
    
    for I := Count - 1 downto 0 do
      with Formats[I]^ do
        if Pos(E, Extension) > 0 then
        begin
          ExtList := Extension;
          repeat
            P := Pos(';', ExtList);
            if (P = 0) and (ExtList = E) or (Pos(E + ';', ExtList) = 1) then
            begin
              Result := GraphicClass;
              Exit;
            end;
            System.Delete(ExtList, 1, P);
          until P = 0;
        end;
  end;
  Result := nil;
end;

function TPicFileFormatsList.FindClassName(const AClassName: string): TGraphicClass;
var
  I: Integer;
begin
  // search backwards so that new formats will be found first
  for I := Count-1 downto 0 do begin
    Result := GetFormats(I)^.GraphicClass;
    if AnsiCompareText(Result.ClassName,AClassname)=0 then
      Exit;
  end;
  Result := nil;
end;

function TPicFileFormatsList.FindByStreamFormat(Stream: TStream): TGraphicClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := GetFormats(I)^.GraphicClass;
    if Result.IsStreamFormatSupported(Stream) then
      Exit;
  end;
  Result := nil;
end;

procedure TPicFileFormatsList.Remove(AClass: TGraphicClass);
// remove all file formats which inherits from AClass
var
  I: Integer;
  P: PPicFileFormat;
begin
  for I := Count - 1 downto 0 do
  begin
    P := GetFormats(I);
    if P^.GraphicClass.InheritsFrom(AClass) then
      Delete(I);
  end;
end;

procedure TPicFileFormatsList.BuildFilterStrings(GraphicClass: TGraphicClass;
  var Descriptions, Filters: string);
var
  C, I: Integer;
  P: PPicFileFormat;
  Filter: String;
begin
  Descriptions := '';
  Filters := '';
  C := 0;
  for I := 0 to Count - 1 do
  begin
    P := GetFormats(I);
    if P^.GraphicClass.InheritsFrom(GraphicClass) and (P^.Extension <> '') then
      with P^ do begin
        if C <> 0 then begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        Filter := GetFormatFilter(I);
        FmtStr(Descriptions, '%s%s (%s)|%s',
             [Descriptions, Description, Filter, Filter]);
        FmtStr(Filters, '%s%s', [Filters, Filter]);
        Inc(C);
      end;
  end;
  if C > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s',
     [rsGraphic, Filters, Descriptions]);
end;

//------------------------------------------------------------------------------

type
  PPicClipboardFormat = ^TPicClipboardFormat;
  TPicClipboardFormat = record
    GraphicClass: TGraphicClass;
    FormatID: TClipboardFormat;
  end;

  TPicClipboardFormats = class(TList)
    // list of TPicClipboardFormat
  private
    function GetFormats(Index: integer): PPicClipboardFormat;
  public
    constructor Create;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Add(AFormatID: TClipboardFormat; AClass: TGraphicClass);
    function FindFormat(FormatID: TClipboardFormat): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    property Formats[Index: integer]: PPicClipboardFormat read GetFormats; default;
  end;

function TPicClipboardFormats.GetFormats(Index: integer): PPicClipboardFormat;
begin
  Result:=PPicClipboardFormat(Items[Index]);
end;

constructor TPicClipboardFormats.Create;
const
  sMimeTypePng = 'image/png';
  sMimeTypeJpg = 'image/jpeg';
begin
  inherited Create;
  Add(PredefinedClipboardFormat(pcfBitmap), TBitmap);
  Add(PredefinedClipboardFormat(pcfDelphiBitmap), TBitmap);
  Add(PredefinedClipboardFormat(pcfPixmap), TPixmap);
  //Add(PredefinedClipboardFormat(pcfIcon), TCustomIcon);
  Add(ClipboardRegisterFormat(sMimeTypePng), TPortableNetworkGraphic);
  Add(ClipboardRegisterFormat(sMimeTypeJpg), TJPegImage);
end;

procedure TPicClipboardFormats.Clear;
var
  i: integer;
  P: PPicClipboardFormat;
begin
  for i := 0 to Count - 1 do
  begin
    P := GetFormats(i);
    Dispose(P);
  end;
  inherited Clear;
end;

procedure TPicClipboardFormats.Delete(Index: Integer);
var
  P: PPicClipboardFormat;
begin
  P := GetFormats(Index);
  Dispose(P);
  inherited Delete(Index);
end;

procedure TPicClipboardFormats.Add(AFormatID: TClipboardFormat;
  AClass: TGraphicClass);
var NewFormat: PPicClipboardFormat;
begin
  if AFormatID=0 then exit;
  New(NewFormat);
  with NewFormat^ do begin
    GraphicClass:=AClass;
    FormatID:=AFormatID;
  end;
  inherited Add(NewFormat);
end;

function TPicClipboardFormats.FindFormat(
  FormatID: TClipboardFormat): TGraphicClass;
var
  I: Integer;
  P: PPicClipboardFormat;
begin
  for I := Count-1 downto 0 do begin
    P:=GetFormats(i);
    if P^.FormatID=FormatID then begin
      Result:=P^.GraphicClass;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TPicClipboardFormats.Remove(AClass: TGraphicClass);
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    if GetFormats(i)^.GraphicClass.InheritsFrom(AClass) then
      Delete(i);
end;

//------------------------------------------------------------------------------

var
  PicClipboardFormats: TPicClipboardFormats=nil;
  PicFileFormats: TPicFileFormatsList=nil;

function GetPicFileFormats: TPicFileFormatsList;
begin
  if not Assigned(PicFileFormats) and not GraphicsFinalized then
    PicFileFormats := TPicFileFormatsList.Create;
  Result := PicFileFormats;
end;

function GetPicClipboardFormats: TPicClipboardFormats;
begin
  if (PicClipboardFormats = nil) and (not GraphicsFinalized) then
    PicClipboardFormats := TPicClipboardFormats.Create;
  Result := PicClipboardFormats;
end;

function GraphicFilter(GraphicClass: TGraphicClass): string;
var
  Filters: string;
begin
  Result := '';
  GetPicFileFormats.BuildFilterStrings(GraphicClass,Result,Filters);
end;

function GraphicExtension(GraphicClass: TGraphicClass): string;
var
  I: Integer;
  PicFormats: TPicFileFormatsList;
begin
  PicFormats := GetPicFileFormats;
  for I := PicFormats.Count-1 downto 0 do
    if PicFormats[I]^.GraphicClass.ClassName = GraphicClass.ClassName then
    begin
      Result := PicFormats.GetFormatExt(I);
      Exit;
    end;
  Result := '';
end;

function GraphicFileMask(GraphicClass: TGraphicClass): string;
var
  Descriptions: string;
begin
  Result := '';
  GetPicFileFormats.BuildFilterStrings(GraphicClass,Descriptions,Result);
end;

function GetGraphicClassForFileExtension(const FileExt: string): TGraphicClass;
begin
  Result:=GetPicFileFormats.FindExt(FileExt);
end;

//--TPicture--------------------------------------------------------------------


constructor TPicture.Create;
begin
  inherited Create;
  GetPicFileFormats;
  GetPicClipboardFormats;
end;

destructor TPicture.Destroy;
begin
  FGraphic.Free;
  inherited Destroy;
end;

procedure TPicture.AssignTo(Dest: TPersistent);
begin
  if Graphic is Dest.ClassType then
    Dest.Assign(Graphic)
  else
    inherited AssignTo(Dest);
end;

procedure TPicture.ForceType(GraphicType: TGraphicClass);
var
  NewGraphic: TGraphic;
begin
  if not (FGraphic is GraphicType) then
  begin
    NewGraphic := GraphicType.Create;
    NewGraphic.Assign(FGraphic);
    FGraphic.Free;
    FGraphic := NewGraphic;
    FGraphic.OnChange := @Changed;
    FGraphic.OnProgress := @Progress;
    Changed(Self);
  end;
end;

function TPicture.GetBitmap: TBitmap;
begin
  ForceType(TBitmap);
  Result := TBitmap(Graphic);
end;

function TPicture.GetPNG: TPortableNetworkGraphic;
begin
  ForceType(TPortableNetworkGraphic);
  Result := TPortableNetworkGraphic(Graphic);
end;

function TPicture.GetPNM: TPortableAnyMapGraphic;
begin
  ForceType(TPortableAnyMapGraphic);
  Result := TPortableAnyMapGraphic(Graphic);
end;

function TPicture.GetPixmap: TPixmap;
begin
  ForceType(TPixmap);
  Result := TPixmap(Graphic);
end;

function TPicture.GetIcon: TIcon;
begin
  ForceType(TIcon);
  Result := TIcon(Graphic);
end;

function TPicture.GetJpeg: TJpegImage;
begin
  ForceType(TJpegImage);
  Result := TJpegImage(Graphic);
end;

procedure TPicture.SetBitmap(Value: TBitmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetPNG(const AValue: TPortableNetworkGraphic);
begin
  SetGraphic(AValue);
end;

procedure TPicture.SetPNM(const AValue: TPortableAnyMapGraphic);
begin
  SetGraphic(AValue);
end;

procedure TPicture.SetPixmap(Value: TPixmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetIcon(Value: TIcon);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetJpeg(Value: TJpegImage);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetGraphic(Value: TGraphic);
var
  NewGraphic: TGraphic;
  ok: boolean;
begin
  if (Value=FGraphic) then exit;
  NewGraphic := nil;
  ok := False;
  try
    if Value <> nil then
    begin
      NewGraphic := TGraphicClass(Value.ClassType).Create;
      NewGraphic.Assign(Value);
      NewGraphic.OnChange := @Changed;
      NewGraphic.OnProgress := @Progress;
    end;
    FGraphic.Free;
    FGraphic := NewGraphic;
    Changed(Self);
    ok := True;
  finally
    // this try..finally construction will in case of an exception
    // not alter the error backtrace output
    if not ok then
      NewGraphic.Free;
  end;
end;

{ Based on the extension of Filename, create the corresponding TGraphic class
  and call its LoadFromFile method. }

procedure TPicture.LoadFromFile(const Filename: string);
var
  Ext: string;
  Stream: TStream;
begin
  Ext := ExtractFileExt(Filename);
  System.Delete(Ext, 1, 1); // delete '.'
  
  Stream := TFileStreamUTF8.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    if Ext <> '' then
      LoadFromStreamWithFileExt(Stream, Ext)
    else
      LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TPicture.LoadFromResourceName(Instance: THandle; const ResName: String);
var
  NewGraphic: TGraphic;
begin
  NewGraphic := CreateGraphicFromResourceName(Instance, ResName);
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := @Changed;
  Changed(Self);
end;

procedure TPicture.LoadFromResourceName(Instance: THandle;
  const ResName: String; AClass: TGraphicClass);
var
  NewGraphic: TGraphic;
  ok: Boolean;
begin
  NewGraphic := AClass.Create;
  ok:=false;
  try
    NewGraphic.OnProgress := @Progress;
    NewGraphic.LoadFromResourceName(Instance, ResName);
    ok:=true;
  finally
    // this try..finally construction will in case of an exception
    // not alter the error backtrace output
    if not ok then NewGraphic.Free;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := @Changed;
  Changed(Self);
end;

procedure TPicture.LoadFromLazarusResource(const AName: string);
var
  Stream: TLazarusResourceStream;
begin
  Stream := TLazarusResourceStream.Create(AName, nil);
  try
    LoadFromStreamWithFileExt(Stream, Stream.Res.ValueType);
  finally
    Stream.Free;
  end;
end;

procedure TPicture.LoadFromStream(Stream: TStream);
var
  GraphicClass: TGraphicClass;
begin
  GraphicClass := GetPicFileFormats.FindByStreamFormat(Stream);
  if GraphicClass = nil then
    raise EInvalidGraphic.Create(rsUnknownPictureFormat);
  LoadFromStreamWithClass(Stream, GraphicClass);
end;

procedure TPicture.SaveToFile(const Filename: string; const FileExt: string = '');
var
  Ext: string;
  Stream: TStream;
begin
  if FileExt <> '' then
    Ext := AnsiLowerCase(FileExt)
  else
    Ext := AnsiLowerCase(ExtractFileExt(Filename));
    
  if (Ext <> '') and (Ext[1] = '.') then System.Delete(Ext, 1, 1); // delete '.'

  Stream := TFileStreamUTF8.Create(Filename, fmCreate);
  try
    SaveToStreamWithFileExt(Stream, Ext);
  finally
    Stream.Free;
  end;
end;

procedure TPicture.SaveToStream(Stream: TStream);
begin
  if Assigned(Graphic) then
    Graphic.SaveToStream(Stream);
end;

procedure TPicture.SaveToStreamWithFileExt(Stream: TStream; const FileExt: string);
var
  GraphicClass: TGraphicClass;
  IntfImg: TLazIntfImage;
  ImgWriter: TFPCustomImageWriter;
  fpBmp: TFPImageBitmap;
begin
  if Graphic = nil then Exit;
  if FileExt <> '' then
    GraphicClass := FindGraphicClassWithFileExt(FileExt);

  if (FileExt = '')
  or (Graphic is GraphicClass)
  then begin
    Graphic.SaveToStream(Stream);
    Exit;
  end;

  // save in different format
  if (Graphic is TFPImageBitmap) and GraphicClass.InheritsFrom(TFPImageBitmap)
  then begin
    fpBmp := TFPImageBitmap(Graphic);
    ImgWriter := nil;
    IntfImg := TLazIntfImage.Create(0,0,[]);
    try
      ImgWriter := TFPImageBitmapClass(GraphicClass).GetWriterClass.Create;
      IntfImg.SetRawImage(fpBmp.GetRawImagePtr^, False);
      fpBmp.InitializeWriter(IntfImg, ImgWriter);
      IntfImg.SaveToStream(Stream, ImgWriter);
      fpBmp.FinalizeWriter(ImgWriter);
    finally
      IntfImg.Free;
      ImgWriter.Free;
    end;
    Exit;
  end;

  // no conversion available yet
  raise Exception.CreateFmt('TODO: Conversion for vector or icon images of format "%s" to "%s"!', [Graphic.GetFileExtensions, FileExt]);
end;

procedure TPicture.LoadFromStreamWithFileExt(Stream: TStream;
  const FileExt: string);
begin
  LoadFromStreamWithClass(Stream, FindGraphicClassWithFileExt(FileExt));
end;

procedure TPicture.LoadFromClipboardFormat(FormatID: TClipboardFormat);
begin
  LoadFromClipboardFormatID(ctClipboard,FormatID);
end;

procedure TPicture.LoadFromClipboardFormatID(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat);
var
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
  ok: boolean;
begin
  GraphicClass := PicClipboardFormats.FindFormat(FormatID);
  if GraphicClass = nil then
    raise EInvalidGraphic.CreateFmt(rsUnsupportedClipboardFormat,
      [ClipboardFormatToMimeType(FormatID)]);

  NewGraphic := GraphicClass.Create;
  ok:=false;
  try
    NewGraphic.OnProgress := @Progress;
    NewGraphic.LoadFromClipboardFormatID(ClipboardType,FormatID);
    ok:=true;
  finally
    if not ok then NewGraphic.Free;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := @Changed;
  Changed(Self);
end;

procedure TPicture.SaveToClipboardFormat(FormatID: TClipboardFormat);
begin
  if FGraphic <> nil then
    FGraphic.SaveToClipboardFormat(FormatID);
end;

class function TPicture.SupportsClipboardFormat(FormatID: TClipboardFormat): Boolean;
begin
  Result := GetPicClipboardFormats.FindFormat(FormatID) <> nil;
end;

procedure TPicture.Assign(Source: TPersistent);
begin
  if Source = nil then
    SetGraphic(nil)
  else if Source is TPicture then
    SetGraphic(TPicture(Source).Graphic)
  else if Source is TGraphic then
    SetGraphic(TGraphic(Source))
  else if Source is TFPCustomImage then
    Bitmap.Assign(Source)
  else
    inherited Assign(Source);
end;

class procedure TPicture.RegisterFileFormat(const AnExtension,
  ADescription: string; AGraphicClass: TGraphicClass);
begin
  GetPicFileFormats.Add(AnExtension, ADescription, AGraphicClass);
end;

class procedure TPicture.RegisterClipboardFormat(FormatID: TClipboardFormat;
  AGraphicClass: TGraphicClass);
begin
  GetPicClipboardFormats.Add(FormatID, AGraphicClass);
end;

class procedure TPicture.UnregisterGraphicClass(AClass: TGraphicClass);
begin
  if PicFileFormats <> nil then PicFileFormats.Remove(AClass);
  if PicClipboardFormats <> nil then PicClipboardFormats.Remove(AClass);
end;

procedure TPicture.Clear;
begin
  SetGraphic(nil);
end;

function TPicture.FindGraphicClassWithFileExt(const Ext: string;
  ExceptionOnNotFound: boolean): TGraphicClass;
var
  FileExt: String;
begin
  FileExt := Ext;
  if (FileExt <> '') and (FileExt[1] = '.') then
    FileExt := Copy(FileExt, 2, length(FileExt));
  Result := GetPicFileFormats.FindExt(FileExt);
  if (Result = nil) and ExceptionOnNotFound then
    raise EInvalidGraphic.CreateFmt(rsUnknownPictureExtension, [Ext]);
end;

procedure TPicture.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPicture.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string;
  var DoContinue: boolean);
begin
  DoContinue:=true;
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg, DoContinue);
end;

procedure TPicture.LoadFromStreamWithClass(Stream: TStream; AClass: TGraphicClass);
var
  NewGraphic: TGraphic;
  ok: Boolean;
begin
  NewGraphic := AClass.Create;
  ok:=false;
  try
    NewGraphic.OnProgress := @Progress;
    NewGraphic.LoadFromStream(Stream);
    ok:=true;
  finally
    // this try..finally construction will in case of an exception
    // not alter the error backtrace output
    if not ok then NewGraphic.Free;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := @Changed;
  Changed(Self);
end;

procedure TPicture.ReadData(Stream: TStream);
var
  GraphicClassName: Shortstring;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
  ok: boolean;
begin
  Stream.Read(GraphicClassName[0], 1);
  Stream.Read(GraphicClassName[1], length(GraphicClassName));
  GraphicClass := GetPicFileFormats.FindClassName(GraphicClassName);
  NewGraphic := nil;
  if GraphicClass <> nil then begin
    NewGraphic := GraphicClass.Create;
    ok:=false;
    try
      NewGraphic.ReadData(Stream);
      ok:=true;
    finally
      if not ok then NewGraphic.Free;
    end;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  if NewGraphic <> nil then begin
    NewGraphic.OnChange := @Changed;
    NewGraphic.OnProgress := @Progress;
  end;
  Changed(Self);
end;

procedure TPicture.WriteData(Stream: TStream);
var
  GraphicClassName: ShortString;
begin
  with Stream do
  begin
    if Graphic <> nil then
      GraphicClassName := Graphic.ClassName
    else
      GraphicClassName := '';
    Write(GraphicClassName, Length(GraphicClassName) + 1);
    if Graphic <> nil then
      Graphic.WriteData(Stream);
  end;
end;

procedure TPicture.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    Ancestor: TPicture;
  begin
    if Assigned(Filer.Ancestor) and (Filer.Ancestor is TPicture) then
    begin
      Ancestor := TPicture(Filer.Ancestor);
      if not Assigned(Graphic) then
        Exit(Assigned(Ancestor.Graphic));
      Result := not Graphic.Equals(Ancestor.Graphic);
    end
    else
      Result := Assigned(Graphic);
  end;

begin
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, DoWrite);
end;

function TPicture.GetWidth: Integer;
begin
  if FGraphic <> nil then
    Result := FGraphic.Width
  else
    Result := 0;
end;

function TPicture.GetHeight: Integer;
begin
  Result := 0;
  if FGraphic <> nil then
    Result := FGraphic.Height
  else
    Result := 0;
end;

// included by graphics.pp
