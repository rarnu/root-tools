{%MainUnit ../graphics.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TGraphic }

constructor TGraphic.Create;
begin
  inherited Create;
end;

procedure TGraphic.Clear;
begin
end;

procedure TGraphic.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TGraphic) then
      Result := not Equals(TGraphic(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, DoWrite);
end;

function TGraphic.GetPalette: HPALETTE;
begin
  Result:=0;
end;

function TGraphic.GetResourceType: TResourceType;
begin
  Result := RT_RCDATA;
end;

procedure TGraphic.Assign(ASource: TPersistent);
begin
  if ASource = nil
  then Clear
  else inherited Assign(ASource);
end;

procedure TGraphic.Changed(Sender: TObject);
begin
  FModified := True;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphic.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string;
  var DoContinue: boolean);
begin
  DoContinue:=true;
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg, DoContinue);
end;

procedure TGraphic.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
var
  DoContinue: Boolean;
begin
  DoContinue:=true;
  Progress(Sender,Stage,PercentDone,RedrawNow,R,Msg,DoContinue);
end;

{$IF declared(vmtEquals)}
function TGraphic.Equals(Obj: TObject): Boolean;
begin
  if Obj is TGraphic then
    Result := Equals(TGraphic(Obj))
  else
    Result := inherited Equals(Obj);
end;
{$IFEND}

function TGraphic.Equals(Graphic: TGraphic): Boolean;
var
  SelfImage, GraphicsImage: TMemoryStream;
  IsEmpty: boolean;
begin
  Result := (Graphic <> nil) and (ClassType = Graphic.ClassType);
  if not Result then exit;
  IsEmpty:=Empty;
  Result:=(IsEmpty=Graphic.Empty);
  if (not Result) or IsEmpty or (Self=Graphic) then exit;
  SelfImage := TMemoryStream.Create;
  try
    WriteData(SelfImage);
    GraphicsImage := TMemoryStream.Create;
    try
      Graphic.WriteData(GraphicsImage);
      Result := (SelfImage.Size = GraphicsImage.Size) and
             CompareMem(SelfImage.Memory, GraphicsImage.Memory,
                        TCompareMemSize(SelfImage.Size));
    finally
      GraphicsImage.Free;
    end;
  finally
    SelfImage.Free;
  end;
end;

procedure TGraphic.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TGraphic.SetPalette(Value: HPALETTE);
begin

end;

procedure TGraphic.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.LoadFromMimeStream(AStream: TStream; const AMimeType: string);
var
  DefMimeType: String;
begin
  DefMimeType := MimeType;
  if (DefMimeType <> '') and (AMimeType = DefMimeType) then
    LoadFromStream(AStream)
  else
    raise EInvalidGraphic.Create(ClassName+': Unsupported MimeType: ' + AMimeType);
end;

procedure TGraphic.LoadFromResourceID(Instance: THandle; ResID: PtrInt);
var
  Stream: TResourceStream;
  ResType: TResourceType;
begin
  ResType := GetResourceType;
  if ResType = nil then Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, ResType);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.LoadFromResourceName(Instance: THandle; const ResName: String);
var
  Stream: TResourceStream;
  ResType: TResourceType;
begin
  ResType := GetResourceType;
  if ResType = nil then Exit;

  Stream := TResourceStream.Create(Instance, ResName, ResType);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.LoadFromClipboardFormat(FormatID: TClipboardFormat);
begin
  LoadFromClipboardFormatID(ctClipboard,FormatID);
end;

procedure TGraphic.LoadFromClipboardFormatID(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat);
begin
  if OnLoadSaveClipBrdGraphicValid
  and Assigned(OnLoadGraphicFromClipboardFormat) then
    OnLoadGraphicFromClipboardFormat(Self,ClipboardType,FormatID);
end;

procedure TGraphic.SaveToClipboardFormat(FormatID: TClipboardFormat);
begin
  SaveToClipboardFormatID(ctClipboard,FormatID);
end;

procedure TGraphic.SaveToClipboardFormatID(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat);
begin
  if OnLoadSaveClipBrdGraphicValid
  and Assigned(OnSaveGraphicToClipboardFormat) then
    OnSaveGraphicToClipboardFormat(Self,ClipboardType,FormatID);
end;

procedure TGraphic.GetSupportedSourceMimeTypes(List: TStrings);
var
  DefMimeType: String;
begin
  List.Clear;
  DefMimeType := MimeType;
  if DefMimeType <> '' then
    List.Add(DefMimeType);
end;

function TGraphic.GetMimeType: string;
begin
  Result:='';
end;

{-------------------------------------------------------------------------------
  function TGraphic.GetFileExtensions: string;

  Returns standard file extensions for reading and writing separated by
  semicolon and without point. For example: "bmp;xpm"
-------------------------------------------------------------------------------}
class function TGraphic.GetFileExtensions: string;
begin
  Result:='';
end;

class function TGraphic.IsStreamFormatSupported(Stream: TStream): Boolean;
begin
  Result := False;
end;

procedure TGraphic.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.LoadFromLazarusResource(const ResName: String);
var
  Stream: TLazarusResourceStream;
begin
  Stream := nil;
  try
    Stream := TLazarusResourceStream.Create(ResName, nil);
    if LazarusResourceTypeValid(Stream.Res.ValueType)
    then LoadFromStream(Stream)
    else raise EInvalidGraphic.Create(ClassName+': Unsupported Resourcetype: '+Stream.Res.ValueType+' Resource Name: '+ResName);
   finally
     Stream.Free;
   end;
end;

procedure TGraphic.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

function TGraphic.LazarusResourceTypeValid(const AResourceType: string): boolean;
begin
  Result := False;
end;

procedure TGraphic.SetModified(Value: Boolean);
begin
  if Value then
    Changed(Self)
  else
    FModified := False;
end;

// included by graphics.pp
