{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclUnicode.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Mike Lischke (public att lischke-online dott de).  }
{ Portions created by Mike Lischke are Copyright (C) 1999-2000 Mike Lischke. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Andreas Hausladen (ahuser)                                                                     }
{   Mike Lischke                                                                                   }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Peter Schraut (http://www.console-dev.de)                                                      }
{   Florent Ouchet (outchy)                                                                        }
{   glchapman                                                                                      }
{                                                                              			   }
{  Modified by Eugene Kryukov                                       	                           }
{                                                                  	                           }
{  Latest change can be found at:                                  	                           }
{    http://www.ksdev.com/mplfiles/                                 	                           }
{                                                                  	                           }
{**************************************************************************************************}

{ TvgWideStrings ==============================================================}

constructor TvgWideStrings.Create;
begin
  inherited Create;
end;

destructor TvgWideStrings.Destroy;
begin
  inherited;
end;

function TvgWideStrings.CompareStrings(const S1, S2: WideString): Integer;
begin
  Result := 0;
end;

procedure TvgWideStrings.Delete(Index: Integer);
begin
end;

function TvgWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TvgWideStrings.Items(AIndex: integer): WideString;
begin
  Result := Strings[AIndex];
end;

function TvgWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TvgWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TvgWideStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TvgWideStrings.AddStrings(Strings: TvgWideStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TvgWideStrings.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TvgWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TvgWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to TStrings(Source).Count - 1 do
          AddObject(TStrings(Source)[I], TStrings(Source).Objects[I]);
      finally
        EndUpdate;
      end;
    end
    else inherited Assign(Source);
end;

procedure TvgWideStrings.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStrings then
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Self.Count - 1 do
          AddObject(Self[I], Self.Objects[I]);
      finally
        EndUpdate;
      end;
    end
    else
      if Dest is TvgWideStrings then
        with Dest as TvgWideStrings do
        begin
          BeginUpdate;
          try
            Clear;
            AddStrings(Self);
          finally
            EndUpdate;
          end;
        end
        else inherited;
end;

procedure TvgWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  System.Inc(FUpdateCount);
end;

procedure TvgWideStrings.clear;
begin

end;

procedure TvgWideStrings.DefineProperties(Filer: TFiler);
  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TvgWideStrings then
        Result := not Equals(TvgWideStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;
begin
  Filer.DefineProperty('strings', ReadData, WriteData, DoWrite);
end;

procedure TvgWideStrings.EndUpdate;
begin
  System.Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TvgWideStrings.Equals(Strings: TvgWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TvgWideStrings.Error(const Msg: String; Data: Integer);
begin

end;

procedure TvgWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TvgWideStrings.GetCapacity: Integer;
begin
  Result := Count;
end;

function TvgWideStrings.GetCommaText: WideString;
var
  S: WideString;
  P: PWideChar;
  I,
  Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := ''
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [vgWideNull..vgWideSpace, System.WideChar('"'), System.WideChar(',')]) do
       System.Inc(P);
//!!!      if (P^ <> WideNull) then S := WideQuotedStr(S, '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TvgWideStrings.GetName(Index: Integer): WideString;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos(WideString('='), Result);
  if P > 0 then SetLength(Result, P - 1)
           else Result := '';
end;

function TvgWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TvgWideStrings.GetTextStr: WideString;
var
  I, L, Size, Count: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do
    System.Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, 2 * L);
      System.Inc(P, L);
    end;
    P^ := vgWideCarriageReturn;
    System.Inc(P);
    P^ := vgWideLineFeed;
    System.Inc(P);
  end;
end;

function TvgWideStrings.GetValueFromIndex(Index: Integer): WideString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, System.MaxInt) else
    Result := '';
end;

procedure TvgWideStrings.SetValueFromIndex(Index: Integer; const Value: WideString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + '=' + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

procedure TvgWideStrings.sort;
begin
end;

function TvgWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then Result := Copy(Get(I), Length(Name) + 2, System.MaxInt)
            else Result := '';
end;

function TvgWideStrings.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to GetCount - 1 do
  begin
    if CompareStrings(Get(Result), S) = 0 then Exit;
  end;
  Result := -1;
end;

function TvgWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos(WideString('='), S);
    if (P > 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TvgWideStrings.IndexOfValue(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos(WideString('='), S);
    if (P > 0) and (CompareStrings(Copy(S, P + 1, Length(S)), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TvgWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TvgWideStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TvgWideStrings.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    //RaiseLastWin32Error;
  end;
end;

procedure TvgWideStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  Order: System.WideChar;
  SW: WideString;
  SA: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    Stream.Read(Order, 2);
    if (Order = BOM_LSB_FIRST) or (Order = BOM_MSB_FIRST) then
    begin
      // unicode
      SetLength(SW, (Size - 2) div 2);
      Stream.Read(PWideChar(SW)^, Size - 2);
      SetTextStr(SW);
    end
    else
    begin
      // ansi
      Stream.Position := Stream.Position - 2;
      SetLength(SA, Size);
      Stream.Read(PAnsiChar(SA)^, Size);
      SetTextStr(SA);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TvgWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TvgWideStrings.Put(Index: Integer; const S: WideString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TvgWideStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TvgWideStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadWideString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TvgWideStrings.SaveToFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TvgWideStrings.SaveToAnsiFile(const FileName: String);
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    S.Text := Text;
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

procedure TvgWideStrings.SaveToStream(Stream: TStream);
var
  SW, BOM: WideString;
begin
  SW := GetTextStr;
  // only save if allowed
  BOM := BOM_LSB_FIRST;
  Stream.WriteBuffer(PWideChar(BOM)^, 2);
  // SW has already been filled
  Stream.WriteBuffer(PWideChar(SW)^, 2 * Length(SW));
end;

procedure TvgWideStrings.SetCapacity(NewCapacity: Integer);
begin
end;

procedure TvgWideStrings.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

procedure TvgWideStrings.SetCommaText(const Value: WideString);
var
  S: WideString;
  P, P1: PWideChar;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [System.WideChar(#1)..vgWideSpace] do System.Inc(P);
    while P^ <> vgWideNull do
    begin
      if P^ = '"' then
//!!!        S := WideExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > vgWideSpace) and (P^ <> ',') do System.Inc(P);
        System.SetString(S, P1, P - P1);
      end;
      Add(S);

      while P^ in [System.WideChar(#1)..vgWideSpace] do
        System.Inc(P);
      if P^ = ',' then
        repeat
          System.Inc(P);
        until not (P^ in [System.WideChar(#1)..vgWideSpace]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TvgWideStrings.SetItem(index: integer; AText: wideString);
begin
  Strings[index] := AText;
end;

procedure TvgWideStrings.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TvgWideStrings.SetText(Text: PWideChar);
begin
  SetTextStr(Text);
end;

procedure TvgWideStrings.SetTextStr(const Value: WideString);
var
  Head, Tail: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> vgWideNull do
    begin
      Tail := Head;
      while not (Tail^ in [vgWideNull, vgWideLineFeed, vgWideCarriageReturn, vgWideVerticalTab, vgWideFormFeed]) and
            (Tail^ <> vgWideLineSeparator) and (Tail^ <> vgWideParagraphSeparator) do
        System.Inc(Tail);
      System.SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> vgWideNull then
      begin
        System.Inc(Head);
        if (Tail^ = vgWideCarriageReturn) and (Head^ = vgWideLineFeed) then
          System.Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TvgWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TvgWideStrings.SetValue(const Name, Value: WideString);
var
  I : Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
    if I >= 0 then Delete(I);
end;

procedure TvgWideStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count-1 do
    Writer.WriteWideString(Get(I));
  Writer.WriteListEnd;
end;

{ TvgWideStringList ===========================================================}

destructor TvgWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TvgWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
  begin
    if sortByObject then
    begin
      Result := FCount
    end
    else
    begin
      if Find(S, Result) then
        case Duplicates of
          dupIgnore:
            Exit;
          dupError:
            Error('Doublicate string', 0);
        end;
    end;
  end;
  InsertItem(Result, S);
end;

procedure TvgWideStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TvgWideStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TvgWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

function TvgWideStringList.CompareStrings(const S1, S2: WideString): Integer;
begin
  {$IFDEF KS_COMPILER6_UP}
  if CaseSensitive then
    Result := WideCompareStr(S1, S2)
  else
    Result := WideCompareText(S1, S2);
  {$ELSE}
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
  {$ENDIF}
end;

procedure TvgWideStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error('List index error', Index);
  Changing;
  FList[Index].FString := '';
  System.Dec(FCount);
  if Index < FCount then System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TWideStringItem));
  Changed;
end;

procedure TvgWideStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error('List index error', Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error('List index error', Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TvgWideStringList.ExchangeItems(Index1, Index2: Integer);
var 
  P1,P2: Pointer;
begin
  P1:=Pointer(Flist^[Index1].FString);
  P2:=Pointer(Flist^[Index1].FObject);
  Pointer(Flist^[Index1].FString) := Pointer(Flist^[Index2].FString);
  Pointer(Flist^[Index1].FObject) := Pointer(Flist^[Index2].FObject);
  Pointer(Flist^[Index2].FString) := P1;
  Pointer(Flist^[Index2].FObject) := P2;
end;

function TvgWideStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TvgWideStringList.FindByObject(const S: TObject;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := goodCompareObj(FList^[I].FObject, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TvgWideStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    Error('List index error', Index);
    Result := '';
  end
  else
    Result := FList[Index].FString;
end;

function TvgWideStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TvgWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TvgWideStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error('List index error', Index);
  Result := FList[Index].FObject;
end;

procedure TvgWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TvgWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then Result := -1;
end;

procedure TvgWideStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then Error('Sorted list error', 0);
  if (Index < 0) or (Index > FCount) then Error('List index error', Index);
  InsertItem(Index, S);
end;

procedure TvgWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  System.Inc(FCount);
  Changed;
end;

procedure TvgWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then Error('Sorted list error', 0);
  if (Index < 0) or (Index >= FCount) then Error('List index error', Index);
  Changing;
  FList[Index].FString := S;
  Changed;
end;

procedure TvgWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error('List index error', Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TvgWideStringList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: WideString;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      {$IFDEF KS_COMPILER6_UP}
      while WideCompareText(FList[I].FString, P) < 0 do System.Inc(I);
      while WideCompareText(FList[J].FString, P) > 0 do System.Dec(J);
      {ELSE}
      while CompareText(FList[I].FString, P) < 0 do System.Inc(I);
      while CompareText(FList[J].FString, P) > 0 do System.Dec(J);
      {$ENDIF}
      if I <= J then
      begin
        ExchangeItems(I, J);
        System.Inc(I);
        System.Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TvgWideStringList.QuickSortByObject(L, R: Integer);
var
  I, J: Integer;
  P: TObject;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FObject;
    repeat
      while goodCompareObj(FList[I].FObject, P) < 0 do System.Inc(I);
      while goodCompareObj(FList[J].FObject, P) > 0 do System.Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        System.Inc(I);
        System.Dec(J);
      end;
    until I > J;
    if L < J then QuickSortByObject(L, J);
    L := I;
  until I >= R;
end;

procedure TvgWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TWideStringItem));
  FCapacity := NewCapacity;
end;

procedure TvgWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing
              else Changed;
end;

procedure TvgWideStringList.Sort;
begin
  if not Sorted and (Count > 1) then
  begin
    Changing;
    if FsortByObject then
      QuickSortByObject(0, FCount - 1)
    else
      QuickSort(0, FCount - 1);
    Changed;
  end;
end;

