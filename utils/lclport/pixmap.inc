{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TPixmap
 ******************************************************************************
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function TestStreamIsXPM(const AStream: TStream): boolean;
type
  TXPMRange = (xrCode,xrStaticKeyWord, xrCharKeyWord);
  
  function Check(const s: string): boolean;
  var
    Buf: string;
  begin
    Result:=false;
    SetLength(Buf,length(s));
    if AStream.Read(Buf[1],length(Buf))<>length(Buf) then exit;
    if Buf<>s then exit;
    Result:=true;
  end;
  
var
  OldPosition: TStreamSeekType;
  c: char;
  Range: TXPMRange;
  LastChar: Char;
begin
  Result:=false;
  //DebugLn('TestStreamIsXPM ');
  OldPosition:=AStream.Position;
  // skip comments and search 'static [const] [unsigned] char*'
  Range:=xrCode;
  try
    repeat
      if AStream.Read(c,1)<>1 then exit;
      case c of
      ' ',#9,#10,#13: ;
      '/':
        begin
          // read comment
          if AStream.Read(c,1)<>1 then exit;
          if c<>'*' then exit;
          repeat
            LastChar:=c;
            if AStream.Read(c,1)<>1 then exit;
            if c in [#0..#8,#11,#12,#14..#31] then exit;
          until (c='/') and (LastChar='*');
        end;
      's':
        begin
          if Range<>xrCode then exit;
          // read 'tatic', the rest of 'static'
          if not Check('tatic') then exit;
          Range:=xrStaticKeyWord;
          // read space
          if (AStream.Read(c,1)<>1) or (not (c in [' ',#9,#10,#13])) then exit;
        end;
      'c':
        begin
          if Range<>xrStaticKeyWord then exit;
          if (AStream.Read(c,1)<>1) then exit;
          if (c='o') then begin
            // read 'nst', the rest of 'const'
            if not Check('nst') then exit;
          end else if (c='h') then begin
            // read 'ar', the rest of 'char'
            if not Check('ar') then exit;
            Range:=xrCharKeyWord;
          end else
            exit;
        end;
      'u':
        begin
          if Range<>xrStaticKeyWord then exit;
          // read 'nsigned', the rest of 'unsigned'
          if not Check('nsigned') then exit;
        end;
      '*':
        begin
          if Range<>xrCharKeyWord then exit;
          Result:=true;
          exit;
        end;
      else
        exit;
      end;
    until false;

  finally
    AStream.Position:=OldPosition;
  end;
end;

{-------------------------------------------------------------------------------
  function XPMToPPChar
  Params: const XPM: string
  Result: PPChar
  
  Converts the source of an XPM image file into an array of PChar.
  The memory is allocated as one block, so freeing can simply be done via
  FreeMem(TheResult). The data is just behind the array.
  
  An XPM file can be included in a C source file, because it has a C syntax.
  In the compiled program it is simply an array of PChar.
  This function converts an XPM source file to the same array, by removing the
  comments and the string constant symbols.
-------------------------------------------------------------------------------}
function XPMToPPChar(const XPM: string): PPChar;
var
  TrimmedXPM: string;
  SrcLen, SrcPos, DestPos, LineCount, CurLine, LineLen: integer;
  LineStart: PChar;
begin
  Result := nil;
  // trim all comments and symbols and extract only the xpm data between ""
  SrcLen:=length(XPM);
  SetLength(TrimmedXPM,SrcLen+1); // +1 because TrimmedXPM should be at least
                                  // one byte
  SrcPos:=1;
  DestPos:=1;
  LineCount:=1;
  while (SrcPos<=SrcLen) do begin
    case XPM[SrcPos] of

    '/':
      begin
        if (SrcPos<SrcLen) and (XPM[SrcPos+1]='*') then begin
          // this is a C comment
          // -> skip comment
          inc(SrcPos,2);
          while (SrcPos<SrcLen) do begin
            if (XPM[SrcPos]='*') and (XPM[SRcPos+1]='/') then begin
              // comment end found
              inc(SrcPos,2);
              break;
            end;
            inc(SrcPos);
          end;
        end;
      end;

    '"':
      begin
        // start of a string constant
        inc(SrcPos);
        while (SrcPos<SrcLen) do begin
          if XPM[SrcPos]='\' then
            inc(SrcPos);
          if (XPM[SrcPos]='"') and (XPM[SrcPos-1]<>'\') then begin
            // string end found
            inc(SrcPos);
            break;
          end;
          // copy char
          TrimmedXPM[DestPos]:=XPM[SrcPos];
          inc(DestPos);
          inc(SrcPos);
        end;
      end;

    ',':
      begin
        // new string constant
        // -> add #0 char
        inc(SrcPos);
        TrimmedXPM[DestPos]:=#0;
        inc(DestPos);
        inc(LineCount);
      end;

    else
      inc(SrcPos);
    end;
  end;
  TrimmedXPM[DestPos]:=#0; // DestPos now contains the length of trimmed data

  // create array of PChar + image data
  GetMem(Result,SizeOf(PChar)*(LineCount+1)+DestPos);

  // copy data
  Move(TrimmedXPM[1],Result[LineCount+1],DestPos);

  // calculate the array of PChar
  LineStart:=PChar(@Result[LineCount+1]);
  for CurLine:=0 to LineCount-1 do begin
    LineLen:=StrLen(LineStart)+1;
    Result[CurLine]:=LineStart;
    inc(LineStart,LineLen);
  end;
  Result[LineCount]:=nil; // mark the end of the array with a nil
end;

{-------------------------------------------------------------------------------
  function LazResourceXPMToPPChar
  Params: const ResourceName: string
  Result: PPChar

  Loads the source of an XPM image file from the LazarusResources into an array
  of PChar.
  See XPMToPPChar for more info.
-------------------------------------------------------------------------------}
function LazResourceXPMToPPChar(const ResourceName: string): PPChar;
var XPMSource: TLResource;
begin
  XPMSource:=LazarusResources.Find(ResourceName);
  if XPMSource.ValueType<>'XPM' then
    raise Exception.Create('LazResourceXPMToPPChar: The resource "'
                           +ResourceName+'" is not of type XPM');
  Result:=XPMToPPChar(XPMSource.Value);
end;

{-------------------------------------------------------------------------------
  function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;

  Converts the source of an XPM image file into an array of PChar.
  See XPMToPPChar for more info.
-------------------------------------------------------------------------------}
function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;
var
  s: string;
begin
  Result:=nil;
  if Size<=0 then exit;
  SetLength(s,Size);
  Stream.Read(s[1],Size);
  Result:=XPMToPPChar(s);
end;

{-------------------------------------------------------------------------------
  function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
    ): boolean;
    
  Reads the first line of an XPM PChar array, whcih contains the width, height
  and number of colors of the XPM.
-------------------------------------------------------------------------------}
function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
  ): boolean;
var
  LinePos: PChar;
  
  function ReadNumber(var i: integer): boolean;
  begin
    Result:=false;
    // skip space
    while (LinePos^ in [' ',#9]) do inc(LinePos);
    // read number
    i:=0;
    while (LinePos^ in ['0'..'9']) do begin
      i:=i*10+ord(LinePos^)-ord('0');
      inc(LinePos);
      Result:=true;
    end;
  end;
  
begin
  Result:=false;
  if (XPM=nil) or (XPM[0]=nil) then exit;
  LinePos:=XPM[0];
  if not ReadNumber(Width) then exit;
  if not ReadNumber(Height) then exit;
  if not ReadNumber(ColorCount) then exit;
  Result:=true;
end;

{ TPixmap }

class function TPixmap.GetFileExtensions: string;
begin
  Result := 'xpm';
end;

class function TPixmap.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TLazReaderXPM;
end;

class function TPixmap.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedPixmap;
end;

class function TPixmap.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TLazWriterXPM;
end;

function TPixmap.LazarusResourceTypeValid(const ResourceType: string): boolean;
begin
  Result:=(ResourceType='XPM');
end;

