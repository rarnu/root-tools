unit LazFreeTypeIntfDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics, FPimage;

type
  TLazIntfImageGetPixelAtProc = procedure(p: pointer; out Color: TFPColor);
  TLazIntfImageSetPixelAtProc = procedure(p: pointer; const Color: TFPColor);
  TLazIntfHorizLineProc = procedure(x1,y,x2: integer; const Color: TFPColor) of object;

implementation

uses LCLType, GraphType;

type
  PFPColorBytes = ^TFPColorBytes;
  TFPColorBytes = record
    {$ifdef ENDIAN_LITTLE}
    Rl, Rh, Gl, Gh, Bl, Bh, Al, Ah: Byte;
    {$else}
    Rh, Rl, Gh, Gl, Bh, Bl, Ah, Al: Byte;
    {$endif}
  end;

  PFourBytes = ^TFourBytes;
  TFourBytes = record
    B0, B1, B2, B3: Byte;
  end;

{ TIntfFreeTypeDrawer }

procedure InternalGetPixelAtWithoutAlphaRGB(p: pointer; out Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B0;
    TFPColorBytes(color).Rl := B0;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B2;
    TFPColorBytes(color).Bl := B2;
    color.alpha := $ffff;
  end;
end;

procedure InternalSetPixelAtWithoutAlphaRGB(p: pointer; const Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    B0 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B2 := TFPColorBytes(color).Bh;
  end;
end;

procedure InternalGetPixelAtWithoutAlphaBGR(p: pointer; out Color: TFPColor);
{$IFDEF CPUI386} assembler; {$ASMMODE INTEL}
asm
  mov cl, [eax+2]
  mov [edx], cl
  mov [edx+1], cl
  mov cl, [eax+1]
  mov [edx+2], cl
  mov [edx+3], cl
  mov cl, [eax]
  mov [edx+4], cl
  mov [edx+5], cl
  xor ecx, ecx
  not ecx
  mov [edx+6], cx
end;
{$ELSE}
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B2;
    TFPColorBytes(color).Rl := B2;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B0;
    TFPColorBytes(color).Bl := B0;
    color.alpha := $ffff;
  end;
end;
{$ENDIF}

procedure InternalSetPixelAtWithoutAlphaBGR(p: pointer; const Color: TFPColor);
{$IFDEF CPUI386} assembler; {$ASMMODE INTEL}
asm
  mov cl, [edx+1]
  mov [eax+2], cl
  mov cl, [edx+3]
  mov [eax+1], cl
  mov cl, [edx+5]
  mov [eax], cl
end;
{$ELSE}
begin
  with PFourBytes(p)^ do
  begin
    B2 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B0 := TFPColorBytes(color).Bh;
  end;
end;
{$ENDIF}

procedure InternalGetPixelAtWithAlphaRGBA(p: pointer; out Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B0;
    TFPColorBytes(color).Rl := B0;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B2;
    TFPColorBytes(color).Bl := B2;
    TFPColorBytes(color).Ah := B3;
    TFPColorBytes(color).Al := B3;
  end;
end;

procedure InternalSetPixelAtWithAlphaRGBA(p: pointer; const Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    B0 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B2 := TFPColorBytes(color).Bh;
    B3 := TFPColorBytes(color).Ah;
  end;
end;

procedure InternalGetPixelAtWithAlphaBGRA(p: pointer; out Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B2;
    TFPColorBytes(color).Rl := B2;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B0;
    TFPColorBytes(color).Bl := B0;
    TFPColorBytes(color).Ah := B3;
    TFPColorBytes(color).Al := B3;
  end;
end;

procedure InternalSetPixelAtWithAlphaBGRA(p: pointer; const Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    B2 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B0 := TFPColorBytes(color).Bh;
    B3 := TFPColorBytes(color).Ah;
  end;
end;

end.


