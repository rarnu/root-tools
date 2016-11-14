{
  Implements non-native regions with support for managing their Z-order

  Author: Felipe Monteiro de Carvalho
}
unit LazRegions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, fpcanvas;

type
  TLazRegionFillMode = (rfmOddEven, rfmWinding);

  TPointArray = array of TPoint;

  { TLazRegionPart }

  TLazRegionPart = class
  public
    function GetBoundingRect: TRect; virtual;
    function IsPointInPart(AX, AY: Integer): Boolean; virtual;
  end;

  { TLazRegionRect }

  TLazRegionRect = class(TLazRegionPart)
  public
    Rect: TRect;
    function IsPointInPart(AX, AY: Integer): Boolean; override;
  end;

  { TLazRegionPolygon }

  TLazRegionPolygon = class(TLazRegionPart)
  public
    Points: array of TPoint;
    FillMode: TLazRegionFillMode;
    function IsPointInPart(AX, AY: Integer): Boolean; override;
  end;

  { TLazRegionEllipse }

  TLazRegionEllipse = class(TLazRegionPart)
  public
    X1, Y1, X2, Y2: Integer;
    function IsPointInPart(AX, AY: Integer): Boolean; override;
  end;

  {$if defined(ver2_6)}
  TFPCustomRegion = class
    function GetBoundingRect: TRect; virtual; abstract;
    function IsPointInRegion(AX, AY: Integer): Boolean; virtual; abstract;
  end;
  {$endif}

  TLazRegion = class(TFPCustomRegion)
  public
    // The parts of a region should all be inside valid areas of the region
    // so if a combination operation removes some areas of the region, then
    // these areas should be removed from all parts of the region
    // There is no z-order for the parts, they are all validly inside the region area
    Parts: TFPList; // of TLazRegionPart
    IsSimpleRectRegion: Boolean; // Indicates whether this region has only 1 rectangular part
    Rect: TRect; // Used for performance increase when IsSimpleRectRegion is on
    constructor Create; virtual;
    destructor Destroy; override;
    // Management operations
    procedure Assign(ASrcRegion: TLazRegion);
    procedure Clear;
    procedure CombineWith(ASrcRegion: TLazRegion; AOperation: Longint);
    function GetRegionKind(): Longint;
    function IsSimpleRectEmpty: Boolean;
    // Setting the contents
    procedure AddPart(APart: TLazRegionPart);
    procedure AddRectangle(ARect: TRect);
    procedure AddPolygon(var APoints: TPointArray; AFillMode: TLazRegionFillMode);
    procedure AddEllipse(AX1, AY1, AX2, AY2: Integer);
    procedure SetAsSimpleRectRegion(ARect: TRect);
    procedure AddPartsFromRegion(ASrcRegion: TLazRegion);
    procedure DoChangeToComplexRegion;
    // Overrides of TFPCustomRegion information query routines
    function GetBoundingRect: TRect; override;
    function IsPointInRegion(AX, AY: Integer): Boolean; override;
  end;

  { This is a region which can hold other region holders inside it }

  { TLazRegionWithChilds }

  TLazRegionWithChilds = class(TLazRegion)
  public
    Parent: TLazRegionWithChilds;
    // The order in this list is also the Z-Order of the sub regions inside it
    // The element with index zero is the bottom-most one
    Childs: TFPList; // of TLazRegionWithChilds
    UserData: TObject; // available link to another object
    constructor Create; override;
    destructor Destroy; override;
    function IsPointInRegion(AX, AY: Integer): TLazRegionWithChilds; virtual; reintroduce;
  end;

function IsPointInPolygon(AX, AY: Integer; const APolygon: array of TPoint): Boolean;

implementation

//  The function will return True if the point x,y is inside the polygon, or
//  False if it is not.
//
//  Original C code: http://www.visibone.com/inpoly/inpoly.c.txt
//
//  Translation from C by Felipe Monteiro de Carvalho
//
//  License: Public Domain
function IsPointInPolygon(AX, AY: Integer; const APolygon: array of TPoint): Boolean;
var
  xnew, ynew: Cardinal;
  xold,yold: Cardinal;
  x1,y1: Cardinal;
  x2,y2: Cardinal;
  i, npoints: Integer;
  inside: Integer = 0;
begin
  Result := False;
  npoints := Length(APolygon);
  if (npoints < 3) then Exit;
  xold := APolygon[npoints-1].X;
  yold := APolygon[npoints-1].Y;
  for i := 0 to npoints - 1 do
  begin
    xnew := APolygon[i].X;
    ynew := APolygon[i].Y;
    if (xnew > xold) then
    begin
      x1:=xold;
      x2:=xnew;
      y1:=yold;
      y2:=ynew;
    end
    else
    begin
      x1:=xnew;
      x2:=xold;
      y1:=ynew;
      y2:=yold;
    end;
    if (((xnew < AX) = (AX <= xold))         // edge "open" at left end
      and ((AY-y1)*(x2-x1) < (y2-y1)*(AX-x1))) then
    begin
      inside := not inside;
    end;
    xold:=xnew;
    yold:=ynew;
  end;
  Result := inside <> 0;
end;

{ TLazRegionEllipse }

{
  The equation of the inner area of an axis aligned ellipse:

  (X/a)^2 + (Y/b)^2 <= 1
}
function TLazRegionEllipse.IsPointInPart(AX, AY: Integer): Boolean;
var
  a, b: Integer;
begin
  a := X2 - X1;
  b := Y2 - Y1;
  if (a < 0) or (b < 0) then Exit(False);

  Result := Sqr(AX/a) + Sqr(AY/b) <= 1;
end;

{ TLazRegionPart }

function TLazRegionPart.GetBoundingRect: TRect;
begin
  Result := Bounds(0, 0, 0, 0);
end;

function TLazRegionPart.IsPointInPart(AX, AY: Integer): Boolean;
begin
  Result := False;
end;

{ TLazRegionRect }

function TLazRegionRect.IsPointInPart(AX, AY: Integer): Boolean;
begin
  Result := (AX >= Rect.Left) and (AX <= Rect.Right) and
    (AY >= Rect.Top) and (AY <= Rect.Bottom);
end;

{ TLazRegionPolygon }

function TLazRegionPolygon.IsPointInPart(AX, AY: Integer): Boolean;
begin
  Result := IsPointInPolygon(AX, AY, Points);
end;

{ TLazRegion }

constructor TLazRegion.Create;
begin
  inherited Create;
  Parts := TFPList.Create;
  IsSimpleRectRegion := True;
end;

destructor TLazRegion.Destroy;
begin
  Parts.Free;
  inherited Destroy;
end;

procedure TLazRegion.Assign(ASrcRegion: TLazRegion);
begin
  Clear;
  AddPartsFromRegion(ASrcRegion);
end;

procedure TLazRegion.Clear;
var
  i: Integer;
begin
  // Free all items
  for i := 0 to Parts.Count - 1 do
    TLazRegionPart(Parts.Items[i]).Free;
  Parts.Clear;

  IsSimpleRectRegion := True;
  Rect := Bounds(0, 0, 0, 0);
end;

procedure TLazRegion.CombineWith(ASrcRegion: TLazRegion; AOperation: Longint);
begin
  case AOperation of
    {RGN_AND:
      QRegion_intersected(RSrc1, RDest, RSrc2);}
    RGN_COPY:
    begin
      Assign(ASrcRegion);
    end;
{    RGN_DIFF:
      QRegion_subtracted(RSrc1, RDest, RSrc2);}
    RGN_OR:
      AddPartsFromRegion(ASrcRegion);
    {RGN_XOR:
      QRegion_xored(RSrc1, RDest, RSrc2);}
  end;
end;

function TLazRegion.GetRegionKind: Longint;
begin
  if not IsSimpleRectRegion then
    Result := COMPLEXREGION
  else if IsSimpleRectEmpty() then
    Result := NULLREGION
  else
    Result := SIMPLEREGION;
end;

function TLazRegion.IsSimpleRectEmpty: Boolean;
begin
  Result := (Rect.Bottom - Rect.Top <= 0) or (Rect.Right - Rect.Left <= 0);
end;

procedure TLazRegion.AddPart(APart: TLazRegionPart);
begin
  Parts.Add(APart);
  DoChangeToComplexRegion();
end;

procedure TLazRegion.AddRectangle(ARect: TRect);
var
  lNewRect: TLazRegionRect;
begin
  lNewRect := TLazRegionRect.Create;
  lNewRect.Rect := ARect;
  AddPart(lNewRect);
end;

procedure TLazRegion.AddPolygon(var APoints: TPointArray;
  AFillMode: TLazRegionFillMode);
var
  lNewPolygon: TLazRegionPolygon;
begin
  lNewPolygon := TLazRegionPolygon.Create;
  lNewPolygon.Points := APoints;
  lNewPolygon.FillMode := AFillMode;
  AddPart(lNewPolygon);
end;

procedure TLazRegion.AddEllipse(AX1, AY1, AX2, AY2: Integer);
var
  lNewEllipse: TLazRegionEllipse;
begin
  lNewEllipse := TLazRegionEllipse.Create;
  lNewEllipse.X1 := AX1;
  lNewEllipse.Y1 := AY1;
  lNewEllipse.X2 := AX2;
  lNewEllipse.Y2 := AY2;
  AddPart(lNewEllipse);
end;

procedure TLazRegion.AddPartsFromRegion(ASrcRegion: TLazRegion);
var
  i: Integer;
begin
  if ASrcRegion.IsSimpleRectRegion then
  begin
    if IsSimpleRectRegion and IsSimpleRectEmpty() then
      Rect := ASrcRegion.Rect
    else
      AddRectangle(ASrcRegion.Rect);
  end
  else
  begin
    for i := 0 to ASrcRegion.Parts.Count-1 do
    begin
      Parts.Add(ASrcRegion.Parts.Items[i]);
    end;
    IsSimpleRectRegion := False;
  end;
end;

procedure TLazRegion.DoChangeToComplexRegion;
var
  OldIsSimpleRectRegion: Boolean;
begin
  OldIsSimpleRectRegion := IsSimpleRectRegion; // This avoids an endless loop when calling AddRectangle
  IsSimpleRectRegion := False;
  if OldIsSimpleRectRegion and (not IsSimpleRectEmpty()) then
    AddRectangle(Rect);
end;

procedure TLazRegion.SetAsSimpleRectRegion(ARect: TRect);
begin
  Clear;
  IsSimpleRectRegion := True;
  Rect := ARect;
end;

function TLazRegion.GetBoundingRect: TRect;
begin
  Result := Rect;
end;

{
  Checks if a point is inside this region
}
function TLazRegion.IsPointInRegion(AX, AY: Integer): Boolean;
var
  i: Integer;
begin
  if IsSimpleRectRegion then
  begin
    Result := (AX >= Rect.Left) and (AX <= Rect.Right) and
      (AY >= Rect.Top) and (AY <= Rect.Bottom);
  end
  else
  begin
    Result := False;
    for i := 0 to Parts.Count-1 do
    begin
      // being inside 1 subpart is enough
      if TLazRegionPart(Parts.Items[i]).IsPointInPart(AX, AY) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

{ TLazRegionWithChilds }

constructor TLazRegionWithChilds.Create;
begin
  inherited Create;
  Childs := TFPList.Create;
end;

destructor TLazRegionWithChilds.Destroy;
begin
  Childs.Free;
  inherited Destroy;
end;

{
  Returns itself or a child, depending on where the point was found
  or nil if the point is neither in the region nor in any children

  Part of the behavior is implemented in TLazRegionWithChilds
}
function TLazRegionWithChilds.IsPointInRegion(AX, AY: Integer): TLazRegionWithChilds;
var
  i: Integer;
  lIsInside: Boolean;
begin
  Result := nil;
  // First check if it is inside itself
  lIsInside := inherited IsPointInRegion(AX, AY);

  // If it is, then check if it is in any of the children
  if lIsInside then
  begin
    Result := nil;

    // The order here is important to respect the Z-order of controls
    for i := Childs.Count-1 downto 0 do
    begin
      Result := TLazRegionWithChilds(Childs.Items[i]).IsPointInRegion(AX, AY);
      if Result <> nil then Break;
    end;

    // if it wasn't in any sub region, it is really in this region
    if Result = nil then Result := Self;
  end;
end;

end.

