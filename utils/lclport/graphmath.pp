{
/***************************************************************************
                             GraphMath.pp
                             ------------
         Math helper routines for use within Graphics/Drawing & related
                   Initial Revision  : Wed Aug 07 2002


***************************************************************************/

*****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
*****************************************************************************
}
{
@abstract(A Set of Math Helper routines to simply Cross-Platfrom Canvas, 
etc)
@author(Andrew Johnson <AJ_Genius@Hotmail.com>)
@created(2002)
@lastmod(2002)
}
unit GraphMath;

{$Mode OBJFPC} {$H+}

interface

Uses
  Types, Classes, SysUtils, Math, LCLProc;

Type
  TFloatPoint = Record
    X, Y : Extended;
  end;

  TBezier = Array[0..3] of TFloatPoint;

  PPoint = ^TPoint;

procedure Angles2Coords(X,Y, Width, Height : Integer;
  Angle1, Angle2 : Extended; var SX, SY, EX, EY : Integer);

procedure Arc2Bezier(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : TBezier);

function Bezier(const C1,C2,C3,C4 : TFloatPoint): TBezier; Overload; inline;
function Bezier(const C1,C2,C3,C4 : TPoint): TBezier; Overload; inline;

procedure Bezier2Polyline(const Bezier : TBezier; var Points : PPoint;
  var Count : Longint);

procedure BezierArcPoints(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : PPoint; var Count : Longint);

function BezierMidPoint(Bezier : TBezier) : TFloatPoint;

procedure Coords2Angles(X, Y, Width, Height : Integer; SX, SY,
  EX, EY : Integer; var Angle1, Angle2 : Extended);

function Distance(PT1,Pt2 : TPoint) : Extended; overload;
function Distance(Pt, SP, EP : TFloatPoint) : Extended; overload;

function EccentricAngle(PT : TPoint; Rect : TRect) : Extended;

function EllipseRadialLength(Rect : TRect; EccentricAngle : Extended) : Longint;

function FloatPoint(AX,AY : Extended): TFloatPoint;

function LineEndPoint(StartPoint : TPoint; Angle, Length : Extended) : TPoint;

procedure PolyBezier2Polyline(Beziers: Array of TBezier;
  var Points : PPoint; var Count : Longint); Overload;
procedure PolyBezier2Polyline(Beziers : Array of TPoint; 
  var Points : PPoint; var Count : Longint; 
  Continuous : Boolean); Overload;
procedure PolyBezier2Polyline(Beziers : PPoint; BCount : Longint;
  var Points : PPoint; var Count : Longint; 
  Continuous : Boolean); Overload;

procedure PolyBezierArcPoints(X, Y, Width, Height : Longint; Angle1,
  Angle2, Rotation : Extended; var Points : PPoint; var Count : Longint);

function Quadrant(PT, Center : TPoint) : Integer;

function RadialPoint(EccentricAngle : Extended; Rect : TRect) : TPoint;

procedure SplitBezier(Bezier : TBezier; var Left, Right : TBezier);

Operator + (Addend1, Addend2 : TFloatPoint) : TFloatPoint;
Operator + (Addend1 : TFloatPoint; Addend2 : Extended) : TFloatPoint;
Operator + (Addend1 : Extended; Addend2 : TFloatPoint) : TFloatPoint;
Operator + (Addend1 : TFloatPoint; Addend2 : TPoint) : TFloatPoint;
Operator + (Addend1 : TPoint; Addend2 : TFloatPoint) : TFloatPoint;

Operator - (Minuend : TFloatPoint; Subtrahend : Extended) : TFloatPoint;
Operator - (Minuend, Subtrahend : TFloatPoint) : TFloatPoint;
Operator - (Minuend : TFloatPoint; Subtrahend : TPoint) : TFloatPoint;
Operator - (Minuend : TPoint; Subtrahend : TFloatPoint) : TFloatPoint;

Operator * (Multiplicand, Multiplier : TFloatPoint) : TFloatPoint;
Operator * (Multiplicand : TFloatPoint; Multiplier : Extended) : TFloatPoint;
Operator * (Multiplicand : Extended; Multiplier : TFloatPoint) : TFloatPoint;
Operator * (Multiplicand : TFloatPoint; Multiplier : TPoint) : TFloatPoint;
Operator * (Multiplicand : TPoint; Multiplier : TFloatPoint) : TFloatPoint;

Operator / (Dividend, Divisor : TFloatPoint) : TFloatPoint;
Operator / (Dividend : TFloatPoint; Divisor : Extended) : TFloatPoint;
Operator / (Dividend : TFloatPoint; Divisor : TPoint) : TFloatPoint;
Operator / (Dividend : TPoint; Divisor : TFloatPoint) : TFloatPoint;

Operator = (Compare1, Compare2  : TPoint) : Boolean;
Operator = (Compare1, Compare2  : TFloatPoint) : Boolean;

Operator := (Value : TFloatPoint) : TPoint;

Operator := (Value : TPoint) : TFloatPoint;

Operator = (Compare1, Compare2  : TRect) : Boolean;


implementation


Operator + (Addend1, Addend2 : TFloatPoint) : TFloatPoint;
Begin
  With Result do begin
    X := Addend1.X + Addend2.X;
    Y := Addend1.Y + Addend2.Y;
  end;
end;

Operator + (Addend1 : TFloatPoint; Addend2 : Extended) : TFloatPoint;
Begin
  With Result do begin
    X := Addend1.X + Addend2;
    Y := Addend1.Y + Addend2;
  end;
end;

Operator + (Addend1 : Extended; Addend2 : TFloatPoint) : TFloatPoint;
begin
  Result := Addend2 + Addend1;
end;

Operator + (Addend1 : TFloatPoint; Addend2 : TPoint) : TFloatPoint;
Begin
  With Result do begin
    X := Addend1.X + Addend2.X;
    Y := Addend1.Y + Addend2.Y;
  end;
end;

Operator + (Addend1 : TPoint; Addend2 : TFloatPoint) : TFloatPoint;
begin
  Result := Addend2 + Addend1;
end;

Operator - (Minuend, Subtrahend:TFloatPoint) : TFloatPoint;
Begin
  With Result do begin
    X := Minuend.X - Subtrahend.X;
    Y := Minuend.Y - Subtrahend.Y;
  end;
end;

Operator - (Minuend : TFloatPoint; Subtrahend : Extended) : TFloatPoint;
Begin
  With Result do begin
    X := Minuend.X - Subtrahend;
    Y := Minuend.Y - Subtrahend;
  end;
end;

Operator - (Minuend : TFloatPoint; Subtrahend : TPoint) : TFloatPoint;
begin
  With Result do begin
    X := Minuend.X - Subtrahend.X;
    Y := Minuend.Y - Subtrahend.Y;
  end;
end;

Operator - (Minuend : TPoint; Subtrahend : TFloatPoint) : TFloatPoint;
begin
  With Result do begin
    X := Minuend.X - Subtrahend.X;
    Y := Minuend.Y - Subtrahend.Y;
  end;
end;

Operator * (Multiplicand, Multiplier : TFloatPoint) : TFloatPoint;
Begin
  With Result do begin
    X := Multiplicand.X * Multiplier.X;
    Y := Multiplicand.Y * Multiplier.Y;
  end;
end;

Operator * (Multiplicand : TFloatPoint; Multiplier : Extended) : TFloatPoint;
Begin
  With Result do begin
    X := Multiplicand.X * Multiplier;
    Y := Multiplicand.Y * Multiplier;
  end;
end;

Operator * (Multiplicand : Extended; Multiplier : TFloatPoint) : TFloatPoint;
Begin
  Result := Multiplier*Multiplicand;
end;

Operator * (Multiplicand : TFloatPoint; Multiplier : TPoint) : TFloatPoint;
begin
  With Result do begin
    X := Multiplicand.X * Multiplier.X;
    Y := Multiplicand.Y * Multiplier.Y;
  end;
end;

Operator * (Multiplicand : TPoint; Multiplier : TFloatPoint) : TFloatPoint;
begin
  Result := Multiplier*Multiplicand;
end;

Operator / (Dividend, Divisor : TFloatPoint) : TFloatPoint;
Begin
  With Result do begin
    X := Dividend.X / Divisor.X;
    Y := Dividend.Y / Divisor.Y;
  end;
end;

Operator / (Dividend : TFloatPoint; Divisor : Extended) : TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor;
    Y := Dividend.Y / Divisor;
  end;
end;

Operator / (Dividend : TFloatPoint; Divisor : TPoint) : TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor.X;
    Y := Dividend.Y / Divisor.Y;
  end;
end;

Operator / (Dividend : TPoint; Divisor : TFloatPoint) : TFloatPoint;
begin
  With Result do begin
    X := Dividend.X / Divisor.X;
    Y := Dividend.Y / Divisor.Y;
  end;
end;

Operator = (Compare1, Compare2  : TPoint) : Boolean;
begin
  Result := (Compare1.X = Compare2.X) and (Compare1.Y = Compare2.Y);
end;

Operator = (Compare1, Compare2  : TFloatPoint) : Boolean;
begin
  Result := (Compare1.X = Compare2.X) and (Compare1.Y = Compare2.Y);
end;

Operator := (Value : TFloatPoint) : TPoint;
begin
  Result.X := Trunc(SimpleRoundTo(Value.X, 0));
  Result.Y := Trunc(SimpleRoundTo(Value.Y, 0));
end;

Operator := (Value : TPoint) : TFloatPoint;
begin
  With Result do begin
    X := Value.X;
    Y := Value.Y;
  end;
end;

Operator = (Compare1, Compare2  : TRect) : Boolean;
begin
  Result := (Compare1.Left = Compare2.Left) and
            (Compare1.Top = Compare2.Top) and
            (Compare1.Right = Compare2.Right) and
            (Compare1.Bottom = Compare2.Bottom);
end;

{------------------------------------------------------------------------------
  Method:   Angles2Coords
  Params:   x,y,width,height,angle1,angle2, sx, sy, ex, ey
  Returns:  Nothing

  Use Angles2Coords to convert an Eccentric(aka Radial) Angle and an
  Angle-Length, such as are used in X-Windows and GTK, into the coords,
  for Start and End Radial-Points, such as are used in the Windows API Arc
  Pie and Chord routines. The angles are 1/16th of a degree. For example, a
  full circle equals 5760 (16*360). Positive values of Angle and AngleLength
  mean counter-clockwise while negative values mean clockwise direction. 
  Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
procedure Angles2Coords(X, Y, Width, Height : Integer;
  Angle1, Angle2 : Extended; var SX, SY, EX, EY : Integer);
var
  aRect : TRect;
  SP, EP : TPoint;
begin
  aRect := Rect(X,Y,X + Width,Y + Height);
  SP := RadialPoint(Angle1 , aRect);
  If Angle2 + Angle1 > 360*16 then
    Angle2 := (Angle2 + Angle1) - 360*16
  else
    Angle2 := Angle2 + Angle1;
  EP := RadialPoint(Angle2, aRect);
  SX := SP.X;
  SY := SP.Y;
  EX := EP.X;
  EY := EP.Y;
end;

{------------------------------------------------------------------------------
  Method:   Arc2Bezier
  Params:   X, Y, Width, Height, Angle1, Angle2, Rotation, Points, Count
  Returns:  Nothing

  Use Arc2Bezier to convert an Arc and ArcLength into a Bezier Aproximation
  of the Arc. The Rotation parameter accepts a Rotation-Angle for a rotated
  Ellipse'- for a non-rotated ellipse this value would be 0, or 360. If the
  AngleLength is greater than 90 degrees, or is equal to 0, it automatically
  exits, as Bezier cannot accurately aproximate any angle greater then 90
  degrees, and in fact for best result no angle greater than 45 should be
  converted, instead an array of Bezier's should be created, each Bezier
  descibing a portion of the total arc no greater than 45 degrees. The angles
  are 1/16th of a degree. For example, a full circle equals 5760 (16*360).
  Positive values of Angle and AngleLength mean counter-clockwise while
  negative values mean clockwise direction. Zero degrees is at the 3'o clock
  position.

------------------------------------------------------------------------------}
procedure Arc2Bezier(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : TBezier);

  function Rotate(Point : TFloatPoint; Rotation : Extended) : TFloatPoint;
  var
    SinA,CosA : Extended;
  begin
    CosA := cos(Rotation);
    SinA := Sin(Rotation);
    Result.X := Point.X*CosA + Point.Y*SinA;
    Result.Y := Point.X*SinA - Point.Y*CosA;
  end;

  function Scale(Point : TFloatPoint; ScaleX, ScaleY : Extended) : TFloatPoint;
  begin
    Result := Point*FloatPoint(ScaleX,ScaleY);
  end;

var
  Beta : Extended;
  P : array[0..3] of TFLoatPoint;
  SinA,CosA : Extended;
  A,B : Extended;
  I : Longint;
  PT : TFloatPoint;
  ScaleX, ScaleY : Extended;
begin
  If ABS(Angle2) > 90*16 then
    exit;
  If Angle2 = 0 then
    exit;

  B := Extended(Height) / 2;
  A := Extended(Width) / 2;

  If (A <> B) and (A <> 0) and (B <> 0) then begin
    If A > B then begin
      ScaleX := Extended(Width) / Height;
      ScaleY := 1;
      A := B;
    end
    else begin
      ScaleX := 1;
      ScaleY := Extended(Height) / Width;
      B := A;
    end;
  end
  else begin
    ScaleX := 1;
    ScaleY := 1;
  end;

  Angle1 := DegToRad(Angle1/16);
  Angle2 := DegToRad(Angle2/16);
  Rotation := -DegToRad(Rotation/16);
  Beta := (4/3)*(1 - Cos(Angle2/2))/(Sin(Angle2/2));
  PT.X := X + Width / 2;
  PT.Y := Y + Height / 2;

  CosA := cos(Angle1);
  SinA := sin(Angle1);

  P[0].X := A *CosA;
  P[0].Y := B *SinA;
  P[1].X := P[0].X - Beta * A * SinA;
  P[1].Y := P[0].Y + Beta * B * CosA;

  CosA := cos(Angle1 + Angle2);
  SinA := sin(Angle1 + Angle2);

  P[3].X := A *CosA;
  P[3].Y := B *SinA;
  P[2].X := P[3].X + Beta * A * SinA;
  P[2].Y := P[3].Y - Beta * B * CosA;

  For I := 0 to 3 do
  begin
    Points[I] := Scale(P[I],ScaleX, ScaleY); //Scale to proper size
    Points[I] := Rotate(Points[I], Rotation); //Rotate Counter-Clockwise
    Points[I] := Points[I] + PT; //Translate to Center
  end;
end;

{------------------------------------------------------------------------------
  Method:   Bezier
  Params:   C1,C2,C3,C4
  Returns:  TBezier

  Use Bezier to get a TBezier. It is Primarily for use with and in Bezier
  routines.

------------------------------------------------------------------------------}
function Bezier(const C1,C2,C3,C4 : TFloatPoint): TBezier;
begin
  Result[0] := C1;
  Result[1] := C2;
  Result[2] := C3;
  Result[3] := C4;
end;

{------------------------------------------------------------------------------
  Method:   Bezier
  Params:   C1,C2,C3,C4
  Returns:  TBezier

  Use Bezier to get a TBezier. It is Primarily for use with and in Bezier
  routines.

------------------------------------------------------------------------------}
function Bezier(const C1,C2,C3,C4 : TPoint): TBezier;
begin
  Result[0] := FloatPoint(C1.X,C1.Y);
  Result[1] := FloatPoint(C2.X,C2.Y);
  Result[2] := FloatPoint(C3.X,C3.Y);
  Result[3] := FloatPoint(C4.X,C4.Y);
end;

{------------------------------------------------------------------------------
  Method:   Bezier2Polyline
  Params:   Bezier, Points, Count
  Returns:  Nothing

  Use BezierToPolyline to convert a 4-Point Bezier into a Pointer Array of
  TPoint and a Count variable which can then be used within either a Polyline,
  or Polygon routine. It is primarily for use within PolyBezier2Polyline. If
  Points is not initialized or Count is less then 0, it is set to nil and
  the array starts at 0, otherwise it tries to append points
  to the array starting at Count. Points should ALWAYS be Freed when done
  by calling to ReallocMem(Points, 0) or FreeMem.

------------------------------------------------------------------------------}
procedure Bezier2Polyline(const Bezier : TBezier; var Points : PPoint;
  var Count : Longint);
var
  Pt : TPoint;

  procedure AddPoint(const Point : TFloatPoint);
  var
    P : TPoint;
  begin
    P := Point;
    if (Pt <> P) then
    begin
      Inc(Count);
      ReallocMem(Points, SizeOf(TPoint) * Count);
      Points[Count - 1] := P;
      Pt := P;
    end;
  end;

  function Colinear(BP : TBezier; Tolerance : Extended) : Boolean;
  var
    D : Extended;
  begin
    D := SQR(Distance(BP[1], BP[0], BP[3]));
    Result := D < Tolerance;
    D := SQR(Distance(BP[2], BP[0], BP[3]));
    If Result then
      Result := Result and (D < Tolerance);
  end;

  procedure SplitRecursive(B : TBezier);
  var
    Left,
    Right : TBezier;
  begin
    If Colinear(B, 1) then begin
      AddPoint(B[0]);
      AddPoint(B[3]);
    end
    else begin
      SplitBezier(B,left,right);
      SplitRecursive(left);
      SplitRecursive(right);
    end;
  end;

begin
  Pt := Point(-1,-1);
  If (not Assigned(Points)) or (Count <= 0) then
  begin
    Count := 0;

    if Assigned(Points) then
      ReallocMem(Points, 0);
  end;
  SplitRecursive(Bezier);
end;

{------------------------------------------------------------------------------
  Method:   BezierArcPoints
  Params:   X, Y, Width, Height, Angle1, Angle2, Rotation, Points, Count
  Returns:  Nothing

  Use BezierArcPoints to convert an Arc and ArcLength into a Pointer Array
  of TPoints for use with Polyline or Polygon. The Rotation parameter accepts
  a Rotation-Angle for a rotated Ellipse'- for a non-rotated ellipse this
  value would be 0, or 360. The result is an Aproximation based on 1 or more
  Beziers. If the AngleLength is greater than 90 degrees, it calls
  PolyBezierArcPoints, otherwise it Converts the angles into a Bezier by
  calling to Arc2Bezier, and then converts the Bezier into an array of Points
  by calling to Bezier2Polyline. The angles are 1/16th of a degree. For example,
  a full circle equals 5760 (16*360). Positive values of Angle and AngleLength
  mean counter-clockwise while negative values mean clockwise direction. Zero
  degrees is at the 3'o clock position. If Points is not initialized or Count
  is less then 0, it is set to nil and the array starts at 0,
  otherwise it tries to append points to the array starting at Count. Points
  should ALWAYS be Freed when done by calling ReallocMem(Points, 0) or FreeMem.

------------------------------------------------------------------------------}
procedure BezierArcPoints(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : PPoint; var Count : Longint);
var
  B : TBezier;
begin
  If ABS(Angle2) > 90*16 then begin
    PolyBezierArcPoints(X, Y, Width, Height, Angle1, Angle2, Rotation, Points,
                        Count);
    Exit;
  end;
  If Angle2 = 0 then
    exit;

  If (not Assigned(Points)) or (Count <= 0) then
  begin
    Count := 0;
    
    if Assigned(Points) then
      ReallocMem(Points, 0);
  end;
  
  Arc2Bezier(X, Y, Width, Height, Angle1, Angle2, Rotation, B);
  Bezier2Polyline(B,Points,Count);
end;

{------------------------------------------------------------------------------
  Method:   BezierMidPoint
  Params:   Bezier
  Returns:  TFloatPoint

  Use BezierMidPoint to get the Mid-Point of any 4-Point Bezier. It is
  primarily for use in SplitBezier.

------------------------------------------------------------------------------}
function BezierMidPoint(Bezier : TBezier) : TFloatPoint;
begin
  Result := (Bezier[0] + 3*Bezier[1] + 3*Bezier[2] + Bezier[3]) / 8;
end;

{------------------------------------------------------------------------------
  Method:   Coords2Angles
  Params:   x,y,width,height,sx,sy,ex,ey, angle1,angle2
  Returns:  Nothing

  Use Coords2Angles to convert the coords for Start and End Radial-Points, such
  as are used in the Windows API Arc Pie and Chord routines, into an Eccentric
  (aka Radial) counter clockwise Angle and an Angle-Length, such as are used in
  X-Windows and GTK. The angles angle1 and angle2 are returned in 1/16th of a
  degree. For example, a full circle equals 5760 (16*360). Zero degrees is at
  the 3'o clock position.

------------------------------------------------------------------------------}
procedure Coords2Angles(X, Y, Width, Height : Integer; SX, SY,
  EX, EY : Integer; var Angle1, Angle2 : Extended);
var
  aRect : TRect;
  SP,EP : TPoint;
begin
  aRect := Rect(X,Y,X + Width,Y + Height);
  SP := Point(SX,SY);
  EP := Point(EX,EY);
  Angle1 := EccentricAngle(SP, aRect);
  Angle2 := EccentricAngle(EP, aRect);
  If Angle2 < Angle1 then
    Angle2 := 360*16 - (Angle1 - Angle2)
  else
    Angle2 := Angle2 - Angle1;
end;

{------------------------------------------------------------------------------
  Method:   Distance
  Params:   PT1, PT2
  Returns:  Extended

  Use Distance to get the distance between any two Points. It is primarily
  for use in other routines such as EccentricAngle.

------------------------------------------------------------------------------}
function Distance(Pt1,Pt2 : TPoint) : Extended;
begin
  Result := Sqrt(Sqr(Pt2.X - Pt1.X) + Sqr(Pt2.Y - Pt1.Y));
end;

{------------------------------------------------------------------------------
  Method:   Distance
  Params:   PT, SP,EP
  Returns:  Extended

  Use Distance to get the distance between any point(PT) and a line defined
  by any two points(SP, EP). Intended for use in Bezier2Polyline, so params
  are TFloatPoint's, NOT TPoint's.

------------------------------------------------------------------------------}
function Distance(Pt, SP, EP : TFloatPoint) : Extended;
var
  A, B, C : Extended;

  function Slope(PT1,Pt2 : TFloatPoint) : Extended;
  begin
    If Pt2.X <> Pt1.X then
      Result := (Pt2.Y - Pt1.Y) / (Pt2.X - Pt1.X)
    else
      Result := 1;
  end;

  function YIntercept(PT1,Pt2 : TFloatPoint) : Extended;
  begin
    Result := Pt1.Y - Slope(Pt1,Pt2)*Pt1.X;
  end;

begin
  A := -Slope(SP,EP);
  B := 1;
  C := -YIntercept(SP, EP);
  Result := ABS(A*Pt.X + B*Pt.Y + C)/Sqrt(Sqr(A) + Sqr(B));
end;

{------------------------------------------------------------------------------
  Method:   EccentricAngle
  Params:   Pt, Rect
  Returns:  Extended

  Use EccentricAngle to get the Eccentric( aka Radial ) Angle of a given
  point on any non-rotated ellipse. It is primarily for use in Coords2Angles.
  The result is in 1/16th of a degree. For example, a full circle equals
  5760 (16*360).  Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
function EccentricAngle(PT : TPoint; Rect : TRect) : Extended;
var
  CenterPt : TPoint;
  Quad : Integer;
  Theta : Extended;
begin
  CenterPt := CenterPoint(Rect);
  Quad := Quadrant(Pt,CenterPt);
  Theta := -1;
  Case Quad of
    1..4:
      begin
        Theta := Distance(CenterPt,Pt);
        If Theta > 0 then
          Theta := RadToDeg(ArcSin(ABS(PT.Y - CenterPt.Y) / Theta));
      end;
  end;
  Case Quad of
    0:{ 0, 0}
      Theta := -1;
    1:{ X, Y}
      Theta := Theta;
    2:{-X, Y}
      Theta := 180 - Theta;
    3:{-X,-Y}
      Theta := 180 + Theta;
    4:{ X,-Y}
      Theta := 360 - Theta;
    5:{ 0, Y}
      Theta := 90;
    6:{ X, 0}
      Theta := 0;
    7:{ 0,-Y}
      Theta := 270;
    8:{-X, 0}
      Theta := 180;
  end;
  Result := Theta*16;
end;

{------------------------------------------------------------------------------
  Method:   EllipseRadialLength
  Params:   Rect, EccentricAngle
  Returns:  Longint

  Use EllipseRadialLength to get the Radial-Length of non-rotated ellipse at
  any given Eccentric( aka Radial ) Angle. It is primarily for use in other
  routines such as RadialPoint. The Eccentric angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position.

------------------------------------------------------------------------------}
function EllipseRadialLength(Rect : TRect; EccentricAngle : Extended) : Longint;
var
  a, b, R : Extended;
begin
  a := (Rect.Right - Rect.Left) div 2;
  b := (Rect.Bottom - Rect.Top) div 2;
  R := Sqr(a)*Sqr(b);
  if R <> 0 then
    R := Sqrt(R / ((Sqr(b)*Sqr(Cos(DegToRad(EccentricAngle/16)))) +
      (Sqr(a)*Sqr(Sin(DegToRad(EccentricAngle/16))))));
  Result := TruncToInt(R);
end;

{------------------------------------------------------------------------------
  Method:   FloatPoint
  Params:   AX, AY
  Returns:  TFloatPoint

  Use FloatPoint to get a TFloatPoint. It is essentialy like Classes. Point in
  use, except that it excepts Extended Parameters. It is Primarily for use with
  and in Bezier routines.

------------------------------------------------------------------------------}
function FloatPoint(AX,AY : Extended): TFloatPoint;
begin
  With Result do begin
    X := AX;
    Y := AY;
  end;
end;

{------------------------------------------------------------------------------
  Method:   LineEndPoint
  Params:   StartPoint, Angle, Length
  Returns:  TPoint

  Use LineEndPoint to get the End-Point of a line of any given Length at
  any given angle with any given Start-Point. It is primarily for use in
  other routines such as RadialPoint. The angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position.

------------------------------------------------------------------------------}
function LineEndPoint(StartPoint : TPoint; Angle, Length : Extended) : 
TPoint;
begin
  if Angle > 360*16 then
    Angle := Frac(Angle / 360*16) * 360*16;

  if Angle < 0 then
    Angle := 360*16 - abs(Angle);

  Result.Y := StartPoint.Y - Round(Length*Sin(DegToRad(Angle/16)));
  Result.X := StartPoint.X + Round(Length*Cos(DegToRad(Angle/16)));
end;


{------------------------------------------------------------------------------
  Method:   PolyBezier2Polyline
  Params:   Beziers, Points, Count
  Returns:  Nothing

  Use BezierToPolyline to convert an array of 4-Point Bezier into a Pointer
  Array of TPoint and a Count variable which can then be used within either a
  Polyline, or Polygon routine. Points is automatically initialized, so any
  existing information is lost, and the array starts at 0. Points should ALWAYS
  be Freed when done by calling to ReallocMem(Points, 0).

------------------------------------------------------------------------------}
procedure PolyBezier2Polyline(Beziers: Array of TBezier;
  var Points : PPoint; var Count : Longint);
var
  I : Integer;
begin
  If (High(Beziers) < 1) then
    exit;
  Count := 0;
  If Assigned(Points) then
    Try
      ReallocMem(Points, 0)
    Finally
      Points := nil;
    end;
  For I := 0 to High(Beziers) - 1 do
    Bezier2PolyLine(Beziers[I], Points, Count);
end;

{------------------------------------------------------------------------------
  Method:   PolyBezier2Polyline
  Params:   Beziers, Points, Count, Continuous
  Returns:  Nothing

  Use BezierToPolyline to convert an array of TPoints descibing 1 or more
  4-Point Beziers into a Pointer Array of TPoint and a Count variable which
  can then be used within either a Polyline, or Polygon routine. If Continuous
  is set to true then the first point of each Bezier is the last point of
  the preceding Bezier, so every bezier must have 3 described points, in
  addition to the initial Starting Point; otherwise each Bezier must have 4
  points. If there are an uneven number of points then the last set of points
  is ignored. Points is automatically initialized, so any existing information
  is lost, and the array starts at 0. Points should ALWAYS be Freed when done
  by calling to ReallocMem(Points, 0).

------------------------------------------------------------------------------}
procedure PolyBezier2Polyline(Beziers : Array of TPoint; var Points : PPoint;
  var Count : Longint; Continuous : Boolean);
begin  
  PolyBezier2Polyline(@Beziers[0],High(Beziers) + 1, Points, Count, 
    	              Continuous);
end;

procedure PolyBezier2Polyline(Beziers : PPoint; BCount : Longint;
  var Points : PPoint; var Count : Longint; Continuous : Boolean);
var
  I : Integer;
  NB : Longint;
begin
  If BCount < 4 then
    exit;
  Count := 0;
  If Assigned(Points) then
    Try
      ReallocMem(Points, 0)
    Finally
      Points := nil;
    end;
  If Not Continuous then begin
    NB := BCount;
    NB := NB div 4;
    For I := 0 to NB - 1 do
      Bezier2PolyLine(Bezier(Beziers[I*4],Beziers[I*4+1],
        Beziers[I*4+2],Beziers[I*4+3]), Points, Count);
  end
  else begin
    NB := BCount - 1;
    NB := NB div 3;
    For I := 0 to NB-1 do
      Bezier2PolyLine(Bezier(Beziers[(I - 1)*3 + 3],Beziers[I*3 + 1],
        Beziers[I*3+2],Beziers[I*3+3]), Points, Count);
  end;
end;
  
{------------------------------------------------------------------------------
  Method:   PolyBezierArcPoints
  Params:   X, Y, Width, Height, Angle1, Angle2, Rotation, Points, Count
  Returns:  Nothing

  Use PolyBezierArcPoints to convert an Agnle and AgnleLength into a
  Pointer Array of TPoints for use with Polyline or Polygon.
  The Rotation parameter accepts a Rotation-Angle for a rotated Ellipse'- for
  a non-rotated ellipse this value would be 0, or 360*16.
  The result is an Aproximation based on 1 or more Beziers. If the AngleLength
  is greater than 45*16 degrees, it recursively breaks the Arc into Arcs of
  45*16 degrees or less, and converts them into Beziers with BezierArcPoints.
  The angles are 1/16th of a degree. For example, a full circle equals
  5760 (16*360).
  Positive values of Angle and AngleLength mean counter-clockwise while negative
  values mean clockwise direction. Zero degrees is at the 3'o clock position.
  Points is automatically initialized, so any existing information is lost,
  and the array starts at 0. Points should ALWAYS be Freed when done by calling
  to ReallocMem(Points, 0).

------------------------------------------------------------------------------}
procedure PolyBezierArcPoints(X, Y, Width, Height : Longint; Angle1, Angle2,
  Rotation : Extended; var Points : PPoint; var Count : Longint);
var
  I,K : Integer;
  FullAngle : Extended;
  TST : Boolean;
begin
  If Abs(Angle2) > 360*16 then begin
    Angle2 := 360*16;
    Angle1 := 0;
  end;
  If Abs(Rotation) > 360*16 then
    Rotation := Frac(Rotation / 360*16)*360*16;
  FullAngle := Angle1 + Angle2;
  K := Ceil(ABS(Angle2/16) / 45);
  Count := 0;
  If Assigned(Points) then
    Try
      ReallocMem(Points, 0)
    Finally
      Points := nil;
    end;
  If Angle2 > 45*16 then
    Angle2 := 45*16
  else
    If Angle2 < -45*16 then
      Angle2 := -45*16;
  For I := 0 to K - 1 do begin
    BezierArcPoints(X, Y, Width,Height,Angle1,Angle2,Rotation,Points,Count);
    Angle1 := Angle1 + Angle2;
    If Angle2 > 0 then
      TST := (FullAngle - Angle1) > 45*16
    else
      TST := ABS(FullAngle - Angle1) > 45*16;
    If TST then begin
      If Angle2 > 0 then
        Angle2 := 45*16
      else
        Angle2 := -45*16;
    end
    else begin
      {If Angle2 > 0 then}
        Angle2 := FullAngle - Angle1
      {else
        Angle2 := -(FullAngle - Angle1);
        - Wrong: This gives the wrong sign to Angle2 - G. Colla
        }
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:   Quadrant
  Params:   PT, Center
  Returns:  Integer

  Use Quadrant to determine the Quadrant of any point, given the Center.
  It is primarily for use in other routines such as EccentricAngle. A result
  of 1-4 represents the primary 4 quardants. A result of 5-8 means the point
  lies on one of the Axis', 5 = -Y Axis, 6 = +X Axis, 7 = +Y Axis, and
  8 = -X Axis. A result of -1 means that it does not fall in any quadrant,
  that is, it is the Center.

------------------------------------------------------------------------------}
function Quadrant(Pt,Center : TPoint) : Integer;
var
  X,Y,CX,CY : Longint;
begin
  X  := Pt.X;
  Y  := Pt.Y;
  CX := Center.X;
  CY := Center.Y;
  Result := -1;
  If (Y < CY) then begin
    If (X > CX) then begin
      Result := 1;
    end
    else
      If (X < CX) then begin
        Result := 2;
      end
    else begin
      Result := 5;
    end;
  end
  else
    If (Y > CY) then begin
      If (X < CX) then begin
        Result := 3;
      end
      else
        If (X > CX) then begin
          Result := 4;
        end
      else begin
        Result := 7;
      end;
    end
  else
    If (Y = CY) then begin
      If (X > CX) then begin
        Result := 6;
      end
      else
        If (X < CX) then begin
          Result := 8;
        end;
    end;
end;

{------------------------------------------------------------------------------
  Method:   RadialPointAngle
  Params:   EccentricAngle, Rect
  Returns:  TPoint

  Use RadialPoint to get the Radial-Point at any given Eccentric( aka Radial )
  angle on any non-rotated ellipse. It is primarily for use in Angles2Coords.
  The EccentricAngle is in 1/16th of a degree. For example, a full circle
  equals 5760 (16*360).  Zero degrees is at the 3'o clock position.

------------------------------------------------------------------------------}
function RadialPoint(EccentricAngle : Extended; Rect : TRect) : TPoint;
var
  R : Longint;
Begin
  R := EllipseRadialLength(Rect,EccentricAngle);
  Result := LineEndPoint(CenterPoint(Rect), EccentricAngle, R);
end;

{------------------------------------------------------------------------------
  Method:   SplitBezier
  Params:   Bezier, Left, Right
  Returns:  Nothing

  Use SplitBezier to split any 4-Point Bezier into two 4-Point Bezier's :
  a 'Left' and a 'Right'. It is primarily for use in Bezier2Polyline.

------------------------------------------------------------------------------}
procedure SplitBezier(Bezier : TBezier; var Left, Right : TBezier);
var
  Tmp : TFloatPoint;
begin
  Tmp := (Bezier[1] + Bezier[2]) / 2;

  left[0]  := Bezier[0];
  Left[1]  := (Bezier[0] + Bezier[1]) / 2;
  left[2]  := (Left[1] + Tmp) / 2;
  Left[3]  := BezierMidPoint(Bezier);

  right[3] := Bezier[3];
  right[2] := (Bezier[2] + Bezier[3]) / 2;
  Right[1] := (Right[2] + Tmp) / 2;
  right[0] := BezierMidPoint(Bezier);
end;

end.
