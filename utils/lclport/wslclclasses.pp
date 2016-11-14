{ $Id: wslclclasses.pp 43222 2013-10-12 15:43:24Z zeljko $}
{
 *****************************************************************************
 *                              wslclclasses.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSLCLClasses;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

{off$DEFINE VerboseWSRegistration}
{off$DEFINE VerboseWSRegistration_methods}
{off$DEFINE VerboseWSRegistration_treedump}
{$IFDEF VerboseWSRegistration_methods}
{$DEFINE VerboseWSRegistration}
{$ENDIF}

interface

uses
  Classes, SysUtils, LCLProc;

type
  { TWSPrivate }

  {
    Internal WidgetSet specific object tree
  }
  TWSPrivate = class(TObject)
  end;
  TWSPrivateClass = class of TWSPrivate;

  { For non-TComponent WS objects }

  TWSObject = class(TObject)
  public
  end;
  TWSObjectClass = class of TWSObject;

  { TWSLCLComponent }

{$M+}
  TWSLCLComponent = class(TObject)
  public
    class function WSPrivate: TWSPrivateClass; //inline;
  end;
{$M-}
  TWSLCLComponentClass = class of TWSLCLComponent;

  { TWSLCLHandleComponent }

  TWSLCLReferenceComponent = class(TWSLCLComponent)
  published
    class procedure DestroyReference(AComponent: TComponent); virtual;
  end;
  TWSLCLReferenceComponentClass = class of TWSLCLReferenceComponent;


function GetWSLazAccessibleObject: TWSObjectClass;
procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);
function GetWSLazDeviceAPIs: TWSObjectClass;
procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);

implementation

type
  PClassNode = ^TClassNode;
  TClassNode = record
    LCLClass: TComponentClass;
    WSClass: TWSLCLComponentClass;
    VClass: Pointer;
    VClassName: ShortString;
    Parent: PClassNode;
    Child: PClassNode;
    Sibling: PClassNode;
  end;

const
  VIRTUAL_VMT_COUNT = 128;
  VIRTUAL_VMT_SIZE = vmtMethodStart + VIRTUAL_VMT_COUNT * SizeOf(Pointer);

const
  // vmtAutoTable is something Delphi 2 and not used, we 'borrow' the vmt entry
  vmtWSPrivate = vmtAutoTable;

var
  WSLazAccessibleObjectClass: TWSObjectClass;
  WSLazDeviceAPIsClass: TWSObjectClass;

type
  TMethodNameTableEntry = packed record
      Name: PShortstring;
      Addr: Pointer;
    end;

  TMethodNameTable = packed record
    Count: DWord;
    Entries: packed array[0..9999999] of TMethodNameTableEntry;
  end;
  PMethodNameTable =  ^TMethodNameTable;

  TPointerArray = packed array[0..9999999] of Pointer;
  PPointerArray = ^TPointerArray;

function GetWSLazAccessibleObject: TWSObjectClass;
begin
  Result := WSLazAccessibleObjectClass;
end;

procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);
begin
  WSLazAccessibleObjectClass := AWSObject;
end;

function GetWSLazDeviceAPIs: TWSObjectClass;
begin
  Result := WSLazDeviceAPIsClass;
end;

procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);
begin
  WSLazDeviceAPIsClass := AWSObject;
end;

{ TWSLCLComponent }

class function TWSLCLComponent.WSPrivate: TWSPrivateClass; //inline;
begin
  Result := TWSPrivateClass(PClass(Pointer(Self) + vmtWSPrivate)^);
end;

{ TWSLCLHandleComponent }

class procedure TWSLCLReferenceComponent.DestroyReference(AComponent: TComponent);
begin
end;

end.
