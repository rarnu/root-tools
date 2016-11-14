{ $Id: lclclasses.pp 41387 2013-05-24 18:30:06Z juha $}
{
 *****************************************************************************
 *                               lclclasses.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Defines the base class for all LCL TComponents including controls.
}
unit LCLClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, WSLCLClasses, WSReferences, LCLType, LCLProc;

type

  // SysUtils.LongRec has unsigned Word for Lo and Hi,
  //  we need a similar record with signed ShortInt
  LazLongRec = packed record
{$ifdef FPC_LITTLE_ENDIAN}
    Lo,Hi : SmallInt;
{$else FPC_LITTLE_ENDIAN}
    Hi,Lo : SmallInt;
{$endif FPC_LITTLE_ENDIAN}
  end;

  { TLCLComponent }

  TLCLComponent = class(TComponent)
  private
    FWidgetSetClass: TWSLCLComponentClass;
    FLCLRefCount: integer;
  protected
    class procedure WSRegisterClass; virtual;
    class function GetWSComponentClass(ASelf: TLCLComponent): TWSLCLComponentClass; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
    procedure IncLCLRefCount;
    procedure DecLCLRefCount;
    property LCLRefCount: integer read FLCLRefCount;
    property WidgetSetClass: TWSLCLComponentClass read FWidgetSetClass;
  end;
  
  { TLCLReferenceComponent }

  // A base class for all components having a handle

  TLCLReferenceComponent = class(TLCLComponent)
  private
    FReferencePtr: PWSReference;

    FCreating: Boolean; // Set if we are creating the handle
    function  GetHandle: THandle;
    function  GetReferenceAllocated: Boolean;
  protected
    procedure CreateParams(var AParams: TCreateParams); virtual;
    procedure DestroyReference;
    function  GetReferenceHandle: THandle; virtual; abstract;
    procedure ReferenceCreated; virtual;    // gets called after the Handle is created
    procedure ReferenceDestroying; virtual; // gets called before the Handle is destroyed
    procedure ReferenceNeeded;
    function  WSCreateReference(AParams: TCreateParams): PWSReference; virtual;
    procedure WSDestroyReference; virtual;
  protected
  public
    destructor Destroy; override;
    property Handle: TLCLIntfHandle read GetHandle; deprecated;
    property HandleAllocated: Boolean read GetReferenceAllocated;
    property ReferenceAllocated: Boolean read GetReferenceAllocated;
  end;

implementation

uses
  InterfaceBase;

class procedure TLCLComponent.WSRegisterClass;
begin
  //
end;

// This method allows descendents to override the FWidgetSetClass
class function TLCLComponent.GetWSComponentClass(ASelf: TLCLComponent): TWSLCLComponentClass;
begin
  Result := FindWSComponentClass(Self);

  if Result = nil then
  begin
    {$IFDEF VerboseLCL}
    DebugLn(['TLCLComponent.NewInstance WARNING: missing FWidgetSetClass ',ClassName]);
    {$ENDIF}
    Result := TWSLCLComponent;
  end;
end;

constructor TLCLComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  {$IFDEF DebugLCLComponents}
  //DebugLn('TLCLComponent.Create ',DbgSName(Self));
  DebugLCLComponents.MarkCreated(Self,DbgSName(Self));
  {$ENDIF}
end;

destructor TLCLComponent.Destroy;
begin
  {$IFNDEF DisableChecks}
  if FLCLRefCount>0 then
    DebugLn(['WARNING: TLCLComponent.Destroy with LCLRefCount>0. Hint: Maybe the component is processing an event?']);
  {$ENDIF}
  {$IFDEF DebugLCLComponents}
  //DebugLn('TLCLComponent.Destroy ',DbgSName(Self));
  DebugLCLComponents.MarkDestroyed(Self);
  {$ENDIF}
  inherited Destroy;
end;

class function TLCLComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  WSRegisterClass;

  TLCLComponent(Result).FWidgetSetClass := GetWSComponentClass(TLCLComponent(Result));
end;

procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject);
begin
end;

procedure TLCLComponent.IncLCLRefCount;
begin
  inc(FLCLRefCount);
end;

procedure TLCLComponent.DecLCLRefCount;
begin
  dec(FLCLRefCount);
end;

{ TLCLReferenceComponent }

procedure TLCLReferenceComponent.CreateParams(var AParams: TCreateParams);
begin
end;

destructor TLCLReferenceComponent.Destroy;
begin
  DestroyReference;
  inherited Destroy;
end;

procedure TLCLReferenceComponent.DestroyReference;
begin
  if ReferenceAllocated then
  begin
    ReferenceDestroying;
    WSDestroyReference;
    FReferencePtr^._Clear;
    FReferencePtr := nil;
  end;
end;

function TLCLReferenceComponent.GetHandle: THandle;
begin
  ReferenceNeeded;
  Result := GetReferenceHandle;
end;

function TLCLReferenceComponent.GetReferenceAllocated: Boolean;
begin
  Result := (FReferencePtr <> nil) and FReferencePtr^.Allocated;
end;

procedure TLCLReferenceComponent.ReferenceCreated;
begin
end;

procedure TLCLReferenceComponent.ReferenceDestroying;
begin
end;

procedure TLCLReferenceComponent.ReferenceNeeded;
var
  Params: TCreateParams;
begin
  if ReferenceAllocated then Exit;

  if FCreating
  then begin
    // raise some error ?
    {$IFNDEF DisableChecks}
    DebugLn('TLCLReferenceComponent: Circular reference creation');
    {$ENDIF}
    Exit;
  end;

  CreateParams(Params);
  FCreating := True;
  try
    FReferencePtr := WSCreateReference(Params);
    if not ReferenceAllocated
    then begin
      // raise some error ?
      {$IFNDEF DisableChecks}
      DebugLn('TLCLHandleComponent: Reference creation failed');
      {$ENDIF}
      Exit;
    end;
  finally
    FCreating := False;
  end;
  ReferenceCreated;
end;

function TLCLReferenceComponent.WSCreateReference(AParams: TCreateParams): PWSReference;
begin
  // this function should be overriden in derrived class
  Result := nil;
end;

procedure TLCLReferenceComponent.WSDestroyReference;
begin
  TWSLCLReferenceComponentClass(WidgetSetClass).DestroyReference(Self);
end;

end.

