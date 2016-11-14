{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Types and methods to cache interface resources.
    See graphics.pp for examples.
}
unit LCLResCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCAdds, Types, LCLType, LCLProc, AvgLvlTree, WSReferences,
  syncobjs;
  
{off $DEFINE CheckResCacheConsistency}

type
  TResourceCache = class;
  TResourceCacheDescriptor = class;

  { TResourceCacheItem }

  TResourceCacheItem = class
  protected
    FDestroying: boolean;
    FReferenceCount: integer;
  public
    Handle: TLCLHandle;
    Cache: TResourceCache;
    FirstDescriptor, LastDescriptor: TResourceCacheDescriptor;
    Next, Prev: TResourceCacheItem;
    constructor Create(TheCache: TResourceCache; TheHandle: TLCLHandle);
    destructor Destroy; override;
    procedure IncreaseRefCount;
    procedure DecreaseRefCount;
    procedure AddToList(var First, Last: TResourceCacheItem);
    procedure RemoveFromList(var First, Last: TResourceCacheItem);
    procedure WarnReferenceHigh; virtual;
  public
    property ReferenceCount: integer read FReferenceCount;
  end;
  TResourceCacheItemClass = class of TResourceCacheItem;


  { TResourceCacheDescriptor }

  TResourceCacheDescriptor = class
  protected
    FDestroying: boolean;
  public
    Item: TResourceCacheItem;
    Cache: TResourceCache;
    Next, Prev: TResourceCacheDescriptor;
    constructor Create(TheCache: TResourceCache; TheItem: TResourceCacheItem);
    destructor Destroy; override;
    procedure AddToList(var First, Last: TResourceCacheDescriptor);
    procedure RemoveFromList(var First, Last: TResourceCacheDescriptor);
  end;
  TResourceCacheDescriptorClass = class of TResourceCacheDescriptor;


  { TResourceCache }

  TResourceCache = class
  protected
    FItems: TAvgLvlTree;
    FDescriptors: TAvgLvlTree;
    FDestroying: boolean;
    FResourceCacheDescriptorClass: TResourceCacheDescriptorClass;
    FResourceCacheItemClass: TResourceCacheItemClass;
    FMaxUnusedItem: integer; // how many freed resources to keep
    FFirstUnusedItem, FLastUnusedItem: TResourceCacheItem;
    FUnUsedItemCount: integer;
    FLock: TCriticalSection;
    procedure RemoveItem(Item: TResourceCacheItem); virtual;
    procedure RemoveDescriptor(Desc: TResourceCacheDescriptor); virtual;
    procedure ItemUsed(Item: TResourceCacheItem);
    procedure ItemUnused(Item: TResourceCacheItem);
    function ItemIsUsed(Item: TResourceCacheItem): boolean;
  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;
    function CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer): integer; virtual;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; virtual; abstract;
    procedure ConsistencyCheck;
    procedure Lock;
    procedure Unlock;
  public
    property MaxUnusedItem: integer read FMaxUnusedItem
                                           write FMaxUnusedItem;
    property ResourceCacheItemClass: TResourceCacheItemClass
                                                   read FResourceCacheItemClass;
    property ResourceCacheDescriptorClass: TResourceCacheDescriptorClass
                                             read FResourceCacheDescriptorClass;
  end;


  { THandleResourceCache }

  THandleResourceCache = class(TResourceCache)
  public
    function FindItem(Handle: TLCLHandle): TResourceCacheItem;
  end;


  { TBlockResourceCacheDescriptor }

  TBlockResourceCacheDescriptor = class(TResourceCacheDescriptor)
  public
    Data: Pointer;
    destructor Destroy; override;
  end;


  { TBlockResourceCache }

  TBlockResourceCache = class(THandleResourceCache)
  private
    FDataSize: integer;
  protected
    FOnCompareDescPtrWithDescriptor: TListSortCompare;
  public
    constructor Create(TheDataSize: integer);
    function FindDescriptor(DescPtr: Pointer): TBlockResourceCacheDescriptor;
    function AddResource(Handle: TLCLHandle; DescPtr: Pointer): TBlockResourceCacheDescriptor;
    function CompareDescriptors(Tree: TAvgLvlTree;
                                Desc1, Desc2: Pointer): integer; override;
  public
    property DataSize: integer read FDataSize;
    property OnCompareDescPtrWithDescriptor: TListSortCompare
                                           read FOnCompareDescPtrWithDescriptor;
  end;

function ComparePHandleWithResourceCacheItem(HandlePtr: PLCLHandle; Item:
  TResourceCacheItem): integer;
function CompareDescPtrWithBlockResDesc(DescPtr: Pointer;
  Item: TBlockResourceCacheDescriptor): integer;

implementation


function ComparePHandleWithResourceCacheItem(HandlePtr: PLCLHandle; Item:
  TResourceCacheItem): integer;
begin
  Result := CompareLCLHandles(HandlePtr^, Item.Handle);
end;

function CompareDescPtrWithBlockResDesc(DescPtr: Pointer;
  Item: TBlockResourceCacheDescriptor): integer;
begin
  Result := CompareMemRange(DescPtr, Item.Data,
              TBlockResourceCache(Item.Cache).DataSize);
end;


{ TResourceCacheItem }

constructor TResourceCacheItem.Create(TheCache: TResourceCache; TheHandle: TLCLHandle);
begin
  Cache := TheCache;
  Handle := TheHandle;
end;

destructor TResourceCacheItem.Destroy;
begin

  FDestroying := True;
  Cache.RemoveItem(Self);
  //debugln('TResourceCacheItem.Destroy B ',dbgs(Self));
  Handle := 0;
  inherited Destroy;
  //debugln('TResourceCacheItem.Destroy END ',dbgs(Self));
end;

procedure TResourceCacheItem.IncreaseRefCount;
begin
  inc(FReferenceCount);
  if FReferenceCount = 1 then
    Cache.ItemUsed(Self);
  if (FReferenceCount = 1000) or (FReferenceCount = 10000) then
    WarnReferenceHigh;
end;

procedure TResourceCacheItem.DecreaseRefCount;

  procedure RaiseRefCountZero;
  begin

  end;

begin
  //debugln('TResourceCacheItem.DecreaseRefCount ',ClassName,' ',dbgs(Self),' ',dbgs(FReferenceCount));
  if FReferenceCount = 0 then
    RaiseRefCountZero;
  dec(FReferenceCount);
  if FReferenceCount = 0  then
    Cache.ItemUnused(Self);
  //debugln('TResourceCacheItem.DecreaseRefCount END ');
end;

procedure TResourceCacheItem.AddToList(var First, Last: TResourceCacheItem);
// add as last
begin
  Next := nil;
  Prev := Last;
  Last := Self;
  if First = nil then First := Self;
  if Prev <> nil then Prev.Next := Self;
end;

procedure TResourceCacheItem.RemoveFromList(var First,Last: TResourceCacheItem);
begin
  if First = Self then First := Next;
  if Last = Self then Last := Prev;
  if Next <> nil then Next.Prev := Prev;
  if Prev <> nil then Prev.Next := Next;
  Next := nil;
  Prev := nil;
end;

procedure TResourceCacheItem.WarnReferenceHigh;
begin

end;

{ TResourceCacheDescriptor }

constructor TResourceCacheDescriptor.Create(TheCache: TResourceCache;
  TheItem: TResourceCacheItem);
begin
  Cache := TheCache;
  Item := TheItem;
  Item.IncreaseRefCount;
  AddToList(Item.FirstDescriptor, Item.LastDescriptor);
end;

destructor TResourceCacheDescriptor.Destroy;
begin

  FDestroying := True;
  Cache.RemoveDescriptor(Self);
  inherited Destroy;
end;

procedure TResourceCacheDescriptor.AddToList(var First, Last: TResourceCacheDescriptor);
// add as last
begin
  Next := nil;
  Prev := Last;
  Last := Self;
  if First = nil then First := Self;
  if Prev <> nil then Prev.Next := Self;
end;

procedure TResourceCacheDescriptor.RemoveFromList(var First, Last: TResourceCacheDescriptor);
begin
  if First = Self then First := Next;
  if Last = Self then Last := Prev;
  if Next <> nil then Next.Prev := Prev;
  if Prev <> nil then Prev.Next := Next;
  Next := nil;
  Prev := nil;
end;

{ TResourceCache }

procedure TResourceCache.RemoveItem(Item: TResourceCacheItem);
begin
  if not FDestroying then
  begin
    while Item.FirstDescriptor <> nil do
    begin

      Item.FirstDescriptor.Free;
    end;
    FItems.Remove(Item);
  end;
end;

procedure TResourceCache.RemoveDescriptor(Desc: TResourceCacheDescriptor);
var
  Item: TResourceCacheItem;
begin
  if not FDestroying then
  begin
    Item := Desc.Item;
    if Item <> nil then
      Desc.RemoveFromList(Item.FirstDescriptor, Item.LastDescriptor);
    FDescriptors.Remove(Desc);
    if (Item <> nil) and (Item.FirstDescriptor = nil) and (not Item.FDestroying) then
      Item.Free;
  end;
end;

procedure TResourceCache.ItemUsed(Item: TResourceCacheItem);
// called after creation or when Item is used again
begin
  if not ItemIsUsed(Item) then
  begin
    // remove from unused list
    Item.RemoveFromList(FFirstUnusedItem, FLastUnusedItem);
    dec(FUnUsedItemCount);
  end;
end;

procedure TResourceCache.ItemUnused(Item: TResourceCacheItem);
// called when Item is not used any more
var
  DeleteItem: TResourceCacheItem;
begin
  {$IFDEF CheckResCacheConsistency}
  ConsistencyCheck;
  {$ENDIF}
  //debugln('TResourceCache.ItemUnused A ',ClassName,' ',dbgs(Self));
  if not ItemIsUsed(Item) then
    raise Exception.Create('TResourceCache.ItemUnused');
  //debugln('TResourceCache.ItemUnused B ',ClassName,' ',dbgs(Self));
  Item.AddToList(FFirstUnusedItem, FLastUnusedItem);
  inc(FUnUsedItemCount);
  //debugln('TResourceCache.ItemUnused C ',ClassName,' ',dbgs(Self));
  if FUnUsedItemCount > FMaxUnusedItem then
  begin
    // maximum unused resources reached -> free the oldest
    DeleteItem := FFirstUnusedItem;
    DeleteItem.RemoveFromList(FFirstUnusedItem, FLastUnusedItem);
    DeleteItem.Free;
  end;
  //debugln('TResourceCache.ItemUnused END ',ClassName,' ',dbgs(Self));
end;

function TResourceCache.ItemIsUsed(Item: TResourceCacheItem): boolean;
begin
  Result := (FFirstUnusedItem <> Item) and (Item.Next = nil) and (Item.Prev = nil)
end;

constructor TResourceCache.Create;
begin
  FMaxUnusedItem := 100;
  FItems := TAvgLvlTree.CreateObjectCompare(@CompareItems);
  FDescriptors := TAvgLvlTree.CreateObjectCompare(@CompareDescriptors);
  FResourceCacheItemClass := TResourceCacheItem;
  FResourceCacheDescriptorClass := TResourceCacheDescriptor;
  FLock := TCriticalSection.Create;
end;

procedure TResourceCache.Clear;
begin
  while FFirstUnusedItem <> nil do
    FFirstUnusedItem.RemoveFromList(FFirstUnusedItem, FLastUnusedItem);
  FItems.FreeAndClear;
  FDescriptors.FreeAndClear;
end;

destructor TResourceCache.Destroy;
begin
  FDestroying := True;
  Clear;
  FItems.Free;
  FItems := nil;
  FDescriptors.Free;
  FDescriptors := nil;
  FLock.Free;
  inherited Destroy;
end;

function TResourceCache.CompareItems(Tree: TAvgLvlTree; Item1, Item2: Pointer): integer;
begin
  Result := CompareLCLHandles(TResourceCacheItem(Item1).Handle,
                              TResourceCacheItem(Item2).Handle);
end;

procedure TResourceCache.ConsistencyCheck;
var
  ANode: TAvgLvlTreeNode;
  Item: TResourceCacheItem;
  Desc: TResourceCacheDescriptor;
  Desc2: TResourceCacheDescriptor;
begin

  // check items
  FItems.ConsistencyCheck;
  ANode := FItems.FindLowest;
  while ANode <> nil do
  begin
    Item := TResourceCacheItem(ANode.Data);
    Desc := Item.FirstDescriptor;
    while Desc <> nil do
    begin
      Desc := Desc.Next;
    end;
    ANode := FItems.FindSuccessor(ANode);
  end;
  
  // check Descriptors
  FDescriptors.ConsistencyCheck;
  ANode := FDescriptors.FindLowest;
  while ANode <> nil do
  begin
    Desc := TResourceCacheDescriptor(ANode.Data);
    Item := Desc.Item;
    Desc2 := Item.FirstDescriptor;
    while (Desc2 <> nil) and (Desc2 <> Desc) do
      Desc2 := Desc2.Next;
    ANode := FItems.FindSuccessor(ANode);
  end;
end;

procedure TResourceCache.Lock;
begin
  FLock.Enter;
end;

procedure TResourceCache.Unlock;
begin
  FLock.Leave;
end;

{ THandleResourceCache }

function THandleResourceCache.FindItem(Handle: TLCLHandle): TResourceCacheItem;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode := FItems.FindKey(@Handle,
                          TListSortCompare(@ComparePHandleWithResourceCacheItem));
  if ANode <> nil then
    Result := TResourceCacheItem(ANode.Data)
  else
    Result := nil;
end;

{ TBlockResourceCache }

constructor TBlockResourceCache.Create(TheDataSize: integer);
begin
  inherited Create;
  FDataSize := TheDataSize;
  FResourceCacheDescriptorClass := TBlockResourceCacheDescriptor;
  FOnCompareDescPtrWithDescriptor := TListSortCompare(@CompareDescPtrWithBlockResDesc);
end;

function TBlockResourceCache.FindDescriptor(DescPtr: Pointer): TBlockResourceCacheDescriptor;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode := FDescriptors.FindKey(DescPtr,FOnCompareDescPtrWithDescriptor);
  if ANode <> nil then
    Result := TBlockResourceCacheDescriptor(ANode.Data)
  else
    Result := nil;
end;

function TBlockResourceCache.AddResource(Handle: TLCLHandle; DescPtr: Pointer): TBlockResourceCacheDescriptor;
var
  Item: TResourceCacheItem;

  procedure RaiseDescriptorAlreadyAdded;
  var
    Msg: String;
    i: Integer;
  begin
    Msg:='TBlockResourceCache.AddResource Descriptor Already Added '+LineEnding;
    for i:=0 to DataSize-1 do
      Msg:=Msg+HexStr(ord(PChar(DescPtr)[i]),2);
    raise Exception.Create(Msg);
  end;

begin
  {$IFDEF CheckResCacheConsistency}
  ConsistencyCheck;
  {$ENDIF}
  Result := FindDescriptor(DescPtr);
  if Result <> nil then
    RaiseDescriptorAlreadyAdded;

  Item := FindItem(Handle);
  if Item = nil then
  begin
    Item := FResourceCacheItemClass.Create(Self, Handle);
    FItems.Add(Item);
  end;
  Result := TBlockResourceCacheDescriptor(FResourceCacheDescriptorClass.Create(Self, Item));
  ReAllocMem(Result.Data, DataSize);
  System.Move(DescPtr^, Result.Data^, DataSize);
  FDescriptors.Add(Result);
end;

function TBlockResourceCache.CompareDescriptors(Tree: TAvgLvlTree; Desc1,
  Desc2: Pointer): integer;
begin
  Result := CompareMemRange(TBlockResourceCacheDescriptor(Desc1).Data,
                            TBlockResourceCacheDescriptor(Desc2).Data,
                            DataSize);
end;

{ TBlockResourceCacheDescriptor }

destructor TBlockResourceCacheDescriptor.Destroy;
begin
  inherited Destroy;
  ReAllocMem(Data, 0);
end;

end.
