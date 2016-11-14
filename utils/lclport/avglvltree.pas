{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    The Tree is sorted ascending from left to right. That means
      Compare(Node.Left,Node.Right) <= 0 for all nodes.

    TAvgLvlTree is an Average Level binary Tree. This binary tree is always
    balanced, so that inserting, deleting and finding a node is performed in
    O(log(#Nodes)).

    Duplicates are supported.
    Order of duplicates is kept, that means the order is stable.

    The compare function must define a total order, that means transitive
      A >= B and B>=C means A >= C for all nodes A,B,C
}
unit AvgLvlTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAvgLvlTree = class;
  
  TObjectSortCompare = function(Tree: TAvgLvlTree; Data1, Data2: Pointer
                                ): integer of object;

  { TAvgLvlTreeNode }

  TAvgLvlTreeNode = class
  public
    Parent, Left, Right: TAvgLvlTreeNode;
    Balance: integer; // = RightDepth-LeftDepth  -2..+2, after balancing: -1,0,+1
    Data: Pointer;
    function Successor: TAvgLvlTreeNode; // next right
    function Precessor: TAvgLvlTreeNode; // next left
    function TreeDepth: integer; // longest WAY down. e.g. only one node => 0 !
    procedure ConsistencyCheck(Tree: TAvgLvlTree); virtual;
    function GetCount: SizeInt;
  end;
  TAvgLvlTreeNodeClass = class of TAvgLvlTreeNode;
  PAvgLvlTreeNode = ^TAvgLvlTreeNode;

  { TAvgLvlTreeNodeEnumerator - left to right, low to high }

  TAvgLvlTreeNodeEnumerator = class
  protected
    FCurrent: TAvgLvlTreeNode;
    FLowToHigh: boolean;
    FTree: TAvgLvlTree;
  public
    constructor Create(Tree: TAvgLvlTree; aLowToHigh: boolean = true);
    function GetEnumerator: TAvgLvlTreeNodeEnumerator; inline;
    function MoveNext: Boolean;
    property Current: TAvgLvlTreeNode read FCurrent;
    property LowToHigh: boolean read FLowToHigh;
  end;

  { TAvgLvlTree }

  TAvgLvlTree = class
  protected
    fRoot: TAvgLvlTreeNode;
    FCount: SizeInt;
    FNodeClass: TAvgLvlTreeNodeClass;
    FOnCompare: TListSortCompare;
    FOnObjectCompare: TObjectSortCompare;
    procedure BalanceAfterInsert(ANode: TAvgLvlTreeNode);
    procedure BalanceAfterDelete(ANode: TAvgLvlTreeNode);
    procedure DeletingNode({%H-}aNode: TAvgLvlTreeNode); virtual;
    function FindInsertPos(Data: Pointer): TAvgLvlTreeNode;
    procedure Init; virtual;
    procedure NodeAdded({%H-}aNode: TAvgLvlTreeNode); virtual;
    procedure RotateLeft(aNode: TAvgLvlTreeNode); virtual;
    procedure RotateRight(aNode: TAvgLvlTreeNode); virtual;
    procedure SwitchPositionWithSuccessor(aNode, aSuccessor: TAvgLvlTreeNode); virtual;
    procedure SetOnCompare(const AValue: TListSortCompare);
    procedure SetOnObjectCompare(const AValue: TObjectSortCompare);
    procedure SetCompares(const NewCompare: TListSortCompare;
                          const NewObjectCompare: TObjectSortCompare);
  public
    constructor Create(OnCompareMethod: TListSortCompare);
    constructor CreateObjectCompare(OnCompareMethod: TObjectSortCompare);
    constructor Create;
    destructor Destroy; override;
    property OnCompare: TListSortCompare read FOnCompare write SetOnCompare;
    property OnObjectCompare: TObjectSortCompare read FOnObjectCompare write SetOnObjectCompare;
    property NodeClass: TAvgLvlTreeNodeClass read FNodeClass write FNodeClass; // used for new nodes

    // add, delete, remove, move
    procedure Add(ANode: TAvgLvlTreeNode);
    function Add(Data: Pointer): TAvgLvlTreeNode;
    procedure Delete(ANode: TAvgLvlTreeNode);
    function Remove(Data: Pointer): boolean;
    function RemovePointer(Data: Pointer): boolean;
    procedure MoveDataLeftMost(var ANode: TAvgLvlTreeNode);
    procedure MoveDataRightMost(var ANode: TAvgLvlTreeNode);
    procedure Clear;
    procedure FreeAndClear;
    procedure FreeAndDelete(ANode: TAvgLvlTreeNode);
    function Equals(Obj: TObject): boolean; override; // same as IsEqual(aTree,false)
    function IsEqual(aTree: TAvgLvlTree; CheckDataPointer: boolean): boolean; // checks only keys or Data (references), not the data itself, O(n)
    procedure Assign(aTree: TAvgLvlTree); virtual; // clear and copy all Data (references), O(n)

    // search
    property Root: TAvgLvlTreeNode read fRoot;
    property Count: SizeInt read FCount;
    function Compare(Data1, Data2: Pointer): integer;
    function Find(Data: Pointer): TAvgLvlTreeNode; // O(log(n))
    function FindKey(Key: Pointer;
                     OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode; // O(log(n))
    function FindNearestKey(Key: Pointer;
                       OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode; // O(log(n))
    function FindSuccessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode; inline;
    function FindPrecessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode; inline;
    function FindLowest: TAvgLvlTreeNode; // O(log(n))
    function FindHighest: TAvgLvlTreeNode; // O(log(n))
    function FindNearest(Data: Pointer): TAvgLvlTreeNode; // O(log(n))
    // search in a tree with duplicates (duplicate means here: Compare function returns 0)
    function FindPointer(Data: Pointer): TAvgLvlTreeNode; // O(log(n))+k (k= maximum number of same values)
    function FindLeftMost(Data: Pointer): TAvgLvlTreeNode;
    function FindRightMost(Data: Pointer): TAvgLvlTreeNode;
    function FindLeftMostKey(Key: Pointer;
                       OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
    function FindRightMostKey(Key: Pointer;
                       OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
    function FindLeftMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
    function FindRightMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;

    // enumerators
    function GetEnumerator: TAvgLvlTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TAvgLvlTreeNodeEnumerator;

    // consistency
    procedure ConsistencyCheck; virtual;
    procedure WriteReportToStream(s: TStream);
    function NodeToReportStr(aNode: TAvgLvlTreeNode): string; virtual;
    function ReportAsString: string;
  end;
  TAvgLvlTreeClass = class of TAvgLvlTree;
  PAvgLvlTree = ^TAvgLvlTree;

type
  TIndexedAVLTreeNode = class(TAvgLvlTreeNode)
  public
    LeftCount: SizeInt; // number of nodes in the Left side
  end;

  { TIndexedAVLTree }

  TIndexedAVLTree = class(TAvgLvlTree)
  private
    function GetItems(Index: SizeInt): Pointer; inline;
  protected
    fLastIndex: SizeInt;
    fLastNode: TIndexedAVLTreeNode;
    procedure DeletingNode(aNode: TAvgLvlTreeNode); override;
    procedure Init; override;
    procedure NodeAdded(aNode: TAvgLvlTreeNode); override;
    procedure RotateLeft(aNode: TAvgLvlTreeNode); override;
    procedure RotateRight(aNode: TAvgLvlTreeNode); override;
    procedure SwitchPositionWithSuccessor(aNode, aSuccessor: TAvgLvlTreeNode); override;
  public
    function GetNodeAtIndex(Index: integer): TIndexedAVLTreeNode;
    function NodeToIndex(Node: TAvgLvlTreeNode): SizeInt;
    function IndexOf(Data: Pointer): SizeInt;
    property Items[Index: SizeInt]: Pointer read GetItems; default;
    procedure ConsistencyCheck; override;
    function NodeToReportStr(aNode: TAvgLvlTreeNode): string; override;
  end;

type
  { TPointerToPointerTree - Associative array }

  TPointerToPointerItem = record
    Key: Pointer;
    Value: Pointer;
  end;
  PPointerToPointerItem = ^TPointerToPointerItem;

  { TPointerToPointerEnumerator }

  TPointerToPointerEnumerator = class
  protected
    FHighToLow: boolean;
    FTree: TAvgLvlTree;
    FCurrent: TAvgLvlTreeNode;
    function GetCurrent: PPointerToPointerItem; inline;
  public
    constructor Create(Tree: TAvgLvlTree);
    function GetEnumerator: TPointerToPointerEnumerator; inline;
    function MoveNext: Boolean;
    property Current: PPointerToPointerItem read GetCurrent;
    property HighToLow: boolean read FHighToLow;
  end;

  TPointerToPointerTree = class
  private
    FItems: TAvgLvlTree;
    function GetCount: SizeInt; inline;
    function GetValues(const Key: Pointer): Pointer;
    procedure SetValues(const Key: Pointer; const AValue: Pointer);
    function FindNode(const Key: Pointer): TAvgLvlTreeNode;
    function GetNode(Node: TAvgLvlTreeNode; out Key, Value: Pointer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearWithFree; // free Values with TObject(Value).Free
    function Equals(Obj: TObject): boolean; override;
    function IsEqual(aTree: TPointerToPointerTree): boolean;
    procedure Assign(aTree: TPointerToPointerTree);
    procedure Remove(Key: Pointer);
    function Contains(const Key: Pointer): Boolean; inline;
    function GetFirst(out Key, Value: Pointer): Boolean;
    function GetLast(out Key, Value: Pointer): Boolean;
    function GetNext(const Key: Pointer; out NextKey, NextValue: Pointer): Boolean;
    function GetPrev(const Key: Pointer; out PrevKey, PrevValue: Pointer): Boolean;
    property Count: SizeInt read GetCount;
    property Values[const Key: Pointer]: Pointer read GetValues write SetValues; default;
    property Tree: TAvgLvlTree read FItems; // tree of PPointerToPointerItem

    // enumerators
    function GetEnumerator: TPointerToPointerEnumerator;
    function GetEnumeratorHighToLow: TPointerToPointerEnumerator;
  end;


function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;


type
  TStringMap = class;

  TStringMapItem = record
    Name: string;
  end;
  PStringMapItem = ^TStringMapItem;

  { TCustomStringMapEnumerator }

  TCustomStringMapEnumerator = class
  protected
    FTree: TAvgLvlTree;
    FCurrent: TAvgLvlTreeNode;
  public
    constructor Create(Tree: TAvgLvlTree);
    function MoveNext: boolean;
    // "Current" is implemented by the descendant classes
  end;

  { TCustomStringMap }

  TCustomStringMap = class
  private
    FCompareKeyItemFunc: TListSortCompare;
    FTree: TAvgLvlTree;// tree of PStringMapItem
    FCaseSensitive: boolean;
    function GetCompareItemsFunc: TListSortCompare;
  protected
    procedure DisposeItem(p: PStringMapItem); virtual;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; virtual;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; virtual;
  public
    constructor Create(TheCaseSensitive: boolean);
    constructor Create(const ACompareItems, ACompareNameWithItem: TListSortCompare;
                       TheCaseSensitive: boolean = false);
    destructor Destroy; override;
    procedure Clear; virtual;
    function Contains(const s: string): boolean; inline;
    procedure GetNames(List: TStrings);
    procedure Remove(const Name: string); virtual;
    property CaseSensitive: boolean read FCaseSensitive;
    property Tree: TAvgLvlTree read FTree; // tree of PStringMapItem
    function FindNode(const s: string): TAvgLvlTreeNode;
    function Count: SizeInt; inline;
    function Equals(OtherTree: TCustomStringMap): boolean; reintroduce;
    procedure Assign(Source: TCustomStringMap); virtual;
    property CompareItemsFunc: TListSortCompare read GetCompareItemsFunc;
    property CompareKeyItemFunc: TListSortCompare read FCompareKeyItemFunc;
    procedure SetCompareFuncs(
            const NewCompareItemsFunc, NewCompareKeyItemFunc: TListSortCompare;
            NewCaseSensitive: boolean);
  end;

  { TStringMapEnumerator }

  TStringMapEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: string; inline;
  public
    property Current: string read GetCurrent;
  end;

  { TStringMap - associative array string to boolean }

  TStringMap = class(TCustomStringMap)
  private
    function GetValues(const s: string): boolean;
    procedure SetValues(const s: string; AValue: boolean);
  public
    procedure Add(const Name: string);
    function GetEnumerator: TStringMapEnumerator;
    property Values[const s: string]: boolean read GetValues write SetValues; default;
  end;

  { TOldStringToStringTree - Associative array }

  TStringToStringItem = record
    Name: string;
    Value: string;
  end;
  PStringToStringItem = ^TStringToStringItem;

  { TStringToStringTreeEnumerator }

  TStringToStringTreeEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: PStringToStringItem; inline;
  public
    property Current: PStringToStringItem read GetCurrent;
  end;

  { TStringToStringTree }

  TStringToStringTree = class(TCustomStringMap)
  private
    function GetValues(const s: string): string;
    procedure SetValues(const s: string; const AValue: string);
  protected
    procedure DisposeItem(p: PStringMapItem); override;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; override;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; override;
    function GetNode(Node: TAvgLvlTreeNode; out Name, Value: string): Boolean;
  public
    function GetString(const Name: string; out Value: string): boolean;
    procedure Add(const Name, Value: string); inline;
    procedure Add(const Name, Value, Delimiter: string);
    procedure AddNameValues(List: TStrings);
    procedure AddValues(List: TStrings); inline; deprecated; // use AddNames
    procedure AddNames(List: TStrings);
    procedure Delete(const Name: string); inline; deprecated; // use Remove
    property Values[const s: string]: string read GetValues write SetValues; default;
    function AsText: string;
    procedure Assign(Source: TCustomStringMap); override;
    function GetEnumerator: TStringToStringTreeEnumerator;
    function GetFirst(out Name, Value: string): Boolean;
    function GetLast(out Name, Value: string): Boolean;
    function GetNext(const Name: string; out NextName, NextValue: string): Boolean;
    function GetPrev(const Name: string; out PrevName, PrevValue: string): Boolean;
  end;

  { TStringToPointerTree - Associative array from string to pointer }

  TStringToPointerItem = record
    Name: string;
    Value: Pointer;
  end;
  PStringToPointerItem = ^TStringToPointerItem;

  { TStringToPointerTreeEnumerator }

  TStringToPointerTreeEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: PStringToPointerItem; inline;
  public
    property Current: PStringToPointerItem read GetCurrent;
  end;

  TStringToPointerTree = class(TCustomStringMap)
  private
    FFreeValues: boolean;
    function GetValues(const s: string): Pointer;
    procedure SetValues(const s: string; const AValue: Pointer);
  protected
    procedure DisposeItem(p: PStringMapItem); override;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; override;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; override;
  public
    function GetData(const Name: string; out Value: Pointer): boolean;
    property Values[const s: string]: Pointer read GetValues write SetValues; default;
    function GetEnumerator: TStringToPointerTreeEnumerator;
    property FreeValues: boolean read FFreeValues write FFreeValues;
  end;

function CompareStringToStringItems(Data1, Data2: Pointer): integer;
function CompareAnsiStringWithStrToStrItem(Key, Data: Pointer): Integer;
function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
function CompareAnsiStringWithStrToStrItemI(Key, Data: Pointer): Integer;

function ComparePointer(Data1, Data2: Pointer): integer;


implementation


function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointer(PPointerToPointerItem(Data1)^.Key,
                         PPointerToPointerItem(Data2)^.Key);
end;

function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;
begin
  Result:=ComparePointer(Key,PPointerToPointerItem(Data)^.Key);
end;

function CompareStringToStringItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(PStringMapItem(Data1)^.Name,
                     PStringMapItem(Data2)^.Name);
end;

function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
begin
  Result:=CompareText(PStringMapItem(Data1)^.Name,
                      PStringMapItem(Data2)^.Name);
end;

function CompareAnsiStringWithStrToStrItem(Key, Data: Pointer): Integer;
begin
  Result:=CompareStr(AnsiString(Key),PStringMapItem(Data)^.Name);
end;

function CompareAnsiStringWithStrToStrItemI(Key, Data: Pointer): Integer;
begin
  Result:=CompareText(AnsiString(Key),PStringMapItem(Data)^.Name);
end;

{ TPointerToPointerEnumerator }

function TPointerToPointerEnumerator.GetCurrent: PPointerToPointerItem;
begin
  Result:=PPointerToPointerItem(FCurrent.Data);
end;

constructor TPointerToPointerEnumerator.Create(Tree: TAvgLvlTree);
begin
  FTree:=Tree;
end;

function TPointerToPointerEnumerator.GetEnumerator: TPointerToPointerEnumerator;
begin
  Result:=Self;
end;

function TPointerToPointerEnumerator.MoveNext: Boolean;
begin
  if FHighToLow then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Precessor
    else
      FCurrent:=FTree.FindHighest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Successor
    else
      FCurrent:=FTree.FindLowest;
  end;
  Result:=FCurrent<>nil;
end;

{ TAvgLvlTreeNodeEnumerator }

constructor TAvgLvlTreeNodeEnumerator.Create(Tree: TAvgLvlTree;
  aLowToHigh: boolean);
begin
  FTree:=Tree;
  FLowToHigh:=aLowToHigh;
end;

function TAvgLvlTreeNodeEnumerator.GetEnumerator: TAvgLvlTreeNodeEnumerator;
begin
  Result:=Self;
end;

function TAvgLvlTreeNodeEnumerator.MoveNext: Boolean;
begin
  if FLowToHigh then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Successor
    else
      FCurrent:=FTree.FindLowest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Precessor
    else
      FCurrent:=FTree.FindHighest;
  end;
  Result:=FCurrent<>nil;
end;

{ TStringToPointerTree }

function TStringToPointerTree.GetValues(const s: string): Pointer;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToPointerItem(Node.Data)^.Value
  else
    Result:=nil
end;

procedure TStringToPointerTree.SetValues(const s: string; const AValue: Pointer);
var
  Node: TAvgLvlTreeNode;
  NewItem: PStringToPointerItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    PStringToPointerItem(Node.Data)^.Value:=AValue;
  end else begin
    New(NewItem);
    NewItem^.Name:=s;
    NewItem^.Value:=AValue;
    FTree.Add(NewItem);
  end;
end;

procedure TStringToPointerTree.DisposeItem(p: PStringMapItem);
var
  Item: PStringToPointerItem absolute p;
begin
  if FreeValues then
    TObject(Item^.Value).Free;
  Dispose(Item);
end;

function TStringToPointerTree.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
var
  Item1: PStringToPointerItem absolute p1;
  Item2: PStringToPointerItem absolute p2;
begin
  Result:=(Item1^.Name=Item2^.Name)
      and (Item1^.Value=Item2^.Value);
end;

function TStringToPointerTree.CreateCopy(Src: PStringMapItem): PStringMapItem;
var
  SrcItem: PStringToPointerItem absolute Src;
  NewItem: PStringToPointerItem;
begin
  New(NewItem);
  NewItem^.Name:=SrcItem^.Name;
  NewItem^.Value:=SrcItem^.Value;
  Result:=PStringMapItem(NewItem);
end;

function TStringToPointerTree.GetData(const Name: string; out Value: Pointer): boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToPointerItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TStringToPointerTree.GetEnumerator: TStringToPointerTreeEnumerator;
begin
  Result:=TStringToPointerTreeEnumerator.Create(FTree);
end;

{ TStringToPointerTreeEnumerator }

function TStringToPointerTreeEnumerator.GetCurrent: PStringToPointerItem;
begin
  Result:=PStringToPointerItem(FCurrent.Data);
end;

{ TStringToStringTree }

function TStringToStringTree.GetValues(const s: string): string;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToStringItem(Node.Data)^.Value
  else
    Result:=''
end;

procedure TStringToStringTree.SetValues(const s: string; const AValue: string);
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    Item:=PStringToStringItem(Node.Data);
    Item^.Name:=s; // update case
    Item^.Value:=AValue;
  end else begin
    New(Item);
    Item^.Name:=s;
    Item^.Value:=AValue;
    FTree.Add(Item);
  end;
end;

procedure TStringToStringTree.DisposeItem(p: PStringMapItem);
var
  Item: PStringToStringItem absolute p;
begin
  Dispose(Item);
end;

function TStringToStringTree.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
var
  Item1: PStringToStringItem absolute p1;
  Item2: PStringToStringItem absolute p2;
begin
  Result:=(Item1^.Name=Item2^.Name)
      and (Item1^.Value=Item2^.Value);
end;

function TStringToStringTree.CreateCopy(Src: PStringMapItem): PStringMapItem;
var
  SrcItem: PStringToStringItem absolute Src;
  NewItem: PStringToStringItem;
begin
  New(NewItem);
  NewItem^.Name:=SrcItem^.Name;
  NewItem^.Value:=SrcItem^.Value;
  Result:=PStringMapItem(NewItem);
end;

function TStringToStringTree.GetNode(Node: TAvgLvlTreeNode; out Name,
  Value: string): Boolean;
var
  Item: PStringToStringItem;
begin
  if Node<>nil then begin
    Item:=PStringToStringItem(Node.Data);
    Name:=Item^.Name;
    Value:=Item^.Value;
    Result:=true;
  end else begin
    Name:='';
    Value:='';
    Result:=false;
  end;
end;

function TStringToStringTree.GetString(const Name: string; out Value: string): boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToStringItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TStringToStringTree.Add(const Name, Value: string);
begin
  Values[Name]:=Value;
end;

procedure TStringToStringTree.Add(const Name, Value, Delimiter: string);
var
  OldValue: string;
begin
  OldValue:=Values[Name];
  if OldValue<>'' then
    OldValue:=OldValue+Delimiter;
  OldValue:=OldValue+Value;
  Values[Name]:=OldValue;
end;

procedure TStringToStringTree.AddNameValues(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List.Names[i]]:=List.ValueFromIndex[i];
end;

procedure TStringToStringTree.AddValues(List: TStrings);
begin
  AddNames(List);
end;

procedure TStringToStringTree.AddNames(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List[i]]:='';
end;

procedure TStringToStringTree.Delete(const Name: string);
begin
  Remove(Name);
end;

function TStringToStringTree.GetFirst(out Name, Value: string): Boolean;
begin
  Result:=GetNode(Tree.FindLowest,Name,Value);
end;

function TStringToStringTree.GetLast(out Name, Value: string): Boolean;
begin
  Result:=GetNode(Tree.FindHighest,Name,Value);
end;

function TStringToStringTree.GetNext(const Name: string; out NextName,
  NextValue: string): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.Successor;
  Result:=GetNode(Node,NextName,NextValue);
end;

function TStringToStringTree.GetPrev(const Name: string; out PrevName,
  PrevValue: string): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.Precessor;
  Result:=GetNode(Node,PrevName,PrevValue);
end;

function TStringToStringTree.AsText: string;
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  Result:='';
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Result:=Result+Item^.Name+'='+Item^.Value+LineEnding;
    Node:=Node.Successor;
  end;
end;

procedure TStringToStringTree.Assign(Source: TCustomStringMap);
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  if (Source=nil) or (Source.ClassType<>ClassType) then
    raise Exception.Create('invalid class');
  Clear;
  Node:=Source.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Values[Item^.Name]:=Item^.Value;
    Node:=Node.Successor;
  end;
end;

function TStringToStringTree.GetEnumerator: TStringToStringTreeEnumerator;
begin
  Result:=TStringToStringTreeEnumerator.Create(FTree);
end;

{ TStringMapEnumerator }

function TStringMapEnumerator.GetCurrent: string;
begin
  Result:=PStringMapItem(FCurrent.Data)^.Name;
end;

{ TStringMap }

function TStringMap.GetValues(const s: string): boolean;
begin
  Result:=Contains(s);
end;

procedure TStringMap.SetValues(const s: string; AValue: boolean);
begin
  if AValue then
    Add(s)
  else
    Remove(s);
end;

procedure TStringMap.Add(const Name: string);
var
  Node: TAvgLvlTreeNode;
  NewItem: PStringMapItem;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    exit;
  end else begin
    New(NewItem);
    NewItem^.Name:=Name;
    FTree.Add(NewItem);
  end;
end;

function TStringMap.GetEnumerator: TStringMapEnumerator;
begin
  Result:=TStringMapEnumerator.Create(Tree);
end;

{ TStringToStringTreeEnumerator }

function TStringToStringTreeEnumerator.GetCurrent: PStringToStringItem;
begin
  Result:=PStringToStringItem(FCurrent.Data);
end;

{ TAvgLvlTree }

function TAvgLvlTree.Add(Data: Pointer): TAvgLvlTreeNode;
begin
  Result:=NodeClass.Create;
  Result.Data:=Data;
  Add(Result);
end;

procedure TAvgLvlTree.Add(ANode: TAvgLvlTreeNode);
// add a node. If there are already nodes with the same value it will be
// inserted rightmost
var InsertPos: TAvgLvlTreeNode;
  InsertComp: integer;
begin
  ANode.Left:=nil;
  ANode.Right:=nil;
  inc(FCount);
  if fRoot<>nil then begin
    InsertPos:=FindInsertPos(ANode.Data);
    InsertComp:=Compare(ANode.Data,InsertPos.Data);
    ANode.Parent:=InsertPos;
    if InsertComp<0 then begin
      // insert to the left
      InsertPos.Left:=ANode;
    end else begin
      // insert to the right
      InsertPos.Right:=ANode;
    end;
    NodeAdded(ANode);
    BalanceAfterInsert(ANode);
  end else begin
    fRoot:=ANode;
    ANode.Parent:=nil;
    NodeAdded(ANode);
  end;
end;

function TAvgLvlTree.FindLowest: TAvgLvlTreeNode;
begin
  Result:=fRoot;
  if Result<>nil then
    while Result.Left<>nil do Result:=Result.Left;
end;

function TAvgLvlTree.FindHighest: TAvgLvlTreeNode;
begin
  Result:=fRoot;
  if Result<>nil then
    while Result.Right<>nil do Result:=Result.Right;
end;
    
procedure TAvgLvlTree.BalanceAfterDelete(ANode: TAvgLvlTreeNode);
var
  OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight: TAvgLvlTreeNode;
begin
  while ANode<>nil do begin
    if ((ANode.Balance=+1) or (ANode.Balance=-1)) then exit;
    OldParent:=ANode.Parent;
    if (ANode.Balance=0) then begin
      // Treeheight has decreased by one
      if (OldParent=nil) then
        exit;
      if(OldParent.Left=ANode) then
        Inc(OldParent.Balance)
      else
        Dec(OldParent.Balance);
      ANode:=OldParent;
    end else if (ANode.Balance=+2) then begin
      // Node is overweighted to the right
      OldRight:=ANode.Right;
      if (OldRight.Balance>=0) then begin
        // OldRight.Balance is 0 or -1
        // rotate ANode,OldRight left
        RotateLeft(ANode);
        ANode.Balance:=(1-OldRight.Balance); // toggle 0 and 1
        Dec(OldRight.Balance);
        ANode:=OldRight;
      end else begin
        // OldRight.Balance=-1
        { double rotate
          = rotate OldRightLeft,OldRight right
            and then rotate ANode,OldRightLeft left
                  OldParent                           OldParent
                      |                                  |
                    ANode                           OldRightLeft
                       \                               /      \
                    OldRight             =>          ANode    OldRight
                      /                                \         /
               OldRightLeft                OldRightLeftLeft OldRightLeftRight
                   /     \
        OldRightLeftLeft OldRightLeftRight
        }
        OldRightLeft:=OldRight.Left;
        RotateRight(OldRight);
        RotateLeft(ANode);
        if (OldRightLeft.Balance<=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=-1;
        if (OldRightLeft.Balance>=0) then
          OldRight.Balance:=0
        else
          OldRight.Balance:=+1;
        OldRightLeft.Balance:=0;
        ANode:=OldRightLeft;
      end;
    end else begin
      // Node.Balance=-2
      // Node is overweighted to the left
      OldLeft:=ANode.Left;
      if (OldLeft.Balance<=0) then begin
        // rotate OldLeft,ANode right
        RotateRight(ANode);
        ANode.Balance:=(-1-OldLeft.Balance); // toggle 0 and -1
        Inc(OldLeft.Balance);
        ANode:=OldLeft;
      end else begin
        // OldLeft.Balance = 1
        { double rotate left right
          = rotate OldLeft,OldLeftRight left
            and then rotate OldLeft,ANode right
                    OldParent                           OldParent
                        |                                  |
                      ANode                            OldLeftRight
                       /                               /         \
                    OldLeft             =>          OldLeft    ANode
                       \                                \         /
                   OldLeftRight               OldLeftRightLeft OldLeftRightRight
                     /     \
          OldLeftRightLeft OldLeftRightRight
        }
        OldLeftRight:=OldLeft.Right;
        RotateLeft(OldLeft);
        RotateRight(ANode);
        if (OldLeftRight.Balance>=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=+1;
        if (OldLeftRight.Balance<=0) then
          OldLeft.Balance:=0
        else
          OldLeft.Balance:=-1;
        OldLeftRight.Balance:=0;
        ANode:=OldLeftRight;
      end;
    end;
  end;
end;

procedure TAvgLvlTree.DeletingNode(aNode: TAvgLvlTreeNode);
// called by Delete
// Node.Left=nil or Node.Right=nil
begin
  // for descendants to override
end;

procedure TAvgLvlTree.BalanceAfterInsert(ANode: TAvgLvlTreeNode);
var
  OldParent, OldRight, OldLeft: TAvgLvlTreeNode;
begin
  OldParent:=ANode.Parent;
  while (OldParent<>nil) do begin
    if (OldParent.Left=ANode) then begin
      // Node is left child
      dec(OldParent.Balance);
      if (OldParent.Balance=0) then exit;
      if (OldParent.Balance=-1) then begin
        ANode:=OldParent;
        OldParent:=ANode.Parent;
        continue;
      end;
      // OldParent.Balance=-2
      if (ANode.Balance=-1) then begin
        { rotate ANode,ANode.Parent right
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                 /                        \
              ANode                     OldParent
                \                        /
              OldRight               OldRight      }
        RotateRight(OldParent);
        ANode.Balance:=0;
        OldParent.Balance:=0;
      end else begin
        // Node.Balance = +1
        { double rotate
          = rotate ANode,OldRight left and then rotate OldRight,OldParent right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldRight
                   /            =>          /        \
                 ANode                   ANode      OldParent
                    \                       \          /
                   OldRight          OldRightLeft  OldRightRight
                     / \
          OldRightLeft OldRightRight
        }
        OldRight:=ANode.Right;
        RotateLeft(ANode);
        RotateRight(OldParent);
        if (OldRight.Balance<=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=-1;
        if (OldRight.Balance=-1) then
          OldParent.Balance:=1
        else
          OldParent.Balance:=0;
        OldRight.Balance:=0;
      end;
      exit;
    end else begin
      // Node is right child
      Inc(OldParent.Balance);
      if (OldParent.Balance=0) then exit;
      if (OldParent.Balance=+1) then begin
        ANode:=OldParent;
        OldParent:=ANode.Parent;
        continue;
      end;
      // OldParent.Balance = +2
      if(ANode.Balance=+1) then begin
        { rotate OldParent,ANode left
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                    \                   /
                  ANode               OldParent
                   /                      \
                OldLeft                 OldLeft      }
        RotateLeft(OldParent);
        ANode.Balance:=0;
        OldParent.Balance:=0;
      end else begin
        // Node.Balance = -1
        { double rotate
          = rotate OldLeft,ANode right and then rotate OldParent,OldLeft right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldLeft
                     \            =>        /       \
                    ANode               OldParent   ANode
                     /                     \          /
                  OldLeft          OldLeftLeft  OldLeftRight
                    / \
         OldLeftLeft OldLeftRight
        }
        OldLeft:=ANode.Left;
        RotateRight(ANode);
        RotateLeft(OldParent);
        if (OldLeft.Balance>=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=+1;
        if (OldLeft.Balance=+1) then
          OldParent.Balance:=-1
        else
          OldParent.Balance:=0;
        OldLeft.Balance:=0;
      end;
      exit;
    end;
  end;
end;

procedure TAvgLvlTree.Clear;

  procedure DeleteNode(ANode: TAvgLvlTreeNode);
  begin
    if ANode<>nil then begin
      if ANode.Left<>nil then DeleteNode(ANode.Left);
      if ANode.Right<>nil then DeleteNode(ANode.Right);
    end;
    ANode.Free;
  end;

// Clear
begin
  // DeleteNode(fRoot);
  fRoot:=nil;
  FCount:=0;
end;

constructor TAvgLvlTree.Create(OnCompareMethod: TListSortCompare);
begin
  inherited Create;
  FOnCompare:=OnCompareMethod;
  Init;
end;

constructor TAvgLvlTree.CreateObjectCompare(OnCompareMethod: TObjectSortCompare);
begin
  inherited Create;
  FOnObjectCompare:=OnCompareMethod;
  Init;
end;

constructor TAvgLvlTree.Create;
begin
  Create(@ComparePointer);
end;

procedure TAvgLvlTree.Delete(ANode: TAvgLvlTreeNode);
var
  OldParent: TAvgLvlTreeNode;
  Child: TAvgLvlTreeNode;
begin
  if (ANode.Left<>nil) and (ANode.Right<>nil) then begin
    // ANode has both: Left and Right
    // Switch ANode position with Successor
    // Because ANode.Right<>nil the Successor is a child of ANode
    SwitchPositionWithSuccessor(ANode,ANode.Successor);
  end;
  // left or right is nil
  DeletingNode(aNode);
  OldParent:=ANode.Parent;
  ANode.Parent:=nil;
  if ANode.Left<>nil then
    Child:=ANode.Left
  else
    Child:=ANode.Right;
  if Child<>nil then
    Child.Parent:=OldParent;
  if (OldParent<>nil) then begin
    // Node has parent
    if (OldParent.Left=ANode) then begin
      // Node is left child of OldParent
      OldParent.Left:=Child;
      Inc(OldParent.Balance);
    end else begin
      // Node is right child of OldParent
      OldParent.Right:=Child;
      Dec(OldParent.Balance);
    end;
    BalanceAfterDelete(OldParent);
  end else begin
    // Node was Root
    fRoot:=Child;
  end;
  dec(FCount);
  ANode.Free;
end;

function TAvgLvlTree.Remove(Data: Pointer): boolean;
var ANode: TAvgLvlTreeNode;
begin
  ANode:=Find(Data);
  if ANode<>nil then begin
    Delete(ANode);
    Result:=true;
  end else
    Result:=false;
end;

function TAvgLvlTree.RemovePointer(Data: Pointer): boolean;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FindPointer(Data);
  if ANode<>nil then begin
    Delete(ANode);
    Result:=true;
  end else
    Result:=false;
end;

destructor TAvgLvlTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TAvgLvlTree.GetEnumerator: TAvgLvlTreeNodeEnumerator;
begin
  Result:=TAvgLvlTreeNodeEnumerator.Create(Self,true);
end;

function TAvgLvlTree.GetEnumeratorHighToLow: TAvgLvlTreeNodeEnumerator;
begin
  Result:=TAvgLvlTreeNodeEnumerator.Create(Self,false);
end;

function TAvgLvlTree.Find(Data: Pointer): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Compare(Data,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TAvgLvlTree.FindKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=OnCompareKeyWithData(Key,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TAvgLvlTree.FindNearestKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=OnCompareKeyWithData(Key,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAvgLvlTree.FindLeftMostKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
var
  LeftNode: TAvgLvlTreeNode;
begin
  Result:=FindKey(Key,OnCompareKeyWithData);
  if Result=nil then exit;
  repeat
    LeftNode:=Result.Precessor;
    if (LeftNode=nil) or (OnCompareKeyWithData(Key,LeftNode.Data)<>0) then break;
    Result:=LeftNode;
  until false;
end;

function TAvgLvlTree.FindRightMostKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
var
  RightNode: TAvgLvlTreeNode;
begin
  Result:=FindKey(Key,OnCompareKeyWithData);
  if Result=nil then exit;
  repeat
    RightNode:=Result.Successor;
    if (RightNode=nil) or (OnCompareKeyWithData(Key,RightNode.Data)<>0) then break;
    Result:=RightNode;
  until false;
end;

function TAvgLvlTree.FindLeftMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
var
  LeftNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode<>nil then begin
    Data:=ANode.Data;
    Result:=ANode;
    repeat
      LeftNode:=Result.Precessor;
      if (LeftNode=nil) or (Compare(Data,LeftNode.Data)<>0) then break;
      Result:=LeftNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAvgLvlTree.FindRightMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
var
  RightNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode<>nil then begin
    Data:=ANode.Data;
    Result:=ANode;
    repeat
      RightNode:=Result.Successor;
      if (RightNode=nil) or (Compare(Data,RightNode.Data)<>0) then break;
      Result:=RightNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAvgLvlTree.FindNearest(Data: Pointer): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Compare(Data,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAvgLvlTree.FindPointer(Data: Pointer): TAvgLvlTreeNode;
// same as Find, but not comparing for key, but same Data too
begin
  Result:=FindLeftMost(Data);
  while (Result<>nil) do begin
    if Result.Data=Data then break;
    Result:=Result.Successor;
    if Result=nil then exit(nil);
    if Compare(Data,Result.Data)<>0 then exit(nil);
  end;
end;

function TAvgLvlTree.FindLeftMost(Data: Pointer): TAvgLvlTreeNode;
var
  Left: TAvgLvlTreeNode;
begin
  Result:=Find(Data);
  while (Result<>nil) do begin
    Left:=Result.Precessor;
    if (Left=nil) or (Compare(Data,Left.Data)<>0) then break;
    Result:=Left;
  end;
end;

function TAvgLvlTree.FindRightMost(Data: Pointer): TAvgLvlTreeNode;
var
  Right: TAvgLvlTreeNode;
begin
  Result:=Find(Data);
  while (Result<>nil) do begin
    Right:=Result.Successor;
    if (Right=nil) or (Compare(Data,Right.Data)<>0) then break;
    Result:=Right;
  end;
end;

function TAvgLvlTree.FindInsertPos(Data: Pointer): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Compare(Data,Result.Data);
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAvgLvlTree.FindSuccessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.Successor
  else
    Result:=nil;
end;

function TAvgLvlTree.FindPrecessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.Precessor
  else
    Result:=nil;
end;

procedure TAvgLvlTree.MoveDataLeftMost(var ANode: TAvgLvlTreeNode);
var LeftMost, PreNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode=nil then exit;
  LeftMost:=ANode;
  repeat
    PreNode:=LeftMost.Precessor;
    if (PreNode=nil) or (Compare(ANode,PreNode)<>0) then break;
    LeftMost:=PreNode;
  until false;
  if LeftMost=ANode then exit;
  Data:=LeftMost.Data;
  LeftMost.Data:=ANode.Data;
  ANode.Data:=Data;
  ANode:=LeftMost;
end;

procedure TAvgLvlTree.MoveDataRightMost(var ANode: TAvgLvlTreeNode);
var RightMost, PostNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode=nil then exit;
  RightMost:=ANode;
  repeat
    PostNode:=RightMost.Successor;
    if (PostNode=nil) or (Compare(ANode,PostNode)<>0) then break;
    RightMost:=PostNode;
  until false;
  if RightMost=ANode then exit;
  Data:=RightMost.Data;
  RightMost.Data:=ANode.Data;
  ANode.Data:=Data;
  ANode:=RightMost;
end;

procedure TAvgLvlTree.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAvgLvlTree.ConsistencyCheck: '+Msg);
  end;

var
  RealCount: SizeInt;
begin
  RealCount:=0;
  if FRoot<>nil then begin
    FRoot.ConsistencyCheck(Self);
    RealCount:=FRoot.GetCount;
  end;
  if Count<>RealCount then
    E('Count<>RealCount');
end;

procedure TAvgLvlTree.FreeAndClear;

  procedure FreeNode(ANode: TAvgLvlTreeNode);
  begin
    if ANode=nil then exit;
    FreeNode(ANode.Left);
    FreeNode(ANode.Right);
    if ANode.Data<>nil then TObject(ANode.Data).Free;
    ANode.Data:=nil;
  end;

// TAvgLvlTree.FreeAndClear
begin
  // free all data
  // FreeNode(fRoot);
  // free all nodes
  Clear;
end;

procedure TAvgLvlTree.FreeAndDelete(ANode: TAvgLvlTreeNode);
var OldData: TObject;
begin
  OldData:=TObject(ANode.Data);
  Delete(ANode);
  OldData.Free;
end;

function TAvgLvlTree.Equals(Obj: TObject): boolean;
begin
  if Obj is TAvgLvlTree then
    Result:=IsEqual(TAvgLvlTree(Obj),false)
  else
    Result:=inherited Equals(Obj);
end;

function TAvgLvlTree.IsEqual(aTree: TAvgLvlTree; CheckDataPointer: boolean
  ): boolean;
var
  MyNode: TAvgLvlTreeNode;
  OtherNode: TAvgLvlTreeNode;
begin
  if aTree=Self then exit(true);
  Result:=false;
  if aTree=nil then exit;
  if Count<>aTree.Count then exit;
  if OnCompare<>aTree.OnCompare then exit;
  if OnObjectCompare<>aTree.OnObjectCompare then exit;
  if NodeClass<>aTree.NodeClass then exit;
  MyNode:=FindLowest;
  OtherNode:=aTree.FindLowest;
  while MyNode<>nil do begin
    if OtherNode=nil then exit;
    if CheckDataPointer then begin
      if MyNode.Data<>OtherNode.Data then exit;
    end else begin
      if Compare(MyNode.Data,OtherNode.Data)<>0 then exit;
    end;
    MyNode:=MyNode.Successor;;
    OtherNode:=OtherNode.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TAvgLvlTree.Assign(aTree: TAvgLvlTree);

  procedure AssignNode(var MyNode: TAvgLvlTreeNode; OtherNode: TAvgLvlTreeNode);
  begin
    MyNode:=NodeClass.Create;
    MyNode.Data:=OtherNode.Data;
    MyNode.Balance:=OtherNode.Balance;
    if OtherNode.Left<>nil then begin
      AssignNode(MyNode.Left,OtherNode.Left);
      MyNode.Left.Parent:=MyNode;
    end;
    if OtherNode.Right<>nil then begin
      AssignNode(MyNode.Right,OtherNode.Right);
      MyNode.Right.Parent:=MyNode;
    end;
  end;

begin
  if aTree=nil then
    raise Exception.Create('TAvgLvlTree.Assign aTree=nil');
  if IsEqual(aTree,true) then exit;
  Clear;
  SetCompares(aTree.OnCompare,aTree.OnObjectCompare);
  FNodeClass:=aTree.NodeClass;
  if aTree.Root<>nil then
    AssignNode(fRoot,aTree.Root);
  FCount:=aTree.Count;
end;

procedure TAvgLvlTree.WriteReportToStream(s: TStream);

  procedure WriteStr(const Txt: string);
  begin
    if Txt='' then exit;
    s.Write(Txt[1],length(Txt));
  end;

  procedure WriteTreeNode(ANode: TAvgLvlTreeNode);
  var
    b: String;
    IsLeft: boolean;
    AParent: TAvgLvlTreeNode;
    WasLeft: Boolean;
  begin
    if ANode=nil then exit;
    WriteTreeNode(ANode.Right);
    AParent:=ANode;
    WasLeft:=false;
    b:='';
    while AParent<>nil do begin
      if AParent.Parent=nil then begin
        if AParent=ANode then
          b:='--'+b
        else
          b:='  '+b;
        break;
      end;
      IsLeft:=AParent.Parent.Left=AParent;
      if AParent=ANode then begin
        if IsLeft then
          b:='\-'
        else
          b:='/-';
      end else begin
        if WasLeft=IsLeft then
          b:='  '+b
        else
          b:='| '+b;
      end;
      WasLeft:=IsLeft;
      AParent:=AParent.Parent;
    end;
    b+=NodeToReportStr(ANode)+LineEnding;
    WriteStr(b);
    WriteTreeNode(ANode.Left);
  end;

// TAvgLvlTree.WriteReportToStream
begin
  WriteStr('-Start-of-AVL-Tree-------------------'+LineEnding);
  WriteTreeNode(fRoot);
  WriteStr('-End-Of-AVL-Tree---------------------'+LineEnding);
end;

function TAvgLvlTree.NodeToReportStr(aNode: TAvgLvlTreeNode): string;
begin
  Result:=Format('%p      Self=%p  Parent=%p  Balance=%d',
             [aNode.Data, Pointer(aNode),Pointer(aNode.Parent), aNode.Balance]);
end;

function TAvgLvlTree.ReportAsString: string;
var ms: TMemoryStream;
begin
  Result:='';
  ms:=TMemoryStream.Create;
  try
    WriteReportToStream(ms);
    ms.Position:=0;
    SetLength(Result,ms.Size);
    if Result<>'' then
      ms.Read(Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

procedure TAvgLvlTree.SetOnCompare(const AValue: TListSortCompare);
begin
  if AValue=nil then
    SetCompares(nil,FOnObjectCompare)
  else
    SetCompares(AValue,nil);
end;

procedure TAvgLvlTree.SetOnObjectCompare(const AValue: TObjectSortCompare);
begin
  if AValue=nil then
    SetCompares(FOnCompare,nil)
  else
    SetCompares(nil,AValue);
end;

procedure TAvgLvlTree.SetCompares(const NewCompare: TListSortCompare;
  const NewObjectCompare: TObjectSortCompare);
var List: PPointer;
  ANode: TAvgLvlTreeNode;
  i, OldCount: integer;
begin
  if (FOnCompare=NewCompare) and (FOnObjectCompare=NewObjectCompare) then exit;
  if Count<=1 then begin
    FOnCompare:=NewCompare;
    FOnObjectCompare:=NewObjectCompare;
    exit;
  end;
  // sort the tree again
  OldCount:=Count;
  GetMem(List,SizeOf(Pointer)*OldCount);
  try
    // save the data in a list
    ANode:=FindLowest;
    i:=0;
    while ANode<>nil do begin
      List[i]:=ANode.Data;
      inc(i);
      ANode:=ANode.Successor;
    end;
    // clear the tree
    Clear;
    // set the new compare function
    FOnCompare:=NewCompare;
    FOnObjectCompare:=NewObjectCompare;
    // re-add all nodes
    for i:=0 to OldCount-1 do
      Add(List[i]);
  finally
    FreeMem(List);
  end;
end;

procedure TAvgLvlTree.RotateLeft(aNode: TAvgLvlTreeNode);
{    Parent                Parent
       |                     |
      Node        =>       OldRight
      /  \                  /
   Left OldRight          Node
          /               /  \
     OldRightLeft      Left OldRightLeft  }
var
  OldRight: TAvgLvlTreeNode;
  AParent: TAvgLvlTreeNode;
  OldRightLeft: TAvgLvlTreeNode;
begin
  OldRight:=aNode.Right;
  OldRightLeft:=OldRight.Left;
  AParent:=aNode.Parent;
  if AParent<>nil then begin
    if AParent.Left=aNode then
      AParent.Left:=OldRight
    else
      AParent.Right:=OldRight;
  end else
    fRoot:=OldRight;
  OldRight.Parent:=AParent;
  aNode.Parent:=OldRight;
  aNode.Right:=OldRightLeft;
  if OldRightLeft<>nil then
    OldRightLeft.Parent:=aNode;
  OldRight.Left:=aNode;
end;

procedure TAvgLvlTree.RotateRight(aNode: TAvgLvlTreeNode);
{       Parent              Parent
          |                   |
         Node        =>     OldLeft
         /   \                 \
    OldLeft  Right            Node
        \                     /  \
   OldLeftRight      OldLeftRight Right  }
var
  OldLeft: TAvgLvlTreeNode;
  AParent: TAvgLvlTreeNode;
  OldLeftRight: TAvgLvlTreeNode;
begin
  OldLeft:=aNode.Left;
  OldLeftRight:=OldLeft.Right;
  AParent:=aNode.Parent;
  if AParent<>nil then begin
    if AParent.Left=aNode then
      AParent.Left:=OldLeft
    else
      AParent.Right:=OldLeft;
  end else
    fRoot:=OldLeft;
  OldLeft.Parent:=AParent;
  aNode.Parent:=OldLeft;
  aNode.Left:=OldLeftRight;
  if OldLeftRight<>nil then
    OldLeftRight.Parent:=aNode;
  OldLeft.Right:=aNode;
end;

procedure TAvgLvlTree.SwitchPositionWithSuccessor(aNode,
  aSuccessor: TAvgLvlTreeNode);
{ called by delete, when aNode.Left<>nil and aNode.Right<>nil
  Switch ANode position with Successor
  Because ANode.Right<>nil the Successor is a child of ANode }
var
  OldBalance: Integer;
  OldParent: TAvgLvlTreeNode;
  OldLeft: TAvgLvlTreeNode;
  OldRight: TAvgLvlTreeNode;
  OldSuccParent: TAvgLvlTreeNode;
  OldSuccLeft: TAvgLvlTreeNode;
  OldSuccRight: TAvgLvlTreeNode;
begin
  OldBalance:=aNode.Balance;
  aNode.Balance:=aSuccessor.Balance;
  aSuccessor.Balance:=OldBalance;

  OldParent:=aNode.Parent;
  OldLeft:=aNode.Left;
  OldRight:=aNode.Right;
  OldSuccParent:=aSuccessor.Parent;
  OldSuccLeft:=aSuccessor.Left;
  OldSuccRight:=aSuccessor.Right;

  if OldParent<>nil then begin
    if OldParent.Left=aNode then
      OldParent.Left:=aSuccessor
    else
      OldParent.Right:=aSuccessor;
  end else
    fRoot:=aSuccessor;
  aSuccessor.Parent:=OldParent;

  if OldSuccParent<>aNode then begin
    if OldSuccParent.Left=aSuccessor then
      OldSuccParent.Left:=aNode
    else
      OldSuccParent.Right:=aNode;
    aSuccessor.Right:=OldRight;
    aNode.Parent:=OldSuccParent;
    if OldRight<>nil then
      OldRight.Parent:=aSuccessor;
  end else begin
    {  aNode            aSuccessor
         \          =>    \
         aSuccessor       aNode  }
    aSuccessor.Right:=aNode;
    aNode.Parent:=aSuccessor;
  end;

  aNode.Left:=OldSuccLeft;
  if OldSuccLeft<>nil then
    OldSuccLeft.Parent:=aNode;
  aNode.Right:=OldSuccRight;
  if OldSuccRight<>nil then
    OldSuccRight.Parent:=aNode;
  aSuccessor.Left:=OldLeft;
  if OldLeft<>nil then
    OldLeft.Parent:=aSuccessor;
end;

procedure TAvgLvlTree.Init;
begin
  FNodeClass:=TAvgLvlTreeNode;
end;

procedure TAvgLvlTree.NodeAdded(aNode: TAvgLvlTreeNode);
begin
  // for descendants to override
end;

function TAvgLvlTree.Compare(Data1, Data2: Pointer): integer;
begin
  if Assigned(FOnCompare) then
    Result:=FOnCompare(Data1,Data2)
  else
    Result:=FOnObjectCompare(Self,Data1,Data2);
end;

{ TAvgLvlTreeNode }

function TAvgLvlTreeNode.TreeDepth: integer;
// longest WAY down. e.g. only one node => 0 !
var LeftDepth, RightDepth: integer;
begin
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if LeftDepth>RightDepth then
    Result:=LeftDepth
  else
    Result:=RightDepth;
end;

procedure TAvgLvlTreeNode.ConsistencyCheck(Tree: TAvgLvlTree);

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAvgLvlTreeNode.ConsistencyCheck: '+Msg);
  end;

var
  LeftDepth: SizeInt;
  RightDepth: SizeInt;
begin
  // test left child
  if Left<>nil then begin
    if Left.Parent<>Self then
      E('Left.Parent<>Self');
    if Tree.Compare(Left.Data,Data)>0 then
      E('Compare(Left.Data,Data)>0');
    Left.ConsistencyCheck(Tree);
  end;
  // test right child
  if Right<>nil then begin
    if Right.Parent<>Self then
      E('Right.Parent<>Self');
    if Tree.Compare(Data,Right.Data)>0 then
      E('Compare(Data,Right.Data)>0');
    Right.ConsistencyCheck(Tree);
  end;
  // test balance
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if Balance<>(RightDepth-LeftDepth) then
    E('Balance['+IntToStr(Balance)+']<>(RightDepth['+IntToStr(RightDepth)+']-LeftDepth['+IntToStr(LeftDepth)+'])');
end;

function TAvgLvlTreeNode.GetCount: SizeInt;
begin
  Result:=1;
  if Left<>nil then inc(Result,Left.GetCount);
  if Right<>nil then inc(Result,Right.GetCount);
end;

function TAvgLvlTreeNode.Successor: TAvgLvlTreeNode;
begin
  Result:=Right;
  if Result<>nil then begin
    while (Result.Left<>nil) do Result:=Result.Left;
  end else begin
    Result:=Self;
    while (Result.Parent<>nil) and (Result.Parent.Right=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

function TAvgLvlTreeNode.Precessor: TAvgLvlTreeNode;
begin
  Result:=Left;
  if Result<>nil then begin
    while (Result.Right<>nil) do Result:=Result.Right;
  end else begin
    Result:=Self;
    while (Result.Parent<>nil) and (Result.Parent.Left=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

{ TIndexedAVLTree }

function TIndexedAVLTree.GetItems(Index: SizeInt): Pointer;
begin
  Result:=GetNodeAtIndex(Index).Data;
end;

procedure TIndexedAVLTree.DeletingNode(aNode: TAvgLvlTreeNode);
var
  aParent: TAvgLvlTreeNode;
begin
  fLastNode:=nil;
  repeat
    aParent:=aNode.Parent;
    if (aParent=nil) then exit;
    if aParent.Left=aNode then
      TIndexedAVLTreeNode(aParent).LeftCount-=1;
    aNode:=aParent;
  until false;
end;

procedure TIndexedAVLTree.Init;
begin
  FNodeClass:=TIndexedAVLTreeNode;
end;

procedure TIndexedAVLTree.NodeAdded(aNode: TAvgLvlTreeNode);
var
  aParent: TAvgLvlTreeNode;
begin
  fLastNode:=nil;
  repeat
    aParent:=aNode.Parent;
    if (aParent=nil) then exit;
    if aParent.Left=aNode then
      TIndexedAVLTreeNode(aParent).LeftCount+=1;
    aNode:=aParent;
  until false;
end;

procedure TIndexedAVLTree.RotateLeft(aNode: TAvgLvlTreeNode);
{    Parent                Parent
       |                     |
    CurNode        =>     OldRight
      /  \                  /
   Left OldRight         CurNode
          /               /  \
     OldRightLeft      Left OldRightLeft  }
var
  CurNode: TIndexedAVLTreeNode absolute aNode;
  OldRight: TIndexedAVLTreeNode;
begin
  OldRight:=TIndexedAVLTreeNode(aNode.Right);
  inherited RotateLeft(aNode);
  OldRight.LeftCount += 1+CurNode.LeftCount;
end;

procedure TIndexedAVLTree.RotateRight(aNode: TAvgLvlTreeNode);
{       Parent              Parent
          |                   |
        CurNode        =>   OldLeft
         /   \                 \
    OldLeft  Right          CurNode
        \                     /  \
   OldLeftRight      OldLeftRight Right  }
var
  CurNode: TIndexedAVLTreeNode absolute aNode;
  OldLeft: TIndexedAVLTreeNode;
begin
  OldLeft:=TIndexedAVLTreeNode(aNode.Left);
  inherited RotateRight(aNode);
  CurNode.LeftCount -= (1 + OldLeft.LeftCount);
end;

procedure TIndexedAVLTree.SwitchPositionWithSuccessor(aNode,
  aSuccessor: TAvgLvlTreeNode);
var
  CurNode: TIndexedAVLTreeNode absolute aNode;
  CurSucc: TIndexedAVLTreeNode absolute aSuccessor;
  h: SizeInt;
begin
  h:=CurNode.LeftCount;
  CurNode.LeftCount:=CurSucc.LeftCount;
  CurSucc.LeftCount:=h;
  inherited SwitchPositionWithSuccessor(aNode, aSuccessor);
end;

function TIndexedAVLTree.GetNodeAtIndex(Index: integer): TIndexedAVLTreeNode;

  procedure RaiseOutOfBounds;
  begin
    raise Exception.Create('TIndexedAVLTree: Index '+IntToStr(Index)+' out of bounds 0..'+IntToStr(Count));
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseOutOfBounds;

  if fLastNode<>nil then begin
    if Index=fLastIndex then
      exit(fLastNode)
    else if Index=fLastIndex+1 then begin
      fLastIndex:=Index;
      fLastNode:=TIndexedAVLTreeNode(fLastNode.Successor);
      exit(fLastNode);
    end else if Index=fLastIndex-1 then begin
      fLastIndex:=Index;
      fLastNode:=TIndexedAVLTreeNode(fLastNode.Precessor);
      exit(fLastNode);
    end;
  end;

  fLastIndex:=Index;
  Result:=TIndexedAVLTreeNode(Root);
  repeat
    if Result.LeftCount>Index then
      Result:=TIndexedAVLTreeNode(Result.Left)
    else if Result.LeftCount=Index then begin
      fLastNode:=TIndexedAVLTreeNode(Result);
      exit;
    end
    else begin
      Index -= Result.LeftCount+1;
      Result:=TIndexedAVLTreeNode(Result.Right);
    end;
  until false;
end;

function TIndexedAVLTree.NodeToIndex(Node: TAvgLvlTreeNode): SizeInt;
var
  CurNode: TIndexedAVLTreeNode;
  CurParent: TIndexedAVLTreeNode;
begin
  if Node=nil then exit(-1);

  if fLastNode=Node then
    exit(fLastIndex);

  CurNode:=TIndexedAVLTreeNode(Node);
  Result:=CurNode.LeftCount;
  repeat
    CurParent:=TIndexedAVLTreeNode(CurNode.Parent);
    if CurParent=nil then break;
    if CurParent.Right=CurNode then
      inc(Result,CurParent.LeftCount+1);
    CurNode:=CurParent;
  until false;

  fLastNode:=TIndexedAVLTreeNode(Node);
  fLastIndex:=Result;
end;

function TIndexedAVLTree.IndexOf(Data: Pointer): SizeInt;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindPointer(Data);
  if Node=nil then exit(-1);
  Result:=NodeToIndex(Node);
end;

procedure TIndexedAVLTree.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TIndexedAVLTree.ConsistencyCheck: '+Msg);
  end;

var
  Node: TAvgLvlTreeNode;
  i: SizeInt;
  LeftCount: SizeInt;
begin
  inherited ConsistencyCheck;
  i:=0;
  for Node in Self do begin
    if Node.Left<>nil then
      LeftCount:=Node.Left.GetCount
    else
      LeftCount:=0;
    if TIndexedAVLTreeNode(Node).LeftCount<>LeftCount then
      E(Format('Node.LeftCount=%d<>%d',[TIndexedAVLTreeNode(Node).LeftCount,LeftCount]));

    if GetNodeAtIndex(i)<>Node then
      E(Format('GetNodeAtIndex(%d)<>%P',[i,Node]));
    fLastNode:=nil;
    if GetNodeAtIndex(i)<>Node then
      E(Format('GetNodeAtIndex(%d)<>%P',[i,Node]));

    if NodeToIndex(Node)<>i then
      E(Format('NodeToIndex(%P)<>%d',[Node,i]));
    fLastNode:=nil;
    if NodeToIndex(Node)<>i then
      E(Format('NodeToIndex(%P)<>%d',[Node,i]));

    inc(i);
  end;
end;

function TIndexedAVLTree.NodeToReportStr(aNode: TAvgLvlTreeNode): string;
begin
  Result:=inherited NodeToReportStr(aNode)+' LeftCount='+IntToStr(TIndexedAVLTreeNode(aNode).LeftCount);
end;

{ TPointerToPointerTree }

function TPointerToPointerTree.GetCount: SizeInt;
begin
  Result:=FItems.Count;
end;

function TPointerToPointerTree.GetValues(const Key: Pointer): Pointer;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Result:=PPointerToPointerItem(Node.Data)^.Value
  else
    Result:=nil;
end;

procedure TPointerToPointerTree.SetValues(const Key: Pointer;
  const AValue: Pointer);
var
  NewItem: PPointerToPointerItem;
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if (Node<>nil) then
    PPointerToPointerItem(Node.Data)^.Value:=AValue
  else begin
    New(NewItem);
    NewItem^.Key:=Key;
    NewItem^.Value:=AValue;
    FItems.Add(NewItem);
  end;
end;

function TPointerToPointerTree.FindNode(const Key: Pointer): TAvgLvlTreeNode;
begin
  Result:=FItems.FindKey(Key,@ComparePointerWithPtrToPtrItem)
end;

function TPointerToPointerTree.GetNode(Node: TAvgLvlTreeNode; out Key,
  Value: Pointer): Boolean;
var
  Item: PPointerToPointerItem;
begin
  if Node<>nil then begin
    Item:=PPointerToPointerItem(Node.Data);
    Key:=Item^.Key;
    Value:=Item^.Value;
    Result:=true;
  end else begin
    Key:=nil;
    Value:=nil;
    Result:=false;
  end;
end;

constructor TPointerToPointerTree.Create;
begin
  FItems:=TAvgLvlTree.Create(@ComparePointerToPointerItems);
end;

destructor TPointerToPointerTree.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TPointerToPointerTree.Clear;
var
  Node: TAvgLvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PPointerToPointerItem(Node.Data);
    Dispose(Item);
    Node:=Node.Successor;
  end;
  FItems.Clear;
end;

procedure TPointerToPointerTree.ClearWithFree;
var
  Node: TAvgLvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PPointerToPointerItem(Node.Data);
    TObject(Item^.Value).Free;
    Dispose(Item);
    Node:=Node.Successor;
  end;
  FItems.Clear;
end;

function TPointerToPointerTree.Equals(Obj: TObject): boolean;
begin
  if Obj is TPointerToPointerTree then
    Result:=IsEqual(TPointerToPointerTree(Obj))
  else
    Result:=inherited Equals(Obj);
end;

function TPointerToPointerTree.IsEqual(aTree: TPointerToPointerTree): boolean;
var
  MyNode: TAvgLvlTreeNode;
  OtherNode: TAvgLvlTreeNode;
  MyItem: PPointerToPointerItem;
  OtherItem: PPointerToPointerItem;
begin
  if aTree=Self then exit(true);
  Result:=false;
  if aTree=nil then exit;
  if Count<>aTree.Count then exit;
  if FItems.OnCompare<>aTree.FItems.OnCompare then exit;
  if FItems.OnObjectCompare<>aTree.FItems.OnObjectCompare then exit;
  if FItems.NodeClass<>aTree.FItems.NodeClass then exit;
  MyNode:=FItems.FindLowest;
  OtherNode:=aTree.FItems.FindLowest;
  while MyNode<>nil do begin
    if OtherNode=nil then exit;
    MyItem:=PPointerToPointerItem(MyNode.Data);
    OtherItem:=PPointerToPointerItem(OtherNode.Data);
    if (MyItem^.Key<>OtherItem^.Key)
    or (MyItem^.Value<>OtherItem^.Value) then exit;
    MyNode:=MyNode.Successor;
    OtherNode:=OtherNode.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TPointerToPointerTree.Assign(aTree: TPointerToPointerTree);
var
  Node: TAvgLvlTreeNode;
  SrcItem, MyItem: PPointerToPointerItem;
begin
  if aTree=nil then
    raise Exception.Create('TPointerToPointerTree.Assign aTree=nil');
  if IsEqual(aTree) then exit;
  // clear and clone node structure, copying Data references
  FItems.Assign(aTree.FItems);
  // clone Data
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    SrcItem:=PPointerToPointerItem(Node.Data);
    New(MyItem);
    MyItem^:=SrcItem^;
    Node.Data:=MyItem;
    Node:=Node.Successor;
  end;
end;

procedure TPointerToPointerTree.Remove(Key: Pointer);
var
  Node: TAvgLvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FindNode(Key);
  if Node=nil then exit;
  Item:=PPointerToPointerItem(Node.Data);
  FItems.Delete(Node);
  Dispose(Item);
end;

function TPointerToPointerTree.Contains(const Key: Pointer): Boolean;
begin
  Result:=FindNode(Key)<>nil;
end;

function TPointerToPointerTree.GetFirst(out Key, Value: Pointer): Boolean;
begin
  Result:=GetNode(Tree.FindLowest,Key,Value);
end;

function TPointerToPointerTree.GetLast(out Key, Value: Pointer): Boolean;
begin
  Result:=GetNode(Tree.FindHighest,Key,Value);
end;

function TPointerToPointerTree.GetNext(const Key: Pointer; out NextKey,
  NextValue: Pointer): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Node.Successor;
  Result:=GetNode(Node,NextKey,NextValue);
end;

function TPointerToPointerTree.GetPrev(const Key: Pointer; out PrevKey,
  PrevValue: Pointer): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Node.Precessor;
  Result:=GetNode(Node,PrevKey,PrevValue);
end;

function TPointerToPointerTree.GetEnumerator: TPointerToPointerEnumerator;
begin
  Result:=TPointerToPointerEnumerator.Create(Tree);
end;

function TPointerToPointerTree.
  GetEnumeratorHighToLow: TPointerToPointerEnumerator;
begin
  Result:=TPointerToPointerEnumerator.Create(Tree);
  Result.fHighToLow:=true;
end;

{ TCustomStringMapEnumerator }

constructor TCustomStringMapEnumerator.Create(Tree: TAvgLvlTree);
begin
  FTree:=Tree;
end;

function TCustomStringMapEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FTree.FindLowest
  else
    FCurrent:=FCurrent.Successor;
  Result:=FCurrent<>nil;
end;

{ TCustomStringMap }

function TCustomStringMap.GetCompareItemsFunc: TListSortCompare;
begin
  Result:=Tree.OnCompare;
end;

function TCustomStringMap.FindNode(const s: string): TAvgLvlTreeNode;
begin
  Result:=FTree.FindKey(Pointer(s),FCompareKeyItemFunc);
end;

procedure TCustomStringMap.DisposeItem(p: PStringMapItem);
begin
  Dispose(p);
end;

function TCustomStringMap.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
begin
  Result:=p1^.Name=p2^.Name;
end;

function TCustomStringMap.CreateCopy(Src: PStringMapItem): PStringMapItem;
begin
  New(Result);
  Result^.Name:=Src^.Name;
end;

constructor TCustomStringMap.Create(TheCaseSensitive: boolean);
begin
  if TheCaseSensitive then
    Create(@CompareStringToStringItems,@CompareAnsiStringWithStrToStrItem,true)
  else
    Create(@CompareStringToStringItemsI,@CompareAnsiStringWithStrToStrItemI,false);
end;

constructor TCustomStringMap.Create(const ACompareItems,
  ACompareNameWithItem: TListSortCompare; TheCaseSensitive: boolean);
begin
  FCaseSensitive:=TheCaseSensitive;
  FCompareKeyItemFunc:=ACompareNameWithItem;
  FTree:=TAvgLvlTree.Create(ACompareItems);
end;

destructor TCustomStringMap.Destroy;
begin
  Clear;
  FTree.Free;
  FTree:=nil;
  inherited Destroy;
end;

procedure TCustomStringMap.Clear;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    DisposeItem(PStringMapItem(Node.Data));
    Node:=Node.Successor;
  end;
  FTree.Clear;
end;

function TCustomStringMap.Contains(const s: string): boolean;
begin
  Result:=FindNode(s)<>nil;
end;

procedure TCustomStringMap.GetNames(List: TStrings);
var
  Node: TAvgLvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringMapItem(Node.Data);
    List.Add(Item^.Name);
    Node:=Node.Successor;
  end;
end;

procedure TCustomStringMap.Remove(const Name: string);
var
  Node: TAvgLvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Item:=PStringMapItem(Node.Data);
    FTree.Delete(Node);
    Dispose(Item);
  end;
end;

function TCustomStringMap.Count: SizeInt;
begin
  Result:=Tree.Count;
end;

function TCustomStringMap.Equals(OtherTree: TCustomStringMap): boolean;
var
  Node: TAvgLvlTreeNode;
  OtherNode: TAvgLvlTreeNode;
  OtherItem: PStringMapItem;
  Item: PStringMapItem;
begin
  Result:=false;
  if (OtherTree=nil) or (OtherTree.ClassType<>ClassType) then exit;
  if Tree.Count<>OtherTree.Tree.Count then exit;
  Node:=Tree.FindLowest;
  OtherNode:=OtherTree.Tree.FindLowest;
  while Node<>nil do begin
    if OtherNode=nil then exit;
    Item:=PStringMapItem(Node.Data);
    OtherItem:=PStringMapItem(OtherNode.Data);
    if not ItemsAreEqual(Item,OtherItem) then exit;
    OtherNode:=OtherNode.Successor;
    Node:=Node.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TCustomStringMap.Assign(Source: TCustomStringMap);
var
  SrcNode: TAvgLvlTreeNode;
  SrcItem: PStringMapItem;
begin
  if (Source=nil) or (Source.ClassType<>ClassType) then
    raise Exception.Create('invalid class');
  Clear;
  SrcNode:=Source.Tree.FindLowest;
  while SrcNode<>nil do begin
    SrcItem:=PStringMapItem(SrcNode.Data);
    Tree.Add(CreateCopy(SrcItem));
    SrcNode:=SrcNode.Successor;
  end;
end;

procedure TCustomStringMap.SetCompareFuncs(const NewCompareItemsFunc,
  NewCompareKeyItemFunc: TListSortCompare; NewCaseSensitive: boolean);
begin
  FCompareKeyItemFunc:=NewCompareKeyItemFunc;
  Tree.OnCompare:=NewCompareItemsFunc;
  FCaseSensitive:=NewCaseSensitive;
end;

end.
