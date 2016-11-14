{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 
  Abstract:
    A dynamic data queue to push and pop arbitrary data.
}
unit DynQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  TDynamicQueueItem = record
    Size: integer;
    Data: array[0..0] of integer;// type is irrelevant, the record is open ended
  end;
  PDynamicQueueItem = ^TDynamicQueueItem;
  ListOfPDynamicQueueItem = ^PDynamicQueueItem;

  { TDynamicDataQueue
    A queue for arbitrary data. That means first in first out.

    Push: put data in the queue
    Pop:  fetch data from the queue (data is removed from queue)
    Top:  read data in the queue (data remains in the queue)

    This queue maintains internally a ring queue for pointers to data chunks of
    TDynamicQueueItem. It is optimised to reduce the amount of data movement. }

  TDynamicDataQueue =  class
  private
    FItems: ListOfPDynamicQueueItem; // ring queue from FTopIndex to FLastIndex
    FItemCapacity: integer; // length of ListOfPDynamicQueueItem
    FTopIndex: integer; // first item in FItems
    FLastIndex: integer; // last item in FItems
    FMaximumBlockSize: integer;
    FMinimumBlockSize: integer;
    FSize: int64;
    FTopItemSpace: integer; // space in top item
    FLastItemSpace: integer; // remaining space in last item
    procedure SetMaximumBlockSize(const AValue: integer);
    procedure SetMinimumBlockSize(const AValue: integer);
    procedure GrowItems;
    procedure AddItem(ItemSize: integer);
    function CalculateItemSize(ItemSize: integer): integer;
    function PushInternal(Source: PByte; AStream: TStream; Count: integer): integer;// add to end of queue
    function PopTopInternal(Dest: PByte; AStream: TStream; Count: integer; KeepData: Boolean): integer;// read from start of queue, remove from queue
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(WriteData: Boolean);
    function Push(const Buffer; Count: integer): integer;// add to end of queue
    function Push(AStream: TStream; Count: integer): integer;// add to end of queue
    function Pop(var Buffer; Count: integer): integer; // read from start of queue, remove from queue
    function Pop(AStream: TStream; Count: integer): integer;// read from start of queue, remove from queue
    function Top(var Buffer; Count: integer): integer; // read from start of queue, keep data
    function Top(AStream: TStream; Count: integer): integer;// read from start of queue, keep data
    property Size: int64 read FSize;
    property MinimumBlockSize: integer read FMinimumBlockSize write SetMinimumBlockSize;
    property MaximumBlockSize: integer read FMaximumBlockSize write SetMaximumBlockSize;
  end;

implementation

{ TDynamicDataQueue }

procedure TDynamicDataQueue.SetMinimumBlockSize(const AValue: integer);
begin
  if (FMinimumBlockSize=AValue) then exit;
  FMinimumBlockSize:=AValue;
  if FMinimumBlockSize<16 then FMinimumBlockSize:=16;
  if FMaximumBlockSize<FMinimumBlockSize then
    FMaximumBlockSize:=FMinimumBlockSize;
end;

procedure TDynamicDataQueue.GrowItems;
var
  NewCapacity: LongInt;
  NewSize: Integer;
  NewItems: ListOfPDynamicQueueItem;
  DestIndex: Integer;
  SrcIndex: LongInt;
begin
  // allocate a new ring queue
  NewCapacity:=FItemCapacity;
  if NewCapacity<8 then
    NewCapacity:=8
  else
    NewCapacity:=NewCapacity*2;
  NewSize:=NewCapacity*SizeOf(Pointer);
  GetMem(NewItems,NewSize);
  FillChar(NewItems^,NewSize,0);

  // copy old items
  DestIndex:=0;
  if FItems<>nil then begin
    SrcIndex:=FTopIndex;
    repeat
      NewItems[DestIndex]:=FItems[SrcIndex];
      if SrcIndex=FLastIndex then break;
      inc(DestIndex);
      inc(SrcIndex);
      if SrcIndex=FItemCapacity then
        SrcIndex:=0;
    until false;
    FreeMem(FItems);
  end;
  FTopIndex:=0;
  FLastIndex:=DestIndex;
  FItems:=NewItems;
  FItemCapacity:=NewCapacity;
end;

procedure TDynamicDataQueue.AddItem(ItemSize: integer);
var
  NewIndex: Integer;

  procedure RaiseInconsistency;
  begin
    raise Exception.Create('TDynamicDataQueue.AddItem NewIndex='+IntToStr(NewIndex));
  end;

begin
  // check that there is space for the new item
  NewIndex:=FLastIndex;
  if (FItems<>nil) and (FItems[NewIndex]<>nil) then begin
    inc(NewIndex);
    if NewIndex>=FItemCapacity then
      NewIndex:=0;
  end;
  if NewIndex=FTopIndex then begin
    GrowItems;
    NewIndex:=FLastIndex;
    if FItems[NewIndex]<>nil then begin
      inc(NewIndex);
      if NewIndex>=FItemCapacity then
        NewIndex:=0;
    end;
  end;
  if (FItems=nil) then RaiseInconsistency;
  if (FItems[NewIndex]<>nil) then RaiseInconsistency;
  
  FLastIndex:=NewIndex;
  GetMem(FItems[FLastIndex],SizeOf(TDynamicQueueItem.Size)+ItemSize);
  FItems[FLastIndex]^.Size:=ItemSize;
end;

function TDynamicDataQueue.CalculateItemSize(ItemSize: integer): integer;
begin
  Result:=ItemSize;
  if Result<MinimumBlockSize then
    Result:=MinimumBlockSize;
  if Result>MaximumBlockSize then
    Result:=MaximumBlockSize;
end;

function TDynamicDataQueue.PushInternal(Source: PByte; AStream: TStream;
  Count: integer): integer;
var
  CurCount: PtrInt;
  NewItemSize: LongInt;
  LastItem: PDynamicQueueItem;
  Dest: Pointer;
begin
  Result:=0;
  if Count<=0 then exit;
  while true do begin
    while FLastItemSpace>0 do begin
      // fill the last item
      CurCount:=Count;
      if CurCount>FLastItemSpace then
        CurCount:=FLastItemSpace;
      LastItem:=FItems[FLastIndex];
      Dest:=Pointer(@(LastItem^.Data))+LastItem^.Size-FLastItemSpace;

      // beware: read from a stream can raise an exception
      if Source<>nil then
        System.Move(Source[Result],Dest^,CurCount)
      else
        CurCount:=AStream.Read(Dest^,CurCount);
      if CurCount<=0 then exit;

      // transfer succeeded
      dec(FLastItemSpace,CurCount); // space decreased
      inc(fSize,CurCount);   // Queue increased
      inc(Result,CurCount);  // bytes transferred
      dec(Count,CurCount);   // less to transfer
      if Count=0 then exit;
    end;
    // add new
    NewItemSize:=CalculateItemSize(Count);
    AddItem(NewItemSize);
    FLastItemSpace:=NewItemSize;
  end;
end;

function TDynamicDataQueue.PopTopInternal(Dest: PByte; AStream: TStream;
  Count: integer; KeepData: Boolean): integer;
  
  procedure RaiseInconsistencySizeNot0;
  begin
    raise Exception.Create('TDynamicDataQueue.PopTopInternal inconsistency size<>0');
  end;
  
  procedure RaiseInconsistencyEmptyItem;
  begin
    raise Exception.Create('TDynamicDataQueue.PopTopInternal inconsistency empty item');
  end;
  
  procedure RaiseInconsistencySizeNegative;
  begin
    raise Exception.Create('TDynamicDataQueue.PopTopInternal inconsistency size<0');
  end;
  
var
  Item: PDynamicQueueItem;
  CurCount: Integer;
  Source: PByte;
  CurItemSize: LongInt;
  ReadIndex: LongInt;
  TransferredCount: LongInt;
begin
  Result:=0;
  if Count<=0 then exit;
  ReadIndex:=FTopIndex;

  while Count>0 do begin
    if FItems=nil then exit; // no data
    
    Item:=FItems[ReadIndex];
    CurItemSize:=Item^.Size;
    if ReadIndex=FLastIndex then
      dec(CurItemSize,FLastItemSpace);
    CurCount:=CurItemSize;
    if ReadIndex=FTopIndex then
      dec(CurCount,FTopItemSpace);
    if CurCount<=0 then
      RaiseInconsistencyEmptyItem;
  
    // copy data from the TopItem
    if CurCount>Count then
      CurCount:=Count;
    Source:=PByte(@Item^.Data);
    if ReadIndex=FTopIndex then
      inc(Source,FTopItemSpace);

    // beware: writing to a stream can raise an exception
    if Dest<>nil then begin
      System.Move(Source^,Dest[Result],CurCount);
      TransferredCount:=CurCount;
    end else
      TransferredCount:=AStream.Write(Dest^,CurCount);
    if TransferredCount<=0 then
      exit;
      
    // transfer succeeded (at least partially)
    inc(Result,TransferredCount); // bytes transferred
    dec(Count,TransferredCount);  // less to transfer
    if (not KeepData) then begin
      dec(FSize,TransferredCount);  // Queue decreased
      if FSize<0 then RaiseInconsistencySizeNegative;
      
      if (ReadIndex=FTopIndex) then begin
        inc(FTopItemSpace,TransferredCount); // space in top item increased

        if (FTopItemSpace=CurItemSize) then begin
          // item complete -> remove item
          FreeMem(Item);
          FItems[FTopIndex]:=nil;
          if FTopIndex=FLastIndex then begin
            // complete queue read
            if Size<>0 then RaiseInconsistencySizeNot0;
            Clear;
            exit;
          end;

          FTopItemSpace:=0;
          inc(FTopIndex);
          if FTopIndex=FItemCapacity then FTopIndex:=0;
        end;
      end;
    end;
    if (Count=0) or (TransferredCount<CurCount) then exit;

    if TransferredCount=CurCount then begin
      // next item
      inc(ReadIndex);
      if ReadIndex=FItemCapacity then ReadIndex:=0;
    end;
  end;
end;

procedure TDynamicDataQueue.SetMaximumBlockSize(const AValue: integer);
begin
  if FMaximumBlockSize=AValue then exit;
  FMaximumBlockSize:=AValue;
  if FMaximumBlockSize<FMinimumBlockSize then
    FMaximumBlockSize:=FMinimumBlockSize;
end;

constructor TDynamicDataQueue.Create;
begin
  FMinimumBlockSize:=512;
  FMaximumBlockSize:=4096;
end;

destructor TDynamicDataQueue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDynamicDataQueue.Push(const Buffer; Count: integer): integer;
begin
  Result:=PushInternal(PByte(@Buffer),nil,Count);
end;

function TDynamicDataQueue.Push(AStream: TStream; Count: integer): integer;
begin
  Result:=PushInternal(nil,AStream,Count);
end;

function TDynamicDataQueue.Pop(var Buffer; Count: integer): integer;
begin
  Result:=PopTopInternal(PByte(@Buffer),nil,Count,false);
end;

function TDynamicDataQueue.Pop(AStream: TStream; Count: integer): integer;
begin
  Result:=PopTopInternal(nil,AStream,Count,false);
end;

function TDynamicDataQueue.Top(var Buffer; Count: integer): integer;
begin
  Result:=PopTopInternal(PByte(@Buffer),nil,Count,true);
end;

function TDynamicDataQueue.Top(AStream: TStream; Count: integer): integer;
begin
  Result:=PopTopInternal(nil,AStream,Count,true);
end;

procedure TDynamicDataQueue.Clear;
begin
  while FTopIndex<>FLastIndex do begin
    FreeMem(FItems[FTopIndex]);
    inc(FTopIndex);
    if FTopIndex=FItemCapacity then
      FTopIndex:=0;
  end;
  FTopIndex:=0;
  FLastIndex:=0;
  FSize:=0;
  FreeMem(FItems);
  FItems:=nil;
  FItemCapacity:=0;
  FTopItemSpace:=0;
  FLastItemSpace:=0;
end;

procedure TDynamicDataQueue.ConsistencyCheck;

  procedure Error(const Msg: string);
  begin
    raise Exception.Create('TDynamicDataQueue.ConsistencyCheck '+Msg);
  end;

var
  i: LongInt;
  RealSize: int64;
  CurSize: LongInt;
begin
  if Size<0 then Error('');
  if FMinimumBlockSize>FMaximumBlockSize then Error('');
  if FMinimumBlockSize<16 then Error('');
  if (FItems=nil) then begin
    if Size<>0 then Error('');
  end else begin
    if FItemCapacity<=0 then Error('');
    if Size=0 then Error('');
    if FTopIndex<0 then Error('');
    if FLastIndex<0 then Error('');
    if FTopIndex>=FItemCapacity then Error('');
    if FLastIndex>=FItemCapacity then Error('');
    
    // check used items
    RealSize:=0;
    i:=FTopIndex;
    repeat
      if FItems[i]=nil then Error('');
      if FItems[i]^.Size<=0 then Error('');
      CurSize:=FItems[i]^.Size;
      if FTopIndex=i then
        dec(CurSize,FTopItemSpace);
      if FLastIndex=i then
        dec(CurSize,FLastItemSpace);
      inc(RealSize,CurSize);
      if i=FLastIndex then break;
      inc(i);
      if i=FItemCapacity then i:=0;
    until false;
    if RealSize<>Size then Error('');
    
    // check unused items
    inc(i);
    if i=FItemCapacity then i:=0;
    while (i<>FTopIndex) do begin
      if FItems[i]<>nil then Error('');
      inc(i);
      if i=FItemCapacity then i:=0;
    end;

    // check space
    if FLastItemSpace<0 then Error('');
    if FItems[FLastIndex]^.Size<=FLastItemSpace then Error('');
    if FTopItemSpace<0 then Error('');
    if FItems[FTopIndex]^.Size<=FTopItemSpace then Error('');
    if (FTopIndex=FLastIndex)
    and (FTopItemSpace>=FItems[FTopIndex]^.Size-FLastItemSpace) then Error('');
  end;
end;

procedure TDynamicDataQueue.WriteDebugReport(WriteData: Boolean);
var
  i: LongInt;
  DataCount: LongInt;
  DataOffset: Integer;
begin

  if FItems<>nil then begin
    i:=FTopIndex;
    repeat
      DataCount:=FItems[i]^.Size;
      DataOffset:=0;
      if FTopIndex=i then begin
        dec(DataCount,FTopItemSpace);
        inc(DataOffset,FTopItemSpace);
      end;
      if i=FLastIndex then
        dec(DataCount,FLastItemSpace);

      if WriteData then begin

      end;
      
      if i=FLastIndex then break;
      inc(i);
      if i=FItemCapacity then i:=0;
    until false;
  end;
end;

end.
