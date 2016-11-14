{
  Author: Mattias Gaertner

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    This unit maintains and stores all lazarus resources in the global list
    named LazarusResources and provides methods and types to stream components.

    A lazarus resource is an ansistring, with a name and a valuetype. Both, name
    and valuetype, are ansistrings as well.
    Lazarus resources are normally included via an include directive in the
    initialization part of a unit. To create such include files use the
    BinaryToLazarusResourceCode procedure.
    To create a LRS file from an LFM file use the LFMtoLRSfile function which
    transforms the LFM text to binary format and stores it as Lazarus resource
    include file.
}
unit LResources;

{$mode objfpc}{$H+}

{ $DEFINE WideStringLenDoubled}

interface

uses
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Types, RtlConsts, FPCAdds, TypInfo, FileUtil, DynQueue,
  LCLProc, LCLStrConsts, LazConfigStorage, LazUTF8, LazUTF8Classes;

{$DEFINE UseLRS}
{$DEFINE UseRES}

const
  LRSComment =  // do not translate this!
    'This is an automatically generated Typhon resource file';  //=== ct9999 ====================
type
  TFilerSignature = array[1..4] of Char;


  { TLResourceList }

  TLResource = class
  public
    Name: AnsiString;
    ValueType: AnsiString;
    Value: AnsiString;
  end;

  TLResourceList = class(TObject)
  private
    FList: TList;  // main list with all resource pointers
    FMergeList: TList; // list needed for mergesort
    FSortedCount: integer; // 0 .. FSortedCount-1 resources are sorted
    function FindPosition(const Name: AnsiString):integer;
    function GetItems(Index: integer): TLResource;
    procedure Sort;
    procedure MergeSort(List, MergeList: TList; Pos1, Pos2: integer);
    procedure Merge(List, MergeList: TList; Pos1, Pos2, Pos3: integer);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Add(const Name, ValueType, Value: AnsiString);
    procedure Add(const Name, ValueType: AnsiString; const Values: array of string);
    function Find(const Name: AnsiString): TLResource; overload;
    function Find(const Name, ValueType: AnsiString): TLResource; overload;
    function Count: integer;
    property Items[Index: integer]: TLResource read GetItems;
  end;

  { TLazarusResourceStream }

  TLazarusResourceStream = class(TCustomMemoryStream)
  private
    FLRes: TLResource;
  {$ifdef UseRES}
    FPRes: TFPResourceHGLOBAL;
  {$endif}
    procedure Initialize(Name, ResType: PChar);
  public
    constructor Create(const ResName: string; ResType: PChar);
    constructor CreateFromID(ResID: Integer; ResType: PChar);
    constructor CreateFromHandle(AHandle: TLResource); overload;
  {$ifdef UseRES}
    // here from FP resource handle
    constructor CreateFromHandle(Instance: TFPResourceHMODULE; AHandle: TFPResourceHandle); overload;
  {$endif}
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Res: TLResource read FLRes;
  end;

  { TAbstractTranslator}
  TAbstractTranslator = class(TObject)//Should it be somewhat more than TObject?
  public
    procedure TranslateStringProperty(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);virtual;abstract;
   //seems like we need nothing more here
  end;


var LRSTranslator: TAbstractTranslator;

type
  TLRSItemType = (
    lrsitCollection,
    lrsitComponent,
    lrsitList,
    lrsitProperty
  );

  TLRSORStackItem = record
    Name: string;
    ItemType: TLRSItemType;
    Root: TComponent;
    PushCount: integer; // waiting for this number of Pop
    ItemNr: integer; // nr in a collection or list
  end;
  PLRSORStackItem = ^TLRSORStackItem;

  { TLRSObjectReader }

  TLRSObjectReader = class(TAbstractObjectReader)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    FStack: PLRSORStackItem;
    FStackPointer: integer;
    FStackCapacity: integer;
    FReader: TReader;
    procedure SkipProperty;
    procedure SkipSetBody;
    procedure Push(ItemType: TLRSItemType; const AName: string = '';
                   Root: TComponent = nil; PushCount: integer = 1);
    procedure Pop;
    procedure ClearStack;
    function InternalReadValue: TValueType;
    procedure EndPropertyIfOpen;
  protected
    function ReadIntegerContent: integer;
  public
    constructor Create(AStream: TStream; BufSize: Integer); virtual;
    destructor Destroy; override;

    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
    function BeginProperty: String; override;
    function GetStackPath: string;

    procedure Read(var Buf; Count: LongInt); override;
    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadFloat: Extended; override;
    function ReadSingle: Single; override;
    function ReadCurrency: Currency; override;
    function ReadDate: TDateTime; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    {$IF FPC_FULLVERSION >= 30000}
    procedure ReadSignature; override;
    {$ENDIF}
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString; override;
    function ReadUnicodeString: UnicodeString; override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  public
    property Stream: TStream read FStream;
    property Reader: TReader read FReader write FReader;
  end;
  TLRSObjectReaderClass = class of TLRSObjectReader;

  { TLRSOWStackItem
    The TLRSObjectWriter can find empty entries and omit writing them to stream.
    For example:
        inline ConditionalOptionsFrame: TCompOptsConditionalsFrame
          inherited COCTreeView: TTreeView
          end
          inherited COCPopupMenu: TPopupMenu
          end
        end

    The empty inherited child components will not be written if
      WriteEmptyInheritedChilds = false (default).

    Reason:
      This allows one to delete/rename controls in ancestors without the need
      to update all descendants.
  }

  TLRSOWStackItemState = (
    lrsowsisStarted,       // now writing header
    lrsowsisHeaderWritten, // header saved on stack, not yet written to stream, waiting for data
    lrsowsisDataWritten    // header written to stream, data written
    );

  TLRSOWStackItem = record
    Name: string;
    ItemType: TLRSItemType;
    Root: TComponent;
    PushCount: integer; // waiting for this number of Pop
    ItemNr: integer; // nr in a collection or list
    SkipIfEmpty: boolean;
    State: TLRSOWStackItemState;
    Buffer: Pointer;
    BufCount: PtrInt;
    BufCapacity: PtrInt;
  end;
  PLRSOWStackItem = ^TLRSOWStackItem;

  { TLRSObjectWriter }

  TLRSObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FSignatureWritten: Boolean;
    FStack: PLRSOWStackItem;
    FStackPointer: integer;
    FStackCapacity: integer;
    FWriteEmptyInheritedChilds: boolean;
    FWriter: TWriter;
    procedure Push(ItemType: TLRSItemType; const AName: string = '';
                   Root: TComponent = nil; PushCount: integer = 1;
                   SkipIfEmpty: boolean = false);
    procedure EndHeader;
    procedure Pop(WriteNull: boolean);
    procedure ClearStack;
    procedure FlushStackToStream;
    procedure WriteToStream(const Buffer; Count: Longint);
  protected
    procedure FlushBuffer;
    procedure WriteValue(Value: TValueType);
    procedure WriteStr(const Value: String);
    procedure WriteIntegerContent(i: integer);
    procedure WriteWordContent(w: word);
    procedure WriteInt64Content(i: int64);
    procedure WriteSingleContent(s: single);
    procedure WriteDoubleContent(d: Double);
    procedure WriteExtendedContent(e: Extended);
    procedure WriteCurrencyContent(c: Currency);
    procedure WriteWideStringContent(const ws: WideString);
    procedure WriteWordsReversed(p: PWord; Count: integer);
    procedure WriteNulls(Count: integer);
  public
    constructor Create(Stream: TStream; BufSize: Integer); virtual;
    destructor Destroy; override;

    { Begin/End markers. Those ones who don't have an end indicator, use
      "EndList", after the occurrence named in the comment. Note that this
      only counts for "EndList" calls on the same level; each BeginXXX call
      increases the current level. }
    procedure BeginCollection; override;{ Ends with the next "EndList" }
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override; { Ends after the second "EndList" }
    {$IF FPC_FULLVERSION >= 30000}
    procedure WriteSignature; override;
    {$ENDIF}
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;
    function GetStackPath: string;

    procedure Write(const Buffer; Count: Longint); override;
    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;

    property WriteEmptyInheritedChilds: boolean read FWriteEmptyInheritedChilds write FWriteEmptyInheritedChilds;
    property Writer: TWriter read FWriter write FWriter;
  end;
  TLRSObjectWriterClass = class of TLRSObjectWriter;

  TLRPositionLink = record
    LFMPosition: int64;
    LRSPosition: int64;
    Data: Pointer;
  end;
  PLRPositionLink = ^TLRPositionLink;
  
  { TLRPositionLinks }

  TLRPositionLinks = class
  private
    FItems: TFPList;
    FCount: integer;
    function GetData(Index: integer): Pointer;
    function GetLFM(Index: integer): Int64;
    function GetLRS(Index: integer): Int64;
    procedure SetCount(const AValue: integer);
    procedure SetData(Index: integer; const AValue: Pointer);
    procedure SetLFM(Index: integer; const AValue: Int64);
    procedure SetLRS(Index: integer; const AValue: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Sort(LFMPositions: Boolean);
    function IndexOf(const Position: int64; LFMPositions: Boolean): integer;
    function IndexOfRange(const FromPos, ToPos: int64;
                          LFMPositions: Boolean): integer;
    procedure SetPosition(const FromPos, ToPos, MappedPos: int64;
                          LFMtoLRSPositions: Boolean);
    procedure Add(const LFMPos, LRSPos: Int64; AData: Pointer);
  public
    property LFM[Index: integer]: int64 read GetLFM write SetLFM;
    property LRS[Index: integer]: int64 read GetLRS write SetLRS;
    property Data[Index: integer]: Pointer read GetData write SetData;
    property Count: integer read FCount write SetCount;
  end;
  
  { TUTF8Parser }

  TUTF8Parser = class(TObject)
  private
    fStream : TStream;
    fBuf : pchar;
    fBufLen : integer; // read
    fPos : integer;
    fLineStart : integer; // column = fPos - fLineStart + 1
    fFloatType : char;
    fSourceLine : integer;
    fToken : char;
    fEofReached : boolean;
    fLastTokenStr : string;
    function GetTokenName(aTok : char) : string;
    procedure LoadBuffer;
    procedure CheckLoadBuffer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure ProcessChar; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsNumber : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsHexNum : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsAlpha : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsAlphaNum : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function GetHexValue(c : char) : byte; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function GetAlphaNum : string;
    procedure HandleNewLine;
    procedure SkipSpaces;
    procedure SkipWhitespace;
    procedure HandleEof;
    procedure HandleAlphaNum;
    procedure HandleNumber;
    procedure HandleHexNumber;
    function HandleQuotedString : string;
    function HandleDecimalString(var IsWideString: Boolean): string;
    procedure HandleString;
    procedure HandleMinus;
    procedure HandleUnknown;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure CheckToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    procedure HexToBinary(Stream: TStream);
    function NextToken: Char;
    function SourcePos: Longint;
    function TokenComponentIdent: string;
    function TokenFloat: Extended;
    function TokenInt: Int64;
    function TokenString: string;
    function TokenSymbolIs(const S: string): Boolean;
    property FloatType: Char read fFloatType;
    property SourceLine: Integer read fSourceLine;
    function SourceColumn: integer;
    property Token: Char read fToken;
  end;

  { TCustomLazComponentQueue
    A queue to stream components, used for multithreading or network.
    The function ConvertComponentAsString converts a component to binary format
    with a leading size information (using WriteLRSInt64MB).
    When streaming components over network, they will arrive in chunks.
    TCustomLazComponentQueue tells you, if a whole component has arrived and if
    it has completely arrived. }
  TCustomLazComponentQueue = class(TComponent)
  private
    FOnFindComponentClass: TFindComponentClassEvent;
  protected
    FQueue: TDynamicDataQueue;
    function ReadComponentSize(out ComponentSize, SizeLength: int64): Boolean; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Write(const Buffer; Count: Longint): Longint;
    function CopyFrom(AStream: TStream; Count: Longint): Longint;
    function HasComponent: Boolean; virtual;
    function ReadComponent(var AComponent: TComponent;
                           NewOwner: TComponent = nil): Boolean; virtual;
    function ConvertComponentAsString(AComponent: TComponent): string;
    property OnFindComponentClass: TFindComponentClassEvent
                         read FOnFindComponentClass write FOnFindComponentClass;
  end;
  
  { TLazComponentQueue }

  TLazComponentQueue = class(TCustomLazComponentQueue)
  published
    property Name;
    property OnFindComponentClass;
  end;

  TPropertyToSkip = record
    PersistentClass: TPersistentClass;
    PropertyName: String;
    Note: String;
    HelpKeyword: String;
  end;
  PRemovedProperty = ^TPropertyToSkip;

  { TPropertyToSkipList }

  TPropertiesToSkip = class(TList)
  private
    function GetItem(AIndex: Integer): PRemovedProperty;
    procedure SetItem(AIndex: Integer; const AValue: PRemovedProperty);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure DoPropertyNotFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
  public
    function IndexOf(AInstance: TPersistent; const APropertyName: String): Integer; overload;
    function IndexOf(AClass: TPersistentClass; APropertyName: String): Integer; overload;
    function Add(APersistentClass: TPersistentClass; const APropertyName, ANote,
      AHelpKeyWord: string): Integer; reintroduce;
    property Items[AIndex: Integer]: PRemovedProperty read GetItem write SetItem;
  end;

const
  ObjStreamMaskInherited = 1;
  ObjStreamMaskChildPos  = 2;
  ObjStreamMaskInline    = 4;

var
  LazarusResources: TLResourceList;
  PropertiesToSkip: TPropertiesToSkip = nil;

  LRSObjectReaderClass: TLRSObjectReaderClass=TLRSObjectReader;
  LRSObjectWriterClass: TLRSObjectWriterClass=TLRSObjectWriter;

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass):Boolean;
function InitLazResourceComponent(Instance: TComponent;
                                  RootAncestor: TClass): Boolean;
function CreateLRSReader(s: TStream; var DestroyDriver: boolean): TReader;
function CreateLRSWriter(s: TStream; var DestroyDriver: boolean): TWriter;

function GetClassNameFromLRSStream(s: TStream; out IsInherited: Boolean): shortstring;
procedure GetComponentInfoFromLRSStream(s: TStream;
                                  out ComponentName, ComponentClassName: string;
                                  out IsInherited: Boolean);
procedure WriteComponentAsBinaryToStream(AStream: TStream;
                                         AComponent: TComponent);
procedure ReadComponentFromBinaryStream(AStream: TStream;
                           var RootComponent: TComponent;
                           OnFindComponentClass: TFindComponentClassEvent;
                           TheOwner: TComponent = nil;
                           Parent: TComponent = nil;
                           ReaderRoot: TComponent = nil);
procedure WriteComponentAsTextToStream(AStream: TStream;
                                       AComponent: TComponent);
procedure ReadComponentFromTextStream(AStream: TStream;
                           var RootComponent: TComponent;
                           OnFindComponentClass: TFindComponentClassEvent;
                           TheOwner: TComponent = nil;
                           Parent: TComponent = nil);
procedure SaveComponentToConfig(Config: TConfigStorage; const Path: string;
                                AComponent: TComponent);
procedure LoadComponentFromConfig(Config: TConfigStorage; const Path: string;
                                 var RootComponent: TComponent;
                                 OnFindComponentClass: TFindComponentClassEvent;
                                 TheOwner: TComponent = nil;
                                 Parent: TComponent = nil);


function CompareComponents(Component1, Component2: TComponent): boolean;
function CompareMemStreams(Stream1, Stream2: TCustomMemoryStream): boolean;

procedure BinaryToLazarusResourceCode(BinStream, ResStream: TStream;
  const ResourceName, ResourceType: String);
function LFMtoLRSfile(const LFMfilename: string): boolean;// true on success
function LFMtoLRSstream(LFMStream, LRSStream: TStream): boolean;// true on success
function FindLFMClassName(LFMStream: TStream):AnsiString;
procedure ReadLFMHeader(LFMStream: TStream;
                        out LFMType, LFMComponentName, LFMClassName: String);
procedure ReadLFMHeader(const LFMSource: string;
                        out LFMClassName: String; out LFMType: String);
procedure ReadLFMHeader(const LFMSource: string;
                        out LFMType, LFMComponentName, LFMClassName: String);
function ReadLFMHeaderFromFile(const Filename: string;
                  out LFMType, LFMComponentName, LFMClassName: String): boolean;
function CreateLFMFile(AComponent: TComponent; LFMStream: TStream): integer;

type
  TLRSStreamOriginalFormat = (sofUnknown, sofBinary, sofText);
  
procedure LRSObjectBinaryToText(Input, Output: TStream); // binary to lfm
procedure LRSObjectTextToBinary(Input, Output: TStream;  // lfm to binary
                                Links: TLRPositionLinks = nil);
procedure LRSObjectToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);

procedure LRSObjectResourceToText(Input, Output: TStream); // lrs to lfm
procedure LRSObjectResToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);
  
function TestFormStreamFormat(Stream: TStream): TLRSStreamOriginalFormat;
procedure FormDataToText(FormStream, TextStream: TStream;
  aFormat: TLRSStreamOriginalFormat = sofUnknown);

function FindResourceLFM(ResName: string): HRSRC;

procedure DefineRectProperty(Filer: TFiler; const Name: string;
                             ARect, DefaultRect: PRect);

procedure ReverseBytes(p: Pointer; Count: integer);
procedure ReverseByteOrderInWords(p: PWord; Count: integer);
function ConvertLRSExtendedToDouble(p: Pointer): Double;
procedure ConvertEndianBigDoubleToLRSExtended(BigEndianDouble,
                                              LRSExtended: Pointer);
                                              
procedure ConvertLEDoubleToLRSExtended(LEDouble, LRSExtended: Pointer);


function ReadLRSShortInt(s: TStream): shortint;
function ReadLRSByte(s: TStream): byte;
function ReadLRSSmallInt(s: TStream): smallint;
function ReadLRSWord(s: TStream): word;
function ReadLRSInteger(s: TStream): integer;
function ReadLRSCardinal(s: TStream): cardinal;
function ReadLRSInt64(s: TStream): int64;
function ReadLRSSingle(s: TStream): Single;
function ReadLRSDouble(s: TStream): Double;
function ReadLRSExtended(s: TStream): Extended;
function ReadLRSCurrency(s: TStream): Currency;
function ReadLRSWideString(s: TStream): WideString;
function ReadLRSEndianLittleExtendedAsDouble(s: TStream): Double;
function ReadLRSValueType(s: TStream): TValueType;
function ReadLRSInt64MB(s: TStream): int64;// multibyte

procedure WriteLRSSmallInt(s: TStream; const i: smallint);
procedure WriteLRSWord(s: TStream; const w: word);
procedure WriteLRSInteger(s: TStream; const i: integer);
procedure WriteLRSCardinal(s: TStream; const c: cardinal);
procedure WriteLRSSingle(s: TStream; const si: Single);
procedure WriteLRSDouble(s: TStream; const d: Double);
procedure WriteLRSExtended(s: TStream; const e: extended);
procedure WriteLRSInt64(s: TStream; const i: int64);
procedure WriteLRSCurrency(s: TStream; const c: Currency);
procedure WriteLRSWideStringContent(s: TStream; const w: WideString);
procedure WriteLRSInt64MB(s: TStream; const Value: int64);// multibyte

procedure WriteLRSReversedWord(s: TStream; w: word);
procedure WriteLRS4BytesReversed(s: TStream; p: Pointer);
procedure WriteLRS8BytesReversed(s: TStream; p: Pointer);
procedure WriteLRS10BytesReversed(s: TStream; p: Pointer);
procedure WriteLRSNull(s: TStream; Count: integer);
procedure WriteLRSEndianBigDoubleAsEndianLittleExtended(s: TStream;
  EndBigDouble: PByte);
procedure WriteLRSDoubleAsExtended(s: TStream; ADouble: PByte);
procedure WriteLRSReversedWords(s: TStream; p: Pointer; Count: integer);

function FloatToLFMStr(const Value: extended; Precision, Digits: Integer
                       ): string;

function CompareLRPositionLinkWithLFMPosition(Item1, Item2: Pointer): integer;
function CompareLRPositionLinkWithLRSPosition(Item1, Item2: Pointer): integer;

procedure RegisterPropertyToSkip(PersistentClass: TPersistentClass;
  const PropertyName, Note, HelpKeyWord: string);

procedure Register;

implementation

const
  LineEnd: ShortString = LineEnding;

var
  ByteToStr: array[char] of shortstring;
  ByteToStrValid: boolean=false;
  
type

  { TDefineRectPropertyClass }

  TDefineRectPropertyClass = class
  public
    Value: PRect;
    DefaultValue: PRect;
    constructor Create(AValue, ADefaultRect: PRect);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function HasData: Boolean;
  end;
  
  { TReaderUniqueNamer - dummy class, used by the reader functions to rename
    components, that are read from a stream, on the fly. }

  TReaderUniqueNamer = class
    procedure OnSetName(Reader: TReader; Component: TComponent;
                        var Name: string);
  end;

{ TPropertiesToSkip }

function TPropertiesToSkip.GetItem(AIndex: Integer): PRemovedProperty;
begin
  Result := inherited Get(AIndex);
end;

procedure TPropertiesToSkip.SetItem(AIndex: Integer;
  const AValue: PRemovedProperty);
begin
  inherited Put(AIndex, AValue);
end;

procedure TPropertiesToSkip.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PRemovedProperty(Ptr))
  else
    inherited Notify(Ptr, Action);
end;

procedure TPropertiesToSkip.DoPropertyNotFound(Reader: TReader; Instance: TPersistent;
  var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Skip := IndexOf(Instance, PropName) >= 0;
  Handled := Skip;
end;

function TPropertiesToSkip.IndexOf(AInstance: TPersistent;
  const APropertyName: String): Integer;
begin
  if AInstance <> nil then
    Result := IndexOf(TPersistentClass(AInstance.ClassType), APropertyName)
  else
    Result := -1;
end;

function TPropertiesToSkip.IndexOf(AClass: TPersistentClass;
  APropertyName: String): Integer;
var
  PropertyInfo: PRemovedProperty;
begin
  APropertyName := LowerCase(APropertyName);
  Result := Count - 1;
  while Result >= 0 do
  begin
    PropertyInfo := Items[Result];
    if AClass.InheritsFrom(PropertyInfo^.PersistentClass) and
       (APropertyName = PropertyInfo^.PropertyName) then
    begin
      Exit;
    end;
    Dec(Result);
  end;
  Result := -1;
end;

function TPropertiesToSkip.Add(APersistentClass: TPersistentClass;
  const APropertyName, ANote, AHelpKeyWord: string): Integer;
var
  Item: PRemovedProperty;
begin
  Result := IndexOf(APersistentClass, APropertyName);
  if Result = -1 then
  begin
    New(Item);
    Item^.PersistentClass := APersistentClass;
    Item^.PropertyName := LowerCase(APropertyName);
    Item^.Note := ANote;
    Item^.HelpKeyword := AHelpKeyWord;
    Result := inherited Add(Item);
  end;
end;

{ TReaderUniqueNamer }

procedure TReaderUniqueNamer.OnSetName(Reader: TReader; Component: TComponent;
  var Name: string);
  
  procedure MakeValidIdentifier;
  var
    i: Integer;
  begin
    for i:=length(Name) downto 1 do
      if not (Name[i] in ['0'..'9','_','a'..'z','A'..'Z']) then
        System.Delete(Name,i,1);
    if (Name<>'') and (Name[1] in ['0'..'9']) then
      Name:='_'+Name;
  end;

  function NameIsUnique: Boolean;
  var
    Owner: TComponent;
    i: Integer;
    CurComponent: TComponent;
  begin
    Result:=true;
    if Name='' then exit;
    Owner:=Component.Owner;
    if Owner=nil then exit;
    for i:=0 to Owner.ComponentCount-1 do begin
      CurComponent:=Owner.Components[i];
      if CurComponent=Component then continue;
      if CompareText(CurComponent.Name,Name)=0 then exit(false);
    end;
  end;
  
begin
  MakeValidIdentifier;
  while not NameIsUnique do
    Name:=CreateNextIdentifier(Name);
end;
  
{ TDefineRectPropertyClass }

constructor TDefineRectPropertyClass.Create(AValue, ADefaultRect: PRect);
begin
  Value:=AValue;
  DefaultValue:=ADefaultRect;
end;

procedure TDefineRectPropertyClass.ReadData(Reader: TReader);
begin
  with Reader do begin
    ReadListBegin;
    Value^.Left:=ReadInteger;
    Value^.Top:=ReadInteger;
    Value^.Right:=ReadInteger;
    Value^.Bottom:=ReadInteger;
    ReadListEnd;
  end;
end;

procedure TDefineRectPropertyClass.WriteData(Writer: TWriter);
begin
  with Writer do begin
    WriteListBegin;
    WriteInteger(Value^.Left);
    WriteInteger(Value^.Top);
    WriteInteger(Value^.Right);
    WriteInteger(Value^.Bottom);
    WriteListEnd;
  end;
end;

function TDefineRectPropertyClass.HasData: Boolean;
begin
  if DefaultValue<>nil then begin
    Result:=(DefaultValue^.Left<>Value^.Left)
         or (DefaultValue^.Top<>Value^.Top)
         or (DefaultValue^.Right<>Value^.Right)
         or (DefaultValue^.Bottom<>Value^.Bottom);
  end else begin
    Result:=(Value^.Left<>0)
         or (Value^.Top<>0)
         or (Value^.Right<>0)
         or (Value^.Bottom<>0);
  end;
end;

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass):Boolean;
begin
  Result := InitLazResourceComponent(Instance, RootAncestor);
end;

function FindResourceLFM(ResName: string): HRSRC;
{$if defined(WinCE)}
//function FindResourceLFM(ResName: string): HRSRC;
//{$if (FPC_FULLVERSION>=20605) and defined(WinCE)}
var
  u: UnicodeString;
begin
  u:=ResName;
  Result := FindResource(HInstance,PWideChar(u),Windows.RT_RCDATA);
end;
{$else}
begin
  Result := FindResource(HInstance,PChar(ResName),
    {$if (FPC_FULLVERSION>=20701) and defined(Windows)}Windows.{$endif}RT_RCDATA);
end;
{$endif}

procedure DefineRectProperty(Filer: TFiler; const Name: string; ARect,
  DefaultRect: PRect);
var
  PropDef: TDefineRectPropertyClass;
begin
  PropDef := TDefineRectPropertyClass.Create(ARect, DefaultRect);
  try
    Filer.DefineProperty(Name,@PropDef.ReadData,@PropDef.WriteData,PropDef.HasData);
  finally
    PropDef.Free;
  end;
end;

procedure InitByteToStr;
var
  c: Char;
begin
  if ByteToStrValid then exit;
  for c:=Low(char) to High(char) do
    ByteToStr[c]:=IntToStr(ord(c));
  ByteToStrValid:=true;
end;

function GetClassNameFromLRSStream(s: TStream; out IsInherited: Boolean
  ): shortstring;
var
  Signature: TFilerSignature;
  NameLen: byte;
  OldPosition: Int64;
begin
  Result:='';
  OldPosition:=s.Position;
  // read signature
  Signature:='1234';
  s.Read(Signature[1],length(Signature));
  if Signature<>FilerSignature then exit;
  // read classname length
  NameLen:=0;
  s.Read(NameLen,1);
  if (NameLen and $f0) = $f0 then begin
    // this was the Flag Byte
    IsInherited := (NameLen and ObjStreamMaskInherited) <> 0;
    // read namelen
    s.Read(NameLen,1);
  end else
    IsInherited := False;
  // read classname
  if NameLen>0 then begin
    SetLength(Result,NameLen);
    s.Read(Result[1],NameLen);
  end;
  s.Position:=OldPosition;
end;

procedure GetComponentInfoFromLRSStream(s: TStream; out ComponentName,
  ComponentClassName: string; out IsInherited: Boolean);
var
  Signature: TFilerSignature;
  NameLen: byte;
  OldPosition: Int64;
  Flag: Byte;
begin
  ComponentName:='';
  ComponentClassName:='';
  OldPosition:=s.Position;
  // read signature
  Signature:='1234';
  s.Read(Signature[1],length(Signature));
  if Signature<>FilerSignature then exit;
  // read classname length
  NameLen:=0;
  s.Read(NameLen,1);
  if (NameLen and $f0) = $f0 then begin
    // Read Flag Byte
    Flag:=NameLen;
    IsInherited := (Flag and ObjStreamMaskInherited) <> 0;
    s.Read(NameLen,1);
  end else
    IsInherited := False;
  // read classname
  if NameLen>0 then begin
    SetLength(ComponentClassName,NameLen);
    s.Read(ComponentClassName[1],NameLen);
  end;
  // read component name length
  NameLen:=0;
  s.Read(NameLen,1);
  // read componentname
  if NameLen>0 then begin
    SetLength(ComponentName,NameLen);
    s.Read(ComponentName[1],NameLen);
  end;
  s.Position:=OldPosition;
end;

procedure WriteComponentAsBinaryToStream(AStream: TStream;
  AComponent: TComponent);
var
  Writer: TWriter;
  DestroyDriver: Boolean;
begin
  DestroyDriver:=false;
  Writer:=nil;
  try
    Writer:=CreateLRSWriter(AStream,DestroyDriver);
    Writer.WriteDescendent(AComponent,nil);
  finally
    if DestroyDriver then
      Writer.Driver.Free;
    Writer.Free;
  end;
end;

procedure ReadComponentFromBinaryStream(AStream: TStream;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent;
  Parent: TComponent; ReaderRoot: TComponent);
var
  DestroyDriver: Boolean;
  Reader: TReader;
  IsInherited: Boolean;
  AClassName: String;
  AClass: TComponentClass;
  UniqueNamer: TReaderUniqueNamer;
begin
  // get root class
  AClassName:=GetClassNameFromLRSStream(AStream,IsInherited);
  if IsInherited then begin
    // inherited is not supported by this simple function
  end;
  AClass:=nil;
  OnFindComponentClass(nil,AClassName,AClass);
  if AClass=nil then
    raise EClassNotFound.CreateFmt('Class "%s" not found', [AClassName]);

  if RootComponent=nil then begin
    // create root component
    // first create the new instance and set the variable ...
    RootComponent:=AClass.NewInstance as TComponent;
    // then call the constructor
    RootComponent.Create(TheOwner);
  end else begin
    // there is a root component, check if class is compatible
    if not RootComponent.InheritsFrom(AClass) then begin
      raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                                      [AClassName,RootComponent.ClassName]);
    end;
  end;

  // read the root component
  DestroyDriver:=false;
  Reader:=nil;
  UniqueNamer:=nil;
  try
    UniqueNamer:=TReaderUniqueNamer.Create;
    Reader:=CreateLRSReader(AStream,DestroyDriver);
    if ReaderRoot = nil then
      Reader.Root:=RootComponent
    else
      Reader.Root:=ReaderRoot;
    Reader.Owner:=TheOwner;
    Reader.Parent:=Parent;
    Reader.OnFindComponentClass:=OnFindComponentClass;
    Reader.OnSetName:=@UniqueNamer.OnSetName;
    Reader.BeginReferences;
    try
      Reader.Driver.BeginRootComponent;
      RootComponent:=Reader.ReadComponent(RootComponent);
      Reader.FixupReferences;
    finally
      Reader.EndReferences;
    end;
  finally
    if DestroyDriver then
      Reader.Driver.Free;
    UniqueNamer.Free;
    Reader.Free;
  end;
end;

procedure WriteComponentAsTextToStream(AStream: TStream; AComponent: TComponent);
var
  BinStream: TMemoryStream;
begin
  BinStream:=nil;
  try
    BinStream:=TMemoryStream.Create;
    WriteComponentAsBinaryToStream(BinStream,AComponent);
    BinStream.Position:=0;
    LRSObjectBinaryToText(BinStream,AStream);
  finally
    BinStream.Free;
  end;
end;

procedure ReadComponentFromTextStream(AStream: TStream;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent;
  Parent: TComponent);
var
  BinStream: TMemoryStream;
begin
  BinStream:=nil;
  try
    BinStream:=TMemoryStream.Create;
    LRSObjectTextToBinary(AStream,BinStream);
    BinStream.Position:=0;
    ReadComponentFromBinaryStream(BinStream,RootComponent,OnFindComponentClass,
                                  TheOwner,Parent);
  finally
    BinStream.Free;
  end;
end;

procedure SaveComponentToConfig(Config: TConfigStorage; const Path: string;
  AComponent: TComponent);
var
  BinStream: TMemoryStream;
  TxtStream: TMemoryStream;
  s: string;
begin
  BinStream:=nil;
  TxtStream:=nil;
  try
    // write component to stream
    BinStream:=TMemoryStream.Create;
    WriteComponentAsBinaryToStream(BinStream,AComponent);
    // convert it to human readable text format
    BinStream.Position:=0;
    TxtStream:=TMemoryStream.Create;
    LRSObjectBinaryToText(BinStream,TxtStream);
    // convert stream to string
    SetLength(s,TxtStream.Size);
    TxtStream.Position:=0;
    if s<>'' then
      TxtStream.Read(s[1],length(s));
    // write to config
    Config.SetDeleteValue(Path,s,'');
  finally
    BinStream.Free;
    TxtStream.Free;
  end;
end;

procedure LoadComponentFromConfig(Config: TConfigStorage; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent;
  Parent: TComponent);
var
  s: String;
  TxtStream: TMemoryStream;
begin
  // read from config
  s:=Config.GetValue(Path,'');
  TxtStream:=nil;
  try
    TxtStream:=TMemoryStream.Create;
    if s<>'' then
      TxtStream.Write(s[1],length(s));
    TxtStream.Position:=0;
    // create component from stream
    ReadComponentFromTextStream(TxtStream,RootComponent,OnFindComponentClass,
                                TheOwner,Parent);
  finally
    TxtStream.Free;
  end;
end;

function CompareComponents(Component1, Component2: TComponent): boolean;
var
  Stream1: TMemoryStream;
  Stream2: TMemoryStream;
  i: Integer;
begin
  if Component1=Component2 then exit(true);
  Result:=false;
  // quick checks
  if (Component1=nil) or (Component2=nil) then exit;
  if (Component1.ClassType<>Component2.ClassType) then exit;
  if Component1.ComponentCount<>Component2.ComponentCount then exit;
  for i:=0 to Component1.ComponentCount-1 do begin
    if Component1.Components[i].ClassType<>Component2.Components[i].ClassType
    then exit;
  end;
  // expensive streaming test
  try
    Stream1:=nil;
    Stream2:=nil;
    try
      Stream1:=TMemoryStream.Create;
      WriteComponentAsBinaryToStream(Stream1,Component1);
      Stream2:=TMemoryStream.Create;
      WriteComponentAsBinaryToStream(Stream2,Component2);
      Result:=CompareMemStreams(Stream1,Stream2);
    finally
      Stream1.Free;
      Stream2.Free;
    end;
  except
  end;
end;

function CompareMemStreams(Stream1, Stream2: TCustomMemoryStream
  ): boolean;
var
  p1: Pointer;
  p2: Pointer;
  Cnt: Int64;
  CurCnt: cardinal;
begin
  if Stream1=Stream2 then exit(true);
  Result:=false;
  if (Stream1=nil) or (Stream2=nil) then exit;
  if Stream1.Size<>Stream2.Size then exit;
  Cnt:=Stream1.Size;
  p1:=Stream1.Memory;
  p2:=Stream2.Memory;
  while Cnt>0 do begin
    CurCnt:=Cnt;
    if CurCnt>=High(Cardinal) then CurCnt:=High(Cardinal);
    if not CompareMem(p1,p2,CurCnt) then exit;
    inc(p1,CurCnt);
    inc(p2,CurCnt);
    dec(Cnt,CurCnt);
  end;
  Result:=true;
end;

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  const ResourceName, ResourceType: String);
{ example ResStream:
  LazarusResources.Add('ResourceName','ResourceType',
    #123#45#34#78#18#72#45#34#78#18#72#72##45#34#78#45#34#78#184#34#78#145#34#78
    +#83#187#6#78#83
  );
}
const
  ReadBufSize = 4096;
  WriteBufSize = 4096;
var
  s, Indent: string;
  x: integer;
  c: char;
  RangeString, NewRangeString: boolean;
  RightMargin, CurLine: integer;
  WriteBufStart, Writebuf: PChar;
  WriteBufPos: Integer;
  ReadBufStart, ReadBuf: PChar;
  ReadBufPos, ReadBufLen: integer;
  MinCharCount: Integer;
  
  procedure FillReadBuf;
  begin
    ReadBuf:=ReadBufStart;
    ReadBufPos:=0;
    ReadBufLen:=BinStream.Read(ReadBuf^,ReadBufSize);
  end;
  
  procedure InitReadBuf;
  begin
    GetMem(ReadBufStart,ReadBufSize);
    FillReadBuf;
  end;

  function ReadChar(var c: char): boolean;
  begin
    if ReadBufPos>=ReadBufLen then begin
      FillReadBuf;
      if ReadBufLen=0 then begin
        Result:=false;
        exit;
      end;
    end;
    c:=ReadBuf^;
    inc(ReadBuf);
    inc(ReadBufPos);
    Result:=true;
  end;

  procedure InitWriteBuf;
  begin
    GetMem(WriteBufStart,WriteBufSize);
    WriteBuf:=WriteBufStart;
    WriteBufPos:=0;
  end;

  procedure FlushWriteBuf;
  begin
    if WriteBufPos>0 then begin
      ResStream.Write(WriteBufStart^,WriteBufPos);
      WriteBuf:=WriteBufStart;
      WriteBufPos:=0;
    end;
  end;
  
  procedure WriteChar(c: char);
  begin
    WriteBuf^:=c;
    inc(WriteBufPos);
    inc(WriteBuf);
    if WriteBufPos>=WriteBufSize then
      FlushWriteBuf;
  end;
  
  procedure WriteString(const s: string);
  var
    i: Integer;
  begin
    for i:=1 to length(s) do WriteChar(s[i]);
  end;

  procedure WriteShortString(const s: string);
  var
    i: Integer;
  begin
    for i:=1 to length(s) do WriteChar(s[i]);
  end;

begin
  // fpc is not optimized for building a constant string out of thousands of
  // lines. It needs huge amounts of memory and becomes very slow. Therefore big
  // files are split into several strings.

  InitReadBuf;
  InitWriteBuf;
  InitByteToStr;

  Indent:='';
  s:=Indent+'LazarusResources.Add('''+ResourceName+''','''+ResourceType+''',['+LineEnd;
  WriteString(s);
  Indent:='  '+Indent;
  WriteString(Indent);
  x:=length(Indent);
  RangeString:=false;
  CurLine:=1;
  RightMargin:=80;
  if ReadBufLen>0 then begin
    while ReadChar(c) do begin
      NewRangeString:=(ord(c)>=32) and (ord(c)<127);
      // check if new char fits into line or if a new line must be started
      if NewRangeString then begin
        if RangeString then
          MinCharCount:=2 // char plus '
        else
          MinCharCount:=3; // ' plus char plus '
        if c='''' then inc(MinCharCount);
      end else begin
        MinCharCount:=1+length(ByteToStr[c]); // # plus number
        if RangeString then
          inc(MinCharCount); // plus ' for ending last string constant
      end;
      if x+MinCharCount>RightMargin then begin
        // break line
        if RangeString then begin
          // end string constant
          WriteChar('''');
        end;
        // write line ending
        WriteShortString(LineEnd);
        x:=0;
        inc(CurLine);
        // write indention
        WriteString(Indent);
        inc(x,length(Indent));
        // write operator
        if (CurLine and 63)<>1 then
          WriteChar('+')
        else
          WriteChar(',');
        inc(x);
        RangeString:=false;
      end;
      // write converted byte
      if RangeString<>NewRangeString then begin
        WriteChar('''');
        inc(x);
      end;
      if NewRangeString then begin
        WriteChar(c);
        inc(x);
        if c='''' then begin
          WriteChar(c);
          inc(x);
        end;
      end else begin
        WriteChar('#');
        inc(x);
        WriteShortString(ByteToStr[c]);
        inc(x,length(ByteToStr[c]));
      end;
      // next
      RangeString:=NewRangeString;
    end;
    if RangeString then begin
      WriteChar('''');
    end;
  end else begin
    WriteShortString('''''');
  end;
  Indent:=copy(Indent,3,length(Indent)-2);
  s:=LineEnd+Indent+']);'+LineEnd;
  WriteString(s);
  FlushWriteBuf;
  FreeMem(ReadBufStart);
  FreeMem(WriteBufStart);
end;

function FindLFMClassName(LFMStream:TStream):ansistring;
{ examples:
  object Form1: TForm1
  inherited AboutBox2: TAboutBox2

  -> the classname is the last word of the first line
}
var c:char;
  StartPos, EndPos: Int64;
begin
  Result:='';
  StartPos:=-1;
  c:=' ';
  // read till end of line
  repeat
    // remember last non identifier char position
    if (not (c in ['a'..'z','A'..'Z','0'..'9','_'])) then
      StartPos:=LFMStream.Position;
    if LFMStream.Read(c,1)<>1 then exit;
    if LFMStream.Position>1000 then exit;
  until c in [#10,#13];
  if StartPos<0 then exit;
  EndPos:=LFMStream.Position-1;
  if EndPos-StartPos>255 then exit;
  SetLength(Result,EndPos-StartPos);
  LFMStream.Position:=StartPos;
  if Length(Result) > 0 then
    LFMStream.Read(Result[1],length(Result));
  LFMStream.Position:=0;
  if (Result='') or (not IsValidIdent(Result)) then
    Result:='';
end;

function LFMtoLRSfile(const LFMfilename: string):boolean;
// returns true if successful
var
  LFMFileStream, LRSFileStream: TFileStreamUTF8;
  LFMMemStream, LRSMemStream: TMemoryStream;
  LRSfilename, LFMfilenameExt: string;
begin
  Result:=true;
  try
    LFMFileStream:=TFileStreamUTF8.Create(LFMfilename,fmOpenRead);
    LFMMemStream:=TMemoryStream.Create;
    LRSMemStream:=TMemoryStream.Create;
    try
      LFMMemStream.SetSize(LFMFileStream.Size);
      LFMMemStream.CopyFrom(LFMFileStream,LFMFileStream.Size);
      LFMMemStream.Position:=0;
      LFMfilenameExt:=ExtractFileExt(LFMfilename);
      LRSfilename:=copy(LFMfilename,1,
                    length(LFMfilename)-length(LFMfilenameExt))+'.lrs';
      Result:=LFMtoLRSstream(LFMMemStream,LRSMemStream);
      if not Result then exit;
      LRSMemStream.Position:=0;
      LRSFileStream:=TFileStreamUTF8.Create(LRSfilename,fmCreate);
      try
        LRSFileStream.CopyFrom(LRSMemStream,LRSMemStream.Size);
      finally
        LRSFileStream.Free;
      end;
    finally
      LFMMemStream.Free;
      LRSMemStream.Free;
      LFMFileStream.Free;
    end;
  except
    on E: Exception do begin
      Result:=false;
    end;
  end;
end;

function LFMtoLRSstream(LFMStream, LRSStream: TStream):boolean;
// returns true if successful
var FormClassName:ansistring;
  BinStream:TMemoryStream;
begin
  Result:=true;
  try
    FormClassName:=FindLFMClassName(LFMStream);
    BinStream:=TMemoryStream.Create;
    try
      LRSObjectTextToBinary(LFMStream,BinStream);
      BinStream.Position:=0;
      BinaryToLazarusResourceCode(BinStream,LRSStream,FormClassName,'FORMDATA');
    finally
      BinStream.Free;
    end;
  except
    on E: Exception do begin
      Result:=false;
    end;
  end;
end;

//==============================================================================

{ TLResourceList }

constructor TLResourceList.Create;
begin
  FList := TList.Create;
  FMergeList := TList.Create;
  FSortedCount := 0;
end;

destructor TLResourceList.Destroy;
var
  a: integer;
begin
  for a := 0 to FList.Count - 1 do
    TLResource(FList[a]).Free;
  FList.Free;
  FMergeList.Free;
end;

function TLResourceList.Count: integer;
begin
  if (Self<>nil) and (FList<>nil) then
    Result:=FList.Count
  else
    Result:=0;
end;

procedure TLResourceList.Add(const Name, ValueType: AnsiString;
  const Values: array of string);
var
  NewLResource: TLResource;
  i, TotalLen, ValueCount, p: integer;
begin
  NewLResource := TLResource.Create;
  NewLResource.Name := Name;
  NewLResource.ValueType := uppercase(ValueType);
  
  ValueCount := High(Values) - Low(Values) + 1;
  case ValueCount of
    0:
      begin
        NewLResource.Free;
        exit;
      end;
    1:
      NewLResource.Value:=Values[0];
  else
    TotalLen := 0;
    for i := Low(Values) to High(Values) do
      inc(TotalLen, length(Values[i]));
    SetLength(NewLResource.Value, TotalLen);
    p := 1;
    for i := Low(Values) to High(Values) do
    begin
      if length(Values[i]) > 0 then
      begin
        Move(Values[i][1], NewLResource.Value[p], length(Values[i]));
        inc(p, length(Values[i]));
      end;
    end;
  end;
  
  FList.Add(NewLResource);
end;

function TLResourceList.Find(const Name: AnsiString):TLResource;
var
  P: Integer;
begin
  P := FindPosition(Name);
  if P >= 0 then
    Result := TLResource(FList[P])
  else
    Result := nil;
end;

function TLResourceList.Find(const Name, ValueType: AnsiString): TLResource;
var
  P, I: Integer;
begin
  P := FindPosition(Name);
  if P >= 0 then
  begin
    // Since we can have many resources that have the same name but different type
    // we should look before and after found position (do not forget that we are searching
    // them by dividing intervals)
  
    // look before position
    for I := P - 1 downto 0 do
    begin
      Result := TLResource(FList[I]);
      if SysUtils.CompareText(Result.Name,Name)<>0 then
        break;
      if Result.ValueType = ValueType then
        Exit;
    end;
    // look behind position
    for I := P to FList.Count - 1 do
    begin
      Result := TLResource(FList[I]);
      if SysUtils.CompareText(Result.Name,Name)<>0 then
        break;
      if Result.ValueType = ValueType then
        Exit;
    end;
  end;
  Result := nil;
end;

function TLResourceList.FindPosition(const Name: AnsiString): Integer;
var
  L, R, C: Integer;
begin
  if FSortedCount < FList.Count then
    Sort;
  L := 0;
  R := FList.Count-1;
  while (L <= R) do
  begin
    Result := (L + R) shr 1;
    C := SysUtils.CompareText(Name, TLResource(FList[Result]).Name);
    if C < 0 then
      R := Result - 1
    else
    if C > 0 then
      L := Result + 1
    else
      Exit;
  end;
  Result := -1;
end;

function TLResourceList.GetItems(Index: integer): TLResource;
begin
  Result := TLResource(FList[Index]);
end;

procedure TLResourceList.Sort;
{$IFNDEF DisableChecks}
var
  i: Integer;
  r1: TLResource;
  r2: TLResource;
{$ENDIF}
begin
  if FSortedCount = FList.Count then
    exit;
  // sort the unsorted elements
  FMergeList.Count := FList.Count;
  MergeSort(FList, FMergeList, FSortedCount, FList.Count - 1);
  // merge both
  Merge(FList, FMergeList, 0, FSortedCount, FList.Count - 1);
  FSortedCount := FList.Count;
  // check for doubles
  {$IFNDEF DisableChecks}
  for i:=0 to FList.Count-2 do
  begin
    r1:=TLResource(FList[i]);
    r2:=TLResource(FList[i+1]);
    if (SysUtils.CompareText(r1.Name,r2.Name)=0) and (r1.ValueType=r2.ValueType) then
    begin
    end;
  end;
  {$ENDIF}
end;

procedure TLResourceList.MergeSort(List, MergeList: TList; Pos1, Pos2: integer);
var
  cmp, mid: integer;
begin
  if Pos1 = Pos2 then
  begin
  end else
  if Pos1 + 1 = Pos2 then
  begin
    cmp := SysUtils.CompareText(TLResource(List[Pos1]).Name, TLResource(List[Pos2]).Name);
    if cmp > 0 then
    begin
      MergeList[Pos1] := List[Pos1];
      List[Pos1] := List[Pos2];
      List[Pos2] := MergeList[Pos1];
    end;
  end else
  begin
    if Pos2 > Pos1 then
    begin
      mid := (Pos1 + Pos2) shr 1;
      MergeSort(List, MergeList, Pos1, mid);
      MergeSort(List, MergeList, mid + 1, Pos2);
      Merge(List, MergeList, Pos1, mid + 1, Pos2);
    end;
  end;
end;

procedure TLResourceList.Merge(List, MergeList: TList; Pos1, Pos2, Pos3: integer);
// merge two sorted arrays
// the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
var
  Src1Pos, Src2Pos, DestPos, cmp, a: integer;
begin
  if (Pos1 >= Pos2) or (Pos2 > Pos3) then
    exit;
  Src1Pos := Pos2 - 1;
  Src2Pos := Pos3;
  DestPos := Pos3;
  while (Src2Pos >= Pos2) and (Src1Pos >= Pos1) do
  begin
    cmp:=SysUtils.CompareText(TLResource(List[Src1Pos]).Name, TLResource(List[Src2Pos]).Name);
    if cmp > 0 then
    begin
      MergeList[DestPos] := List[Src1Pos];
      dec(Src1Pos);
    end else
    begin
      MergeList[DestPos] := List[Src2Pos];
      dec(Src2Pos);
    end;
    dec(DestPos);
  end;
  while Src2Pos >= Pos2 do
  begin
    MergeList[DestPos] := List[Src2Pos];
    dec(Src2Pos);
    dec(DestPos);
  end;
  for a := DestPos + 1 to Pos3 do
    List[a] := MergeList[a];
end;

procedure TLResourceList.Add(const Name, ValueType, Value: AnsiString);
begin
  Add(Name, ValueType, [Value]);
end;

//------------------------------------------------------------------------------
// Delphi object streams

type
  TDelphiValueType = (dvaNull, dvaList, dvaInt8, dvaInt16, dvaInt32, dvaExtended,
    dvaString, dvaIdent, dvaFalse, dvaTrue, dvaBinary, dvaSet, dvaLString,
    dvaNil, dvaCollection, dvaSingle, dvaCurrency, dvaDate, dvaWString,
    dvaInt64, dvaUTF8String);
    
  TDelphiReader = class
  private
    FStream: TStream;
  protected
    procedure SkipBytes(Count: Integer);
    procedure SkipSetBody;
    procedure SkipProperty;
  public
    constructor Create(Stream: TStream);
    procedure ReadSignature;
    procedure Read(out Buf; Count: Longint);
    function ReadInteger: Longint;
    function ReadValue: TDelphiValueType;
    function NextValue: TDelphiValueType;
    function ReadStr: string;
    function EndOfList: Boolean;
    procedure SkipValue;
    procedure CheckValue(Value: TDelphiValueType);
    procedure ReadListEnd;
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer); virtual;
    function ReadFloat: Extended;
    function ReadSingle: Single;
    function ReadCurrency: Currency;
    function ReadDate: TDateTime;
    function ReadString: string;
    //function ReadWideString: WideString;
    function ReadInt64: Int64;
    function ReadIdent: string;
  end;

  TDelphiWriter = class
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    procedure Write(const Buf; Count: Longint);
  end;
  
{ TDelphiReader }

procedure ReadError(Msg: string);
begin
  raise EReadError.Create(Msg);
end;

procedure PropValueError;
begin
  ReadError(rsInvalidPropertyValue);
end;

procedure TDelphiReader.SkipBytes(Count: Integer);
begin
  FStream.Position:=FStream.Position+Count;
end;

procedure TDelphiReader.SkipSetBody;
begin
  while ReadStr <> '' do ;
end;

procedure TDelphiReader.SkipProperty;
begin
  ReadStr; { Skips property name }
  SkipValue;
end;

constructor TDelphiReader.Create(Stream: TStream);
begin
  FStream:=Stream;
end;

procedure TDelphiReader.ReadSignature;
var
  Signature: TFilerSignature;
begin
  Signature:='1234';
  Read(Signature[1], length(Signature));
  if Signature<>FilerSignature then
    ReadError(rsInvalidStreamFormat);
end;

procedure TDelphiReader.Read(out Buf; Count: Longint);
begin
  FStream.Read(Buf,Count);
end;

function TDelphiReader.ReadInteger: Longint;
var
  S: Shortint;
  I: Smallint;
begin
  case ReadValue of
    dvaInt8:
      begin
        Read(S, SizeOf(Shortint));
        Result := S;
      end;
    dvaInt16:
      begin
        Read(I, SizeOf(I));
        Result := I;
      end;
    dvaInt32:
      Read(Result, SizeOf(Result));
  else
    Result:=0;
    PropValueError;
  end;
end;

function TDelphiReader.ReadValue: TDelphiValueType;
var b: byte;
begin
  Read(b,1);
  Result:=TDelphiValueType(b);
end;

function TDelphiReader.NextValue: TDelphiValueType;
begin
  Result := ReadValue;
  FStream.Position:=FStream.Position-1;
end;

function TDelphiReader.ReadStr: string;
var
  L: Byte;
begin
  Read(L, SizeOf(Byte));
  SetLength(Result, L);
  if L>0 then
    Read(Result[1], L);
end;

function TDelphiReader.EndOfList: Boolean;
begin
  Result := (ReadValue = dvaNull);
  FStream.Position:=FStream.Position-1;
end;

procedure TDelphiReader.SkipValue;

  procedure SkipList;
  begin
    while not EndOfList do SkipValue;
    ReadListEnd;
  end;

  procedure SkipBinary(BytesPerUnit: Integer);
  var
    Count: Longint;
  begin
    Read(Count, SizeOf(Count));
    SkipBytes(Count * BytesPerUnit);
  end;

  procedure SkipCollection;
  begin
    while not EndOfList do
    begin
      if NextValue in [dvaInt8, dvaInt16, dvaInt32] then SkipValue;
      SkipBytes(1);
      while not EndOfList do SkipProperty;
      ReadListEnd;
    end;
    ReadListEnd;
  end;

begin
  case ReadValue of
    dvaNull: { no value field, just an identifier };
    dvaList: SkipList;
    dvaInt8: SkipBytes(SizeOf(Byte));
    dvaInt16: SkipBytes(SizeOf(Word));
    dvaInt32: SkipBytes(SizeOf(LongInt));
    dvaExtended: SkipBytes(SizeOf(Extended));
    dvaString, dvaIdent: ReadStr;
    dvaFalse, dvaTrue: { no value field, just an identifier };
    dvaBinary: SkipBinary(1);
    dvaSet: SkipSetBody;
    dvaLString: SkipBinary(1);
    dvaCollection: SkipCollection;
    dvaSingle: SkipBytes(Sizeof(Single));
    dvaCurrency: SkipBytes(SizeOf(Currency));
    dvaDate: SkipBytes(Sizeof(TDateTime));
    dvaWString: SkipBinary(Sizeof(WideChar));
    dvaInt64: SkipBytes(Sizeof(Int64));
    dvaUTF8String: SkipBinary(1);
  end;
end;

procedure TDelphiReader.CheckValue(Value: TDelphiValueType);
begin
  if ReadValue <> Value then
  begin
    FStream.Position:=FStream.Position-1;
    SkipValue;
    PropValueError;
  end;
end;

procedure TDelphiReader.ReadListEnd;
begin
  CheckValue(dvaNull);
end;

procedure TDelphiReader.ReadPrefix(var Flags: TFilerFlags;
  var AChildPos: Integer);
var
  Prefix: Byte;
begin
  Flags := [];
  if Byte(NextValue) and $F0 = $F0 then
  begin
    Prefix := Byte(ReadValue);
    if (Prefix and ObjStreamMaskInherited)>0 then
      Include(Flags,ffInherited);
    if (Prefix and ObjStreamMaskChildPos)>0 then
      Include(Flags,ffChildPos);
    if (Prefix and ObjStreamMaskInline)>0 then
      Include(Flags,ffInline);
    if ffChildPos in Flags then AChildPos := ReadInteger;
  end;
end;

function TDelphiReader.ReadFloat: Extended;
begin
  if ReadValue = dvaExtended then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadSingle: Single;
begin
  if ReadValue = dvaSingle then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadCurrency: Currency;
begin
  if ReadValue = dvaCurrency then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadDate: TDateTime;
begin
  if ReadValue = dvaDate then
    Read(Result, SizeOf(Result))
  else begin
    FStream.Position:=FStream.Position-1;
    Result := ReadInteger;
  end;
end;

function TDelphiReader.ReadString: string;
var
  L: Integer;
begin
  Result := '';
  if NextValue in [dvaWString, dvaUTF8String] then begin
    ReadError('TDelphiReader.ReadString: WideString and UTF8String are not implemented yet');
    //Result := ReadWideString;
  end else
  begin
    L := 0;
    case ReadValue of
      dvaString:
        Read(L, SizeOf(Byte));
      dvaLString:
        Read(L, SizeOf(Integer));
    else
      PropValueError;
    end;
    SetLength(Result, L);
    Read(Pointer(Result)^, L);
  end;
end;

function TDelphiReader.ReadInt64: Int64;
begin
  if NextValue = dvaInt64 then
  begin
    ReadValue;
    Read(Result, Sizeof(Result));
  end
  else
    Result := ReadInteger;
end;

function TDelphiReader.ReadIdent: string;
var
  L: Byte;
begin
  case ReadValue of
    dvaIdent:
      begin
        Read(L, SizeOf(Byte));
        SetLength(Result, L);
        Read(Result[1], L);
      end;
    dvaFalse:
      Result := 'False';
    dvaTrue:
      Result := 'True';
    dvaNil:
      Result := 'nil';
    dvaNull:
      Result := 'Null';
  else
    Result:='';
    PropValueError;
  end;
end;

{ TDelphiWriter }

{ MultiByte Character Set (MBCS) byte type }
type
  TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);

function ByteType(const S: string; Index: Integer): TMbcsByteType;
begin
  Result := mbSingleByte;
  { ToDo:
    if SysLocale.FarEast then
      Result := ByteTypeTest(PChar(S), Index-1);
  }
end;

constructor TDelphiWriter.Create(Stream: TStream);
begin
  FStream:=Stream;
end;

procedure TDelphiWriter.Write(const Buf; Count: Longint);
begin
  FStream.Write(Buf,Count);
end;

procedure ReadLFMHeader(LFMStream: TStream;
  out LFMType, LFMComponentName, LFMClassName: String);
var
  c:char;
  Token: String;
begin
  { examples:
    object Form1: TForm1
    inherited AboutBox2: TAboutBox2
  }
  LFMComponentName:='';
  LFMClassName := '';
  LFMType := '';
  Token := '';
  while (LFMStream.Read(c,1)=1) and (LFMStream.Position<1000) do begin
    if c in ['a'..'z','A'..'Z','0'..'9','_'] then
      Token := Token + c
    else begin
      if Token<>'' then begin
        if LFMType = '' then
          LFMType := Token
        else if LFMComponentName='' then
          LFMComponentName:=Token
        else if LFMClassName = '' then
          LFMClassName := Token;
        Token := '';
      end;
      if c in [#10,#13] then break;
    end;
  end;
  LFMStream.Position:=0;
end;

procedure ReadLFMHeader(const LFMSource: string;
  out LFMClassName: String; out LFMType: String);
var
  LFMComponentName: string;
begin
  ReadLFMHeader(LFMSource,LFMType,LFMComponentName,LFMClassName);
end;

procedure ReadLFMHeader(const LFMSource: string; out LFMType, LFMComponentName,
  LFMClassName: String);
var
  p: Integer;
  StartPos: LongInt;
begin
  { examples:
    object Form1: TForm1
    inherited AboutBox2: TAboutBox2

    - LFMType is the first word on the line
    - LFMComponentName is the second word
    - LFMClassName is the fourth token
  }
  
  // read first word => LFMType
  p:=1;
  while (p<=length(LFMSource))
  and (LFMSource[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
    inc(p);
  LFMType:=copy(LFMSource,1,p-1);
  
  // read second word => LFMComponentName
  while (p<=length(LFMSource)) and (LFMSource[p] in [' ',#9]) do inc(p);
  StartPos:=p;
  while (p<=length(LFMSource))
  and (LFMSource[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
    inc(p);
  LFMComponentName:=copy(LFMSource,StartPos,p-StartPos);

  // read third word => LFMClassName
  while (p<=length(LFMSource)) and (LFMSource[p] in [' ',#9,':']) do inc(p);
  StartPos:=p;
  while (p<=length(LFMSource))
  and (LFMSource[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
    inc(p);
  LFMClassName:=copy(LFMSource,StartPos,p-StartPos);
end;

function ReadLFMHeaderFromFile(const Filename: string; out LFMType,
  LFMComponentName, LFMClassName: String): boolean;
var
  fs: TFileStreamUTF8;
  Header: string;
  Cnt: LongInt;
begin
  Result:=false;
  try
    fs:=TFileStreamUTF8.Create(Filename,fmOpenRead);
    try
      SetLength(Header,600);
      Cnt:=fs.Read(Header[1],length(Header));
      SetLength(Header,Cnt);
      ReadLFMHeader(Header,LFMType,LFMComponentName,LFMClassName);
      Result:=LFMClassName<>'';
    finally
      fs.Free;
    end;
  except
  end;
end;

function CreateLFMFile(AComponent: TComponent; LFMStream: TStream): integer;
// 0 = ok
// -1 = error while streaming AForm to binary stream
// -2 = error while streaming binary stream to text file
var
  BinStream: TMemoryStream;
  DestroyDriver: Boolean;
  Writer: TWriter;
begin
  Result:=0;
  BinStream:=TMemoryStream.Create;
  try
    try
      // write component to binary stream
      DestroyDriver:=false;
      Writer:=CreateLRSWriter(BinStream,DestroyDriver);
      try
        Writer.WriteDescendent(AComponent,nil);
      finally
        if DestroyDriver then Writer.Driver.Free;
        Writer.Free;
      end;
    except
      Result:=-1;
      exit;
    end;
    try
      // transform binary to text
      BinStream.Position:=0;
      LRSObjectBinaryToText(BinStream,LFMStream);
    except
      Result:=-2;
      exit;
    end;
  finally
    BinStream.Free;
  end;
end;

procedure LRSObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(const s: String);
  {$IFDEF VerboseLRSObjectBinaryToText}
  var
    i: Integer;
  {$ENDIF}
  begin
    {$IFDEF VerboseLRSObjectBinaryToText}
    for i:=1 to length(s) do begin
      if (s[i] in [#0..#8,#11..#12,#14..#31]) then begin
        DbgOut('#'+IntToStr(ord(s[i])));
        RaiseGDBException('ObjectLRSToText: Invalid character');
      end else
        DbgOut(s[i]);
    end;
    {$ENDIF}
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(const s: String);
  begin
    OutStr(s + LineEnding);
  end;

  procedure OutString(const s: String);
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    if s<>'' then begin
      res := '';
      InString := False;
      for i := 1 to Length(s) do begin
        NewInString := InString;
        case s[i] of
          #0..#31: begin
              NewInString := False;
              NewStr := '#' + IntToStr(Ord(s[i]));
            end;
          '''': begin
              NewInString := True;
              NewStr:=''''''; // write two ticks, so the reader will read one
            end;
          else begin
            NewInString := True;
            NewStr := s[i];
          end;
        end;
        if NewInString <> InString then begin
          NewStr := '''' + NewStr;
          InString := NewInString;
        end;
        res := res + NewStr;
      end;
      if InString then res := res + '''';
    end else begin
      res:='''''';
    end;
    OutStr(res);
  end;

  procedure OutWideString(const s: WideString);
  // write as normal string
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    //debugln('OutWideString ',s);
    res := '';
    if s<>'' then begin
      InString := False;
      for i := 1 to Length(s) do begin
        NewInString := InString;
        if (ord(s[i])<ord(' ')) or (ord(s[i])>=127) then begin
          // special char
          NewInString := False;
          NewStr := '#' + IntToStr(Ord(s[i]));
        end
        else if s[i]='''' then begin
          // '
          if InString then
            NewStr := ''''''
          else
            NewStr := '''''''';
        end
        else begin
          // normal char
          NewInString := True;
          NewStr := AnsiString(s[i]);
        end;
        if NewInString <> InString then begin
          NewStr := '''' + NewStr;
          InString := NewInString;
        end;
        res := res + NewStr;
      end;
      if InString then res := res + '''';
    end else begin
      res:='''''';
    end;
    OutStr(res);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  var
    w: Word;
  begin
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: begin
          w:=ReadLRSWord(Input);
          //DebugLn('ReadInt vaInt16 w=',IntToStr(w));
          Result := SmallInt(w);
        end;
      vaInt32: Result := ReadLRSInteger(Input);
      else Result := 0;
    end;
  end;

  function ReadInt: LongInt;
  begin
    Result := ReadInt(TValueType(Input.ReadByte));
  end;

  function ReadShortString: String;
  var
    len: Byte;
  begin
    len := Input.ReadByte;
    SetLength(Result, len);
    if (Len > 0) then
      Input.Read(Result[1], len);
  end;

  function ReadLongString: String;
  var
    len: integer;
  begin
    len := ReadLRSInteger(Input);
    SetLength(Result, len);
    if (Len > 0) then
      Input.Read(Result[1], len);
  end;

  procedure ReadPropList(const indent: String);

    procedure ProcessValue(ValueType: TValueType; const Indent: String);

      procedure Stop(const s: String);
      begin

      end;
      
      function ValueTypeAsString(ValueType: TValueType): string;
      begin
        case ValueType of
        vaNull: Result:='vaNull';
        vaList: Result:='vaList';
        vaInt8: Result:='vaInt8';
        vaInt16: Result:='vaInt16';
        vaInt32: Result:='vaInt32';
        vaExtended: Result:='vaExtended';
        vaString: Result:='vaString';
        vaIdent: Result:='vaIdent';
        vaFalse: Result:='vaFalse';
        vaTrue: Result:='vaTrue';
        vaBinary: Result:='vaBinary';
        vaSet: Result:='vaSet';
        vaLString: Result:='vaLString';
        vaNil: Result:='vaNil';
        vaCollection: Result:='vaCollection';
        vaSingle: Result:='vaSingle';
        vaCurrency: Result:='vaCurrency';
        vaDate: Result:='vaDate';
        vaWString: Result:='vaWString';
        vaInt64: Result:='vaInt64';
        vaUTF8String: Result:='vaUTF8String';
        vaUString: Result:='vaUString';
        vaQWord : Result:='vaQWord';
        else Result:='Unknown ValueType='+dbgs(Ord(ValueType));
        end;
      end;
      
      procedure UnknownValueType;
      var
        s: String;
        {$IFNDEF DisableChecks}
        HintStr: string;
        HintLen: Int64;
        {$ENDIF}
      begin
        s:=ValueTypeAsString(ValueType);
        if s<>'' then
          s:='Unimplemented ValueType='+s;
        {$IFNDEF DisableChecks}
        HintLen:=Output.Position;
        if HintLen>50 then HintLen:=50;
        SetLength(HintStr,HintLen);
        if HintStr<>'' then begin
          try
            Output.Position:=Output.Position-length(HintStr);
            Output.Read(HintStr[1],length(HintStr));
            //debugln('ObjectLRSToText:');
          except
          end;
        end;
        {$ENDIF}
        s:=s+' ';
        Stop(s);
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, StartPos, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
        p: pchar;
      const
        HexDigits: array[0..$F] of char = '0123456789ABCDEF';
      begin
        ToDo := ReadLRSCardinal(Input);
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          StartPos := length(s);
          Input.Read(lbuf, DoNow);
          setlength(s, StartPos+DoNow*2);
          p := @s[StartPos];
          for i := 0 to DoNow - 1 do begin
            inc(p);
            p^ := HexDigits[(lbuf[i] shr 4) and $F];
            inc(p);
            p^ := HexDigits[lbuf[i] and $F];
          end;
          OutLn(s);
        end;
        OutStr(indent);
        OutLn('}');
      end;

    var
      s: String;
      IsFirst: Boolean;
      ext: Extended;
      ASingle: single;
      ADate: TDateTime;
      ACurrency: Currency;
      AWideString: WideString;

    begin
      //DebugLn(['ProcessValue ',Indent,' ValueType="',ValueTypeAsString(ValueType),'"']);
      case ValueType of
        vaList: begin
            OutStr('(');
            IsFirst := True;
            while True do begin
              ValueType := TValueType(Input.ReadByte);
              if ValueType = vaNull then break;
              if IsFirst then begin
                OutLn('');
                IsFirst := False;
              end;
              OutStr(Indent + '  ');
              ProcessValue(ValueType, Indent + '  ');
            end;
            OutLn(Indent + ')');
          end;
        vaInt8: begin
            // MG: IntToStr has a bug with ShortInt, therefore these typecasts
            OutLn(IntToStr(Integer(ShortInt(Input.ReadByte))));
          end;
        vaInt16: OutLn(IntToStr(SmallInt(ReadLRSWord(Input))));
        vaInt32: OutLn(IntToStr(ReadLRSInteger(Input)));
        vaInt64: OutLn(IntToStr(ReadLRSInt64(Input)));
        vaExtended: begin
            ext:=ReadLRSExtended(Input);
            OutLn(FloatToStr(ext));
          end;
        vaString: begin
            OutString(ReadShortString);
            OutLn('');
          end;
        vaIdent: OutLn(ReadShortString);
        vaFalse: OutLn('False');
        vaTrue: OutLn('True');
        vaBinary: ProcessBinary;
        vaSet: begin
            OutStr('[');
            IsFirst := True;
            while True do begin
              s := ReadShortString;
              if Length(s) = 0 then break;
              if not IsFirst then OutStr(', ');
              IsFirst := False;
              OutStr(s);
            end;
            OutLn(']');
          end;
        vaLString: begin
            OutString(ReadLongString);
            OutLn('');
          end;
        vaNil:
          OutLn('nil');
        vaCollection: begin
            OutStr('<');
            while Input.ReadByte <> 0 do begin
              OutLn(Indent);
              Input.Seek(-1, soFromCurrent);
              OutStr(indent + '  item');
              ValueType := TValueType(Input.ReadByte);
              if ValueType <> vaList then
                OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
              OutLn('');
              ReadPropList(indent + '    ');
              OutStr(indent + '  end');
            end;
            OutLn('>');
          end;
        vaSingle: begin
            ASingle:=ReadLRSSingle(Input);
            OutLn(FloatToStr(ASingle));
          end;
        vaDate: begin
            ADate:=TDateTime(ReadLRSDouble(Input));
            OutLn(FloatToStr(ADate));
          end;
        vaCurrency: begin
            ACurrency:=ReadLRSCurrency(Input);
            OutLn(FloatToStr(ACurrency));
          end;
        vaWString,vaUString: begin
            AWideString:=ReadLRSWideString(Input);
            OutWideString(AWideString);
            OutLn('');
          end;
        else
          if ord(ValueType)=20 then begin
            // vaUTF8String
            // Delphi saves widestrings as UTF8 strings
            // The LCL does not use widestrings, but UTF8 directly
            // so, simply read and write the string
            OutString(ReadLongString);
            OutLn('');
          end else
            UnknownValueType;
      end;
    end;

  var
    NextByte: Byte;
  begin
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadShortString + ' = ');
      NextByte:=Input.ReadByte;
      if NextByte<>0 then
        ProcessValue(TValueType(NextByte), Indent)
      else
        OutLn('');
    end;
  end;

  procedure ReadObject(const indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    ChildPos := 0;
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and ObjStreamMaskChildPos) <> 0 then
        ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadShortString;
    ObjName := ReadShortString;

    OutStr(Indent);
    if (b and ObjStreamMaskInherited) <> 0 then OutStr('inherited')
    else if (b and ObjStreamMaskInline) <> 0 then OutStr('inline')
    else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and ObjStreamMaskChildPos) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

var
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
  Signature: TFilerSignature;
begin
  // Endian note: comparing 2 cardinals is endian independent
  Signature:='1234';
  Input.Read(Signature[1], length(Signature));
  if Signature<>FilerSignature then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.ThousandSeparator:=',';
  try
    ReadObject('');
  finally
    DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
    DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
  end;
end;

function TestFormStreamFormat(Stream: TStream): TLRSStreamOriginalFormat;
var
  Pos: TStreamSeekType;
  Signature: TFilerSignature;
begin
  Pos := Stream.Position;
  Signature[1] := #0; // initialize, in case the stream is at its end
  Stream.Read(Signature, length(Signature));
  Stream.Position := Pos;
  if (Signature[1] = #$FF) or (Signature = FilerSignature) then
    Result := sofBinary
    // text format may begin with "object", "inherited", or whitespace
  else if Signature[1] in ['o','O','i','I',' ',#13,#11,#9] then
    Result := sofText
  else
    Result := sofUnknown;
end;

type
  TObjectTextConvertProc = procedure (Input, Output: TStream);

procedure InternalLRSBinaryToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat;
  ConvertProc: TObjectTextConvertProc;
  BinarySignature: TFilerSignature);
var
  Pos: TStreamSeekType;
  Signature: TFilerSignature;
begin
  Pos := Input.Position;
  Signature := BinarySignature;
  Signature[1]:=#0;
  Input.Read(Signature[1], length(Signature));
  Input.Position := Pos;
  if Signature = BinarySignature then
  begin     // definitely binary format
    if OriginalFormat = sofBinary then begin
      if Output is TMemoryStream then
        TMemoryStream(Output).SetSize(Output.Position+(Input.Size-Input.Position));
      Output.CopyFrom(Input, Input.Size - Input.Position)
    end else
    begin
      if OriginalFormat = sofUnknown then
        Originalformat := sofBinary;
      ConvertProc(Input, Output);
    end;
  end
  else  // might be text format
  begin
    if OriginalFormat = sofBinary then
      ConvertProc(Input, Output)
    else
    begin
      if OriginalFormat = sofUnknown then
      begin   // text format may begin with "object", "inherited", or whitespace
        if Signature[1] in ['o','O','i','I',' ',#13,#11,#9] then
          OriginalFormat := sofText
        else    // not binary, not text... let it raise the exception
        begin
          ConvertProc(Input, Output);
          Exit;
        end;
      end;
      if OriginalFormat = sofText then begin
        if Output is TMemoryStream then
          TMemoryStream(Output).SetSize(Output.Position
                                        +(Input.Size - Input.Position));
        Output.CopyFrom(Input, Input.Size - Input.Position);
      end;
    end;
  end;
end;

procedure LRSObjectTextToBinary(Input, Output: TStream; Links: TLRPositionLinks);
var
  parser: {$IFDEF DisableWindowsUnicodeSupport}TParser{$ELSE}TUTF8Parser{$ENDIF};
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
  TokenStartPos: LongInt;

  procedure WriteShortString(const s: String);
  var
    Size: Integer;
  begin
    Size:=length(s);
    if Size>255 then Size:=255;
    Output.WriteByte(byte(Size));
    if Size > 0 then
      Output.Write(s[1], Size);
  end;

  procedure WriteLongString(const s: String);
  begin
    WriteLRSInteger(Output,Length(s));
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure WriteWideString(const s: WideString);
  begin
    WriteLRSInteger(Output,Length(s));
    if Length(s) > 0 then
      Output.Write(s[1], Length(s)*2);
  end;

  procedure WriteInteger(value: LongInt);
  begin
    if (value >= -128) and (value <= 127) then begin
      Output.WriteByte(Ord(vaInt8));
      Output.WriteByte(Byte(value));
    end else if (value >= -32768) and (value <= 32767) then begin
      Output.WriteByte(Ord(vaInt16));
      WriteLRSWord(Output,Word(value));
    end else begin
      Output.WriteByte(ord(vaInt32));
      WriteLRSInteger(Output,value);
    end;
  end;

  procedure WriteInt64(const Value: Int64);
  begin
    if (Value >= -$80000000) and (Value <= $7fffffff) then
      WriteInteger(Integer(Value))
    else begin
      Output.WriteByte(ord(vaInt64));
      WriteLRSInt64(Output,Value);
    end;
  end;

  procedure WriteIntegerStr(const s: string);
  begin
    if length(s)>7 then
      WriteInt64(StrToInt64(s))
    else
      WriteInteger(StrToInt(s));
  end;

  {$IFDEF DisableWindowsUnicodeSupport}
  function WideStringNeeded(const s: widestring): Boolean;
  var
    i: Integer;
  begin
    i:=length(s);
    while (i>=1) and (ord(s[i])<256) do dec(i);
    Result:=i>=1;
  end;

  function WideStrToAnsiStrWithoutConversion(const s: widestring): string;
  var
    i: Integer;
  begin
    SetLength(Result,Length(s){$IFDEF WideStringLenDoubled} div 2{$ENDIF});
    for i:=1 to length(Result) do
      Result[i]:=chr(ord(s[i]));
  end;

  function WideStrToShortStrWithoutConversion(const s: widestring): shortstring;
  var
    i: Integer;
  begin
    SetLength(Result,Length(s){$IFDEF WideStringLenDoubled} div 2{$ENDIF});
    for i:=1 to length(Result) do
      Result[i]:=chr(ord(s[i]));
  end;
  {$ENDIF}

  function ParserNextToken: Char;
  begin
    TokenStartPos:=Parser.SourcePos;
    Result:=Parser.NextToken;
    if Links<>nil then
      Links.SetPosition(TokenStartPos,Parser.SourcePos,Output.Position,true);
  end;
  
  procedure ProcessProperty; forward;

  {$if not declared(toWString)}
    const toWString = char(5);
  {$endif}

  procedure ProcessValue;
  
    procedure RaiseValueExpected;
    begin
      parser.Error('Value expected, but '+parser.TokenString+' found');
    end;
  
  var
    flt: Extended;
    stream: TMemoryStream;
    BinDataSize: LongInt;
    toStringBuf: String;
  begin
    if parser.TokenSymbolIs('END') then exit;
    if parser.TokenSymbolIs('OBJECT') then
      RaiseValueExpected;
    case parser.Token of
      toInteger:
        begin
          WriteIntegerStr(parser.TokenString);
          ParserNextToken;
        end;
      toFloat:
        begin
          Output.WriteByte(Ord(vaExtended));
          flt := Parser.TokenFloat;
          WriteLRSExtended(Output,flt);
          ParserNextToken;
        end;
      toString:
        begin
          toStringBuf := parser.TokenString;
          //DebugLn(['ProcessValue toStringBuf="',toStringBuf,'" ',dbgstr(toStringBuf)]);
          while ParserNextToken = '+' do
          begin
            ParserNextToken;   // Get next string fragment
            if not (parser.Token in [toString,toWString]) then
              parser.CheckToken(toString);
            toStringBuf := toStringBuf + parser.TokenString;
          end;
          if length(toStringBuf)<256 then begin
            //debugln('LRSObjectTextToBinary.ProcessValue WriteShortString');
            Output.WriteByte(Ord(vaString));
            WriteShortString(toStringBuf);
          end else begin
            //debugln('LRSObjectTextToBinary.ProcessValue WriteLongString');
            Output.WriteByte(Ord(vaLString));
            WriteLongString(toStringBuf);
          end;
        end;
      toWString:
        begin
          toStringBuf := parser.TokenString;
          //DebugLn(['ProcessValue toStringBuf="',toStringBuf,'" ',dbgstr(toStringBuf)]);
          while ParserNextToken = '+' do
          begin
            ParserNextToken;   // Get next string fragment
            if not (parser.Token in [toString,toWString]) then
              parser.CheckToken(toString);
            toStringBuf := toStringBuf + parser.TokenString;
          end;
          Output.WriteByte(Ord(vaWString));
          WriteWideString(UTF8Decode(toStringBuf));
        end;
      toSymbol:
        begin
          if CompareText(parser.TokenString, 'True') = 0 then
            Output.WriteByte(Ord(vaTrue))
          else if CompareText(parser.TokenString, 'False') = 0 then
            Output.WriteByte(Ord(vaFalse))
          else if CompareText(parser.TokenString, 'nil') = 0 then
            Output.WriteByte(Ord(vaNil))
          else
          begin
            Output.WriteByte(Ord(vaIdent));
            WriteShortString(parser.TokenComponentIdent);
          end;
          ParserNextToken;
        end;
      // Set
      '[':
        begin
          ParserNextToken;
          Output.WriteByte(Ord(vaSet));
          if parser.Token <> ']' then
            while True do
            begin
              parser.CheckToken(toSymbol);
              WriteShortString(parser.TokenString);
              ParserNextToken;
              if parser.Token = ']' then
                break;
              parser.CheckToken(',');
              ParserNextToken;
            end;
          Output.WriteByte(0);
          ParserNextToken;
        end;
      // List
      '(':
        begin
          Output.WriteByte(Ord(vaList));
          ParserNextToken;
          while parser.Token <> ')' do
            ProcessValue;
          Output.WriteByte(0);
          ParserNextToken;
        end;
      // Collection
      '<':
        begin
          ParserNextToken;
          Output.WriteByte(Ord(vaCollection));
          while parser.Token <> '>' do
          begin
            parser.CheckTokenSymbol('item');
            ParserNextToken;
            // ConvertOrder
            Output.WriteByte(Ord(vaList));
            while not parser.TokenSymbolIs('end') do
              ProcessProperty;
            ParserNextToken;   // Skip 'end'
            Output.WriteByte(0);
          end;
          Output.WriteByte(0);
          ParserNextToken;
        end;
      // Binary data
      '{':
        begin
          Output.WriteByte(Ord(vaBinary));
          stream := TMemoryStream.Create;
          try
            parser.HexToBinary(stream);
            BinDataSize:=integer(stream.Size);
            WriteLRSInteger(Output,BinDataSize);
            Output.Write(Stream.Memory^, BinDataSize);
            Stream.Position:=0;
            //debugln('LRSObjectTextToBinary binary data "',dbgMemStream(Stream,30),'"');
          finally
            stream.Free;
          end;
          ParserNextToken;
        end;
      else
        parser.Error('Invalid Property');
    end;
  end;

  procedure ProcessProperty;
  var
    name: String;
  begin
    // Get name of property
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
    while True do begin
      ParserNextToken;
      if parser.Token <> '.' then break;
      ParserNextToken;
      parser.CheckToken(toSymbol);
      name := name + '.' + parser.TokenString;
    end;
    WriteShortString(name);
    parser.CheckToken('=');
    ParserNextToken;
    ProcessValue;
  end;

  procedure ProcessObject;
  var
    Flags: Byte;
    ChildPos: Integer;
    ObjectName, ObjectType: String;
  begin
    if parser.TokenSymbolIs('OBJECT') then
      Flags :=0  { IsInherited := False }
    else if parser.TokenSymbolIs('INHERITED') then
      Flags := 1 { IsInherited := True; }
    else begin
      parser.CheckTokenSymbol('INLINE');
      Flags := 4;
    end;
    ParserNextToken;
    parser.CheckToken(toSymbol);
    if parser.TokenSymbolIs('END') then begin
      // 'object end': no name, no content
      // this is normally invalid, but Delphi can create this, so ignore it
      exit;
    end;
    ObjectName := '';
    ObjectType := parser.TokenString;
    ParserNextToken;
    ChildPos := 0;
    if parser.Token = ':' then begin
      ParserNextToken;
      parser.CheckToken(toSymbol);
      ObjectName := ObjectType;
      ObjectType := parser.TokenString;
      ParserNextToken;
      if parser.Token = '[' then begin
        ParserNextToken;
        ChildPos := parser.TokenInt;
        ParserNextToken;
        parser.CheckToken(']');
        ParserNextToken;
        Flags := Flags or 2;
      end;
    end;
    if Flags <> 0 then begin
      Output.WriteByte($f0 or Flags);
      if (Flags and ObjStreamMaskChildPos) <> 0 then
        WriteInteger(ChildPos);
    end;
    WriteShortString(ObjectType);
    WriteShortString(ObjectName);

    // Convert property list
    while not (parser.TokenSymbolIs('END') or
      parser.TokenSymbolIs('OBJECT') or
      parser.TokenSymbolIs('INHERITED') or
      parser.TokenSymbolIs('INLINE'))
    do
      ProcessProperty;
    Output.WriteByte(0);        // Terminate property list

    // Convert child objects
    while not parser.TokenSymbolIs('END') do ProcessObject;
    ParserNextToken;            // Skip end token
    Output.WriteByte(0);        // Terminate property list
  end;

var
  Count: Integer;
begin
  if Links<>nil then begin
    // sort links for LFM positions
    Links.Sort(true);
  end;
  parser := {$IFDEF DisableWindowsUnicodeSupport}TParser{$ELSE}TUTF8Parser{$ENDIF}.Create(Input);
  OldDecimalSeparator:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  OldThousandSeparator:=DefaultFormatSettings.ThousandSeparator;
  DefaultFormatSettings.ThousandSeparator:=',';
  try
    Count:=0;
    repeat
      Output.Write(FilerSignature[1], length(FilerSignature));
      ProcessObject;
      inc(Count);
    until parser.TokenString='';
    if Count>1 then
      Output.WriteByte(0);        // Terminate object list
  finally
    parser.Free;
    DefaultFormatSettings.DecimalSeparator:=OldDecimalSeparator;
    DefaultFormatSettings.ThousandSeparator:=OldThousandSeparator;
  end;
end;

procedure LRSObjectToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);
begin
  InternalLRSBinaryToText(Input, Output, OriginalFormat,
    @LRSObjectBinaryToText, FilerSignature);
end;

procedure LRSObjectResToText(Input, Output: TStream;
  var OriginalFormat: TLRSStreamOriginalFormat);
begin
  InternalLRSBinaryToText(Input, Output, OriginalFormat,
    @LRSObjectResourceToText, #255);
end;

procedure LRSObjectResourceToText(Input, Output: TStream);
begin
  Input.ReadResHeader;
  LRSObjectBinaryToText(Input, Output);
end;

procedure FormDataToText(FormStream, TextStream: TStream; aFormat: TLRSStreamOriginalFormat);
begin
  if aFormat = sofUnknown then
    aFormat := TestFormStreamFormat(FormStream);
  case aFormat of
    sofBinary:
      LRSObjectResourceToText(FormStream, TextStream);

    sofText:
      begin
        if TextStream is TMemoryStream then
          TMemoryStream(TextStream).SetSize(TextStream.Position+FormStream.Size);
        TextStream.CopyFrom(FormStream,FormStream.Size);
      end;

    else
      raise Exception.Create(rsInvalidFormObjectStream);
  end;
end;

function InitLazResourceComponent(Instance: TComponent;
  RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  var
    {$ifdef UseLRS}
    LazResource: TLResource;
    {$endif}
    {$ifdef UseRES}
    FPResource: TFPResourceHandle;
    {$endif}
    ResName: String;
    Stream: TStream;
    Reader: TReader;
    DestroyDriver: Boolean;
    Driver: TAbstractObjectReader;
  begin
    //DebugLn(['[InitComponent] ClassType=',ClassType.Classname,' Instance=',DbgsName(Instance),' RootAncestor=',DbgsName(RootAncestor),' ClassType.ClassParent=',DbgsName(ClassType.ClassParent)]);
    Result := False;
    if (ClassType = TComponent) or (ClassType = RootAncestor) then
      Exit;
    if Assigned(ClassType.ClassParent) then
      Result := InitComponent(ClassType.ClassParent);
      
    Stream := nil;
    ResName := ClassType.ClassName;
    
    {$ifdef UseLRS}
    LazResource := LazarusResources.Find(ResName);
    if (LazResource <> nil) and (LazResource.Value <> '') then
      Stream := TLazarusResourceStream.CreateFromHandle(LazResource);
    //DebugLn('[InitComponent] CompResource found for ',ClassType.Classname);
    {$endif}

    {$ifdef UseRES}
    if Stream = nil then
    begin
      FPResource := FindResourceLFM(ResName);
      if FPResource <> 0 then
        Stream := TLazarusResourceStream.CreateFromHandle(HInstance, FPResource);
    end;
    {$endif}
    
    if Stream = nil then
      Exit;
      
    try
      //DebugLn('Form Stream "',ClassType.ClassName,'"');
      //try
      DestroyDriver:=false;
      Reader := CreateLRSReader(Stream, DestroyDriver);
      try
        Reader.ReadRootComponent(Instance);
      finally
        Driver := Reader.Driver;
        Reader.Free;
        if DestroyDriver then
          Driver.Free;
      end;
      //except
      //  on E: Exception do begin
      //    DebugLn(Format(rsFormStreamingError,[ClassType.ClassName,E.Message]));
      //    exit;
      //  end;
      //end;
    finally
      Stream.Free;
    end;
    Result := True;
  end;

begin
  if Instance.ComponentState * [csLoading, csInline] <> []
  then begin
    // global loading not needed
    Result := InitComponent(Instance.ClassType);
  end
  else try
    BeginGlobalLoading;
    Result := InitComponent(Instance.ClassType);
    NotifyGlobalLoading;
  finally
    EndGlobalLoading;
  end;
end;

function CreateLRSReader(s: TStream; var DestroyDriver: boolean): TReader;
var
  p: Pointer;
  Driver: TAbstractObjectReader;
begin
  Result:=TReader.Create(s,4096);
  //If included Default translator LRSTranslator will be set
  if Assigned(LRSTranslator) then
    Result.OnReadStringProperty:=@(LRSTranslator.TranslateStringProperty);

  Result.OnPropertyNotFound := @(PropertiesToSkip.DoPropertyNotFound);

  DestroyDriver:=false;
  if Result.Driver.ClassType=LRSObjectReaderClass then
  begin
    TLRSObjectReader(Result.Driver).Reader:=Result;
    exit;
  end;
  // hack to set a write protected variable.
  // DestroyDriver:=true; TReader will free it
  Driver:=LRSObjectReaderClass.Create(s,4096);
  p:=@Result.Driver;
  Result.Driver.Free;
  TAbstractObjectReader(p^):=Driver;
  TLRSObjectReader(Driver).Reader:=Result;
end;

function CreateLRSWriter(s: TStream; var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=LRSObjectWriterClass.Create(s,4096);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
  TLRSObjectWriter(Driver).Writer:=Result;
end;

{ LRS format converter functions }

procedure ReverseBytes(p: Pointer; Count: integer);
var
  p1: PChar;
  p2: PChar;
  c: Char;
begin
  p1:=PChar(p);
  p2:=PChar(p)+Count-1;
  while p1<p2 do begin
    c:=p1^;
    p1^:=p2^;
    p2^:=c;
    inc(p1);
    dec(p2);
  end;
end;

procedure ReverseByteOrderInWords(p: PWord; Count: integer);
var
  i: Integer;
  w: Word;
begin
  for i:=0 to Count-1 do begin
    w:=p[i];
    w:=(w shr 8) or ((w and $ff) shl 8);
    p[i]:=w;
  end;
end;

function ConvertLRSExtendedToDouble(p: Pointer): Double;
type
  Ti386ExtendedReversed = packed record
    {$IFDEF FPC_BIG_ENDIAN}
    ExponentAndSign: word;
    Mantissa: qword;
    {$ELSE}
    Mantissa: qword;
    ExponentAndSign: word;
    {$ENDIF}
  end;
var
  e: Ti386ExtendedReversed;
  Exponent: word;
  ExponentAndSign: word;
  Mantissa: qword;
begin
  System.Move(p^,e,10);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@e,10);
  {$ENDIF}
  // i386 extended
  Exponent:=(e.ExponentAndSign and $7fff);
  if (Exponent>$4000+$3ff) or (Exponent<$4000-$400) then begin
    // exponent out of bounds
    Result:=0;
    exit;
  end;
  dec(Exponent,$4000-$400);
  ExponentAndSign:=Exponent or ((e.ExponentAndSign and $8000) shr 4);
  // i386 extended has leading 1, double has not (shl 1)
  // i386 has 64 bit, double has 52 bit (shr 12)
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
    {$IFDEF FPC_BIG_ENDIAN}
    // accessing Mantissa will couse trouble, copy it first
    System.Move(e.Mantissa, Mantissa, SizeOf(Mantissa));
    Mantissa := (Mantissa shl 1) shr 12;
    {$ELSE FPC_BIG_ENDIAN}
    Mantissa := (e.Mantissa shl 1) shr 12;
    {$ENDIF FPC_BIG_ENDIAN}
  {$ELSE FPC_REQUIRES_PROPER_ALIGNMENT}
  Mantissa := (e.Mantissa shl 1) shr 12;
  {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  // put together
  QWord(Result):=Mantissa or (qword(ExponentAndSign) shl 52);
end;

procedure ConvertEndianBigDoubleToLRSExtended(BigEndianDouble,
  LRSExtended: Pointer);
// Floats consists of a sign bit, some exponent bits and the mantissa bits
// A 0 is all bits 0
// not 0 has always a leading 1, which exponent is stored
// Single/Double does not save the leading 1, Extended does.
//
// Double is 8 bytes long, leftmost bit is sign,
// then 11 bit exponent based $400, then 52 bit mantissa without leading 1
//
// Extended is 10 bytes long, leftmost bit is sign,
// then 15 bit exponent based $4000, then 64 bit mantissa with leading 1
// EndianLittle means reversed byte order
var
  e: array[0..9] of byte;
  i: Integer;
  Exponent: Word;
  d: PByte;
begin
  d:=PByte(BigEndianDouble);
  // convert ppc double to i386 extended
  if (PCardinal(d)[0] or PCardinal(d)[1])=0 then begin
    // 0
    FillChar(LRSExtended^,10,#0);
  end else begin
    Exponent:=((d[0] and $7f) shl 4)+(d[1] shr 4);
    inc(Exponent,$4000-$400);
    if (d[0] and $80)>0 then
      // signed
      inc(Exponent,$8000);
    e[9]:=Exponent shr 8;
    e[8]:=Exponent and $ff;
    e[7]:=($80 or (d[1] shl 3) or (d[2] shr 5)) and $ff;
    for i:=3 to 7 do begin
      e[9-i]:=((d[i-1] shl 3) or (d[i] shr 5)) and $ff;
    end;
    e[1]:=(d[7] shl 3) and $ff;
    e[0]:=0;
    System.Move(e[0],LRSExtended^,10);
  end;
end;

procedure ConvertLEDoubleToLRSExtended(LEDouble, LRSExtended: Pointer);
type
  TMantissaWrap = record
    case boolean of
      True: (Q: QWord);
      False: (B: array[0..7] of Byte);
  end;

  TExpWrap = packed record
    Mantissa: TMantissaWrap;
    Exp: Word;
  end;

var
  Q: PQWord absolute LEDouble;
  C: PCardinal absolute LEDouble;
  W: PWord absolute LEDouble;
  E: ^TExpWrap absolute LRSExtended;
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Mantissa: TMantissaWrap;
  {$endif}
begin
  if W[3] and $7FF0 = $7FF0 // infinite or NaN
  then E^.Exp := $7FFF
  else E^.Exp := (W[3] and $7FFF) shr 4 - $3FF + $3FFF;
  E^.Exp := E^.Exp or (W[3] and $8000); // sign
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Mantissa.Q := (Q^ shl 11);
  Mantissa.B[7] := Mantissa.B[7] or $80; // add ignored 1
  System.Move(Mantissa, E^.Mantissa, 8);
  {$else}
  E^.Mantissa.Q := (Q^ shl 11);
  E^.Mantissa.B[7] := E^.Mantissa.B[7] or $80; // add ignored 1
  {$endif}
end;

function ReadLRSShortInt(s: TStream): shortint;
begin
  Result:=0;
  s.Read(Result,1);
end;

function ReadLRSByte(s: TStream): byte;
begin
  Result:=0;
  s.Read(Result,1);
end;

function ReadLRSWord(s: TStream): word;
begin
  Result:=0;
  s.Read(Result,2);
  {$IFDEF FPC_BIG_ENDIAN}
  Result:=((Result and $ff) shl 8) or (Result shr 8);
  {$ENDIF}
end;

function ReadLRSSmallInt(s: TStream): smallint;
begin
  Result:=0;
  {$IFDEF FPC_BIG_ENDIAN}
  Result:=smallint(ReadLRSWord(s));
  {$ELSE}
  s.Read(Result,2);
  {$ENDIF}
end;

function ReadLRSInteger(s: TStream): integer;
begin
  Result:=0;
  s.Read(Result,4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSCardinal(s: TStream): cardinal;
begin
  Result:=0;
  s.Read(Result,4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSInt64(s: TStream): int64;
begin
  Result:=0;
  s.Read(Result,8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSSingle(s: TStream): Single;
begin
  Result:=0;
  s.Read(Result,4);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$ENDIF}
end;

function ReadLRSDouble(s: TStream): Double;
begin
  Result:=0;
  s.Read(Result,8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSExtended(s: TStream): Extended;
begin
  Result:=0;
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    s.Read(Result,10);
    {$IFDEF FPC_BIG_ENDIAN}
    ReverseBytes(@Result,10);
    {$ENDIF}
  {$ELSE}
    // possible endian conversion is handled in ConvertLRSExtendedToDouble
    Result:=ReadLRSEndianLittleExtendedAsDouble(s);
  {$ENDIF}
end;

function ReadLRSCurrency(s: TStream): Currency;
begin
  Result:=0;
  s.Read(Result,8);
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$ENDIF}
end;

function ReadLRSWideString(s: TStream): WideString;
var
  Len: LongInt;
begin
  Len:=ReadLRSInteger(s);
  SetLength(Result,Len);
  if Len>0 then begin
    s.Read(Result[1],Len*2);
    {$IFDEF FPC_BIG_ENDIAN}
    ReverseByteOrderInWords(PWord(@Result[1]),Len);
    {$ENDIF}
  end;
end;

function ReadLRSEndianLittleExtendedAsDouble(s: TStream): Double;
var
  e: array[1..10] of byte;
begin
  s.Read(e,10);
  Result:=ConvertLRSExtendedToDouble(@e);
end;

function ReadLRSValueType(s: TStream): TValueType;
var
  b: byte;
begin
  s.Read(b,1);
  Result:=TValueType(b);
end;

function ReadLRSInt64MB(s: TStream): int64;
var
  v: TValueType;
begin
  v:=ReadLRSValueType(s);
  case v of
  vaInt8: Result:=ReadLRSShortInt(s);
  vaInt16: Result:=ReadLRSSmallInt(s);
  vaInt32: Result:=ReadLRSInteger(s);
  vaInt64: Result:=ReadLRSInt64(s);
  else
    raise EInOutError.Create('ordinal valuetype missing');
  end;
end;

procedure WriteLRSReversedWord(s: TStream; w: word);
begin
  w:=(w shr 8) or ((w and $ff) shl 8);
  s.Write(w,2);
end;

procedure WriteLRS4BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..3] of char;
  i: Integer;
begin
  for i:=0 to 3 do
    a[i]:=PChar(p)[3-i];
  s.Write(a[0],4);
end;

procedure WriteLRS8BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..7] of char;
  i: Integer;
begin
  for i:=0 to 7 do
    a[i]:=PChar(p)[7-i];
  s.Write(a[0],8);
end;

procedure WriteLRS10BytesReversed(s: TStream; p: Pointer);
var
  a: array[0..9] of char;
  i: Integer;
begin
  for i:=0 to 9 do
    a[i]:=PChar(p)[9-i];
  s.Write(a[0],10);
end;

procedure WriteLRSReversedWords(s: TStream; p: Pointer; Count: integer);
var
  w: Word;
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    w:=PWord(P)[i];
    w:=(w shr 8) or ((w and $ff) shl 8);
    s.Write(w,2);
  end;
end;

function FloatToLFMStr(const Value: extended; Precision, Digits: Integer): string;
var
  P: Integer;
  TooSmall, TooLarge: Boolean;
  DeletePos: LongInt;
begin
  Result:='';
  If (Precision = -1) or (Precision > 15) then Precision := 15;

  TooSmall := (Abs(Value) < 0.00001) and (Value>0.0);
  if TooSmall then begin
    P := 0;
    TooLarge := False;
  end
  else begin
    Str(Value:digits:precision, Result);
    P := Pos('.', Result);
    TooLarge :=(P > Precision + 1) or (Pos('E', Result)<>0);
  End;

  if TooSmall or TooLarge then begin
    // use exponential format
    Str(Value:Precision + 8, Result);
    P:=4;
    while (P>0) and (Digits < P) and (Result[Precision + 5] = '0') do begin
      if P<>1 then
        system.Delete(Result, Precision + 5, 1)
      else
        system.Delete(Result, Precision + 3, 3);
      Dec(P);
    end;
    if Result[1] = ' ' then
      System.Delete(Result, 1, 1);
    // Strip unneeded zeroes.
    P:=Pos('E',result)-1;
    If P>=0 then begin
      { delete superfluous +? }
      if result[p+2]='+' then
        system.Delete(Result,P+2,1);
      DeletePos:=p;
      while (DeletePos>1) and (Result[DeletePos]='0') do
        Dec(DeletePos);
      if (DeletePos>0) and (Result[DeletePos]=DefaultFormatSettings.DecimalSeparator) Then
        Dec(DeletePos);
      if (DeletePos<p) then
        system.Delete(Result,DeletePos,p-DeletePos);
    end;
  end
  else if (P<>0) then begin
    // we have a decimalseparator
    P := Length(Result);
    While (P>0) and (Result[P] = '0') Do
      Dec(P);
    If (P>0) and (Result[P]=DefaultFormatSettings.DecimalSeparator) Then
      Dec(P);
    SetLength(Result, P);
  end;
end;

function CompareLRPositionLinkWithLFMPosition(Item1, Item2: Pointer): integer;
var
  p1: Int64;
  p2: Int64;
begin
  p1:=PLRPositionLink(Item1)^.LFMPosition;
  p2:=PLRPositionLink(Item2)^.LFMPosition;
  if p1<p2 then
    Result:=1
  else if p1>p2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareLRPositionLinkWithLRSPosition(Item1, Item2: Pointer): integer;
var
  p1: Int64;
  p2: Int64;
begin
  p1:=PLRPositionLink(Item1)^.LRSPosition;
  p2:=PLRPositionLink(Item2)^.LRSPosition;
  if p1<p2 then
    Result:=1
  else if p1>p2 then
    Result:=-1
  else
    Result:=0;
end;

procedure RegisterPropertyToSkip(PersistentClass: TPersistentClass;
  const PropertyName, Note, HelpKeyWord: string);
begin
  PropertiesToSkip.Add(PersistentClass, PropertyName, Note, HelpKeyWord);
end;

procedure Register;
begin
  RegisterComponents('System',[TLazComponentQueue]);
end;

procedure WriteLRSNull(s: TStream; Count: integer);
var
  c: char;
  i: Integer;
begin
  c:=#0;
  for i:=0 to Count-1 do
    s.Write(c,1);
end;

procedure WriteLRSEndianBigDoubleAsEndianLittleExtended(s: TStream;
  EndBigDouble: PByte);
var
  e: array[0..9] of byte;
begin
  ConvertEndianBigDoubleToLRSExtended(EndBigDouble,@e);
  s.Write(e[0],10);
end;

procedure WriteLRSDoubleAsExtended(s: TStream; ADouble: PByte);
var
  e: array[0..9] of byte;
begin
  {$ifdef FPC_LITTLE_ENDIAN}
  ConvertLEDoubleToLRSExtended(ADouble,@e);
  {$else}
  ConvertEndianBigDoubleToLRSExtended(ADouble,@e);
  {$endif}
  s.Write(e[0],10);
end;


procedure WriteLRSSmallInt(s: TStream; const i: SmallInt);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i,2);
  {$ELSE}
  WriteLRSReversedWord(s,Word(i));
  {$ENDIF}
end;

procedure WriteLRSWord(s: TStream; const w: word);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(w,2);
  {$ELSE}
  WriteLRSReversedWord(s,w);
  {$ENDIF}
end;

procedure WriteLRSInteger(s: TStream; const i: integer);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@i);
  {$ENDIF}
end;

procedure WriteLRSCardinal(s: TStream; const c: cardinal);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(c,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@c);
  {$ENDIF}
end;

procedure WriteLRSSingle(s: TStream; const si: Single);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(si,4);
  {$ELSE}
  WriteLRS4BytesReversed(s,@si);
  {$ENDIF}
end;

procedure WriteLRSDouble(s: TStream; const d: Double);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(d,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@d);
  {$ENDIF}
end;

procedure WriteLRSExtended(s: TStream; const e: extended);
begin
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    {$IFDEF FPC_BIG_ENDIAN}
      WriteLRS10BytesReversed(s, @e);
    {$ELSE}
      s.Write(e,10);
    {$ENDIF}
  {$ELSE}
    WriteLRSDoubleAsExtended(s,pbyte(@e))
  {$ENDIF}
end;

procedure WriteLRSInt64(s: TStream; const i: int64);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(i,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@i);
  {$ENDIF}
end;

procedure WriteLRSCurrency(s: TStream; const c: Currency);
begin
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(c,8);
  {$ELSE}
  WriteLRS8BytesReversed(s,@c);
  {$ENDIF}
end;

procedure WriteLRSWideStringContent(s: TStream; const w: WideString);
var
  Size: Integer;
begin
  Size:=length(w);
  if Size=0 then exit;
  {$IFDEF FPC_LITTLE_ENDIAN}
  s.Write(w[1], Size * 2);
  {$ELSE}
  WriteLRSReversedWords(s,@w[1],Size);
  {$ENDIF}
end;

procedure WriteLRSInt64MB(s: TStream; const Value: int64);
var
  w: Word;
  i: Integer;
  b: Byte;
begin
  // Use the smallest possible integer type for the given value:
  if (Value >= -128) and (Value <= 127) then
  begin
    b:=byte(vaInt8);
    s.Write(b, 1);
    b:=byte(Value);
    s.Write(b, 1);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    b:=byte(vaInt16);
    s.Write(b, 1);
    w:=Word(Value);
    WriteLRSWord(s,w);
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    b:=byte(vaInt32);
    s.Write(b, 1);
    i:=Integer(Value);
    WriteLRSInteger(s,i);
  end else
  begin
    b:=byte(vaInt64);
    s.Write(b, 1);
    WriteLRSInt64(s,Value);
  end;
end;

{ TLRSObjectReader }

procedure TLRSObjectReader.Read(var Buf; Count: LongInt);
var
  CopyNow: LongInt;
  Dest: Pointer;
begin
  Dest := @Buf;
  while Count > 0 do
  begin
    if FBufPos >= FBufEnd then
    begin
      FBufEnd := FStream.Read(FBuffer^, FBufSize);
      if FBufEnd = 0 then
        raise EReadError.Create('Read Error');
      FBufPos := 0;
    end;
    CopyNow := FBufEnd - FBufPos;
    if CopyNow > Count then
      CopyNow := Count;
    Move(PChar(FBuffer)[FBufPos], Dest^, CopyNow);
    Inc(FBufPos, CopyNow);
    Dest:=Dest+CopyNow;
    Dec(Count, CopyNow);
  end;
end;

procedure TLRSObjectReader.SkipProperty;
begin
  { Skip property name, then the property value }
  ReadStr;
  SkipValue;
end;

procedure TLRSObjectReader.SkipSetBody;
begin
  while Length(ReadStr) > 0 do;
end;

procedure TLRSObjectReader.Push(ItemType: TLRSItemType; const AName: string;
                                Root: TComponent; PushCount: integer);
begin
  if FStackPointer=FStackCapacity then begin
    FStackCapacity:=FStackCapacity*2+10;
    ReAllocMem(FStack,SizeOf(TLRSORStackItem)*FStackCapacity);
    FillByte(FStack[FStackPointer],SizeOf(TLRSORStackItem)*(FStackCapacity-FStackPointer),0);
  end;
  //DebugLn(['TLRSObjectReader.Push AName=',AName,' Type=', GetEnumName(TypeInfo(TLRSItemType), Integer(ItemType)),' PushCount=',PushCount]);
  FStack[FStackPointer].Name:=AName;
  FStack[FStackPointer].ItemType:=ItemType;
  FStack[FStackPointer].Root:=Root;
  FStack[FStackPointer].PushCount:=PushCount;
  FStack[FStackPointer].ItemNr:=-1;
  inc(FStackPointer);
end;

procedure TLRSObjectReader.Pop;
var
  Item: PLRSORStackItem;
begin
  if FStackPointer=0 then
    raise Exception.Create('Error: TLRSObjectReader.Pop stack is empty');
  Item:=@FStack[FStackPointer-1];
  //DebugLn(['TLRSObjectReader.Pop AName=',Item^.Name,
  //        ' Type=',GetEnumName(TypeInfo(TLRSItemType), Integer(item^.ItemType)),
  //        ' PushCount=',item^.PushCount,' StackPtr=', FStackPointer]);
  if Item^.PushCount>1 then begin
    // stack item still needs more EndList
    dec(Item^.PushCount);
  end else begin
    // stack item is complete
    dec(FStackPointer);
  end;
end;

procedure TLRSObjectReader.ClearStack;
var
  i: Integer;
begin
  for i:=0 to FStackCapacity-1 do begin
    FStack[i].Name:='';
  end;
  ReAllocMem(FStack,0);
end;

function TLRSObjectReader.InternalReadValue: TValueType;
var
  b: byte;
begin
  Result := vaNull; { Necessary in FPC as TValueType is larger than 1 byte! }
  Read(b,1);
  Result:=TValueType(b);
end;

function TLRSObjectReader.ReadIntegerContent: integer;
begin
  Result:=0;
  Read(Result,4);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$endif}
end;

constructor TLRSObjectReader.Create(AStream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream := AStream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TLRSObjectReader.Destroy;
begin
  { Seek back the amount of bytes that we didn't process until now: }
  if Assigned(FStream) then
    FStream.Seek(Integer(FBufPos) - Integer(FBufEnd), soFromCurrent);

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  ClearStack;

  inherited Destroy;
end;

function TLRSObjectReader.ReadValue: TValueType;
begin
  Result := InternalReadValue;
  case Result of
    vaNull:
      begin
        EndPropertyIfOpen;
        // End previous element collection, list or component.
        if FStackPointer > 0 then
          Pop;
      end;
    vaCollection:
      begin
        Push(lrsitCollection);
      end;
    vaList:
      begin
        // Increase counter for next collection item.
        if (FStackPointer > 0) and (FStack[FStackPointer-1].ItemType = lrsitCollection) then
          Inc(FStack[FStackPointer-1].ItemNr);
        Push(lrsitList);
      end;
  end;
end;

function TLRSObjectReader.NextValue: TValueType;
begin
  Result := InternalReadValue;
  { We only 'peek' at the next value, so seek back to unget the read value: }
  Dec(FBufPos);
end;

procedure TLRSObjectReader.BeginRootComponent;
var
  Signature: TFilerSignature;
begin
  { Read filer signature }
  Signature:='1234';
  Read(Signature[1],length(Signature));
  if Signature <> FilerSignature then
    raise EReadError.Create('Invalid Filer Signature');
end;

procedure TLRSObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
var
  Prefix: Byte;
  ValueType: TValueType;
  ItemName: String;
  ItemRoot: TComponent;
begin
  { Every component can start with a special prefix: }
  Flags := [];
  if (Byte(NextValue) and $f0) = $f0 then
  begin
    Prefix := Byte(ReadValue);
    if (ObjStreamMaskInherited and Prefix)<>0 then
      Include(Flags,ffInherited);
    if (ObjStreamMaskInline and Prefix)<>0 then
      Include(Flags,ffInline);
    if (ObjStreamMaskChildPos and Prefix)<>0 then
    begin
      Include(Flags,ffChildPos);
      ValueType := ReadValue;
      case ValueType of
        vaInt8:
          AChildPos := ReadInt8;
        vaInt16:
          AChildPos := ReadInt16;
        vaInt32:
          AChildPos := ReadInt32;
        else
          PropValueError;
      end;
    end;
  end;

  CompClassName := ReadStr;
  CompName := ReadStr;

  // Top component is addressed by ClassName.
  if FStackPointer = 0 then
  begin
    ItemName := CompClassName;
    ItemRoot := nil;
  end
  else
  begin
    ItemName := CompName;
    if Assigned(Reader) then
      // Reader.LookupRoot is the current Root component.
      ItemRoot := Reader.LookupRoot
    else
      ItemRoot := nil;
  end;

  // A component has two lists: properties and childs, hence PopCount=2.
  Push(lrsitComponent, ItemName, ItemRoot, 2);
end;

function TLRSObjectReader.BeginProperty: String;
begin
  EndPropertyIfOpen;
  Result := ReadStr;
  Push(lrsitProperty, Result);
end;

procedure TLRSObjectReader.EndPropertyIfOpen;
begin
  // End previous property.
  if (FStackPointer > 0) and (FStack[FStackPointer-1].ItemType = lrsitProperty) then
    Pop;
end;

function TLRSObjectReader.GetStackPath: string;
var
  i: Integer;
  CurName: string;
  Item: PLRSORStackItem;
begin
  Result:='';

  for i:=0 to FStackPointer-1 do
  begin
    Item := @FStack[i];

    // Reader.Root is the top component in the module.
    if Assigned(Reader) and
       (Item^.ItemType = lrsitComponent) and
       (Item^.Root = Reader.Root) and
       (Item^.Root <> nil) then
    begin
      // Restart path from top component.
      Result := Item^.Root.ClassName;
    end;

    CurName:=Item^.Name;
    if CurName<>'' then begin
      if Result<>'' then Result:=Result+'.';
      Result:=Result+CurName;
    end;
    if Item^.ItemNr >= 0 then
      Result := Result + '[' + IntToStr(Item^.ItemNr) + ']';
  end;
end;

procedure TLRSObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  BinSize: LongInt;
begin
  BinSize:=ReadIntegerContent;
  DestData.Size := BinSize;
  Read(DestData.Memory^, BinSize);
end;

function TLRSObjectReader.ReadFloat: Extended;
{$ifndef FPC_HAS_TYPE_EXTENDED}
var
  e: array[1..10] of byte;
{$endif}
begin
  Result:=0;
  {$ifdef FPC_HAS_TYPE_EXTENDED}
    Read(Result, 10);
    {$ifdef FPC_BIG_ENDIAN}
      ReverseBytes(@Result, 10);
    {$endif FPC_BIG_ENDIAN}
  {$else FPC_HAS_TYPE_EXTENDED}
    Read(e, 10);
    Result := ConvertLRSExtendedToDouble(@e);
  {$endif FPC_HAS_TYPE_EXTENDED}
end;

function TLRSObjectReader.ReadSingle: Single;
begin
  Result:=0;
  Read(Result, 4);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$endif}
end;

function TLRSObjectReader.ReadCurrency: Currency;
begin
  Result:=0;
  Read(Result, 8);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$endif}
end;

function TLRSObjectReader.ReadDate: TDateTime;
begin
  Result:=0;
  Read(Result, 8);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$endif}
end;

function TLRSObjectReader.ReadIdent(ValueType: TValueType): String;
var
  b: Byte;
begin
  case ValueType of
    vaIdent:
      begin
        Read(b, 1);
        SetLength(Result, b);
        if ( b > 0 ) then
          Read(Result[1], b);
      end;
    vaNil:
      Result := 'nil';
    vaFalse:
      Result := 'False';
    vaTrue:
      Result := 'True';
    vaNull:
      Result := 'Null';
  else
    Result:='';

  end;
end;

function TLRSObjectReader.ReadInt8: ShortInt;
begin
  Result:=0;
  Read(Result, 1);
end;

function TLRSObjectReader.ReadInt16: SmallInt;
begin
  Result:=0;
  Read(Result, 2);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,2);
  {$endif}
end;

function TLRSObjectReader.ReadInt32: LongInt;
begin
  Result:=0;
  Read(Result, 4);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,4);
  {$endif}
end;

function TLRSObjectReader.ReadInt64: Int64;
begin
  Result:=0;
  Read(Result, 8);
  {$ifdef FPC_BIG_ENDIAN}
  ReverseBytes(@Result,8);
  {$endif}
end;

function TLRSObjectReader.ReadSet(EnumType: Pointer): Integer;
type
  tset = set of 0..31;
var
  OName: String;
  OValue: Integer;
begin
  try
    Result := 0;
    while True do
    begin
      OName := ReadStr;
      if Length(OName) = 0 then
        break;
      OValue := GetEnumValue(PTypeInfo(EnumType), OName);
      // Eg. "Options" is a set and can give an error when changing component type.
      // Do nothing on error (OValue = -1), was PropValueError;  (JuMa)
      if OValue >= 0 then
        include(tset(result),OValue);
    end;
  except
    SkipSetBody;
    raise;
  end;
end;

{$IF FPC_FULLVERSION >= 30000}
procedure TLRSObjectReader.ReadSignature;
begin
end;
{$ENDIF}

function TLRSObjectReader.ReadStr: String;
var
  b: Byte;
begin
  Read(b, 1);
  SetLength(Result, b);
  if b > 0 then
    Read(Result[1], b);
end;

function TLRSObjectReader.ReadString(StringType: TValueType): String;
var
  i: Integer;
  b: byte;
begin
  case StringType of
    vaString:
      begin
        Read(b, 1);
        i:=b;
      end;
    vaLString:
      i:=ReadIntegerContent;
  else
    raise Exception.Create('TLRSObjectReader.ReadString invalid StringType');
  end;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i);
end;

function TLRSObjectReader.ReadWideString: WideString;
var
  i: Integer;
begin
  i:=ReadIntegerContent;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i*2);
  //debugln('TLRSObjectReader.ReadWideString ',Result);
end;

function TLRSObjectReader.ReadUnicodeString: UnicodeString;
var
  i: Integer;
begin
  i:=ReadIntegerContent;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i*2);
  //debugln('TLRSObjectReader.ReadWideString ',Result);
end;

procedure TLRSObjectReader.SkipComponent(SkipComponentInfos: Boolean);
var
  Flags: TFilerFlags;
  Dummy: Integer;
  CompClassName, CompName: String;
begin
  if SkipComponentInfos then
    { Skip prefix, component class name and component object name }
    BeginComponent(Flags, Dummy, CompClassName, CompName);

  { Skip properties }
  while NextValue <> vaNull do
    SkipProperty;
  ReadValue;

  { Skip children }
  while NextValue <> vaNull do
    SkipComponent(True);
  ReadValue;
end;

procedure TLRSObjectReader.SkipValue;

  procedure SkipBytes(Count: LongInt);
  var
    Dummy: array[0..1023] of Byte;
    SkipNow: Integer;
  begin
    while Count > 0 do
    begin
      if Count > 1024 then
        SkipNow := 1024
      else
        SkipNow := Count;
      Read(Dummy, SkipNow);
      Dec(Count, SkipNow);
    end;
  end;

var
  Count: LongInt;
begin
  case ReadValue of
    vaNull, vaFalse, vaTrue, vaNil: ;
    vaList:
      begin
        while NextValue <> vaNull do
          SkipValue;
        ReadValue;
      end;
    vaInt8:
      SkipBytes(1);
    vaInt16:
      SkipBytes(2);
    vaInt32:
      SkipBytes(4);
    vaExtended:
      SkipBytes(10);
    vaString, vaIdent:
      ReadStr;
    vaBinary, vaLString:
      begin
        Count:=ReadIntegerContent;
        SkipBytes(Count);
      end;
    vaWString, vaUString:
      begin
        Count:=ReadIntegerContent;
        SkipBytes(Count*2);
      end;
    vaSet:
      SkipSetBody;
    vaCollection:
      begin
        while NextValue <> vaNull do
        begin
          { Skip the order value if present }
          if NextValue in [vaInt8, vaInt16, vaInt32] then
            SkipValue;
          SkipBytes(1);
          while NextValue <> vaNull do
            SkipProperty;
          ReadValue;
        end;
        ReadValue;
      end;
    vaSingle:
      SkipBytes(4);
    vaCurrency:
      SkipBytes(SizeOf(Currency));
    vaDate:
      SkipBytes(8);
    vaInt64:
      SkipBytes(8);
  else

  end;
end;

{ TLRSObjectWriter }

procedure TLRSObjectWriter.Push(ItemType: TLRSItemType; const AName: string;
                                Root: TComponent; PushCount: integer;
                                SkipIfEmpty: boolean);
begin
  if FStackPointer=FStackCapacity then begin
    FStackCapacity:=FStackCapacity*2+10;
    ReAllocMem(FStack,SizeOf(TLRSOWStackItem)*FStackCapacity);
    FillByte(FStack[FStackPointer],SizeOf(TLRSOWStackItem)*(FStackCapacity-FStackPointer),0);
  end;
  //if AName<>'' then DebugLn(['TLRSObjectWriter.Push AName=',AName, ' Type=', GetEnumName(TypeInfo(TLRSItemType), Integer(ItemType)),' PushCount=',PushCount,' SkipIfEmpty=',SkipIfEmpty]);
  FStack[FStackPointer].Name:=AName;
  FStack[FStackPointer].ItemType:=ItemType;
  FStack[FStackPointer].Root:=Root;
  FStack[FStackPointer].PushCount:=PushCount;
  FStack[FStackPointer].ItemNr:=-1;
  FStack[FStackPointer].SkipIfEmpty:=SkipIfEmpty;
  FStack[FStackPointer].BufCount:=0;
  if SkipIfEmpty then
    FStack[FStackPointer].State:=lrsowsisStarted
  else begin
    FlushStackToStream;
    FStack[FStackPointer].State:=lrsowsisDataWritten;
  end;
  inc(FStackPointer);
end;

procedure TLRSObjectWriter.EndHeader;
var
  Item: PLRSOWStackItem;
begin
  Item:=@FStack[FStackPointer-1];
  if Item^.State=lrsowsisStarted then
    Item^.State:=lrsowsisHeaderWritten;
end;

procedure TLRSObjectWriter.Pop(WriteNull: boolean);
var
  Item: PLRSOWStackItem;
begin
  if FStackPointer=0 then
    raise Exception.Create('Error: TLRSObjectWriter.Pop stack is empty');
  Item:=@FStack[FStackPointer-1];
  if Item^.PushCount>1 then begin
    // stack item still needs more EndList
    dec(Item^.PushCount);
    if WriteNull then begin
      if Item^.State=lrsowsisHeaderWritten then begin
        // no data yet, append EndList to header
        Item^.State:=lrsowsisStarted;
        WriteValue(vaNull);
        // wait again for data
        Item^.State:=lrsowsisHeaderWritten;
      end else begin
        // write EndList to stream
        WriteValue(vaNull);
      end;
    end;
  end else begin
    // stack item is complete
    dec(FStackPointer);
    //if Item^.BufCount>0 then DebugLn(['TLRSObjectWriter.Pop SKIPPED: ',Item^.Name]);
    if (Item^.State=lrsowsisDataWritten) and WriteNull then
      WriteValue(vaNull);
  end;
end;

procedure TLRSObjectWriter.ClearStack;
var
  i: Integer;
begin
  for i:=0 to FStackCapacity-1 do begin
    FStack[i].Name:='';
    ReAllocMem(FStack[i].Buffer,0);
  end;
  ReAllocMem(FStack,0);
end;

procedure TLRSObjectWriter.FlushStackToStream;
var
  i: Integer;
  Item: PLRSOWStackItem;
begin
  for i:=0 to FStackPointer-1 do begin
    Item:=@FStack[i];
    if Item^.State<>lrsowsisDataWritten then begin
      //DebugLn(['TLRSObjectWriter.Write FLUSH from stack to stream']);
      Item^.State:=lrsowsisDataWritten;
      WriteToStream(Item^.Buffer^,Item^.BufCount);
      Item^.BufCount:=0;
    end;
  end;
end;

procedure TLRSObjectWriter.WriteToStream(const Buffer; Count: Longint);
var
  CopyNow: LongInt;
  SourceBuf: PChar;
begin
  //DebugLn(['TLRSObjectWriter.WriteToStream ',dbgMemRange(@Buffer,Count,80)]);
  if Count<2*FBufSize then begin
    // write a small amount of data
    SourceBuf:=@Buffer;
    while Count > 0 do
    begin
      CopyNow := Count;
      if CopyNow > FBufSize - FBufPos then
        CopyNow := FBufSize - FBufPos;
      Move(SourceBuf^, PChar(FBuffer)[FBufPos], CopyNow);
      Dec(Count, CopyNow);
      Inc(FBufPos, CopyNow);
      SourceBuf:=SourceBuf+CopyNow;
      if FBufPos = FBufSize then
        FlushBuffer;
    end;
  end else begin
    // write a big amount of data
    if FBufPos>0 then
      FlushBuffer;
    FStream.WriteBuffer(Buffer, Count);
  end;
end;

procedure TLRSObjectWriter.FlushBuffer;
begin
  FStream.WriteBuffer(FBuffer^, FBufPos);
  FBufPos := 0;
end;

procedure TLRSObjectWriter.Write(const Buffer; Count: Longint);
var
  Item: PLRSOWStackItem;
begin
  if Count=0 then exit;
  if (FStackPointer>0) then
  begin
    Item:=@FStack[FStackPointer-1];
    case Item^.State of
    lrsowsisStarted:
      begin
        // store data on stack
        //DebugLn(['TLRSObjectWriter.Write STORE data on stack']);
        if Item^.BufCount+Count>Item^.BufCapacity then
        begin
          Item^.BufCapacity:=Item^.BufCount+Count+10;
          ReAllocMem(Item^.Buffer,Item^.BufCapacity);
        end;
        System.Move(Buffer,PByte(Item^.Buffer)[Item^.BufCount],Count);
        inc(Item^.BufCount,Count);
        exit;
      end;
    lrsowsisHeaderWritten:
      begin
        // flush header(s) from stack to stream
        FlushStackToStream;
      end;
    end;
  end;
  // write data to stream
  WriteToStream(Buffer,Count);
end;

procedure TLRSObjectWriter.WriteValue(Value: TValueType);
var
  b: byte;
begin
  b:=byte(Value);
  Write(b, 1);
end;

procedure TLRSObjectWriter.WriteStr(const Value: String);
var
  i: Integer;
  b: Byte;
begin
  i := Length(Value);
  if i > 255 then
    i := 255;
  b:=byte(i);
  Write(b,1);
  if i > 0 then
    Write(Value[1], i);
end;

procedure TLRSObjectWriter.WriteIntegerContent(i: integer);
begin
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@i,4);
  {$ENDIF}
  Write(i,4);
end;

procedure TLRSObjectWriter.WriteWordContent(w: word);
begin
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@w,2);
  {$ENDIF}
  Write(w,2);
end;

procedure TLRSObjectWriter.WriteInt64Content(i: int64);
begin
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@i,8);
  {$ENDIF}
  Write(i,8);
end;

procedure TLRSObjectWriter.WriteSingleContent(s: single);
begin
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@s,4);
  {$ENDIF}
  Write(s,4);
end;

procedure TLRSObjectWriter.WriteDoubleContent(d: Double);
begin
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@d,8);
  {$ENDIF}
  Write(d,8);
end;

procedure TLRSObjectWriter.WriteExtendedContent(e: Extended);
{$IFNDEF FPC_HAS_TYPE_EXTENDED}
var
  LRSExtended: array[1..10] of byte;
{$endif}
begin
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    {$IFDEF FPC_BIG_ENDIAN}
      ReverseBytes(@e,10);
    {$ENDIF}
      Write(e,10);
  {$ELSE}
    {$IFDEF FPC_BIG_ENDIAN}
      ConvertEndianBigDoubleToLRSExtended(@e,@LRSExtended);
    {$ELSE}
      ConvertLEDoubleToLRSExtended(@e,@LRSExtended);
    {$ENDIF}
      Write(LRSExtended,10);
  {$ENDIF}
end;

procedure TLRSObjectWriter.WriteCurrencyContent(c: Currency);
begin
  {$IFDEF FPC_BIG_ENDIAN}
  ReverseBytes(@c,8);
  {$ENDIF}
  Write(c,8);
end;

procedure TLRSObjectWriter.WriteWideStringContent(const ws: WideString);
begin
  if ws='' then exit;
  {$IFDEF FPC_BIG_ENDIAN}
  WriteWordsReversed(PWord(@ws[1]),length(ws));
  {$ELSE}
  Write(ws[1],length(ws)*2);
  {$ENDIF}
end;

procedure TLRSObjectWriter.WriteWordsReversed(p: PWord; Count: integer);
var
  i: Integer;
  w: Word;
begin
  for i:=0 to Count-1 do begin
    w:=p[i];
    w:=((w and $ff) shl 8) or (w and $ff);
    Write(w,2);
  end;
end;

procedure TLRSObjectWriter.WriteNulls(Count: integer);
var
  c: Char;
  i: Integer;
begin
  c:=#0;
  for i:=0 to Count-1 do Write(c,1);
end;

constructor TLRSObjectWriter.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TLRSObjectWriter.Destroy;
begin
  // Flush all data which hasn't been written yet
  if Assigned(FStream) then
    FlushBuffer;

  if Assigned(FBuffer) then begin
    FreeMem(FBuffer, FBufSize);
    FBuffer:=nil;
  end;

  ClearStack;

  inherited Destroy;
end;

procedure TLRSObjectWriter.BeginCollection;
begin
  //DebugLn(['TLRSObjectWriter.BeginCollection ',FStackPointer]);
  Push(lrsitCollection);
  WriteValue(vaCollection);
end;

procedure TLRSObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  Prefix: Byte;
  CanBeOmitted: boolean;
  ItemName: String;
  ItemRoot: TComponent;
begin
  //DebugLn(['TLRSObjectWriter.BeginComponent ',FStackPointer]);
  // an inherited child component can be omitted if empty
  CanBeOmitted:=(not WriteEmptyInheritedChilds)
            and (FStackPointer>0) and (ffInherited in Flags)
            and (not (ffChildPos in Flags));

  // Top component is addressed by ClassName.
  if FStackPointer = 0 then
  begin
    ItemName := Component.ClassName;
    ItemRoot := nil;
  end
  else
  begin
    ItemName := Component.Name;
    if Assigned(Writer) then
      // Writer.Root is the current Root component.
      ItemRoot := Writer.Root
    else
      ItemRoot := nil;
  end;

  // A component has two lists: properties and childs, hence PopCount=2.
  Push(lrsitComponent, ItemName, ItemRoot, 2, CanBeOmitted);

  if not FSignatureWritten then
  begin
    Write(FilerSignature[1], length(FilerSignature));
    FSignatureWritten := True;
  end;

  { Only write the flags if they are needed! }
  if Flags <> [] then
  begin
    Prefix := $f0;
    if ffInherited in Flags then
      inc(Prefix,ObjStreamMaskInherited);
    if ffInline in Flags then
      inc(Prefix,ObjStreamMaskInline);
    if ffChildPos in Flags then
      inc(Prefix,ObjStreamMaskChildPos);
    Write(Prefix, 1);
    if ffChildPos in Flags then
      WriteInteger(ChildPos);
  end;

  WriteStr(Component.ClassName);
  WriteStr(Component.Name);

  EndHeader;
end;

{$IF FPC_FULLVERSION >= 30000}
procedure TLRSObjectWriter.WriteSignature;
begin
end;
{$ENDIF}

procedure TLRSObjectWriter.BeginList;
begin
  // Increase counter for next collection item.
  if (FStackPointer > 0) and (FStack[FStackPointer-1].ItemType = lrsitCollection) then
    Inc(FStack[FStackPointer-1].ItemNr);
  //DebugLn(['TLRSObjectWriter.BeginList ',FStackPointer]);
  Push(lrsitList);
  WriteValue(vaList);
end;

procedure TLRSObjectWriter.EndList;
begin
  //DebugLn(['TLRSObjectWriter.EndList ',FStackPointer]);
  Pop(true);
end;

procedure TLRSObjectWriter.BeginProperty(const PropName: String);
begin
  //DebugLn(['TLRSObjectWriter.BeginProperty ',FStackPointer,' ',PropName]);
  Push(lrsitProperty, PropName);
  WriteStr(PropName);
end;

procedure TLRSObjectWriter.EndProperty;
begin
  //DebugLn(['TLRSObjectWriter.EndProperty ',FStackPointer]);
  Pop(false);
end;

function TLRSObjectWriter.GetStackPath: string;
var
  i: Integer;
  CurName: string;
  Item: PLRSOWStackItem;
begin
  Result:='';

  for i:=0 to FStackPointer-1 do
  begin
    Item := @FStack[i];

    // Writer.LookupRoot is the top component in the module.
    if Assigned(Writer) and
       (Item^.ItemType = lrsitComponent) and
       (Item^.Root = Writer.LookupRoot) and
       (Item^.Root <> nil) then
    begin
      // Restart path from top component.
      Result := Item^.Root.ClassName;
    end;

    CurName:=Item^.Name;
    if CurName<>'' then begin
      if Result<>'' then Result:=Result+'.';
      Result:=Result+CurName;
    end;
    if Item^.ItemNr >= 0 then
      Result := Result + '[' + IntToStr(Item^.ItemNr) + ']';
  end;
end;

procedure TLRSObjectWriter.WriteBinary(const Buffer; Count: LongInt);
begin
  WriteValue(vaBinary);
  WriteIntegerContent(Count);
  Write(Buffer, Count);
end;

procedure TLRSObjectWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    WriteValue(vaTrue)
  else
    WriteValue(vaFalse);
end;

procedure TLRSObjectWriter.WriteFloat(const Value: Extended);
begin
  WriteValue(vaExtended);
  WriteExtendedContent(Value);
end;

procedure TLRSObjectWriter.WriteSingle(const Value: Single);
begin
  WriteValue(vaSingle);
  WriteSingleContent(Value);
end;

procedure TLRSObjectWriter.WriteCurrency(const Value: Currency);
begin
  WriteValue(vaCurrency);
  WriteCurrencyContent(Value);
end;

procedure TLRSObjectWriter.WriteDate(const Value: TDateTime);
begin
  WriteValue(vaDate);
  WriteDoubleContent(Value);
end;

procedure TLRSObjectWriter.WriteIdent(const Ident: string);
begin
  { Check if Ident is a special identifier before trying to just write
    Ident directly }
  if UpperCase(Ident) = 'NIL' then
    WriteValue(vaNil)
  else if UpperCase(Ident) = 'FALSE' then
    WriteValue(vaFalse)
  else if UpperCase(Ident) = 'TRUE' then
    WriteValue(vaTrue)
  else if UpperCase(Ident) = 'NULL' then
    WriteValue(vaNull) else
  begin
    WriteValue(vaIdent);
    WriteStr(Ident);
  end;
end;

procedure TLRSObjectWriter.WriteInteger(Value: Int64);
var
  w: Word;
  i: Integer;
  b: Byte;
begin
  //debugln('TLRSObjectWriter.WriteInteger Value=',Value);
  // Use the smallest possible integer type for the given value:
  if (Value >= -128) and (Value <= 127) then
  begin
    WriteValue(vaInt8);
    b:=Byte(Value);
    Write(b, 1);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    WriteValue(vaInt16);
    w:=Word(Value);
    WriteWordContent(w);
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    WriteValue(vaInt32);
    i:=Integer(Value);
    WriteIntegerContent(i);
  end else
  begin
    WriteValue(vaInt64);
    WriteInt64Content(Value);
  end;
end;

procedure TLRSObjectWriter.WriteMethodName(const Name: String);
begin
  if Length(Name) > 0 then
  begin
    WriteValue(vaIdent);
    WriteStr(Name);
  end else
    WriteValue(vaNil);
end;

procedure TLRSObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
type
  tset = set of 0..31;
var
  i: Integer;
begin
  WriteValue(vaSet);
  for i := 0 to 31 do
  begin
    if (i in tset(Value)) then
      WriteStr(GetEnumName(PTypeInfo(SetType), i));
  end;
  WriteStr('');
end;

procedure TLRSObjectWriter.WriteString(const Value: String);
var
  i: Integer;
  b: Byte;
begin
  i := Length(Value);
  if i <= 255 then
  begin
    WriteValue(vaString);
    b:=byte(i);
    Write(b, 1);
  end else
  begin
    WriteValue(vaLString);
    WriteIntegerContent(i);
  end;
  if i > 0 then
    Write(Value[1], i);
end;

procedure TLRSObjectWriter.WriteWideString(const Value: WideString);
var
  i: Integer;
begin
  WriteValue(vaWString);
  i := Length(Value);
  WriteIntegerContent(i);
  WriteWideStringContent(Value);
end;

procedure TLRSObjectWriter.WriteUnicodeString(const Value: UnicodeString);
var
  i: Integer;
begin
  WriteValue(vaUString);
  i := Length(Value);
  WriteIntegerContent(i);
  WriteWideStringContent(Value);
end;

{ TLRPositionLinks }

function TLRPositionLinks.GetLFM(Index: integer): Int64;
begin
  Result:=PLRPositionLink(FItems[Index])^.LFMPosition;
end;

function TLRPositionLinks.GetData(Index: integer): Pointer;
begin
  Result:=PLRPositionLink(FItems[Index])^.Data;
end;

function TLRPositionLinks.GetLRS(Index: integer): Int64;
begin
  Result:=PLRPositionLink(FItems[Index])^.LRSPosition;
end;

procedure TLRPositionLinks.SetCount(const AValue: integer);
var
  i: LongInt;
  Item: PLRPositionLink;
begin
  if FCount=AValue then exit;
  // free old items
  for i:=AValue to FCount-1 do begin
    Item:=PLRPositionLink(FItems[i]);
    Dispose(Item);
  end;
  // create new items
  FItems.Count:=AValue;
  for i:=FCount to AValue-1 do begin
    New(Item);
    Item^.LFMPosition:=-1;
    Item^.LRSPosition:=-1;
    Item^.Data:=nil;
    FItems[i]:=Item;
  end;
  FCount:=AValue;
end;

procedure TLRPositionLinks.SetData(Index: integer; const AValue: Pointer);
begin
  PLRPositionLink(FItems[Index])^.Data:=AValue;
end;

procedure TLRPositionLinks.SetLFM(Index: integer; const AValue: Int64);
begin
  PLRPositionLink(FItems[Index])^.LFMPosition:=AValue;
end;

procedure TLRPositionLinks.SetLRS(Index: integer; const AValue: Int64);
begin
  PLRPositionLink(FItems[Index])^.LRSPosition:=AValue;
end;

constructor TLRPositionLinks.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TLRPositionLinks.Destroy;
begin
  Count:=0;
  FItems.Free;
  inherited Destroy;
end;

procedure TLRPositionLinks.Clear;
begin
  Count:=0;
end;

procedure TLRPositionLinks.Sort(LFMPositions: Boolean);
begin
  if LFMPositions then
    FItems.Sort(@CompareLRPositionLinkWithLFMPosition)
  else
    FItems.Sort(@CompareLRPositionLinkWithLRSPosition)
end;

function TLRPositionLinks.IndexOf(const Position: int64; LFMPositions: Boolean
  ): integer;
var
  l, r, m: integer;
  p: Int64;
begin
  // binary search for the line
  l:=0;
  r:=FCount-1;
  while r>=l do begin
    m:=(l+r) shr 1;
    if LFMPositions then
      p:=PLRPositionLink(FItems[m])^.LFMPosition
    else
      p:=PLRPositionLink(FItems[m])^.LRSPosition;
    if p>Position then begin
      // too high, search lower
      r:=m-1;
    end else if p<Position then begin
      // too low, search higher
      l:=m+1;
    end else begin
      // position found
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
end;

function TLRPositionLinks.IndexOfRange(const FromPos, ToPos: int64;
  LFMPositions: Boolean): integer;
var
  l, r, m: integer;
  p: Int64;
  Item: PLRPositionLink;
begin
  // binary search for the line
  l:=0;
  r:=FCount-1;
  while r>=l do begin
    m:=(l+r) shr 1;
    Item:=PLRPositionLink(FItems[m]);
    if LFMPositions then
      p:=Item^.LFMPosition
    else
      p:=Item^.LRSPosition;
    if p>=ToPos then begin
      // too high, search lower
      r:=m-1;
    end else if p<FromPos then begin
      // too low, search higher
      l:=m+1;
    end else begin
      // position found
      Result:=m;
      exit;
    end;
  end;
  Result:=-1;
end;

procedure TLRPositionLinks.SetPosition(const FromPos, ToPos, MappedPos: int64;
  LFMtoLRSPositions: Boolean);
var
  i: LongInt;
begin
  i:=IndexOfRange(FromPos,ToPos,LFMtoLRSPositions);
  if i>=0 then
    if LFMtoLRSPositions then
      PLRPositionLink(FItems[i])^.LRSPosition:=MappedPos
    else
      PLRPositionLink(FItems[i])^.LFMPosition:=MappedPos;
end;

procedure TLRPositionLinks.Add(const LFMPos, LRSPos: Int64; AData: Pointer);
var
  Item: PLRPositionLink;
begin
  Count:=Count+1;
  Item:=PLRPositionLink(FItems[Count-1]);
  Item^.LFMPosition:=LFMPos;
  Item^.LRSPosition:=LRSPos;
  Item^.Data:=AData;
end;

{ TCustomLazComponentQueue }

function TCustomLazComponentQueue.ReadComponentSize(out ComponentSize,
  SizeLength: int64): Boolean;
// returns true if there are enough bytes to read the ComponentSize
//   and returns the ComponentSize
//   and returns the size (SizeLength) needed to store the ComponentSize

  procedure ReadBytes(var p);
  var a: array[1..9] of byte;
  begin
    FQueue.Top(a[1],1+SizeLength);
    System.Move(a[2],p,SizeLength);
    {$IFDEF FPC_BIG_ENDIAN}
    ReverseBytes(@p,SizeLength);
    {$ENDIF}
  end;

var
  v8: ShortInt;
  v16: SmallInt;
  v32: Integer;
  v64: int64;
  vt: TValueType;
begin
  Result:=false;
  // check if there are enough bytes
  if (FQueue.Size<2) then exit;
  FQueue.Top(vt,1);
  case vt of
  vaInt8: SizeLength:=1;
  vaInt16: SizeLength:=2;
  vaInt32: SizeLength:=4;
  vaInt64: SizeLength:=8;
  else
    raise EInOutError.Create('Invalid size type');
  end;
  if FQueue.Size<1+SizeLength then exit; // need more data
  // read the ComponentSize
  Result:=true;
  case vt of
  vaInt8:
    begin
      ReadBytes(v8);
      ComponentSize:=v8;
    end;
  vaInt16:
    begin
      ReadBytes(v16);
      ComponentSize:=v16;
    end;
  vaInt32:
    begin
      ReadBytes(v32);
      ComponentSize:=v32;
    end;
  vaInt64:
    begin
      ReadBytes(v64);
      ComponentSize:=v64;
    end;
  end;
  inc(SizeLength);
  if ComponentSize<0 then
    raise EInOutError.Create('Size of data in queue is negative');
end;

constructor TCustomLazComponentQueue.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FQueue:=TDynamicDataQueue.Create;
end;

destructor TCustomLazComponentQueue.Destroy;
begin
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TCustomLazComponentQueue.Clear;
begin
  FQueue.Clear;
end;

function TCustomLazComponentQueue.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=FQueue.Push(Buffer,Count);
end;

function TCustomLazComponentQueue.CopyFrom(AStream: TStream; Count: Longint
  ): Longint;
begin
  Result:=FQueue.Push(AStream,Count);
end;

function TCustomLazComponentQueue.HasComponent: Boolean;
var
  ComponentSize, SizeLength: int64;
begin
  if not ReadComponentSize(ComponentSize,SizeLength) then exit(false);
  Result:=FQueue.Size-SizeLength>=ComponentSize;
end;

function TCustomLazComponentQueue.ReadComponent(var AComponent: TComponent;
  NewOwner: TComponent): Boolean;
var
  ComponentSize, SizeLength: int64;
  AStream: TMemoryStream;
begin
  Result:=false;
  if not ReadComponentSize(ComponentSize,SizeLength) then exit;
  if (FQueue.Size-SizeLength<ComponentSize) then exit;
  // a complete component is in the buffer -> copy it to a stream
  AStream:=TMemoryStream.Create;
  try
    // copy component to stream
    AStream.Size:=SizeLength+ComponentSize;
    FQueue.Pop(AStream,SizeLength+ComponentSize);
    // create/read the component
    AStream.Position:=SizeLength;
    ReadComponentFromBinaryStream(AStream,AComponent,
                                  OnFindComponentClass,NewOwner);
  finally
    AStream.Free;
  end;
  Result:=true;
end;

function TCustomLazComponentQueue.ConvertComponentAsString(AComponent: TComponent
  ): string;
var
  AStream: TMemoryStream;
  ComponentSize: Int64;
  LengthSize: Int64;
begin
  // write component to stream
  AStream:=TMemoryStream.Create;
  try
    WriteComponentAsBinaryToStream(AStream,AComponent);

    ComponentSize:=AStream.Size;
    WriteLRSInt64MB(AStream,ComponentSize);
    LengthSize:=AStream.Size-ComponentSize;
    //debugln('TCustomLazComponentQueue.ConvertComponentAsString ComponentSize=',ComponentSize,' LengthSize=',LengthSize);

    SetLength(Result,AStream.Size);
    // write size
    AStream.Position:=ComponentSize;
    AStream.Read(Result[1],LengthSize);
    //debugln('TCustomLazComponentQueue.ConvertComponentAsString ',hexstr(ord(Result[1]),2),' ',hexstr(ord(Result[2]),2),' ',hexstr(ord(Result[3]),2),' ',hexstr(ord(Result[4]),2));
    // write component
    AStream.Position:=0;
    AStream.Read(Result[LengthSize+1],ComponentSize);
  finally
    AStream.Free;
  end;
end;

{ TLazarusResourceStream }

procedure TLazarusResourceStream.Initialize(Name, ResType: PChar);
begin
  if ResType <> nil then
    FLRes := LazarusResources.Find(Name, ResType)
  else
    FLRes := LazarusResources.Find(Name);
    
  if FLRes = nil then
    raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  SetPointer(PChar(FLRes.Value), Length(FLRes.Value));
end;

constructor TLazarusResourceStream.Create(const ResName: string; ResType: PChar);
begin
  inherited Create;
  Initialize(PChar(ResName), ResType);
end;

constructor TLazarusResourceStream.CreateFromID(ResID: Integer; ResType: PChar);
begin
  inherited Create;
  Initialize(PChar(PtrInt(ResID)), ResType);
end;

constructor TLazarusResourceStream.CreateFromHandle(AHandle: TLResource);
begin
  inherited Create;
  FLRes := AHandle;
  SetPointer(PChar(FLRes.Value), Length(FLRes.Value));
end;

{$ifdef UseRes}
constructor TLazarusResourceStream.CreateFromHandle(Instance: TFPResourceHMODULE; AHandle: TFPResourceHandle);
begin
  FPRes := LoadResource(Instance, AHandle);
  if FPRes <> 0 then
    SetPointer(LockResource(FPRes), SizeOfResource(Instance, AHandle));
end;
{$endif}

destructor TLazarusResourceStream.Destroy;
begin
{$ifdef UseRES}
  if FPRes <> 0 then
  begin
    UnlockResource(FPRes);
    FreeResource(FPRes);
  end;
{$endif}
  inherited Destroy;
end;

function TLazarusResourceStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  raise EStreamError.Create(SCantWriteResourceStreamError);
end;

const
  ParseBufSize     = 4096;
  LastSpecialToken = 5;

  TokNames : array[0..LastSpecialToken] of string =
  (
    'EOF',
    'Symbol',
    'String',
    'Integer',
    'Float',
    'WideString'
  );

function TUTF8Parser.GetTokenName(aTok: char): string;
begin
  if ord(aTok) <= LastSpecialToken then
    Result:=TokNames[ord(aTok)]
  else Result:=aTok;
end;

procedure TUTF8Parser.LoadBuffer;
var newread : integer;
begin
  newread:=fStream.Read(fBuf[0],ParseBufSize);
  fBuf[newread]:=#0;
  fLineStart:=fLineStart-fPos; // column = fPos - fLineStart + 1
  fPos:=0;
  fBufLen:=newread;
  fEofReached:=newread=0;
end;

procedure TUTF8Parser.CheckLoadBuffer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  if fBuf[fPos]<>#0 then exit;
  if fPos<fBufLen then begin
    // skip #0
    repeat
      inc(fPos);
      if fBuf[fPos]<>#0 then exit;
    until (fPos=fBufLen);
  end;
  LoadBuffer;
end;

procedure TUTF8Parser.ProcessChar; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  fLastTokenStr:=fLastTokenStr+fBuf[fPos];
  inc(fPos);
  CheckLoadBuffer;
end;

function TUTF8Parser.IsNumber: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=fBuf[fPos] in ['0'..'9'];
end;

function TUTF8Parser.IsHexNum: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=fBuf[fPos] in ['0'..'9','A'..'F','a'..'f'];
end;

function TUTF8Parser.IsAlpha: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=fBuf[fPos] in ['_','A'..'Z','a'..'z'];
end;

function TUTF8Parser.IsAlphaNum: boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  Result:=IsAlpha or IsNumber;
end;

function TUTF8Parser.GetHexValue(c: char): byte; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
begin
  case c of
    '0'..'9' : Result:=ord(c)-$30;
    'A'..'F' : Result:=ord(c)-$37; //-$41+$0A
    'a'..'f' : Result:=ord(c)-$57; //-$61+$0A
  end;
end;

function TUTF8Parser.GetAlphaNum: string;
begin
  if not IsAlpha then
    ErrorFmt(SParExpected,[GetTokenName(toSymbol)]);
  Result:='';
  while IsAlphaNum do
  begin
    Result:=Result+fBuf[fPos];
    inc(fPos);
    CheckLoadBuffer;
  end;
end;

procedure TUTF8Parser.HandleNewLine;
begin
  if fBuf[fPos]=#13 then //CR
  begin
    inc(fPos);
    CheckLoadBuffer;
    if fBuf[fPos]=#10 then inc(fPos); //CR LF
  end
  else
    inc(fPos); //LF
  CheckLoadBuffer;
  inc(fSourceLine);
  fLineStart:=fPos;
end;

procedure TUTF8Parser.SkipSpaces;
begin
  while fBuf[fPos] in [' ',#9] do begin
    inc(fPos);
    CheckLoadBuffer;
  end;
end;

procedure TUTF8Parser.SkipWhitespace;
begin
  while true do
  begin
    case fBuf[fPos] of
      ' ',#9  : SkipSpaces;
      #10,#13 : HandleNewLine
      else break;
    end;
  end;
end;

procedure TUTF8Parser.HandleEof;
begin
  fToken:=toEOF;
  fLastTokenStr:='';
end;

procedure TUTF8Parser.HandleAlphaNum;
begin
  fLastTokenStr:=GetAlphaNum;
  fToken:=toSymbol;
end;

procedure TUTF8Parser.HandleNumber;
type
  floatPunct = (fpDot,fpE);
  floatPuncts = set of floatPunct;
var
  allowed : floatPuncts;
begin
  fLastTokenStr:='';
  while IsNumber do
    ProcessChar;
  fToken:=toInteger;
  if (fBuf[fPos] in ['.','e','E']) then
  begin
    fToken:=toFloat;
    allowed:=[fpDot,fpE];
    while (fBuf[fPos] in ['.','e','E','0'..'9']) do
    begin
      case fBuf[fPos] of
        '.'     : if fpDot in allowed then Exclude(allowed,fpDot) else break;
        'E','e' : if fpE in allowed then
                  begin
                    allowed:=[];
                    ProcessChar;
                    if (fBuf[fPos] in ['+','-']) then ProcessChar;
                    if not (fBuf[fPos] in ['0'..'9']) then
                      ErrorFmt(SParInvalidFloat,[fLastTokenStr+fBuf[fPos]]);
                  end
                  else break;
      end;
      ProcessChar;
    end;
  end;
  if (fBuf[fPos] in ['s','S','d','D','c','C']) then //single, date, currency
  begin
    fFloatType:=fBuf[fPos];
    inc(fPos);
    CheckLoadBuffer;
    fToken:=toFloat;
  end
  else fFloatType:=#0;
end;

procedure TUTF8Parser.HandleHexNumber;
var valid : boolean;
begin
  fLastTokenStr:='$';
  inc(fPos);
  CheckLoadBuffer;
  valid:=false;
  while IsHexNum do
  begin
    valid:=true;
    ProcessChar;
  end;
  if not valid then
    ErrorFmt(SParInvalidInteger,[fLastTokenStr]);
  fToken:=toInteger;
end;

function TUTF8Parser.HandleQuotedString: string;
begin
  Result:='';
  inc(fPos);
  CheckLoadBuffer;
  while true do
  begin
    case fBuf[fPos] of
      #0     : ErrorStr(SParUnterminatedString);
      #13,#10 : ErrorStr(SParUnterminatedString);
      ''''   : begin
                 inc(fPos);
                 CheckLoadBuffer;
                 if fBuf[fPos]<>'''' then exit;
               end;
    end;
    Result:=Result+fBuf[fPos];
    inc(fPos);
    CheckLoadBuffer;
  end;
end;

function TUTF8Parser.HandleDecimalString(var IsWideString: Boolean): string;
var i : integer;
begin
  Result:='';
  inc(fPos);
  CheckLoadBuffer;
  while IsNumber do
  begin
    Result:=Result+fBuf[fPos];
    inc(fPos);
    CheckLoadBuffer;
  end;
  if not TryStrToInt(Result,i) then
    i:=0;
  if i > 255 then begin
    Result:=UnicodeToUTF8(i); // widestring
    IsWideString:=true;
  end else if i > 127 then
    Result:=SysToUTF8(chr(i)) // windows codepage
  else
    Result:=chr(i); // ascii, does not happen
end;

procedure TUTF8Parser.HandleString;
var
  IsWideString: Boolean;
begin
  fLastTokenStr:='';
  IsWideString := false;
  while true do begin
    case fBuf[fPos] of
      '''' : fLastTokenStr:=fLastTokenStr+HandleQuotedString;
      '#'  : fLastTokenStr:=fLastTokenStr+HandleDecimalString(IsWideString);
      else break;
    end;
  end;
  if IsWideString then
    fToken:=toWString
  else
    fToken:=Classes.toString;
end;

procedure TUTF8Parser.HandleMinus;
begin
  inc(fPos);
  CheckLoadBuffer;
  if IsNumber then
  begin
    HandleNumber;
    fLastTokenStr:='-'+fLastTokenStr;
  end
  else
  begin
    fToken:='-';
    fLastTokenStr:=fToken;
  end;
end;

procedure TUTF8Parser.HandleUnknown;
begin
  fToken:=fBuf[fPos];
  fLastTokenStr:=fToken;
  inc(fPos);
  CheckLoadBuffer;
end;

constructor TUTF8Parser.Create(Stream: TStream);
begin
  fStream:=Stream;
  fBuf:=GetMem(ParseBufSize+1);
  fBufLen:=0;
  fPos:=0;
  fLineStart:=0;
  fSourceLine:=1;
  fEofReached:=false;
  fLastTokenStr:='';
  fFloatType:=#0;
  fToken:=#0;
  LoadBuffer;
  NextToken;
end;

destructor TUTF8Parser.Destroy;
begin
  fStream.Position:=SourcePos;
  FreeMem(fBuf);
end;

procedure TUTF8Parser.CheckToken(T: Char);
begin
  if fToken<>T then
    ErrorFmt(SParWrongTokenType,[GetTokenName(T),GetTokenName(fToken)]);
end;

procedure TUTF8Parser.CheckTokenSymbol(const S: string);
begin
  CheckToken(toSymbol);
  if CompareText(fLastTokenStr,S)<>0 then
    ErrorFmt(SParWrongTokenSymbol,[s,fLastTokenStr]);
end;

procedure TUTF8Parser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TUTF8Parser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident,Args));
end;

procedure TUTF8Parser.ErrorStr(const Message: string);
begin

  raise EParserError.CreateFmt(Message+SParLocInfo,[SourceLine,SourceColumn,SourcePos]);
end;

procedure TUTF8Parser.HexToBinary(Stream: TStream);
var outbuf : array[0..ParseBufSize-1] of byte;
    b : byte;
    i : integer;
begin
  i:=0;
  SkipWhitespace;
  while IsHexNum do
  begin
    b:=(GetHexValue(fBuf[fPos]) shl 4);
    inc(fPos);
    CheckLoadBuffer;
    if not IsHexNum then
      Error(SParUnterminatedBinValue);
    b:=b or GetHexValue(fBuf[fPos]);
    inc(fPos);
    CheckLoadBuffer;
    outbuf[i]:=b;
    inc(i);
    if i>=ParseBufSize then
    begin
      Stream.WriteBuffer(outbuf[0],i);
      i:=0;
    end;
    SkipWhitespace;
  end;
  if i>0 then
    Stream.WriteBuffer(outbuf[0],i);
  NextToken;
end;

function TUTF8Parser.NextToken: Char;

begin
  SkipWhiteSpace;
  if fEofReached then
    HandleEof
  else
    case fBuf[fPos] of
      '_','A'..'Z','a'..'z' : HandleAlphaNum;
      '$'                   : HandleHexNumber;
      '-'                   : HandleMinus;
      '0'..'9'              : HandleNumber;
      '''','#'              : HandleString
      else
        HandleUnknown;
    end;
  Result:=fToken;
end;

function TUTF8Parser.SourcePos: Longint;
begin
  Result:=fStream.Position-fBufLen+fPos;
end;

function TUTF8Parser.TokenComponentIdent: string;
begin
  if fToken<>toSymbol then
    ErrorFmt(SParExpected,[GetTokenName(toSymbol)]);
  CheckLoadBuffer;
  while fBuf[fPos]='.' do
  begin
    ProcessChar;
    fLastTokenStr:=fLastTokenStr+GetAlphaNum;
  end;
  Result:=fLastTokenStr;
end;

function TUTF8Parser.TokenFloat: Extended;

var errcode : word;

begin
  Val(fLastTokenStr,Result,errcode);
  if errcode<>0 then
    ErrorFmt(SParInvalidFloat,[fLastTokenStr]);
end;

function TUTF8Parser.TokenInt: Int64;
begin
  if not TryStrToInt64(fLastTokenStr,Result) then
    Result:=Int64(StrToQWord(fLastTokenStr)); //second chance for malformed files
end;

function TUTF8Parser.TokenString: string;
begin
  case fToken of
    toFloat : if fFloatType<>#0 then
                Result:=fLastTokenStr+fFloatType
              else Result:=fLastTokenStr
    else
      Result:=fLastTokenStr;
  end;
end;

function TUTF8Parser.TokenSymbolIs(const S: string): Boolean;
begin
  Result:=(fToken=toSymbol) and (CompareText(fLastTokenStr,S)=0);
end;

function TUTF8Parser.SourceColumn: integer;
begin
  Result:=fPos-fLineStart+1;
end;

//------------------------------------------------------------------------------
procedure InternalInit;
begin
  LazarusResources := TLResourceList.Create;
  RegisterInitComponentHandler(TComponent, @InitResourceComponent);
  PropertiesToSkip := TPropertiesToSkip.Create;
end;

initialization
  InternalInit;

finalization
  FreeAndNil(LazarusResources);
  FreeAndNil(PropertiesToSkip);

end.



