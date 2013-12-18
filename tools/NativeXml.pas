unit NativeXml;

interface

uses
    Graphics,
    Classes,
    SysUtils;

const
    cNativeXmlVersion = '3.06';

type
    TPointer = Pointer;

type
    TSeekOrigin = word;
    UTF8String = ansistring;

const
    soBeginning = soFromBeginning;
    soCurrent = soFromCurrent;
    soEnd = soFromEnd;

type
    UnicodeString = WideString;
    UnicodeChar = widechar;
    PUnicodeChar = PWideChar;
    RawByteString = ansistring;

type

    TXmlFormatType = (xfReadable, xfCompact);

    TXmlElementType = (xeNormal,
        xeComment,
        xeCData,
        xeDeclaration,
        xeStylesheet,
        xeDoctype,
        xeElement,
        xeAttList,
        xeEntity,
        xeNotation,
        xeExclam,
        xeQuestion,
        xeCharData,
        xeUnknown
        );

    TBinaryEncodingType = (xbeBinHex, xbeBase64);


    TStringEncodingType = (seAnsi,
        seUCS4BE,
        seUCS4LE,
        seUCS4_2143,
        seUCS4_3412,
        se16BitBE,
        se16BitLE,
        seUTF8,
        seUTF16BE,
        seUTF16LE,
        seEBCDIC
        );

    TXmlCompareOption = (xcNodeName, xcNodeType, xcNodeValue, xcAttribCount,
        xcAttribNames, xcAttribValues, xcChildCount, xcChildNames, xcChildValues,
        xcRecursive);

    TXmlCompareOptions = set of TXmlCompareOption;

const

    xcAll: TXmlCompareOptions = [xcNodeName, xcNodeType, xcNodeValue,
        xcAttribCount, xcAttribNames, xcAttribValues, xcChildCount, xcChildNames,
        xcChildValues, xcRecursive];

var

    cDefaultEncodingString: UTF8String = 'UTF-8';
    cDefaultExternalEncoding: TStringEncodingType = seUTF8;
    cDefaultVersionString: UTF8String = '1.0';
    cDefaultXmlFormat: TXmlFormatType = xfCompact;
    cDefaultWriteOnDefault: boolean = True;
    cDefaultBinaryEncoding: TBinaryEncodingType = xbeBase64;
    cDefaultIndentString: UTF8String = '  ';
    cDefaultDropCommentsOnParse: boolean = False;
    cDefaultUseFullNodes: boolean = False;
    cDefaultFloatAllowScientific: boolean = True;
    cDefaultFloatSignificantDigits: integer = 6;

type

    TXmlNode = class;
    TNativeXml = class;
    TsdCodecStream = class;
    TXmlNodeEvent = procedure(Sender: TObject; Node: TXmlNode) of object;
    TXmlProgressEvent = procedure(Sender: TObject; Size: integer) of object;
    TXmlNodeCompareEvent = function(Sender: TObject; Node1, Node2: TXmlNode; Info: TPointer): integer of object;
    TXMLNodeCompareFunction = function(Node1, Node2: TXmlNode; Info: TPointer): integer;

    TsdUTF8StringList = class(TPersistent)
    private
        FItems: array of UTF8String;
        FCount: integer;
        function GetItems(Index: integer): UTF8String;
        procedure SetItems(Index: integer; const Value: UTF8String);
        function GetValues(const Name: UTF8String): UTF8String;
        function GetNames(Index: integer): UTF8String;
        procedure SetValues(const Name, Value: UTF8String);
        function GetText: UTF8String;
    public
        function Add(const S: UTF8String): integer;
        procedure Assign(Source: TPersistent); override;
        procedure Clear;
        procedure Delete(Index: integer);
        function IndexOfName(const Name: UTF8String): integer;
        property Count: integer read FCount;
        property Items[Index: integer]: UTF8String read GetItems write SetItems; default;
        property Names[Index: integer]: UTF8String read GetNames;
        property Values[const Name: UTF8String]: UTF8String read GetValues write SetValues;
        property Text: UTF8String read GetText;
    end;

    TXmlNode = class(TPersistent)
    private
        FName: UTF8String;
        FValue: UTF8String;
        FAttributes: TsdUTF8StringList;
        FNodes: TList;
        FParent: TXmlNode;
        FDocument: TNativeXml;
        FElementType: TXmlElementType;
        FTag: integer;
        function AbortParsing: boolean;
        function GetValueAsString: UTF8String;
        procedure SetAttributeName(Index: integer; const Value: UTF8String);
        procedure SetAttributeValue(Index: integer; const Value: UTF8String);
        procedure SetValueAsString(const AValue: UTF8String);
        function GetIndent: UTF8String;
        function GetLineFeed: UTF8String;
        function GetTreeDepth: integer;
        function GetAttributeCount: integer;
        function GetAttributePair(Index: integer): UTF8String;
        function GetAttributeName(Index: integer): UTF8String;
        function GetAttributeValue(Index: integer): UTF8String;
        function GetWriteOnDefault: boolean;
        function GetBinaryEncoding: TBinaryEncodingType;
        function GetCascadedName: UTF8String;
        function QualifyAsDirectNode: boolean;
        procedure SetName(const Value: UTF8String);
        function GetFullPath: UTF8String;
        procedure SetBinaryEncoding(const Value: TBinaryEncodingType);
        function GetBinaryString: RawByteString;
        procedure SetBinaryString(const Value: RawByteString);
        function UseFullNodes: boolean;
        function GetValueAsUnicodeString: UnicodeString;
        procedure SetValueAsUnicodeString(const Value: UnicodeString);
        function GetAttributeByName(const AName: UTF8String): UTF8String;
        procedure SetAttributeByName(const AName, Value: UTF8String);
        function GetValueAsInteger: integer;
        procedure SetValueAsInteger(const Value: integer);
        function GetValueAsFloat: double;
        procedure SetValueAsFloat(const Value: double);
        function GetValueAsDateTime: TDateTime;
        procedure SetValueAsDateTime(const Value: TDateTime);
        function GetValueAsBool: boolean;
        procedure SetValueAsBool(const Value: boolean);
        function GetValueAsInt64: int64;
        procedure SetValueAsInt64(const Value: int64);
        procedure CheckCreateAttributesList;
        function GetAttributeValueAsUnicodeString(Index: integer): UnicodeString;
        procedure SetAttributeValueAsUnicodeString(Index: integer; const Value: UnicodeString);
        function GetAttributeValueAsInteger(Index: integer): integer;
        procedure SetAttributeValueAsInteger(Index: integer; const Value: integer);
        function GetAttributeByNameWide(const AName: UTF8String): UnicodeString;
        procedure SetAttributeByNameWide(const AName: UTF8String; const Value: UnicodeString);
        function GetTotalNodeCount: integer;
        function FloatSignificantDigits: integer;
        function FloatAllowScientific: boolean;
        function GetAttributeValueDirect(Index: integer): UTF8String;
        procedure SetAttributeValueDirect(Index: integer; const Value: UTF8String);
    protected
        function CompareNodeName(const NodeName: UTF8String): integer;
        procedure DeleteEmptyAttributes;
        function GetNodes(Index: integer): TXmlNode; virtual;
        function GetNodeCount: integer; virtual;
        procedure ParseTag(const AValue: UTF8String; TagStart, TagClose: integer);
        procedure ReadFromStream(S: TStream); virtual;
        procedure ReadFromString(const AValue: UTF8String); virtual;
        procedure ResolveEntityReferences;
        function UnescapeString(const AValue: UTF8String): UTF8String; virtual;
        function WriteInnerTag: UTF8String; virtual;
        procedure WriteToStream(S: TStream); virtual;
        procedure ChangeDocument(ADocument: TNativeXml);
    public
        constructor Create(ADocument: TNativeXml); virtual;
        constructor CreateName(ADocument: TNativeXml; const AName: UTF8String); virtual;
        constructor CreateNameValue(ADocument: TNativeXml; const AName, AValue: UTF8String); virtual;
        constructor CreateType(ADocument: TNativeXml; AType: TXmlElementType); virtual;
        procedure Assign(Source: TPersistent); override;
        procedure Delete; virtual;
        procedure DeleteEmptyNodes;
        destructor Destroy; override;
        procedure AttributeAdd(const AName: UTF8String; AValue: integer); overload;
        procedure AttributeAdd(const AName, AValue: UTF8String); overload;
        procedure AttributeDelete(Index: integer);
        procedure AttributeExchange(Index1, Index2: integer);
        function AttributeIndexByname(const AName: UTF8String): integer;
        procedure AttributesClear; virtual;
        procedure BufferRead(var Buffer; Count: integer); virtual;
        procedure BufferWrite(const Buffer; Count: integer); virtual;
        function BufferLength: integer; virtual;
        procedure Clear; virtual;
        function FindNode(const NodeName: UTF8String): TXmlNode;
        procedure FindNodes(const NodeName: UTF8String; const AList: TList);
        function FromAnsiString(const S: ansistring): UTF8String;
        function FromUnicodeString(const W: UnicodeString): UTF8String;
        function HasAttribute(const AName: UTF8String): boolean; virtual;
        function IndexInParent: integer;
        function IsClear: boolean; virtual;
        function IsEmpty: boolean; virtual;
        function IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions; MismatchNodes: TList = nil): boolean;
        function NodeAdd(ANode: TXmlNode): integer; virtual;
        function NodeByAttributeValue(const NodeName, AttribName, AttribValue: UTF8String; ShouldRecurse: boolean = True): TXmlNode;
        function NodeByElementType(ElementType: TXmlElementType): TXmlNode;
        function NodeByName(const AName: UTF8String): TXmlNode; virtual;
        procedure NodeDelete(Index: integer); virtual;
        procedure NodeExchange(Index1, Index2: integer);
        function NodeExtract(ANode: TXmlNode): TXmlNode; virtual;
        function NodeFindOrCreate(const AName: UTF8String): TXmlNode; virtual;
        function NodeIndexByName(const AName: UTF8String): integer; virtual;
        function NodeIndexByNameFrom(const AName: UTF8String; AFrom: integer): integer; virtual;
        function NodeIndexOf(ANode: TXmlNode): integer;
        procedure NodeInsert(Index: integer; ANode: TXmlNode); virtual;
        function NodeNew(const AName: UTF8String): TXmlNode; virtual;
        function NodeNewAtIndex(Index: integer; const AName: UTF8String): TXmlNode; virtual;
        function NodeRemove(ANode: TXmlNode): integer;
        procedure NodesClear; virtual;
        procedure NodesByName(const AName: UTF8String; const AList: TList);
        function ReadAttributeBool(const AName: UTF8String; ADefault: boolean = False): boolean; virtual;
        function ReadAttributeDateTime(const AName: UTF8String; ADefault: TDateTime = 0): TDateTime; virtual;
        function ReadAttributeInteger(const AName: UTF8String; ADefault: integer = 0): integer; virtual;
        function ReadAttributeInt64(const AName: UTF8String; ADefault: int64 = 0): int64; virtual;
        function ReadAttributeFloat(const AName: UTF8String; ADefault: double = 0): double;
        function ReadAttributeString(const AName: UTF8String; const ADefault: UTF8String = ''): UTF8String; virtual;
        function ReadBool(const AName: UTF8String; ADefault: boolean = False): boolean; virtual;
        procedure ReadBrush(const AName: UTF8String; ABrush: TBrush); virtual;
        function ReadColor(const AName: UTF8String; ADefault: TColor = clBlack): TColor; virtual;
        procedure ReadFont(const AName: UTF8String; AFont: TFont); virtual;
        procedure ReadPen(const AName: UTF8String; APen: TPen); virtual;
        function ReadDateTime(const AName: UTF8String; ADefault: TDateTime = 0): TDateTime; virtual;
        function ReadFloat(const AName: UTF8String; ADefault: double = 0.0): double; virtual;
        function ReadInt64(const AName: UTF8String; ADefault: int64 = 0): int64; virtual;
        function ReadInteger(const AName: UTF8String; ADefault: integer = 0): integer; virtual;
        function ReadString(const AName: UTF8String; const ADefault: UTF8String = ''): UTF8String; virtual;
        function ReadUnicodeString(const AName: UTF8String; const ADefault: UnicodeString = ''): UnicodeString; virtual;
        procedure SortChildNodes(Compare: TXMLNodeCompareFunction = nil; Info: TPointer = nil);
        function ToUnicodeString(const S: UTF8String): UnicodeString;
        function ValueAsBoolDef(ADefault: boolean): boolean; virtual;
        function ValueAsDateTimeDef(ADefault: TDateTime): TDateTime; virtual;
        function ValueAsFloatDef(ADefault: double): double; virtual;
        function ValueAsInt64Def(ADefault: int64): int64; virtual;
        function ValueAsIntegerDef(ADefault: integer): integer; virtual;
        procedure WriteAttributeBool(const AName: UTF8String; AValue: boolean; ADefault: boolean = False); virtual;
        procedure WriteAttributeDateTime(const AName: UTF8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
        procedure WriteAttributeInteger(const AName: UTF8String; AValue: integer; ADefault: integer = 0); virtual;
        procedure WriteAttributeInt64(const AName: UTF8String; const AValue: int64; ADefault: int64 = 0); virtual;
        procedure WriteAttributeFloat(const AName: UTF8String; AValue: double; ADefault: double = 0); virtual;
        procedure WriteAttributeString(const AName: UTF8String; const AValue: UTF8String; const ADefault: UTF8String = ''); virtual;
        procedure WriteBool(const AName: UTF8String; AValue: boolean; ADefault: boolean = False); virtual;
        procedure WriteBrush(const AName: UTF8String; ABrush: TBrush); virtual;
        procedure WriteColor(const AName: UTF8String; AValue: TColor; ADefault: TColor = clBlack); virtual;
        procedure WriteFont(const AName: UTF8String; AFont: TFont); virtual;
        procedure WritePen(const AName: UTF8String; APen: TPen); virtual;
        procedure WriteDateTime(const AName: UTF8String; AValue: TDateTime; ADefault: TDateTime = 0); virtual;
        procedure WriteFloat(const AName: UTF8String; AValue: double; ADefault: double = 0.0); virtual;
        procedure WriteHex(const AName: UTF8String; AValue: integer; Digits: integer; ADefault: integer = 0); virtual;
        procedure WriteInt64(const AName: UTF8String; AValue: int64; ADefault: int64 = 0); virtual;
        procedure WriteInteger(const AName: UTF8String; AValue: integer; ADefault: integer = 0); virtual;
        procedure WriteString(const AName, AValue: UTF8String; const ADefault: UTF8String = ''); virtual;
        function WriteToString: UTF8String; virtual;
        procedure WriteUnicodeString(const AName: UTF8String; const AValue: UnicodeString; const ADefault: UnicodeString = ''); virtual;
        property AttributeByName[const AName: UTF8String]: UTF8String read GetAttributeByName write SetAttributeByName;
        property AttributeByNameWide[const AName: UTF8String]: UnicodeString read GetAttributeByNameWide write SetAttributeByNameWide;
        property AttributeCount: integer read GetAttributeCount;
        property AttributeName[Index: integer]: UTF8String read GetAttributeName write SetAttributeName;
        property AttributePair[Index: integer]: UTF8String read GetAttributePair;
        property AttributeValue[Index: integer]: UTF8String read GetAttributeValue write SetAttributeValue;
        property AttributeValueAsUnicodeString[Index: integer]: UnicodeString read GetAttributeValueAsUnicodeString write SetAttributeValueAsUnicodeString;
        property AttributeValueAsInteger[Index: integer]: integer read GetAttributeValueAsInteger write SetAttributeValueAsInteger;
        property AttributeValueDirect[Index: integer]: UTF8String read GetAttributeValueDirect write SetAttributeValueDirect;
        property BinaryEncoding: TBinaryEncodingType read GetBinaryEncoding write SetBinaryEncoding;
        property BinaryString: RawByteString read GetBinaryString write SetBinaryString;
        property CascadedName: UTF8String read GetCascadedName;
        property Document: TNativeXml read FDocument write FDocument;
        property ElementType: TXmlElementType read FElementType write FElementType;
        property FullPath: UTF8String read GetFullPath;
        property Name: UTF8String read FName write SetName;
        property Parent: TXmlNode read FParent write FParent;
        property NodeCount: integer read GetNodeCount;
        property Nodes[Index: integer]: TXmlNode read GetNodes; default;
        property Tag: integer read FTag write FTag;
        property TotalNodeCount: integer read GetTotalNodeCount;
        property TreeDepth: integer read GetTreeDepth;
        property ValueAsBool: boolean read GetValueAsBool write SetValueAsBool;
        property ValueAsDateTime: TDateTime read GetValueAsDateTime write SetValueAsDateTime;
        property ValueAsInt64: int64 read GetValueAsInt64 write SetValueAsInt64;
        property ValueAsInteger: integer read GetValueAsInteger write SetValueAsInteger;
        property ValueAsFloat: double read GetValueAsFloat write SetValueAsFloat;
        property ValueAsString: UTF8String read GetValueAsString write SetValueAsString;
        property ValueAsUnicodeString: UnicodeString read GetValueAsUnicodeString write SetValueAsUnicodeString;
        property ValueDirect: UTF8String read FValue write FValue;
        property WriteOnDefault: boolean read GetWriteOnDefault;
    end;

    TXmlNodeList = class(TList)
    private
        function GetItems(Index: integer): TXmlNode;
        procedure SetItems(Index: integer; const Value: TXmlNode);
    public
        function ByAttribute(const AName, AValue: UTF8String): TXmlNode;
        property Items[Index: integer]: TXmlNode read GetItems write SetItems; default;
    end;

    TNativeXml = class(TPersistent)
    private
        FAbortParsing: boolean;
        FBinaryEncoding: TBinaryEncodingType;
        FCodecStream: TsdCodecStream;
        FDropCommentsOnParse: boolean;
        FExternalEncoding: TStringEncodingType;
        FFloatAllowScientific: boolean;
        FFloatSignificantDigits: integer;
        FParserWarnings: boolean;
        FRootNodes: TXmlNode;
        FIndentString: UTF8String;
        FUseFullNodes: boolean;
        FWriteOnDefault: boolean;
        FXmlFormat: TXmlFormatType;
        FOnNodeCompare: TXmlNodeCompareEvent;
        FOnNodeNew: TXmlNodeEvent;
        FOnNodeLoaded: TXmlNodeEvent;
        FOnProgress: TXmlProgressEvent;
        FOnUnicodeLoss: TNotifyEvent;
        procedure DoNodeNew(Node: TXmlNode);
        procedure DoNodeLoaded(Node: TXmlNode);
        procedure DoUnicodeLoss(Sender: TObject);
        function GetCommentString: UTF8String;
        procedure SetCommentString(const Value: UTF8String);
        function GetEntityByName(AName: UTF8String): UTF8String;
        function GetRoot: TXmlNode;
        function GetEncodingString: UTF8String;
        procedure SetEncodingString(const Value: UTF8String);
        function GetVersionString: UTF8String;
        procedure SetVersionString(const Value: UTF8String);
        function GetStyleSheetNode: TXmlNode;
        function GetUtf8Encoded: boolean;
    protected
        procedure CopyFrom(Source: TNativeXml); virtual;
        procedure DoProgress(Size: integer);
        function LineFeed: UTF8String; virtual;
        procedure ParseDTD(ANode: TXmlNode; S: TStream); virtual;
        procedure ReadFromStream(S: TStream); virtual;
        procedure WriteToStream(S: TStream); virtual;
        procedure SetDefaults; virtual;
    public
        constructor Create; virtual;
        constructor CreateName(const ARootName: UTF8String); virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Clear; virtual;
        function IsEmpty: boolean; virtual;
        procedure LoadFromStream(Stream: TStream); virtual;
        procedure LoadFromFile(const AFileName: string); virtual;
        procedure ReadFromString(const AValue: UTF8String); virtual;
        procedure ResolveEntityReferences;
        procedure SaveToStream(Stream: TStream); virtual;
        procedure SaveToFile(const AFileName: string); virtual;
        function WriteToString: UTF8String; virtual;
        property AbortParsing: boolean read FAbortParsing write FAbortParsing;
        property BinaryEncoding: TBinaryEncodingType read FBinaryEncoding write FBinaryEncoding;
        property CommentString: UTF8String read GetCommentString write SetCommentString;
        property DropCommentsOnParse: boolean read FDropCommentsOnParse write FDropCommentsOnParse;
        property EncodingString: UTF8String read GetEncodingString write SetEncodingString;
        property EntityByName[AName: UTF8String]: UTF8String read GetEntityByName;
        property ExternalEncoding: TStringEncodingType read FExternalEncoding write FExternalEncoding;
        property FloatAllowScientific: boolean read FFloatAllowScientific write FFloatAllowScientific;
        property FloatSignificantDigits: integer read FFloatSignificantDigits write FFloatSignificantDigits;
        property IndentString: UTF8String read FIndentString write FIndentString;
        property Root: TXmlNode read GetRoot;
        property RootNodeList: TXmlNode read FRootNodes;
        property StyleSheetNode: TXmlNode read GetStyleSheetNode;
        property UseFullNodes: boolean read FUseFullNodes write FUseFullNodes;
        property Utf8Encoded: boolean read GetUtf8Encoded;
        property VersionString: UTF8String read GetVersionString write SetVersionString;
        property WriteOnDefault: boolean read FWriteOnDefault write FWriteOnDefault;
        property XmlFormat: TXmlFormatType read FXmlFormat write FXmlFormat;
        property ParserWarnings: boolean read FParserWarnings write FParserWarnings;
        property OnNodeCompare: TXmlNodeCompareEvent read FOnNodeCompare write FOnNodeCompare;
        property OnNodeNew: TXmlNodeEvent read FOnNodeNew write FOnNodeNew;
        property OnNodeLoaded: TXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded;
        property OnProgress: TXmlProgressEvent read FOnProgress write FOnProgress;
        property OnUnicodeLoss: TNotifyEvent read FOnUnicodeLoss write FOnUnicodeLoss;
    end;

    TsdStreamModeType = (umUnknown, umRead, umWrite);

    TBigByteArray = array [0 .. MaxInt - 1] of byte;
    PBigByteArray = ^TBigByteArray;

    TsdBufferedReadStream = class(TStream)
    private
        FStream: TStream;
        FBuffer: PBigByteArray;
        FPage: integer;
        FBufPos: integer;
        FBufSize: integer;
        FPosition: longint;
        FOwned: boolean;
        FMustCheck: boolean;
    protected
        procedure CheckPosition;
    public
        constructor Create(AStream: TStream; Owned: boolean = False);
        destructor Destroy; override;
        function Read(var Buffer; Count: longint): longint; override;
        function Write(const Buffer; Count: longint): longint; override;
        function Seek(Offset: longint; Origin: word): longint; override;
    end;

    TsdBufferedWriteStream = class(TStream)
    private
        FStream: TStream;
        FBuffer: PBigByteArray;
        FBufPos: integer;
        FPosition: longint;
        FOwned: boolean;
    protected
        procedure Flush;
    public
        constructor Create(AStream: TStream; Owned: boolean = False);
        destructor Destroy; override;
        function Read(var Buffer; Count: longint): longint; override;
        function Write(const Buffer; Count: longint): longint; override;
        function Seek(Offset: longint; Origin: word): longint; override;
    end;

    TsdCodecStream = class(TStream)
    private
        FBuffer: UTF8String;
        FBufferPos: integer;
        FEncoding: TStringEncodingType;
        FMode: TsdStreamModeType;
        FPosMin1: integer;
        FPosMin2: integer;
        FStream: TStream;
        FSwapByteOrder: boolean;
        FWarningUnicodeLoss: boolean;
        FWriteBom: boolean;
        FOnUnicodeLoss: TNotifyEvent;
    protected
        function ReadByte: byte; virtual;
        procedure StorePrevPositions; virtual;
        procedure WriteByte(const B: byte); virtual;
        procedure WriteBuf(const Buffer; Offset, Count: longint); virtual;
        function InternalRead(var Buffer; Offset, Count: longint): longint;
        function InternalSeek(Offset: longint; Origin: TSeekOrigin): longint;
        function InternalWrite(const Buffer; Offset, Count: longint): longint;
    public
        constructor Create(AStream: TStream); virtual;
        function Read(var Buffer; Count: longint): longint; override;
        function Seek(Offset: longint; Origin: word): longint; override;
        function Write(const Buffer; Count: longint): longint; override;
        property Encoding: TStringEncodingType read FEncoding write FEncoding;
        property WarningUnicodeLoss: boolean read FWarningUnicodeLoss;
        property OnUnicodeLoss: TNotifyEvent read FOnUnicodeLoss write FOnUnicodeLoss;
    end;

    TsdUtf8Stream = class(TsdCodecStream)
    private
    protected
        function ReadByte: byte; override;
        procedure WriteByte(const B: byte); override;
        procedure WriteBuf(const Buffer; Offset, Count: longint); override;
    end;

    TsdSurplusReader = class
    private
        FStream: TStream;
        FSurplus: UTF8String;
    public
        constructor Create(AStream: TStream);
        property Surplus: UTF8String read FSurplus write FSurplus;
        function ReadChar(var Ch: AnsiChar): integer;
        function ReadCharSkipBlanks(var Ch: AnsiChar): boolean;
    end;

    TsdStringBuilder = class
    private
        FData: UTF8String;
        FCurrentIdx: integer;
        function GetData(Index: integer): AnsiChar;
        procedure Reallocate(RequiredLength: integer);
    public
        constructor Create;
        procedure Clear;
        procedure AddChar(Ch: AnsiChar);
        procedure AddString(var S: UTF8String);
        function StringCopy(AFirst, ALength: integer): UTF8String;
        function Value: UTF8String;
        property Length: integer read FCurrentIdx;
        property Data[Index: integer]: AnsiChar read GetData; default;
    end;

function sdUTF8StringReplace(const S, OldPattern, NewPattern: UTF8String): UTF8String;

function sdUTF8EscapeString(const AValue: UTF8String): UTF8String;

function sdUTF8UnEscapeString(const AValue: UTF8String): UTF8String;

function sdUTF8QuotedString(const AValue: UTF8String): UTF8String;

function sdUTF8UnQuotedString(const AValue: UTF8String): UTF8String;

function sdAddControlChars(const AValue: UTF8String; const Chars: UTF8String; Interval: integer): UTF8String;

function sdRemoveControlChars(const AValue: UTF8String): UTF8String;

function sdDateTimeFromString(const ADate: UTF8String): TDateTime;

function sdDateTimeFromStringDefault(const ADate: UTF8String; ADefault: TDateTime): TDateTime;

function sdDateTimeToString(ADate: TDateTime): UTF8String;

function sdWriteNumber(Value: double; SignificantDigits: integer; AllowScientific: boolean): UTF8String;

procedure sdUTF8WriteStringToStream(S: TStream; const AString: UTF8String);

function sdUpCase(Ch: AnsiChar): AnsiChar;

function sdUnicodeToUtf8(const W: UnicodeString): UTF8String;

function sdAnsiToUtf8(const S: ansistring): UTF8String;

function sdUtf8ToUnicode(const S: UTF8String): UnicodeString;

function sdUtf8ToAnsi(const S: UTF8String): ansistring;


function sdUTF8FindString(const SubString, S: UTF8String; Start, Close: integer; var APos: integer): boolean;

function sdUTF8MatchString(const SubString: UTF8String; const S: UTF8String; Start: integer): boolean;

procedure sdUTF8ParseAttributes(const AValue: UTF8String; Start, Close: integer; Attributes: TsdUTF8StringList);

function sdUTF8TrimPos(const AValue: UTF8String; var Start, Close: integer): boolean;

function sdUTF8Trim(const AValue: UTF8String): UTF8String;

function EncodeBase64(const Source: RawByteString): UTF8String;

function DecodeBase64(const Source: UTF8String): RawByteString;

function EncodeBinHex(const Source: RawByteString): UTF8String;

function DecodeBinHex(const Source: UTF8String): RawByteString;

resourcestring

    sxeErrorCalcStreamLength = 'Error while calculating streamlength';
    sxeMissingDataInBinaryStream = 'Missing data in binary stream';
    sxeMissingElementName = 'Missing element name';
    sxeMissingCloseTag = 'Missing close tag in element %s';
    sxeMissingDataAfterGreaterThan = 'Missing data after "<" in element %s';
    sxeMissingLessThanInCloseTag = 'Missing ">" in close tag of element %s';
    sxeIncorrectCloseTag = 'Incorrect close tag in element %s';
    sxeIllegalCharInNodeName = 'Illegal character in node name "%s"';
    sxeMoreThanOneRootElement = 'More than one root element found in xml';
    sxeMoreThanOneDeclaration = 'More than one xml declaration found in xml';
    sxeDeclarationMustBeFirstElem = 'Xml declaration must be first element';
    sxeMoreThanOneDoctype = 'More than one doctype declaration found in root';
    sxeDoctypeAfterRootElement = 'Doctype declaration found after root element';
    sxeNoRootElement = 'No root element found in xml';
    sxeIllegalElementType = 'Illegal element type';
    sxeCDATAInRoot = 'No CDATA allowed in root';
    sxeRootElementNotDefined = 'XML root element not defined.';
    sxeCodecStreamNotAssigned = 'Encoding stream unassigned';
    sxeUnsupportedEncoding = 'Unsupported string encoding';
    sxeCannotReadCodecForWriting =
        'Cannot read from a conversion stream opened for writing';
    sxeCannotWriteCodecForReading =
        'Cannot write to an UTF stream opened for reading';
    sxeCannotReadMultipeChar =
        'Cannot read multiple chars from conversion stream at once';
    sxeCannotPerformSeek = 'Cannot perform seek on codec stream';
    sxeCannotSeekBeforeReadWrite =
        'Cannot seek before reading or writing in conversion stream';
    sxeCannotSeek = 'Cannot perform seek in conversion stream';
    sxeCannotWriteToOutputStream = 'Cannot write to output stream';
    sxeXmlNodeNotAssigned = 'XML Node is not assigned';
    sxeCannotConverToBool = 'Cannot convert value to bool';
    sxeCannotConvertToFloat = 'Cannot convert value to float';
    sxeSignificantDigitsOutOfRange = 'Significant digits out of range';

implementation

type

    TTagType = record
        Start: UTF8String;
        Close: UTF8String;
        Style: TXmlElementType;
    end;

    PByte = ^byte;

    TBomInfo = packed record
        BOM: array [0 .. 3] of byte;
        Len: integer;
        Encoding: TStringEncodingType;
        HasBOM: boolean;
    end;

const

    cEscapeCount = 5;
    cEscapes: array [0 .. cEscapeCount - 1] of UTF8String = ('&', '<', '>', '''',
        '"');

    cReplaces: array [0 .. cEscapeCount - 1] of UTF8String = ('&amp;', '&lt;',
        '&gt;', '&apos;', '&quot;');

    cQuoteChars: set of AnsiChar = ['"', ''''];
    cControlChars: set of AnsiChar = [#9, #10, #13, #32]; { Tab, LF, CR, Space }

    cTagCount = 12;

    cTags: array [0 .. cTagCount - 1] of TTagType = (
        (Start: '<![CDATA['; Close: ']]>'; Style: xeCData), (Start: '<!DOCTYPE';
        Close: '>'; Style: xeDoctype), (Start: '<!ELEMENT'; Close: '>';
        Style: xeElement), (Start: '<!ATTLIST'; Close: '>'; Style: xeAttList),
        (Start: '<!ENTITY'; Close: '>'; Style: xeEntity), (Start: '<!NOTATION';
        Close: '>'; Style: xeNotation), (Start: '<?xml-stylesheet'; Close: '?>';
        Style: xeStylesheet), (Start: '<?xml'; Close: '?>';
        Style: xeDeclaration), (Start: '<!--'; Close: '-->'; Style: xeComment),
        (Start: '<!'; Close: '>'; Style: xeExclam), (Start: '<?'; Close: '?>';
        Style: xeQuestion), (Start: '<'; Close: '>'; Style: xeNormal));
    cHexChar: array [0 .. 15] of AnsiChar = '0123456789ABCDEF';
    cHexCharLoCase: array [0 .. 15] of AnsiChar = '0123456789abcdef';

    cBase64Char: array [0 .. 63] of AnsiChar =
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    cBase64PadChar: AnsiChar = '=';

    cNodeValueBuf = 2048;

const
    cBomInfoCount = 15;

const
    cBomInfo: array [0 .. cBomInfoCount - 1] of TBomInfo =
        ((BOM: ($00, $00, $FE, $FF); Len: 4; Encoding: seUCS4BE; HasBOM: True),
        (BOM: ($FF, $FE, $00, $00); Len: 4; Encoding: seUCS4LE; HasBOM: True),
        (BOM: ($00, $00, $FF, $FE); Len: 4; Encoding: seUCS4_2143;
        HasBOM: True), (BOM: ($FE, $FF, $00, $00); Len: 4;
        Encoding: seUCS4_3412; HasBOM: True), (BOM: ($FE, $FF, $00, $00);
        Len: 2; Encoding: seUTF16BE; HasBOM: True), (BOM: ($FF, $FE, $00, $00);
        Len: 2; Encoding: seUTF16LE; HasBOM: True), (BOM: ($EF, $BB, $BF, $00);
        Len: 3; Encoding: seUTF8; HasBOM: True), (BOM: ($00, $00, $00, $3C);
        Len: 4; Encoding: seUCS4BE; HasBOM: False), (BOM: ($3C, $00, $00, $00);
        Len: 4; Encoding: seUCS4LE; HasBOM: False), (BOM: ($00, $00, $3C, $00);
        Len: 4; Encoding: seUCS4_2143; HasBOM: False),
        (BOM: ($00, $3C, $00, $00); Len: 4; Encoding: seUCS4_3412; HasBOM: False),
        (BOM: ($00, $3C, $00, $3F); Len: 4; Encoding: seUTF16BE; HasBOM: False),
        (BOM: ($3C, $00, $3F, $00); Len: 4; Encoding: seUTF16LE; HasBOM: False),
        (BOM: ($3C, $3F, $78, $6D); Len: 4; Encoding: seAnsi; HasBOM: False),
        (BOM: ($4C, $6F, $A7, $94); Len: 4; Encoding: seEBCDIC; HasBOM: False));

type
    TBytes = TBigByteArray;

function StrScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
    Result := Str;
    while Result^ <> Chr do
    begin
        if Result^ = #0 then
        begin
            Result := nil;
            Exit;
        end;
        Inc(Result);
    end;
end;

function UTF8QuotedStr(const S: UTF8String; Quote: AnsiChar): UTF8String;
var
    P, Src, Dest: PAnsiChar;
    AddCount: integer;
begin
    AddCount := 0;
    P := StrScan(PAnsiChar(S), Quote);
    while P <> nil do
    begin
        Inc(P);
        Inc(AddCount);
        P := StrScan(P, Quote);
    end;
    if AddCount = 0 then
    begin
        Result := UTF8String(Quote) + S + UTF8String(Quote);
        Exit;
    end;
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := Pointer(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := Pointer(S);
    P := StrScan(Src, Quote);
    repeat
        Inc(P);
        Move(Src^, Dest^, P - Src);
        Inc(Dest, P - Src);
        Dest^ := Quote;
        Inc(Dest);
        Src := P;
        P := StrScan(Src, Quote);
    until P = nil;
    P := StrEnd(Src);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
end;

function UTF8ExtractQuotedStr(const S: UTF8String; Quote: AnsiChar): UTF8String;
var
    P, Src, Dest: PAnsiChar;
    DropCount: integer;
begin
    Result := '';
    Src := PAnsiChar(S);
    if (Src = nil) or (Src^ <> Quote) then
        Exit;
    Inc(Src);
    DropCount := 1;
    P := Src;
    Src := StrScan(Src, Quote);
    while Src <> nil do
    begin
        Inc(Src);
        if Src^ <> Quote then
            Break;
        Inc(Src);
        Inc(DropCount);
        Src := StrScan(Src, Quote);
    end;
    if Src = nil then
        Src := StrEnd(P);
    if ((Src - P) <= 1) then
        Exit;
    if DropCount = 1 then
        SetString(Result, P, Src - P - 1)
    else
    begin
        SetLength(Result, Src - P - DropCount);
        Dest := PAnsiChar(Result);
        Src := StrScan(P, Quote);
        while Src <> nil do
        begin
            Inc(Src);
            if Src^ <> Quote then
                Break;
            Move(P^, Dest^, Src - P);
            Inc(Dest, Src - P);
            Inc(Src);
            P := Src;
            Src := StrScan(Src, Quote);
        end;
        if Src = nil then
            Src := StrEnd(P);
        Move(P^, Dest^, Src - P - 1);
    end;
end;

function Utf8Pos(const Substr, S: UTF8String): integer;
var
    i, x: integer;
    Len, LenSubStr: integer;
begin
    i := 1;
    LenSubStr := Length(Substr);
    Len := Length(S) - LenSubStr + 1;
    while i <= Len do
    begin
        if S[i] = Substr[1] then
        begin
            x := 1;
            while (x < LenSubStr) and (S[i + x] = Substr[x + 1]) do
                Inc(x);
            if (x = LenSubStr) then
            begin
                Result := i;
                Exit;
            end;
        end;
        Inc(i);
    end;
    Result := 0;
end;

function StreamWrite(Stream: TStream; const Buffer; Offset, Count: longint): longint;
begin
    Result := Stream.Write(TBytes(Buffer)[Offset], Count);
end;

type
    TsdUTF8StringStream = class(TMemoryStream)
    public
        constructor Create(const S: UTF8String);
        function DataString: UTF8String;
    end;

constructor TsdUTF8StringStream.Create(const S: UTF8String);
begin
    inherited Create;
    SetSize(Length(S));
    if Size > 0 then
    begin
        Write(S[1], Size);
        Position := 0;
    end;
end;

function TsdUTF8StringStream.DataString: UTF8String;
begin
    SetLength(Result, Size);
    if Size > 0 then
    begin
        Position := 0;
        Read(Result[1], Length(Result));
    end;
end;


function Min(A, B: integer): integer;
begin
    if A < B then
        Result := A
    else
        Result := B;
end;

function Max(A, B: integer): integer;
begin
    if A > B then
        Result := A
    else
        Result := B;
end;

function sdUTF8StringReplace(const S, OldPattern, NewPattern: UTF8String): UTF8String;
var
    SearchStr, NewStr: UTF8String;
    Offset: integer;
begin
    SearchStr := S;
    NewStr := S;
    Result := '';
    while SearchStr <> '' do
    begin
        Offset := Utf8Pos(OldPattern, SearchStr);
        if Offset = 0 then
        begin
            Result := Result + NewStr;
            Break;
        end;
        Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
        NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
        SearchStr := Copy(SearchStr, Offset + Length(OldPattern), MaxInt);
    end;
end;

function sdUTF8EscapeString(const AValue: UTF8String): UTF8String;
var
    i: integer;
begin
    Result := AValue;
    for i := 0 to cEscapeCount - 1 do
        Result := sdUTF8StringReplace(Result, cEscapes[i], cReplaces[i]);
end;

function sdUTF8UnEscapeString(const AValue: UTF8String): UTF8String;
var
    SearchStr, Reference, Replace: UTF8String;
    i, Offset, Code: integer;
    W: word;
begin
    SearchStr := AValue;
    Result := '';
    while SearchStr <> '' do
    begin
        Offset := Utf8Pos('&', SearchStr);
        if Offset = 0 then
        begin
            Result := Result + SearchStr;
            Break;
        end;
        Result := Result + Copy(SearchStr, 1, Offset - 1);
        SearchStr := Copy(SearchStr, Offset, MaxInt);
        Offset := Utf8Pos(';', SearchStr);
        if Offset = 0 then
        begin
            Result := Result + SearchStr;
            Break;
        end;
        Reference := Copy(SearchStr, 1, Offset);
        SearchStr := Copy(SearchStr, Offset + 1, MaxInt);
        Replace := Reference;
        if Copy(Reference, 1, 2) = '&#' then
        begin
            Reference := Copy(Reference, 3, Length(Reference) - 3);
            if Length(Reference) > 0 then
            begin
                if sdUpCase(Reference[1]) = 'X' then
                    Reference[1] := '$';
                Code := StrToIntDef(string(Reference), -1);
                if (Code >= 0) and (Code < $FFFF) then
                begin
                    W := Code;
                    Replace := AnsiChar(W and $FF);
                end;
            end;
        end
        else
        begin
            for i := 0 to cEscapeCount - 1 do
                if Reference = cReplaces[i] then
                begin
                    Replace := cEscapes[i];
                    Break;
                end;
        end;
        Result := Result + Replace;
    end;
end;

function sdUTF8QuotedString(const AValue: UTF8String): UTF8String;
var
    Quote: AnsiChar;
begin
    Quote := '"';
    if Utf8Pos('"', AValue) > 0 then
        Quote := '''';
    Result := UTF8QuotedStr(AValue, Quote);
end;

function sdUTF8UnQuotedString(const AValue: UTF8String): UTF8String;
var
    Quote: AnsiChar;
begin
    if Length(AValue) < 2 then
    begin
        Result := AValue;
        Exit;
    end;
    Quote := AValue[1];
    if Quote in cQuoteChars then
    begin
        Result := UTF8ExtractQuotedStr(AValue, Quote);
    end
    else
        Result := AValue;
end;

function sdAddControlChars(const AValue: UTF8String; const Chars: UTF8String; Interval: integer): UTF8String;
var
    i, j, ALength: integer;

    procedure InsertControlChars;
    var
        k: integer;
    begin
        for k := 1 to Length(Chars) do
        begin
            Result[j] := Chars[k];
            Inc(j);
        end;
    end;

begin
    if (Length(Chars) = 0) or (Interval <= 0) then
    begin
        Result := AValue;
        Exit;
    end;

    ALength := Length(AValue) + ((Length(AValue) - 1) div Interval + 3) * Length
        (Chars);
    SetLength(Result, ALength);

    j := 1;
    for i := 1 to Length(AValue) do
    begin
        if (i mod Interval) = 1 then
            InsertControlChars;
        Result[j] := AValue[i];
        Inc(j);
    end;
    InsertControlChars;

    Dec(j);
    if ALength > j then
        SetLength(Result, j);
end;

function sdRemoveControlChars(const AValue: UTF8String): UTF8String;
var
    i, j: integer;
begin
    SetLength(Result, Length(AValue));
    i := 1;
    j := 1;
    while i <= Length(AValue) do
        if AValue[i] in cControlChars then
            Inc(i)
        else
        begin
            Result[j] := AValue[i];
            Inc(i);
            Inc(j);
        end;
    if i <> j then
        SetLength(Result, j - 1);
end;

function sdUTF8FindString(const SubString, S: UTF8String; Start, Close: integer; var APos: integer): boolean;
var
    CharIndex: integer;
begin
    Result := False;
    APos := 0;
    for CharIndex := Start to Close - Length(SubString) do
        if sdUTF8MatchString(SubString, S, CharIndex) then
        begin
            APos := CharIndex;
            Result := True;
            Exit;
        end;
end;

function UTF8CompareText(const S1, S2: UTF8String): integer;
begin
    Result := AnsiCompareText(string(S1), string(S2));
end;

function IntToUTF8Str(Value: integer): UTF8String;
begin
    Result := UTF8String(IntToStr(Value));
end;

function sdUTF8MatchString(const SubString: UTF8String; const S: UTF8String; Start: integer): boolean;
var
    CharIndex: integer;
begin
    Result := False;
    if (Length(S) - Start + 1) < Length(SubString) then
        Exit;

    CharIndex := 0;
    while CharIndex < Length(SubString) do
        if sdUpCase(SubString[CharIndex + 1]) = sdUpCase(S[Start + CharIndex]) then
            Inc(CharIndex)
        else
            Exit;
    Result := True;
end;

procedure sdUTF8ParseAttributes(const AValue: UTF8String; Start, Close: integer; Attributes: TsdUTF8StringList);
var
    i: integer;
    InQuotes: boolean;
    Quote: AnsiChar;
begin
    InQuotes := False;
    Quote := '"';
    if not assigned(Attributes) then
        Exit;
    if not sdUTF8TrimPos(AValue, Start, Close) then
        Exit;

    Attributes.Clear;

    for i := Start to Close - 1 do
    begin

        if InQuotes then
        begin
            if AValue[i] = Quote then
                InQuotes := False;
        end
        else
        begin
            if AValue[i] in cQuoteChars then
            begin
                InQuotes := True;
                Quote := AValue[i];
            end;
        end;

        if not InQuotes then
            if AValue[i] in cControlChars then
            begin
                if i > Start then
                    Attributes.Add(Copy(AValue, Start, i - Start));
                Start := i + 1;
            end;
    end;

    if Start < Close then
        Attributes.Add(Copy(AValue, Start, Close - Start));

    for i := Attributes.Count - 1 downto 1 do
        if Attributes[i][1] = '=' then
        begin
            Attributes[i - 1] := Attributes[i - 1] + Attributes[i];
            Attributes.Delete(i);
        end;

    for i := Attributes.Count - 1 downto 1 do
        if (Attributes[i][1] in cQuoteChars) and
            (Utf8Pos('=', Attributes[i - 1]) > 0) then
        begin
            Attributes[i - 1] := Attributes[i - 1] + Attributes[i];
            Attributes.Delete(i);
        end;
end;

function sdUTF8TrimPos(const AValue: UTF8String; var Start, Close: integer): boolean;
begin
    Start := Max(1, Start);
    Close := Min(Length(AValue) + 1, Close);
    if Close <= Start then
    begin
        Result := False;
        Exit;
    end;

    while (Start < Close) and (AValue[Start] in cControlChars) do
        Inc(Start);

    while (Start < Close) and (AValue[Close - 1] in cControlChars) do
        Dec(Close);

    Result := Close > Start;
end;

function sdUTF8Trim(const AValue: UTF8String): UTF8String;
var
    Start, Close: integer;
    Res: boolean;
begin
    Start := 1;
    Close := Length(AValue) + 1;
    Res := sdUTF8TrimPos(AValue, Start, Close);
    if Res then
        Result := Copy(AValue, Start, Close - Start)
    else
        Result := '';
end;

procedure sdUTF8WriteStringToStream(S: TStream; const AString: UTF8String);
begin
    if Length(AString) > 0 then
    begin
        S.Write(AString[1], Length(AString));
    end;
end;

function sdUpCase(Ch: AnsiChar): AnsiChar;
begin
    Result := Ch;
    case Result of
        'a' .. 'z':
            Dec(Result, Ord('a') - Ord('A'));
    end;
end;

function ReadOpenTag(AReader: TsdSurplusReader): integer;
var
    AIndex, i: integer;
    Found: boolean;
    Ch: AnsiChar;
    Candidates: array [0 .. cTagCount - 1] of boolean;
    Surplus: UTF8String;
begin
    Surplus := '';
    Result := cTagCount - 1;
    for i := 0 to cTagCount - 1 do
        Candidates[i] := True;
    AIndex := 1;
    repeat
        Found := False;
        Inc(AIndex);
        if AReader.ReadChar(Ch) = 0 then
            Exit;
        Surplus := Surplus + UTF8String(Ch);
        for i := cTagCount - 1 downto 0 do
            if Candidates[i] and (Length(cTags[i].Start) >= AIndex) then
            begin
                if cTags[i].Start[AIndex] = Ch then
                begin
                    Found := True;
                    if Length(cTags[i].Start) = AIndex then
                        Result := i;
                end
                else
                    Candidates[i] := False;
            end;
    until Found = False;
    AReader.Surplus := Copy(Surplus, Length(cTags[Result].Start),
        Length(Surplus));
end;

function ReadStringFromStreamUntil(AReader: TsdSurplusReader; const ASearch: UTF8String; var AValue: UTF8String; SkipQuotes: boolean): boolean;
var
    AIndex, ValueIndex, SearchIndex: integer;
    LastSearchChar, Ch: AnsiChar;
    InQuotes: boolean;
    QuoteChar: AnsiChar;
    SB: TsdStringBuilder;
begin
    Result := False;
    InQuotes := False;

    AIndex := Length(ASearch);
    if AIndex = 0 then
        Exit;
    LastSearchChar := ASearch[AIndex];

    SB := TsdStringBuilder.Create;
    try
        QuoteChar := #0;

        repeat
            if AReader.ReadChar(Ch) = 0 then
                Exit;
            SB.AddChar(Ch);

            if SkipQuotes then
            begin
                if InQuotes then
                begin
                    if (Ch = QuoteChar) then
                        InQuotes := False;
                end
                else
                begin
                    if Ch in cQuoteChars then
                    begin
                        InQuotes := True;
                        QuoteChar := Ch;
                    end;
                end;
            end;

            if not InQuotes then
            begin
                if Ch = LastSearchChar then
                begin

                    ValueIndex := SB.Length - 1;
                    SearchIndex := Length(ASearch) - 1;
                    if ValueIndex < SearchIndex then
                        continue;

                    Result := True;
                    while (SearchIndex > 0) and Result do
                    begin
                        Result := SB[ValueIndex] = ASearch[SearchIndex];
                        Dec(ValueIndex);
                        Dec(SearchIndex);
                    end;
                end;
            end;
        until Result;

        AValue := SB.StringCopy(1, SB.Length - Length(ASearch));
    finally
        SB.Free;
    end;
end;

function ReadStringFromStreamWithQuotes(S: TStream; const Terminator: UTF8String; var AValue: UTF8String): boolean;
var
    Ch, QuoteChar: AnsiChar;
    InQuotes: boolean;
    SB: TsdStringBuilder;
begin
    SB := TsdStringBuilder.Create;
    try
        QuoteChar := #0;
        Result := False;
        InQuotes := False;
        repeat
            if S.Read(Ch, 1) = 0 then
                Exit;
            if not InQuotes then
            begin
                if (Ch = '"') or (Ch = '''') then
                begin
                    InQuotes := True;
                    QuoteChar := Ch;
                end;
            end
            else
            begin
                if Ch = QuoteChar then
                    InQuotes := False;
            end;
            if not InQuotes and (UTF8String(Ch) = Terminator) then
                Break;
            SB.AddChar(Ch);
        until False;
        AValue := SB.Value;
        Result := True;
    finally
        SB.Free;
    end;
end;

function sdDateTimeFromString(const ADate: UTF8String): TDateTime;
var
    AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
begin
    AYear := StrToInt(string(Copy(ADate, 1, 4)));
    AMonth := StrToInt(string(Copy(ADate, 6, 2)));
    ADay := StrToInt(string(Copy(ADate, 9, 2)));
    if Length(ADate) > 16 then
    begin
        AHour := StrToInt(string(Copy(ADate, 12, 2)));
        AMin := StrToInt(string(Copy(ADate, 15, 2)));
        ASec := StrToIntDef(string(Copy(ADate, 18, 2)), 0);
        AMSec := StrToIntDef(string(Copy(ADate, 21, 3)), 0);
    end
    else
    begin
        AHour := 0;
        AMin := 0;
        ASec := 0;
        AMSec := 0;
    end;
    Result := EncodeDate(AYear, AMonth, ADay) + EncodeTime(AHour, AMin, ASec,
        AMSec);
end;

function sdDateTimeFromStringDefault(const ADate: UTF8String; ADefault: TDateTime): TDateTime;
begin
    try
        Result := sdDateTimeFromString(ADate);
    except
        Result := ADefault;
    end;
end;

function sdDateTimeToString(ADate: TDateTime): UTF8String;
var
    AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    DecodeTime(ADate, AHour, AMin, ASec, AMSec);
    if frac(ADate) = 0 then
        Result := UTF8String(Format('%.4d-%.2d-%.2d', [AYear, AMonth, ADay]))
    else
        Result := UTF8String(Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ',
            [AYear, AMonth, ADay, AHour, AMin, ASec, AMSec]));
end;

function sdWriteNumber(Value: double; SignificantDigits: integer; AllowScientific: boolean): UTF8String;
const
    Limits: array [1 .. 9] of integer = (10, 100, 1000, 10000, 100000, 1000000,
        10000000, 100000000, 1000000000);
var
    Limit, Limitd, PointPos, IntVal, ScPower: integer;
    Body: UTF8String;
begin
    if (SignificantDigits < 1) or (SignificantDigits > 9) then
        raise Exception.Create(sxeSignificantDigitsOutOfRange);
    if Value = 0 then
    begin
        Result := '0';
        Exit;
    end;

    if Value < 0 then
    begin
        Result := '-';
        Value := -Value;
    end
    else
        Result := '';

    Limit := Limits[SignificantDigits];
    Limitd := Limit div 10;
    PointPos := SignificantDigits;
    while Value < Limitd do
    begin
        Value := Value * 10;
        Dec(PointPos);
    end;
    while Value >= Limit do
    begin
        Value := Value * 0.1;
        Inc(PointPos);
    end;

    IntVal := round(Value);

    if IntVal = Limit then
    begin
        IntVal := IntVal div 10;
        Inc(PointPos);
    end;

    while (IntVal mod 10 = 0) and (PointPos < SignificantDigits) do
    begin
        Dec(SignificantDigits);
        IntVal := IntVal div 10;
    end;

    ScPower := 0;
    if AllowScientific and ((PointPos < -1) or (PointPos > SignificantDigits + 2)
        ) then
    begin
        ScPower := PointPos - 1;
        Dec(PointPos, ScPower);
    end;

    Body := IntToUTF8Str(IntVal);
    while PointPos > SignificantDigits do
    begin
        Body := Body + '0';
        Inc(SignificantDigits);
    end;
    while PointPos < 0 do
    begin
        Body := '0' + Body;
        Inc(PointPos);
    end;
    if PointPos = 0 then
        Body := '.' + Body
    else if PointPos < SignificantDigits then
        Body := Copy(Body, 1, PointPos) + '.' + Copy(Body, PointPos + 1,
            SignificantDigits);

    if ScPower = 0 then
        Result := Result + Body
    else
        Result := Result + Body + 'E' + IntToUTF8Str(ScPower);
end;

function PtrUnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: cardinal; Source: PUnicodeChar; SourceChars: cardinal): cardinal;
var
    i, Count: cardinal;
    c: cardinal;
begin
    Result := 0;
    if not assigned(Source) or not assigned(Dest) then
        Exit;

    Count := 0;
    i := 0;

    while (i < SourceChars) and (Count < MaxDestBytes) do
    begin
        c := cardinal(Source[i]);
        Inc(i);
        if c <= $7F then
        begin
            Dest[Count] := AnsiChar(c);
            Inc(Count);
        end
        else if c > $7FF then
        begin
            if Count + 3 > MaxDestBytes then
                Break;
            Dest[Count] := AnsiChar($E0 or (c shr 12));
            Dest[Count + 1] := AnsiChar($80 or ((c shr 6) and $3F));
            Dest[Count + 2] := AnsiChar($80 or (c and $3F));
            Inc(Count, 3);
        end
        else
        begin
            if Count + 2 > MaxDestBytes then
                Break;
            Dest[Count] := AnsiChar($C0 or (c shr 6));
            Dest[Count + 1] := AnsiChar($80 or (c and $3F));
            Inc(Count, 2);
        end;
    end;
    if Count >= MaxDestBytes then
        Count := MaxDestBytes - 1;
    Dest[Count] := #0;
    Result := Count + 1;
end;

function PtrUtf8ToUnicode(Dest: PUnicodeChar; MaxDestChars: cardinal; Source: PAnsiChar; SourceBytes: cardinal): cardinal;
var
    i, Count: cardinal;
    c: byte;
    wc: cardinal;
begin
    if not assigned(Dest) or not assigned(Source) then
    begin
        Result := 0;
        Exit;
    end;
    Result := cardinal(-1);
    Count := 0;
    i := 0;
    while (i < SourceBytes) and (Count < MaxDestChars) do
    begin
        wc := cardinal(Source[i]);
        Inc(i);
        if (wc and $80) <> 0 then
        begin
            if i >= SourceBytes then
                Exit;
            wc := wc and $3F;
            if (wc and $20) <> 0 then
            begin
                c := byte(Source[i]);
                Inc(i);
                if (c and $C0) <> $80 then
                    Exit;
                if i >= SourceBytes then
                    Exit;
                wc := (wc shl 6) or (c and $3F);
            end;
            c := byte(Source[i]);
            Inc(i);
            if (c and $C0) <> $80 then
                Exit;
            Dest[Count] := UnicodeChar((wc shl 6) or (c and $3F));
        end
        else
            Dest[Count] := UnicodeChar(wc);
        Inc(Count);
    end;

    if Count >= MaxDestChars then
        Count := MaxDestChars - 1;

    Dest[Count] := #0;
    Result := Count + 1;
end;

function sdUnicodeToUtf8(const W: UnicodeString): UTF8String;
var
    L: integer;
    Temp: UTF8String;
begin
    Result := '';
    if W = '' then
        Exit;
    SetLength(Temp, Length(W) * 3);

    L := PtrUnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PUnicodeChar(W),
        Length(W));
    if L > 0 then
        SetLength(Temp, L - 1)
    else
        Temp := '';
    Result := Temp;
end;

function sdUtf8ToUnicode(const S: UTF8String): UnicodeString;
var
    L: integer;
    Temp: UnicodeString;
begin
    Result := '';
    if S = '' then
        Exit;
    SetLength(Temp, Length(S));

    L := PtrUtf8ToUnicode(PUnicodeChar(Temp), Length(Temp) + 1, PAnsiChar(S),
        Length(S));
    if L > 0 then
        SetLength(Temp, L - 1)
    else
        Temp := '';
    Result := Temp;
end;

function EncodeBase64Buf(const Buffer; Count: integer): UTF8String;
var
    i, j: integer;
    ACore: integer;
    ALong: cardinal;
    S: PByte;
begin
    ACore := (Count + 2) div 3;

    SetLength(Result, ACore * 4);
    S := @Buffer;
    for i := 0 to ACore - 1 do
    begin
        ALong := 0;
        for j := 0 to 2 do
        begin
            ALong := ALong shl 8 + S^;
            Inc(S);
        end;
        for j := 0 to 3 do
        begin
            Result[i * 4 + 4 - j] := cBase64Char[ALong and $3F];
            ALong := ALong shr 6;
        end;
    end;
    case ACore * 3 - Count of
        0:
            ;
        1:
            Result[ACore * 4] := cBase64PadChar;
        2:
        begin
            Result[ACore * 4] := cBase64PadChar;
            Result[ACore * 4 - 1] := cBase64PadChar;
        end;
    end;
end;

function EncodeBase64(const Source: RawByteString): UTF8String;
begin
    if Length(Source) > 0 then
        Result := EncodeBase64Buf(Source[1], Length(Source))
    else
        Result := '';
end;

procedure DecodeBase64Buf(var Source: UTF8String; var Buffer; Count: integer);
var
    i, j: integer;
    BufPos, Core: integer;
    LongVal: cardinal;
    D: PByte;
    Map: array [AnsiChar] of byte;
begin
    Core := Length(Source) div 4;
    if Count > Core * 3 then
        raise EFilerError.Create(sxeMissingDataInBinaryStream);

    for i := 0 to 63 do
        Map[cBase64Char[i]] := i;
    D := @Buffer;

    BufPos := Length(Source);
    if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
    begin
        Source[BufPos] := cBase64Char[0];
        Dec(BufPos);
        if (BufPos > 0) and (Source[BufPos] = cBase64PadChar) then
            Source[BufPos] := cBase64Char[0];
    end;

    for i := 0 to Core - 1 do
    begin
        LongVal := 0;
        for j := 0 to 3 do
            LongVal := LongVal shl 6 + Map[Source[i * 4 + j + 1]];
        for j := 2 downto 0 do
        begin
            if integer(D) - integer(@Buffer) >= Count then
                Exit;
            D^ := LongVal shr (j * 8) and $FF;
            Inc(D);
        end;
    end;
end;

function DecodeBase64(const Source: UTF8String): RawByteString;
var
    BufData: UTF8String;
    BufSize, BufPos: integer;
begin
    BufData := sdRemoveControlChars(Source);

    BufSize := Length(BufData) div 4;
    if BufSize * 4 <> Length(BufData) then
        raise EFilerError.Create(sxeErrorCalcStreamLength);
    BufSize := BufSize * 3;
    BufPos := Length(BufData);
    if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
    begin
        Dec(BufPos);
        Dec(BufSize);
        if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
            Dec(BufSize);
    end;
    SetLength(Result, BufSize);

    if BufSize > 0 then
        DecodeBase64Buf(BufData, Result[1], BufSize);
end;

function sdAnsiToUtf8(const S: ansistring): UTF8String;
begin
    Result := sdUnicodeToUtf8(UnicodeString(S));
end;

function sdUtf8ToAnsi(const S: UTF8String): ansistring;
begin
    Result := ansistring(sdUtf8ToUnicode(S));
end;

function EncodeBinHexBuf(const Source; Count: integer): UTF8String;
var
    Text: UTF8String;
begin
    SetLength(Text, Count * 2);
    BinToHex(PAnsiChar(@Source), PAnsiChar(Text), Count);
    Result := Text;
end;

function EncodeBinHex(const Source: RawByteString): UTF8String;
var
    Text: UTF8String;
begin
    SetLength(Text, Length(Source) * 2);
    BinToHex(PAnsiChar(Source), PAnsiChar(Text), Length(Source));
    Result := Text;
end;

procedure DecodeBinHexBuf(const Source: UTF8String; var Buffer; Count: integer);
begin
    if Length(Source) div 2 < Count then
        raise EFilerError.Create(sxeMissingDataInBinaryStream);
    HexToBin(PAnsiChar(Source), PAnsiChar(@Buffer), Count);
end;

function DecodeBinHex(const Source: UTF8String): RawByteString;
var
    Data: UTF8String;
    Size: integer;
    Buffer: RawByteString;
begin
    Data := sdRemoveControlChars(Source);

    Size := Length(Data) div 2;
    if Size * 2 <> Length(Data) then
        raise EFilerError.Create(sxeErrorCalcStreamLength);

    SetLength(Buffer, Size);
    HexToBin(PAnsiChar(Data), PAnsiChar(Buffer), Size);
    Result := Buffer;
end;

function sdStringToBool(const AValue: UTF8String): boolean;
var
    Ch: AnsiChar;
begin
    if Length(AValue) > 0 then
    begin
        Ch := sdUpCase(AValue[1]);
        if Ch in ['T', 'Y', '1'] then
        begin
            Result := True;
            Exit;
        end;
        if Ch in ['F', 'N', '0'] then
        begin
            Result := False;
            Exit;
        end;
    end;
    raise Exception.Create(sxeCannotConverToBool);
end;

function sdStringFromBool(ABool: boolean): UTF8String;
const
    cBoolValues: array [boolean] of UTF8String = ('false', 'true');
begin
    Result := cBoolValues[ABool];
end;


function TsdUTF8StringList.Add(const S: UTF8String): integer;
var
    L: integer;
begin
    L := Length(FItems);
    if L = FCount then
    begin
        SetLength(FItems, FCount + 4);
    end;
    FItems[FCount] := S;
    Result := FCount;
    Inc(FCount);
end;

procedure TsdUTF8StringList.Assign(Source: TPersistent);
var
    i: integer;
    SL: TsdUTF8StringList;
begin
    if Source is TsdUTF8StringList then
    begin
        SL := TsdUTF8StringList(Source);
        SetLength(FItems, SL.FCount);
        for i := 0 to SL.FCount - 1 do
            FItems[i] := SL.FItems[i];
        FCount := SL.FCount;
    end
    else
        inherited;
end;

procedure TsdUTF8StringList.Clear;
begin
    FCount := 0;
end;

procedure TsdUTF8StringList.Delete(Index: integer);
var
    i: integer;
begin
    if (Index < 0) or (Index >= Count) then
        Exit;
    for i := Index + 1 to Count - 1 do
        FItems[i - 1] := FItems[i];
    Dec(FCount);
end;

function TsdUTF8StringList.GetItems(Index: integer): UTF8String;
begin
    if (Index >= 0) and (Index < Count) then
        Result := FItems[Index]
    else
        Result := '';
end;

function TsdUTF8StringList.GetNames(Index: integer): UTF8String;
var
    P: integer;
begin
    Result := Items[Index];
    P := Utf8Pos('=', Result);
    if P <> 0 then
        SetLength(Result, P - 1)
    else
        SetLength(Result, 0);
end;

function TsdUTF8StringList.GetText: UTF8String;
const
    cLB: UTF8String = #13#10;
var
    i, L, LItem: integer;
    P: PAnsiChar;
begin
    L := 0;
    for i := 0 to Count - 1 do
    begin
        Inc(L, Length(FItems[i]));
        Inc(L, 2);
    end;
    SetLength(Result, L);
    if L = 0 then
        Exit;
    P := @Result[1];
    for i := 0 to Count - 1 do
    begin
        LItem := Length(FItems[i]);
        if LItem > 0 then
        begin
            System.Move(FItems[i][1], P^, LItem);
            Inc(P, LItem);
        end;
        System.Move(cLB[1], P^, 2);
        Inc(P, 2);
    end;
end;

function TsdUTF8StringList.GetValues(const Name: UTF8String): UTF8String;
var
    Idx: integer;
begin
    Idx := IndexOfName(Name);
    if Idx >= 0 then
        Result := Copy(FItems[Idx], Length(Name) + 2, MaxInt)
    else
        Result := '';
end;

function TsdUTF8StringList.IndexOfName(const Name: UTF8String): integer;
begin
    for Result := 0 to Count - 1 do
    begin
        if sdUTF8MatchString(Name + '=', FItems[Result], 1) then
            Exit;
    end;
    Result := -1;
end;

procedure TsdUTF8StringList.SetItems(Index: integer; const Value: UTF8String);
begin
    if (Index >= 0) and (Index < Count) then
        FItems[Index] := Value;
end;

procedure TsdUTF8StringList.SetValues(const Name, Value: UTF8String);
var
    Idx: integer;
begin
    Idx := IndexOfName(Name);
    if Value <> '' then
    begin
        if Idx < 0 then
            Idx := Add('');
        FItems[Idx] := Name + '=' + Value;
    end
    else
        Delete(Idx);
end;

function TXmlNode.AbortParsing: boolean;
begin
    Result := assigned(Document) and Document.AbortParsing;
end;

procedure TXmlNode.Assign(Source: TPersistent);
var
    i: integer;
    Node: TXmlNode;
begin
    if Source is TXmlNode then
    begin
        Clear;

        FElementType := TXmlNode(Source).FElementType;
        FName := TXmlNode(Source).FName;
        FTag := TXmlNode(Source).FTag;
        FValue := TXmlNode(Source).FValue;

        if assigned(TXmlNode(Source).FAttributes) then
        begin
            CheckCreateAttributesList;
            FAttributes.Assign(TXmlNode(Source).FAttributes);
        end;

        for i := 0 to TXmlNode(Source).NodeCount - 1 do
        begin
            Node := NodeNew('');
            Node.Assign(TXmlNode(Source).Nodes[i]);
        end;
    end
    else if Source is TNativeXml then
    begin
        Assign(TNativeXml(Source).FRootNodes);
    end
    else
        inherited;
end;

procedure TXmlNode.AttributeAdd(const AName, AValue: UTF8String);
var
    Attr: UTF8String;
begin
    Attr := UTF8String(Format('%s=%s', [AName,
        sdUTF8QuotedString(sdUTF8EscapeString(AValue))]));
    CheckCreateAttributesList;
    FAttributes.Add(Attr);
end;

procedure TXmlNode.AttributeAdd(const AName: UTF8String; AValue: integer);
begin
    AttributeAdd(AName, IntToUTF8Str(AValue));
end;

procedure TXmlNode.AttributeDelete(Index: integer);
begin
    if (Index >= 0) and (Index < AttributeCount) then
        FAttributes.Delete(Index);
end;

procedure TXmlNode.AttributeExchange(Index1, Index2: integer);
var
    Temp: UTF8String;
begin
    if (Index1 <> Index2) and (Index1 >= 0) and (Index1 < FAttributes.Count) and
        (Index2 >= 0) and (Index2 < FAttributes.Count) then
    begin
        Temp := FAttributes[Index1];
        FAttributes[Index1] := FAttributes[Index2];
        FAttributes[Index2] := Temp;
    end;
end;

function TXmlNode.AttributeIndexByname(const AName: UTF8String): integer;
begin
    if assigned(FAttributes) then
        Result := FAttributes.IndexOfName(AName)
    else
        Result := -1;
end;

procedure TXmlNode.AttributesClear;
begin
    FreeAndNil(FAttributes);
end;

function TXmlNode.BufferLength: integer;
var
    BufData: UTF8String;
    BufPos: integer;
begin
    BufData := sdRemoveControlChars(FValue);
    case BinaryEncoding of
        xbeBinHex:
        begin
            Result := Length(BufData) div 2;
            if Result * 2 <> Length(BufData) then
                raise EFilerError.Create(sxeErrorCalcStreamLength);
        end;
        xbeBase64:
        begin
            Result := Length(BufData) div 4;
            if Result * 4 <> Length(BufData) then
                raise EFilerError.Create(sxeErrorCalcStreamLength);
            Result := Result * 3;
            BufPos := Length(BufData);
            if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
            begin
                Dec(BufPos);
                Dec(Result);
                if (BufPos > 0) and (BufData[BufPos] = cBase64PadChar) then
                    Dec(Result);
            end;
        end;
        else
            Result := 0;
    end;
end;

procedure TXmlNode.BufferRead(var Buffer; Count: integer);
var
    BufData: UTF8String;
begin
    BufData := sdRemoveControlChars(FValue);
    case BinaryEncoding of
        xbeBinHex:
        begin
            DecodeBinHexBuf(BufData, Buffer, Count);
            BufData := sdRemoveControlChars(FValue);
        end;

        xbeBase64:
            DecodeBase64Buf(BufData, Buffer, Count);
    end;
end;

procedure TXmlNode.BufferWrite(const Buffer; Count: integer);
var
    BufData: UTF8String;
begin
    if Count > 0 then
        case BinaryEncoding of
            xbeBinHex:
                FValue := EncodeBinHexBuf(Buffer, Count);
            xbeBase64:
            begin
                BufData := EncodeBase64Buf(Buffer, Count);
                FValue := sdAddControlChars(BufData, GetLineFeed + GetIndent, 76);
            end;

        end;

end;

procedure TXmlNode.ChangeDocument(ADocument: TNativeXml);
var
    i: integer;
begin
    FDocument := ADocument;
    for i := 0 to NodeCount - 1 do
        Nodes[i].ChangeDocument(ADocument);
end;

procedure TXmlNode.CheckCreateAttributesList;
begin
    if not assigned(FAttributes) then
        FAttributes := TsdUTF8StringList.Create;
end;

procedure TXmlNode.Clear;
begin
    FName := '';
    FValue := '';
    AttributesClear;
    NodesClear;
end;

function TXmlNode.CompareNodeName(const NodeName: UTF8String): integer;
begin
    if Length(NodeName) > 0 then
        if NodeName[1] = '/' then
        begin
            Result := UTF8CompareText(FullPath, NodeName);
            Exit;
        end;
    Result := UTF8CompareText(Name, NodeName);
end;

constructor TXmlNode.Create(ADocument: TNativeXml);
begin
    inherited Create;
    FDocument := ADocument;
end;

constructor TXmlNode.CreateName(ADocument: TNativeXml; const AName: UTF8String);
begin
    Create(ADocument);
    Name := AName;
end;

constructor TXmlNode.CreateNameValue(ADocument: TNativeXml; const AName, AValue: UTF8String);
begin
    Create(ADocument);
    Name := AName;
    ValueAsString := AValue;
end;

constructor TXmlNode.CreateType(ADocument: TNativeXml; AType: TXmlElementType);
begin
    Create(ADocument);
    FElementType := AType;
end;

procedure TXmlNode.Delete;
begin
    if assigned(Parent) then
        Parent.NodeRemove(Self);
end;

procedure TXmlNode.DeleteEmptyAttributes;
var
    i: integer;
    V: UTF8String;
begin
    for i := AttributeCount - 1 downto 0 do
    begin
        V := AttributeValue[i];
        if Length(V) = 0 then
            FAttributes.Delete(i);
    end;
end;

procedure TXmlNode.DeleteEmptyNodes;
var
    i: integer;
    Node: TXmlNode;
begin
    for i := NodeCount - 1 downto 0 do
    begin
        Node := Nodes[i];
        Node.DeleteEmptyNodes;
        if Node.IsEmpty then
            NodeDelete(i);
    end;
end;

destructor TXmlNode.Destroy;
begin
    NodesClear;
    AttributesClear;
    inherited;
end;

function TXmlNode.FindNode(const NodeName: UTF8String): TXmlNode;
var
    i: integer;
begin
    Result := nil;
    for i := 0 to NodeCount - 1 do
    begin
        Result := Nodes[i];
        if Result.CompareNodeName(NodeName) = 0 then
            Exit;
        Result := Result.FindNode(NodeName);
        if assigned(Result) then
            Exit;
    end;
end;

procedure TXmlNode.FindNodes(const NodeName: UTF8String; const AList: TList);

    procedure FindNodesRecursive(ANode: TXmlNode; AList: TList);
    var
        i: integer;
    begin
        with ANode do
            for i := 0 to NodeCount - 1 do
            begin
                if Nodes[i].CompareNodeName(NodeName) = 0 then
                    AList.Add(Nodes[i]);
                FindNodesRecursive(Nodes[i], AList);
            end;
    end;

begin
    AList.Clear;
    FindNodesRecursive(Self, AList);
end;

function TXmlNode.FloatAllowScientific: boolean;
begin
    if assigned(Document) then
        Result := Document.FloatAllowScientific
    else
        Result := cDefaultFloatAllowScientific;
end;

function TXmlNode.FloatSignificantDigits: integer;
begin
    if assigned(Document) then
        Result := Document.FloatSignificantDigits
    else
        Result := cDefaultFloatSignificantDigits;
end;

function TXmlNode.FromAnsiString(const S: ansistring): UTF8String;
begin
    Result := sdAnsiToUtf8(S);
end;

function TXmlNode.FromUnicodeString(const W: UnicodeString): UTF8String;
begin
    Result := sdUnicodeToUtf8(W);
end;

function TXmlNode.GetAttributeByName(const AName: UTF8String): UTF8String;
begin
    if assigned(FAttributes) then
        Result := sdUTF8UnEscapeString
            (sdUTF8UnQuotedString(FAttributes.Values[AName]))
    else
        Result := '';
end;

function TXmlNode.GetAttributeByNameWide(const AName: UTF8String): UnicodeString;
begin
    Result := ToUnicodeString(GetAttributeByName(AName));
end;

function TXmlNode.GetAttributeCount: integer;
begin
    if assigned(FAttributes) then
        Result := FAttributes.Count
    else
        Result := 0;
end;

function TXmlNode.GetAttributeName(Index: integer): UTF8String;
begin
    if (Index >= 0) and (Index < AttributeCount) then
        Result := FAttributes.Names[Index];
end;

function TXmlNode.GetAttributePair(Index: integer): UTF8String;
begin
    if (Index >= 0) and (Index < AttributeCount) then
        Result := FAttributes[Index];
end;

function TXmlNode.GetAttributeValue(Index: integer): UTF8String;
var
    P: integer;
    S: UTF8String;
begin
    Result := '';
    if (Index >= 0) and (Index < AttributeCount) then
    begin
        S := FAttributes[Index];
        P := Utf8Pos('=', S);
        if P > 0 then
            Result := sdUTF8UnEscapeString(sdUTF8UnQuotedString(Copy(S, P + 1,
                MaxInt)));
    end;
end;

function TXmlNode.GetAttributeValueAsInteger(Index: integer): integer;
begin
    Result := StrToIntDef(string(GetAttributeValue(Index)), 0);
end;

function TXmlNode.GetAttributeValueAsUnicodeString(Index: integer): UnicodeString;
begin
    Result := ToUnicodeString(GetAttributeValue(Index));
end;

function TXmlNode.GetAttributeValueDirect(Index: integer): UTF8String;
var
    P: integer;
    S: UTF8String;
begin
    Result := '';
    if (Index >= 0) and (Index < AttributeCount) then
    begin
        S := FAttributes[Index];
        P := Utf8Pos('=', S);
        if P > 0 then
            Result := sdUTF8UnQuotedString(Copy(S, P + 1, MaxInt));
    end;
end;

function TXmlNode.GetBinaryEncoding: TBinaryEncodingType;
begin
    Result := xbeBinHex;
    if assigned(Document) then
        Result := Document.BinaryEncoding;
end;

function TXmlNode.GetBinaryString: RawByteString;
var
    OldEncoding: TBinaryEncodingType;
begin
    OldEncoding := BinaryEncoding;
    try
        BinaryEncoding := xbeBase64;
        SetLength(Result, BufferLength);
        if Length(Result) > 0 then
            BufferRead(Result[1], Length(Result));
    finally
        BinaryEncoding := OldEncoding;
    end;
end;

function TXmlNode.GetCascadedName: UTF8String;
var
    LName: UTF8String;
begin
    LName := UTF8String(Format('%s%.4d', [Name,
        StrToIntDef(string(AttributeByName['Index']), 0)]));
    if assigned(Parent) then
        Result := UTF8String(Format('%s_%s', [Parent.CascadedName, LName]))
    else
        Result := LName;
end;

function TXmlNode.GetFullPath: UTF8String;
begin
    Result := '/' + Name;
    if TreeDepth > 0 then
        Result := Parent.GetFullPath + Result;
end;

function TXmlNode.GetIndent: UTF8String;
var
    i: integer;
begin
    if assigned(Document) then
    begin
        case Document.XmlFormat of
            xfCompact:
                Result := '';
            xfReadable:
                for i := 0 to TreeDepth - 1 do
                    Result := Result + Document.IndentString;
        end;
    end
    else
        Result := '';
end;

function TXmlNode.GetLineFeed: UTF8String;
begin
    if assigned(Document) then
    begin
        case Document.XmlFormat of
            xfCompact:
                Result := '';
            xfReadable:
                Result := #13#10;
            else
                Result := #10;
        end;
    end
    else
        Result := '';
end;

function TXmlNode.GetNodeCount: integer;
begin
    if assigned(FNodes) then
        Result := FNodes.Count
    else
        Result := 0;
end;

function TXmlNode.GetNodes(Index: integer): TXmlNode;
begin
    if (Index >= 0) and (Index < NodeCount) then
        Result := TXmlNode(FNodes[Index])
    else
        Result := nil;
end;

function TXmlNode.GetTotalNodeCount: integer;
var
    i: integer;
begin
    Result := NodeCount;
    for i := 0 to NodeCount - 1 do
        Inc(Result, Nodes[i].TotalNodeCount);
end;

function TXmlNode.GetTreeDepth: integer;
begin
    Result := -1;
    if assigned(Parent) then
        Result := Parent.TreeDepth + 1;
end;

function TXmlNode.GetValueAsBool: boolean;
begin
    Result := sdStringToBool(FValue);
end;

function TXmlNode.GetValueAsDateTime: TDateTime;
begin
    Result := sdDateTimeFromString(ValueAsString);
end;

function TXmlNode.GetValueAsFloat: double;
var
    Code: integer;
begin
    val(string(sdUTF8StringReplace(FValue, ',', '.')), Result, Code);
    if Code > 0 then
        raise Exception.Create(sxeCannotConvertToFloat);
end;

function TXmlNode.GetValueAsInt64: int64;
begin
    Result := StrToInt64(string(FValue));
end;

function TXmlNode.GetValueAsInteger: integer;
begin
    Result := StrToInt(string(FValue));
end;

function TXmlNode.GetValueAsString: UTF8String;
begin
    Result := UnescapeString(FValue);
end;

function TXmlNode.GetValueAsUnicodeString: UnicodeString;
begin
    Result := ToUnicodeString(ValueAsString);
end;

function TXmlNode.GetWriteOnDefault: boolean;
begin
    Result := True;
    if assigned(Document) then
        Result := Document.WriteOnDefault;
end;

function TXmlNode.HasAttribute(const AName: UTF8String): boolean;
begin
    if assigned(FAttributes) then
        Result := FAttributes.IndexOfName(AName) >= 0
    else
        Result := False;
end;

function TXmlNode.IndexInParent: integer;
begin
    Result := -1;
    if assigned(Parent) then
        Result := Parent.FNodes.IndexOf(Self);
end;

function TXmlNode.IsClear: boolean;
begin
    Result := (Length(FName) = 0) and IsEmpty;
end;

function TXmlNode.IsEmpty: boolean;
begin
    Result := (Length(FValue) = 0) and (NodeCount = 0) and (AttributeCount = 0);
end;

function TXmlNode.IsEqualTo(ANode: TXmlNode; Options: TXmlCompareOptions; MismatchNodes: TList): boolean;
var
    i, Index: integer;
    NodeResult, ChildResult: boolean;
begin
    Result := False;
    NodeResult := False;
    if not assigned(ANode) then
        Exit;

    ChildResult := True;

    if (xcChildNames in Options) or (xcChildValues in Options) or
        (xcRecursive in Options) then
        for i := 0 to NodeCount - 1 do
        begin
            Index := ANode.NodeIndexByName(Nodes[i].Name);
            if Index < 0 then
            begin
                if xcChildNames in Options then
                begin
                    if assigned(MismatchNodes) then
                        MismatchNodes.Add(Nodes[i]);
                    ChildResult := False;
                end;
            end
            else
            begin
                if xcChildValues in Options then
                    if UTF8CompareText(Nodes[i].ValueAsString,
                        ANode.Nodes[Index].ValueAsString) <> 0 then
                    begin
                        if assigned(MismatchNodes) then
                            MismatchNodes.Add(Nodes[i]);
                        ChildResult := False;
                    end;
                if xcRecursive in Options then
                    if not Nodes[i].IsEqualTo(ANode.Nodes[Index], Options,
                        MismatchNodes) then
                        ChildResult := False;
            end;
        end;

    try
        NodeResult := False;

        if xcNodeName in Options then
            if UTF8CompareText(Name, ANode.Name) <> 0 then
                Exit;

        if xcNodeType in Options then
            if ElementType <> ANode.ElementType then
                Exit;

        if xcNodeValue in Options then
            if UTF8CompareText(ValueAsString, ANode.ValueAsString) <> 0 then
                Exit;

        if xcAttribCount in Options then
            if AttributeCount <> ANode.AttributeCount then
                Exit;

        if (xcAttribNames in Options) or (xcAttribValues in Options) then
            for i := 0 to AttributeCount - 1 do
            begin
                Index := ANode.AttributeIndexByname(AttributeName[i]);
                if Index < 0 then
                    if xcAttribNames in Options then
                        Exit
                    else
                        continue;
                if xcAttribValues in Options then
                    if UTF8CompareText(AttributeValue[i], ANode.AttributeValue[Index]) <> 0 then
                        Exit;
            end;

        if xcChildCount in Options then
            if NodeCount <> ANode.NodeCount then
                Exit;

        NodeResult := True;

    finally

        Result := ChildResult and NodeResult;
        if (not NodeResult) and assigned(MismatchNodes) then
            MismatchNodes.Insert(0, Self);

    end;
end;

function TXmlNode.NodeAdd(ANode: TXmlNode): integer;
begin
    if assigned(ANode) then
    begin
        ANode.Parent := Self;
        ANode.ChangeDocument(Document);
        if not assigned(FNodes) then
            FNodes := TList.Create;
        Result := FNodes.Add(ANode);
    end
    else
        Result := -1;
end;

function TXmlNode.NodeByAttributeValue(const NodeName, AttribName, AttribValue: UTF8String; ShouldRecurse: boolean): TXmlNode;
var
    i: integer;
    Node: TXmlNode;
begin
    Result := nil;
    for i := 0 to NodeCount - 1 do
    begin
        Node := Nodes[i];
        if (UTF8CompareText(Node.Name, NodeName) = 0) and Node.HasAttribute
            (AttribName) and (UTF8CompareText(Node.AttributeByName[AttribName],
            AttribValue) = 0) then
        begin
            Result := Node;
            Exit;
        end;
        if ShouldRecurse then
            Result := Node.NodeByAttributeValue(NodeName, AttribName, AttribValue,
                True);
        if assigned(Result) then
            Exit;
    end;
end;

function TXmlNode.NodeByElementType(ElementType: TXmlElementType): TXmlNode;
var
    i: integer;
begin
    Result := nil;
    for i := 0 to NodeCount - 1 do
        if Nodes[i].ElementType = ElementType then
        begin
            Result := Nodes[i];
            Exit;
        end;
end;

function TXmlNode.NodeByName(const AName: UTF8String): TXmlNode;
var
    i: integer;
begin
    Result := nil;
    for i := 0 to NodeCount - 1 do
        if UTF8CompareText(Nodes[i].Name, AName) = 0 then
        begin
            Result := Nodes[i];
            Exit;
        end;
end;

procedure TXmlNode.NodeDelete(Index: integer);
begin
    if (Index >= 0) and (Index < NodeCount) then
    begin
        TXmlNode(FNodes[Index]).Free;
        FNodes.Delete(Index);
    end;
end;

procedure TXmlNode.NodeExchange(Index1, Index2: integer);
begin
    if (Index1 >= 0) and (Index1 < NodeCount) and (Index2 >= 0) and
        (Index2 < NodeCount) then
        FNodes.Exchange(Index1, Index2);
end;

function TXmlNode.NodeExtract(ANode: TXmlNode): TXmlNode;
var
    Index: integer;
begin
    Result := nil;
    if assigned(FNodes) then
    begin
        Index := FNodes.IndexOf(ANode);
        if Index >= 0 then
        begin
            Result := ANode;
            FNodes.Delete(Index);
        end;
    end;
end;

function TXmlNode.NodeFindOrCreate(const AName: UTF8String): TXmlNode;
begin
    Result := NodeByName(AName);
    if not assigned(Result) then
        Result := NodeNew(AName);
end;

function TXmlNode.NodeIndexByName(const AName: UTF8String): integer;
begin
    Result := 0;
    while Result < NodeCount do
    begin
        if UTF8CompareText(Nodes[Result].Name, AName) = 0 then
            Exit;
        Inc(Result);
    end;
    if Result = NodeCount then
        Result := -1;
end;

function TXmlNode.NodeIndexByNameFrom(const AName: UTF8String; AFrom: integer): integer;
begin
    Result := AFrom;
    while Result < NodeCount do
    begin
        if UTF8CompareText(Nodes[Result].Name, AName) = 0 then
            Exit;
        Inc(Result);
    end;
    if Result = NodeCount then
        Result := -1;
end;

function TXmlNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
    if assigned(ANode) and assigned(FNodes) then
        Result := FNodes.IndexOf(ANode)
    else
        Result := -1;
end;

procedure TXmlNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
    if not assigned(ANode) then
        Exit;
    if (Index >= 0) and (Index <= NodeCount) then
    begin
        if not assigned(FNodes) then
            FNodes := TList.Create;
        ANode.Parent := Self;
        FNodes.Insert(Index, ANode);
    end;
end;

function TXmlNode.NodeNew(const AName: UTF8String): TXmlNode;
begin
    Result := Nodes[NodeAdd(TXmlNode.CreateName(Document, AName))];
end;

function TXmlNode.NodeNewAtIndex(Index: integer; const AName: UTF8String): TXmlNode;
begin
    if (Index >= 0) and (Index <= NodeCount) then
    begin
        Result := TXmlNode.CreateName(Document, AName);
        NodeInsert(Index, Result);
    end
    else
        Result := nil;
end;

function TXmlNode.NodeRemove(ANode: TXmlNode): integer;
begin
    Result := NodeIndexOf(ANode);
    if Result >= 0 then
        NodeDelete(Result);
end;

procedure TXmlNode.NodesByName(const AName: UTF8String; const AList: TList);
var
    i: integer;
begin
    if not assigned(AList) then
        Exit;
    AList.Clear;
    for i := 0 to NodeCount - 1 do
        if UTF8CompareText(Nodes[i].Name, AName) = 0 then
            AList.Add(Nodes[i]);
end;

procedure TXmlNode.NodesClear;
var
    i: integer;
begin
    for i := 0 to NodeCount - 1 do
        TXmlNode(FNodes[i]).Free;
    FreeAndNil(FNodes);
end;

procedure TXmlNode.ParseTag(const AValue: UTF8String; TagStart, TagClose: integer);
var
    LItems: TsdUTF8StringList;
begin
    LItems := TsdUTF8StringList.Create;
    try
        sdUTF8ParseAttributes(AValue, TagStart, TagClose, LItems);

        case ElementType of
            xeDeclaration:
                FName := 'xml';
            xeStylesheet:
            begin
                FName := 'xml-stylesheet';
                ValueDirect := sdUTF8Trim(Copy(AValue, TagStart,
                    TagClose - TagStart));
            end;
            else
                if LItems.Count = 0 then
                    raise EFilerError.Create(sxeMissingElementName);

                FName := LItems[0];
                LItems.Delete(0);
        end;
        if LItems.Count > 0 then
        begin
            CheckCreateAttributesList;
            FAttributes.Assign(LItems);
        end;

    finally
        LItems.Free;
    end;
end;

function TXmlNode.QualifyAsDirectNode: boolean;
begin
    Result := (Length(FValue) = 0) and (NodeCount = 0) and
        (ElementType = xeNormal) and not UseFullNodes and (TreeDepth > 0);
end;

function TXmlNode.ReadAttributeBool(const AName: UTF8String; ADefault: boolean): boolean;
var
    V: UTF8String;
begin
    V := AttributeByName[AName];
    try
        Result := sdStringToBool(V);
    except
        Result := ADefault;
    end;
end;

function TXmlNode.ReadAttributeDateTime(const AName: UTF8String; ADefault: TDateTime): TDateTime;
var
    V: UTF8String;
begin
    V := AttributeByName[AName];
    try
        Result := sdDateTimeFromStringDefault(V, ADefault);
    except
        Result := ADefault;
    end;
end;

function TXmlNode.ReadAttributeFloat(const AName: UTF8String; ADefault: double): double;
var
    V: UTF8String;
    Code: integer;
begin
    V := AttributeByName[AName];
    val(string(sdUTF8StringReplace(V, ',', '.')), Result, Code);
    if Code > 0 then
        Result := ADefault;
end;

function TXmlNode.ReadAttributeInteger(const AName: UTF8String; ADefault: integer): integer;
begin
    Result := StrToIntDef(string(AttributeByName[AName]), ADefault);
end;

function TXmlNode.ReadAttributeInt64(const AName: UTF8String; ADefault: int64): int64;
begin
    Result := StrToInt64Def(string(AttributeByName[AName]), ADefault);
end;

function TXmlNode.ReadAttributeString(const AName: UTF8String; const ADefault: UTF8String): UTF8String;
begin
    Result := AttributeByName[AName];
    if Length(Result) = 0 then
        Result := ADefault;
end;

function TXmlNode.ReadBool(const AName: UTF8String; ADefault: boolean): boolean;
var
    Index: integer;
begin
    Result := ADefault;
    Index := NodeIndexByName(AName);
    if Index >= 0 then
        Result := Nodes[Index].ValueAsBoolDef(ADefault);
end;

procedure TXmlNode.ReadBrush(const AName: UTF8String; ABrush: TBrush);
var
    Child: TXmlNode;
begin
    Child := NodeByName(AName);
    if assigned(Child) then
        with Child do
        begin
            ABrush.Color := ReadColor('Color', clWhite);
            ABrush.Style := TBrushStyle(ReadInteger('Style', integer(bsSolid)));
        end
    else
    begin
        ABrush.Bitmap := nil;
        ABrush.Color := clWhite;
        ABrush.Style := bsSolid;
    end;
end;

function TXmlNode.ReadColor(const AName: UTF8String; ADefault: TColor): TColor;
var
    Index: integer;
begin
    Result := ADefault;
    Index := NodeIndexByName(AName);
    if Index >= 0 then
        Result := StrToInt(string(Nodes[Index].ValueAsString));
end;

function TXmlNode.ReadDateTime(const AName: UTF8String; ADefault: TDateTime): TDateTime;
begin
    Result := sdDateTimeFromStringDefault(ReadString(AName, ''), ADefault);
end;

function TXmlNode.ReadFloat(const AName: UTF8String; ADefault: double): double;
var
    Index: integer;
begin
    Result := ADefault;
    Index := NodeIndexByName(AName);
    if Index >= 0 then
        Result := Nodes[Index].ValueAsFloatDef(ADefault);
end;

procedure TXmlNode.ReadFont(const AName: UTF8String; AFont: TFont);
var
    Child: TXmlNode;
begin
    Child := NodeByName(AName);
    AFont.Style := [];
    if assigned(Child) then
        with Child do
        begin
            AFont.Name := string(ReadString('Name', 'Arial'));
            AFont.Color := ReadColor('Color', clBlack);
            AFont.Size := ReadInteger('Size', 14);
            if ReadBool('Bold', False) then
                AFont.Style := AFont.Style + [fsBold];
            if ReadBool('Italic', False) then
                AFont.Style := AFont.Style + [fsItalic];
            if ReadBool('Underline', False) then
                AFont.Style := AFont.Style + [fsUnderline];
            if ReadBool('Strikeout', False) then
                AFont.Style := AFont.Style + [fsStrikeout];
        end
    else
    begin
        AFont.Name := 'Arial';
        AFont.Color := clBlack;
        AFont.Size := 14;
    end;
end;


procedure TXmlNode.ReadFromStream(S: TStream);
var
    Ch: AnsiChar;
    i: integer;
    TagIndex: integer;
    V: UTF8String;
    Len: integer;
    Node: TXmlNode;
    NodeValue: UTF8String;
    ValuePos, ValueLen: integer;
    ClosePos: integer;
    HasCR: boolean;
    HasSubtags: boolean;
    Words: TsdUTF8StringList;
    IsDirect: boolean;
    Reader: TsdSurplusReader;

    procedure AddCharDataNode(PreserveWhiteSpace: boolean);
    var
        V: UTF8String;
        Node: TXmlNode;
        L: integer;
    begin
        if ValuePos > 0 then
        begin
            V := Copy(NodeValue, 1, ValuePos);

            if PreserveWhiteSpace then
                L := Length(V)
            else
                L := Length(sdUTF8Trim(V));

            if L > 0 then
            begin
                Node := TXmlNode.CreateType(Document, xeCharData);
                Node.ValueDirect := V;
                NodeAdd(Node);
            end;
            ValuePos := 0;
        end;
    end;

begin
    if AbortParsing then
        Exit;
    Clear;
    ValuePos := 0;
    ValueLen := 80;
    SetLength(NodeValue, ValueLen);
    HasCR := False;
    HasSubtags := False;
    Reader := TsdSurplusReader.Create(S);
    try
        if not Reader.ReadCharSkipBlanks(Ch) then
            Exit;

        if Ch = '<' then
        begin
            TagIndex := ReadOpenTag(Reader);
            if TagIndex >= 0 then
            begin
                try
                    ElementType := cTags[TagIndex].Style;
                    case ElementType of
                        xeNormal, xeDeclaration, xeStylesheet:
                        begin
                            ReadStringFromStreamUntil(Reader, cTags[TagIndex].Close, V,
                                True);
                            Len := Length(V);

                            IsDirect := False;
                            if (ElementType = xeNormal) and (Len > 0) then
                                if V[Len] = '/' then
                                begin
                                    Dec(Len);
                                    IsDirect := True;
                                end;
                            ParseTag(V, 1, Len + 1);

                            if assigned(Document) then
                            begin
                                Document.DoNodeNew(Self);
                                if AbortParsing then
                                    Exit;
                            end;

                            if IsDirect or (ElementType in [xeDeclaration,
                                xeStylesheet]) then
                                Exit;

                            repeat

                                if S.Read(Ch, 1) <> 1 then
                                    raise EFilerError.CreateFmt(sxeMissingCloseTag, [Name]);

                                if Ch = '<' then
                                begin
                                    if not Reader.ReadCharSkipBlanks(Ch) then
                                        raise EFilerError.CreateFmt
                                            (sxeMissingDataAfterGreaterThan, [Name]);
                                    if Ch = '/' then
                                    begin

                                        if not ReadStringFromStreamUntil(Reader, '>', V,
                                            True) then
                                            raise EFilerError.CreateFmt
                                                (sxeMissingLessThanInCloseTag, [Name]);
                                        if UTF8CompareText(sdUTF8Trim(V), Name) <> 0 then
                                            raise EFilerError.CreateFmt
                                                (sxeIncorrectCloseTag,
                                                [Name]);
                                        V := '';
                                        Break;

                                    end
                                    else
                                    begin

                                        AddCharDataNode(False);

                                        HasCR := False;

                                        HasSubtags := True;
                                        S.Seek(-2, soCurrent);
                                        Node := TXmlNode.Create(Document);
                                        NodeAdd(Node);
                                        Node.ReadFromStream(S);

                                        if assigned(Document) and Document.DropCommentsOnParse and
                                            (Node.ElementType = xeComment) then
                                            NodeDelete(NodeIndexOf(Node));

                                    end;
                                end
                                else
                                begin

                                    if Ch = #13 then
                                        HasCR := True;

                                    Inc(ValuePos);
                                    if ValuePos > ValueLen then
                                    begin
                                        Inc(ValueLen, cNodeValueBuf);
                                        SetLength(NodeValue, ValueLen);
                                    end;
                                    NodeValue[ValuePos] := Ch;

                                end;
                            until False or AbortParsing;

                            AddCharDataNode(not HasSubtags);

                            if HasSubtags and HasCR then
                            begin
                                for i := 0 to NodeCount - 1 do
                                    if Nodes[i].ElementType = xeCharData then
                                    begin
                                        ClosePos := Length(Nodes[i].FValue);
                                        while (ClosePos > 0) and
                                            (Nodes[i].FValue[ClosePos] in [#10,
                                                #13, ' ']) do
                                            Dec(ClosePos);
                                        Nodes[i].FValue := Copy(Nodes[i].FValue, 1,
                                            ClosePos);
                                    end;
                            end;

                            if NodeCount > 0 then
                                if Nodes[0].ElementType = xeCharData then
                                begin
                                    ValueDirect := Nodes[0].ValueDirect;
                                    NodeDelete(0);
                                end;

                        end;
                        xeDoctype:
                        begin
                            Name := 'DTD';
                            if assigned(Document) then
                            begin
                                Document.DoNodeNew(Self);
                                if AbortParsing then
                                    Exit;
                            end;
                            if assigned(Document) then
                                Document.ParseDTD(Self, S);
                        end;
                        xeElement, xeAttList, xeEntity, xeNotation:
                        begin
                            ReadStringFromStreamWithQuotes(S, cTags[TagIndex].Close, V);
                            Len := Length(V);
                            Words := TsdUTF8StringList.Create;
                            try
                                sdUTF8ParseAttributes(V, 1, Len + 1, Words);
                                if Words.Count > 0 then
                                begin
                                    Name := Words[0];
                                    Words.Delete(0);
                                end;
                                ValueDirect := sdUTF8Trim(Words.Text);
                            finally
                                Words.Free;
                            end;
                            if assigned(Document) then
                            begin
                                Document.DoNodeNew(Self);
                                if AbortParsing then
                                    Exit;
                            end;
                        end;
                        else
                            case ElementType of
                                xeComment:
                                    Name := 'Comment';
                                xeCData:
                                    Name := 'CData';
                                xeExclam:
                                    Name := 'Special';
                                xeQuestion:
                                    Name := 'Special';
                                else
                                    Name := 'Unknown';
                            end;
                            if assigned(Document) then
                            begin
                                Document.DoNodeNew(Self);
                                if AbortParsing then
                                    Exit;
                            end;

                            ReadStringFromStreamUntil(Reader, cTags[TagIndex].Close, V,
                                False);
                            ValueDirect := V;
                    end;
                finally
                    if assigned(Document) and not AbortParsing then
                    begin
                        Document.DoProgress(S.Position);
                        Document.DoNodeLoaded(Self);
                    end;
                end;
            end;
        end;
    finally
        Reader.Free;
    end;
end;

procedure TXmlNode.ReadFromString(const AValue: UTF8String);
var
    S: TStream;
begin
    S := TsdUTF8StringStream.Create(AValue);
    try
        ReadFromStream(S);
    finally
        S.Free;
    end;
end;

function TXmlNode.ReadInt64(const AName: UTF8String; ADefault: int64): int64;
var
    Index: integer;
begin
    Result := ADefault;
    Index := NodeIndexByName(AName);
    if Index >= 0 then
        Result := Nodes[Index].ValueAsInt64Def(ADefault);
end;

function TXmlNode.ReadInteger(const AName: UTF8String; ADefault: integer): integer;
var
    Index: integer;
begin
    Result := ADefault;
    Index := NodeIndexByName(AName);
    if Index >= 0 then
        Result := Nodes[Index].ValueAsIntegerDef(ADefault);
end;


procedure TXmlNode.ReadPen(const AName: UTF8String; APen: TPen);
var
    Child: TXmlNode;
begin
    Child := NodeByName(AName);
    if assigned(Child) then
        with Child do
        begin
            APen.Color := ReadColor('Color', clBlack);
            APen.Mode := TPenMode(ReadInteger('Mode', integer(pmCopy)));
            APen.Style := TPenStyle(ReadInteger('Style', integer(psSolid)));
            APen.Width := ReadInteger('Width', 1);
        end
    else
    begin
        APen.Color := clBlack;
        APen.Mode := pmCopy;
        APen.Style := psSolid;
        APen.Width := 1;
    end;
end;

function TXmlNode.ReadString(const AName: UTF8String; const ADefault: UTF8String): UTF8String;
var
    Index: integer;
begin
    Result := ADefault;
    Index := NodeIndexByName(AName);
    if Index >= 0 then
        Result := Nodes[Index].ValueAsString;
end;

function TXmlNode.ReadUnicodeString(const AName: UTF8String; const ADefault: UnicodeString): UnicodeString;
begin
    Result := ToUnicodeString(ReadString(AName, FromUnicodeString(ADefault)));
end;

procedure TXmlNode.ResolveEntityReferences;

    function SplitReference(const AValue: UTF8String; var Text1, Text2: UTF8String): UTF8String;
    var
        P: integer;
    begin
        Result := '';
        P := Utf8Pos('&', AValue);
        Text1 := '';
        Text2 := AValue;
        if P = 0 then
            Exit;
        Text1 := Copy(AValue, 1, P - 1);
        Text2 := Copy(AValue, P + 1, Length(AValue));
        P := Utf8Pos(';', Text2);
        if P = 0 then
            Exit;
        Result := Copy(Text2, 1, P - 1);
        Text2 := Copy(Text2, P + 1, Length(Text2));
    end;

    function ReplaceEntityReferenceByNodes(ARoot: TXmlNode; const AValue: UTF8String; var InsertPos: integer; var Text1, Text2: UTF8String): boolean;
    var
        Reference: UTF8String;
        Entity: UTF8String;
        Node: TXmlNode;
        S: TStream;
    begin
        Result := False;
        Reference := SplitReference(AValue, Text1, Text2);
        if (Length(Reference) = 0) or not assigned(Document) then
            Exit;

        Entity := Document.EntityByName[Reference];

        if (Length(Entity) > 0) and (Utf8Pos('<', Entity) > 0) then
        begin
            S := TsdUTF8StringStream.Create(Entity);
            try
                while S.Position < S.Size do
                begin
                    Node := TXmlNode.Create(Document);
                    Node.ReadFromStream(S);
                    if Node.IsEmpty then
                        Node.Free
                    else
                    begin
                        ARoot.NodeInsert(InsertPos, Node);
                        Inc(InsertPos);
                        Result := True;
                    end;
                end;
            finally
                S.Free;
            end;
        end;
    end;

var
    i: integer;
    InsertPos: integer;
    Text1, Text2: UTF8String;
    Node: TXmlNode;
    V, Reference, Replace, Entity, First, Last: UTF8String;
begin
    if Length(FValue) > 0 then
    begin
        if ElementType = xeNormal then
        begin
            InsertPos := 0;
            if ReplaceEntityReferenceByNodes(Self, FValue, InsertPos, Text1,
                Text2) then
            begin
                FValue := Text1;
                if Length(sdUTF8Trim(Text2)) > 0 then
                begin
                    Node := TXmlNode.CreateType(Document, xeCharData);
                    Node.ValueDirect := Text2;
                    NodeInsert(InsertPos, Node);
                end;
            end;
        end
        else if (ElementType = xeCharData) and assigned(Parent) then
        begin
            InsertPos := Parent.NodeIndexOf(Self);
            if ReplaceEntityReferenceByNodes(Parent, FValue, InsertPos, Text1,
                Text2) then
            begin
                FValue := Text1;
                if Length(sdUTF8Trim(FValue)) = 0 then
                    FValue := '';
                if Length(sdUTF8Trim(Text2)) > 0 then
                begin
                    Node := TXmlNode.CreateType(Document, xeCharData);
                    Node.ValueDirect := Text2;
                    Parent.NodeInsert(InsertPos, Node);
                end;
            end;
        end;
    end;

    for i := 0 to AttributeCount - 1 do
    begin
        Last := AttributeValue[i];
        V := '';
        repeat
            Reference := SplitReference(Last, First, Last);
            Replace := '';
            if Length(Reference) > 0 then
            begin
                Entity := Document.EntityByName[Reference];
                if Length(Entity) > 0 then
                    Replace := Entity
                else
                    Replace := '&' + Reference + ';';
            end;
            V := V + First + Replace;
        until Length(Reference) = 0;
        V := V + Last;
        AttributeValue[i] := V;
    end;

    i := 0;
    while i < NodeCount do
    begin
        Nodes[i].ResolveEntityReferences;
        Inc(i);
    end;

    for i := NodeCount - 1 downto 0 do
        if (Nodes[i].ElementType = xeCharData) and
            (Length(Nodes[i].ValueDirect) = 0) then
            NodeDelete(i);
end;

procedure TXmlNode.SetAttributeByName(const AName, Value: UTF8String);
begin
    CheckCreateAttributesList;
    FAttributes.Values[AName] := sdUTF8QuotedString(sdUTF8EscapeString(Value));
end;

procedure TXmlNode.SetAttributeByNameWide(const AName: UTF8String; const Value: UnicodeString);
begin
    SetAttributeByName(AName, FromUnicodeString(Value));
end;

procedure TXmlNode.SetAttributeName(Index: integer; const Value: UTF8String);
var
    S: UTF8String;
    P: integer;
begin
    if (Index >= 0) and (Index < AttributeCount) then
    begin
        S := FAttributes[Index];
        P := Utf8Pos('=', S);
        if P > 0 then
            FAttributes[Index] := Value + '=' + Copy(S, P + 1, MaxInt);
    end;
end;

procedure TXmlNode.SetAttributeValue(Index: integer; const Value: UTF8String);
begin
    if (Index >= 0) and (Index < AttributeCount) then
        FAttributes[Index] := AttributeName[Index] + '=' + sdUTF8QuotedString
            (sdUTF8EscapeString(Value));
end;

procedure TXmlNode.SetAttributeValueAsInteger(Index: integer; const Value: integer);
begin
    SetAttributeValue(Index, IntToUTF8Str(Value));
end;

procedure TXmlNode.SetAttributeValueAsUnicodeString(Index: integer; const Value: UnicodeString);
begin
    SetAttributeValue(Index, FromUnicodeString(Value));
end;

procedure TXmlNode.SetAttributeValueDirect(Index: integer; const Value: UTF8String);
begin
    if (Index >= 0) and (Index < AttributeCount) then
        FAttributes[Index] := AttributeName[Index] + '=' + sdUTF8QuotedString
            (Value);
end;

procedure TXmlNode.SetBinaryEncoding(const Value: TBinaryEncodingType);
begin
    if assigned(Document) then
        Document.BinaryEncoding := Value;
end;

procedure TXmlNode.SetBinaryString(const Value: RawByteString);
var
    OldEncoding: TBinaryEncodingType;
begin
    OldEncoding := BinaryEncoding;
    try
        BinaryEncoding := xbeBase64;
        if Length(Value) = 0 then
        begin
            ValueAsString := '';
            Exit;
        end;
        BufferWrite(Value[1], Length(Value));
    finally
        BinaryEncoding := OldEncoding;
    end;
end;

procedure TXmlNode.SetName(const Value: UTF8String);
var
    i: integer;
begin
    if FName <> Value then
    begin
        for i := 1 to Length(Value) do
            if Value[i] in cControlChars then
                raise Exception.Create(Format(sxeIllegalCharInNodeName, [Value]));
        FName := Value;
    end;
end;

procedure TXmlNode.SetValueAsBool(const Value: boolean);
begin
    FValue := sdStringFromBool(Value);
end;

procedure TXmlNode.SetValueAsDateTime(const Value: TDateTime);
begin
    ValueAsString := sdDateTimeToString(Value);
end;

procedure TXmlNode.SetValueAsFloat(const Value: double);
begin
    FValue := sdWriteNumber(Value, FloatSignificantDigits, FloatAllowScientific);
end;

procedure TXmlNode.SetValueAsInt64(const Value: int64);
begin
    FValue := IntToUTF8Str(Value);
end;

procedure TXmlNode.SetValueAsInteger(const Value: integer);
begin
    FValue := IntToUTF8Str(Value);
end;

procedure TXmlNode.SetValueAsString(const AValue: UTF8String);
begin
    FValue := sdUTF8EscapeString(AValue);
end;

procedure TXmlNode.SetValueAsUnicodeString(const Value: UnicodeString);
begin
    ValueAsString := FromUnicodeString(Value);
end;

procedure TXmlNode.SortChildNodes(Compare: TXMLNodeCompareFunction; Info: TPointer);

    function DoNodeCompare(Node1, Node2: TXmlNode): integer;
    begin
        if assigned(Compare) then
            Result := Compare(Node1, Node2, Info)
        else if assigned(Document) and assigned(Document.OnNodeCompare) then
            Result := Document.OnNodeCompare(Document, Node1, Node2, Info)
        else
            Result := UTF8CompareText(Node1.Name, Node2.Name);
    end;

    procedure QuickSort(iLo, iHi: integer);
    var
        Lo, Hi, Mid: longint;
    begin
        Lo := iLo;
        Hi := iHi;
        Mid := (Lo + Hi) div 2;
        repeat
            while DoNodeCompare(Nodes[Lo], Nodes[Mid]) < 0 do
                Inc(Lo);
            while DoNodeCompare(Nodes[Hi], Nodes[Mid]) > 0 do
                Dec(Hi);
            if Lo <= Hi then
            begin
                NodeExchange(Lo, Hi);
                if Mid = Lo then
                    Mid := Hi
                else if Mid = Hi then
                    Mid := Lo;
                Inc(Lo);
                Dec(Hi);
            end;
        until Lo > Hi;
        if Hi > iLo then
            QuickSort(iLo, Hi);
        if Lo < iHi then
            QuickSort(Lo, iHi);
    end;

begin
    if NodeCount > 1 then
        QuickSort(0, NodeCount - 1);
end;

function TXmlNode.ToUnicodeString(const S: UTF8String): UnicodeString;
begin
    Result := sdUtf8ToUnicode(S);
end;

function TXmlNode.UnescapeString(const AValue: UTF8String): UTF8String;
begin
    Result := sdUTF8UnEscapeString(AValue);
end;

function TXmlNode.UseFullNodes: boolean;
begin
    Result := False;
    if assigned(Document) then
        Result := Document.UseFullNodes;
end;

function TXmlNode.ValueAsBoolDef(ADefault: boolean): boolean;
var
    Ch: AnsiChar;
begin
    Result := ADefault;
    if Length(FValue) = 0 then
        Exit;
    Ch := sdUpCase(FValue[1]);
    if Ch in ['T', 'Y'] then
    begin
        Result := True;
        Exit;
    end;
    if Ch in ['F', 'N'] then
    begin
        Result := False;
        Exit;
    end;
end;

function TXmlNode.ValueAsDateTimeDef(ADefault: TDateTime): TDateTime;
begin
    Result := sdDateTimeFromStringDefault(ValueAsString, ADefault);
end;

function TXmlNode.ValueAsFloatDef(ADefault: double): double;
var
    Code: integer;
begin
    try
        val(string(sdUTF8StringReplace(FValue, ',', '.')), Result, Code);
        if Code > 0 then
            Result := ADefault;
    except
        Result := ADefault;
    end;
end;

function TXmlNode.ValueAsInt64Def(ADefault: int64): int64;
begin
    Result := StrToInt64Def(string(FValue), ADefault);
end;

function TXmlNode.ValueAsIntegerDef(ADefault: integer): integer;
begin
    Result := StrToIntDef(string(FValue), ADefault);
end;

procedure TXmlNode.WriteAttributeBool(const AName: UTF8String; AValue: boolean; ADefault: boolean);
var
    Index: integer;
begin
    if WriteOnDefault or (AValue <> ADefault) then
    begin
        Index := AttributeIndexByname(AName);
        if Index >= 0 then
            AttributeValue[Index] := sdStringFromBool(AValue)
        else
            AttributeAdd(AName, sdStringFromBool(AValue));
    end;
end;

procedure TXmlNode.WriteAttributeDateTime(const AName: UTF8String; AValue, ADefault: TDateTime);
var
    Index: integer;
begin
    if WriteOnDefault or (AValue <> ADefault) then
    begin
        Index := AttributeIndexByname(AName);
        if Index >= 0 then
            AttributeValue[Index] := sdDateTimeToString(AValue)
        else
            AttributeAdd(AName, sdDateTimeToString(AValue));
    end;
end;

procedure TXmlNode.WriteAttributeFloat(const AName: UTF8String; AValue, ADefault: double);
var
    Index: integer;
    S: UTF8String;
begin
    if WriteOnDefault or (AValue <> ADefault) then
    begin
        Index := AttributeIndexByname(AName);
        S := sdWriteNumber(AValue, FloatSignificantDigits, FloatAllowScientific);
        if Index >= 0 then
            AttributeValue[Index] := S
        else
            AttributeAdd(AName, S);
    end;
end;

procedure TXmlNode.WriteAttributeInteger(const AName: UTF8String; AValue: integer; ADefault: integer);
var
    Index: integer;
begin
    if WriteOnDefault or (AValue <> ADefault) then
    begin
        Index := AttributeIndexByname(AName);
        if Index >= 0 then
            AttributeValue[Index] := IntToUTF8Str(AValue)
        else
            AttributeAdd(AName, IntToUTF8Str(AValue));
    end;
end;

procedure TXmlNode.WriteAttributeInt64(const AName: UTF8String; const AValue: int64; ADefault: int64);
var
    Index: integer;
begin
    if WriteOnDefault or (AValue <> ADefault) then
    begin
        Index := AttributeIndexByname(AName);
        if Index >= 0 then
            AttributeValue[Index] := IntToUTF8Str(AValue)
        else
            AttributeAdd(AName, IntToUTF8Str(AValue));
    end;
end;

procedure TXmlNode.WriteAttributeString(const AName, AValue, ADefault: UTF8String);
var
    Index: integer;
begin
    if WriteOnDefault or (AValue <> ADefault) then
    begin
        Index := AttributeIndexByname(AName);
        if Index >= 0 then
            AttributeValue[Index] := AValue
        else
            AttributeAdd(AName, AValue);
    end;
end;

procedure TXmlNode.WriteBool(const AName: UTF8String; AValue: boolean; ADefault: boolean);
const
    cBoolValues: array [boolean] of UTF8String = ('False', 'True');
begin
    if WriteOnDefault or (AValue <> ADefault) then
        with NodeFindOrCreate(AName) do
            ValueAsString := cBoolValues[AValue];
end;


procedure TXmlNode.WriteBrush(const AName: UTF8String; ABrush: TBrush);
begin
    with NodeFindOrCreate(AName) do
    begin
        WriteColor('Color', ABrush.Color, clBlack);
        WriteInteger('Style', integer(ABrush.Style), 0);
    end;
end;

procedure TXmlNode.WriteColor(const AName: UTF8String; AValue, ADefault: TColor);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        WriteHex(AName, ColorToRGB(AValue), 8, 0);
end;


procedure TXmlNode.WriteDateTime(const AName: UTF8String; AValue, ADefault: TDateTime);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        WriteString(AName, sdDateTimeToString(AValue), '');
end;

procedure TXmlNode.WriteFloat(const AName: UTF8String; AValue: double; ADefault: double);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        with NodeFindOrCreate(AName) do
            ValueAsString := sdWriteNumber(AValue, FloatSignificantDigits,
                FloatAllowScientific);
end;


procedure TXmlNode.WriteFont(const AName: UTF8String; AFont: TFont);
begin
    with NodeFindOrCreate(AName) do
    begin
        WriteString('Name', UTF8String(AFont.Name), 'Arial');
        WriteColor('Color', AFont.Color, clBlack);
        WriteInteger('Size', AFont.Size, 14);
        WriteBool('Bold', fsBold in AFont.Style, False);
        WriteBool('Italic', fsItalic in AFont.Style, False);
        WriteBool('Underline', fsUnderline in AFont.Style, False);
        WriteBool('Strikeout', fsStrikeout in AFont.Style, False);
    end;
end;

procedure TXmlNode.WriteHex(const AName: UTF8String; AValue, Digits: integer; ADefault: integer);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        with NodeFindOrCreate(AName) do
            ValueAsString := '$' + UTF8String(IntToHex(AValue, Digits));
end;

function TXmlNode.WriteInnerTag: UTF8String;
var
    i: integer;
begin
    Result := '';
    for i := 0 to AttributeCount - 1 do
        Result := Result + ' ' + AttributePair[i];
    if QualifyAsDirectNode then
        Result := Result + '/';
end;

procedure TXmlNode.WriteInt64(const AName: UTF8String; AValue, ADefault: int64);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        with NodeFindOrCreate(AName) do
            ValueAsString := IntToUTF8Str(AValue);
end;

procedure TXmlNode.WriteInteger(const AName: UTF8String; AValue: integer; ADefault: integer);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        with NodeFindOrCreate(AName) do
            ValueAsString := IntToUTF8Str(AValue);
end;

procedure TXmlNode.WritePen(const AName: UTF8String; APen: TPen);
begin
    with NodeFindOrCreate(AName) do
    begin
        WriteColor('Color', APen.Color, clBlack);
        WriteInteger('Mode', integer(APen.Mode), 0);
        WriteInteger('Style', integer(APen.Style), 0);
        WriteInteger('Width', APen.Width, 0);
    end;
end;

procedure TXmlNode.WriteString(const AName, AValue: UTF8String; const ADefault: UTF8String);
begin
    if WriteOnDefault or (AValue <> ADefault) then
        with NodeFindOrCreate(AName) do
            ValueAsString := AValue;
end;

procedure TXmlNode.WriteToStream(S: TStream);
var
    i: integer;
    Indent: UTF8String;
    LFeed: UTF8String;
    Line: UTF8String;
    ThisNode, NextNode: TXmlNode;
    AddLineFeed: boolean;
begin
    Indent := GetIndent;
    LFeed := GetLineFeed;

    Line := Indent;

    case ElementType of
        xeDeclaration:
        begin
            DeleteEmptyAttributes;
            Line := Indent + '<?xml' + WriteInnerTag + '?>';
        end;
        xeStylesheet:
            Line := Indent + '<?xml-stylesheet' + WriteInnerTag + '?>';
        xeDoctype:
        begin
            if NodeCount = 0 then
                Line := Indent + '<!DOCTYPE ' + Name + ' ' + ValueDirect + '>'
            else
            begin
                Line := Indent + '<!DOCTYPE ' + Name + ' ' + ValueDirect + ' [' +
                    LFeed;
                sdUTF8WriteStringToStream(S, Line);
                for i := 0 to NodeCount - 1 do
                begin
                    Nodes[i].WriteToStream(S);
                    sdUTF8WriteStringToStream(S, LFeed);
                end;
                Line := ']>';
            end;
        end;
        xeElement:
            Line := Indent + '<!ELEMENT ' + Name + ' ' + ValueDirect + '>';
        xeAttList:
            Line := Indent + '<!ATTLIST ' + Name + ' ' + ValueDirect + '>';
        xeEntity:
            Line := Indent + '<!ENTITY ' + Name + ' ' + ValueDirect + '>';
        xeNotation:
            Line := Indent + '<!NOTATION ' + Name + ' ' + ValueDirect + '>';
        xeComment:
            Line := Indent + '<!--' + ValueDirect + '-->';
        xeCData:
            Line := Indent + '<![CDATA[' + ValueDirect + ']]>';
        xeExclam:
            Line := Indent + '<!' + ValueDirect + '>';
        xeQuestion:
            Line := Indent + '<?' + ValueDirect + '?>';
        xeCharData:
            Line := FValue;
        xeUnknown:
            Line := Indent + '<' + ValueDirect + '>';
        xeNormal:
        begin

            Line := Line + '<' + FName + WriteInnerTag + '>';

            Line := Line + FValue;
            if (NodeCount > 0) then
                Line := Line + LFeed;

            sdUTF8WriteStringToStream(S, Line);

            for i := 0 to NodeCount - 1 do
            begin
                ThisNode := Nodes[i];
                NextNode := Nodes[i + 1];
                ThisNode.WriteToStream(S);
                AddLineFeed := True;
                if ThisNode.ElementType = xeCharData then
                    AddLineFeed := False;
                if assigned(NextNode) then
                    if NextNode.ElementType = xeCharData then
                        AddLineFeed := False;
                if AddLineFeed then
                    sdUTF8WriteStringToStream(S, LFeed);
            end;

            Line := '';
            if not QualifyAsDirectNode then
            begin
                if NodeCount > 0 then
                    Line := Indent;
                Line := Line + '</' + FName + '>';
            end;
        end;
        else
            raise EFilerError.Create(sxeIllegalElementType);
    end;
    sdUTF8WriteStringToStream(S, Line);

    if assigned(Document) then
        Document.DoProgress(S.Position);
end;

function TXmlNode.WriteToString: UTF8String;
var
    S: TsdUTF8StringStream;
begin
    S := TsdUTF8StringStream.Create('');
    try
        WriteToStream(S);
        Result := S.DataString;
    finally
        S.Free;
    end;
end;

procedure TXmlNode.WriteUnicodeString(const AName: UTF8String; const AValue: UnicodeString; const ADefault: UnicodeString);
begin
    WriteString(AName, FromUnicodeString(AValue), FromUnicodeString(ADefault));
end;

function TXmlNodeList.ByAttribute(const AName, AValue: UTF8String): TXmlNode;
var
    i: integer;
begin
    for i := 0 to Count - 1 do
        if UTF8CompareText(Items[i].AttributeByName[AName], AValue) = 0 then
        begin
            Result := Items[i];
            Exit;
        end;
    Result := nil;
end;

function TXmlNodeList.GetItems(Index: integer): TXmlNode;
begin
    Result := TXmlNode(Get(Index));
end;

procedure TXmlNodeList.SetItems(Index: integer; const Value: TXmlNode);
begin
    Put(Index, TPointer(Value));
end;


procedure TNativeXml.Assign(Source: TPersistent);

    procedure SetDocumentRecursively(ANode: TXmlNode; ADocument: TNativeXml);
    var
        i: integer;
    begin
        ANode.Document := ADocument;
        for i := 0 to ANode.NodeCount - 1 do
            SetDocumentRecursively(ANode.Nodes[i], ADocument);
    end;

begin
    if Source is TNativeXml then
    begin
        FBinaryEncoding := TNativeXml(Source).FBinaryEncoding;
        FDropCommentsOnParse := TNativeXml(Source).FDropCommentsOnParse;
        FExternalEncoding := TNativeXml(Source).FExternalEncoding;
        FParserWarnings := TNativeXml(Source).FParserWarnings;
        FIndentString := TNativeXml(Source).FIndentString;
        FUseFullNodes := TNativeXml(Source).FUseFullNodes;
        FWriteOnDefault := TNativeXml(Source).FWriteOnDefault;
        FXmlFormat := TNativeXml(Source).FXmlFormat;
        FRootNodes.Assign(TNativeXml(Source).FRootNodes);
        SetDocumentRecursively(FRootNodes, Self);
    end
    else if Source is TXmlNode then
    begin
        FRootNodes.Assign(Source);
        SetDocumentRecursively(FRootNodes, Self);
    end
    else
        inherited;
end;

procedure TNativeXml.Clear;
var
    Node: TXmlNode;
begin
    SetDefaults;
    FRootNodes.Clear;
    Node := TXmlNode.CreateType(Self, xeDeclaration);
    Node.Name := 'xml';
    Node.AttributeAdd('version', cDefaultVersionString);
    Node.AttributeAdd('encoding', cDefaultEncodingString);
    FRootNodes.NodeAdd(Node);
    FRootNodes.NodeNew('');
end;

procedure TNativeXml.CopyFrom(Source: TNativeXml);
begin
    if not assigned(Source) then
        Exit;
    Assign(Source);
end;

constructor TNativeXml.Create;
begin
    inherited Create;
    FRootNodes := TXmlNode.Create(Self);
    Clear;
end;

constructor TNativeXml.CreateName(const ARootName: UTF8String);
begin
    Create;
    Root.Name := ARootName;
end;

destructor TNativeXml.Destroy;
begin
    FreeAndNil(FRootNodes);
    inherited;
end;

procedure TNativeXml.DoNodeLoaded(Node: TXmlNode);
begin
    if assigned(FOnNodeLoaded) then
        FOnNodeLoaded(Self, Node);
end;

procedure TNativeXml.DoNodeNew(Node: TXmlNode);
begin
    if assigned(FOnNodeNew) then
        FOnNodeNew(Self, Node);
end;

procedure TNativeXml.DoProgress(Size: integer);
begin
    if assigned(FOnProgress) then
        FOnProgress(Self, Size);
end;

procedure TNativeXml.DoUnicodeLoss(Sender: TObject);
begin
    if assigned(FOnUnicodeLoss) then
        FOnUnicodeLoss(Self);
end;

function TNativeXml.GetCommentString: UTF8String;
var
    Node: TXmlNode;
begin
    Result := '';
    Node := FRootNodes.NodeByElementType(xeComment);
    if assigned(Node) then
        Result := Node.ValueAsString;
end;

function TNativeXml.GetEncodingString: UTF8String;
begin
    Result := '';
    if FRootNodes.NodeCount > 0 then
        if FRootNodes[0].ElementType = xeDeclaration then
            Result := FRootNodes[0].AttributeByName['encoding'];
end;

function TNativeXml.GetEntityByName(AName: UTF8String): UTF8String;
var
    i, j: integer;
begin
    Result := '';
    for i := 0 to FRootNodes.NodeCount - 1 do
        if FRootNodes[i].ElementType = xeDoctype then
            with FRootNodes[i] do
            begin
                for j := 0 to NodeCount - 1 do
                    if (Nodes[j].ElementType = xeEntity) and (Nodes[j].Name = AName) then
                    begin
                        Result := sdUTF8UnQuotedString
                            (sdUTF8Trim(Nodes[j].ValueDirect));
                        Exit;
                    end;
            end;
end;

function TNativeXml.GetRoot: TXmlNode;
begin
    Result := FRootNodes.NodeByElementType(xeNormal);
end;

function TNativeXml.GetStyleSheetNode: TXmlNode;
begin
    Result := FRootNodes.NodeByElementType(xeStylesheet);
    if not assigned(Result) then
    begin
        Result := TXmlNode.CreateType(Self, xeStylesheet);
        FRootNodes.NodeInsert(1, Result);
    end;
end;

function TNativeXml.GetUtf8Encoded: boolean;
begin
    Result := True;
end;

function TNativeXml.GetVersionString: UTF8String;
begin
    Result := '';
    if FRootNodes.NodeCount > 0 then
        if FRootNodes[0].ElementType = xeDeclaration then
            Result := FRootNodes[0].AttributeByName['version'];
end;

function TNativeXml.IsEmpty: boolean;
var
    R: TXmlNode;
begin
    Result := True;
    R := GetRoot;
    if assigned(R) then
        Result := R.IsClear;
end;

function TNativeXml.LineFeed: UTF8String;
begin
    case XmlFormat of
        xfReadable:
            Result := #13#10;
        xfCompact:
            Result := #10;
        else
            Result := #10;
    end;
end;

procedure TNativeXml.LoadFromFile(const AFileName: string);
var
    S: TStream;
begin
    S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
        LoadFromStream(S);
    finally
        S.Free;
    end;
end;

procedure TNativeXml.LoadFromStream(Stream: TStream);
var
    B: TsdBufferedReadStream;
begin
    B := TsdBufferedReadStream.Create(Stream, False);
    try
        FCodecStream := TsdUtf8Stream.Create(B);
        try
            FCodecStream.OnUnicodeLoss := @DoUnicodeLoss;
            ReadFromStream(FCodecStream);
            FExternalEncoding := FCodecStream.Encoding;
        finally
            FreeAndNil(FCodecStream);
        end;
    finally
        B.Free;
    end;
end;

procedure TNativeXml.ParseDTD(ANode: TXmlNode; S: TStream);

    procedure ParseMarkupDeclarations;
    var
        Ch: AnsiChar;
    begin
        repeat
            ANode.NodeNew('').ReadFromStream(S);
            repeat
                if S.Read(Ch, 1) = 0 then
                    Exit;
            until not (Ch in cControlChars);
            if Ch = ']' then
                Break;
            S.Seek(-1, soCurrent);
        until False;
    end;

var
    Prework: UTF8String;
    Ch: AnsiChar;
    Words: TsdUTF8StringList;
begin
    Prework := '';
    repeat
        if S.Read(Ch, 1) = 0 then
            Exit;
        if Ch in ['[', '>'] then
            Break;
        Prework := Prework + UTF8String(Ch);
    until False;
    Words := TsdUTF8StringList.Create;
    try
        sdUTF8ParseAttributes(Prework, 1, Length(Prework) + 1, Words);
        if Words.Count > 0 then
        begin
            ANode.Name := Words[0];
            Words.Delete(0);
            ANode.ValueDirect := sdUTF8Trim(sdUTF8StringReplace(Words.Text, #13#10,
                ' '));
        end;
    finally
        Words.Free;
    end;

    if Ch = '[' then
    begin

        ParseMarkupDeclarations;

        repeat
            if S.Read(Ch, 1) = 0 then
                Exit;
            if Ch = '>' then
                Break;
        until False;

    end;
end;

procedure TNativeXml.ReadFromStream(S: TStream);
var
    i: integer;
    Node: TXmlNode;
    Enc: UTF8String;
    NormalCount, DeclarationCount, DoctypeCount, CDataCount: integer;
    NormalPos, DoctypePos: integer;
begin
    FAbortParsing := False;
    with FRootNodes do
    begin
        Clear;
        DoProgress(0);
        repeat
            Node := NodeNew('');
            Node.ReadFromStream(S);
            if AbortParsing then
                Exit;

            if Node.ElementType = xeDeclaration then
            begin
                if Node.HasAttribute('encoding') then
                    Enc := Node.AttributeByName['encoding'];
                if assigned(FCodecStream) and (Enc = 'UTF-8') then
                    FCodecStream.Encoding := seUTF8;
            end;
            if Node.IsClear then
                NodeDelete(NodeCount - 1);
        until S.Position >= S.Size;
        DoProgress(S.Size);

        NormalCount := 0;
        DeclarationCount := 0;
        DoctypeCount := 0;
        CDataCount := 0;
        NormalPos := -1;
        DoctypePos := -1;
        for i := 0 to NodeCount - 1 do
        begin
            case Nodes[i].ElementType of
                xeNormal:
                begin
                    Inc(NormalCount);
                    NormalPos := i;
                end;
                xeDeclaration:
                    Inc(DeclarationCount);
                xeDoctype:
                begin
                    Inc(DoctypeCount);
                    DoctypePos := i;
                end;
                xeCData:
                    Inc(CDataCount);
            end;
        end;

        if NormalCount = 0 then
            raise EFilerError.Create(sxeNoRootElement);

        if FParserWarnings then
        begin

            if NormalCount > 1 then
                raise EFilerError.Create(sxeMoreThanOneRootElement);

            if DeclarationCount > 1 then
                raise EFilerError.Create(sxeMoreThanOneDeclaration);

            if DeclarationCount = 1 then
                if Nodes[0].ElementType <> xeDeclaration then
                    raise EFilerError.Create(sxeDeclarationMustBeFirstElem);

            if DoctypeCount > 1 then
                raise EFilerError.Create(sxeMoreThanOneDoctype);

            if (DoctypeCount = 1) and (DoctypePos > NormalPos) then
                raise EFilerError.Create(sxeDoctypeAfterRootElement);

            if CDataCount > 0 then
                raise EFilerError.Create(sxeCDATAInRoot);
        end;
    end;
end;

procedure TNativeXml.ReadFromString(const AValue: UTF8String);
var
    S: TStream;
begin
    S := TsdUTF8StringStream.Create(AValue);
    try
        ReadFromStream(S);
    finally
        S.Free;
    end;
end;

procedure TNativeXml.ResolveEntityReferences;
begin
    if assigned(Root) then
        Root.ResolveEntityReferences;
end;

procedure TNativeXml.SaveToFile(const AFileName: string);
var
    S: TStream;
begin
    S := TFileStream.Create(AFileName, fmCreate);
    try
        SaveToStream(S);
    finally
        S.Free;
    end;
end;

procedure TNativeXml.SaveToStream(Stream: TStream);
var
    B: TsdBufferedWriteStream;
begin
    B := TsdBufferedWriteStream.Create(Stream, False);
    try
        FCodecStream := TsdUtf8Stream.Create(B);
        try
            FCodecStream.Encoding := FExternalEncoding;
            WriteToStream(FCodecStream);
        finally
            FreeAndNil(FCodecStream);
        end;
    finally
        B.Free;
    end;
end;

procedure TNativeXml.SetCommentString(const Value: UTF8String);
var
    Node: TXmlNode;
begin
    Node := FRootNodes.NodeByElementType(xeComment);
    if not assigned(Node) and (Length(Value) > 0) then
    begin
        Node := TXmlNode.CreateType(Self, xeComment);
        FRootNodes.NodeInsert(1, Node);
    end;
    if assigned(Node) then
        Node.ValueAsString := Value;
end;

procedure TNativeXml.SetDefaults;
begin
    FExternalEncoding := cDefaultExternalEncoding;
    FXmlFormat := cDefaultXmlFormat;
    FWriteOnDefault := cDefaultWriteOnDefault;
    FBinaryEncoding := cDefaultBinaryEncoding;
    FIndentString := cDefaultIndentString;
    FDropCommentsOnParse := cDefaultDropCommentsOnParse;
    FUseFullNodes := cDefaultUseFullNodes;
    FFloatAllowScientific := cDefaultFloatAllowScientific;
    FFloatSignificantDigits := cDefaultFloatSignificantDigits;
    FOnNodeNew := nil;
    FOnNodeLoaded := nil;
end;

procedure TNativeXml.SetEncodingString(const Value: UTF8String);
var
    Node: TXmlNode;
begin
    if Value = GetEncodingString then
        Exit;
    Node := FRootNodes[0];
    if not assigned(Node) or (Node.ElementType <> xeDeclaration) then
    begin
        Node := TXmlNode.CreateType(Self, xeDeclaration);
        FRootNodes.NodeInsert(0, Node);
    end;
    if assigned(Node) then
        Node.AttributeByName['encoding'] := Value;
end;

procedure TNativeXml.SetVersionString(const Value: UTF8String);
var
    Node: TXmlNode;
begin
    if Value = GetVersionString then
        Exit;
    Node := FRootNodes[0];
    if not assigned(Node) or (Node.ElementType <> xeDeclaration) then
    begin
        if Length(Value) > 0 then
        begin
            Node := TXmlNode.CreateType(Self, xeDeclaration);
            FRootNodes.NodeInsert(0, Node);
        end;
    end;
    if assigned(Node) then
        Node.AttributeByName['version'] := Value;
end;

procedure TNativeXml.WriteToStream(S: TStream);
var
    i: integer;
begin
    if not assigned(Root) and FParserWarnings then
        raise EFilerError.Create(sxeRootElementNotDefined);

    DoProgress(0);

    for i := 0 to FRootNodes.NodeCount - 1 do
    begin
        FRootNodes[i].WriteToStream(S);
        sdUTF8WriteStringToStream(S, LineFeed);
    end;

    DoProgress(S.Size);
end;

function TNativeXml.WriteToString: UTF8String;
var
    S: TsdUTF8StringStream;
begin
    S := TsdUTF8StringStream.Create('');
    try
        WriteToStream(S);
        Result := S.DataString;
    finally
        S.Free;
    end;
end;


constructor TsdCodecStream.Create(AStream: TStream);
begin
    inherited Create;
    FStream := AStream;
end;

function TsdCodecStream.InternalRead(var Buffer; Offset, Count: longint): longint;
var
    i, j: integer;
    BOM: array [0 .. 3] of byte;
    BytesRead: integer;
    Found: boolean;
begin
    Result := 0;
    if FMode = umUnknown then
    begin
        FMode := umRead;
        if not assigned(FStream) then
            raise EStreamError.Create(sxeCodecStreamNotAssigned);

        FEncoding := seAnsi;
        BytesRead := FStream.Read(BOM, 4);
        for i := 0 to cBomInfoCount - 1 do
        begin
            Found := True;
            for j := 0 to Min(BytesRead, cBomInfo[i].Len) - 1 do
            begin
                if BOM[j] <> cBomInfo[i].BOM[j] then
                begin
                    Found := False;
                    Break;
                end;
            end;
            if Found then
                Break;
        end;
        if Found then
        begin
            FEncoding := cBomInfo[i].Encoding;
            FWriteBom := cBomInfo[i].HasBOM;
        end
        else
        begin
            FEncoding := seAnsi;
            FWriteBom := False;
        end;

        if FEncoding in [seUCS4BE, seUCS4_2143, seUCS4_3412, seEBCDIC] then
            raise EStreamError.Create(sxeUnsupportedEncoding);

        if FWriteBom then
            FStream.Seek(cBomInfo[i].Len - BytesRead, soCurrent)
        else
            FStream.Seek(-BytesRead, soCurrent);

        if FEncoding in [se16BitBE, seUTF16BE] then
            FSwapByteOrder := True;

    end;

    if FMode <> umRead then
        raise EStreamError.Create(sxeCannotReadCodecForWriting);

    if Count <> 1 then
        raise EStreamError.Create(sxeCannotReadMultipeChar);

    TBytes(Buffer)[Offset] := ReadByte;
    if TBytes(Buffer)[Offset] <> 0 then
        Result := 1;
end;

function TsdCodecStream.Read(var Buffer; Count: longint): longint;
begin
    Result := InternalRead(Buffer, 0, Count);
end;


function TsdCodecStream.ReadByte: byte;
begin
    Result := 0;
end;

function TsdCodecStream.InternalSeek(Offset: longint; Origin: TSeekOrigin): longint;
begin
    Result := 0;
    if FMode = umUnknown then
        raise EStreamError.Create(sxeCannotSeekBeforeReadWrite);

    if Origin = soCurrent then
    begin
        if Offset = 0 then
        begin
            Result := FStream.Position;
            Exit;
        end;
        if (FMode = umRead) and ((Offset = -1) or (Offset = -2)) then
        begin
            FBuffer := '';
            case Offset of
                -1:
                    FStream.Seek(FPosMin1, soBeginning);
                -2:
                    FStream.Seek(FPosMin2, soBeginning);
            end;
            Exit;
        end;
    end;
    if (Origin = soEnd) and (Offset = 0) then
    begin
        Result := FStream.Size;
        Exit;
    end;
    if Origin = soBeginning then
        Exit;
    raise EStreamError.Create(sxeCannotPerformSeek);
end;


function TsdCodecStream.Seek(Offset: longint; Origin: word): longint;
begin
    Result := InternalSeek(Offset, TSeekOrigin(Origin));
end;

procedure TsdCodecStream.StorePrevPositions;
begin
    FPosMin2 := FPosMin1;
    FPosMin1 := FStream.Position;
end;

function TsdCodecStream.InternalWrite(const Buffer; Offset, Count: longint): longint;
var
    i: integer;
begin
    if FMode = umUnknown then
    begin
        FMode := umWrite;

        if FEncoding in [seUCS4BE, seUCS4_2143, seUCS4_3412, seEBCDIC] then
            raise EStreamError.Create(sxeUnsupportedEncoding);

        for i := 0 to cBomInfoCount - 1 do
            if cBomInfo[i].Encoding = FEncoding then
            begin
                FWriteBom := cBomInfo[i].HasBOM;
                Break;
            end;

        if FWriteBom then
            FStream.WriteBuffer(cBomInfo[i].BOM, cBomInfo[i].Len);

        if FEncoding in [se16BitBE, seUTF16BE] then
            FSwapByteOrder := True;
    end;

    if FMode <> umWrite then
        raise EStreamError.Create(sxeCannotWriteCodecForReading);
    WriteBuf(Buffer, Offset, Count);
    Result := Count;
end;

function TsdCodecStream.Write(const Buffer; Count: longint): longint;
begin
    Result := InternalWrite(byte(Buffer), 0, Count);
end;

procedure TsdCodecStream.WriteBuf(const Buffer; Offset, Count: longint);
var
    i: integer;
begin
    for i := 0 to Count - 1 do
        WriteByte(TBytes(Buffer)[Offset + i]);
end;

procedure TsdCodecStream.WriteByte(const B: byte);
begin
end;

function TsdUtf8Stream.ReadByte: byte;
var
    B, B1, B2, B3: byte;
    W: word;
    SA: ansistring;
begin
    Result := 0;

    if (Length(FBuffer) = 0) or (FBufferPos > Length(FBuffer)) then
    begin
        StorePrevPositions;
        FBufferPos := 1;
        case FEncoding of
            seAnsi:
            begin
                B := 0;
                FStream.Read(B, 1);
                SA := AnsiChar(B);
                FBuffer := sdAnsiToUtf8(SA);
            end;
            seUTF8:
            begin
                B1 := 0;
                FStream.Read(B1, 1);
                FBuffer := AnsiChar(B1);
                if (B1 and $80) > 0 then
                begin
                    if (B1 and $20) <> 0 then
                    begin
                        B2 := 0;
                        FStream.Read(B2, 1);
                        FBuffer := FBuffer + UTF8String(AnsiChar(B2));
                    end;
                    B3 := 0;
                    FStream.Read(B3, 1);
                    FBuffer := FBuffer + UTF8String(AnsiChar(B3));
                end;
            end;
            se16BitBE, se16BitLE, seUTF16BE, seUTF16LE:
            begin
                W := 0;
                FStream.Read(W, 2);
                if FSwapByteOrder then
                    W := swap(W);
                FBuffer := sdUnicodeToUtf8(char(W and $FF));
            end;
            else
                raise EStreamError.Create(sxeUnsupportedEncoding);
        end;
    end;

    if (FBufferPos > 0) and (FBufferPos <= Length(FBuffer)) then
        Result := byte(FBuffer[FBufferPos]);
    Inc(FBufferPos);
end;

procedure TsdUtf8Stream.WriteBuf(const Buffer; Offset, Count: longint);
begin
    case FEncoding of
        seUTF8:
        begin
            if StreamWrite(FStream, Buffer, Offset, Count) <> Count then
                raise EStreamError.Create(sxeCannotWriteToOutputStream);
        end
        else
            inherited;
    end;
end;

procedure TsdUtf8Stream.WriteByte(const B: byte);
var
    SA: ansistring;
    SW: UnicodeString;
    MustWrite: boolean;
begin
    case FEncoding of
        seAnsi, se16BitBE, se16BitLE, seUTF16BE, seUTF16LE:
        begin
            MustWrite := True;
            case Length(FBuffer) of
                0:
                begin
                    FBuffer := AnsiChar(B);
                    if (B and $80) <> 0 then
                        MustWrite := False;
                end;
                1:
                begin
                    FBuffer := FBuffer + UTF8String(AnsiChar(B));
                    if (byte(FBuffer[1]) and $20) <> 0 then
                        MustWrite := False;
                end;
                2:
                    FBuffer := FBuffer + UTF8String(AnsiChar(B));
            end;
            if MustWrite then
            begin
                if FEncoding = seAnsi then
                begin
                    SA := sdUtf8ToAnsi(FBuffer);
                    if Length(SA) = 1 then
                        if FStream.Write(SA[1], 1) <> 1 then
                            raise EStreamError.Create(sxeCannotWriteToOutputStream);
                end
                else
                begin
                    SW := sdUtf8ToUnicode(FBuffer);
                    if Length(SW) = 1 then
                        if FStream.Write(SW[1], 2) <> 2 then
                            raise EStreamError.Create(sxeCannotWriteToOutputStream);
                end;
                FBuffer := '';
            end;
        end;
        seUTF8:
        begin
            if FStream.Write(B, 1) <> 1 then
                raise EStreamError.Create(sxeCannotWriteToOutputStream);
        end;
        else
            raise EStreamError.Create(sxeUnsupportedEncoding);
    end;
end;

const
    cMaxBufferSize = $10000;

procedure TsdBufferedReadStream.CheckPosition;
var
    NewPage: integer;
    FStartPos: longint;
begin
    NewPage := FPosition div cMaxBufferSize;
    FBufPos := FPosition mod cMaxBufferSize;

    if (NewPage <> FPage) then
    begin
        FPage := NewPage;

        FStartPos := FPage * cMaxBufferSize;
        FBufSize := Min(cMaxBufferSize, FStream.Size - FStartPos);

        FStream.Seek(FStartPos, soBeginning);
        if FBufSize > 0 then
            FStream.Read(FBuffer^, FBufSize);
    end;
    FMustCheck := False;
end;

constructor TsdBufferedReadStream.Create(AStream: TStream; Owned: boolean);
begin
    inherited Create;
    FStream := AStream;
    FOwned := Owned;
    FMustCheck := True;
    FPage := -1;
    ReallocMem(FBuffer, cMaxBufferSize);
end;

destructor TsdBufferedReadStream.Destroy;
begin
    if FOwned then
        FreeAndNil(FStream);
    ReallocMem(FBuffer, 0);
    inherited;
end;

function TsdBufferedReadStream.Read(var Buffer; Count: longint): longint;
var
    Packet: PByte;
    PacketCount: integer;
begin
    if FMustCheck then
        CheckPosition;

    if (Count = 1) and (FBufPos < FBufSize - 1) then
    begin
        byte(Buffer) := FBuffer^[FBufPos];
        Inc(FBufPos);
        Inc(FPosition);
        Result := 1;
        Exit;
    end;

    Packet := @Buffer;
    Result := 0;
    while Count > 0 do
    begin
        PacketCount := Min(FBufSize - FBufPos, Count);
        if PacketCount <= 0 then
            Exit;
        Move(FBuffer^[FBufPos], Packet^, PacketCount);
        Dec(Count, PacketCount);
        Inc(Packet, PacketCount);
        Inc(Result, PacketCount);
        Inc(FPosition, PacketCount);
        Inc(FBufPos, PacketCount);
        if FBufPos >= FBufSize then
            CheckPosition;
    end;
end;

function TsdBufferedReadStream.Seek(Offset: longint; Origin: word): longint;
begin
    case Origin of
        soFromBeginning:
            FPosition := Offset;
        soFromCurrent:
        begin
            if Offset = 0 then
            begin
                Result := FPosition;
                Exit;
            end;
            FPosition := FPosition + Offset;
        end;
        soFromEnd:
            FPosition := FStream.Size + Offset;
    end;
    Result := FPosition;
    FMustCheck := True;
end;

function TsdBufferedReadStream.Write(const Buffer; Count: longint): longint;
begin
    raise EStreamError.Create(sxeCannotWriteCodecForReading);
end;

constructor TsdBufferedWriteStream.Create(AStream: TStream; Owned: boolean);
begin
    inherited Create;
    FStream := AStream;
    FOwned := Owned;
    ReallocMem(FBuffer, cMaxBufferSize);
end;

destructor TsdBufferedWriteStream.Destroy;
begin
    Flush;
    if FOwned then
        FreeAndNil(FStream);
    ReallocMem(FBuffer, 0);
    inherited;
end;

procedure TsdBufferedWriteStream.Flush;
begin
    if FBufPos > 0 then
    begin
        FStream.Write(FBuffer^, FBufPos);
        FBufPos := 0;
    end;
end;

function TsdBufferedWriteStream.Read(var Buffer; Count: longint): longint;
begin
    raise EStreamError.Create(sxeCannotReadCodecForWriting);
end;

function TsdBufferedWriteStream.Seek(Offset: longint; Origin: word): longint;
begin
    case Origin of
        soFromBeginning:
            if Offset = FPosition then
            begin
                Result := FPosition;
                Exit;
            end;
        soFromCurrent:
        begin
            if Offset = 0 then
            begin
                Result := FPosition;
                Exit;
            end;
        end;
        soFromEnd:
            if Offset = 0 then
            begin
                Result := FPosition;
                Exit;
            end;
    end;
    raise EStreamError.Create(sxeCannotPerformSeek);
end;

function TsdBufferedWriteStream.Write(const Buffer; Count: longint): longint;
var
    Packet: PByte;
    PacketCount: integer;
begin
    if (FBufPos + Count < cMaxBufferSize) then
    begin
        Move(Buffer, FBuffer^[FBufPos], Count);
        Inc(FBufPos, Count);
        Inc(FPosition, Count);
        Result := Count;
        Exit;
    end;

    Packet := @Buffer;
    Result := 0;
    while Count > 0 do
    begin
        PacketCount := Min(cMaxBufferSize - FBufPos, Count);
        if PacketCount <= 0 then
            Exit;
        Move(Packet^, FBuffer^[FBufPos], PacketCount);
        Dec(Count, PacketCount);
        Inc(Result, PacketCount);
        Inc(FPosition, PacketCount);
        Inc(Packet, PacketCount);
        Inc(FBufPos, PacketCount);
        if FBufPos = cMaxBufferSize then
            Flush;
    end;
end;

constructor TsdSurplusReader.Create(AStream: TStream);
begin
    inherited Create;
    FStream := AStream;
end;

function TsdSurplusReader.ReadChar(var Ch: AnsiChar): integer;
begin
    if Length(FSurplus) > 0 then
    begin
        Ch := FSurplus[1];
        FSurplus := Copy(FSurplus, 2, Length(FSurplus) - 1);
        Result := 1;
    end
    else
        Result := FStream.Read(Ch, 1);
end;

function TsdSurplusReader.ReadCharSkipBlanks(var Ch: AnsiChar): boolean;
begin
    Result := False;
    repeat
        if ReadChar(Ch) = 0 then
            Exit;
        if not (Ch in cControlChars) then
            Break;
    until False;
    Result := True;
end;


procedure TsdStringBuilder.AddChar(Ch: AnsiChar);
begin
    Inc(FCurrentIdx);
    Reallocate(FCurrentIdx);
    FData[FCurrentIdx] := Ch;
end;

procedure TsdStringBuilder.AddString(var S: UTF8String);
var
    Count: integer;
begin
    Count := System.Length(S);
    if Count = 0 then
        Exit;
    Reallocate(FCurrentIdx + Count);
    Move(S[1], FData[FCurrentIdx + 1], Count);
    Inc(FCurrentIdx, Count);
end;

procedure TsdStringBuilder.Clear;
begin
    FCurrentIdx := 0;
end;

function TsdStringBuilder.StringCopy(AFirst, ALength: integer): UTF8String;
begin
    if ALength > FCurrentIdx - AFirst + 1 then
        ALength := FCurrentIdx - AFirst + 1;
    Result := Copy(FData, AFirst, ALength);
end;

constructor TsdStringBuilder.Create;
begin
    inherited Create;
    SetLength(FData, 64);
end;

function TsdStringBuilder.GetData(Index: integer): AnsiChar;
begin
    Result := FData[Index];
end;

procedure TsdStringBuilder.Reallocate(RequiredLength: integer);
begin
    while System.Length(FData) < RequiredLength do
        SetLength(FData, System.Length(FData) * 2);
end;

function TsdStringBuilder.Value: UTF8String;
begin
    Result := Copy(FData, 1, FCurrentIdx);
end;

end.