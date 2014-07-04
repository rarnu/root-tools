const
  MAX_FRONT_ITEMS = 10;

type
  TLFront = class // it's a queue ladies and gents
   protected
    FEmptyItem: TLFTPStatusRec;
    FItems: array[0..MAX_FRONT_ITEMS-1] of TLFTPStatusRec;
    FTop, FBottom: Integer;
    FCount: Integer;
    function GetEmpty: Boolean;
   public
    constructor Create(const DefaultItem: TLFTPStatusRec);
    function First: TLFTPStatusRec;
    function Remove: TLFTPStatusRec;
    function Insert(const Value: TLFTPStatusRec): Boolean;
    procedure Clear;
    property Count: Integer read FCount;
    property Empty: Boolean read GetEmpty;
  end;


