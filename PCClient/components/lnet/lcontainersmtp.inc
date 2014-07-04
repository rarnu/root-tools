const
  MAX_FRONT_ITEMS = 10;

type
  TLFront = class // it's a queue ladies and gents
   protected
    FEmptyItem: TLSMTPStatusRec;
    FItems: array[0..MAX_FRONT_ITEMS-1] of TLSMTPStatusRec;
    FTop, FBottom: Integer;
    FCount: Integer;
    function GetEmpty: Boolean;
   public
    constructor Create(const DefaultItem: TLSMTPStatusRec);
    function First: TLSMTPStatusRec;
    function Remove: TLSMTPStatusRec;
    function Insert(const Value: TLSMTPStatusRec): Boolean;
    procedure Clear;
    property Count: Integer read FCount;
    property Empty: Boolean read GetEmpty;
  end;


