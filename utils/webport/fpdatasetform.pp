{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpdatasetform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphtml, htmldefs, htmlwriter, db, htmlelements;

type

  THTMLDatasetFormProducer = class;
  TFormFieldItem = class;
  TFormButtonItem = class;

  TFieldCellEvent = procedure (Sender:THTMLDatasetFormProducer; FieldDef:TFormFieldItem;
                      IsLabel:boolean; Cell : THTMLCustomelement) of object;
  TButtonEvent = procedure (Sender:THTMLDatasetFormProducer; ButtonDef:TFormButtonItem;
                      Button : THTMLAttrsElement) of object;
  TProducerEvent = procedure (Sender:THTMLDatasetFormProducer;  FieldDef:TFormFieldItem;
                      Producer:THTMLContentProducer) of object;
  TProducerSetRecordEvent = procedure (Sender:THTMLDatasetFormProducer) of object;
  THTMLElementEvent = procedure (Sender:THTMLDatasetFormProducer; element : THTMLCustomElement) of object;
  TFieldCheckEvent = procedure (aField:TField; var check:boolean) of object;
  
  TFieldItemEvent = procedure (Sender:TFormFieldItem; var aValue : string) of object;
  
  TFormInputType = (fittext,fitpassword,fitcheckbox,fitradio,fitfile,fithidden,
                    fitproducer,fittextarea,fitrecordselection,fitlabel);

  { TTablePosition }

  TTablePosition = class (TPersistent)
  private
    FAlignHor: THTMLalign;
    FAlignVer: THTMLvalign;
    FColSpan: integer;
    FLeft: integer;
    FRowSpan: integer;
    FTop: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor create;
  published
    property Left : integer read FLeft write FLeft;
    property Top : integer read FTop write FTop;
    property ColSpan : integer read FColSpan write FColSpan default 1;
    property RowSpan : integer read FRowSpan write FRowSpan default 1;
    property AlignVertical : THTMLvalign read FAlignVer write FAlignVer default vaEmpty;
    property AlignHorizontal : THTMLalign read FAlignHor write FAlignHor default alEmpty;
  end;

  { TFormFieldItem }

  TFormFieldItem = class (TCollectionItem)
  private
    FAction: string;
    FField: TField;
    FFieldName: string;
    FInputType: TFormInputType;
    FLabelCaption: string;
    FLabelAbove : boolean;
    FLabelPos: TTablePosition;
    FProducer: THTMLContentProducer;
    FValuePos: TTablePosition;
    FName : String;

    FOnGetValue: TFieldItemEvent;
    FOnGetLabel: TFieldItemEvent;
    FOnGetAction: TFieldItemEvent;
    procedure SetLabelPos(const AValue: TTablePosition);
    procedure SetValuePos(const AValue: TTablePosition);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    Function  GetDisplayName : String; override;
    Procedure SetDisplayName(const Value : String); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function getValue : String; virtual;
    function getLabel : String; virtual;
    function getAction : String; virtual;
    property Field : TField read FField;
  published
    Property Name : String Read GetDisplayName Write SetDisplayName;
    property Fieldname : string read FFieldName write FFieldname;
      // the field to show/edit
    property LabelCaption : string read FLabelCaption write FLabelCaption;
      // the text to show for the control
    property LabelPos : TTablePosition read FLabelPos write SetLabelPos;
      // place of the label in the table-grid
    property LabelAbove : boolean read FLabelAbove write FLabelAbove default false;
      // if not SeparateLabel then place a <BR> between label and edit/value
    property ValuePos : TTablePosition read FValuePos write SetValuePos;
      // place of the value in the table-grid
    { only when editting: }
    property InputType : TFormInputType read FInputType write FInputType default fittext;
      // the type of form control to use
    property Producer : THTMLContentProducer read FProducer write FProducer;
      // the producer to include when generating the value
    { only when showing: }
    property Action : string read FAction write FAction;
      // the link to include in the value
    property OnGetValue : TFieldItemEvent read FOnGetValue write FOnGetValue;
    property OnGetLabel : TFieldItemEvent read FOnGetLabel write FOnGetLabel;
    property OnGetAction : TFieldItemEvent read FOnGetAction write FOnGetAction;
  end;

  { TFormFieldCollection }

  TFormFieldCollection = class (TCollection)
  private
    function GetItem(index : integer): TFormFieldItem;
    procedure SetItem(index : integer; const AValue: TFormFieldItem);
  public
    constructor create;
    function AddField (afieldname, acaption : string) : TFormFieldItem;
    Function FindItem(AName : String): TFormFieldItem;
    Function IndexofItem(AName : String) : Integer;
    property Items [index : integer] : TFormFieldItem read GetItem write SetItem;
  end;

  TFormButtonType = (fbtSubmit, fbtReset, fbtPushbutton, fbtInputSubmit,fbtInputReset,fbtInputPushButton);
  TImagePlace = (ipOnly, ipBefore, ipAfter, ipUnder, ipAbove);

  { TFormButtonItem }

  TFormButtonItem = class (TCollectionItem)
  private
    FButtonType: TFormButtonType;
    FCaption: string;
    FImage: string;
    FName: string;
    FImagePlace: TImagePlace;
    FValue: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor create (ACollection : TCollection); override;
  published
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
    property Caption : string read FCaption write FCaption;
      // Text on button, or as hint with image
    property Image : string read FImage write FImage;
      // Image to show on the button
    property ImagePlace : TImagePlace read FImagePlace write FImagePlace;
      // where the image is placed regarding from the caption.
      // if ipOnly; the caption is placed in the alternate text of the image (hint)
    property ButtonType : TFormButtonType read FButtonType write FButtonType default fbtPushButton;
      // Where the button is used for
  end;

  { TFormButtonCollection }

  TFormButtonCollection = class (TCollection)
  private
    function GetItem(index : integer): TFormButtonItem;
    procedure SetItem(index : integer; const AValue: TFormButtonItem);
  public
    constructor create;
    function AddButton (aname, avalue, acaption : string) : TFormButtonItem;
    function AddButton (aname, acaption : string) : TFormButtonItem;
    function AddButton (acaption : string) : TFormButtonItem;
    property Items [index : integer] : TFormButtonItem read GetItem write SetItem;
  end;

  TCellType = (ctEmpty, ctInput, ctLabel, ctProducer, ctSpanned);

  { TTableCell }

  TTableCell = class (TCollectionItem)
  private
    FAlignHor: THTMLalign;
    FAlignVer: THTMLvalign;
    FCaption: string;
    FCellType: TCellType;
    FChecked: boolean;
    FDisabled: boolean;
    FColSpan: integer;
    FEndRow: boolean;
    FFormField: TFormFieldItem;
    FIncludeBreak: boolean;
    FInputType: TFormInputType;
    FIsLabel: boolean;
    FLink: string;
    FMaxLength: integer;
    FName: string;
    FProducer: THTMLContentProducer;
    FRowSpan: integer;
    FSize: integer;
    FSpanned: boolean;
    FValue: string;
    procedure WriteLabel(aWriter: THTMLWriter);
  public
    function WriteContent (aWriter : THTMLWriter) : THTMLCustomElement;
    function WriteHeader (aWriter : THTMLWriter) : THTMLCustomElement;
    property FormField : TFormFieldItem read FFormField write FFormField;
      // field definition that origintated this cell
    property IsLabel : boolean read FIsLabel write FIsLabel;
      // Label or Value ?
    property Caption : string read FCaption write FCaption;
      // label to place with the edit/value if not separateLabel
    property IncludeBreak : boolean read FIncludeBreak write FIncludeBreak;
      // place <br> between label and edit/value if label is included in cell
    property CellType : TCellType read FCellType write FCellType;
    { Cell properties: }
    property ColSpan : integer read FColSpan write FColSpan;
    property RowSpan : integer read FRowSpan write FRowSpan;
    property AlignVertical : THTMLvalign read FAlignVer write FAlignVer default vaEmpty;
    property AlignHorizontal : THTMLalign read FAlignHor write FAlignHor default alEmpty;
    property Value : string read FValue write FValue;
          // Contains the text for labels, or the value for input, or unused for producer and empty
    { properties to correctly generate the rows and the table ends }
    property EndOfRow : boolean read FEndRow write FEndRow;
    property SpannedOut : boolean read FSpanned write FSpanned;
    { only for input: }
    property Name : string read FName write FName;
          // name of the control
    property InputType : TFormInputType read FInputType write FInputType;
          // type of the input element
    property Size : integer read FSize write FSize;
          // size of text input element
    property MaxLength : integer read FMaxLength write FMaxLength;
          // MaxLength of text input element
    property Checked : boolean read FChecked write FChecked;
          // checked or not for radio,checkbox
    property Disabled : boolean read FDisabled write FDisabled;
          // disabled or not for radio,checkbox
    { only for labels: }
    property Link : string read FLink write FLink;
          // link to place around the text
    { only for producers: }
    property Producer : THTMLContentProducer read FProducer write FProducer;
          // producer to include
  end;

  { TTableDef }

  TTableDef = class (TCollection)
  private
    fCols, fRows : integer;
    function GetCell(x, y : integer): TTableCell;
    function GetItem(index: integer): TTableCell;
  public
    Constructor Create (acols, arows : integer);
    function CopyTablePosition (position : TTablePosition) : TTableCell;
    property Cells [x,y : integer] : TTableCell read GetCell; default;
    property items [index:integer] : TTableCell read GetItem;
  end;

  TButtonVerPosition = (bvpTop, bvpBottom);
  TButtonVerPositionSet = set of TButtonVerPosition;
  TButtonHorPosition = (bhpLeft, bhpCenter, bhpJustify, bhpRight);
  TFormMethod = (fmNone, fmGet, fmPost);

  { THTMLDatasetFormProducer }

  THTMLDatasetFormProducer = class (THTMLContentProducer)
  private
    FAfterSetRecord: TProducerSetRecordEvent;
    FOnInitializeProducer : TProducerEvent;
    FOnFieldChecked : TFieldCheckEvent;
    FAfterTBodyCreate,
    FAfterTableCreate : THTMLElementEvent;
    FAfterButtonCreate: TButtonEvent;
    FAfterCellCreate: TFieldCellEvent;
    Fbuttonrow: TFormButtonCollection;
    FButtonsHor: TButtonHorPosition;
    FButtonsVer: TButtonVerPositionSet;
    FControls: TFormFieldCollection;
    FDatasource: TDatasource;
    FFormAction: string;
    FFormMethod: TFormMethod;
    FIncludeHeader: boolean;
    FSeparateLabel: boolean;
    FTableCols: integer;
    FTableRows: integer;
    FTableDef : TTableDef;
    FPage: integer;
    FRecordsPerPage: integer;
    procedure SetIncludeHeader(const AValue: boolean);
    procedure SetSeparateLabel(const AValue: boolean);
    procedure WriteButtons (aWriter : THTMLWriter);
    procedure WriteTableDef (aWriter : THTMLWriter);
    procedure WriteHeaderTableDef (aWriter : THTMLWriter);
    procedure CorrectCellSpans;
    procedure SearchControlFields;
  protected
    procedure FillTableDef (IsHeader:boolean); virtual;
    procedure PlaceFieldValue(aControldef : TFormFieldItem); virtual;
    procedure ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean); virtual; abstract;
    function StartForm (aWriter : THTMLWriter) : THTMLCustomElement; virtual;
    procedure EndForm (aWriter : THTMLWriter); virtual;
    property TableDef : TTableDef read FTableDef;
    function SingleRecord : boolean; dynamic;
      // generate form for 1 record or for the selected pages
    property RecordsPerPage : integer read FRecordsPerPage write FRecordsPerPage default 20;
      // number of records to show
    property Page : integer read FPage write FPage default -1;
      // page to show. -1 shows all records. zero based
    property IncludeHeader : boolean read FIncludeHeader write SetIncludeHeader;
      // create a header cell for each control
  public
    constructor create (aOwner : TComponent); override;
    destructor destroy; override;
    function WriteContent (aWriter : THTMLWriter) : THTMLCustomElement; override;
  published
    property FormAction : string read FFormAction write FFormAction;
      // action of the form (link), if not given; don't use a form element
    property FormMethod : TFormMethod read FFormMethod write FFormMethod;
      // method of the form, Get or Post
    Property DataSource : TDataSource read FDataSource write FDataSource;
      // the data to use
    property Controls : TFormFieldCollection read FControls write FControls;
      // configuration of the fields and how to generate the html
    property SeparateLabel : boolean read FSeparateLabel write SetSeparateLabel;
      // place label and value/edit in same table cell
    property buttonrow : TFormButtonCollection read Fbuttonrow write Fbuttonrow;
      // buttons to place in the form
    property TableCols : integer read FTableCols write FTableCols default 2;
      // number columns in the grid for 1 record
    property TableRows : integer read FTableRows write FTableRows;
      // number of rows in the grid for 1 record
    property ButtonsHorizontal : TButtonHorPosition read FButtonsHor write FButtonsHor default bhpleft;
      // where to place the buttons horizontally
    property ButtonsVertical : TButtonVerPositionSet read FButtonsVer write FButtonsVer default [bvpTop,bvpBottom];
      // where to place the buttons vertically
    property OnInitializeProducer : TProducerEvent read FOnInitializeProducer write FOnInitializeProducer;
      // Called before the producer creates it's HTML code
    property AfterCellCreate : TFieldCellEvent read FAfterCellCreate write FAfterCellCreate;
      // Called after each creation of a cell in the table makeup in the form
    property AfterButtonCreate : TButtonEvent read FAfterButtonCreate write FAfterButtonCreate;
      // Called after each creation of a button
    property AfterTableCreate : THTMLElementEvent read FAfterTableCreate write FAfterTableCreate;
      // Called after the creation of the table
    property AfterTBodyCreate : THTMLElementEvent read FAfterTBodyCreate write FAfterTBodyCreate;
      // Called after finishing the tbody of each record
    property AfterSetRecord : TProducerSetRecordEvent read FAfterSetRecord write FAfterSetRecord;
      // Called after the dataset is scrolled to the next record
    property OnFieldChecked : TFieldCheckEvent read FOnFieldChecked write FOnFieldChecked;
      // return if the field is true or false if the false string differs from '0','false','-'
  end;

  { THTMLDatasetFormEditProducer }

  THTMLDatasetFormEditProducer = class (THTMLDatasetFormProducer)
    procedure ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean); override;
  end;

  { THTMLDatasetFormShowProducer }

  THTMLDatasetFormShowProducer = class (THTMLDatasetFormProducer)
  protected
    procedure ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean); override;
  end;

  { THTMLDatasetFormGridProducer }

  THTMLDatasetFormGridProducer = class (THTMLDatasetFormProducer)
  protected
    procedure ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean); override;
    function SingleRecord : boolean; override;
  public
    constructor Create (aOwner : TComponent); override;
  published
    property RecordsPerPage;
    property Page;
    property IncludeHeader;
    property AfterSetRecord;
  end;
  
implementation

{ TTableDef }

function TTableDef.GetItem(index: integer): TTableCell;
begin
  result := TTableCell (inherited items[index]);
end;

function TTableDef.GetCell(x, y : integer): TTableCell;
var r : integer;
begin
  r := x + (y * fcols);
  result := getItem (r);
end;

constructor TTableDef.Create(acols, arows: integer);
var r, t : integer;
begin
  inherited create (TTableCell);
  fRows := aRows;
  fCols := aCols;
  for r := 1 to aRows do
    begin
    for t := 1 to aCols-1 do
      Add;
    TTableCell(Add).EndOfRow := True;
    end;
end;

function TTableDef.CopyTablePosition(position: TTablePosition): TTableCell;
begin
  result := Cells[position.left,position.top];
  with result do
    begin
    AlignHorizontal := position.AlignHorizontal;
    AlignVertical := position.FAlignVer;
    ColSpan := position.ColSpan;
    RowSpan := position.RowSpan;
    end;
end;

{ TTablePosition }

procedure TTablePosition.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if dest is TTablePosition then
    with TTablePosition(Dest) do
      begin
      FTop := self.FTop;
      FLeft := self.FLeft;
      FColSpan := self.FColSpan;
      FRowSpan := self.FRowSpan;
      FAlignVer := self.FAlignVer;
      FalignHor := self.FAlignHor;
      end;
end;

constructor TTablePosition.create;
begin
  inherited create;
  FColSpan := 1;
  FRowSpan := 1;
  FAlignVer := vaEmpty;
  FAlignHor := alEmpty;
end;

{ TFormFieldItem }

procedure TFormFieldItem.SetLabelPos(const AValue: TTablePosition);
begin
  FLabelPos.assign(AValue);
end;

procedure TFormFieldItem.SetValuePos(const AValue: TTablePosition);
begin
  FValuePos.assign(AValue);
end;

procedure TFormFieldItem.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if dest is TFormFieldItem then
    with TFormFIeldItem(Dest) do
      begin
      FAction := self.FAction;
      FFieldName := self.FFieldName;
      FInputType := self.FInputType;
      FLabelCaption := self.FLabelCaption;
      FLabelPos.assign (self.FLabelPos);
      FProducer := self.FProducer;
      FValuePos.assign(self.FValuePos);
      end;
end;

function TFormFieldItem.GetDisplayName: String;
begin
  If (FName='') then
    FName:=ClassName+IntToStr(self.Index);
  Result:=FName;
end;

procedure TFormFieldItem.SetDisplayName(const Value: String);
begin
  Inherited;
  FName:=Value;
end;

constructor TFormFieldItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLabelPos := TTablePosition.Create;
  FValuePos := TTablePosition.Create;
end;

destructor TFormFieldItem.Destroy;
begin
  FLabelPos.Free;
  FValuePos.Free;
  inherited Destroy;
end;

function TFormFieldItem.getValue: String;
begin
  if inputType in [fitcheckbox,fitradio] then
    Result := 'T'
  else
    Result := FField.DisplayText;
  if assigned (FOnGetValue) then
    onGetValue(self,Result);
end;

function TFormFieldItem.getLabel: String;
begin
  Result := LabelCaption;
  if assigned(FOnGetLabel) then
    onGetLabel(Self,Result);
end;

function TFormFieldItem.getAction: String;
begin
  Result := Format(Action,[FField.asstring]);
  if assigned(FOnGetAction) then
    onGetAction(Self,Result);
end;

{ TFormFieldCollection }

function TFormFieldCollection.GetItem(index : integer): TFormFieldItem;
begin
  result := TFormFieldItem(inherited items[index]);
end;

procedure TFormFieldCollection.SetItem(index : integer;
  const AValue: TFormFieldItem);
begin
  inherited items[index] := AValue;
end;

constructor TFormFieldCollection.create;
begin
  inherited create (TFormFieldItem);
end;

function TFormFieldCollection.AddField(afieldname, acaption: string): TFormFieldItem;
begin
  result := TFormFieldItem (Add);
  result.fieldname := afieldname;
  result.labelcaption := acaption;
end;

function TFormFieldCollection.FindItem(AName: String): TFormFieldItem;
Var
  I : Integer;

begin
  I:=IndexofItem(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=Items[I];
end;

function TFormFieldCollection.IndexofItem(AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(Items[Result].Name,AName)<>0) do
    Dec(Result);
end;

{ TFormButtonItem }

procedure TFormButtonItem.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if dest is TFormButtonItem then
    with TFormButtonItem(Dest) do
      begin
      FButtonType := self.FButtonType;
      FCaption := self.FCaption;
      FImage := self.FImage;
      FImagePlace := self.FImagePlace;
      FName := self.FName;
      FValue := self.FValue;
      end;
end;

constructor TFormButtonItem.create(ACollection: TCollection);
begin
  inherited create(ACollection);
  ButtonType := fbtPushButton;
end;

{ TFormButtonCollection }

function TFormButtonCollection.GetItem(index: integer): TFormButtonItem;
begin
  result := TFormButtonItem(inherited items[index]);
end;

procedure TFormButtonCollection.SetItem(index: integer;
  const AValue: TFormButtonItem);
begin
  inherited items[index] := AValue;
end;

constructor TFormButtonCollection.create;
begin
  inherited create (TFormButtonItem);
end;

function TFormButtonCollection.AddButton(aname, avalue, acaption: string): TFormButtonItem;
begin
  result := TFormButtonItem(Add);
  with result do
    begin
    name := aname;
    value := avalue;
    caption := acaption;
    end;
end;

function TFormButtonCollection.AddButton(aname, acaption: string): TFormButtonItem;
begin
  result := AddButton (aName, aCaption, acaption);
end;

function TFormButtonCollection.AddButton(acaption: string): TFormButtonItem;
begin
  result := AddButton (acaption, acaption, acaption);
end;

{ THTMLDatasetFormProducer }

procedure THTMLDatasetFormProducer.WriteButtons(aWriter: THTMLWriter);

  procedure WriteButton (aButton : TFormButtonItem);
  const ButtonTypes : array[TFormButtontype] of THTMLbuttontype = (btsubmit,btreset,btbutton,btreset,btreset,btreset);
  const InputTypes : array[TFormButtontype] of THTMLinputtype = (itreset,itreset,itreset,itsubmit,itreset,itbutton);
  var b : THTML_Button;
      ib: THTML_input;
  begin
    with aWriter do
     if aButton.ButtonType in [fbtInputPushButton,fbtInputReset,fbtInputSubmit] then
      begin
      ib := input;
      with ib do
        begin
        Name := aButton.name;
        Value := aButton.value;
        TheType := InputTypes[aButton.ButtonType];
        if assigned (FAfterButtonCreate) then
          FAfterButtonCreate (self, aButton, ib);
        end;
      end
     else
      begin
      b := Startbutton;
      with b do
        begin
        Name := aButton.name;
        Value := aButton.value;
        TheType := ButtonTypes[aButton.ButtonType];
        if aButton.Image = '' then
          Text (aButton.Caption)
        else
          begin
          if aButton.ImagePlace in [ipAfter, ipUnder] then
            begin
            Text (aButton.Caption);
            if aButton.ImagePlace = ipUnder then
              linebreak;
            end;
          with image do
            begin
            src := aButton.image;
            if aButton.ImagePlace = ipOnly then
              alt := aButton.Caption;
            end;
          if aButton.ImagePlace in [ipBefore, ipAbove] then
            begin
            if aButton.ImagePlace = ipAbove then
              linebreak;
            Text (aButton.Caption);
            end;
          end;
        if assigned (FAfterButtonCreate) then
          FAfterButtonCreate (self, aButton, b);
        Endbutton;
        end;
      end;
  end;

const ButHorAlign : array[TButtonHorPosition] of THTMLalign = (alleft,alcenter,aljustify,alright);
var r : integer;
begin
  with aWriter do
    begin
    StartTableRow;
    with StartTableCell do
      begin
      ColSpan := inttostr(FTableCols);
      align := ButHorAlign[ButtonsHorizontal];
      end;
    for r := 0 to buttonrow.count-1 do
      WriteButton (buttonrow.Items[r]);
    EndTableCell;
    EndTableRow;
    end;
end;

procedure THTMLDatasetFormProducer.SetSeparateLabel(const AValue: boolean);
begin
  if AValue <> FSeparateLabel then
    begin
    FSeparateLabel := AValue;
    if AValue then
      FIncludeHeader := false;
    end;
end;

procedure THTMLDatasetFormProducer.SetIncludeHeader(const AValue: boolean);
begin
  if FIncludeHeader <> AValue then
    begin
    FIncludeHeader := AValue;
    if AValue then
      SeparateLabel := false;
    end;
end;

procedure THTMLDatasetFormProducer.WriteTableDef(aWriter: THTMLWriter);
var r : integer;
    c : THTMLCustomelement;
begin
  c := aWriter.Starttablebody;
  if assigned (FAfterTBodyCreate) then
    FAfterTBodyCreate (self, c);
  aWriter.StartTableRow;
  with tabledef do
    begin
    for r := 0 to count-1 do
      with TTableCell (Items[r]) do
        begin
        if CellType <> ctSpanned then
          begin
          if (CellType = ctProducer) and assigned (FOnInitializeProducer) then
            FOnInitializeProducer (self, FFormField, Producer);
          c := WriteContent(aWriter);
          if assigned (FAfterCellCreate) then
            FAfterCellCreate(self, Items[r].FormField, IsLabel, c);
          end;
        if EndOfRow then
          begin
          aWriter.EndTableRow;
          aWriter.StartTableRow;
          end;
        end;
    end;
  aWriter.EndTableRow;
  aWriter.Endtablebody;
end;

procedure THTMLDatasetFormProducer.WriteHeaderTableDef(aWriter: THTMLWriter);
var r : integer;
    c : THTMLCustomelement;
begin
  aWriter.Starttablehead;
  aWriter.StartTableRow;
  with tabledef do
    begin
    for r := 0 to count-1 do
      with TTableCell (Items[r]) do
        begin
        c := WriteHeader(aWriter);
        if assigned (FAfterCellCreate) then
          FAfterCellCreate(self, Items[r].FormField, true, c);
        if EndOfRow then
          begin
          aWriter.EndTableRow;
          aWriter.StartTableRow;
          end;
        end;
    end;
  aWriter.EndTableRow;
  aWriter.Endtablehead;
end;

procedure THTMLDatasetFormProducer.CorrectCellSpans;
var r, s, t : integer;
    c : TTableCell;
begin
  for r := 0 to TableDef.count-1 do
    with TableDef.items[r] do
      if CellType <> ctSpanned then
        begin
        // CollSpan marking other cells as spanned
        s := 1;
        c := TableDef.Items[r];
        while (s < ColSpan) and not c.EndOfRow do
          begin
          c := TableDef.Items[r+s];
          c.celltype := ctSpanned;
          inc (s);
          end;
        // the same for rowsapn
        s := 1;
        t := r + (s*tablecols);
        while (s < rowspan) and (t < TableDef.count) do
          begin
          TableDef.items[t].CellType := ctSpanned;
          inc (s);
          inc (t, tablecols);
          end;
        end;
end;

procedure THTMLDatasetFormProducer.SearchControlFields;
var r : integer;
begin
  for r := 0 to FControls.count-1 do
    with FControls.items[r] do
      FField := datasource.dataset.FindField(FFieldname);
end;

function THTMLDatasetFormProducer.StartForm(aWriter: THTMLWriter) : THTMLCustomElement;
const MethodAttribute : array[TFormMethod] of string = ('','GET','POST');
var t : THTMLCustomElement;
begin
  if Self.FormMethod <> fmNone then
    begin
    result := aWriter.Startform;
    with THTML_Form(result) do
      begin
      method := MethodAttribute[self.FormMethod];
      action := FormAction;
      end;
    t := aWriter.Starttable;
    end
  else
    begin
    t := aWriter.Starttable;
    result := t;
    end;
  if assigned (FAfterTableCreate) then
    FAfterTableCreate (self, t);
end;

procedure THTMLDatasetFormProducer.EndForm(aWriter: THTMLWriter);
begin
  with aWriter do
    begin
    EndTable;
    if self.FormMethod <> fmNone then
      Endform;
    end;
end;

function THTMLDatasetFormProducer.WriteContent(aWriter: THTMLWriter): THTMLCustomElement;
var r : integer;
begin
  if assigned (datasource) and assigned(datasource.dataset) then
    begin
    Ftabledef := TTableDef.Create (TableCols, TableRows);
    try
      SearchControlFields;
      result := StartForm (aWriter);
      if bvpTop in ButtonsVertical then
        WriteButtons (aWriter);
      if SingleRecord then
        begin
        FillTableDef (false);
        CorrectCellSpans;
        WriteTableDef (aWriter);
        end
      else
        with datasource.dataset do
          begin
          if FIncludeHeader then
            begin
            FillTableDef (true);
            CorrectCellSpans;
            WriteHeaderTableDef (aWriter);
            end;
          if Page < 0 then
            first
          else
            begin
            try  // Catch exception if the record doesn't exist.
              RecNo := ((Page-1) * RecordsPerPage) + 1; // zero based? yes: + 1 has to be deleted
            except
              Last;
              end;
            end;
          r := 0;
          while not eof and (r < RecordsPerPage) do
            begin
            if assigned (FAfterSetRecord) then
              FAfterSetRecord (self);
            FillTableDef (false);
            CorrectCellSpans;
            WriteTableDef (aWriter);
            Next;
            inc (r);
            end;
          end;
      if bvpBottom in ButtonsVertical then
        WriteButtons (aWriter);
      EndForm (aWriter)
    finally
      tabledef.Free;
    end;
    end;
end;

procedure THTMLDatasetFormProducer.FillTableDef (IsHeader:boolean);
var r : integer;
begin
  for r := 0 to Controls.Count-1 do
    ControlToTableDef (Controls.items[r], IsHeader);
end;

procedure THTMLDatasetFormProducer.PlaceFieldValue(aControldef : TFormFieldItem);
var check : boolean;
begin
  with TableDef.CopyTablePosition(aControlDef.ValuePos) do
    begin
    FormField := aControldef;
    case aControlDef.inputtype of
      fitlabel,
      fittext,
      fitpassword,
      fitcheckbox,
      fitradio,
      fitfile,
      fithidden,
      fittextarea :
        begin
        CellType := ctInput;
        InputType := aControlDef.InputType;
        Name := aControlDef.Field.FieldName;
        Size := aControlDef.Field.DisplayWidth;
        MaxLength := aControldef.Field.Size;
        if aControlDef.inputType in [fitcheckbox,fitradio] then
          begin
          with aControlDef.Field do
            Check := asBoolean;
          if assigned (FOnFieldChecked) then
            FOnFieldChecked (aControlDef.Field, check);
          Checked := check;
          end;
        end;
      fitproducer :
        begin
        CellType := ctProducer;
        Producer := aControlDef.Producer;
//        if Producer is THTMLSelectProducer then THTMLSelectProducer(Producer).PreSelected:=aControldef.getValue;
        end;
      fitrecordselection : ;
    end;
    IsLabel := false;
    Value := aControlDef.getValue;
    Link := aControldef.getAction;
    if not FSeparateLabel and not FIncludeHeader then
      begin
      Caption := aControldef.getLabel;
      IncludeBreak := aControldef.LabelAbove;
      end;
    end;
end;

function THTMLDatasetFormProducer.SingleRecord: boolean;
begin
  result := true;
end;

constructor THTMLDatasetFormProducer.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FTableCols := 2;
  FButtonsHor := bhpLeft;
  FButtonsVer := [bvpTop, bvpBottom];
  Fbuttonrow := TFormButtonCollection.create;
  FControls := TFormFieldCollection.Create;
end;

destructor THTMLDatasetFormProducer.destroy;
begin
  Fbuttonrow.Free;
  FControls.Free;
  inherited destroy;
end;

{ THTMLDatasetFormEditProducer }

procedure THTMLDatasetFormEditProducer.ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean);

  procedure PlaceLabel;
  begin
    with TableDef.CopyTablePosition(aControlDef.LabelPos) do
      begin
      FormField := aControldef;
      CellType := ctLabel;
      IsLabel := true;
      Value := aControldef.getLabel;
      end;
  end;

begin
  if assigned (aControlDef.FField) then
    PlaceFieldValue(aControldef);
  if FSeparateLabel and (aControlDef.getLabel <> '') then
    PlaceLabel;
end;

{ THTMLDatasetFormShowProducer }

(**** TTableCell *****
    property IsLabel : boolean read FIsLabel write FIsLabel;
      // Label or Value ?
    property CellType : TCellType read FCellType write FCellType;
      ctEmpty, ctInput, ctLabel, ctProducer, ctSpanned
    { Cell properties: }
    property ColSpan : integer read FColSpan write FColSpan;
    property RowSpan : integer read FRowSpan write FRowSpan;
    property AlignVertical : THTMLvalign read FAlignVer write FAlignVer default vaEmpty;
    property AlignHorizontal : THTMLalign read FAlignHor write FAlignHor default alEmpty;
    property Value : string read FValue write FValue;
          // Contains the text for labels, or the value for input, or unused for producer and empty
    { only for input: }
    property Name : string read FName write FName;
          // name of the control
    property InputType : TFormInputType read FInputType write FInputType;
          // type of the input element
    property Size : integer read FSize write FSize;
          // size of text input element
    property MaxLength : integer read FMaxLength write FMaxLength;
          // MaxLength of text input element
    property Checked : boolean read FChecked write FChecked;
          // checked or not for radio,checkbox
    { only for labels: }
    property Link : string read FLink write FLink;
          // link to place around the text
    { only for producers: }
    property Producer : THTMLContentProducer read FProducer write FProducer;
          // producer to include

***** TFormFieldItem *****
    property Fieldname : string read FFieldName write FFieldname;
    property Field : TField
      // the field to show/edit
    property LabelCaption : string read FLabelCaption write FLabelCaption;
      // the text to show for the control
    property InputType : TFormInputType read FInputType write FInputType default fittext;
      // the type of form control to use
      (fittext,fitpassword,fitcheckbox,fitradio,fitfile,fithidden,fitproducer,fittextarea,fitrecordselection)
    property Producer : THTMLContentProducer read FProducer write FProducer;
      // the producer to include when generating the value
    property Action : string read FAction write FAction;
      // when showing the link to include in the value
    property LabelPos : TTablePosition read FLabelPos write SetLabelPos;
      // place of the label in the table-grid
    property ValuePos : TTablePosition read FValuePos write SetValuePos;
      // place of the value in the table-grid   *)

procedure THTMLDatasetFormShowProducer.ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean);

  procedure PlaceFieldValue;
  begin
    with TableDef.CopyTablePosition(aControlDef.ValuePos) do
      begin
      CellType := ctLabel;
      IsLabel := false;
      FormField := aControldef;
      Value := aControlDef.getValue;
      if not FSeparateLabel and not FIncludeHeader then
        begin
        Caption := aControldef.getLabel;
        IncludeBreak := aControldef.LabelAbove;
        end;
      end;
  end;
  
  procedure PlaceLabel;
  begin
    with TableDef.CopyTablePosition(aControlDef.LabelPos) do
      begin
      CellType := ctLabel;
      FormField := aControldef;
      IsLabel := true;
      Value := aControldef.getLabel;
      end;
  end;

begin
  if assigned (aControlDef.FField) then
    PlaceFieldValue;
  if FSeparateLabel and (aControlDef.getLabel <> '') then
    PlaceLabel;
end;

{ THTMLDatasetFormGridProducer }

procedure THTMLDatasetFormGridProducer.ControlToTableDef (aControldef : TFormFieldItem; IsHeader:boolean);

  procedure PlaceLabel;
  begin
    with TableDef.CopyTablePosition(aControlDef.LabelPos) do
      begin
      CellType := ctLabel;
      IsLabel := true;
      Value := aControldef.getLabel;
      end;
  end;

begin
  if assigned (aControlDef.FField) and not IsHeader then
    PlaceFieldValue(aControldef);
  if (IsHeader or FSeparateLabel) and (aControlDef.getLabel <> '') then
    PlaceLabel;
end;

function THTMLDatasetFormGridProducer.SingleRecord: boolean;
begin
  Result := false;
end;

constructor THTMLDatasetFormGridProducer.Create(aOwner: TComponent);
begin
  inherited create(aOwner);
  RecordsPerPage := 20;
  Page := -1;
end;

{ TTableCell }

procedure TTableCell.WriteLabel(aWriter: THTMLWriter);
var HasLink : boolean;
begin
  HasLink := (Link <> '');
  if HasLink then
    aWriter.Anchor(Value).href := Link
  else
    aWriter.Text (Value);
end;


function TTableCell.WriteContent(aWriter: THTMLWriter) : THTMLCustomElement;

  procedure WriteTextArea;
  begin
    aWriter.textarea(value).name := Name;
  end;

  procedure WriteInput;
  var s, m : string;
  begin
    if size > 0 then
      s := inttostr(size)
    else
      s := '';
    if MaxLength > 0 then
      m := inttostr(MaxLength)
    else
      m := '';
    case InputType of
      fittext :
        with aWriter.FormText (Name, Value) do
          begin
          Size := s;
          MaxLength := m;
          end;
      fitpassword :
          with aWriter.FormPasswd (Name) do
          begin
          if self.Value <> '' then
            Value := self.value;
          Size := s;
          MaxLength := m;
          end;
      fitcheckbox, fitrecordselection :
        aWriter.FormCheckbox (Name, Value, checked).disabled := Disabled;
      fitradio :
        aWriter.FormRadio(Name, Value, checked);
      fitfile :
        aWriter.FormFile(Name, Value);
      fithidden :
        aWriter.FormHidden (Name, Value);
      fitlabel :
        if link <> '' then
          aWriter.Anchor(Value).href := Link
        else
          aWriter.Text (Value);
    end;
  end;

  procedure WriteProducer;
  begin
    if assigned(Producer) then with Producer do
      begin
      ParentElement := aWriter.CurrentElement;
      HTMLDocument := aWriter.Document;
      WriteContent (aWriter);
      end;
  end;

var c : THTML_td;

begin
  if CellType <> ctSpanned then
    with aWriter do
      begin
      c := Starttablecell;
      with c do
        begin
        if self.ColSpan > 1 then
          colspan := IntToStr(self.Colspan);
        if self.RowSpan > 1 then
          Rowspan := IntToStr(self.Rowspan);
        align := AlignHorizontal;
        valign := AlignVertical;
        end;
      if Self.Caption <> '' then
        begin
        span(self.caption);
        if IncludeBreak then
          linebreak;
        end;
      case CellType of
        ctEmpty : ;
        ctInput :
          if InputType = fittextarea then
            WriteTextArea
          else
            WriteInput;
        ctLabel : WriteLabel(aWriter);
        ctProducer : WriteProducer;
      end;
      Endtablecell;
      result := c;
      end
  else
    result := nil;
end;

function TTableCell.WriteHeader(aWriter: THTMLWriter) : THTMLCustomElement;
var c : THTML_th;
begin
    with aWriter do
      begin
      c := Starttableheadcell;
      with c do
        begin
        if self.ColSpan > 1 then
          ColSpan := IntToStr(self.Colspan);
        if self.RowSpan > 1 then
          RowSpan := IntToStr(self.Rowspan);
        align := AlignHorizontal;
        valign := AlignVertical;
        end;
      case CellType of
        ctEmpty : ;
        ctLabel : WriteLabel(aWriter);
//        ctProducer : WriteProducer;
      end;
      Endtableheadcell;
      result := c;
      end;
end;

end.

