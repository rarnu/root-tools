unit basethread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intf_notify;

type

  { TThreadBase }

  TThreadBase = class(TThread)
  private

  protected
    FNotifyId: Integer;
    FNotify: INotifyable;
    function MakeNotifyMap: TStringList; virtual; abstract;
    procedure ThreadTerminated(Sender: TObject);
  public
    constructor Create(ANotifyId: Integer; ANotify: INotifyable);

  end;

implementation

{ TThreadBase }

procedure TThreadBase.ThreadTerminated(Sender: TObject);
begin
  FNotify.ThreadNotify(FNotifyId, MakeNotifyMap);
end;

constructor TThreadBase.Create(ANotifyId: Integer; ANotify: INotifyable);
begin
  Inherited Create(true);
  FNotify := ANotify;
  FNotifyId := ANotifyId;
  FreeOnTerminate:=True;
  OnTerminate:= @ThreadTerminated;
end;

end.

