unit basethread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseform;

type

  { TThreadBase }

  TThreadBase = class(TThread)
  private

  protected
    FNotifyId: Integer;
    FBase: TFormBase;
    function MakeNotifyMap: TStringList; virtual; abstract;
    procedure ThreadTerminated(Sender: TObject);
  public
    constructor Create(ANotifyId: Integer; ABase: TFormBase);

  end;

implementation

{ TThreadBase }

procedure TThreadBase.ThreadTerminated(Sender: TObject);
begin
  FBase.ThreadNotify(FNotifyId, MakeNotifyMap);
end;

constructor TThreadBase.Create(ANotifyId: Integer; ABase: TFormBase);
begin
  Inherited Create(true);
  FBase := ABase;
  FNotifyId := ANotifyId;
  FreeOnTerminate:=True;
  OnTerminate:= @ThreadTerminated;
end;

end.

