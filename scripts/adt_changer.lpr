program adt_changer;

{$mode objfpc}{$H+}

uses
    cthreads,
    Classes,
    SysUtils,
    CustApp,
    unt_changer;

type

    { TAdtChanger }

    TAdtChanger = class(TCustomApplication)
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
    end;

    { TAdtChanger }

    procedure TAdtChanger.DoRun;
    var
        AFileName: string;
    begin
        if HasOption('n') then
        begin
            AFileName := GetOptionValue('n');
        end;
        if AFileName <> '' then
        begin
            ChangeFile(AFileName);
            WriteLn('Done Patching File.');
        end;
        Terminate;
    end;

    constructor TAdtChanger.Create(TheOwner: TComponent);
    begin
        inherited Create(TheOwner);
        StopOnException := True;
    end;

    destructor TAdtChanger.Destroy;
    begin
        inherited Destroy;
    end;

var
    Application: TAdtChanger;
begin
    Application := TAdtChanger.Create(nil);
    Application.Run;
    Application.Free;
end.
