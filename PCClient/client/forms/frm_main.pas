unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, baseform,
  vg_controls, vg_objects;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    procedure FormCreate(Sender: TObject);
  private
    FButton: TvgHudButton;
  protected
    function SetWindowTitle: string; override;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FButton:= TvgHudButton.Create(Panel);
  FButton.Parent := Panel;
end;

function TFormMain.SetWindowTitle: string;
begin
  Result := Config.GetString('title');
end;

end.

