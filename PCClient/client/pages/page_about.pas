unit page_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,basepage, vg_scene, vg_controls, vg_objects, vg_ani, baseform;

type

  { TPageBlank }

  TPageAbout = class(TPageBase)
  protected
    procedure InitPage; override;
  public
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;

  end;

implementation

{ TPageBlank }

procedure TPageAbout.InitPage;
begin
  HideNoDevice;
end;

procedure TPageAbout.ThreadNotify(NotifyId: integer; AMap: TStringList);
begin

end;

end.

