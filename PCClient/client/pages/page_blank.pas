unit page_blank;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,basepage, vg_scene, vg_controls, vg_objects, vg_ani, baseform;

type

  { TPageBlank }

  TPageBlank = class(TPageBase)
  protected
    procedure InitPage; override;
  public
    procedure ThreadNotify(NotifyId: integer; AMap: TStringList); override;

  end;

implementation

{ TPageBlank }

procedure TPageBlank.InitPage;
begin

end;

procedure TPageBlank.ThreadNotify(NotifyId: integer; AMap: TStringList);
begin

end;

end.

