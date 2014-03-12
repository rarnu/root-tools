unit page_blank;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, vg_scene, vg_controls, vg_objects, vg_ani, baseform;

type

  { TPageBlank }

  TPageBlank = class(TvgHudPanel)
  private

    FBase: TFormBase;
  public
    constructor Create(AOwner: TComponent; ABase: TFormBase); reintroduce;
    destructor Destroy; override;

  end;

implementation

{ TPageBlank }

constructor TPageBlank.Create(AOwner: TComponent; ABase: TFormBase);
begin
  Inherited Create(AOwner);
  FBase := ABase;
end;

destructor TPageBlank.Destroy;
begin
  inherited Destroy;
end;

end.

