program sm;

{$mode objfpc}{$H+}

uses
    cthreads,
    Interfaces,
    Forms,
    frm_main,
    unt_server, unt_expect, unt_config, frm_config;

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TFormMain, FormMain);
    Application.CreateForm(TFormConfig, FormConfig);
    Application.Run;
end.
