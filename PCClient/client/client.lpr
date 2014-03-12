program client;

{$mode objfpc}{$H+}

uses
  cthreads, Interfaces, Forms, vgscene, baseform, baseconfig, frm_main,
  res_mapping, platform_mapping, page_main, item_main, page_blank, unt_command,
  unt_env, th_device_id, basethread, th_usb, frm_skin, frm_blank;

{$R *.res}

begin
  Application.Title:='RootTools';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
