program client;

{$mode objfpc}{$H+}

uses
  cthreads, Interfaces, Forms, vgscene, baseform, baseconfig, frm_main,
  res_mapping, platform_mapping, page_device, item_main, page_blank,
  unt_command, unt_env, th_device_id, basethread, th_usb, frm_skin, frm_blank,
  basepage, unt_android, th_android, intf_notify, intf_paint, 
item_device_build_prop, page_root_tools;

{$R *.res}

begin
  Application.Title:='RootTools';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
