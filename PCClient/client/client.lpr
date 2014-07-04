program client;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms,
  baseform,
  baseconfig,
  frm_main,
  res_mapping,
  platform_mapping,
  page_device,
  item_main,
  page_about,
  unt_command,
  unt_env,
  th_device_id,
  basethread,
  th_usb,
  frm_skin,
  frm_blank,
  basepage,
  unt_android,
  unt_json,
  th_android,
  intf_notify,
  intf_paint,
  item_device_build_prop,
  page_root_tools,
  th_network,
  basenetwork,
  item_update_log,
  unt_download,
  pop_env;

{$R *.res}

begin
  Application.Title := 'RootTools';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
