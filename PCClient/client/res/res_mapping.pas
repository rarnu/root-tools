unit res_mapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  RES_TITLE = 'title';
  RES_INSTALL_ROOTTOOLS = 'install_roottools';

  // RES_MAIN_ITEM: array[0..9] of string = ('item_system_app','item_backup_mgr','item_restore_mgr','item_batch_install','item_batch_uninstall','item_disk','item_files','item_hosts','item_system_components','item_google');
  RES_MAIN_ITEM: array[0..2] of string = ('item_device', 'item_roottools', 'item_about');
  RES_NO_DEVICE = 'no_device';
  RES_MY_DEVICE = 'my_device';
  RES_SKIN = 'skin';
  RES_OK = 'ok';
  RES_RESET = 'reset';
  RES_REFRESH_DEVICE = 'btn_refresh_device';
  RES_CURRENT_VERSION = 'current_version';
  RES_LAST_VERSION = 'last_version';
  RES_UPDATE = 'btn_update';
  RES_SYNC_FILE = 'sync_file';
  RES_NOT_INSTALLED = 'app_not_installed';
  RES_CONNECTING = 'connecting';

implementation

end.

