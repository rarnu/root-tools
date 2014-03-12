unit res_mapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  RES_TITLE = 'title';
  RES_INSTALL_ROOTTOOLS = 'install_roottools';

  RES_MAIN_ITEM: array[0..9] of string = ('item_system_app','item_backup_mgr','item_restore_mgr','item_batch_install','item_batch_uninstall','item_disk','item_files','item_hosts','item_system_components','item_google');

  RES_NO_DEVICE = 'no_device';
  RES_MY_DEVICE = 'my_device';
  RES_SKIN = 'skin';
  RES_OK = 'ok';
  RES_RESET = 'reset';

implementation

end.

