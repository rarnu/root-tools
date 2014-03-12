{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit vgscene;

interface

uses
  vg_ani, vg_colors, vg_controls, vg_dsgn, vg_dsgn_bmp, vg_dsgn_path, 
  vg_effects, vg_extctrls, vg_inspector, vg_layouts, vg_listbox, vg_objects, 
  vg_reg, vg_scene, vg_tabcontrol, vg_textbox, vg_treeview, vg_version, 
  vg_dsgn_styles, vg_canvas_macos, vg_canvas_gdip, vg_memo, vg_canvas_cairo, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('vg_reg', @vg_reg.Register);
end;

initialization
  RegisterPackage('vgscene', @Register);
end.
