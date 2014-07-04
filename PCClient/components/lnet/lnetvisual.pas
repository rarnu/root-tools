{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit lnetvisual; 

interface

uses
  LCLNet, lNetComponents, LNetVisualReg, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('LNetVisualReg', @LNetVisualReg.Register); 
end; 

initialization
  RegisterPackage('lnetvisual', @Register); 
end.
