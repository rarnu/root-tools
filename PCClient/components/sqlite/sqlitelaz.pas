{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit sqlitelaz; 

interface

uses
  registersqlite, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registersqlite', @registersqlite.Register); 
end; 

initialization
  RegisterPackage('sqlitelaz', @Register); 
end.
