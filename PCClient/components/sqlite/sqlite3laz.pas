{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit sqlite3laz; 

interface

uses
  registersqlite3, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registersqlite3', @registersqlite3.Register); 
end; 

initialization
  RegisterPackage('sqlite3laz', @Register); 
end.
