{
 *****************************************************************************
 *                                                                           *
 *  See the file LICENSE and LICENSE.ADDON, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Ales Katona
  
  This unit registers the lnet components.
}
unit LNetVisualReg;

interface

procedure Register;

implementation

uses
  Classes, LResources, LazarusPackageIntf, PropEdits,
  lNetComponents;

procedure Register;
begin
  RegisterComponents('lNet' , [TLTcpComponent, TLUdpComponent, TLTelnetClientComponent,
                               TLFTPClientComponent, TLSMTPClientComponent,
                               TLHTTPClientComponent, TLHTTPServerComponent,
                               TLSSLSessionComponent]);
                               
  RegisterPropertyEditor(TypeInfo(ansistring), TLSSLSessionComponent, 'CAFile',
                                                     TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(ansistring), TLSSLSessionComponent, 'KeyFile',
                                                     TFileNamePropertyEditor);
end;

initialization
 {$i lnetvisualreg.lrs}

end.
 
