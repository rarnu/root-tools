{ 
 /*************************************************************************** 
                         Interfaces.pp  -  determines what interface to use
                             ------------------- 

                   Initial Revision  : Thu July 1st CST 1999 


 ******************** *******************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Interfaces;

{$mode objfpc}{$H+}

interface 

uses
  InterfaceBase;

implementation

uses
  customdrawnint, Forms;

initialization
  CreateWidgetset(TCDWidgetSet);

finalization
  FreeWidgetSet;

end.
