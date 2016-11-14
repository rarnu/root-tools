{
 /***************************************************************************
                                   wsproc.pp
                                   ---------
                             Widgetset Utility Code


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Useful lower level helper functions and classes for implementing widgetsets.
}
unit WSProc;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface

uses
  LCLClasses, LCLProc;


function WSCheckReferenceAllocated(const AComponent: TLCLReferenceComponent;
                                   const AProcName: String): Boolean;

implementation

function WSCheckReferenceAllocated(const AComponent: TLCLReferenceComponent;
  const AProcName: String): Boolean;

  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without reference for %s(%s)', [AProcName, AComponent.Name, AComponent.ClassName]);
  end;
begin
  Result := AComponent.ReferenceAllocated;
  if Result then Exit;
  Warn;
end;


end.
