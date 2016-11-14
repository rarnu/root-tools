{
 /***************************************************************************
                                  forms.pp
                                  --------
                             Component Library Code


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999
                   Revised : Sat Jul 15 1999

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Forms;

{$mode objfpc}{$H+}{$macro on}
{$I lcl_defines.inc}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

{$DEFINE HasDefaultValues}

uses
  Classes, SysUtils, TypInfo, InterfaceBase,
  LCLType, LCLProc, LCLIntf, LazFileUtils, LazUTF8,
  LResources, GraphType, Graphics,
   (*Controls,*) Themes
  {$ifndef wince},gettext{$endif}
  ;

type
  TProcedure = procedure;
  TProcedureOfObject = procedure of object;

procedure CreateWidgetset(AWidgetsetClass: TWidgetsetClass);
procedure FreeWidgetSet;

implementation

// {$R cursors.res}

{$ifdef WinCE}
  {$define extdecl := cdecl}
{$else}
  {$define extdecl := stdcall}
{$endif}

procedure CreateWidgetset(AWidgetsetClass: TWidgetsetClass);
begin
  CallInterfaceInitializationHandlers;
  WidgetSet := AWidgetsetClass.Create;
end;

procedure FreeWidgetSet;
begin
  CallInterfaceFinalizationHandlers;
  WidgetSet.Free;
  WidgetSet:=nil;
end;


//==============================================================================


finalization
  LCLProc.OwnerFormDesignerModifiedProc:=nil;

end.
