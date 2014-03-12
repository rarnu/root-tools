{==============================================================================

  VGScene Version
  Copyright (C) by Eugene Kryukov
  All rights reserved

  All conTeThements of this file and all other files included in this archive
  are Copyright (C) Eugene Kryukov. Use and/or distribution of
  them requires acceptance of the License Agreement.

  See License.txt for licence information

  $Id$

===============================================================================}

unit vg_version;

interface

{$I vg_define.inc}

const
  SVGSceneVersionPropText = 'VGScene';

type

  TfeFlashVersion = string;

const
  Sig: PChar = '- ' + SVGSceneVersionPropText +
    {$IFDEF vg_CBUILDER4} ' - CB4 - ' + {$ENDIF}
    {$IFDEF vg_DELPHI4} ' - D4 - '+ {$ENDIF}
    {$IFDEF vg_CBUILDER5} ' - CB5 - '+ {$ENDIF}
    {$IFDEF vg_DELPHI5} ' - D5 - '+ {$ENDIF}
    {$IFDEF vg_CBUILDER6} ' - CB6 - '+ {$ENDIF}
    {$IFDEF vg_DELPHI6} ' - D6 - '+ {$ENDIF}
    {$IFDEF vg_CBUILDER7} ' - CB7 - '+ {$ENDIF}
    {$IFDEF vg_DELPHI7} ' - D7 - '+ {$ENDIF}
    'Copyright (C) by Eugene Kryukov -';

procedure ShowVersion;
procedure ShowVersion2;

implementation {===============================================================}

{$IFNDEF NOVCL}
uses Forms, Dialogs, SysUtils;
{$ENDIF}

procedure ShowVersion;
const
  AboutText =
    '%s'#13#10 +
    'Copyright (C) by Eugene Kryukov'#13#10 +
    'For conditions of distribution and use, see LICENSE.TXT.'#13#10 +
    #13#10 +
    'Visit our web site for the latest versions of VGScene:'#13#10 +
    'http://www.ksdev.com/';
begin
  {$IFNDEF NOVCL}
  MessageDlg(Format(AboutText, [SVGSceneVersionPropText]), mtInformation, [mbOK], 0);
  {$ENDIF}
end;

procedure ShowVersion2;
const
  AboutText =
    'This application uses a trial version of VGScene.'#13#10#13#10 +
    'Please contact the provider of the application for a registered version.'#13#10 +
    #13#10+
    '%s'#13#10 +
    'Copyright (C) by Eugene Kryukov'#13#10 +
    #13#10 +
    'For conditions of distribution and use, see LICENSE.TXT.'#13#10 +
    #13#10 +
    'Visit our web site for the latest versions of VGScene:'#13#10 +
    #13#10 +
    'http://www.ksdev.com/' +
    #13#10 +
    'support@ksdev.com';
var
  X, Y: integer;
begin
  {$IFNDEF NOVCL}
  X := Random(Screen.Width - 400);
  Y := Random(Screen.Height - 200);
  MessageDlgPos(Format(AboutText, [SVGSceneVersionPropText]), mtInformation, [mbOk], 0, X, Y);
  {$ENDIF}
end;

initialization
  Sig := Sig;
end.
