unit stdio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  clib = 'c';

{$I glue.inc}
{$I typesh.inc}
{$I ctypeh.inc}
{$I bstath.inc}
{$I gconvh.inc}
{$I gconfigh.inc}
{$I libioh.inc}
{$I bfcntlh.inc}
{$I fileh.inc}
{$I stdioh.inc}

implementation

{$I types.inc}
{$I libio.inc}

end.

