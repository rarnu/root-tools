unit stdlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  clib = 'c';

{$I glue.inc}
{$I typesh.inc}
{$I xlocaleh.inc}
{$I stdlibh.inc}

implementation

{$I types.inc}

end.

