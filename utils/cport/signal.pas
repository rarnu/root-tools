unit signal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  clib = 'c';

{$I glue.inc}
{$I typesh.inc}
{$I signumh.inc}
{$I sigcontexth.inc}
{$I sigstackh.inc}
{$I sigseth.inc}
{$I sigactionh.inc}
{$I siginfoh.inc}
{$I signalh.inc}

implementation

{$I types.inc}

end.

