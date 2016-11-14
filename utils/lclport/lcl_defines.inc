// Add defines here. This file should be included in all LCL units headers

{$DEFINE UseCLDefault}
{$WARN SYMBOL_PLATFORM OFF}

{$IF FPC_FULLVERSION>=20701}
  // NOREMOVEEMPTYPROCS bites double inheritance in wslclclasses.pp
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
{$ENDIF}

