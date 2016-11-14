// Add defines here. This file should be included in all LazUtils units headers


{$undef UTF8_RTL}
{$undef ACP_RTL}
{$undef NO_CP_RTL}


{$ifdef FPC_HAS_CPSTRING}
  {$ifndef DisableUTF8RTL}
    {$define UTF8_RTL}
  {$else DisableUTF8RTL}
    {$define ACP_RTL}
  {$endif DisableUTF8RTL}
{$else FPC_HAS_CPSTRING}
  {$define NO_CP_RTL}
  {$undef DisableUTF8RTL}
{$endif FPC_HAS_CPSTRING}
