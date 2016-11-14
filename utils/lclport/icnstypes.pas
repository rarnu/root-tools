{  $Id: icnstypes.pas 41387 2013-05-24 18:30:06Z juha $  }
{
 /***************************************************************************
                              IcnsTypes.pas
                              ---------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Paul Ishenin

  Abstract:
    Types, consts and functions we need to read Mac OS X icon resource files - icns
    Most defines present here were taken from univint package of fpc
}
unit IcnsTypes;

{$mode objfpc}{$H+}

interface

type
  FourCharCode = array[0..3] of char;

type
  TIconFamilyElement = record
    elementType: FourCharCode; {  'ICN#', 'icl8', etc... }
    elementSize: LongInt;      { Size of this element }
  end;

  TIconFamilyResource = record
    resourceType: FourCharCode; {  Always 'icns' }
    resourceSize: LongInt;      {  Total size of this resource }
  end;

const
  kIconFamilyType = 'icns';

  kIconServices512PixelDataARGB: FourCharCode = 'ic09';
  kIconServices256PixelDataARGB: FourCharCode = 'ic08';
  kThumbnail32BitData          : FourCharCode = 'it32';
  kThumbnail8BitMask           : FourCharCode = 't8mk';
  kHuge1BitMask                : FourCharCode = 'ich#';
  kHuge4BitData                : FourCharCode = 'ich4';
  kHuge8BitData                : FourCharCode= 'ich8';
  kHuge32BitData               : FourCharCode= 'ih32';
  kHuge8BitMask                : FourCharCode= 'h8mk';
  {	 The following icon types can be used as a resource type 	}
  {	 or as an icon element type inside a 'icns' icon family 	}
  kLarge1BitMask               : FourCharCode= 'ICN#';
  kLarge4BitData               : FourCharCode= 'icl4';
  kLarge8BitData               : FourCharCode= 'icl8';
  kLarge32BitData              : FourCharCode= 'il32';
  kLarge8BitMask               : FourCharCode= 'l8mk';
  kSmall1BitMask               : FourCharCode= 'ics#';
  kSmall4BitData               : FourCharCode= 'ics4';
  kSmall8BitData               : FourCharCode= 'ics8';
  kSmall32BitData              : FourCharCode= 'is32';
  kSmall8BitMask               : FourCharCode= 's8mk';
  kMini1BitMask                : FourCharCode= 'icm#';
  kMini4BitData                : FourCharCode= 'icm4';
  kMini8BitData                : FourCharCode= 'icm8';

type
  // from lower to higher
  TicnsIconType =
  (
    iitNone,
    // data icons
    iitMini4BitData,
    iitMini8BitData,
    iitSmall4BitData,
    iitSmall8BitData,
    iitSmall32BitData,
    iitLarge4BitData,
    iitLarge8BitData,
    iitLarge32BitData,
    iitHuge4BitData,
    iitHuge8BitData,
    iitHuge32BitData,
    iitThumbnail32BitData,
    // mask icons
    iitMini1BitMask,
    iitSmall1BitMask,
    iitSmall8BitMask,
    iitLarge1BitMask,
    iitLarge8BitMask,
    iitHuge1BitMask,
    iitHuge8BitMask,
    iitThumbnail8BitMask,
    // alpha icons
    iit256PixelDataARGB,
    iit512PixelDataARGB
  );
  
  TicnsIconTypes = set of TicnsIconType;
  
  TicnsIconInfo = record
    Width: Integer;
    Height: Integer;
    Depth: Integer;
  end;

const
  icnsDataTypes =
    [
      iitMini4BitData, iitMini8BitData, iitSmall4BitData, iitSmall8BitData,
      iitSmall32BitData, iitLarge4BitData, iitLarge8BitData, iitLarge32BitData,
      iitHuge4BitData, iitHuge8BitData, iitHuge32BitData, iitThumbnail32BitData
    ];
  icnsMaskTypes =
    [
      iitMini1BitMask, iitSmall1BitMask, iitSmall8BitMask, iitLarge1BitMask,
      iitLarge8BitMask, iitHuge1BitMask, iitHuge8BitMask, iitThumbnail8BitMask
    ];
  icnsRGB =
    [
      iitSmall32BitData, iitLarge32BitData, iitHuge32BitData, iitThumbnail32BitData
    ];
  icnsWithAlpha =
    [
      iit256PixelDataARGB, iit512PixelDataARGB
    ];

  icnsIconTypeInfo: array[TicnsIconType] of TicnsIconInfo =
  (
 { iitNone               } (Width: 000; Height: 000; Depth: 00),
 { iitMini4BitData       } (Width: 016; Height: 012; Depth: 04),
 { iitMini8BitData       } (Width: 016; Height: 012; Depth: 08),
 { iitSmall4BitData      } (Width: 016; Height: 016; Depth: 04),
 { iitSmall8BitData      } (Width: 016; Height: 016; Depth: 08),
 { iitSmall32BitData     } (Width: 016; Height: 016; Depth: 32),
 { iitLarge4BitData      } (Width: 032; Height: 032; Depth: 04),
 { iitLarge8BitData      } (Width: 032; Height: 032; Depth: 08),
 { iitLarge32BitData     } (Width: 032; Height: 032; Depth: 32),
 { iitHuge4BitData       } (Width: 048; Height: 048; Depth: 04),
 { iitHuge8BitData       } (Width: 048; Height: 048; Depth: 08),
 { iitHuge32BitData      } (Width: 048; Height: 048; Depth: 32),
 { iitThumbnail32BitData } (Width: 128; Height: 128; Depth: 32),
 { iitMini1BitMask       } (Width: 016; Height: 012; Depth: 01),
 { iitSmall1BitMask      } (Width: 016; Height: 016; Depth: 01),
 { iitSmall8BitMask      } (Width: 016; Height: 016; Depth: 08),
 { iitLarge1BitMask      } (Width: 032; Height: 032; Depth: 01),
 { iitLarge8BitMask      } (Width: 032; Height: 032; Depth: 08),
 { iitHuge1BitMask       } (Width: 048; Height: 048; Depth: 01),
 { iitHuge8BitMask       } (Width: 048; Height: 048; Depth: 08),
 { iitThumbnail8BitMask  } (Width: 128; Height: 128; Depth: 08),
 { iit256PixelDataARGB   } (Width: 256; Height: 256; Depth: 32),
 { iit512PixelDataARGB   } (Width: 512; Height: 512; Depth: 32)
  );
  
  icnsMaskToImageMap: array[iitMini1BitMask..iitThumbnail8BitMask] of TicnsIconTypes =
  (
 { iitMini1BitMask       } [iitMini4BitData, iitMini8BitData],
 { iitSmall1BitMask      } [iitSmall4BitData, iitSmall8BitData, iitSmall32BitData],
 { iitSmall8BitMask      } [iitSmall4BitData, iitSmall8BitData, iitSmall32BitData],
 { iitLarge1BitMask      } [iitLarge4BitData, iitLarge8BitData, iitLarge32BitData],
 { iitLarge8BitMask      } [iitLarge4BitData, iitLarge8BitData, iitLarge32BitData],
 { iitHuge1BitMask       } [iitHuge4BitData, iitHuge8BitData, iitHuge32BitData],
 { iitHuge8BitMask       } [iitHuge4BitData, iitHuge8BitData, iitHuge32BitData],
 { iitThumbnail8BitMask  } [iitThumbnail32BitData]
  );

function GetIcnsIconType(const StrIconType: FourCharCode): TicnsIconType;

// Returns proper TicnsIconType, or iitNone, if Width/Height is incorrect
function GetDataTypeRGB(Width, Height: Integer; var FourChar: FourCharCode): TicnsIconType;
function GetMaskType8bit(Width, Height: Integer; var FourChar: FourCharCode): TicnsIconType;

implementation

function GetDataTypeRGB(Width, Height: Integer; var FourChar: FourCharCode): TicnsIconType;
begin
  Result := iitNone;
  if Width <> Height then Exit;
  
  case Width of
    16: begin
      Result := iitSmall32BitData;
      FourChar := kSmall32BitData;
    end;
    32: begin
      Result := iitLarge32BitData;
      FourChar := kLarge32BitData;
    end;
    48: begin
      Result := iitHuge32BitData;
      FourChar := kHuge32BitData;
    end;      
    128: begin
      Result := iitThumbnail32BitData;
      FourChar := kThumbnail32BitData;
    end;
    256: begin
      Result := iit256PixelDataARGB;
      FourChar :=  kIconServices256PixelDataARGB;
    end;
    512: begin
      Result := iit512PixelDataARGB;
      FourChar := kIconServices512PixelDataARGB;
    end;
  end; 
end;

function GetMaskType8bit(Width, Height: Integer; var FourChar: FourCharCode): TicnsIconType;
begin
  Result := iitNone;
  if Width <> Height then Exit;
  
  case Width of
    16: begin
      Result := iitSmall8BitMask;
      FourChar := kSmall8BitMask;
    end;
    32: begin
      Result := iitLarge8BitMask;
      FourChar := kLarge8BitMask;
    end;
    48: begin
      Result := iitHuge8BitMask;
      FourChar := kHuge8BitMask;
    end;
    128: begin
      Result := iitThumbnail8BitMask;
      FourChar := kThumbnail8BitMask;
    end;
  end; 
end;

function GetIcnsIconType(const StrIconType: FourCharCode): TicnsIconType;
begin
  Result := iitNone;

  if StrIconType = kMini4BitData then
    exit(iitMini4BitData);
    
  if StrIconType = kMini8BitData then
    exit(iitMini8BitData);

  if StrIconType = kSmall4BitData then
    exit(iitSmall4BitData);

  if StrIconType = kSmall8BitData then
    exit(iitSmall8BitData);

  if StrIconType = kSmall32BitData then
    exit(iitSmall32BitData);

  if StrIconType = kLarge4BitData then
    exit(iitLarge4BitData);

  if StrIconType = kLarge8BitData then
    exit(iitLarge8BitData);

  if StrIconType = kLarge32BitData then
    exit(iitLarge32BitData);

  if StrIconType = kHuge4BitData then
    exit(iitHuge4BitData);

  if StrIconType = kHuge8BitData then
    exit(iitHuge8BitData);

  if StrIconType = kHuge32BitData then
    exit(iitHuge32BitData);
    
  if StrIconType = kThumbnail32BitData then
    exit(iitThumbnail32BitData);

  if StrIconType = kMini1BitMask then
    exit(iitMini1BitMask);

  if StrIconType = kSmall1BitMask then
    exit(iitSmall1BitMask);

  if StrIconType = kSmall8BitMask then
    exit(iitSmall8BitMask);

  if StrIconType = kLarge1BitMask then
    exit(iitLarge1BitMask);

  if StrIconType = kLarge8BitMask then
    exit(iitLarge8BitMask);

  if StrIconType = kHuge1BitMask then
    exit(iitHuge1BitMask);

  if StrIconType = kHuge8BitMask then
    exit(iitHuge8BitMask);

  if StrIconType = kThumbnail8BitMask then
    exit(iitThumbnail8BitMask);

  if StrIconType = kIconServices256PixelDataARGB then
    exit(iit256PixelDataARGB);

  if StrIconType = kIconServices512PixelDataARGB then
    exit(iit512PixelDataARGB);
end;

end.

