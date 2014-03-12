{$IFOPT M+}
  {$M-}
  {$DEFINE ENABLE_TYPEINFO}
{$ENDIF}

type
{ IPropertyBag2 interface }

  tagPROPBAG2 = record
    dwType: DWORD;
    vt: TVarType;
    cfType: TClipFormat;
    dwHint: DWORD;
    pstrName: POleStr;
    clsid: TCLSID;
  end;
  TPropBag2 = tagPROPBAG2;
  PPropBag2 = ^TPropBag2;

  IPropertyBag2 = interface(IUnknown)
    ['{22F55882-280B-11d0-A8A9-00A0C90C2004}']
    function Read(pPropBag: PPropBag2; pErrLog: IErrorLog;
      pvarValue: PVariant; phrError: PHResult): HRESULT; stdcall;

    function Write(cProperties: ULONG; pPropBag: PPropBag2;
      pvarValue: PVariant): HRESULT; stdcall;
    function CountProperties(var pcProperties: ULONG): HRESULT; stdcall;

    function GetPropertyInfo(iProperty, cProperties: ULONG;
      pPropBag: PPropBag2; var pcProperties: ULONG): HRESULT; stdcall;

    function LoadObject(pstrName:POleStr; dwHint: DWORD; pUnkObject: IUnknown;
      pErrLog: IErrorLog): HRESULT; stdcall;
  end;

const
  SID_IWICPalette                         = '{00000040-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapSource                    = '{00000120-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICFormatConverter                 = '{00000301-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapScaler                    = '{00000302-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapClipper                   = '{E4FBCF03-223D-4e81-9333-D635556DD1B5}';
  SID_IWICBitmapFlipRotator               = '{5009834F-2D6A-41ce-9E1B-17C5AFF7A782}';
  SID_IWICBitmapLock                      = '{00000123-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmap                          = '{00000121-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICColorTransform                  = '{B66F034F-D0E2-40ab-B436-6DE39E321A94}';
  SID_IWICColorContext                    = '{3C613A02-34B2-44ea-9A7C-45AEA9C6FD6D}';
  SID_IWICFastMetadataEncoder             = '{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}';
  SID_IWICStream                          = '{135FF860-22B7-4ddf-B0F6-218F4F299A43}';
  SID_IWICEnumMetadataItem                = '{DC2BB46D-3F07-481E-8625-220C4AEDBB33}';
  SID_IWICMetadataQueryReader             = '{30989668-E1C9-4597-B395-458EEDB808DF}';
  SID_IWICMetadataQueryWriter             = '{A721791A-0DEF-4d06-BD91-2118BF1DB10B}';
  SID_IWICBitmapEncoder                   = '{00000103-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapFrameEncode               = '{00000105-a8f2-4877-ba0a-fd2b6645fb94}';
  SID_IWICBitmapDecoder                   = '{9EDDE9E7-8DEE-47ea-99DF-E6FAF2ED44BF}';
  SID_IWICBitmapSourceTransform           = '{3B16811B-6A43-4ec9-B713-3D5A0C13B940}';
  SID_IWICBitmapFrameDecode               = '{3B16811B-6A43-4ec9-A813-3D930C13B940}';
  SID_IWICProgressiveLevelControl         = '{DAAC296F-7AA5-4dbf-8D15-225C5976F891}';
  SID_IWICProgressCallback                = '{4776F9CD-9517-45FA-BF24-E89C5EC5C60C}';
  SID_IWICBitmapCodecProgressNotification = '{64C1024E-C3CF-4462-8078-88C2B11C46D9}';
  SID_IWICComponentInfo                   = '{23BC3F0A-698B-4357-886B-F24D50671334}';
  SID_IWICFormatConverterInfo             = '{9F34FB65-13F4-4f15-BC57-3726B5E53D9F}';
  SID_IWICBitmapCodecInfo                 = '{E87A44C4-B76E-4c47-8B09-298EB12A2714}';
  SID_IWICBitmapEncoderInfo               = '{94C9B4EE-A09F-4f92-8A1E-4A9BCE7E76FB}';
  SID_IWICBitmapDecoderInfo               = '{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}';
  SID_IWICPixelFormatInfo                 = '{E8EDA601-3D48-431a-AB44-69059BE88BBE}';
  SID_IWICPixelFormatInfo2                = '{A9DB33A2-AF5F-43C7-B679-74F5984B5AA4}';
  SID_IWICImagingFactory                  = '{ec5ec8a9-c395-4314-9c77-54d7a935ff70}';
  SID_IWICDevelopRawNotificationCallback  = '{95c75a6e-3e8c-4ec2-85a8-aebcc551e59b}';
  SID_IWICDevelopRaw                      = '{fbec5e44-f7be-4b65-b7f8-c0c81fef026d}';

  IID_IWICPalette:                         TGUID = SID_IWICPalette;
  {$EXTERNALSYM IID_IWICPalette}
  IID_IWICBitmapSource:                    TGUID = SID_IWICBitmapSource;
  {$EXTERNALSYM IID_IWICBitmapSource}
  IID_IWICFormatConverter:                 TGUID = SID_IWICFormatConverter;
  {$EXTERNALSYM IID_IWICFormatConverter}
  IID_IWICBitmapScaler:                    TGUID = SID_IWICBitmapScaler;
  {$EXTERNALSYM IID_IWICBitmapScaler}
  IID_IWICBitmapClipper:                   TGUID = SID_IWICBitmapClipper;
  {$EXTERNALSYM IID_IWICBitmapClipper}
  IID_IWICBitmapFlipRotator:               TGUID = SID_IWICBitmapFlipRotator;
  {$EXTERNALSYM IID_IWICBitmapFlipRotator}
  IID_IWICBitmapLock:                      TGUID = SID_IWICBitmapLock;
  {$EXTERNALSYM IID_IWICBitmapLock}
  IID_IWICBitmap:                          TGUID = SID_IWICBitmap;
  {$EXTERNALSYM IID_IWICBitmap}
  IID_IWICColorTransform:                  TGUID = SID_IWICColorTransform;
  {$EXTERNALSYM IID_IWICColorTransform}
  IID_IWICColorContext:                    TGUID = SID_IWICColorContext;
  {$EXTERNALSYM IID_IWICColorContext}
  IID_IWICFastMetadataEncoder:             TGUID = SID_IWICFastMetadataEncoder;
  {$EXTERNALSYM IID_IWICFastMetadataEncoder}
  IID_IWICStream:                          TGUID = SID_IWICStream;
  {$EXTERNALSYM IID_IWICStream}
  IID_IWICEnumMetadataItem:                TGUID = SID_IWICEnumMetadataItem;
  {$EXTERNALSYM IID_IWICEnumMetadataItem}
  IID_IWICMetadataQueryReader:             TGUID = SID_IWICMetadataQueryReader;
  {$EXTERNALSYM IID_IWICMetadataQueryReader}
  IID_IWICMetadataQueryWriter:             TGUID = SID_IWICMetadataQueryWriter;
  {$EXTERNALSYM IID_IWICMetadataQueryWriter}
  IID_IWICBitmapEncoder:                   TGUID = SID_IWICBitmapEncoder;
  {$EXTERNALSYM IID_IWICBitmapEncoder}
  IID_IWICBitmapFrameEncode:               TGUID = SID_IWICBitmapFrameEncode;
  {$EXTERNALSYM IID_IWICBitmapFrameEncode}
  IID_IWICBitmapDecoder:                   TGUID = SID_IWICBitmapDecoder;
  {$EXTERNALSYM IID_IWICBitmapDecoder}
  IID_IWICBitmapSourceTransform:           TGUID = SID_IWICBitmapSourceTransform;
  {$EXTERNALSYM IID_IWICBitmapSourceTransform}
  IID_IWICBitmapFrameDecode:               TGUID = SID_IWICBitmapFrameDecode;
  {$EXTERNALSYM IID_IWICBitmapFrameDecode}
  IID_IWICProgressiveLevelControl:         TGUID = SID_IWICProgressiveLevelControl;
  {$EXTERNALSYM IID_IWICProgressiveLevelControl}
  IID_IWICProgressCallback:                TGUID = SID_IWICProgressCallback;
  {$EXTERNALSYM IID_IWICProgressCallback}
  IID_IWICBitmapCodecProgressNotification: TGUID = SID_IWICBitmapCodecProgressNotification;
  {$EXTERNALSYM IID_IWICBitmapCodecProgressNotification}
  IID_IWICComponentInfo:                   TGUID = SID_IWICComponentInfo;
  {$EXTERNALSYM IID_IWICComponentInfo}
  IID_IWICFormatConverterInfo:             TGUID = SID_IWICFormatConverterInfo;
  {$EXTERNALSYM IID_IWICFormatConverterInfo}
  IID_IWICBitmapCodecInfo:                 TGUID = SID_IWICBitmapCodecInfo;
  {$EXTERNALSYM IID_IWICBitmapCodecInfo}
  IID_IWICBitmapEncoderInfo:               TGUID = SID_IWICBitmapEncoderInfo;
  {$EXTERNALSYM IID_IWICBitmapEncoderInfo}
  IID_IWICBitmapDecoderInfo:               TGUID = SID_IWICBitmapDecoderInfo;
  {$EXTERNALSYM IID_IWICBitmapDecoderInfo}
  IID_IWICPixelFormatInfo:                 TGUID = SID_IWICPixelFormatInfo;
  {$EXTERNALSYM IID_IWICPixelFormatInfo}
  IID_IWICPixelFormatInfo2:                TGUID = SID_IWICPixelFormatInfo2;
  {$EXTERNALSYM IID_IWICPixelFormatInfo2}
  IID_IWICImagingFactory:                  TGUID = SID_IWICImagingFactory;
  {$EXTERNALSYM IID_IWICImagingFactory}
  IID_IWICDevelopRawNotificationCallback:  TGUID = SID_IWICDevelopRawNotificationCallback;
  {$EXTERNALSYM IID_IWICDevelopRawNotificationCallback}
  IID_IWICDevelopRaw:                      TGUID = SID_IWICDevelopRaw;
  {$EXTERNALSYM IID_IWICDevelopRaw}

const
  WINCODEC_SDK_VERSION = $0236; 
  {$EXTERNALSYM WINCODEC_SDK_VERSION}
  CLSID_WICImagingFactory:           TGUID = '{CACAF262-9370-4615-A13B-9F5539DA4C0A}';
  {$EXTERNALSYM CLSID_WICImagingFactory}
  GUID_VendorMicrosoft:              TGUID = '{F0E749CA-EDEF-4589-A73A-EE0E626A2A2B}';
  {$EXTERNALSYM GUID_VendorMicrosoft}
  GUID_VendorMicrosoftBuiltIn:       TGUID = '{257A30FD-06B6-462B-AEA4-63F70B86E533}';
  {$EXTERNALSYM GUID_VendorMicrosoftBuiltIn}
  CLSID_WICBmpDecoder:               TGUID = '{6B462062-7CBF-400D-9FDB-813DD10F2778}';
  {$EXTERNALSYM CLSID_WICBmpDecoder}
  CLSID_WICPngDecoder:               TGUID = '{389EA17B-5078-4CDE-B6EF-25C15175C751}';
  {$EXTERNALSYM CLSID_WICPngDecoder}
  CLSID_WICIcoDecoder:               TGUID = '{C61BFCDF-2E0F-4AAD-A8D7-E06BAFEBCDFE}';
  {$EXTERNALSYM CLSID_WICIcoDecoder}
  CLSID_WICJpegDecoder:              TGUID = '{9456A480-E88B-43EA-9E73-0B2D9B71B1CA}';
  {$EXTERNALSYM CLSID_WICJpegDecoder}
  CLSID_WICGifDecoder:               TGUID = '{381DDA3C-9CE9-4834-A23E-1F98F8FC52BE}';
  {$EXTERNALSYM CLSID_WICGifDecoder}
  CLSID_WICTiffDecoder:              TGUID = '{B54E85D9-FE23-499F-8B88-6ACEA713752B}';
  {$EXTERNALSYM CLSID_WICTiffDecoder}
  CLSID_WICWmpDecoder:               TGUID = '{A26CEC36-234C-4950-AE16-E34AACE71D0D}';
  {$EXTERNALSYM CLSID_WICWmpDecoder}
  CLSID_WICBmpEncoder:               TGUID = '{69BE8BB4-D66D-47C8-865A-ED1589433782}';
  {$EXTERNALSYM CLSID_WICBmpEncoder}
  CLSID_WICPngEncoder:               TGUID = '{27949969-876A-41D7-9447-568F6A35A4DC}';
  {$EXTERNALSYM CLSID_WICPngEncoder}
  CLSID_WICJpegEncoder:              TGUID = '{1A34F5C1-4A5A-46DC-B644-1F4567E7A676}';
  {$EXTERNALSYM CLSID_WICJpegEncoder}
  CLSID_WICGifEncoder:               TGUID = '{114F5598-0B22-40A0-86A1-C83EA495ADBD}';
  {$EXTERNALSYM CLSID_WICGifEncoder}
  CLSID_WICTiffEncoder:              TGUID = '{0131BE10-2001-4C5F-A9B0-CC88FAB64CE8}';
  {$EXTERNALSYM CLSID_WICTiffEncoder}
  CLSID_WICWmpEncoder:               TGUID = '{AC4CE3CB-E1C1-44CD-8215-5A1665509EC2}';
  {$EXTERNALSYM CLSID_WICWmpEncoder}
  GUID_ContainerFormatBmp:           TGUID = '{0AF1D87E-FCFE-4188-BDEB-A7906471CBE3}';
  {$EXTERNALSYM GUID_ContainerFormatBmp}
  GUID_ContainerFormatPng:           TGUID = '{1B7CFAF4-713F-473C-BBCD-6137425FAEAF}';
  {$EXTERNALSYM GUID_ContainerFormatPng}
  GUID_ContainerFormatIco:           TGUID = '{A3A860C4-338F-4C17-919A-FBA4B5628F21}';
  {$EXTERNALSYM GUID_ContainerFormatIco}
  GUID_ContainerFormatJpeg:          TGUID = '{19E4A5AA-5662-4FC5-A0C0-1758028E1057}';
  {$EXTERNALSYM GUID_ContainerFormatJpeg}
  GUID_ContainerFormatTiff:          TGUID = '{163BCC30-E2E9-4F0B-961D-A3E9FDB788A3}';
  {$EXTERNALSYM GUID_ContainerFormatTiff}
  GUID_ContainerFormatGif:           TGUID = '{1F8A5601-7D4D-4CBD-9C82-1BC8D4EEB9A5}';
  {$EXTERNALSYM GUID_ContainerFormatGif}
  GUID_ContainerFormatWmp:           TGUID = '{57A37CAA-367A-4540-916B-F183C5093A4B}';
  {$EXTERNALSYM GUID_ContainerFormatWmp}
  CLSID_WICImagingCategories:        TGUID = '{FAE3D380-FEA4-4623-8C75-C6B61110B681}';
  {$EXTERNALSYM CLSID_WICImagingCategories}
  CATID_WICBitmapDecoders:           TGUID = '{7ED96837-96F0-4812-B211-F13C24117ED3}';
  {$EXTERNALSYM CATID_WICBitmapDecoders}
  CATID_WICBitmapEncoders:           TGUID = '{AC757296-3522-4E11-9862-C17BE5A1767E}';
  {$EXTERNALSYM CATID_WICBitmapEncoders}
  CATID_WICPixelFormats:             TGUID = '{2B46E70F-CDA7-473E-89F6-DC9630A2390B}';
  {$EXTERNALSYM CATID_WICPixelFormats}
  CATID_WICFormatConverters:         TGUID = '{7835EAE8-BF14-49D1-93CE-533A407B2248}';
  {$EXTERNALSYM CATID_WICFormatConverters}
  CATID_WICMetadataReader:           TGUID = '{05AF94D8-7174-4CD2-BE4A-4124B80EE4B8}';
  {$EXTERNALSYM CATID_WICMetadataReader}
  CATID_WICMetadataWriter:           TGUID = '{ABE3B9A4-257D-4B97-BD1A-294AF496222E}';
  {$EXTERNALSYM CATID_WICMetadataWriter}
  CLSID_WICDefaultFormatConverter:   TGUID = '{1A3F11DC-B514-4B17-8C5F-2154513852F1}';
  {$EXTERNALSYM CLSID_WICDefaultFormatConverter}
  CLSID_WICFormatConverterHighColor: TGUID = '{AC75D454-9F37-48F8-B972-4E19BC856011}';
  {$EXTERNALSYM CLSID_WICFormatConverterHighColor}
  CLSID_WICFormatConverterNChannel:  TGUID = '{C17CABB2-D4A3-47D7-A557-339B2EFBD4F1}';
  {$EXTERNALSYM CLSID_WICFormatConverterNChannel}
  CLSID_WICFormatConverterWMPhoto:   TGUID = '{9CB5172B-D600-46BA-AB77-77BB7E3A00D9}';
  {$EXTERNALSYM CLSID_WICFormatConverterWMPhoto}

type
  WICColor = Cardinal;
  {$EXTERNALSYM WICColor}
  TWICColor = WICColor;
  PWICColor = ^TWicColor;

  WICRect = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;
  {$EXTERNALSYM WICRect}
  PWICRect = ^WICRect;
  {$EXTERNALSYM PWICRect}


  WICInProcPointer = ^Byte;
  { $EXTERNALSYM WICInProcPointer}
  TWICInProcPointer = WICInProcPointer;
  PWICInProcPointer = ^TWICInProcPointer;

type
  WICColorContextType = type Integer;
  {$EXTERNALSYM WICColorContextType}
const
  WICColorContextUninitialized  = 0;
  {$EXTERNALSYM WICColorContextUninitialized}
  WICColorContextProfile        = $1;
  {$EXTERNALSYM WICColorContextProfile}
  WICColorContextExifColorSpace = $2;
  {$EXTERNALSYM WICColorContextExifColorSpace}

type
  REFWICPixelFormatGUID = PGUID;
  {$EXTERNALSYM REFWICPixelFormatGUID}
  WICPixelFormatGUID = TGUID;
  {$EXTERNALSYM WICPixelFormatGUID}
  TWICPixelFormatGUID = WICPixelFormatGUID;
  PWICPixelFormatGUID = ^TWICPixelFormatGUID;

const
  GUID_WICPixelFormatUndefined:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';
  {$EXTERNALSYM GUID_WICPixelFormatUndefined}
  GUID_WICPixelFormatDontCare:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC900}';
  {$EXTERNALSYM GUID_WICPixelFormatDontCare}
  GUID_WICPixelFormat1bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC901}';
  {$EXTERNALSYM GUID_WICPixelFormat1bppIndexed}
  GUID_WICPixelFormat2bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC902}';
  {$EXTERNALSYM GUID_WICPixelFormat2bppIndexed}
  GUID_WICPixelFormat4bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC903}';
  {$EXTERNALSYM GUID_WICPixelFormat4bppIndexed}
  GUID_WICPixelFormat8bppIndexed:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC904}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppIndexed}
  GUID_WICPixelFormatBlackWhite:           TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC905}';
  {$EXTERNALSYM GUID_WICPixelFormatBlackWhite}
  GUID_WICPixelFormat2bppGray:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC906}';
  {$EXTERNALSYM GUID_WICPixelFormat2bppGray}
  GUID_WICPixelFormat4bppGray:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC907}';
  {$EXTERNALSYM GUID_WICPixelFormat4bppGray}
  GUID_WICPixelFormat8bppGray:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC908}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppGray}
  GUID_WICPixelFormat8bppAlpha:            TGUID = '{E6CD0116-EEBA-4161-AA85-27DD9FB3A895}';
  {$EXTERNALSYM GUID_WICPixelFormat8bppAlpha}
  GUID_WICPixelFormat16bppBGR555:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC909}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppBGR555}
  GUID_WICPixelFormat16bppBGR565:          TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90A}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppBGR565}
  GUID_WICPixelFormat16bppBGRA5551:        TGUID = '{05EC7C2B-F1E6-4961-AD46-E1CC810A87D2}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppBGRA5551}
  GUID_WICPixelFormat16bppGray:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90B}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppGray}
  GUID_WICPixelFormat24bppBGR:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90C}';
  {$EXTERNALSYM GUID_WICPixelFormat24bppBGR}
  GUID_WICPixelFormat24bppRGB:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90D}';
  {$EXTERNALSYM GUID_WICPixelFormat24bppRGB}
  GUID_WICPixelFormat32bppBGR:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90E}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppBGR}
  GUID_WICPixelFormat32bppBGRA:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90F}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppBGRA}
  GUID_WICPixelFormat32bppPBGRA:           TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC910}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppPBGRA}
  GUID_WICPixelFormat32bppGrayFloat:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC911}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppGrayFloat}
  GUID_WICPixelFormat32bppRGBA:            TGUID = '{F5C7AD2D-6A8D-43DD-A7A8-A29935261AE9}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBA}
  GUID_WICPixelFormat32bppPRGBA:           TGUID = '{3CC4A650-A527-4D37-A916-3142C7EBEDBA}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppPRGBA}
  GUID_WICPixelFormat48bppRGB:             TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC915}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppRGB}
  GUID_WICPixelFormat48bppBGR:             TGUID = '{E605A384-B468-46CE-BB2E-36F180E64313}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppBGR}
  GUID_WICPixelFormat64bppRGBA:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC916}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBA}
  GUID_WICPixelFormat64bppBGRA:            TGUID = '{1562FF7C-D352-46F9-979E-42976B792246}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppBGRA}
  GUID_WICPixelFormat64bppPRGBA:           TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC917}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppPRGBA}
  GUID_WICPixelFormat64bppPBGRA:           TGUID = '{8C518E8E-A4EC-468B-AE70-C9A35A9C5530}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppPBGRA}
  GUID_WICPixelFormat16bppGrayFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC913}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppGrayFixedPoint}
  GUID_WICPixelFormat32bppBGR101010:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC914}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppBGR101010}
  GUID_WICPixelFormat48bppRGBFixedPoint:   TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC912}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppRGBFixedPoint}
  GUID_WICPixelFormat48bppBGRFixedPoint:   TGUID = '{49CA140E-CAB6-493B-9DDF-60187C37532A}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppBGRFixedPoint}
  GUID_WICPixelFormat96bppRGBFixedPoint:   TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC918}';
  {$EXTERNALSYM GUID_WICPixelFormat96bppRGBFixedPoint}
  GUID_WICPixelFormat128bppRGBAFloat:      TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC919}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBAFloat}
  GUID_WICPixelFormat128bppPRGBAFloat:     TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91A}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppPRGBAFloat}
  GUID_WICPixelFormat128bppRGBFloat:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91B}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBFloat}
  GUID_WICPixelFormat32bppCMYK:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91C}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppCMYK}
  GUID_WICPixelFormat64bppRGBAFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91D}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBAFixedPoint}
  GUID_WICPixelFormat64bppBGRAFixedPoint:  TGUID = '{356de33c-54d2-4a23-bb04-9b7bf9b1d42d}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppBGRAFixedPoint}
  GUID_WICPixelFormat64bppRGBFixedPoint:   TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC940}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBFixedPoint}
  GUID_WICPixelFormat128bppRGBAFixedPoint: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91E}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBAFixedPoint}
  GUID_WICPixelFormat128bppRGBFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC941}';
  {$EXTERNALSYM GUID_WICPixelFormat128bppRGBFixedPoint}
  GUID_WICPixelFormat64bppRGBAHalf:        TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93A}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBAHalf}
  GUID_WICPixelFormat64bppRGBHalf:         TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC942}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppRGBHalf}
  GUID_WICPixelFormat48bppRGBHalf:         TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93B}';
  {$EXTERNALSYM GUID_WICPixelFormat48bppRGBHalf}
  GUID_WICPixelFormat32bppRGBE:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93D}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBE}
  GUID_WICPixelFormat16bppGrayHalf:        TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93E}';
  {$EXTERNALSYM GUID_WICPixelFormat16bppGrayHalf}
  GUID_WICPixelFormat32bppGrayFixedPoint:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC93F}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppGrayFixedPoint}
  GUID_WICPixelFormat32bppRGBA1010102:     TGUID = '{25238D72-FCF9-4522-B514-5578E5AD55E0}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBA1010102}
  GUID_WICPixelFormat32bppRGBA1010102XR:   TGUID = '{00DE6B9A-C101-434B-B502-D0165EE1122C}';
  {$EXTERNALSYM GUID_WICPixelFormat32bppRGBA1010102XR}
  GUID_WICPixelFormat64bppCMYK:            TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC91F}';
  {$EXTERNALSYM GUID_WICPixelFormat64bppCMYK}
  GUID_WICPixelFormat24bpp3Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC920}';
  {$EXTERNALSYM GUID_WICPixelFormat24bpp3Channels}
  GUID_WICPixelFormat32bpp4Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC921}';
  {$EXTERNALSYM GUID_WICPixelFormat32bpp4Channels}
  GUID_WICPixelFormat40bpp5Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC922}';
  {$EXTERNALSYM GUID_WICPixelFormat40bpp5Channels}
  GUID_WICPixelFormat48bpp6Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC923}';
  {$EXTERNALSYM GUID_WICPixelFormat48bpp6Channels}
  GUID_WICPixelFormat56bpp7Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC924}';
  {$EXTERNALSYM GUID_WICPixelFormat56bpp7Channels}
  GUID_WICPixelFormat64bpp8Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC925}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp8Channels}
  GUID_WICPixelFormat48bpp3Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC926}';
  {$EXTERNALSYM GUID_WICPixelFormat48bpp3Channels}
  GUID_WICPixelFormat64bpp4Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC927}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp4Channels}
  GUID_WICPixelFormat80bpp5Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC928}';
  {$EXTERNALSYM GUID_WICPixelFormat80bpp5Channels}
  GUID_WICPixelFormat96bpp6Channels:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC929}';
  {$EXTERNALSYM GUID_WICPixelFormat96bpp6Channels}
  GUID_WICPixelFormat112bpp7Channels:      TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92A}';
  {$EXTERNALSYM GUID_WICPixelFormat112bpp7Channels}
  GUID_WICPixelFormat128bpp8Channels:      TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92B}';
  {$EXTERNALSYM GUID_WICPixelFormat128bpp8Channels}
  GUID_WICPixelFormat40bppCMYKAlpha:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92C}';
  {$EXTERNALSYM GUID_WICPixelFormat40bppCMYKAlpha}
  GUID_WICPixelFormat80bppCMYKAlpha:       TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92D}';
  {$EXTERNALSYM GUID_WICPixelFormat80bppCMYKAlpha}
  GUID_WICPixelFormat32bpp3ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92E}';
  {$EXTERNALSYM GUID_WICPixelFormat32bpp3ChannelsAlpha}
  GUID_WICPixelFormat40bpp4ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC92F}';
  {$EXTERNALSYM GUID_WICPixelFormat40bpp4ChannelsAlpha}
  GUID_WICPixelFormat48bpp5ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC930}';
  {$EXTERNALSYM GUID_WICPixelFormat48bpp5ChannelsAlpha}
  GUID_WICPixelFormat56bpp6ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC931}';
  {$EXTERNALSYM GUID_WICPixelFormat56bpp6ChannelsAlpha}
  GUID_WICPixelFormat64bpp7ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC932}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp7ChannelsAlpha}
  GUID_WICPixelFormat72bpp8ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC933}';
  {$EXTERNALSYM GUID_WICPixelFormat72bpp8ChannelsAlpha}
  GUID_WICPixelFormat64bpp3ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC934}';
  {$EXTERNALSYM GUID_WICPixelFormat64bpp3ChannelsAlpha}
  GUID_WICPixelFormat80bpp4ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC935}';
  {$EXTERNALSYM GUID_WICPixelFormat80bpp4ChannelsAlpha}
  GUID_WICPixelFormat96bpp5ChannelsAlpha:  TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC936}';
  {$EXTERNALSYM GUID_WICPixelFormat96bpp5ChannelsAlpha}
  GUID_WICPixelFormat112bpp6ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC937}';
  {$EXTERNALSYM GUID_WICPixelFormat112bpp6ChannelsAlpha}
  GUID_WICPixelFormat128bpp7ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC938}';
  {$EXTERNALSYM GUID_WICPixelFormat128bpp7ChannelsAlpha}
  GUID_WICPixelFormat144bpp8ChannelsAlpha: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC939}';
  {$EXTERNALSYM GUID_WICPixelFormat144bpp8ChannelsAlpha}
type
  WICBitmapCreateCacheOption = type Integer;
  {$EXTERNALSYM WICBitmapCreateCacheOption}
const
  WICBitmapNoCache                       = 0;
  {$EXTERNALSYM WICBitmapNoCache}
  WICBitmapCacheOnDemand                 = $1;
  {$EXTERNALSYM WICBitmapCacheOnDemand}
  WICBitmapCacheOnLoad                   = $2;
  {$EXTERNALSYM WICBitmapCacheOnLoad}
  WICBITMAPCREATECACHEOPTION_FORCE_DWORD = $7FFFFFFF;
  {$EXTERNALSYM WICBITMAPCREATECACHEOPTION_FORCE_DWORD}

type
  WICDecodeOptions = type Integer; 
  {$EXTERNALSYM WICDecodeOptions}
const
  WICDecodeMetadataCacheOnDemand     = 0; 
  {$EXTERNALSYM WICDecodeMetadataCacheOnDemand}
  WICDecodeMetadataCacheOnLoad       = $1; 
  {$EXTERNALSYM WICDecodeMetadataCacheOnLoad}
  WICMETADATACACHEOPTION_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICMETADATACACHEOPTION_FORCE_DWORD}

type
  WICBitmapEncoderCacheOption = type Integer; 
  {$EXTERNALSYM WICBitmapEncoderCacheOption}
const
  WICBitmapEncoderCacheInMemory           = 0; 
  {$EXTERNALSYM WICBitmapEncoderCacheInMemory}
  WICBitmapEncoderCacheTempFile           = $1; 
  {$EXTERNALSYM WICBitmapEncoderCacheTempFile}
  WICBitmapEncoderNoCache                 = $2;
  {$EXTERNALSYM WICBitmapEncoderNoCache}
  WICBITMAPENCODERCACHEOPTION_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPENCODERCACHEOPTION_FORCE_DWORD}

type
  WICComponentType = type Integer; 
  {$EXTERNALSYM WICComponentType}
const
  WICDecoder                   = $1; 
  {$EXTERNALSYM WICDecoder}
  WICEncoder                   = $2; 
  {$EXTERNALSYM WICEncoder}
  WICPixelFormatConverter      = $4; 
  {$EXTERNALSYM WICPixelFormatConverter}
  WICMetadataReader            = $8; 
  {$EXTERNALSYM WICMetadataReader}
  WICMetadataWriter            = $10; 
  {$EXTERNALSYM WICMetadataWriter}
  WICPixelFormat               = $20; 
  {$EXTERNALSYM WICPixelFormat}
  WICAllComponents             = $3F; 
  {$EXTERNALSYM WICAllComponents}
  WICCOMPONENTTYPE_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICCOMPONENTTYPE_FORCE_DWORD}

type
  WICComponentEnumerateOptions = type Integer; 
  {$EXTERNALSYM WICComponentEnumerateOptions}
const
  WICComponentEnumerateDefault             = 0; 
  {$EXTERNALSYM WICComponentEnumerateDefault}
  WICComponentEnumerateRefresh             = $1; 
  {$EXTERNALSYM WICComponentEnumerateRefresh}
  WICComponentEnumerateDisabled            = $80000000; 
  {$EXTERNALSYM WICComponentEnumerateDisabled}
  WICComponentEnumerateUnsigned            = $40000000; 
  {$EXTERNALSYM WICComponentEnumerateUnsigned}
  WICComponentEnumerateBuiltInOnly         = $20000000; 
  {$EXTERNALSYM WICComponentEnumerateBuiltInOnly}
  WICCOMPONENTENUMERATEOPTIONS_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICCOMPONENTENUMERATEOPTIONS_FORCE_DWORD}

type
  WICBitmapPattern = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Pattern: PBYTE;
    Mask: PBYTE;
    EndOfStream: BOOL;
  end;
  {$EXTERNALSYM WICBitmapPattern}
  TWICBitmapPattern = WICBitmapPattern;
  PWICBitmapPattern = ^WICBitmapPattern;

type
  WICBitmapInterpolationMode = type Integer; 
  {$EXTERNALSYM WICBitmapInterpolationMode}
const
  WICBitmapInterpolationModeNearestNeighbor = 0; 
  {$EXTERNALSYM WICBitmapInterpolationModeNearestNeighbor}
  WICBitmapInterpolationModeLinear          = $1; 
  {$EXTERNALSYM WICBitmapInterpolationModeLinear}
  WICBitmapInterpolationModeCubic           = $2; 
  {$EXTERNALSYM WICBitmapInterpolationModeCubic}
  WICBitmapInterpolationModeFant            = $3; 
  {$EXTERNALSYM WICBitmapInterpolationModeFant}
  WICBITMAPINTERPOLATIONMODE_FORCE_DWORD    = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPINTERPOLATIONMODE_FORCE_DWORD}

type
  WICBitmapPaletteType = type Integer; 
  {$EXTERNALSYM WICBitmapPaletteType}
const
  WICBitmapPaletteTypeCustom           = 0; 
  {$EXTERNALSYM WICBitmapPaletteTypeCustom}
  WICBitmapPaletteTypeMedianCut        = $1; 
  {$EXTERNALSYM WICBitmapPaletteTypeMedianCut}
  WICBitmapPaletteTypeFixedBW          = $2; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedBW}
  WICBitmapPaletteTypeFixedHalftone8   = $3; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone8}
  WICBitmapPaletteTypeFixedHalftone27  = $4; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone27}
  WICBitmapPaletteTypeFixedHalftone64  = $5; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone64}
  WICBitmapPaletteTypeFixedHalftone125 = $6;
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone125}
  WICBitmapPaletteTypeFixedHalftone216 = $7; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone216}
  WICBitmapPaletteTypeFixedWebPalette  = WICBITMAPPALETTETYPEFIXEDHALFTONE216; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedWebPalette}
  WICBitmapPaletteTypeFixedHalftone252 = $8; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone252}
  WICBitmapPaletteTypeFixedHalftone256 = $9; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedHalftone256}
  WICBitmapPaletteTypeFixedGray4       = $A; 
  {$EXTERNALSYM WICBitmapPaletteTypeFixedGray4}
  WICBitmapPaletteTypeFixedGray16      = $B;
  {$EXTERNALSYM WICBitmapPaletteTypeFixedGray16}
  WICBitmapPaletteTypeFixedGray256     = $C;
  {$EXTERNALSYM WICBitmapPaletteTypeFixedGray256}
  WICBITMAPPALETTETYPE_FORCE_DWORD     = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPPALETTETYPE_FORCE_DWORD}

type
  WICBitmapDitherType = type Integer; 
  {$EXTERNALSYM WICBitmapDitherType}
const
  WICBitmapDitherTypeNone           = 0; 
  {$EXTERNALSYM WICBitmapDitherTypeNone}
  WICBitmapDitherTypeSolid          = 0; 
  {$EXTERNALSYM WICBitmapDitherTypeSolid}
  WICBitmapDitherTypeOrdered4x4     = $1; 
  {$EXTERNALSYM WICBitmapDitherTypeOrdered4x4}
  WICBitmapDitherTypeOrdered8x8     = $2; 
  {$EXTERNALSYM WICBitmapDitherTypeOrdered8x8}
  WICBitmapDitherTypeOrdered16x16   = $3; 
  {$EXTERNALSYM WICBitmapDitherTypeOrdered16x16}
  WICBitmapDitherTypeSpiral4x4      = $4; 
  {$EXTERNALSYM WICBitmapDitherTypeSpiral4x4}
  WICBitmapDitherTypeSpiral8x8      = $5; 
  {$EXTERNALSYM WICBitmapDitherTypeSpiral8x8}
  WICBitmapDitherTypeDualSpiral4x4  = $6; 
  {$EXTERNALSYM WICBitmapDitherTypeDualSpiral4x4}
  WICBitmapDitherTypeDualSpiral8x8  = $7; 
  {$EXTERNALSYM WICBitmapDitherTypeDualSpiral8x8}
  WICBitmapDitherTypeErrorDiffusion = $8; 
  {$EXTERNALSYM WICBitmapDitherTypeErrorDiffusion}
  WICBITMAPDITHERTYPE_FORCE_DWORD   = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPDITHERTYPE_FORCE_DWORD}

type
  WICBitmapAlphaChannelOption = type Integer; 
  {$EXTERNALSYM WICBitmapAlphaChannelOption}
const
  WICBitmapUseAlpha                        = 0; 
  {$EXTERNALSYM WICBitmapUseAlpha}
  WICBitmapUsePremultipliedAlpha           = $1; 
  {$EXTERNALSYM WICBitmapUsePremultipliedAlpha}
  WICBitmapIgnoreAlpha                     = $2; 
  {$EXTERNALSYM WICBitmapIgnoreAlpha}
  WICBITMAPALPHACHANNELOPTIONS_FORCE_DWORD = $7FFFFFFF;
  {$EXTERNALSYM WICBITMAPALPHACHANNELOPTIONS_FORCE_DWORD}

type
  WICBitmapTransformOptions = type Integer;
  {$EXTERNALSYM WICBitmapTransformOptions}
const
  WICBitmapTransformRotate0             = 0; 
  {$EXTERNALSYM WICBitmapTransformRotate0}
  WICBitmapTransformRotate90            = $1; 
  {$EXTERNALSYM WICBitmapTransformRotate90}
  WICBitmapTransformRotate180           = $2; 
  {$EXTERNALSYM WICBitmapTransformRotate180}
  WICBitmapTransformRotate270           = $3; 
  {$EXTERNALSYM WICBitmapTransformRotate270}
  WICBitmapTransformFlipHorizontal      = $8; 
  {$EXTERNALSYM WICBitmapTransformFlipHorizontal}
  WICBitmapTransformFlipVertical        = $10; 
  {$EXTERNALSYM WICBitmapTransformFlipVertical}
  WICBITMAPTRANSFORMOPTIONS_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPTRANSFORMOPTIONS_FORCE_DWORD}

type
  WICBitmapLockFlags = type Integer; 
  {$EXTERNALSYM WICBitmapLockFlags}
const
  WICBitmapLockRead              = $1; 
  {$EXTERNALSYM WICBitmapLockRead}
  WICBitmapLockWrite             = $2; 
  {$EXTERNALSYM WICBitmapLockWrite}
  WICBITMAPLOCKFLAGS_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPLOCKFLAGS_FORCE_DWORD}

type
  WICBitmapDecoderCapabilities = type Integer; 
  {$EXTERNALSYM WICBitmapDecoderCapabilities}
const
  WICBitmapDecoderCapabilitySameEncoder          = $1; 
  {$EXTERNALSYM WICBitmapDecoderCapabilitySameEncoder}
  WICBitmapDecoderCapabilityCanDecodeAllImages   = $2; 
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanDecodeAllImages}
  WICBitmapDecoderCapabilityCanDecodeSomeImages  = $4; 
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanDecodeSomeImages}
  WICBitmapDecoderCapabilityCanEnumerateMetadata = $8; 
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanEnumerateMetadata}
  WICBitmapDecoderCapabilityCanDecodeThumbnail   = $10; 
  {$EXTERNALSYM WICBitmapDecoderCapabilityCanDecodeThumbnail}
  WICBITMAPDECODERCAPABILITIES_FORCE_DWORD       = $7FFFFFFF; 
  {$EXTERNALSYM WICBITMAPDECODERCAPABILITIES_FORCE_DWORD}

type
  WICProgressOperation = type Integer; 
  {$EXTERNALSYM WICProgressOperation}
const
  WICProgressOperationCopyPixels   = $1;
  {$EXTERNALSYM WICProgressOperationCopyPixels}
  WICProgressOperationWritePixels  = $2; 
  {$EXTERNALSYM WICProgressOperationWritePixels}
  WICProgressOperationAll          = $FFFF; 
  {$EXTERNALSYM WICProgressOperationAll}
  WICPROGRESSOPERATION_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPROGRESSOPERATION_FORCE_DWORD}

type
  WICProgressNotification = type Integer; 
  {$EXTERNALSYM WICProgressNotification}
const
  WICProgressNotificationBegin        = $10000; 
  {$EXTERNALSYM WICProgressNotificationBegin}
  WICProgressNotificationEnd          = $20000; 
  {$EXTERNALSYM WICProgressNotificationEnd}
  WICProgressNotificationFrequent     = $40000; 
  {$EXTERNALSYM WICProgressNotificationFrequent}
  WICProgressNotificationAll          = $FFFF0000; 
  {$EXTERNALSYM WICProgressNotificationAll}
  WICPROGRESSNOTIFICATION_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPROGRESSNOTIFICATION_FORCE_DWORD}

type
  WICComponentSigning = type Integer; 
  {$EXTERNALSYM WICComponentSigning}
const
  WICComponentSigned              = $1; 
  {$EXTERNALSYM WICComponentSigned}
  WICComponentUnsigned            = $2;
  {$EXTERNALSYM WICComponentUnsigned}
  WICComponentSafe                = $4; 
  {$EXTERNALSYM WICComponentSafe}
  WICComponentDisabled            = $80000000; 
  {$EXTERNALSYM WICComponentDisabled}
  WICCOMPONENTSIGNING_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICCOMPONENTSIGNING_FORCE_DWORD}

type
  WICGifLogicalScreenDescriptorProperties = type Integer; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorProperties}
const
  WICGifLogicalScreenSignature                        = $1; 
  {$EXTERNALSYM WICGifLogicalScreenSignature}
  WICGifLogicalScreenDescriptorWidth                  = $2; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorWidth}
  WICGifLogicalScreenDescriptorHeight                 = $3; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorHeight}
  WICGifLogicalScreenDescriptorGlobalColorTableFlag   = $4;
  {$EXTERNALSYM WICGifLogicalScreenDescriptorGlobalColorTableFlag}
  WICGifLogicalScreenDescriptorColorResolution        = $5; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorColorResolution}
  WICGifLogicalScreenDescriptorSortFlag               = $6; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorSortFlag}
  WICGifLogicalScreenDescriptorGlobalColorTableSize   = $7; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorGlobalColorTableSize}
  WICGifLogicalScreenDescriptorBackgroundColorIndex   = $8;
  {$EXTERNALSYM WICGifLogicalScreenDescriptorBackgroundColorIndex}
  WICGifLogicalScreenDescriptorPixelAspectRatio       = $9; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorPixelAspectRatio}
  WICGifLogicalScreenDescriptorProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICGifLogicalScreenDescriptorProperties_FORCE_DWORD}

type
  WICGifImageDescriptorProperties = type Integer; 
  {$EXTERNALSYM WICGifImageDescriptorProperties}
const
  WICGifImageDescriptorLeft                   = $1; 
  {$EXTERNALSYM WICGifImageDescriptorLeft}
  WICGifImageDescriptorTop                    = $2; 
  {$EXTERNALSYM WICGifImageDescriptorTop}
  WICGifImageDescriptorWidth                  = $3;
  {$EXTERNALSYM WICGifImageDescriptorWidth}
  WICGifImageDescriptorHeight                 = $4; 
  {$EXTERNALSYM WICGifImageDescriptorHeight}
  WICGifImageDescriptorLocalColorTableFlag    = $5; 
  {$EXTERNALSYM WICGifImageDescriptorLocalColorTableFlag}
  WICGifImageDescriptorInterlaceFlag          = $6; 
  {$EXTERNALSYM WICGifImageDescriptorInterlaceFlag}
  WICGifImageDescriptorSortFlag               = $7; 
  {$EXTERNALSYM WICGifImageDescriptorSortFlag}
  WICGifImageDescriptorLocalColorTableSize    = $8; 
  {$EXTERNALSYM WICGifImageDescriptorLocalColorTableSize}
  WICGifImageDescriptorProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICGifImageDescriptorProperties_FORCE_DWORD}

type
  WICGifGraphicControlExtensionProperties = type Integer; 
  {$EXTERNALSYM WICGifGraphicControlExtensionProperties}
const
  WICGifGraphicControlExtensionDisposal               = $1; 
  {$EXTERNALSYM WICGifGraphicControlExtensionDisposal}
  WICGifGraphicControlExtensionUserInputFlag          = $2; 
  {$EXTERNALSYM WICGifGraphicControlExtensionUserInputFlag}
  WICGifGraphicControlExtensionTransparencyFlag       = $3; 
  {$EXTERNALSYM WICGifGraphicControlExtensionTransparencyFlag}
  WICGifGraphicControlExtensionDelay                  = $4;
  {$EXTERNALSYM WICGifGraphicControlExtensionDelay}
  WICGifGraphicControlExtensionTransparentColorIndex  = $5; 
  {$EXTERNALSYM WICGifGraphicControlExtensionTransparentColorIndex}
  WICGifGraphicControlExtensionProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICGifGraphicControlExtensionProperties_FORCE_DWORD}

type
  WICGifApplicationExtensionProperties = type Integer; 
  {$EXTERNALSYM WICGifApplicationExtensionProperties}
const
  WICGifApplicationExtensionApplication            = $1; 
  {$EXTERNALSYM WICGifApplicationExtensionApplication}
  WICGifApplicationExtensionData                   = $2; 
  {$EXTERNALSYM WICGifApplicationExtensionData}
  WICGifApplicationExtensionProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICGifApplicationExtensionProperties_FORCE_DWORD}

type
  WICGifCommentExtensionProperties = type Integer; 
  {$EXTERNALSYM WICGifCommentExtensionProperties}
const
  WICGifCommentExtensionText                   = $1; 
  {$EXTERNALSYM WICGifCommentExtensionText}
  WICGifCommentExtensionProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICGifCommentExtensionProperties_FORCE_DWORD}

type
  WICJpegCommentProperties = type Integer; 
  {$EXTERNALSYM WICJpegCommentProperties}
const
  WICJpegCommentText                   = $1; 
  {$EXTERNALSYM WICJpegCommentText}
  WICJpegCommentProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICJpegCommentProperties_FORCE_DWORD}

type
  WICJpegLuminanceProperties = type Integer; 
  {$EXTERNALSYM WICJpegLuminanceProperties}
const
  WICJpegLuminanceTable                  = $1; 
  {$EXTERNALSYM WICJpegLuminanceTable}
  WICJpegLuminanceProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICJpegLuminanceProperties_FORCE_DWORD}

type
  WICJpegChrominanceProperties = type Integer; 
  {$EXTERNALSYM WICJpegChrominanceProperties}
const
  WICJpegChrominanceTable                  = $1;
  {$EXTERNALSYM WICJpegChrominanceTable}
  WICJpegChrominanceProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICJpegChrominanceProperties_FORCE_DWORD}

type
  WIC8BIMIptcProperties = type Integer; 
  {$EXTERNALSYM WIC8BIMIptcProperties}
const
  WIC8BIMIptcPString                = 0; 
  {$EXTERNALSYM WIC8BIMIptcPString}
  WIC8BIMIptcEmbeddedIPTC           = $1; 
  {$EXTERNALSYM WIC8BIMIptcEmbeddedIPTC}
  WIC8BIMIptcProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WIC8BIMIptcProperties_FORCE_DWORD}

type
  WIC8BIMResolutionInfoProperties = type Integer; 
  {$EXTERNALSYM WIC8BIMResolutionInfoProperties}
const
  WIC8BIMResolutionInfoPString                = $1; 
  {$EXTERNALSYM WIC8BIMResolutionInfoPString}
  WIC8BIMResolutionInfoHResolution            = $2; 
  {$EXTERNALSYM WIC8BIMResolutionInfoHResolution}
  WIC8BIMResolutionInfoHResolutionUnit        = $3; 
  {$EXTERNALSYM WIC8BIMResolutionInfoHResolutionUnit}
  WIC8BIMResolutionInfoWidthUnit              = $4; 
  {$EXTERNALSYM WIC8BIMResolutionInfoWidthUnit}
  WIC8BIMResolutionInfoVResolution            = $5; 
  {$EXTERNALSYM WIC8BIMResolutionInfoVResolution}
  WIC8BIMResolutionInfoVResolutionUnit        = $6; 
  {$EXTERNALSYM WIC8BIMResolutionInfoVResolutionUnit}
  WIC8BIMResolutionInfoHeightUnit             = $7; 
  {$EXTERNALSYM WIC8BIMResolutionInfoHeightUnit}
  WIC8BIMResolutionInfoProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WIC8BIMResolutionInfoProperties_FORCE_DWORD}

type
  WIC8BIMIptcDigestProperties = type Integer;
  {$EXTERNALSYM WIC8BIMIptcDigestProperties}
const
  WIC8BIMIptcDigestPString                = $1; 
  {$EXTERNALSYM WIC8BIMIptcDigestPString}
  WIC8BIMIptcDigestIptcDigest             = $2; 
  {$EXTERNALSYM WIC8BIMIptcDigestIptcDigest}
  WIC8BIMIptcDigestProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WIC8BIMIptcDigestProperties_FORCE_DWORD}

type
  WICPngGamaProperties = type Integer;
  {$EXTERNALSYM WICPngGamaProperties}
const
  WICPngGamaGamma                  = $1;
  {$EXTERNALSYM WICPngGamaGamma}
  WICPngGamaProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngGamaProperties_FORCE_DWORD}

type
  WICPngBkgdProperties = type Integer; 
  {$EXTERNALSYM WICPngBkgdProperties}
const
  WICPngBkgdBackgroundColor        = $1; 
  {$EXTERNALSYM WICPngBkgdBackgroundColor}
  WICPngBkgdProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngBkgdProperties_FORCE_DWORD}

type
  WICPngItxtProperties = type Integer; 
  {$EXTERNALSYM WICPngItxtProperties}
const
  WICPngItxtKeyword                = $1; 
  {$EXTERNALSYM WICPngItxtKeyword}
  WICPngItxtCompressionFlag        = $2; 
  {$EXTERNALSYM WICPngItxtCompressionFlag}
  WICPngItxtLanguageTag            = $3; 
  {$EXTERNALSYM WICPngItxtLanguageTag}
  WICPngItxtTranslatedKeyword      = $4; 
  {$EXTERNALSYM WICPngItxtTranslatedKeyword}
  WICPngItxtText                   = $5; 
  {$EXTERNALSYM WICPngItxtText}
  WICPngItxtProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngItxtProperties_FORCE_DWORD}

type
  WICPngChrmProperties = type Integer; 
  {$EXTERNALSYM WICPngChrmProperties}
const
  WICPngChrmWhitePointX            = $1;
  {$EXTERNALSYM WICPngChrmWhitePointX}
  WICPngChrmWhitePointY            = $2; 
  {$EXTERNALSYM WICPngChrmWhitePointY}
  WICPngChrmRedX                   = $3; 
  {$EXTERNALSYM WICPngChrmRedX}
  WICPngChrmRedY                   = $4; 
  {$EXTERNALSYM WICPngChrmRedY}
  WICPngChrmGreenX                 = $5; 
  {$EXTERNALSYM WICPngChrmGreenX}
  WICPngChrmGreenY                 = $6; 
  {$EXTERNALSYM WICPngChrmGreenY}
  WICPngChrmBlueX                  = $7;
  {$EXTERNALSYM WICPngChrmBlueX}
  WICPngChrmBlueY                  = $8; 
  {$EXTERNALSYM WICPngChrmBlueY}
  WICPngChrmProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngChrmProperties_FORCE_DWORD}

type
  WICPngHistProperties = type Integer; 
  {$EXTERNALSYM WICPngHistProperties}
const
  WICPngHistFrequencies            = $1; 
  {$EXTERNALSYM WICPngHistFrequencies}
  WICPngHistProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngHistProperties_FORCE_DWORD}

type
  WICPngIccpProperties = type Integer; 
  {$EXTERNALSYM WICPngIccpProperties}
const
  WICPngIccpProfileName            = $1; 
  {$EXTERNALSYM WICPngIccpProfileName}
  WICPngIccpProfileData            = $2; 
  {$EXTERNALSYM WICPngIccpProfileData}
  WICPngIccpProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngIccpProperties_FORCE_DWORD}

type
  WICPngSrgbProperties = type Integer; 
  {$EXTERNALSYM WICPngSrgbProperties}
const
  WICPngSrgbRenderingIntent        = $1; 
  {$EXTERNALSYM WICPngSrgbRenderingIntent}
  WICPngSrgbProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngSrgbProperties_FORCE_DWORD}

type
  WICPngTimeProperties = type Integer;
  {$EXTERNALSYM WICPngTimeProperties}
const
  WICPngTimeYear                   = $1; 
  {$EXTERNALSYM WICPngTimeYear}
  WICPngTimeMonth                  = $2; 
  {$EXTERNALSYM WICPngTimeMonth}
  WICPngTimeDay                    = $3; 
  {$EXTERNALSYM WICPngTimeDay}
  WICPngTimeHour                   = $4; 
  {$EXTERNALSYM WICPngTimeHour}
  WICPngTimeMinute                 = $5;
  {$EXTERNALSYM WICPngTimeMinute}
  WICPngTimeSecond                 = $6; 
  {$EXTERNALSYM WICPngTimeSecond}
  WICPngTimeProperties_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPngTimeProperties_FORCE_DWORD}

type
  WICSectionAccessLevel = type Integer; 
  {$EXTERNALSYM WICSectionAccessLevel}
const
  WICSectionAccessLevelRead         = $1; 
  {$EXTERNALSYM WICSectionAccessLevelRead}
  WICSectionAccessLevelReadWrite    = $3; 
  {$EXTERNALSYM WICSectionAccessLevelReadWrite}
  WICSectionAccessLevel_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICSectionAccessLevel_FORCE_DWORD}

type
  WICPixelFormatNumericRepresentation = type Integer;
  {$EXTERNALSYM WICPixelFormatNumericRepresentation}
const
  WICPixelFormatNumericRepresentationUnspecified     = 0; 
  {$EXTERNALSYM WICPixelFormatNumericRepresentationUnspecified}
  WICPixelFormatNumericRepresentationIndexed         = $1; 
  {$EXTERNALSYM WICPixelFormatNumericRepresentationIndexed}
  WICPixelFormatNumericRepresentationUnsignedInteger = $2;
  {$EXTERNALSYM WICPixelFormatNumericRepresentationUnsignedInteger}
  WICPixelFormatNumericRepresentationSignedInteger   = $3; 
  {$EXTERNALSYM WICPixelFormatNumericRepresentationSignedInteger}
  WICPixelFormatNumericRepresentationFixed           = $4; 
  {$EXTERNALSYM WICPixelFormatNumericRepresentationFixed}
  WICPixelFormatNumericRepresentationFloat           = $5; 
  {$EXTERNALSYM WICPixelFormatNumericRepresentationFloat}
  WICPixelFormatNumericRepresentation_FORCE_DWORD    = $7FFFFFFF; 
  {$EXTERNALSYM WICPixelFormatNumericRepresentation_FORCE_DWORD}

type
  IWICBitmapSource = interface;
  IWICMetadataQueryWriter = interface;
  IWICBitmapEncoderInfo = interface;
  IWICBitmapFrameEncode = interface;
  IWICBitmapDecoderInfo = interface;
  IWICBitmapFrameDecode = interface;

{ interface IWICPalette }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPalette);'}
  IWICPalette = interface(IUnknown)
    [SID_IWICPalette]
    function InitializePredefined(ePaletteType: WICBitmapPaletteType; 
      fAddTransparentColor: BOOL): HRESULT; stdcall;

    function InitializeCustom(pColors: PWICColor; cCount: UINT): HRESULT; stdcall;
  
    function InitializeFromBitmap(pISurface: IWICBitmapSource; cCount: UINT;
      fAddTransparentColor: BOOL): HRESULT; stdcall;

    function InitializeFromPalette(pIPalette: IWICPalette): HRESULT; stdcall;

    function GetType(var pePaletteType: WICBitmapPaletteType): HRESULT; stdcall;

    function GetColorCount(var pcCount: UINT): HRESULT; stdcall;

    function GetColors(cCount: UINT; pColors: PWICColor;
      var pcActualColors: UINT): HRESULT; stdcall;

    function IsBlackWhite(var pfIsBlackWhite: BOOL): HRESULT; stdcall;

    function IsGrayscale(var pfIsGrayscale: BOOL): HRESULT; stdcall;

    function HasAlpha(var pfHasAlpha: BOOL): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICPalette}

{ interface IWICBitmapSource }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapSource);'}
  IWICBitmapSource = interface(IUnknown)
    [SID_IWICBitmapSource]
    function GetSize(var puiWidth: UINT; var puiHeight: UINT): HRESULT; stdcall;

    function GetPixelFormat(
      var pPixelFormat: WICPixelFormatGUID): HRESULT; stdcall;

    function GetResolution(var pDpiX: Double; var pDpiY: Double): HRESULT; stdcall;

    function CopyPalette(pIPalette: IWICPalette): HRESULT; stdcall;

    function CopyPixels(prc: PWICRect; cbStride: UINT; cbBufferSize: UINT;
      pbBuffer: PByte): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapSource}

{ interface IWICFormatConverter }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICFormatConverter);'}
  IWICFormatConverter = interface(IWICBitmapSource)
    [SID_IWICFormatConverter]
    function Initialize(pISource: IWICBitmapSource;
      const dstFormat: WICPixelFormatGUID; dither: WICBitmapDitherType;
      const pIPalette: IWICPalette; alphaThresholdPercent: Double;
      paletteTranslate: WICBitmapPaletteType): HRESULT; stdcall;

    function CanConvert(srcPixelFormat: REFWICPixelFormatGUID;
      dstPixelFormat: REFWICPixelFormatGUID;
      var pfCanConvert: BOOL): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICFormatConverter}

{ interface IWICBitmapScaler }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapScaler);'}
  IWICBitmapScaler = interface(IWICBitmapSource)
    [SID_IWICBitmapScaler]
    function Initialize(pISource: IWICBitmapSource; uiWidth: UINT;
      uiHeight: UINT; mode: WICBitmapInterpolationMode): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapScaler}

{ interface IWICBitmapClipper }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapClipper);'}
  IWICBitmapClipper = interface(IWICBitmapSource)
    [SID_IWICBitmapClipper]
    function Initialize(pISource: IWICBitmapSource;
      var prc: WICRect): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapClipper}

{ interface IWICBitmapFlipRotator }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapFlipRotator);'}
  IWICBitmapFlipRotator = interface(IWICBitmapSource)
    [SID_IWICBitmapFlipRotator]
    function Initialize(pISource: IWICBitmapSource;
      options: WICBitmapTransformOptions): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapFlipRotator}

{ interface IWICBitmapLock }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapLock);'}
  IWICBitmapLock = interface(IUnknown)
    [SID_IWICBitmapLock]
    function GetSize(var puiWidth: UINT; var puiHeight: UINT): HRESULT; stdcall;

    function GetStride(var pcbStride: UINT): HRESULT; stdcall;

    function GetDataPointer(var pcbBufferSize: UINT;
      var ppbData: WICInProcPointer): HRESULT; stdcall;

    function GetPixelFormat(
      var pPixelFormat: WICPixelFormatGUID): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapLock}

{ interface IWICBitmap }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmap);'}
  IWICBitmap = interface(IWICBitmapSource)
    [SID_IWICBitmap]
    function Lock(prcLock: PWICRect; flags: DWORD;
      out ppILock: IWICBitmapLock): HRESULT; stdcall;

    function SetPalette(pIPalette: IWICPalette): HRESULT; stdcall;

    function SetResolution(dpiX: Double; dpiY: Double): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmap}

{ interface IWICColorContext }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICColorContext);'}
  IWICColorContext = interface(IUnknown)
    [SID_IWICColorContext]
    function InitializeFromFilename(wzFilename: LPCWSTR): HRESULT; stdcall;

    function InitializeFromMemory(const pbBuffer: PByte;
      cbBufferSize: UINT): HRESULT; stdcall;

    function InitializeFromExifColorSpace(value: UINT): HRESULT; stdcall;

    function GetType(var pType: WICColorContextType): HRESULT; stdcall;

    function GetProfileBytes(cbBuffer: UINT;
      pbBuffer: PBYTE; var pcbActual: UINT): HRESULT; stdcall;

    function GetExifColorSpace(var pValue: UINT): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICColorContext}
  PIWICColorContext = ^IWICColorContext;
  {$EXTERNALSYM PIWICColorContext}                    

{ interface IWICColorTransform }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICColorTransform);'}
  IWICColorTransform = interface(IWICBitmapSource)
    [SID_IWICColorTransform]
    function Initialize(pIBitmapSource: IWICBitmapSource;
      pIContextSource: IWICColorContext; pIContextDest: IWICColorContext;
      pixelFmtDest: REFWICPixelFormatGUID): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICColorTransform}

{ interface IWICFastMetadataEncoder }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICFastMetadataEncoder);'}
  IWICFastMetadataEncoder = interface(IUnknown)
    [SID_IWICFastMetadataEncoder]
    function Commit: HRESULT; stdcall;

    function GetMetadataQueryWriter(
      out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICFastMetadataEncoder}

{ interface IWICStream }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICStream);'}
  IWICStream = interface(IStream)
    [SID_IWICStream]
    function InitializeFromIStream(pIStream: IStream): HRESULT; stdcall;

    function InitializeFromFilename(wzFileName: LPCWSTR;
      dwDesiredAccess: DWORD): HRESULT; stdcall;

    function InitializeFromMemory(pbBuffer: WICInProcPointer;
      cbBufferSize: DWORD): HRESULT; stdcall;

    function InitializeFromIStreamRegion(pIStream: IStream;
      ulOffset: ULARGE_INTEGER; ulMaxSize: ULARGE_INTEGER): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICStream}

{ interface IWICEnumMetadataItem }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICEnumMetadataItem);'}
  IWICEnumMetadataItem = interface(IUnknown)
    [SID_IWICEnumMetadataItem]
    function Next(celt: Cardinal;
      rgeltSchema: PPropVariant;
      rgeltID: PPropVariant;
      rgeltValue: PPropVariant;
      var pceltFetched: ULONG): HRESULT; stdcall;

    function Skip(celt: Cardinal): HRESULT; stdcall;

    function Reset: HRESULT; stdcall;

    function Clone(
      out ppIEnumMetadataItem: IWICEnumMetadataItem): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICEnumMetadataItem}

{ interface IWICMetadataQueryReader }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataQueryReader);'}
  IWICMetadataQueryReader = interface(IUnknown)
    [SID_IWICMetadataQueryReader]
    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;

    function GetLocation(cchMaxLength: UINT;
      wzNamespace: PWCHAR;
      var pcchActualLength: UINT): HRESULT; stdcall;

    function GetMetadataByName(wzName: LPCWSTR;
      var pvarValue: PROPVARIANT): HRESULT; stdcall;

    function GetEnumerator(out ppIEnumString: IEnumString): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICMetadataQueryReader}

{ interface IWICMetadataQueryWriter }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICMetadataQueryWriter);'}
  IWICMetadataQueryWriter = interface(IWICMetadataQueryReader)
    [SID_IWICMetadataQueryWriter]
    function SetMetadataByName(wzName: LPCWSTR;
      const pvarValue: TPropVariant): HRESULT; stdcall;

    function RemoveMetadataByName(wzName: LPCWSTR): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICMetadataQueryWriter}

{ interface IWICBitmapEncoder }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapEncoder);'}
  IWICBitmapEncoder = interface(IUnknown)
    [SID_IWICBitmapEncoder]
    function Initialize(pIStream: IStream;
      cacheOption: WICBitmapEncoderCacheOption): HRESULT; stdcall;

    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;

    function GetEncoderInfo(
      out ppIEncoderInfo: IWICBitmapEncoderInfo): HRESULT; stdcall;

    function SetColorContexts(cCount: UINT;
      ppIColorContext: PIWICColorContext): HRESULT; stdcall;

    function SetPalette(pIPalette: IWICPalette): HRESULT; stdcall;

    function SetThumbnail(pIThumbnail: IWICBitmapSource): HRESULT; stdcall;

    function SetPreview(pIPreview: IWICBitmapSource): HRESULT; stdcall;

    function CreateNewFrame(out ppIFrameEncode: IWICBitmapFrameEncode;
      var ppIEncoderOptions: IPropertyBag2): HRESULT; stdcall;

    function Commit: HRESULT; stdcall;

    function GetMetadataQueryWriter(
      out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapEncoder}

{ interface IWICBitmapFrameEncode }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapFrameEncode);'}
  IWICBitmapFrameEncode = interface(IUnknown)
    [SID_IWICBitmapFrameEncode]
    function Initialize(pIEncoderOptions: IUnknown): HRESULT; stdcall;

    function SetSize(uiWidth: UINT; uiHeight: UINT): HRESULT; stdcall;

    function SetResolution(dpiX: Double; dpiY: Double): HRESULT; stdcall;

    function SetPixelFormat(
      var pPixelFormat: WICPixelFormatGUID): HRESULT; stdcall;

    function SetColorContexts(cCount: UINT;
      ppIColorContext: PIWICColorContext): HRESULT; stdcall;

    function SetPalette(pIPalette: IWICPalette): HRESULT; stdcall;

    function SetThumbnail(pIThumbnail: IWICBitmapSource): HRESULT; stdcall;

    function WritePixels(lineCount: UINT; cbStride: UINT; cbBufferSize: UINT;
      pbPixels: PByte): HRESULT; stdcall;

    function WriteSource(pIBitmapSource: IWICBitmapSource;
      prc: PWICRect): HRESULT; stdcall;

    function Commit: HRESULT; stdcall;

    function GetMetadataQueryWriter(
      out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapFrameEncode}

{ interface IWICBitmapDecoder }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapDecoder);'}
  IWICBitmapDecoder = interface(IUnknown)
    [SID_IWICBitmapDecoder]
    function QueryCapability(pIStream: IStream;
      var pdwCapability: DWORD): HRESULT; stdcall;

    function Initialize(pIStream: IStream;
      cacheOptions: WICDecodeOptions): HRESULT; stdcall;

    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;

    function GetDecoderInfo(
      out ppIDecoderInfo: IWICBitmapDecoderInfo): HRESULT; stdcall;

    function CopyPalette(pIPalette: IWICPalette): HRESULT; stdcall;

    function GetMetadataQueryReader(
      out ppIMetadataQueryReader: IWICMetadataQueryReader): HRESULT; stdcall;

    function GetPreview(out ppIBitmapSource: IWICBitmapSource): HRESULT; stdcall;

    function GetColorContexts(cCount: UINT;
      ppIColorContexts: PIWICColorContext;
      var pcActualCount : UINT): HRESULT; stdcall;

    function GetThumbnail(out ppIThumbnail: IWICBitmapSource): HRESULT; stdcall;

    function GetFrameCount(var pCount: UINT): HRESULT; stdcall;

    function GetFrame(index: UINT;
      out ppIBitmapFrame: IWICBitmapFrameDecode): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapDecoder}

{ interface IWICBitmapSourceTransform }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapSourceTransform);'}
  IWICBitmapSourceTransform = interface(IUnknown)
    [SID_IWICBitmapSourceTransform]
    function CopyPixels(var prc: WICRect; uiWidth: UINT; uiHeight: UINT;
      var pguidDstFormat: WICPixelFormatGUID;
      dstTransform: WICBitmapTransformOptions; nStride: UINT; cbBufferSize: UINT;
      pbBuffer: PByte): HRESULT; stdcall;

    function GetClosestSize(var puiWidth: UINT;
      var puiHeight: UINT): HRESULT; stdcall;

    function GetClosestPixelFormat(
      var pguidDstFormat: WICPixelFormatGUID): HRESULT; stdcall;

    function DoesSupportTransform(dstTransform: WICBitmapTransformOptions;
      var pfIsSupported: BOOL): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapSourceTransform}

{ interface IWICBitmapFrameDecode }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapFrameDecode);'}
  IWICBitmapFrameDecode = interface(IWICBitmapSource)
    [SID_IWICBitmapFrameDecode]
    function GetMetadataQueryReader(
      out ppIMetadataQueryReader: IWICMetadataQueryReader): HRESULT; stdcall;

    function GetColorContexts(cCount: UINT;
      ppIColorContexts: PIWICColorContext;
      var pcActualCount : UINT): HRESULT; stdcall;

    function GetThumbnail(out ppIThumbnail: IWICBitmapSource): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapFrameDecode}

{ interface IWICProgressiveLevelControl }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICProgressiveLevelControl);'}
  IWICProgressiveLevelControl = interface(IUnknown)
    [SID_IWICProgressiveLevelControl]
    function GetLevelCount(var pcLevels: UINT): HRESULT; stdcall;

    function GetCurrentLevel(var pnLevel: UINT): HRESULT; stdcall;

    function SetCurrentLevel(nLevel: UINT): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICProgressiveLevelControl}

{ interface IWICProgressCallback }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICProgressCallback);'}
  IWICProgressCallback = interface(IUnknown)
    [SID_IWICProgressCallback]
    function Notify(uFrameNum: Cardinal; operation: WICProgressOperation;
      dblProgress: Double): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICProgressCallback}

{ interface IWICBitmapCodecProgressNotification }
  FNProgressNotification = function(pvData: Pointer; uFrameNum: Cardinal;
    operation: WICProgressOperation; dblProgress: Double): HRESULT; stdcall;
  PFNProgressNotification = ^FNProgressNotification;
{$EXTERNALSYM PFNProgressNotification}

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapCodecProgressNotification);'}
  IWICBitmapCodecProgressNotification = interface(IUnknown)
    [SID_IWICBitmapCodecProgressNotification]
    function RegisterProgressNotification(
      pfnProgressNotification: PFNProgressNotification; pvData: Pointer;
      dwProgressFlags: DWORD): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapCodecProgressNotification}

{ interface IWICComponentInfo }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICComponentInfo);'}
  IWICComponentInfo = interface(IUnknown)
    [SID_IWICComponentInfo]
    function GetComponentType(var pType: WICComponentType): HRESULT; stdcall;

    function GetCLSID(var pclsid: TGUID): HRESULT; stdcall;

    function GetSigningStatus(var pStatus: DWORD): HRESULT; stdcall;

    function GetAuthor(cchAuthor: UINT;
      wzAuthor: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetVendorGUID(var pguidVendor: TGUID): HRESULT; stdcall;

    function GetVersion(cchVersion: UINT;
      wzVersion: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetSpecVersion(cchSpecVersion: UINT;
      wzSpecVersion: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetFriendlyName(cchFriendlyName: UINT;
      wzFriendlyName: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICComponentInfo}

{ interface IWICFormatConverterInfo }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICFormatConverterInfo);'}
  IWICFormatConverterInfo = interface(IWICComponentInfo)
    [SID_IWICFormatConverterInfo]
    function GetPixelFormats(cFormats: UINT;
      pPixleFormatGUIDs: PWICPixelFormatGUID;
      var pcActual: UINT): HRESULT; stdcall;

    function CreateInstance(
      out ppIConverter: IWICFormatConverter): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICFormatConverterInfo}

{ interface IWICBitmapCodecInfo }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapCodecInfo);'}
  IWICBitmapCodecInfo = interface(IWICComponentInfo)
    [SID_IWICBitmapCodecInfo]
    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;

    function GetPixelFormats(cFormats: UINT;
      var guidPixelFormats: PGUID;
      var pcActual: UINT): HRESULT; stdcall;

    function GetColorManagementVersion(cchColorManagementVersion: UINT;
      wzColorManagementVersion: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetDeviceManufacturer(cchDeviceManufacturer: UINT;
      wzDeviceManufacturer: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetDeviceModels(cchDeviceModels: UINT;
      wzDeviceModels: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetMimeTypes(cchMimeTypes: UINT;
      wzMimeTypes: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function GetFileExtensions(cchFileExtensions: UINT;
      wzFileExtensions: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;

    function DoesSupportAnimation(var pfSupportAnimation: BOOL): HRESULT; stdcall;

    function DoesSupportChromakey(var pfSupportChromakey: BOOL): HRESULT; stdcall;

    function DoesSupportLossless(var pfSupportLossless: BOOL): HRESULT; stdcall;

    function DoesSupportMultiframe(
      var pfSupportMultiframe: BOOL): HRESULT; stdcall;

    function MatchesMimeType(wzMimeType: LPCWSTR;
      var pfMatches: BOOL): HRESULT; stdcall;

  end;
  {$EXTERNALSYM IWICBitmapCodecInfo}

{ interface IWICBitmapEncoderInfo }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapEncoderInfo);'}
  IWICBitmapEncoderInfo = interface(IWICBitmapCodecInfo)
    [SID_IWICBitmapEncoderInfo]
    function CreateInstance(
      out ppIBitmapEncoder: IWICBitmapEncoder): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapEncoderInfo}

{ interface IWICBitmapDecoderInfo }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICBitmapDecoderInfo);'}
  IWICBitmapDecoderInfo = interface(IWICBitmapCodecInfo)
    [SID_IWICBitmapDecoderInfo]
    function GetPatterns(cbSizePatterns: UINT; pPatterns: PWICBitmapPattern;
      var pcPatterns: UINT; var pcbPatternsActual: UINT): HRESULT; stdcall;

    function MatchesPattern(pIStream: IStream;
      var pfMatches: BOOL): HRESULT; stdcall;

    function CreateInstance(
      out ppIBitmapDecoder: IWICBitmapDecoder): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICBitmapDecoderInfo}

{ interface IWICPixelFormatInfo }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPixelFormatInfo);'}
  IWICPixelFormatInfo = interface(IWICComponentInfo)
    [SID_IWICPixelFormatInfo]
    function GetFormatGUID(var pFormat: TGUID): HRESULT; stdcall;

    function GetColorContext(
      out ppIColorContext: IWICColorContext): HRESULT; stdcall;

    function GetBitsPerPixel(var puiBitsPerPixel: UINT): HRESULT; stdcall;

    function GetChannelCount(var puiChannelCount: UINT): HRESULT; stdcall;

    function GetChannelMask(uiChannelIndex: UINT; cbMaskBuffer: UINT;
      pbMaskBuffer: PBYTE; var pcbActual: UINT): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICPixelFormatInfo}

{ interface IWICPixelFormatInfo2 }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICPixelFormatInfo2);'}
  IWICPixelFormatInfo2 = interface(IWICPixelFormatInfo)
    [SID_IWICPixelFormatInfo2]
    function SupportsTransparency(
      var pfSupportsTransparency: BOOL): HRESULT; stdcall;

    function GetNumericRepresentation(
      var pNumericRepresentation: WICPixelFormatNumericRepresentation): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICPixelFormatInfo2}

{ interface IWICImagingFactory }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICImagingFactory);'}
  IWICImagingFactory = interface(IUnknown)
    [SID_IWICImagingFactory]
    function CreateDecoderFromFilename(wzFilename: LPCWSTR; pguidVendor: PGUID;
      dwDesiredAccess: DWORD; metadataOptions: WICDecodeOptions;
      out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;

    function CreateDecoderFromStream(pIStream: IStream; pguidVendor: PGUID;
      metadataOptions: WICDecodeOptions;
      out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;

    function CreateDecoderFromFileHandle(hFile: LongWord; const pguidVendor: TGUID;
      metadataOptions: WICDecodeOptions;
      out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;

    function CreateComponentInfo(const clsidComponent: TGUID;
      out ppIInfo: IWICComponentInfo): HRESULT; stdcall;

    function CreateDecoder(const guidContainerFormat: TGuid; const pguidVendor: TGUID;
      out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;

    function CreateEncoder(const guidContainerFormat: TGuid; const pguidVendor: PGUID;
      out ppIEncoder: IWICBitmapEncoder): HRESULT; stdcall;

    function CreatePalette(out ppIPalette: IWICPalette): HRESULT; stdcall;

    function CreateFormatConverter(
      out ppIFormatConverter: IWICFormatConverter): HRESULT; stdcall;

    function CreateBitmapScaler(
      out ppIBitmapScaler: IWICBitmapScaler): HRESULT; stdcall;

    function CreateBitmapClipper(
      out ppIBitmapClipper: IWICBitmapClipper): HRESULT; stdcall;

    function CreateBitmapFlipRotator(
      out ppIBitmapFlipRotator: IWICBitmapFlipRotator): HRESULT; stdcall;

    function CreateStream(out ppIWICStream: IWICStream): HRESULT; stdcall;

    function CreateColorContext(
      out ppIWICColorContext: IWICColorContext): HRESULT; stdcall;

    function CreateColorTransformer(
      out ppIWICColorTransform: IWICColorTransform): HRESULT; stdcall;

    function CreateBitmap(uiWidth: UINT; uiHeight: UINT;
      pixelFormat: REFWICPixelFormatGUID; option: WICBitmapCreateCacheOption;
      out ppIBitmap: IWICBitmap): HRESULT; stdcall;

    function CreateBitmapFromSource(pIBitmapSource: IWICBitmapSource;
      option: WICBitmapCreateCacheOption;
      out ppIBitmap: IWICBitmap): HRESULT; stdcall;

    function CreateBitmapFromSourceRect(pIBitmapSource: IWICBitmapSource;
      x: UINT; y: UINT; width: UINT; height: UINT;
      out ppIBitmap: IWICBitmap): HRESULT; stdcall;

    function CreateBitmapFromMemory(uiWidth: UINT; uiHeight: UINT;
      const pixelFormat: WICPixelFormatGUID; cbStride: UINT; cbBufferSize: UINT;
      pbBuffer: PByte; out ppIBitmap: IWICBitmap): HRESULT; stdcall;

    function CreateBitmapFromHBITMAP(hBitmap: HBITMAP; hPalette: HPALETTE;
      options: WICBitmapAlphaChannelOption;
      out ppIBitmap: IWICBitmap): HRESULT; stdcall;

    function CreateBitmapFromHICON(hIcon: HICON;
      out ppIBitmap: IWICBitmap): HRESULT; stdcall;

    function CreateComponentEnumerator(componentTypes: DWORD; options: DWORD;
      out ppIEnumUnknown: IEnumUnknown): HRESULT; stdcall;

    function CreateFastMetadataEncoderFromDecoder(pIDecoder: IWICBitmapDecoder;
      out ppIFastEncoder: IWICFastMetadataEncoder): HRESULT; stdcall;

    function CreateFastMetadataEncoderFromFrameDecode(
      pIFrameDecoder: IWICBitmapFrameDecode;
      out ppIFastEncoder: IWICFastMetadataEncoder): HRESULT; stdcall;

    function CreateQueryWriter(const guidMetadataFormat: TGuid;
      const pguidVendor: TGUID;
      out ppIQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;

    function CreateQueryWriterFromReader(pIQueryReader: IWICMetadataQueryReader;
      const pguidVendor: TGUID;
      out ppIQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICImagingFactory}

const
  FACILITY_WINCODEC_ERR = $898;
  {$EXTERNALSYM FACILITY_WINCODEC_ERR}
  WINCODEC_ERR_BASE = $2000;
  {$EXTERNALSYM WINCODEC_ERR_BASE}

  WINCODEC_ERR_GENERIC_ERROR                    = E_FAIL;
  {$EXTERNALSYM WINCODEC_ERR_GENERIC_ERROR}
  WINCODEC_ERR_INVALIDPARAMETER                 = E_INVALIDARG;
  {$EXTERNALSYM WINCODEC_ERR_INVALIDPARAMETER}
  WINCODEC_ERR_OUTOFMEMORY                      = E_OUTOFMEMORY;
  {$EXTERNALSYM WINCODEC_ERR_OUTOFMEMORY}
  WINCODEC_ERR_NOTIMPLEMENTED                   = E_NOTIMPL;
  {$EXTERNALSYM WINCODEC_ERR_NOTIMPLEMENTED}
  WINCODEC_ERR_ABORTED                          = E_ABORT;
  {$EXTERNALSYM WINCODEC_ERR_ABORTED}
  WINCODEC_ERR_ACCESSDENIED                     = E_ACCESSDENIED;
  {$EXTERNALSYM WINCODEC_ERR_ACCESSDENIED}
  WINCODEC_ERR_VALUEOVERFLOW                    = $80070216; //INTSAFE_E_ARITHMETIC_OVERFLOW;
  {$EXTERNALSYM WINCODEC_ERR_VALUEOVERFLOW}
  WINCODEC_ERR_WRONGSTATE                       = $88982f04;
  {$EXTERNALSYM WINCODEC_ERR_WRONGSTATE}
  WINCODEC_ERR_VALUEOUTOFRANGE                  = $88982f05;
  {$EXTERNALSYM WINCODEC_ERR_VALUEOUTOFRANGE}
  WINCODEC_ERR_UNKNOWNIMAGEFORMAT               = $88982f07;
  {$EXTERNALSYM WINCODEC_ERR_UNKNOWNIMAGEFORMAT}
  WINCODEC_ERR_UNSUPPORTEDVERSION               = $88982f0B;
  {$EXTERNALSYM WINCODEC_ERR_UNSUPPORTEDVERSION}
  WINCODEC_ERR_NOTINITIALIZED                   = $88982f0C;
  {$EXTERNALSYM WINCODEC_ERR_NOTINITIALIZED}
  WINCODEC_ERR_ALREADYLOCKED                    = $88982f0D;
  {$EXTERNALSYM WINCODEC_ERR_ALREADYLOCKED}
  WINCODEC_ERR_PROPERTYNOTFOUND                 = $88982f40;
  {$EXTERNALSYM WINCODEC_ERR_PROPERTYNOTFOUND}
  WINCODEC_ERR_PROPERTYNOTSUPPORTED             = $88982f41;
  {$EXTERNALSYM WINCODEC_ERR_PROPERTYNOTSUPPORTED}
  WINCODEC_ERR_PROPERTYSIZE                     = $88982f42;
  {$EXTERNALSYM WINCODEC_ERR_PROPERTYSIZE}
  WINCODEC_ERR_CODECPRESENT                     = $88982f43;
  {$EXTERNALSYM WINCODEC_ERR_CODECPRESENT}
  WINCODEC_ERR_CODECNOTHUMBNAIL                 = $88982f44;
  {$EXTERNALSYM WINCODEC_ERR_CODECNOTHUMBNAIL}
  WINCODEC_ERR_PALETTEUNAVAILABLE               = $88982f45;
  {$EXTERNALSYM WINCODEC_ERR_PALETTEUNAVAILABLE}
  WINCODEC_ERR_CODECTOOMANYSCANLINES            = $88982f46;
  {$EXTERNALSYM WINCODEC_ERR_CODECTOOMANYSCANLINES}
  WINCODEC_ERR_INTERNALERROR                    = $88982f48;
  {$EXTERNALSYM WINCODEC_ERR_INTERNALERROR}
  WINCODEC_ERR_SOURCERECTDOESNOTMATCHDIMENSIONS = $88982f49;
  {$EXTERNALSYM WINCODEC_ERR_SOURCERECTDOESNOTMATCHDIMENSIONS}
  WINCODEC_ERR_COMPONENTNOTFOUND                = $88982f50;
  {$EXTERNALSYM WINCODEC_ERR_COMPONENTNOTFOUND}
  WINCODEC_ERR_IMAGESIZEOUTOFRANGE              = $88982f51;
  {$EXTERNALSYM WINCODEC_ERR_IMAGESIZEOUTOFRANGE}
  WINCODEC_ERR_TOOMUCHMETADATA                  = $88982f52;
  {$EXTERNALSYM WINCODEC_ERR_TOOMUCHMETADATA}
  WINCODEC_ERR_BADIMAGE                         = $88982f60;
  {$EXTERNALSYM WINCODEC_ERR_BADIMAGE}
  WINCODEC_ERR_BADHEADER                        = $88982f61;
  {$EXTERNALSYM WINCODEC_ERR_BADHEADER}
  WINCODEC_ERR_FRAMEMISSING                     = $88982f62;
  {$EXTERNALSYM WINCODEC_ERR_FRAMEMISSING}
  WINCODEC_ERR_BADMETADATAHEADER                = $88982f63;
  {$EXTERNALSYM WINCODEC_ERR_BADMETADATAHEADER}
  WINCODEC_ERR_BADSTREAMDATA                    = $88982f70;
  {$EXTERNALSYM WINCODEC_ERR_BADSTREAMDATA}
  WINCODEC_ERR_STREAMWRITE                      = $88982f71;
  {$EXTERNALSYM WINCODEC_ERR_STREAMWRITE}
  WINCODEC_ERR_STREAMREAD                       = $88982f72;
  {$EXTERNALSYM WINCODEC_ERR_STREAMREAD}
  WINCODEC_ERR_STREAMNOTAVAILABLE               = $88982f73;
  {$EXTERNALSYM WINCODEC_ERR_STREAMNOTAVAILABLE}
  WINCODEC_ERR_UNSUPPORTEDPIXELFORMAT           = $88982f80;
  {$EXTERNALSYM WINCODEC_ERR_UNSUPPORTEDPIXELFORMAT}
  WINCODEC_ERR_UNSUPPORTEDOPERATION             = $88982f81;
  {$EXTERNALSYM WINCODEC_ERR_UNSUPPORTEDOPERATION}
  WINCODEC_ERR_INVALIDREGISTRATION              = $88982f8A;
  {$EXTERNALSYM WINCODEC_ERR_INVALIDREGISTRATION}
  WINCODEC_ERR_COMPONENTINITIALIZEFAILURE       = $88982f8B;
  {$EXTERNALSYM WINCODEC_ERR_COMPONENTINITIALIZEFAILURE}
  WINCODEC_ERR_INSUFFICIENTBUFFER               = $88982f8C;
  {$EXTERNALSYM WINCODEC_ERR_INSUFFICIENTBUFFER}
  WINCODEC_ERR_DUPLICATEMETADATAPRESENT         = $88982f8D;
  {$EXTERNALSYM WINCODEC_ERR_DUPLICATEMETADATAPRESENT}
  WINCODEC_ERR_PROPERTYUNEXPECTEDTYPE           = $88982f8E;
  {$EXTERNALSYM WINCODEC_ERR_PROPERTYUNEXPECTEDTYPE}
  WINCODEC_ERR_UNEXPECTEDSIZE                   = $88982f8F;
  {$EXTERNALSYM WINCODEC_ERR_UNEXPECTEDSIZE}
  WINCODEC_ERR_INVALIDQUERYREQUEST              = $88982f90;
  {$EXTERNALSYM WINCODEC_ERR_INVALIDQUERYREQUEST}
  WINCODEC_ERR_UNEXPECTEDMETADATATYPE           = $88982f91;
  {$EXTERNALSYM WINCODEC_ERR_UNEXPECTEDMETADATATYPE}
  WINCODEC_ERR_REQUESTONLYVALIDATMETADATAROOT   = $88982f92;
  {$EXTERNALSYM WINCODEC_ERR_REQUESTONLYVALIDATMETADATAROOT}
  WINCODEC_ERR_INVALIDQUERYCHARACTER            = $88982f93;
  {$EXTERNALSYM WINCODEC_ERR_INVALIDQUERYCHARACTER}
  WINCODEC_ERR_WIN32ERROR                       = $88982f94;
  {$EXTERNALSYM WINCODEC_ERR_WIN32ERROR}
  WINCODEC_ERR_INVALIDPROGRESSIVELEVEL          = $88982f95;
  {$EXTERNALSYM WINCODEC_ERR_INVALIDPROGRESSIVELEVEL}

type
  WICTiffCompressionOption = type Integer; 
  {$EXTERNALSYM WICTiffCompressionOption}
const
  WICTiffCompressionDontCare           = 0; 
  {$EXTERNALSYM WICTiffCompressionDontCare}
  WICTiffCompressionNone               = $1; 
  {$EXTERNALSYM WICTiffCompressionNone}
  WICTiffCompressionCCITT3             = $2; 
  {$EXTERNALSYM WICTiffCompressionCCITT3}
  WICTiffCompressionCCITT4             = $3; 
  {$EXTERNALSYM WICTiffCompressionCCITT4}
  WICTiffCompressionLZW                = $4; 
  {$EXTERNALSYM WICTiffCompressionLZW}
  WICTiffCompressionRLE                = $5; 
  {$EXTERNALSYM WICTiffCompressionRLE}
  WICTiffCompressionZIP                = $6; 
  {$EXTERNALSYM WICTiffCompressionZIP}
  WICTiffCompressionLZWHDifferencing   = $7; 
  {$EXTERNALSYM WICTiffCompressionLZWHDifferencing}
  WICTIFFCOMPRESSIONOPTION_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICTIFFCOMPRESSIONOPTION_FORCE_DWORD}

type
  WICJpegYCrCbSubsamplingOption = type Integer; 
  {$EXTERNALSYM WICJpegYCrCbSubsamplingOption}
const
  WICJpegYCrCbSubsamplingDefault      = 0;
  {$EXTERNALSYM WICJpegYCrCbSubsamplingDefault}
  WICJpegYCrCbSubsampling420          = $1; 
  {$EXTERNALSYM WICJpegYCrCbSubsampling420}
  WICJpegYCrCbSubsampling422          = $2; 
  {$EXTERNALSYM WICJpegYCrCbSubsampling422}
  WICJpegYCrCbSubsampling444          = $3; 
  {$EXTERNALSYM WICJpegYCrCbSubsampling444}
  WICJPEGYCRCBSUBSAMPLING_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICJPEGYCRCBSUBSAMPLING_FORCE_DWORD}

type
  WICPngFilterOption = type Integer; 
  {$EXTERNALSYM WICPngFilterOption}
const
  WICPngFilterUnspecified        = 0; 
  {$EXTERNALSYM WICPngFilterUnspecified}
  WICPngFilterNone               = $1; 
  {$EXTERNALSYM WICPngFilterNone}
  WICPngFilterSub                = $2; 
  {$EXTERNALSYM WICPngFilterSub}
  WICPngFilterUp                 = $3; 
  {$EXTERNALSYM WICPngFilterUp}
  WICPngFilterAverage            = $4; 
  {$EXTERNALSYM WICPngFilterAverage}
  WICPngFilterPaeth              = $5; 
  {$EXTERNALSYM WICPngFilterPaeth}
  WICPngFilterAdaptive           = $6; 
  {$EXTERNALSYM WICPngFilterAdaptive}
  WICPNGFILTEROPTION_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICPNGFILTEROPTION_FORCE_DWORD}

type
  WICNamedWhitePoint = type Integer;
  {$EXTERNALSYM WICNamedWhitePoint}
const
  WICWhitePointDefault           = $1; 
  {$EXTERNALSYM WICWhitePointDefault}
  WICWhitePointDaylight          = $2; 
  {$EXTERNALSYM WICWhitePointDaylight}
  WICWhitePointCloudy            = $4;
  {$EXTERNALSYM WICWhitePointCloudy}
  WICWhitePointShade             = $8; 
  {$EXTERNALSYM WICWhitePointShade}
  WICWhitePointTungsten          = $10; 
  {$EXTERNALSYM WICWhitePointTungsten}
  WICWhitePointFluorescent       = $20; 
  {$EXTERNALSYM WICWhitePointFluorescent}
  WICWhitePointFlash             = $40; 
  {$EXTERNALSYM WICWhitePointFlash}
  WICWhitePointUnderwater        = $80; 
  {$EXTERNALSYM WICWhitePointUnderwater}
  WICWhitePointCustom            = $100; 
  {$EXTERNALSYM WICWhitePointCustom}
  WICWhitePointAutoWhiteBalance  = $200; 
  {$EXTERNALSYM WICWhitePointAutoWhiteBalance}
  WICWhitePointAsShot            = WICWHITEPOINTDEFAULT; 
  {$EXTERNALSYM WICWhitePointAsShot}
  WICNAMEDWHITEPOINT_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICNAMEDWHITEPOINT_FORCE_DWORD}

type
  WICRawCapabilities = type Integer; 
  {$EXTERNALSYM WICRawCapabilities}
const
  WICRawCapabilityNotSupported   = 0; 
  {$EXTERNALSYM WICRawCapabilityNotSupported}
  WICRawCapabilityGetSupported   = $1; 
  {$EXTERNALSYM WICRawCapabilityGetSupported}
  WICRawCapabilityFullySupported = $2; 
  {$EXTERNALSYM WICRawCapabilityFullySupported}
  WICRAWCAPABILITIES_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICRAWCAPABILITIES_FORCE_DWORD}

type
  WICRawRotationCapabilities = type Integer; 
  {$EXTERNALSYM WICRawRotationCapabilities}
const
  WICRawRotationCapabilityNotSupported           = 0; 
  {$EXTERNALSYM WICRawRotationCapabilityNotSupported}
  WICRawRotationCapabilityGetSupported           = $1; 
  {$EXTERNALSYM WICRawRotationCapabilityGetSupported}
  WICRawRotationCapabilityNinetyDegreesSupported = $2;
  {$EXTERNALSYM WICRawRotationCapabilityNinetyDegreesSupported}
  WICRawRotationCapabilityFullySupported         = $3; 
  {$EXTERNALSYM WICRawRotationCapabilityFullySupported}
  WICRAWROTATIONCAPABILITIES_FORCE_DWORD         = $7FFFFFFF; 
  {$EXTERNALSYM WICRAWROTATIONCAPABILITIES_FORCE_DWORD}

type
  WICRawCapabilitiesInfo = record
    cbSize: UINT;
    CodecMajorVersion: UINT;
    CodecMinorVersion: UINT;
    ExposureCompensationSupport: WICRawCapabilities;
    ContrastSupport: WICRawCapabilities;
    RGBWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupport: WICRawCapabilities;
    NamedWhitePointSupportMask: UINT;
    KelvinWhitePointSupport: WICRawCapabilities;
    GammaSupport: WICRawCapabilities;
    TintSupport: WICRawCapabilities;
    SaturationSupport: WICRawCapabilities;
    SharpnessSupport: WICRawCapabilities;
    NoiseReductionSupport: WICRawCapabilities;
    DestinationColorProfileSupport: WICRawCapabilities;
    ToneCurveSupport: WICRawCapabilities;
    RotationSupport: WICRawRotationCapabilities;
    RenderModeSupport: WICRawCapabilities;
  end;
  {$EXTERNALSYM WICRawCapabilitiesInfo}

type
  WICRawParameterSet = type Integer; 
  {$EXTERNALSYM WICRawParameterSet}
const
  WICAsShotParameterSet          = $1; 
  {$EXTERNALSYM WICAsShotParameterSet}
  WICUserAdjustedParameterSet    = $2; 
  {$EXTERNALSYM WICUserAdjustedParameterSet}
  WICAutoAdjustedParameterSet    = $3; 
  {$EXTERNALSYM WICAutoAdjustedParameterSet}
  WICRAWPARAMETERSET_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICRAWPARAMETERSET_FORCE_DWORD}

type
  WICRawRenderMode = type Integer; 
  {$EXTERNALSYM WICRawRenderMode}
const
  WICRawRenderModeDraft        = $1; 
  {$EXTERNALSYM WICRawRenderModeDraft}
  WICRawRenderModeNormal       = $2; 
  {$EXTERNALSYM WICRawRenderModeNormal}
  WICRawRenderModeBestQuality  = $3; 
  {$EXTERNALSYM WICRawRenderModeBestQuality}
  WICRAWRENDERMODE_FORCE_DWORD = $7FFFFFFF; 
  {$EXTERNALSYM WICRAWRENDERMODE_FORCE_DWORD}

type
  WICRawToneCurvePoint = record
    Input: Double;
    Output: Double;
  end;
  {$EXTERNALSYM WICRawToneCurvePoint}

  WICRawToneCurve = record
    cPoints: UINT;
    apoints: array[0..0] of WICRawToneCurvePoint;
  end;
  {$EXTERNALSYM WICRawToneCurve}
  TWICRawToneCurve = WICRawToneCurve;
  PWICRawToneCurve = ^TWICRawToneCurve;

const
  WICRawChangeNotification_ExposureCompensation    = $00000001;
  {$EXTERNALSYM WICRawChangeNotification_ExposureCompensation}
  WICRawChangeNotification_NamedWhitePoint         = $00000002;
  {$EXTERNALSYM WICRawChangeNotification_NamedWhitePoint}
  WICRawChangeNotification_KelvinWhitePoint        = $00000004;
  {$EXTERNALSYM WICRawChangeNotification_KelvinWhitePoint}
  WICRawChangeNotification_RGBWhitePoint           = $00000008;
  {$EXTERNALSYM WICRawChangeNotification_RGBWhitePoint}
  WICRawChangeNotification_Contrast                = $00000010;
  {$EXTERNALSYM WICRawChangeNotification_Contrast}
  WICRawChangeNotification_Gamma                   = $00000020;
  {$EXTERNALSYM WICRawChangeNotification_Gamma}
  WICRawChangeNotification_Sharpness               = $00000040;
  {$EXTERNALSYM WICRawChangeNotification_Sharpness}
  WICRawChangeNotification_Saturation              = $00000080;
  {$EXTERNALSYM WICRawChangeNotification_Saturation}
  WICRawChangeNotification_Tint                    = $00000100;
  {$EXTERNALSYM WICRawChangeNotification_Tint}
  WICRawChangeNotification_NoiseReduction          = $00000200;
  {$EXTERNALSYM WICRawChangeNotification_NoiseReduction}
  WICRawChangeNotification_DestinationColorContext = $00000400;
  {$EXTERNALSYM WICRawChangeNotification_DestinationColorContext}
  WICRawChangeNotification_ToneCurve               = $00000800;
  {$EXTERNALSYM WICRawChangeNotification_ToneCurve}
  WICRawChangeNotification_Rotation                = $00001000;
  {$EXTERNALSYM WICRawChangeNotification_Rotation}
  WICRawChangeNotification_RenderMode              = $00002000;
  {$EXTERNALSYM WICRawChangeNotification_RenderMode}

{ interface IWICDevelopRawNotificationCallback }
type
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDevelopRawNotificationCallback);'}
  IWICDevelopRawNotificationCallback = interface(IUnknown)
    [SID_IWICDevelopRawNotificationCallback]
    function Notify(NotificationMask: UINT): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICDevelopRawNotificationCallback}

{ interface IWICDevelopRaw }
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IWICDevelopRaw);'}
  IWICDevelopRaw = interface(IWICBitmapFrameDecode)
    [SID_IWICDevelopRaw]
    function QueryRawCapabilitiesInfo(
      var pInfo: WICRawCapabilitiesInfo): HRESULT; stdcall;

    function LoadParameterSet(ParameterSet: WICRawParameterSet): HRESULT; stdcall;

    function GetCurrentParameterSet(
      out ppCurrentParameterSet: IUnknown): HRESULT; stdcall;

    function SetExposureCompensation(ev: Double): HRESULT; stdcall;

    function GetExposureCompensation(var pEV: Double): HRESULT; stdcall;

    function SetWhitePointRGB(Red: UINT; Green: UINT;
      Blue: UINT): HRESULT; stdcall;

    function GetWhitePointRGB(var pRed: UINT; var pGreen: UINT;
      var pBlue: UINT): HRESULT; stdcall;

    function SetNamedWhitePoint(WhitePoint: WICNamedWhitePoint): HRESULT; stdcall;

    function GetNamedWhitePoint(
      var pWhitePoint: WICNamedWhitePoint): HRESULT; stdcall;

    function SetWhitePointKelvin(WhitePointKelvin: UINT): HRESULT; stdcall;

    function GetWhitePointKelvin(var pWhitePointKelvin: UINT): HRESULT; stdcall;

    function GetKelvinRangeInfo(var pMinKelvinTemp: UINT; var pMaxKelvinTemp: UINT;
      var pKelvinTempStepValue: UINT): HRESULT; stdcall;

    function SetContrast(Contrast: Double): HRESULT; stdcall;

    function GetContrast(var pContrast: Double): HRESULT; stdcall;

    function SetGamma(Gamma: Double): HRESULT; stdcall;

    function GetGamma(var pGamma: Double): HRESULT; stdcall;

    function SetSharpness(Sharpness: Double): HRESULT; stdcall;

    function GetSharpness(var pSharpness: Double): HRESULT; stdcall;

    function SetSaturation(Saturation: Double): HRESULT; stdcall;

    function GetSaturation(var pSaturation: Double): HRESULT; stdcall;

    function SetTint(Tint: Double): HRESULT; stdcall;

    function GetTint(var pTint: Double): HRESULT; stdcall;

    function SetNoiseReduction(NoiseReduction: Double): HRESULT; stdcall;

    function GetNoiseReduction(var pNoiseReduction: Double): HRESULT; stdcall;

    function SetDestinationColorContext(
      pColorContext: IWICColorContext): HRESULT; stdcall;

    function SetToneCurve(cbToneCurveSize: UINT;
      pToneCurve: PWICRawToneCurve): HRESULT; stdcall;

    function GetToneCurve(cbToneCurveBufferSize: UINT;
      pToneCurve: PWICRawToneCurve;
      var pcbActualToneCurveBufferSize: UINT): HRESULT; stdcall;

    function SetRotation(Rotation: Double): HRESULT; stdcall;

    function GetRotation(var pRotation: Double): HRESULT; stdcall;

    function SetRenderMode(RenderMode: WICRawRenderMode): HRESULT; stdcall;

    function GetRenderMode(var pRenderMode: WICRawRenderMode): HRESULT; stdcall;

    function SetNotificationCallback(
      pCallback: IWICDevelopRawNotificationCallback): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IWICDevelopRaw}


{$Z4}

type
  LargeInt=int64;
  PLongBool=^LongBool;
  PHRESULT=^HRESULT;
  SIZE_T=LongWord;
  PSIZE_T=^SIZE_T;
  HANDLE=THandle;
  PTRect=PRect;
  PTPoint=PPoint;
  TdxColorArray=array[0..3] of Single;
  TQuadArray_UInt=array[0..3] of LongWord;
  TQuadArray_Float=array[0..3] of Single;

  D3DCOLOR=LongWord;
  TD3DColor=D3DCOLOR;
  PD3DColor=^TD3DColor;
  PTD3DColor=^TD3DColor;

  D3DCOLORVALUE=packed record
    R:Single;
    G:Single;
    B:Single;
    A:Single;
  end;
  TD3DColorValue=D3DCOLORVALUE;
  PD3DColorValue=^TD3DColorValue;
  PTD3DColorValue=^TD3DColorValue;

function ColorArray(R,G,B,A:Single):TdxColorArray;
begin
  Result[0]:=R;
  Result[1]:=G;
  Result[2]:=B;
  Result[3]:=A;
end;


///////////////////////////////////////////////////////////////////////////////
// Begin "DXGIFormat.h"
///////////////////////////////////////////////////////////////////////////////

const
  DXGI_FORMAT_DEFINED=1;

type
  TDXGI_Format=
  (
    DXGI_FORMAT_UNKNOWN=0,
    DXGI_FORMAT_R32G32B32A32_TYPELESS=1,
    DXGI_FORMAT_R32G32B32A32_FLOAT=2,
    DXGI_FORMAT_R32G32B32A32_UINT=3,
    DXGI_FORMAT_R32G32B32A32_SINT=4,
    DXGI_FORMAT_R32G32B32_TYPELESS=5,
    DXGI_FORMAT_R32G32B32_FLOAT=6,
    DXGI_FORMAT_R32G32B32_UINT=7,
    DXGI_FORMAT_R32G32B32_SINT=8,
    DXGI_FORMAT_R16G16B16A16_TYPELESS=9,
    DXGI_FORMAT_R16G16B16A16_FLOAT=10,
    DXGI_FORMAT_R16G16B16A16_UNORM=11,
    DXGI_FORMAT_R16G16B16A16_UINT=12,
    DXGI_FORMAT_R16G16B16A16_SNORM=13,
    DXGI_FORMAT_R16G16B16A16_SINT=14,
    DXGI_FORMAT_R32G32_TYPELESS=15,
    DXGI_FORMAT_R32G32_FLOAT=16,
    DXGI_FORMAT_R32G32_UINT=17,
    DXGI_FORMAT_R32G32_SINT=18,
    DXGI_FORMAT_R32G8X24_TYPELESS=19,
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT=20,
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS=21,
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT=22,
    DXGI_FORMAT_R10G10B10A2_TYPELESS=23,
    DXGI_FORMAT_R10G10B10A2_UNORM=24,
    DXGI_FORMAT_R10G10B10A2_UINT=25,
    DXGI_FORMAT_R11G11B10_FLOAT=26,
    DXGI_FORMAT_R8G8B8A8_TYPELESS=27,
    DXGI_FORMAT_R8G8B8A8_UNORM=28,
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB=29,
    DXGI_FORMAT_R8G8B8A8_UINT=30,
    DXGI_FORMAT_R8G8B8A8_SNORM=31,
    DXGI_FORMAT_R8G8B8A8_SINT=32,
    DXGI_FORMAT_R16G16_TYPELESS=33,
    DXGI_FORMAT_R16G16_FLOAT=34,
    DXGI_FORMAT_R16G16_UNORM=35,
    DXGI_FORMAT_R16G16_UINT=36,
    DXGI_FORMAT_R16G16_SNORM=37,
    DXGI_FORMAT_R16G16_SINT=38,
    DXGI_FORMAT_R32_TYPELESS=39,
    DXGI_FORMAT_D32_FLOAT=40,
    DXGI_FORMAT_R32_FLOAT=41,
    DXGI_FORMAT_R32_UINT=42,
    DXGI_FORMAT_R32_SINT=43,
    DXGI_FORMAT_R24G8_TYPELESS=44,
    DXGI_FORMAT_D24_UNORM_S8_UINT=45,
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS=46,
    DXGI_FORMAT_X24_TYPELESS_G8_UINT=47,
    DXGI_FORMAT_R8G8_TYPELESS=48,
    DXGI_FORMAT_R8G8_UNORM=49,
    DXGI_FORMAT_R8G8_UINT=50,
    DXGI_FORMAT_R8G8_SNORM=51,
    DXGI_FORMAT_R8G8_SINT=52,
    DXGI_FORMAT_R16_TYPELESS=53,
    DXGI_FORMAT_R16_FLOAT=54,
    DXGI_FORMAT_D16_UNORM=55,
    DXGI_FORMAT_R16_UNORM=56,
    DXGI_FORMAT_R16_UINT=57,
    DXGI_FORMAT_R16_SNORM=58,
    DXGI_FORMAT_R16_SINT=59,
    DXGI_FORMAT_R8_TYPELESS=60,
    DXGI_FORMAT_R8_UNORM=61,
    DXGI_FORMAT_R8_UINT=62,
    DXGI_FORMAT_R8_SNORM=63,
    DXGI_FORMAT_R8_SINT=64,
    DXGI_FORMAT_A8_UNORM=65,
    DXGI_FORMAT_R1_UNORM=66,
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP=67,
    DXGI_FORMAT_R8G8_B8G8_UNORM=68,
    DXGI_FORMAT_G8R8_G8B8_UNORM=69,
    DXGI_FORMAT_BC1_TYPELESS=70,
    DXGI_FORMAT_BC1_UNORM=71,
    DXGI_FORMAT_BC1_UNORM_SRGB=72,
    DXGI_FORMAT_BC2_TYPELESS=73,
    DXGI_FORMAT_BC2_UNORM=74,
    DXGI_FORMAT_BC2_UNORM_SRGB=75,
    DXGI_FORMAT_BC3_TYPELESS=76,
    DXGI_FORMAT_BC3_UNORM=77,
    DXGI_FORMAT_BC3_UNORM_SRGB=78,
    DXGI_FORMAT_BC4_TYPELESS=79,
    DXGI_FORMAT_BC4_UNORM=80,
    DXGI_FORMAT_BC4_SNORM=81,
    DXGI_FORMAT_BC5_TYPELESS=82,
    DXGI_FORMAT_BC5_UNORM=83,
    DXGI_FORMAT_BC5_SNORM=84,
    DXGI_FORMAT_B5G6R5_UNORM=85,
    DXGI_FORMAT_B5G5R5A1_UNORM=86,
    DXGI_FORMAT_B8G8R8A8_UNORM=87,
    DXGI_FORMAT_B8G8R8X8_UNORM=88,
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM=89,
    DXGI_FORMAT_B8G8R8A8_TYPELESS=90,
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB=91,
    DXGI_FORMAT_B8G8R8X8_TYPELESS=92,
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB=93,
    DXGI_FORMAT_BC6H_TYPELESS=94,
    DXGI_FORMAT_BC6H_UF16=95,
    DXGI_FORMAT_BC6H_SF16=96,
    DXGI_FORMAT_BC7_TYPELESS=97,
    DXGI_FORMAT_BC7_UNORM=98,
    DXGI_FORMAT_BC7_UNORM_SRGB=99
  );
  PTDXGI_Format=^TDXGI_Format;
  DXGI_FORMAT=TDXGI_Format;
  PDXGI_FORMAT=^TDXGI_Format;

///////////////////////////////////////////////////////////////////////////////
// End "DXGIFormat.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "DXGIType.h"
///////////////////////////////////////////////////////////////////////////////

const
  _FACDXGI=$87a;
  DXGI_STATUS_Base=LongWord(_FACDXGI shl 16);
  DXGI_HRESULT_Base=DXGI_STATUS_Base or LongWord(1 shl 31);
  //
  DXGI_STATUS_OCCLUDED=DXGI_STATUS_Base or 1;
  DXGI_STATUS_CLIPPED=DXGI_STATUS_Base or 2;
  DXGI_STATUS_NO_REDIRECTION=DXGI_STATUS_Base or 4;
  DXGI_STATUS_NO_DESKTOP_ACCESS=DXGI_STATUS_Base or 5;
  DXGI_STATUS_GRAPHICS_VIDPN_SOURCE_IN_USE=DXGI_STATUS_Base or 6;
  DXGI_STATUS_MODE_CHANGED=DXGI_STATUS_Base or 7;
  DXGI_STATUS_MODE_CHANGE_IN_PROGRESS=DXGI_STATUS_Base or 8;
  //
  DXGI_ERROR_INVALID_CALL=HResult(DXGI_HRESULT_Base or 1);
  DXGI_ERROR_NOT_FOUND=HResult(DXGI_HRESULT_Base or 2);
  DXGI_ERROR_MORE_DATA=HResult(DXGI_HRESULT_Base or 3);
  DXGI_ERROR_UNSUPPORTED=HResult(DXGI_HRESULT_Base or 4);
  DXGI_ERROR_DEVICE_REMOVED=HResult(DXGI_HRESULT_Base or 5);
  DXGI_ERROR_DEVICE_HUNG=HResult(DXGI_HRESULT_Base or 6);
  DXGI_ERROR_DEVICE_RESET=HResult(DXGI_HRESULT_Base or 7);
  DXGI_ERROR_WAS_STILL_DRAWING=HResult(DXGI_HRESULT_Base or 10);
  DXGI_ERROR_FRAME_STATISTICS_DISJOINT=HResult(DXGI_HRESULT_Base or 11);
  DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE=HResult(DXGI_HRESULT_Base or 12);
  DXGI_ERROR_DRIVER_INTERNAL_ERROR=HResult(DXGI_HRESULT_Base or 32);
  DXGI_ERROR_NONEXCLUSIVE=HResult(DXGI_HRESULT_Base or 33);
  DXGI_ERROR_NOT_CURRENTLY_AVAILABLE=HResult(DXGI_HRESULT_Base or 34);
  DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED=HResult(DXGI_HRESULT_Base or 35);
  DXGI_ERROR_REMOTE_OUTOFMEMORY=HResult(DXGI_HRESULT_Base or 36);
  //
  DXGI_CPU_ACCESS_NONE=0;
  DXGI_CPU_ACCESS_DYNAMIC=1;
  DXGI_CPU_ACCESS_READ_WRITE=2;
  DXGI_CPU_ACCESS_SCRATCH=3;
  DXGI_CPU_ACCESS_FIELD=15;
  DXGI_USAGE_SHADER_INPUT=1 shl (0 + 4);
  DXGI_USAGE_RENDER_TARGET_OUTPUT=1 shl (1 + 4);
  DXGI_USAGE_BACK_BUFFER=1 shl (2 + 4);
  DXGI_USAGE_SHARED=1 shl (3 + 4);
  DXGI_USAGE_READ_ONLY=1 shl (4 + 4);
  DXGI_USAGE_DISCARD_ON_PRESENT=1 shl (5 + 4);
  DXGI_USAGE_UNORDERED_ACCESS=1 shl (6 + 4);

type
  TDXGI_RGB=record
    Red:Single;
    Green:Single;
    Blue:Single;
  end;
  PTDXGI_RGB=^TDXGI_RGB;
  DXGI_RGB=TDXGI_RGB;
  PDXGI_RGB=^TDXGI_RGB;

  TDXGI_GammaControl=record
    Scale:TDXGI_RGB;
    Offset:TDXGI_RGB;
    GammaCurve:array[0..1024] of TDXGI_RGB;
  end;
  PTDXGI_GammaControl=^TDXGI_GammaControl;
  DXGI_GAMMA_CONTROL=TDXGI_GammaControl;
  PDXGI_GAMMA_CONTROL=^TDXGI_GammaControl;

  TDXGI_GammaControlCapabilities=record
    ScaleAndOffsetSupported:LongBool;
    MaxConvertedValue:Single;
    MinConvertedValue:Single;
    NumGammaControlPoints:LongWord;
    ControlPointPositions:array[0..1024] of Single;
  end;
  PTDXGI_GammaControlCapabilities=^TDXGI_GammaControlCapabilities;
  DXGI_GAMMA_CONTROL_CAPABILITIES=TDXGI_GammaControlCapabilities;
  PDXGI_GAMMA_CONTROL_CAPABILITIES=^TDXGI_GammaControlCapabilities;

  TDXGI_Rational=record
    Numerator:LongWord;
    Denominator:LongWord;
  end;
  PTDXGI_Rational=^TDXGI_Rational;
  DXGI_RATIONAL=TDXGI_Rational;
  PDXGI_RATIONAL=^TDXGI_Rational;

  TDXGI_ModeScanlineOrder=
  (
    DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED=0,
    DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE=1,
    DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST=2,
    DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST=3
  );
  PTDXGI_ModeScanlineOrder=^TDXGI_ModeScanlineOrder;
  DXGI_MODE_SCANLINE_ORDER=TDXGI_ModeScanlineOrder;
  PDXGI_MODE_SCANLINE_ORDER=^TDXGI_ModeScanlineOrder;

  TDXGI_ModeScaling=
  (
    DXGI_MODE_SCALING_UNSPECIFIED=0,
    DXGI_MODE_SCALING_CENTERED=1,
    DXGI_MODE_SCALING_STRETCHED=2
  );
  PTDXGI_ModeScaling=^TDXGI_ModeScaling;
  DXGI_MODE_SCALING=TDXGI_ModeScaling;
  PDXGI_MODE_SCALING=^TDXGI_ModeScaling;

  TDXGI_ModeRotation=
  (
    DXGI_MODE_ROTATION_UNSPECIFIED=0,
    DXGI_MODE_ROTATION_IDENTITY=1,
    DXGI_MODE_ROTATION_ROTATE90=2,
    DXGI_MODE_ROTATION_ROTATE180=3,
    DXGI_MODE_ROTATION_ROTATE270=4
  );
  PTDXGI_ModeRotation=^TDXGI_ModeRotation;
  DXGI_MODE_ROTATION=TDXGI_ModeRotation;
  PDXGI_MODE_ROTATION=^TDXGI_ModeRotation;

  TDXGI_ModeDesc=record
    Width:LongWord;
    Height:LongWord;
    RefreshRate:TDXGI_Rational;
    Format:TDXGI_Format;
    ScanlineOrdering:TDXGI_ModeScanlineOrder;
    Scaling:TDXGI_ModeScaling;
  end;
  PTDXGI_ModeDesc=^TDXGI_ModeDesc;
  DXGI_MODE_DESC=TDXGI_ModeDesc;
  PDXGI_MODE_DESC=^TDXGI_ModeDesc;

  TDXGI_SampleDesc=record
    Count:LongWord;
    Quality:LongWord;
  end;
  PTDXGI_SampleDesc=^TDXGI_SampleDesc;
  DXGI_SAMPLE_DESC=TDXGI_SampleDesc;
  PDXGI_SAMPLE_DESC=^TDXGI_SampleDesc;

///////////////////////////////////////////////////////////////////////////////
// End "DXGIType.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "DXGI.h"
///////////////////////////////////////////////////////////////////////////////

const
(* Already defined in "DXGIType.h"
  DXGI_CPU_ACCESS_NONE=0;
  DXGI_CPU_ACCESS_DYNAMIC=1;
  DXGI_CPU_ACCESS_READ_WRITE=2;
  DXGI_CPU_ACCESS_SCRATCH=3;
  DXGI_CPU_ACCESS_FIELD=15;
  DXGI_USAGE_SHADER_INPUT=1L shl (0 + 4);
  DXGI_USAGE_RENDER_TARGET_OUTPUT=1L shl (1 + 4);
  DXGI_USAGE_BACK_BUFFER=1L shl (2 + 4);
  DXGI_USAGE_SHARED=1L shl (3 + 4);
  DXGI_USAGE_READ_ONLY=1L shl (4 + 4);
  DXGI_USAGE_DISCARD_ON_PRESENT=1L shl (5 + 4);
  DXGI_USAGE_UNORDERED_ACCESS=1L shl (6 + 4);
*)
  DXGI_RESOURCE_PRIORITY_MINIMUM=$28000000;
  DXGI_RESOURCE_PRIORITY_LOW=$50000000;
  DXGI_RESOURCE_PRIORITY_NORMAL=$78000000;
  DXGI_RESOURCE_PRIORITY_HIGH=$a0000000;
  DXGI_RESOURCE_PRIORITY_MAXIMUM=$c8000000;
  DXGI_MAP_READ=1;
  DXGI_MAP_WRITE=2;
  DXGI_MAP_DISCARD=4;
  DXGI_ENUM_MODES_INTERLACED=1;
  DXGI_ENUM_MODES_SCALING=2;
  DXGI_MAX_SWAP_CHAIN_BUFFERS=16;
  DXGI_PRESENT_TEST=$00000001;
  DXGI_PRESENT_DO_NOT_SEQUENCE=$00000002;
  DXGI_PRESENT_RESTART=$00000004;
  DXGI_MWA_NO_WINDOW_CHANGES=1 shl 0;
  DXGI_MWA_NO_ALT_ENTER=1 shl 1;
  DXGI_MWA_NO_PRINT_SCREEN=1 shl 2;
  DXGI_MWA_VALID=$7;

type
  IDXGIObject=interface;
  PIDXGIObject=^IDXGIObject;

  IDXGIDeviceSubObject=interface;
  PIDXGIDeviceSubObject=^IDXGIDeviceSubObject;

  IDXGIResource=interface;
  PIDXGIResource=^IDXGIResource;

  IDXGIKeyedMutex=interface;
  PIDXGIKeyedMutex=^IDXGIKeyedMutex;

  IDXGISurface=interface;
  PIDXGISurface=^IDXGISurface;

  IDXGISurface1=interface;
  PIDXGISurface1=^IDXGISurface1;

  IDXGIAdapter=interface;
  PIDXGIAdapter=^IDXGIAdapter;

  IDXGIOutput=interface;
  PIDXGIOutput=^IDXGIOutput;

  IDXGISwapChain=interface;
  PIDXGISwapChain=^IDXGISwapChain;

  IDXGIFactory=interface;
  PIDXGIFactory=^IDXGIFactory;

  IDXGIDevice=interface;
  PIDXGIDevice=^IDXGIDevice;

  IDXGIFactory1=interface;
  PIDXGIFactory1=^IDXGIFactory1;

  IDXGIAdapter1=interface;
  PIDXGIAdapter1=^IDXGIAdapter1;

  IDXGIDevice1=interface;
  PIDXGIDevice1=^IDXGIDevice1;

  TDXGI_Usage=UINT;
  PTDXGI_Usage=^TDXGI_Usage;
  DXGI_USAGE=TDXGI_Usage;
  PDXGI_USAGE=^TDXGI_Usage;

  TDXGI_FrameStatistics=record
    PresentCount:LongWord;
    PresentRefreshCount:LongWord;
    SyncRefreshCount:LongWord;
    SyncQPCTime:LARGE_INTEGER;
    SyncGPUTime:LARGE_INTEGER;
  end;
  PTDXGI_FrameStatistics=^TDXGI_FrameStatistics;
  DXGI_FRAME_STATISTICS=TDXGI_FrameStatistics;
  PDXGI_FRAME_STATISTICS=^TDXGI_FrameStatistics;

  TDXGI_MappedRect=record
    Pitch:Integer;
    pBits:PByte;
  end;
  PTDXGI_MappedRect=^TDXGI_MappedRect;
  DXGI_MAPPED_RECT=TDXGI_MappedRect;
  PDXGI_MAPPED_RECT=^TDXGI_MappedRect;

  PLUID = ^TLUID;
  {$EXTERNALSYM PLUID}
  _LUID = record
    LowPart: DWORD;
    HighPart: Integer;
  end;
  {$EXTERNALSYM _LUID}
  TLUID = _LUID;
  LUID = _LUID;
  {$EXTERNALSYM LUID}

  TDXGI_AdapterDesc=record
    Description:array[0..127] of WideChar;
    VendorId:LongWord;
    DeviceId:LongWord;
    SubSysId:LongWord;
    Revision:LongWord;
    DedicatedVideoMemory:SIZE_T;
    DedicatedSystemMemory:SIZE_T;
    SharedSystemMemory:SIZE_T;
    AdapterLuid:TLUID;
  end;
  PTDXGI_AdapterDesc=^TDXGI_AdapterDesc;
  DXGI_ADAPTER_DESC=TDXGI_AdapterDesc;
  PDXGI_ADAPTER_DESC=^TDXGI_AdapterDesc;

  TDXGI_OutputDesc=record
    DeviceName:array[0..31] of WideChar;
    DesktopCoordinates:TRECT;
    AttachedToDesktop:LongBool;
    Rotation:TDXGI_ModeRotation;
    Monitor:HMONITOR;
  end;
  PTDXGI_OutputDesc=^TDXGI_OutputDesc;
  DXGI_OUTPUT_DESC=TDXGI_OutputDesc;
  PDXGI_OUTPUT_DESC=^TDXGI_OutputDesc;

  TDXGI_SharedResource=record
    Handle:THANDLE;
  end;
  PTDXGI_SharedResource=^TDXGI_SharedResource;
  DXGI_SHARED_RESOURCE=TDXGI_SharedResource;
  PDXGI_SHARED_RESOURCE=^TDXGI_SharedResource;

  TDXGI_Residency=
  (
    DXGI_RESIDENCY_FULLY_RESIDENT=1,
    DXGI_RESIDENCY_RESIDENT_IN_SHARED_MEMORY=2,
    DXGI_RESIDENCY_EVICTED_TO_DISK=3
  );
  PTDXGI_Residency=^TDXGI_Residency;
  DXGI_RESIDENCY=TDXGI_Residency;
  PDXGI_RESIDENCY=^TDXGI_Residency;

  TDXGI_SurfaceDesc=record
    Width:LongWord;
    Height:LongWord;
    Format:TDXGI_Format;
    SampleDesc:TDXGI_SampleDesc;
  end;
  PTDXGI_SurfaceDesc=^TDXGI_SurfaceDesc;
  DXGI_SURFACE_DESC=TDXGI_SurfaceDesc;
  PDXGI_SURFACE_DESC=^TDXGI_SurfaceDesc;

  TDXGI_SwapEffect=
  (
    DXGI_SWAP_EFFECT_DISCARD=0,
    DXGI_SWAP_EFFECT_SEQUENTIAL=1
  );
  PTDXGI_SwapEffect=^TDXGI_SwapEffect;
  DXGI_SWAP_EFFECT=TDXGI_SwapEffect;
  PDXGI_SWAP_EFFECT=^TDXGI_SwapEffect;

  TDXGI_SwapChainFlag=
  (
    DXGI_SWAP_CHAIN_FLAG_NONPREROTATED=1,
    DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH=2,
    DXGI_SWAP_CHAIN_FLAG_GDI_COMPATIBLE=4
  );
  PTDXGI_SwapChainFlag=^TDXGI_SwapChainFlag;
  DXGI_SWAP_CHAIN_FLAG=TDXGI_SwapChainFlag;
  PDXGI_SWAP_CHAIN_FLAG=^TDXGI_SwapChainFlag;

  TDXGI_SwapChainDesc=record
    BufferDesc:TDXGI_ModeDesc;
    SampleDesc:TDXGI_SampleDesc;
    BufferUsage:TDXGI_Usage;
    BufferCount:LongWord;
    OutputWindow:HWND;
    Windowed:LongBool;
    SwapEffect:TDXGI_SwapEffect;
    Flags:LongWord;
  end;
  PTDXGI_SwapChainDesc=^TDXGI_SwapChainDesc;
  DXGI_SWAP_CHAIN_DESC=TDXGI_SwapChainDesc;
  PDXGI_SWAP_CHAIN_DESC=^TDXGI_SwapChainDesc;

  IDXGIObject=interface(IUnknown)
    ['{AEC22FB8-76F3-4639-9BE0-28EB43A67A2E}']
    function SetPrivateData
    (
      const Name:TGUID; (* [in] *)
      DataSize:LongWord; (* [in] *)
      pData:Pointer (* [in] *)
    ):HResult; stdcall;

    function SetPrivateDataInterface
    (
      const Name:TGUID; (* [in] *)
      Unknown:IUnknown (* [in] *)
    ):HResult; stdcall;

    function GetPrivateData
    (
      const Name:TGUID; (* [in] *)
      var DataSize:LongWord; (* [out][in] *)
      pData:Pointer (* [out] *)
    ):HResult; stdcall;

    function GetParent
    (
      const IID:TGUID; (* [in] *)
      out pParent {Interface} (* [retval][out] *)
    ):HResult; stdcall;
  end;

  IDXGIDeviceSubObject=interface(IDXGIObject)
    ['{3D3E0379-F9DE-4D58-BB6C-18D62992F1A6}']
    function GetDevice
    (
      const IID:TGUID; (* [in] *)
      out pDevice {Interface} (* [retval][out] *)
    ):HResult; stdcall;
  end;

  IDXGIResource=interface(IDXGIDeviceSubObject)
    ['{035F3AB4-482E-4E50-B41F-8A7F8BD8960B}']
    function GetSharedHandle
    (
      out SharedHandle:THANDLE (* [out] *)
    ):HResult; stdcall;

    function GetUsage
    (
      out Usage:TDXGI_Usage (* [out] *)
    ):HResult; stdcall;

    function SetEvictionPriority
    (
      EvictionPriority:LongWord (* [in] *)
    ):HResult; stdcall;

    function GetEvictionPriority
    (
      out EvictionPriority:LongWord (* [retval][out] *)
    ):HResult; stdcall;
  end;

  IDXGIKeyedMutex=interface(IDXGIDeviceSubObject)
    ['{9D8E1289-D7B3-465F-8126-250E349AF85D}']
    function AcquireSync
    (
      Key:LargeInt; (* [in] *)
      Milliseconds:LongWord (* [in] *)
    ):HResult; stdcall;

    function ReleaseSync
    (
      Key:LargeInt (* [in] *)
    ):HResult; stdcall;
  end;

  IDXGISurface=interface(IDXGIDeviceSubObject)
    ['{CAFCB56C-6AC3-4889-BF47-9E23BBD260EC}']
    function GetDesc
    (
      out Desc:TDXGI_SurfaceDesc (* [out] *)
    ):HResult; stdcall;

    function Map
    (
      out LockedRect:TDXGI_MappedRect; (* [out] *)
      MapFlags:LongWord (* [in] *)
    ):HResult; stdcall;

    function Unmap:HResult; stdcall;
  end;

  IDXGISurface1=interface(IDXGISurface)
    ['{4AE63092-6327-4C1B-80AE-BFE12EA32B86}']
    function GetDC
    (
      Discard:LongBool; (* [in] *)
      out HDC:HDC (* [out] *)
    ):HResult; stdcall;

    function ReleaseDC
    (
      const DirtyRect:TRECT (* [in] *)
    ):HResult; stdcall;
  end;

  IDXGIAdapter=interface(IDXGIObject)
    ['{2411E7E1-12AC-4CCF-BD14-9798E8534DC0}']
    function EnumOutputs
    (
      OutputIndex:LongWord; (* [in] *)
      out Output:IDXGIOutput (* [out][in] *)
    ):HResult; stdcall;

    function GetDesc
    (
      out Desc:TDXGI_AdapterDesc (* [out] *)
    ):HResult; stdcall;

    function CheckInterfaceSupport
    (
      const InterfaceName:TGUID; (* [in] *)
      out UMDVersion:LARGE_INTEGER (* [out] *)
    ):HResult; stdcall;
  end;

  IDXGIOutput=interface(IDXGIObject)
    ['{AE02EEDB-C735-4690-8D52-5A8DC20213AA}']
    function GetDesc
    (
      out Desc:TDXGI_OutputDesc (* [out] *)
    ):HResult; stdcall;

    function GetDisplayModeList
    (
      EnumFormat:TDXGI_Format; (* [in] *)
      Flags:LongWord; (* [in] *)
      var NumModes:LongWord; (* [out][in] *)
      out Desc:TDXGI_ModeDesc (* [out] *)
    ):HResult; stdcall;

    function FindClosestMatchingMode
    (
      const ModeToMatch:TDXGI_ModeDesc; (* [in] *)
      out ClosestMatch:TDXGI_ModeDesc; (* [out] *)
      ConcernedDevice:IUnknown (* [in] *)
    ):HResult; stdcall;

    function WaitForVBlank:HResult; stdcall;

    function TakeOwnership
    (
      Device:IUnknown; (* [in] *)
      Exclusive:LongBool
    ):HResult; stdcall;

    procedure ReleaseOwnership; stdcall;

    function GetGammaControlCapabilities
    (
      out GammaCaps:TDXGI_GammaControlCapabilities (* [out] *)
    ):HResult; stdcall;

    function SetGammaControl
    (
      const _Array:TDXGI_GammaControl (* [in] *)
    ):HResult; stdcall;

    function GetGammaControl
    (
      out _Array:TDXGI_GammaControl (* [out] *)
    ):HResult; stdcall;

    function SetDisplaySurface
    (
      ScanoutSurface:IDXGISurface (* [in] *)
    ):HResult; stdcall;

    function GetDisplaySurfaceData
    (
      Destination:IDXGISurface (* [in] *)
    ):HResult; stdcall;

    function GetFrameStatistics
    (
      out Stats:TDXGI_FrameStatistics (* [out] *)
    ):HResult; stdcall;
  end;

  IDXGISwapChain=interface(IDXGIDeviceSubObject)
    ['{310D36A0-D2E7-4C0A-AA04-6A9D23B8886A}']
    function Present
    (
      SyncInterval:LongWord; (* [in] *)
      Flags:LongWord (* [in] *)
    ):HResult; stdcall;

    function GetBuffer
    (
      Buffer:LongWord; (* [in] *)
      const IID:TGUID; (* [in] *)
      var pSurface {Interface} (* [out][in] *)
    ):HResult; stdcall;

    function SetFullscreenState
    (
      Fullscreen:LongBool; (* [in] *)
      Target:IDXGIOutput (* [in] *)
    ):HResult; stdcall;

    function GetFullscreenState
    (
      out Fullscreen:LongBool; (* [out] *)
      out Target:IDXGIOutput (* [out] *)
    ):HResult; stdcall;

    function GetDesc
    (
      out Desc:TDXGI_SwapChainDesc (* [out] *)
    ):HResult; stdcall;

    function ResizeBuffers
    (
      BufferCount:LongWord; (* [in] *)
      Width:LongWord; (* [in] *)
      Height:LongWord; (* [in] *)
      NewFormat:TDXGI_Format; (* [in] *)
      SwapChainFlags:LongWord (* [in] *)
    ):HResult; stdcall;

    function ResizeTarget
    (
      const NewTargetParameters:TDXGI_ModeDesc (* [in] *)
    ):HResult; stdcall;

    function GetContainingOutput
    (
      out Output:IDXGIOutput
    ):HResult; stdcall;

    function GetFrameStatistics
    (
      out Stats:TDXGI_FrameStatistics (* [out] *)
    ):HResult; stdcall;

    function GetLastPresentCount
    (
      out LastPresentCount:LongWord (* [out] *)
    ):HResult; stdcall;
  end;

  IDXGIFactory=interface(IDXGIObject)
    ['{7B7166EC-21C7-44AE-B21A-C9AE321AE369}']
    function EnumAdapters
    (
      AdapterIndex:LongWord; (* [in] *)
      out Adapter:IDXGIAdapter (* [out] *)
    ):HResult; stdcall;

    function MakeWindowAssociation
    (
      WindowHandle:HWND;
      Flags:LongWord
    ):HResult; stdcall;

    function GetWindowAssociation
    (
      out WindowHandle:HWND (* [out] *)
    ):HResult; stdcall;

    function CreateSwapChain
    (
      Device:IUnknown; (* [in] *)
      const Desc:TDXGI_SwapChainDesc; (* [in] *)
      out SwapChain:IDXGISwapChain
    ):HResult; stdcall;

    function CreateSoftwareAdapter
    (
      Module:HMODULE; (* [in] *)
      out Adapter:IDXGIAdapter (* [out] *)
    ):HResult; stdcall;
  end;

  IDXGIDevice=interface(IDXGIObject)
    ['{54EC77FA-1377-44E6-8C32-88FD5F44C84C}']
    function GetAdapter
    (
      out pAdapter:IDXGIAdapter (* [out] *)
    ):HResult; stdcall;

    function CreateSurface
    (
      const Desc:TDXGI_SurfaceDesc; (* [in] *)
      NumSurfaces:LongWord; (* [in] *)
      Usage:TDXGI_Usage; (* [in] *)
      const SharedResource:TDXGI_SharedResource; (* [in] *)
      out Surface:IDXGISurface (* [out] *)
    ):HResult; stdcall;

    function QueryResourceResidency
    (
      out Resources:IUnknown; (* [size_is][in] *)
      out ResidencyStatus:TDXGI_Residency; (* [size_is][out] *)
      NumResources:LongWord (* [in] *)
    ):HResult; stdcall;

    function SetGPUThreadPriority
    (
      Priority:Integer (* [in] *)
    ):HResult; stdcall;

    function GetGPUThreadPriority
    (
      out Priority:Integer (* [retval][out] *)
    ):HResult; stdcall;
  end;

  TDXGI_AdapterFlag=
  (
    DXGI_ADAPTER_FLAG_NONE=0,
    DXGI_ADAPTER_FLAG_REMOTE=1
  );
  PTDXGI_AdapterFlag=^TDXGI_AdapterFlag;
  DXGI_ADAPTER_FLAG=TDXGI_AdapterFlag;
  PDXGI_ADAPTER_FLAG=^TDXGI_AdapterFlag;

  TDXGI_AdapterDesc1=record
    Description:array[0..127] of WideChar;
    VendorId:LongWord;
    DeviceId:LongWord;
    SubSysId:LongWord;
    Revision:LongWord;
    DedicatedVideoMemory:SIZE_T;
    DedicatedSystemMemory:SIZE_T;
    SharedSystemMemory:SIZE_T;
    AdapterLuid:TLUID;
    Flags:LongWord;
  end;
  PTDXGI_AdapterDesc1=^TDXGI_AdapterDesc1;
  DXGI_ADAPTER_DESC1=TDXGI_AdapterDesc1;
  PDXGI_ADAPTER_DESC1=^TDXGI_AdapterDesc1;

  TDXGI_DisplayColorSpace=record
    PrimaryCoordinates:array[0..7,0..1] of Single;
    WhitePoints:array[0..15,0..1] of Single;
  end;
  PTDXGI_DisplayColorSpace=^TDXGI_DisplayColorSpace;
  DXGI_DISPLAY_COLOR_SPACE=TDXGI_DisplayColorSpace;
  PDXGI_DISPLAY_COLOR_SPACE=^TDXGI_DisplayColorSpace;

  IDXGIFactory1=interface(IDXGIFactory)
    ['{770AAE78-F26F-4DBA-A829-253C83D1B387}']
    function EnumAdapters1
    (
      AdapterIndex:LongWord; (* [in] *)
      out Adapter:IDXGIAdapter1 (* [out] *)
    ):HResult; stdcall;

    function IsCurrent:LongBool; stdcall;
  end;

  IDXGIAdapter1=interface(IDXGIAdapter)
    ['{29038F61-3839-4626-91FD-086879011A05}']
    function GetDesc1
    (
      out Desc:TDXGI_AdapterDesc1 (* [out] *)
    ):HResult; stdcall;
  end;

  IDXGIDevice1=interface(IDXGIDevice)
    ['{77DB970F-6276-48BA-BA28-070143B4392C}']
    function SetMaximumFrameLatency
    (
      MaxLatency:LongWord (* [in] *)
    ):HResult; stdcall;

    function GetMaximumFrameLatency
    (
      out MaxLatency:LongWord (* [out] *)
    ):HResult; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
// End "DXGI.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_16BIT_INDEX_STRIP_CUT_VALUE=$ffff;
  D3D10_32BIT_INDEX_STRIP_CUT_VALUE=$ffffffff;
  D3D10_8BIT_INDEX_STRIP_CUT_VALUE=$ff;
  D3D10_ARRAY_AXIS_ADDRESS_RANGE_BIT_COUNT=9;
  D3D10_CLIP_OR_CULL_DISTANCE_COUNT=8;
  D3D10_CLIP_OR_CULL_DISTANCE_ELEMENT_COUNT=2;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT=14;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_COMPONENTS=4;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_COMPONENT_BIT_COUNT=32;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_HW_SLOT_COUNT=15;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_COMPONENTS=4;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_COUNT=15;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_CONSTANT_BUFFER_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_FLOWCONTROL_NESTING_LIMIT=64;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_COMPONENTS=4;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_COUNT=1;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_IMMEDIATE_CONSTANT_BUFFER_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_IMMEDIATE_VALUE_COMPONENT_BIT_COUNT=32;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_COMPONENTS=1;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_COUNT=128;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_INPUT_RESOURCE_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT=128;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_COMPONENTS=1;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_COUNT=16;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_READS_PER_INST=1;
  D3D10_COMMONSHADER_SAMPLER_REGISTER_READ_PORTS=1;
  D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT=16;
  D3D10_COMMONSHADER_SUBROUTINE_NESTING_LIMIT=32;
  D3D10_COMMONSHADER_TEMP_REGISTER_COMPONENTS=4;
  D3D10_COMMONSHADER_TEMP_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_COMMONSHADER_TEMP_REGISTER_COUNT=4096;
  D3D10_COMMONSHADER_TEMP_REGISTER_READS_PER_INST=3;
  D3D10_COMMONSHADER_TEMP_REGISTER_READ_PORTS=3;
  D3D10_COMMONSHADER_TEXCOORD_RANGE_REDUCTION_MAX=10;
  D3D10_COMMONSHADER_TEXCOORD_RANGE_REDUCTION_MIN=-10;
  D3D10_COMMONSHADER_TEXEL_OFFSET_MAX_NEGATIVE=-8;
  D3D10_COMMONSHADER_TEXEL_OFFSET_MAX_POSITIVE=7;
  D3D10_DEFAULT_BLEND_FACTOR_ALPHA=1.0;
  D3D10_DEFAULT_BLEND_FACTOR_BLUE=1.0;
  D3D10_DEFAULT_BLEND_FACTOR_GREEN=1.0;
  D3D10_DEFAULT_BLEND_FACTOR_RED=1.0;
  D3D10_DEFAULT_BORDER_COLOR_COMPONENT=0.0;
  D3D10_DEFAULT_DEPTH_BIAS=0;
  D3D10_DEFAULT_DEPTH_BIAS_CLAMP=0.0;
  D3D10_DEFAULT_MAX_ANISOTROPY=16.0;
  D3D10_DEFAULT_MIP_LOD_BIAS=0.0;
  D3D10_DEFAULT_RENDER_TARGET_ARRAY_INDEX=0;
  D3D10_DEFAULT_SAMPLE_MASK=$ffffffff;
  D3D10_DEFAULT_SCISSOR_ENDX=0;
  D3D10_DEFAULT_SCISSOR_ENDY=0;
  D3D10_DEFAULT_SCISSOR_STARTX=0;
  D3D10_DEFAULT_SCISSOR_STARTY=0;
  D3D10_DEFAULT_SLOPE_SCALED_DEPTH_BIAS=0.0;
  D3D10_DEFAULT_STENCIL_READ_MASK=$ff;
  D3D10_DEFAULT_STENCIL_REFERENCE=0;
  D3D10_DEFAULT_STENCIL_WRITE_MASK=$ff;
  D3D10_DEFAULT_VIEWPORT_AND_SCISSORRECT_INDEX=0;
  D3D10_DEFAULT_VIEWPORT_HEIGHT=0;
  D3D10_DEFAULT_VIEWPORT_MAX_DEPTH=0.0;
  D3D10_DEFAULT_VIEWPORT_MIN_DEPTH=0.0;
  D3D10_DEFAULT_VIEWPORT_TOPLEFTX=0;
  D3D10_DEFAULT_VIEWPORT_TOPLEFTY=0;
  D3D10_DEFAULT_VIEWPORT_WIDTH=0;
  D3D10_FLOAT16_FUSED_TOLERANCE_IN_ULP=0.6;
  D3D10_FLOAT32_MAX=3.402823466e+38;
  D3D10_FLOAT32_TO_INTEGER_TOLERANCE_IN_ULP=0.6;
  D3D10_FLOAT_TO_SRGB_EXPONENT_DENOMINATOR=2.4;
  D3D10_FLOAT_TO_SRGB_EXPONENT_NUMERATOR=1.0;
  D3D10_FLOAT_TO_SRGB_OFFSET=0.055;
  D3D10_FLOAT_TO_SRGB_SCALE_1=12.92;
  D3D10_FLOAT_TO_SRGB_SCALE_2=1.055;
  D3D10_FLOAT_TO_SRGB_THRESHOLD=0.0031308;
  D3D10_FTOI_INSTRUCTION_MAX_INPUT=2147483647.999;
  D3D10_FTOI_INSTRUCTION_MIN_INPUT=-2147483648.999;
  D3D10_FTOU_INSTRUCTION_MAX_INPUT=4294967295.999;
  D3D10_FTOU_INSTRUCTION_MIN_INPUT=0.0;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_COMPONENTS=1;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_COUNT=1;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_READS_PER_INST=2;
  D3D10_GS_INPUT_PRIM_CONST_REGISTER_READ_PORTS=1;
  D3D10_GS_INPUT_REGISTER_COMPONENTS=4;
  D3D10_GS_INPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_GS_INPUT_REGISTER_COUNT=16;
  D3D10_GS_INPUT_REGISTER_READS_PER_INST=2;
  D3D10_GS_INPUT_REGISTER_READ_PORTS=1;
  D3D10_GS_INPUT_REGISTER_VERTICES=6;
  D3D10_GS_OUTPUT_ELEMENTS=32;
  D3D10_GS_OUTPUT_REGISTER_COMPONENTS=4;
  D3D10_GS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_GS_OUTPUT_REGISTER_COUNT=32;
  D3D10_IA_DEFAULT_INDEX_BUFFER_OFFSET_IN_BYTES=0;
  D3D10_IA_DEFAULT_PRIMITIVE_TOPOLOGY=0;
  D3D10_IA_DEFAULT_VERTEX_BUFFER_OFFSET_IN_BYTES=0;
  D3D10_IA_INDEX_INPUT_RESOURCE_SLOT_COUNT=1;
  D3D10_IA_INSTANCE_ID_BIT_COUNT=32;
  D3D10_IA_INTEGER_ARITHMETIC_BIT_COUNT=32;
  D3D10_IA_PRIMITIVE_ID_BIT_COUNT=32;
  D3D10_IA_VERTEX_ID_BIT_COUNT=32;
  D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT=16;
  D3D10_IA_VERTEX_INPUT_STRUCTURE_ELEMENTS_COMPONENTS=64;
  D3D10_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT=16;
  D3D10_INTEGER_DIVIDE_BY_ZERO_QUOTIENT=$ffffffff;
  D3D10_INTEGER_DIVIDE_BY_ZERO_REMAINDER=$ffffffff;
  D3D10_LINEAR_GAMMA=1.0;
  D3D10_MAX_BORDER_COLOR_COMPONENT=1.0;
  D3D10_MAX_DEPTH=1.0;
  D3D10_MAX_MAXANISOTROPY=16;
  D3D10_MAX_MULTISAMPLE_SAMPLE_COUNT=32;
  D3D10_MAX_POSITION_VALUE=3.402823466e+34;
  D3D10_MAX_TEXTURE_DIMENSION_2_TO_EXP=17;
  D3D10_MIN_BORDER_COLOR_COMPONENT=0.0;
  D3D10_MIN_DEPTH=0.0;
  D3D10_MIN_MAXANISOTROPY=0;
  D3D10_MIP_LOD_BIAS_MAX=15.99;
  D3D10_MIP_LOD_BIAS_MIN=-16.0;
  D3D10_MIP_LOD_FRACTIONAL_BIT_COUNT=6;
  D3D10_MIP_LOD_RANGE_BIT_COUNT=8;
  D3D10_MULTISAMPLE_ANTIALIAS_LINE_WIDTH=1.4;
  D3D10_NONSAMPLE_FETCH_OUT_OF_RANGE_ACCESS_RESULT=0;
  D3D10_PIXEL_ADDRESS_RANGE_BIT_COUNT=13;
  D3D10_PRE_SCISSOR_PIXEL_ADDRESS_RANGE_BIT_COUNT=15;
  D3D10_PS_FRONTFACING_DEFAULT_VALUE=$ffffffff;
  D3D10_PS_FRONTFACING_FALSE_VALUE=0;
  D3D10_PS_FRONTFACING_TRUE_VALUE=$ffffffff;
  D3D10_PS_INPUT_REGISTER_COMPONENTS=4;
  D3D10_PS_INPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_PS_INPUT_REGISTER_COUNT=32;
  D3D10_PS_INPUT_REGISTER_READS_PER_INST=2;
  D3D10_PS_INPUT_REGISTER_READ_PORTS=1;
  D3D10_PS_LEGACY_PIXEL_CENTER_FRACTIONAL_COMPONENT=0.0;
  D3D10_PS_OUTPUT_DEPTH_REGISTER_COMPONENTS=1;
  D3D10_PS_OUTPUT_DEPTH_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_PS_OUTPUT_DEPTH_REGISTER_COUNT=1;
  D3D10_PS_OUTPUT_REGISTER_COMPONENTS=4;
  D3D10_PS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_PS_OUTPUT_REGISTER_COUNT=8;
  D3D10_PS_PIXEL_CENTER_FRACTIONAL_COMPONENT=0.5;
  D3D10_REQ_BLEND_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_BUFFER_RESOURCE_TEXEL_COUNT_2_TO_EXP=27;
  D3D10_REQ_CONSTANT_BUFFER_ELEMENT_COUNT=4096;
  D3D10_REQ_DEPTH_STENCIL_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_DRAWINDEXED_INDEX_COUNT_2_TO_EXP=32;
  D3D10_REQ_DRAW_VERTEX_COUNT_2_TO_EXP=32;
  D3D10_REQ_FILTERING_HW_ADDRESSABLE_RESOURCE_DIMENSION=8192;
  D3D10_REQ_GS_INVOCATION_32BIT_OUTPUT_COMPONENT_LIMIT=1024;
  D3D10_REQ_IMMEDIATE_CONSTANT_BUFFER_ELEMENT_COUNT=4096;
  D3D10_REQ_MAXANISOTROPY=16;
  D3D10_REQ_MIP_LEVELS=14;
  D3D10_REQ_MULTI_ELEMENT_STRUCTURE_SIZE_IN_BYTES=2048;
  D3D10_REQ_RASTERIZER_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_RENDER_TO_BUFFER_WINDOW_WIDTH=8192;
  D3D10_REQ_RESOURCE_SIZE_IN_MEGABYTES=128;
  D3D10_REQ_RESOURCE_VIEW_COUNT_PER_CONTEXT_2_TO_EXP=20;
  D3D10_REQ_SAMPLER_OBJECT_COUNT_PER_CONTEXT=4096;
  D3D10_REQ_TEXTURE1D_ARRAY_AXIS_DIMENSION=512;
  D3D10_REQ_TEXTURE1D_U_DIMENSION=8192;
  D3D10_REQ_TEXTURE2D_ARRAY_AXIS_DIMENSION=512;
  D3D10_REQ_TEXTURE2D_U_OR_V_DIMENSION=8192;
  D3D10_REQ_TEXTURE3D_U_V_OR_W_DIMENSION=2048;
  D3D10_REQ_TEXTURECUBE_DIMENSION=8192;
  D3D10_RESINFO_INSTRUCTION_MISSING_COMPONENT_RETVAL=0;
  D3D10_SHADER_MAJOR_VERSION=4;
  D3D10_SHADER_MINOR_VERSION=0;
  D3D10_SHIFT_INSTRUCTION_PAD_VALUE=0;
  D3D10_SHIFT_INSTRUCTION_SHIFT_VALUE_BIT_COUNT=5;
  D3D10_SIMULTANEOUS_RENDER_TARGET_COUNT=8;
  D3D10_SO_BUFFER_MAX_STRIDE_IN_BYTES=2048;
  D3D10_SO_BUFFER_MAX_WRITE_WINDOW_IN_BYTES=256;
  D3D10_SO_BUFFER_SLOT_COUNT=4;
  D3D10_SO_DDI_REGISTER_INDEX_DENOTING_GAP=$ffffffff;
  D3D10_SO_MULTIPLE_BUFFER_ELEMENTS_PER_BUFFER=1;
  D3D10_SO_SINGLE_BUFFER_COMPONENT_LIMIT=64;
  D3D10_SRGB_GAMMA=2.2;
  D3D10_SRGB_TO_FLOAT_DENOMINATOR_1=12.92;
  D3D10_SRGB_TO_FLOAT_DENOMINATOR_2=1.055;
  D3D10_SRGB_TO_FLOAT_EXPONENT=2.4;
  D3D10_SRGB_TO_FLOAT_OFFSET=0.055;
  D3D10_SRGB_TO_FLOAT_THRESHOLD=0.04045;
  D3D10_SRGB_TO_FLOAT_TOLERANCE_IN_ULP=0.5;
  D3D10_STANDARD_COMPONENT_BIT_COUNT=32;
  D3D10_STANDARD_COMPONENT_BIT_COUNT_DOUBLED=64;
  D3D10_STANDARD_MAXIMUM_ELEMENT_ALIGNMENT_BYTE_MULTIPLE=4;
  D3D10_STANDARD_PIXEL_COMPONENT_COUNT=128;
  D3D10_STANDARD_PIXEL_ELEMENT_COUNT=32;
  D3D10_STANDARD_VECTOR_SIZE=4;
  D3D10_STANDARD_VERTEX_ELEMENT_COUNT=16;
  D3D10_STANDARD_VERTEX_TOTAL_COMPONENT_COUNT=64;
  D3D10_SUBPIXEL_FRACTIONAL_BIT_COUNT=8;
  D3D10_SUBTEXEL_FRACTIONAL_BIT_COUNT=6;
  D3D10_TEXEL_ADDRESS_RANGE_BIT_COUNT=18;
  D3D10_UNBOUND_MEMORY_ACCESS_RESULT=0;
  D3D10_VIEWPORT_AND_SCISSORRECT_MAX_INDEX=15;
  D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE=16;
  D3D10_VIEWPORT_BOUNDS_MAX=16383;
  D3D10_VIEWPORT_BOUNDS_MIN=-16384;
  D3D10_VS_INPUT_REGISTER_COMPONENTS=4;
  D3D10_VS_INPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_VS_INPUT_REGISTER_COUNT=16;
  D3D10_VS_INPUT_REGISTER_READS_PER_INST=2;
  D3D10_VS_INPUT_REGISTER_READ_PORTS=1;
  D3D10_VS_OUTPUT_REGISTER_COMPONENTS=4;
  D3D10_VS_OUTPUT_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_VS_OUTPUT_REGISTER_COUNT=16;
  D3D10_WHQL_CONTEXT_COUNT_FOR_RESOURCE_LIMIT=10;
  D3D10_WHQL_DRAWINDEXED_INDEX_COUNT_2_TO_EXP=25;
  D3D10_WHQL_DRAW_VERTEX_COUNT_2_TO_EXP=25;
  D3D_MAJOR_VERSION=10;
  D3D_MINOR_VERSION=0;
  D3D_SPEC_DATE_DAY=8;
  D3D_SPEC_DATE_MONTH=8;
  D3D_SPEC_DATE_YEAR=2006;
  D3D_SPEC_VERSION=1.050005;

const
  _FACD3D10=$879;
  _FACD3D10DEBUG=_FACD3D10+1;
  D3D10_STATUS_Base=LongWord(_FACD3D10 shl 16);
  D3D10_HRESULT_Base=D3D10_STATUS_Base or LongWord(1 shl 31);
  //
  D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS=HResult(D3D10_HRESULT_Base or 1);
  D3D10_ERROR_FILE_NOT_FOUND=HResult(D3D10_HRESULT_Base or 2);
  //
  D3D10_APPEND_ALIGNED_ELEMENT=$ffffffff;
  D3D10_FILTER_TYPE_MASK=$3;
  D3D10_MIN_FILTER_SHIFT=4;
  D3D10_MAG_FILTER_SHIFT=2;
  D3D10_MIP_FILTER_SHIFT=0;
  D3D10_COMPARISON_FILTERING_BIT=$80;
  D3D10_ANISOTROPIC_FILTERING_BIT=$40;
  D3D10_TEXT_1BIT_BIT=$80000000;
  D3D10_SDK_VERSION=29;

type
  ID3D10DeviceChild=interface;
  PID3D10DeviceChild=^ID3D10DeviceChild;

  ID3D10DepthStencilState=interface;
  PID3D10DepthStencilState=^ID3D10DepthStencilState;

  ID3D10BlendState=interface;
  PID3D10BlendState=^ID3D10BlendState;

  ID3D10RasterizerState=interface;
  PID3D10RasterizerState=^ID3D10RasterizerState;

  ID3D10Resource=interface;
  PID3D10Resource=^ID3D10Resource;

  ID3D10Buffer=interface;
  PID3D10Buffer=^ID3D10Buffer;

  ID3D10Texture1D=interface;
  PID3D10Texture1D=^ID3D10Texture1D;

  ID3D10Texture2D=interface;
  PID3D10Texture2D=^ID3D10Texture2D;

  ID3D10Texture3D=interface;
  PID3D10Texture3D=^ID3D10Texture3D;

  ID3D10View=interface;
  PID3D10View=^ID3D10View;

  ID3D10ShaderResourceView=interface;
  PID3D10ShaderResourceView=^ID3D10ShaderResourceView;

  ID3D10RenderTargetView=interface;
  PID3D10RenderTargetView=^ID3D10RenderTargetView;

  ID3D10DepthStencilView=interface;
  PID3D10DepthStencilView=^ID3D10DepthStencilView;

  ID3D10VertexShader=interface;
  PID3D10VertexShader=^ID3D10VertexShader;

  ID3D10GeometryShader=interface;
  PID3D10GeometryShader=^ID3D10GeometryShader;

  ID3D10PixelShader=interface;
  PID3D10PixelShader=^ID3D10PixelShader;

  ID3D10InputLayout=interface;
  PID3D10InputLayout=^ID3D10InputLayout;

  ID3D10SamplerState=interface;
  PID3D10SamplerState=^ID3D10SamplerState;

  ID3D10Asynchronous=interface;
  PID3D10Asynchronous=^ID3D10Asynchronous;

  ID3D10Query=interface;
  PID3D10Query=^ID3D10Query;

  ID3D10Predicate=interface;
  PID3D10Predicate=^ID3D10Predicate;

  ID3D10Counter=interface;
  PID3D10Counter=^ID3D10Counter;

  ID3D10Device=interface;
  PID3D10Device=^ID3D10Device;

  ID3D10Multithread=interface;
  PID3D10Multithread=^ID3D10Multithread;

  TD3D10_InputClassification=
  (
    D3D10_INPUT_PER_VERTEX_DATA=0,
    D3D10_INPUT_PER_INSTANCE_DATA=1
  );
  PTD3D10_InputClassification=^TD3D10_InputClassification;
  D3D10_INPUT_CLASSIFICATION=TD3D10_InputClassification;
  PD3D10_INPUT_CLASSIFICATION=^TD3D10_InputClassification;

  TD3D10_InputElementDesc=record
    SemanticName:PAnsiChar;
    SemanticIndex:LongWord;
    Format:TDXGI_Format;
    InputSlot:LongWord;
    AlignedByteOffset:LongWord;
    InputSlotClass:TD3D10_InputClassification;
    InstanceDataStepRate:LongWord;
  end;
  PTD3D10_InputElementDesc=^TD3D10_InputElementDesc;
  D3D10_INPUT_ELEMENT_DESC=TD3D10_InputElementDesc;
  PD3D10_INPUT_ELEMENT_DESC=^TD3D10_InputElementDesc;

  TD3D10_FillMode=
  (
    D3D10_FILL_WIREFRAME=2,
    D3D10_FILL_SOLID=3
  );
  PTD3D10_FillMode=^TD3D10_FillMode;
  D3D10_FILL_MODE=TD3D10_FillMode;
  PD3D10_FILL_MODE=^TD3D10_FillMode;

  TD3D10_PrimitiveTopology=
  (
    D3D10_PRIMITIVE_TOPOLOGY_UNDEFINED=0,
    D3D10_PRIMITIVE_TOPOLOGY_POINTLIST=1,
    D3D10_PRIMITIVE_TOPOLOGY_LINELIST=2,
    D3D10_PRIMITIVE_TOPOLOGY_LINESTRIP=3,
    D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST=4,
    D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP=5,
    D3D10_PRIMITIVE_TOPOLOGY_LINELIST_ADJ=10,
    D3D10_PRIMITIVE_TOPOLOGY_LINESTRIP_ADJ=11,
    D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST_ADJ=12,
    D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP_ADJ=13
  );
  PTD3D10_PrimitiveTopology=^TD3D10_PrimitiveTopology;
  D3D10_PRIMITIVE_TOPOLOGY=TD3D10_PrimitiveTopology;
  PD3D10_PRIMITIVE_TOPOLOGY=^TD3D10_PrimitiveTopology;

  TD3D10_Primitive=
  (
    D3D10_PRIMITIVE_UNDEFINED=0,
    D3D10_PRIMITIVE_POINT=1,
    D3D10_PRIMITIVE_LINE=2,
    D3D10_PRIMITIVE_TRIANGLE=3,
    D3D10_PRIMITIVE_LINE_ADJ=6,
    D3D10_PRIMITIVE_TRIANGLE_ADJ=7
  );
  PTD3D10_Primitive=^TD3D10_Primitive;
  D3D10_PRIMITIVE=TD3D10_Primitive;
  PD3D10_PRIMITIVE=^TD3D10_Primitive;

  TD3D10_CullMode=
  (
    D3D10_CULL_NONE=1,
    D3D10_CULL_FRONT=2,
    D3D10_CULL_BACK=3
  );
  PTD3D10_CullMode=^TD3D10_CullMode;
  D3D10_CULL_MODE=TD3D10_CullMode;
  PD3D10_CULL_MODE=^TD3D10_CullMode;

  TD3D10_SoDeclarationEntry=record
    SemanticName:PAnsiChar;
    SemanticIndex:LongWord;
    StartComponent:Byte;
    ComponentCount:Byte;
    OutputSlot:Byte;
  end;
  PTD3D10_SoDeclarationEntry=^TD3D10_SoDeclarationEntry;
  D3D10_SO_DECLARATION_ENTRY=TD3D10_SoDeclarationEntry;
  PD3D10_SO_DECLARATION_ENTRY=^TD3D10_SoDeclarationEntry;

  TD3D10_Viewport=record
    TopLeftX:Integer;
    TopLeftY:Integer;
    Width:LongWord;
    Height:LongWord;
    MinDepth:Single;
    MaxDepth:Single;
  end;
  PTD3D10_Viewport=^TD3D10_Viewport;
  D3D10_VIEWPORT=TD3D10_Viewport;
  PD3D10_VIEWPORT=^TD3D10_Viewport;

  TD3D10_ResourceDimension=
  (
    D3D10_RESOURCE_DIMENSION_UNKNOWN=0,
    D3D10_RESOURCE_DIMENSION_BUFFER=1,
    D3D10_RESOURCE_DIMENSION_TEXTURE1D=2,
    D3D10_RESOURCE_DIMENSION_TEXTURE2D=3,
    D3D10_RESOURCE_DIMENSION_TEXTURE3D=4
  );
  PTD3D10_ResourceDimension=^TD3D10_ResourceDimension;
  D3D10_RESOURCE_DIMENSION=TD3D10_ResourceDimension;
  PD3D10_RESOURCE_DIMENSION=^TD3D10_ResourceDimension;

  TD3D10_SrvDimension=
  (
    D3D10_SRV_DIMENSION_UNKNOWN=0,
    D3D10_SRV_DIMENSION_BUFFER=1,
    D3D10_SRV_DIMENSION_TEXTURE1D=2,
    D3D10_SRV_DIMENSION_TEXTURE1DARRAY=3,
    D3D10_SRV_DIMENSION_TEXTURE2D=4,
    D3D10_SRV_DIMENSION_TEXTURE2DARRAY=5,
    D3D10_SRV_DIMENSION_TEXTURE2DMS=6,
    D3D10_SRV_DIMENSION_TEXTURE2DMSARRAY=7,
    D3D10_SRV_DIMENSION_TEXTURE3D=8,
    D3D10_SRV_DIMENSION_TEXTURECUBE=9
  );
  PTD3D10_SrvDimension=^TD3D10_SrvDimension;
  D3D10_SRV_DIMENSION=TD3D10_SrvDimension;
  PD3D10_SRV_DIMENSION=^TD3D10_SrvDimension;

  TD3D10_DsvDimension=
  (
    D3D10_DSV_DIMENSION_UNKNOWN=0,
    D3D10_DSV_DIMENSION_TEXTURE1D=1,
    D3D10_DSV_DIMENSION_TEXTURE1DARRAY=2,
    D3D10_DSV_DIMENSION_TEXTURE2D=3,
    D3D10_DSV_DIMENSION_TEXTURE2DARRAY=4,
    D3D10_DSV_DIMENSION_TEXTURE2DMS=5,
    D3D10_DSV_DIMENSION_TEXTURE2DMSARRAY=6
  );
  PTD3D10_DsvDimension=^TD3D10_DsvDimension;
  D3D10_DSV_DIMENSION=TD3D10_DsvDimension;
  PD3D10_DSV_DIMENSION=^TD3D10_DsvDimension;

  TD3D10_RtvDimension=
  (
    D3D10_RTV_DIMENSION_UNKNOWN=0,
    D3D10_RTV_DIMENSION_BUFFER=1,
    D3D10_RTV_DIMENSION_TEXTURE1D=2,
    D3D10_RTV_DIMENSION_TEXTURE1DARRAY=3,
    D3D10_RTV_DIMENSION_TEXTURE2D=4,
    D3D10_RTV_DIMENSION_TEXTURE2DARRAY=5,
    D3D10_RTV_DIMENSION_TEXTURE2DMS=6,
    D3D10_RTV_DIMENSION_TEXTURE2DMSARRAY=7,
    D3D10_RTV_DIMENSION_TEXTURE3D=8
  );
  PTD3D10_RtvDimension=^TD3D10_RtvDimension;
  D3D10_RTV_DIMENSION=TD3D10_RtvDimension;
  PD3D10_RTV_DIMENSION=^TD3D10_RtvDimension;

  TD3D10_Usage=
  (
    D3D10_USAGE_DEFAULT=0,
    D3D10_USAGE_IMMUTABLE=1,
    D3D10_USAGE_DYNAMIC=2,
    D3D10_USAGE_STAGING=3
  );
  PTD3D10_Usage=^TD3D10_Usage;
  D3D10_USAGE=TD3D10_Usage;
  PD3D10_USAGE=^TD3D10_Usage;

  TD3D10_BindFlag=
  (
    D3D10_BIND_VERTEX_BUFFER=$1,
    D3D10_BIND_INDEX_BUFFER=$2,
    D3D10_BIND_CONSTANT_BUFFER=$4,
    D3D10_BIND_SHADER_RESOURCE=$8,
    D3D10_BIND_STREAM_OUTPUT=$10,
    D3D10_BIND_RENDER_TARGET=$20,
    D3D10_BIND_DEPTH_STENCIL=$40
  );
  PTD3D10_BindFlag=^TD3D10_BindFlag;
  D3D10_BIND_FLAG=TD3D10_BindFlag;
  PD3D10_BIND_FLAG=^TD3D10_BindFlag;

  TD3D10_CpuAccessFlag=
  (
    D3D10_CPU_ACCESS_WRITE=$10000,
    D3D10_CPU_ACCESS_READ=$20000
  );
  PTD3D10_CpuAccessFlag=^TD3D10_CpuAccessFlag;
  D3D10_CPU_ACCESS_FLAG=TD3D10_CpuAccessFlag;
  PD3D10_CPU_ACCESS_FLAG=^TD3D10_CpuAccessFlag;

  TD3D10_ResourceMiscFlag=
  (
    D3D10_RESOURCE_MISC_GENERATE_MIPS=$1,
    D3D10_RESOURCE_MISC_SHARED=$2,
    D3D10_RESOURCE_MISC_TEXTURECUBE=$4,
    D3D10_RESOURCE_MISC_SHARED_KEYEDMUTEX=$10,
    D3D10_RESOURCE_MISC_GDI_COMPATIBLE=$20
  );
  PTD3D10_ResourceMiscFlag=^TD3D10_ResourceMiscFlag;
  D3D10_RESOURCE_MISC_FLAG=TD3D10_ResourceMiscFlag;
  PD3D10_RESOURCE_MISC_FLAG=^TD3D10_ResourceMiscFlag;

  TD3D10_Map=
  (
    D3D10_MAP_READ=1,
    D3D10_MAP_WRITE=2,
    D3D10_MAP_READ_WRITE=3,
    D3D10_MAP_WRITE_DISCARD=4,
    D3D10_MAP_WRITE_NO_OVERWRITE=5
  );
  PTD3D10_Map=^TD3D10_Map;
  D3D10_MAP=TD3D10_Map;
  PD3D10_MAP=^TD3D10_Map;

  TD3D10_MapFlag=
  (
    D3D10_MAP_FLAG_DO_NOT_WAIT=$100000
  );
  PTD3D10_MapFlag=^TD3D10_MapFlag;
  D3D10_MAP_FLAG=TD3D10_MapFlag;
  PD3D10_MAP_FLAG=^TD3D10_MapFlag;

  TD3D10_RaiseFlag=
  (
    D3D10_RAISE_FLAG_DRIVER_INTERNAL_ERROR=$1
  );
  PTD3D10_RaiseFlag=^TD3D10_RaiseFlag;
  D3D10_RAISE_FLAG=TD3D10_RaiseFlag;
  PD3D10_RAISE_FLAG=^TD3D10_RaiseFlag;

  TD3D10_ClearFlag=
  (
    D3D10_CLEAR_DEPTH=$1,
    D3D10_CLEAR_STENCIL=$2
  );
  PTD3D10_ClearFlag=^TD3D10_ClearFlag;
  D3D10_CLEAR_FLAG=TD3D10_ClearFlag;
  PD3D10_CLEAR_FLAG=^TD3D10_ClearFlag;

  TD3D10_Rect=TRECT;
  PTD3D10_Rect=^TD3D10_Rect;

  TD3D10_Box=record
    Left:LongWord;
    Top:LongWord;
    Front:LongWord;
    Right:LongWord;
    Bottom:LongWord;
    Back:LongWord;
  end;
  PTD3D10_Box=^TD3D10_Box;
  D3D10_BOX=TD3D10_Box;
  PD3D10_BOX=^TD3D10_Box;

  ID3D10DeviceChild=interface(IUnknown)
    ['{9B7E4C00-342C-4106-A19F-4F2704F689F0}']
    procedure GetDevice
    (
      out Device:ID3D10Device (* __out *)
    ); stdcall;

    function GetPrivateData
    (
      const Guid:TGUID; (* __in *)
      var DataSize:LongWord; (* __inout *)
      pData:Pointer (* __out_bcount_opt(*pDataSize) *)
    ):HResult; stdcall;

    function SetPrivateData
    (
      const Guid:TGUID; (* __in *)
      DataSize:LongWord; (* __in *)
      pData:Pointer (* __in_bcount_opt(DataSize) *)
    ):HResult; stdcall;

    function SetPrivateDataInterface
    (
      const Guid:TGUID; (* __in *)
      Data:IUnknown (* __in_opt *)
    ):HResult; stdcall;
  end;

  TD3D10_ComparisonFunc=
  (
    D3D10_COMPARISON_NEVER=1,
    D3D10_COMPARISON_LESS=2,
    D3D10_COMPARISON_EQUAL=3,
    D3D10_COMPARISON_LESS_EQUAL=4,
    D3D10_COMPARISON_GREATER=5,
    D3D10_COMPARISON_NOT_EQUAL=6,
    D3D10_COMPARISON_GREATER_EQUAL=7,
    D3D10_COMPARISON_ALWAYS=8
  );
  PTD3D10_ComparisonFunc=^TD3D10_ComparisonFunc;
  D3D10_COMPARISON_FUNC=TD3D10_ComparisonFunc;
  PD3D10_COMPARISON_FUNC=^TD3D10_ComparisonFunc;

  TD3D10_DepthWriteMask=
  (
    D3D10_DEPTH_WRITE_MASK_ZERO=0,
    D3D10_DEPTH_WRITE_MASK_ALL=1
  );
  PTD3D10_DepthWriteMask=^TD3D10_DepthWriteMask;
  D3D10_DEPTH_WRITE_MASK=TD3D10_DepthWriteMask;
  PD3D10_DEPTH_WRITE_MASK=^TD3D10_DepthWriteMask;

  TD3D10_StencilOp=
  (
    D3D10_STENCIL_OP_KEEP=1,
    D3D10_STENCIL_OP_ZERO=2,
    D3D10_STENCIL_OP_REPLACE=3,
    D3D10_STENCIL_OP_INCR_SAT=4,
    D3D10_STENCIL_OP_DECR_SAT=5,
    D3D10_STENCIL_OP_INVERT=6,
    D3D10_STENCIL_OP_INCR=7,
    D3D10_STENCIL_OP_DECR=8
  );
  PTD3D10_StencilOp=^TD3D10_StencilOp;
  D3D10_STENCIL_OP=TD3D10_StencilOp;
  PD3D10_STENCIL_OP=^TD3D10_StencilOp;

  TD3D10_DepthStencilopDesc=record
    StencilFailOp:TD3D10_StencilOp;
    StencilDepthFailOp:TD3D10_StencilOp;
    StencilPassOp:TD3D10_StencilOp;
    StencilFunc:TD3D10_ComparisonFunc;
  end;
  PTD3D10_DepthStencilopDesc=^TD3D10_DepthStencilopDesc;
  D3D10_DEPTH_STENCILOP_DESC=TD3D10_DepthStencilopDesc;
  PD3D10_DEPTH_STENCILOP_DESC=^TD3D10_DepthStencilopDesc;

  TD3D10_DepthStencilDesc=record
    DepthEnable:LongBool;
    DepthWriteMask:TD3D10_DepthWriteMask;
    DepthFunc:TD3D10_ComparisonFunc;
    StencilEnable:LongBool;
    StencilReadMask:Byte;
    StencilWriteMask:Byte;
    FrontFace:TD3D10_DepthStencilopDesc;
    BackFace:TD3D10_DepthStencilopDesc;
  end;
  PTD3D10_DepthStencilDesc=^TD3D10_DepthStencilDesc;
  D3D10_DEPTH_STENCIL_DESC=TD3D10_DepthStencilDesc;
  PD3D10_DEPTH_STENCIL_DESC=^TD3D10_DepthStencilDesc;

  ID3D10DepthStencilState=interface(ID3D10DeviceChild)
    ['{2B4B1CC8-A4AD-41F8-8322-CA86FC3EC675}']
    procedure GetDesc
    (
      out Desc:TD3D10_DepthStencilDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Blend=
  (
    D3D10_BLEND_ZERO=1,
    D3D10_BLEND_ONE=2,
    D3D10_BLEND_SRC_COLOR=3,
    D3D10_BLEND_INV_SRC_COLOR=4,
    D3D10_BLEND_SRC_ALPHA=5,
    D3D10_BLEND_INV_SRC_ALPHA=6,
    D3D10_BLEND_DEST_ALPHA=7,
    D3D10_BLEND_INV_DEST_ALPHA=8,
    D3D10_BLEND_DEST_COLOR=9,
    D3D10_BLEND_INV_DEST_COLOR=10,
    D3D10_BLEND_SRC_ALPHA_SAT=11,
    D3D10_BLEND_BLEND_FACTOR=14,
    D3D10_BLEND_INV_BLEND_FACTOR=15,
    D3D10_BLEND_SRC1_COLOR=16,
    D3D10_BLEND_INV_SRC1_COLOR=17,
    D3D10_BLEND_SRC1_ALPHA=18,
    D3D10_BLEND_INV_SRC1_ALPHA=19
  );
  PTD3D10_Blend=^TD3D10_Blend;
  D3D10_BLEND=TD3D10_Blend;
  PD3D10_BLEND=^TD3D10_Blend;

  TD3D10_BlendOp=
  (
    D3D10_BLEND_OP_ADD=1,
    D3D10_BLEND_OP_SUBTRACT=2,
    D3D10_BLEND_OP_REV_SUBTRACT=3,
    D3D10_BLEND_OP_MIN=4,
    D3D10_BLEND_OP_MAX=5
  );
  PTD3D10_BlendOp=^TD3D10_BlendOp;
  D3D10_BLEND_OP=TD3D10_BlendOp;
  PD3D10_BLEND_OP=^TD3D10_BlendOp;

  TD3D10_ColorWriteEnable=
  (
    D3D10_COLOR_WRITE_ENABLE_RED=1,
    D3D10_COLOR_WRITE_ENABLE_GREEN=2,
    D3D10_COLOR_WRITE_ENABLE_BLUE=4,
    D3D10_COLOR_WRITE_ENABLE_ALPHA=8,
    D3D10_COLOR_WRITE_ENABLE_ALL=15
  );
  PTD3D10_ColorWriteEnable=^TD3D10_ColorWriteEnable;
  D3D10_COLOR_WRITE_ENABLE=TD3D10_ColorWriteEnable;
  PD3D10_COLOR_WRITE_ENABLE=^TD3D10_ColorWriteEnable;

  TD3D10_BlendDesc=record
    AlphaToCoverageEnable:LongBool;
    BlendEnable:array[0..7] of LongBool;
    SrcBlend:TD3D10_Blend;
    DestBlend:TD3D10_Blend;
    BlendOp:TD3D10_BlendOp;
    SrcBlendAlpha:TD3D10_Blend;
    DestBlendAlpha:TD3D10_Blend;
    BlendOpAlpha:TD3D10_BlendOp;
    RenderTargetWriteMask:array[0..7] of Byte;
  end;
  PTD3D10_BlendDesc=^TD3D10_BlendDesc;
  D3D10_BLEND_DESC=TD3D10_BlendDesc;
  PD3D10_BLEND_DESC=^TD3D10_BlendDesc;

  ID3D10BlendState=interface(ID3D10DeviceChild)
    ['{EDAD8D19-8A35-4D6D-8566-2EA276CDE161}']
    procedure GetDesc
    (
      out Desc:TD3D10_BlendDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_RasterizerDesc=record
    FillMode:TD3D10_FillMode;
    CullMode:TD3D10_CullMode;
    FrontCounterClockwise:LongBool;
    DepthBias:Integer;
    DepthBiasClamp:Single;
    SlopeScaledDepthBias:Single;
    DepthClipEnable:LongBool;
    ScissorEnable:LongBool;
    MultisampleEnable:LongBool;
    AntialiasedLineEnable:LongBool;
  end;
  PTD3D10_RasterizerDesc=^TD3D10_RasterizerDesc;
  D3D10_RASTERIZER_DESC=TD3D10_RasterizerDesc;
  PD3D10_RASTERIZER_DESC=^TD3D10_RasterizerDesc;

  ID3D10RasterizerState=interface(ID3D10DeviceChild)
    ['{A2A07292-89AF-4345-BE2E-C53D9FBB6E9F}']
    procedure GetDesc
    (
      out Desc:TD3D10_RasterizerDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_SubresourceData=record
    pSysMem:Pointer;
    SysMemPitch:LongWord;
    SysMemSlicePitch:LongWord;
  end;
  PTD3D10_SubresourceData=^TD3D10_SubresourceData;
  D3D10_SUBRESOURCE_DATA=TD3D10_SubresourceData;
  PD3D10_SUBRESOURCE_DATA=^TD3D10_SubresourceData;

  ID3D10Resource=interface(ID3D10DeviceChild)
    ['{9B7E4C01-342C-4106-A19F-4F2704F689F0}']
    procedure GetType
    (
      out _Type:TD3D10_ResourceDimension (* __out *)
    ); stdcall;

    procedure SetEvictionPriority
    (
      EvictionPriority:LongWord (* __in *)
    ); stdcall;

    function GetEvictionPriority:LongWord; stdcall;
  end;

  TD3D10_BufferDesc=record
    ByteWidth:LongWord;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_BufferDesc=^TD3D10_BufferDesc;
  D3D10_BUFFER_DESC=TD3D10_BufferDesc;
  PD3D10_BUFFER_DESC=^TD3D10_BufferDesc;

  ID3D10Buffer=interface(ID3D10Resource)
    ['{9B7E4C02-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out pData:Pointer (* __out *)
    ):HResult; stdcall;

    procedure Unmap; stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_BufferDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Texture1DDesc=record
    Width:LongWord;
    MipLevels:LongWord;
    ArraySize:LongWord;
    Format:TDXGI_Format;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_Texture1DDesc=^TD3D10_Texture1DDesc;
  D3D10_TEXTURE1D_DESC=TD3D10_Texture1DDesc;
  PD3D10_TEXTURE1D_DESC=^TD3D10_Texture1DDesc;

  ID3D10Texture1D=interface(ID3D10Resource)
    ['{9B7E4C03-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      Subresource:LongWord; (* __in *)
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out pData:Pointer (* __out *)
    ):HResult; stdcall;

    procedure Unmap
    (
      Subresource:LongWord (* __in *)
    ); stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_Texture1DDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Texture2DDesc=record
    Width:LongWord;
    Height:LongWord;
    MipLevels:LongWord;
    ArraySize:LongWord;
    Format:TDXGI_Format;
    SampleDesc:TDXGI_SampleDesc;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_Texture2DDesc=^TD3D10_Texture2DDesc;
  D3D10_TEXTURE2D_DESC=TD3D10_Texture2DDesc;
  PD3D10_TEXTURE2D_DESC=^TD3D10_Texture2DDesc;

  TD3D10_MappedTexture2D=record
    pData:Pointer;
    RowPitch:LongWord;
  end;
  PTD3D10_MappedTexture2D=^TD3D10_MappedTexture2D;
  D3D10_MAPPED_TEXTURE2D=TD3D10_MappedTexture2D;
  PD3D10_MAPPED_TEXTURE2D=^TD3D10_MappedTexture2D;

  ID3D10Texture2D=interface(ID3D10Resource)
    ['{9B7E4C04-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      Subresource:LongWord; (* __in *)
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out MappedTex2D:TD3D10_MappedTexture2D (* __out *)
    ):HResult; stdcall;

    procedure Unmap
    (
      Subresource:LongWord (* __in *)
    ); stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_Texture2DDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Texture3DDesc=record
    Width:LongWord;
    Height:LongWord;
    Depth:LongWord;
    MipLevels:LongWord;
    Format:TDXGI_Format;
    Usage:TD3D10_Usage;
    BindFlags:LongWord;
    CPUAccessFlags:LongWord;
    MiscFlags:LongWord;
  end;
  PTD3D10_Texture3DDesc=^TD3D10_Texture3DDesc;
  D3D10_TEXTURE3D_DESC=TD3D10_Texture3DDesc;
  PD3D10_TEXTURE3D_DESC=^TD3D10_Texture3DDesc;

  TD3D10_MappedTexture3D=record
    pData:Pointer;
    RowPitch:LongWord;
    DepthPitch:LongWord;
  end;
  PTD3D10_MappedTexture3D=^TD3D10_MappedTexture3D;
  D3D10_MAPPED_TEXTURE3D=TD3D10_MappedTexture3D;
  PD3D10_MAPPED_TEXTURE3D=^TD3D10_MappedTexture3D;

  ID3D10Texture3D=interface(ID3D10Resource)
    ['{9B7E4C05-342C-4106-A19F-4F2704F689F0}']
    function Map
    (
      Subresource:LongWord; (* __in *)
      MapType:TD3D10_Map; (* __in *)
      MapFlags:LongWord; (* __in *)
      out MappedTex3D:TD3D10_MappedTexture3D (* __out *)
    ):HResult; stdcall;

    procedure Unmap
    (
      Subresource:LongWord (* __in *)
    ); stdcall;

    procedure GetDesc
    (
      out Desc:TD3D10_Texture3DDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_TexturecubeFace=
  (
    D3D10_TEXTURECUBE_FACE_POSITIVE_X=0,
    D3D10_TEXTURECUBE_FACE_NEGATIVE_X=1,
    D3D10_TEXTURECUBE_FACE_POSITIVE_Y=2,
    D3D10_TEXTURECUBE_FACE_NEGATIVE_Y=3,
    D3D10_TEXTURECUBE_FACE_POSITIVE_Z=4,
    D3D10_TEXTURECUBE_FACE_NEGATIVE_Z=5
  );
  PTD3D10_TexturecubeFace=^TD3D10_TexturecubeFace;
  D3D10_TEXTURECUBE_FACE=TD3D10_TexturecubeFace;
  PD3D10_TEXTURECUBE_FACE=^TD3D10_TexturecubeFace;

  ID3D10View=interface(ID3D10DeviceChild)
    ['{C902B03F-60A7-49BA-9936-2A3AB37A7E33}']
    procedure GetResource
    (
      out Resource:ID3D10Resource (* __out *)
    ); stdcall;
  end;

  TD3D10_BufferSrv=record
    case Byte of
      0:
      (
        FirstElement:UINT;
        NumElements:UINT;
      );

      1:
      (
        ElementOffset:UINT;
        ElementWidth:UINT;
      );
  end;
  PTD3D10_BufferSrv=^TD3D10_BufferSrv;
  D3D10_BUFFER_SRV=TD3D10_BufferSrv;
  PD3D10_BUFFER_SRV=^TD3D10_BufferSrv;

  TD3D10_Tex1DSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_Tex1DSrv=^TD3D10_Tex1DSrv;
  D3D10_TEX1D_SRV=TD3D10_Tex1DSrv;
  PD3D10_TEX1D_SRV=^TD3D10_Tex1DSrv;

  TD3D10_Tex1DArraySrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex1DArraySrv=^TD3D10_Tex1DArraySrv;
  D3D10_TEX1D_ARRAY_SRV=TD3D10_Tex1DArraySrv;
  PD3D10_TEX1D_ARRAY_SRV=^TD3D10_Tex1DArraySrv;

  TD3D10_Tex2DSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_Tex2DSrv=^TD3D10_Tex2DSrv;
  D3D10_TEX2D_SRV=TD3D10_Tex2DSrv;
  PD3D10_TEX2D_SRV=^TD3D10_Tex2DSrv;

  TD3D10_Tex2DArraySrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DArraySrv=^TD3D10_Tex2DArraySrv;
  D3D10_TEX2D_ARRAY_SRV=TD3D10_Tex2DArraySrv;
  PD3D10_TEX2D_ARRAY_SRV=^TD3D10_Tex2DArraySrv;

  TD3D10_Tex3DSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_Tex3DSrv=^TD3D10_Tex3DSrv;
  D3D10_TEX3D_SRV=TD3D10_Tex3DSrv;
  PD3D10_TEX3D_SRV=^TD3D10_Tex3DSrv;

  TD3D10_TexcubeSrv=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
  end;
  PTD3D10_TexcubeSrv=^TD3D10_TexcubeSrv;
  D3D10_TEXCUBE_SRV=TD3D10_TexcubeSrv;
  PD3D10_TEXCUBE_SRV=^TD3D10_TexcubeSrv;

  TD3D10_Tex2DmsSrv=record
    UnusedField_NothingToDefine:LongWord;
  end;
  PTD3D10_Tex2DmsSrv=^TD3D10_Tex2DmsSrv;
  D3D10_TEX2DMS_SRV=TD3D10_Tex2DmsSrv;
  PD3D10_TEX2DMS_SRV=^TD3D10_Tex2DmsSrv;

  TD3D10_Tex2DmsArraySrv=record
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DmsArraySrv=^TD3D10_Tex2DmsArraySrv;
  D3D10_TEX2DMS_ARRAY_SRV=TD3D10_Tex2DmsArraySrv;
  PD3D10_TEX2DMS_ARRAY_SRV=^TD3D10_Tex2DmsArraySrv;

  TD3D10_ShaderResourceViewDesc=record
    Format:TDXGI_Format;
    ViewDimension:TD3D10_SrvDimension;
    case Integer of
      0: (Buffer:D3D10_BUFFER_SRV);
      1: (Texture1D:D3D10_TEX1D_SRV);
      2: (Texture1DArray:D3D10_TEX1D_ARRAY_SRV);
      3: (Texture2D:D3D10_TEX2D_SRV);
      4: (Texture2DArray:D3D10_TEX2D_ARRAY_SRV);
      5: (Texture2DMS:D3D10_TEX2DMS_SRV);
      6: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_SRV);
      7: (Texture3D:D3D10_TEX3D_SRV);
      8: (TextureCube:D3D10_TEXCUBE_SRV);
  end;
  PTD3D10_ShaderResourceViewDesc=^TD3D10_ShaderResourceViewDesc;
  D3D10_SHADER_RESOURCE_VIEW_DESC=TD3D10_ShaderResourceViewDesc;
  PD3D10_SHADER_RESOURCE_VIEW_DESC=^TD3D10_ShaderResourceViewDesc;

  ID3D10ShaderResourceView=interface(ID3D10View)
    ['{9B7E4C07-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_ShaderResourceViewDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_BufferRtv=record
    case Byte of
      0:
      (
        FirstElement:UINT;
        NumElements:UINT;
      );

      1:
      (
        ElementOffset:UINT;
        ElementWidth:UINT;
      );
  end;
  PTD3D10_BufferRtv=^TD3D10_BufferRtv;
  D3D10_BUFFER_RTV=TD3D10_BufferRtv;
  PD3D10_BUFFER_RTV=^TD3D10_BufferRtv;

  TD3D10_Tex1DRtv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex1DRtv=^TD3D10_Tex1DRtv;
  D3D10_TEX1D_RTV=TD3D10_Tex1DRtv;
  PD3D10_TEX1D_RTV=^TD3D10_Tex1DRtv;

  TD3D10_Tex1DArrayRtv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex1DArrayRtv=^TD3D10_Tex1DArrayRtv;
  D3D10_TEX1D_ARRAY_RTV=TD3D10_Tex1DArrayRtv;
  PD3D10_TEX1D_ARRAY_RTV=^TD3D10_Tex1DArrayRtv;

  TD3D10_Tex2DRtv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex2DRtv=^TD3D10_Tex2DRtv;
  D3D10_TEX2D_RTV=TD3D10_Tex2DRtv;
  PD3D10_TEX2D_RTV=^TD3D10_Tex2DRtv;

  TD3D10_Tex2DmsRtv=record
    UnusedField_NothingToDefine:LongWord;
  end;
  PTD3D10_Tex2DmsRtv=^TD3D10_Tex2DmsRtv;
  D3D10_TEX2DMS_RTV=TD3D10_Tex2DmsRtv;
  PD3D10_TEX2DMS_RTV=^TD3D10_Tex2DmsRtv;

  TD3D10_Tex2DArrayRtv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DArrayRtv=^TD3D10_Tex2DArrayRtv;
  D3D10_TEX2D_ARRAY_RTV=TD3D10_Tex2DArrayRtv;
  PD3D10_TEX2D_ARRAY_RTV=^TD3D10_Tex2DArrayRtv;

  TD3D10_Tex2DmsArrayRtv=record
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DmsArrayRtv=^TD3D10_Tex2DmsArrayRtv;
  D3D10_TEX2DMS_ARRAY_RTV=TD3D10_Tex2DmsArrayRtv;
  PD3D10_TEX2DMS_ARRAY_RTV=^TD3D10_Tex2DmsArrayRtv;

  TD3D10_Tex3DRtv=record
    MipSlice:LongWord;
    FirstWSlice:LongWord;
    WSize:LongWord;
  end;
  PTD3D10_Tex3DRtv=^TD3D10_Tex3DRtv;
  D3D10_TEX3D_RTV=TD3D10_Tex3DRtv;
  PD3D10_TEX3D_RTV=^TD3D10_Tex3DRtv;

  TD3D10_RenderTargetViewDesc=record
    Format:TDXGI_Format;
    ViewDimension:TD3D10_RtvDimension;
    case Integer of
      0: (Buffer:D3D10_BUFFER_RTV);
      1: (Texture1D:D3D10_TEX1D_RTV);
      2: (Texture1DArray:D3D10_TEX1D_ARRAY_RTV);
      3: (Texture2D:D3D10_TEX2D_RTV);
      4: (Texture2DArray:D3D10_TEX2D_ARRAY_RTV);
      5: (Texture2DMS:D3D10_TEX2DMS_RTV);
      6: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_RTV);
      7: (Texture3D:D3D10_TEX3D_RTV);
  end;
  PTD3D10_RenderTargetViewDesc=^TD3D10_RenderTargetViewDesc;
  D3D10_RENDER_TARGET_VIEW_DESC=TD3D10_RenderTargetViewDesc;
  PD3D10_RENDER_TARGET_VIEW_DESC=^TD3D10_RenderTargetViewDesc;

  ID3D10RenderTargetView=interface(ID3D10View)
    ['{9B7E4C08-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_RenderTargetViewDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_Tex1DDsv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex1DDsv=^TD3D10_Tex1DDsv;
  D3D10_TEX1D_DSV=TD3D10_Tex1DDsv;
  PD3D10_TEX1D_DSV=^TD3D10_Tex1DDsv;

  TD3D10_Tex1DArrayDsv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex1DArrayDsv=^TD3D10_Tex1DArrayDsv;
  D3D10_TEX1D_ARRAY_DSV=TD3D10_Tex1DArrayDsv;
  PD3D10_TEX1D_ARRAY_DSV=^TD3D10_Tex1DArrayDsv;

  TD3D10_Tex2DDsv=record
    MipSlice:LongWord;
  end;
  PTD3D10_Tex2DDsv=^TD3D10_Tex2DDsv;
  D3D10_TEX2D_DSV=TD3D10_Tex2DDsv;
  PD3D10_TEX2D_DSV=^TD3D10_Tex2DDsv;

  TD3D10_Tex2DArrayDsv=record
    MipSlice:LongWord;
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DArrayDsv=^TD3D10_Tex2DArrayDsv;
  D3D10_TEX2D_ARRAY_DSV=TD3D10_Tex2DArrayDsv;
  PD3D10_TEX2D_ARRAY_DSV=^TD3D10_Tex2DArrayDsv;

  TD3D10_Tex2DmsDsv=record
    UnusedField_NothingToDefine:LongWord;
  end;
  PTD3D10_Tex2DmsDsv=^TD3D10_Tex2DmsDsv;
  D3D10_TEX2DMS_DSV=TD3D10_Tex2DmsDsv;
  PD3D10_TEX2DMS_DSV=^TD3D10_Tex2DmsDsv;

  TD3D10_Tex2DmsArrayDsv=record
    FirstArraySlice:LongWord;
    ArraySize:LongWord;
  end;
  PTD3D10_Tex2DmsArrayDsv=^TD3D10_Tex2DmsArrayDsv;
  D3D10_TEX2DMS_ARRAY_DSV=TD3D10_Tex2DmsArrayDsv;
  PD3D10_TEX2DMS_ARRAY_DSV=^TD3D10_Tex2DmsArrayDsv;

  TD3D10_DepthStencilViewDesc=record
    Format:TDXGI_Format;
    ViewDimension:TD3D10_DsvDimension;
    case Integer of
      0: (Texture1D:D3D10_TEX1D_DSV);
      1: (Texture1DArray:D3D10_TEX1D_ARRAY_DSV);
      2: (Texture2D:D3D10_TEX2D_DSV);
      3: (Texture2DArray:D3D10_TEX2D_ARRAY_DSV);
      4: (Texture2DMS:D3D10_TEX2DMS_DSV);
      5: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_DSV);
  end;
  PTD3D10_DepthStencilViewDesc=^TD3D10_DepthStencilViewDesc;
  D3D10_DEPTH_STENCIL_VIEW_DESC=TD3D10_DepthStencilViewDesc;
  PD3D10_DEPTH_STENCIL_VIEW_DESC=^TD3D10_DepthStencilViewDesc;

  ID3D10DepthStencilView=interface(ID3D10View)
    ['{9B7E4C09-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_DepthStencilViewDesc (* __out *)
    ); stdcall;
  end;

  ID3D10VertexShader=interface(ID3D10DeviceChild)
    ['{9B7E4C0A-342C-4106-A19F-4F2704F689F0}']
  end;

  ID3D10GeometryShader=interface(ID3D10DeviceChild)
    ['{6316BE88-54CD-4040-AB44-20461BC81F68}']
  end;

  ID3D10PixelShader=interface(ID3D10DeviceChild)
    ['{4968B601-9D00-4CDE-8346-8E7F675819B6}']
  end;

  ID3D10InputLayout=interface(ID3D10DeviceChild)
    ['{9B7E4C0B-342C-4106-A19F-4F2704F689F0}']
  end;

  TD3D10_Filter=
  (
    D3D10_FILTER_MIN_MAG_MIP_POINT=0,
    D3D10_FILTER_MIN_MAG_POINT_MIP_LINEAR=$1,
    D3D10_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT=$4,
    D3D10_FILTER_MIN_POINT_MAG_MIP_LINEAR=$5,
    D3D10_FILTER_MIN_LINEAR_MAG_MIP_POINT=$10,
    D3D10_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR=$11,
    D3D10_FILTER_MIN_MAG_LINEAR_MIP_POINT=$14,
    D3D10_FILTER_MIN_MAG_MIP_LINEAR=$15,
    D3D10_FILTER_ANISOTROPIC=$55,
    D3D10_FILTER_COMPARISON_MIN_MAG_MIP_POINT=$80,
    D3D10_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR=$81,
    D3D10_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT=$84,
    D3D10_FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR=$85,
    D3D10_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT=$90,
    D3D10_FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR=$91,
    D3D10_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT=$94,
    D3D10_FILTER_COMPARISON_MIN_MAG_MIP_LINEAR=$95,
    D3D10_FILTER_COMPARISON_ANISOTROPIC=$d5,
    D3D10_FILTER_TEXT_1BIT=Integer($80000000)
  );
  PTD3D10_Filter=^TD3D10_Filter;
  D3D10_FILTER=TD3D10_Filter;
  PD3D10_FILTER=^TD3D10_Filter;

  TD3D10_FilterType=
  (
    D3D10_FILTER_TYPE_POINT=0,
    D3D10_FILTER_TYPE_LINEAR=1
  );
  PTD3D10_FilterType=^TD3D10_FilterType;
  D3D10_FILTER_TYPE=TD3D10_FilterType;
  PD3D10_FILTER_TYPE=^TD3D10_FilterType;

  TD3D10_TextureAddressMode=
  (
    D3D10_TEXTURE_ADDRESS_WRAP=1,
    D3D10_TEXTURE_ADDRESS_MIRROR=2,
    D3D10_TEXTURE_ADDRESS_CLAMP=3,
    D3D10_TEXTURE_ADDRESS_BORDER=4,
    D3D10_TEXTURE_ADDRESS_MIRROR_ONCE=5
  );
  PTD3D10_TextureAddressMode=^TD3D10_TextureAddressMode;
  D3D10_TEXTURE_ADDRESS_MODE=TD3D10_TextureAddressMode;
  PD3D10_TEXTURE_ADDRESS_MODE=^TD3D10_TextureAddressMode;

  TD3D10_SamplerDesc=record
    Filter:TD3D10_Filter;
    AddressU:TD3D10_TextureAddressMode;
    AddressV:TD3D10_TextureAddressMode;
    AddressW:TD3D10_TextureAddressMode;
    MipLODBias:Single;
    MaxAnisotropy:LongWord;
    ComparisonFunc:TD3D10_ComparisonFunc;
    BorderColor:array[0..3] of Single;
    MinLOD:Single;
    MaxLOD:Single;
  end;
  PTD3D10_SamplerDesc=^TD3D10_SamplerDesc;
  D3D10_SAMPLER_DESC=TD3D10_SamplerDesc;
  PD3D10_SAMPLER_DESC=^TD3D10_SamplerDesc;

  ID3D10SamplerState=interface(ID3D10DeviceChild)
    ['{9B7E4C0C-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_SamplerDesc (* __out *)
    ); stdcall;
  end;

  TD3D10_FormatSupport=
  (
    D3D10_FORMAT_SUPPORT_BUFFER=$1,
    D3D10_FORMAT_SUPPORT_IA_VERTEX_BUFFER=$2,
    D3D10_FORMAT_SUPPORT_IA_INDEX_BUFFER=$4,
    D3D10_FORMAT_SUPPORT_SO_BUFFER=$8,
    D3D10_FORMAT_SUPPORT_TEXTURE1D=$10,
    D3D10_FORMAT_SUPPORT_TEXTURE2D=$20,
    D3D10_FORMAT_SUPPORT_TEXTURE3D=$40,
    D3D10_FORMAT_SUPPORT_TEXTURECUBE=$80,
    D3D10_FORMAT_SUPPORT_SHADER_LOAD=$100,
    D3D10_FORMAT_SUPPORT_SHADER_SAMPLE=$200,
    D3D10_FORMAT_SUPPORT_SHADER_SAMPLE_COMPARISON=$400,
    D3D10_FORMAT_SUPPORT_SHADER_SAMPLE_MONO_TEXT=$800,
    D3D10_FORMAT_SUPPORT_MIP=$1000,
    D3D10_FORMAT_SUPPORT_MIP_AUTOGEN=$2000,
    D3D10_FORMAT_SUPPORT_RENDER_TARGET=$4000,
    D3D10_FORMAT_SUPPORT_BLENDABLE=$8000,
    D3D10_FORMAT_SUPPORT_DEPTH_STENCIL=$10000,
    D3D10_FORMAT_SUPPORT_CPU_LOCKABLE=$20000,
    D3D10_FORMAT_SUPPORT_MULTISAMPLE_RESOLVE=$40000,
    D3D10_FORMAT_SUPPORT_DISPLAY=$80000,
    D3D10_FORMAT_SUPPORT_CAST_WITHIN_BIT_LAYOUT=$100000,
    D3D10_FORMAT_SUPPORT_MULTISAMPLE_RENDERTARGET=$200000,
    D3D10_FORMAT_SUPPORT_MULTISAMPLE_LOAD=$400000,
    D3D10_FORMAT_SUPPORT_SHADER_GATHER=$800000,
    D3D10_FORMAT_SUPPORT_BACK_BUFFER_CAST=$1000000
  );
  PTD3D10_FormatSupport=^TD3D10_FormatSupport;
  D3D10_FORMAT_SUPPORT=TD3D10_FormatSupport;
  PD3D10_FORMAT_SUPPORT=^TD3D10_FormatSupport;

  ID3D10Asynchronous=interface(ID3D10DeviceChild)
    ['{9B7E4C0D-342C-4106-A19F-4F2704F689F0}']
    procedure _Begin; stdcall;

    procedure _End; stdcall;

    function GetData
    (
      pData:Pointer; (* __out_bcount_opt(DataSize) *)
      DataSize:LongWord; (* __in *)
      GetDataFlags:LongWord (* __in *)
    ):HResult; stdcall;

    function GetDataSize:LongWord; stdcall;
  end;

  TD3D10_AsyncGetdataFlag=
  (
    D3D10_ASYNC_GETDATA_DONOTFLUSH=$1
  );
  PTD3D10_AsyncGetdataFlag=^TD3D10_AsyncGetdataFlag;
  D3D10_ASYNC_GETDATA_FLAG=TD3D10_AsyncGetdataFlag;
  PD3D10_ASYNC_GETDATA_FLAG=^TD3D10_AsyncGetdataFlag;

  TD3D10_Query=
  (
    D3D10_QUERY_EVENT=0,
    D3D10_QUERY_OCCLUSION=D3D10_QUERY_EVENT + 1,
    D3D10_QUERY_TIMESTAMP=D3D10_QUERY_OCCLUSION + 1,
    D3D10_QUERY_TIMESTAMP_DISJOINT=D3D10_QUERY_TIMESTAMP + 1,
    D3D10_QUERY_PIPELINE_STATISTICS=D3D10_QUERY_TIMESTAMP_DISJOINT + 1,
    D3D10_QUERY_OCCLUSION_PREDICATE=D3D10_QUERY_PIPELINE_STATISTICS + 1,
    D3D10_QUERY_SO_STATISTICS=D3D10_QUERY_OCCLUSION_PREDICATE + 1,
    D3D10_QUERY_SO_OVERFLOW_PREDICATE=D3D10_QUERY_SO_STATISTICS + 1
  );
  PTD3D10_Query=^TD3D10_Query;
  D3D10_QUERY=TD3D10_Query;
  PD3D10_QUERY=^TD3D10_Query;

  TD3D10_QueryMiscFlag=
  (
    D3D10_QUERY_MISC_PREDICATEHINT=$1
  );
  PTD3D10_QueryMiscFlag=^TD3D10_QueryMiscFlag;
  D3D10_QUERY_MISC_FLAG=TD3D10_QueryMiscFlag;
  PD3D10_QUERY_MISC_FLAG=^TD3D10_QueryMiscFlag;

  TD3D10_QueryDesc=record
    Query:TD3D10_Query;
    MiscFlags:LongWord;
  end;
  PTD3D10_QueryDesc=^TD3D10_QueryDesc;
  D3D10_QUERY_DESC=TD3D10_QueryDesc;
  PD3D10_QUERY_DESC=^TD3D10_QueryDesc;

  ID3D10Query=interface(ID3D10Asynchronous)
    ['{9B7E4C0E-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_QueryDesc (* __out *)
    ); stdcall;
  end;

  ID3D10Predicate=interface(ID3D10Query)
    ['{9B7E4C10-342C-4106-A19F-4F2704F689F0}']
  end;

  TD3D10_QueryDataTimestampDisjoint=record
    Frequency:LargeInt;
    Disjoint:LongBool;
  end;
  PTD3D10_QueryDataTimestampDisjoint=^TD3D10_QueryDataTimestampDisjoint;
  D3D10_QUERY_DATA_TIMESTAMP_DISJOINT=TD3D10_QueryDataTimestampDisjoint;
  PD3D10_QUERY_DATA_TIMESTAMP_DISJOINT=^TD3D10_QueryDataTimestampDisjoint;

  TD3D10_QueryDataPipelineStatistics=record
    IAVertices:LargeInt;
    IAPrimitives:LargeInt;
    VSInvocations:LargeInt;
    GSInvocations:LargeInt;
    GSPrimitives:LargeInt;
    CInvocations:LargeInt;
    CPrimitives:LargeInt;
    PSInvocations:LargeInt;
  end;
  PTD3D10_QueryDataPipelineStatistics=^TD3D10_QueryDataPipelineStatistics;
  D3D10_QUERY_DATA_PIPELINE_STATISTICS=TD3D10_QueryDataPipelineStatistics;
  PD3D10_QUERY_DATA_PIPELINE_STATISTICS=^TD3D10_QueryDataPipelineStatistics;

  TD3D10_QueryDataSoStatistics=record
    NumPrimitivesWritten:LargeInt;
    PrimitivesStorageNeeded:LargeInt;
  end;
  PTD3D10_QueryDataSoStatistics=^TD3D10_QueryDataSoStatistics;
  D3D10_QUERY_DATA_SO_STATISTICS=TD3D10_QueryDataSoStatistics;
  PD3D10_QUERY_DATA_SO_STATISTICS=^TD3D10_QueryDataSoStatistics;

  TD3D10_Counter=
  (
    D3D10_COUNTER_GPU_IDLE=0,
    D3D10_COUNTER_VERTEX_PROCESSING=D3D10_COUNTER_GPU_IDLE + 1,
    D3D10_COUNTER_GEOMETRY_PROCESSING=D3D10_COUNTER_VERTEX_PROCESSING + 1,
    D3D10_COUNTER_PIXEL_PROCESSING=D3D10_COUNTER_GEOMETRY_PROCESSING + 1,
    D3D10_COUNTER_OTHER_GPU_PROCESSING=D3D10_COUNTER_PIXEL_PROCESSING + 1,
    D3D10_COUNTER_HOST_ADAPTER_BANDWIDTH_UTILIZATION=D3D10_COUNTER_OTHER_GPU_PROCESSING + 1,
    D3D10_COUNTER_LOCAL_VIDMEM_BANDWIDTH_UTILIZATION=D3D10_COUNTER_HOST_ADAPTER_BANDWIDTH_UTILIZATION + 1,
    D3D10_COUNTER_VERTEX_THROUGHPUT_UTILIZATION=D3D10_COUNTER_LOCAL_VIDMEM_BANDWIDTH_UTILIZATION + 1,
    D3D10_COUNTER_TRIANGLE_SETUP_THROUGHPUT_UTILIZATION=D3D10_COUNTER_VERTEX_THROUGHPUT_UTILIZATION + 1,
    D3D10_COUNTER_FILLRATE_THROUGHPUT_UTILIZATION=D3D10_COUNTER_TRIANGLE_SETUP_THROUGHPUT_UTILIZATION + 1,
    D3D10_COUNTER_VS_MEMORY_LIMITED=D3D10_COUNTER_FILLRATE_THROUGHPUT_UTILIZATION + 1,
    D3D10_COUNTER_VS_COMPUTATION_LIMITED=D3D10_COUNTER_VS_MEMORY_LIMITED + 1,
    D3D10_COUNTER_GS_MEMORY_LIMITED=D3D10_COUNTER_VS_COMPUTATION_LIMITED + 1,
    D3D10_COUNTER_GS_COMPUTATION_LIMITED=D3D10_COUNTER_GS_MEMORY_LIMITED + 1,
    D3D10_COUNTER_PS_MEMORY_LIMITED=D3D10_COUNTER_GS_COMPUTATION_LIMITED + 1,
    D3D10_COUNTER_PS_COMPUTATION_LIMITED=D3D10_COUNTER_PS_MEMORY_LIMITED + 1,
    D3D10_COUNTER_POST_TRANSFORM_CACHE_HIT_RATE=D3D10_COUNTER_PS_COMPUTATION_LIMITED + 1,
    D3D10_COUNTER_TEXTURE_CACHE_HIT_RATE=D3D10_COUNTER_POST_TRANSFORM_CACHE_HIT_RATE + 1,
    D3D10_COUNTER_DEVICE_DEPENDENT_0=$40000000
  );
  PTD3D10_Counter=^TD3D10_Counter;
  D3D10_COUNTER=TD3D10_Counter;
  PD3D10_COUNTER=^TD3D10_Counter;

  TD3D10_CounterType=
  (
    D3D10_COUNTER_TYPE_FLOAT32=0,
    D3D10_COUNTER_TYPE_UINT16=D3D10_COUNTER_TYPE_FLOAT32 + 1,
    D3D10_COUNTER_TYPE_UINT32=D3D10_COUNTER_TYPE_UINT16 + 1,
    D3D10_COUNTER_TYPE_UINT64=D3D10_COUNTER_TYPE_UINT32 + 1
  );
  PTD3D10_CounterType=^TD3D10_CounterType;
  D3D10_COUNTER_TYPE=TD3D10_CounterType;
  PD3D10_COUNTER_TYPE=^TD3D10_CounterType;

  TD3D10_CounterDesc=record
    Counter:TD3D10_Counter;
    MiscFlags:LongWord;
  end;
  PTD3D10_CounterDesc=^TD3D10_CounterDesc;
  D3D10_COUNTER_DESC=TD3D10_CounterDesc;
  PD3D10_COUNTER_DESC=^TD3D10_CounterDesc;

  TD3D10_CounterInfo=record
    LastDeviceDependentCounter:TD3D10_Counter;
    NumSimultaneousCounters:LongWord;
    NumDetectableParallelUnits:Byte;
  end;
  PTD3D10_CounterInfo=^TD3D10_CounterInfo;
  D3D10_COUNTER_INFO=TD3D10_CounterInfo;
  PD3D10_COUNTER_INFO=^TD3D10_CounterInfo;

  ID3D10Counter=interface(ID3D10Asynchronous)
    ['{9B7E4C11-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc
    (
      out Desc:TD3D10_CounterDesc (* __out *)
    ); stdcall;
  end;

  ID3D10Device=interface(IUnknown)
    ['{9B7E4C0F-342C-4106-A19F-4F2704F689F0}']
    procedure VSSetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure PSSetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __in_ecount(NumViews) *)
    ); stdcall;

    procedure PSSetShader
    (
      PixelShader:ID3D10PixelShader (* __in_opt *)
    ); stdcall;

    procedure PSSetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __in_ecount(NumSamplers) *)
    ); stdcall;

    procedure VSSetShader
    (
      VertexShader:ID3D10VertexShader (* __in_opt *)
    ); stdcall;

    procedure DrawIndexed
    (
      IndexCount:LongWord; (* __in *)
      StartIndexLocation:LongWord; (* __in *)
      BaseVertexLocation:Integer (* __in *)
    ); stdcall;

    procedure Draw
    (
      VertexCount:LongWord; (* __in *)
      StartVertexLocation:LongWord (* __in *)
    ); stdcall;

    procedure PSSetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure IASetInputLayout
    (
      InputLayout:ID3D10InputLayout (* __in_opt *)
    ); stdcall;

    procedure IASetVertexBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pVertexBuffers:PID3D10Buffer; (* __in_ecount(NumBuffers) *)
      pStrides:PLongWord; (* __in_ecount(NumBuffers) *)
      pOffsets:PLongWord (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure IASetIndexBuffer
    (
      IndexBuffer:ID3D10Buffer; (* __in_opt *)
      Format:TDXGI_Format; (* __in *)
      Offset:LongWord (* __in *)
    ); stdcall;

    procedure DrawIndexedInstanced
    (
      IndexCountPerInstance:LongWord; (* __in *)
      InstanceCount:LongWord; (* __in *)
      StartIndexLocation:LongWord; (* __in *)
      BaseVertexLocation:Integer; (* __in *)
      StartInstanceLocation:LongWord (* __in *)
    ); stdcall;

    procedure DrawInstanced
    (
      VertexCountPerInstance:LongWord; (* __in *)
      InstanceCount:LongWord; (* __in *)
      StartVertexLocation:LongWord; (* __in *)
      StartInstanceLocation:LongWord (* __in *)
    ); stdcall;

    procedure GSSetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __in_ecount(NumBuffers) *)
    ); stdcall;

    procedure GSSetShader
    (
      Shader:ID3D10GeometryShader (* __in_opt *)
    ); stdcall;

    procedure IASetPrimitiveTopology
    (
      Topology:TD3D10_PrimitiveTopology (* __in *)
    ); stdcall;

    procedure VSSetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __in_ecount(NumViews) *)
    ); stdcall;

    procedure VSSetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __in_ecount(NumSamplers) *)
    ); stdcall;

    procedure SetPredication
    (
      Predicate:ID3D10Predicate; (* __in_opt *)
      PredicateValue:LongBool (* __in *)
    ); stdcall;

    procedure GSSetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __in_ecount(NumViews) *)
    ); stdcall;

    procedure GSSetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __in_ecount(NumSamplers) *)
    ); stdcall;

    procedure OMSetRenderTargets
    (
      NumViews:LongWord; (* __in_range( 0, D3D10_SIMULTANEOUS_RENDER_TARGET_COUNT ) *)
      pRenderTargetViews:PID3D10RenderTargetView; (* __in_ecount_opt(NumViews) *)
      DepthStencilView:ID3D10DepthStencilView (* __in_opt *)
    ); stdcall;

    procedure OMSetBlendState
    (
      BlendState:ID3D10BlendState; (* __in_opt *)
      const BlendFactor:TdxColorArray; (* __in *)
      SampleMask:LongWord (* __in *)
    ); stdcall;

    procedure OMSetDepthStencilState
    (
      DepthStencilState:ID3D10DepthStencilState; (* __in_opt *)
      StencilRef:LongWord (* __in *)
    ); stdcall;

    procedure SOSetTargets
    (
      NumBuffers:LongWord; (* __in_range( 0, D3D10_SO_BUFFER_SLOT_COUNT) *)
      pSOTargets:PID3D10Buffer; (* __in_ecount_opt(NumBuffers) *)
      pOffsets:PLongWord (* __in_ecount_opt(NumBuffers) *)
    ); stdcall;

    procedure DrawAuto; stdcall;

    procedure RSSetState
    (
      RasterizerState:ID3D10RasterizerState (* __in_opt *)
    ); stdcall;

    procedure RSSetViewports
    (
      NumViewports:LongWord; (* __in_range(0, D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE) *)
      pViewports:PTD3D10_Viewport (* __in_ecount_opt(NumViewports) *)
    ); stdcall;

    procedure RSSetScissorRects
    (
      NumRects:LongWord; (* __in_range(0, D3D10_VIEWPORT_AND_SCISSORRECT_OBJECT_COUNT_PER_PIPELINE) *)
      pRects:PTD3D10_Rect (* __in_ecount_opt(NumRects) *)
    ); stdcall;

    procedure CopySubresourceRegion
    (
      DstResource:ID3D10Resource; (* __in *)
      DstSubresource:LongWord; (* __in *)
      DstX:LongWord; (* __in *)
      DstY:LongWord; (* __in *)
      DstZ:LongWord; (* __in *)
      SrcResource:ID3D10Resource; (* __in *)
      SrcSubresource:LongWord; (* __in *)
      pSrcBox:PTD3D10_Box (* __in_opt *)
    ); stdcall;

    procedure CopyResource
    (
      DstResource:ID3D10Resource; (* __in *)
      SrcResource:ID3D10Resource (* __in *)
    ); stdcall;

    procedure UpdateSubresource
    (
      DstResource:ID3D10Resource; (* __in *)
      DstSubresource:LongWord; (* __in *)
      pDstBox:PTD3D10_Box; (* __in_opt *)
      pSrcData:Pointer; (* __in *)
      SrcRowPitch:LongWord; (* __in *)
      SrcDepthPitch:LongWord (* __in *)
    ); stdcall;

    procedure ClearRenderTargetView
    (
      RenderTargetView:ID3D10RenderTargetView; (* __in *)
      const ColorRGBA:TdxColorArray (* __in *)
    ); stdcall;

    procedure ClearDepthStencilView
    (
      DepthStencilView:ID3D10DepthStencilView; (* __in *)
      ClearFlags:LongWord; (* __in *)
      Depth:Single; (* __in *)
      Stencil:Byte (* __in *)
    ); stdcall;

    procedure GenerateMips
    (
      ShaderResourceView:ID3D10ShaderResourceView (* __in *)
    ); stdcall;

    procedure ResolveSubresource
    (
      DstResource:ID3D10Resource; (* __in *)
      DstSubresource:LongWord; (* __in *)
      SrcResource:ID3D10Resource; (* __in *)
      SrcSubresource:LongWord; (* __in *)
      Format:TDXGI_Format (* __in *)
    ); stdcall;

    procedure VSGetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __out_ecount(NumBuffers) *)
    ); stdcall;

    procedure PSGetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __out_ecount(NumViews) *)
    ); stdcall;

    procedure PSGetShader
    (
      out PixelShader:ID3D10PixelShader (* __out *)
    ); stdcall;

    procedure PSGetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __out_ecount(NumSamplers) *)
    ); stdcall;

    procedure VSGetShader
    (
      out VertexShader:ID3D10VertexShader (* __out *)
    ); stdcall;

    procedure PSGetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __out_ecount(NumBuffers) *)
    ); stdcall;

    procedure IAGetInputLayout
    (
      out InputLayout:ID3D10InputLayout (* __out *)
    ); stdcall;

    procedure IAGetVertexBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pVertexBuffers:PID3D10Buffer; (* __out_ecount_opt(NumBuffers) *)
      pStrides:PLongWord; (* __out_ecount_opt(NumBuffers) *)
      pOffsets:PLongWord (* __out_ecount_opt(NumBuffers) *)
    ); stdcall;

    procedure IAGetIndexBuffer
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pIndexBuffer:PID3D10Buffer{$ELSE}out pIndexBuffer:ID3D10Buffer{$ENDIF}; (* __out_opt *)
      Format:PTDXGI_Format; (* __out_opt *)
      Offset:PLongWord (* __out_opt *)
    ); stdcall;

    procedure GSGetConstantBuffers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - 1 ) *)
      NumBuffers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT - StartSlot ) *)
      pConstantBuffers:PID3D10Buffer (* __out_ecount(NumBuffers) *)
    ); stdcall;

    procedure GSGetShader
    (
      out GeometryShader:ID3D10GeometryShader (* __out *)
    ); stdcall;

    procedure IAGetPrimitiveTopology
    (
      out Topology:TD3D10_PrimitiveTopology (* __out *)
    ); stdcall;

    procedure VSGetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __out_ecount(NumViews) *)
    ); stdcall;

    procedure VSGetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __out_ecount(NumSamplers) *)
    ); stdcall;

    procedure GetPredication
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pPredicate:PID3D10Predicate{$ELSE}out Predicate:ID3D10Predicate{$ENDIF}; (* __out_opt *)
      pPredicateValue:PLongBool (* __out_opt *)
    ); stdcall;

    procedure GSGetShaderResources
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - 1 ) *)
      NumViews:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT - StartSlot ) *)
      pShaderResourceViews:PID3D10ShaderResourceView (* __out_ecount(NumViews) *)
    ); stdcall;

    procedure GSGetSamplers
    (
      StartSlot:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - 1 ) *)
      NumSamplers:LongWord; (* __in_range( 0, D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT - StartSlot ) *)
      pSamplers:PID3D10SamplerState (* __out_ecount(NumSamplers) *)
    ); stdcall;

    procedure OMGetRenderTargets
    (
      NumViews:LongWord; (* __in_range( 0, D3D10_SIMULTANEOUS_RENDER_TARGET_COUNT ) *)
      pRenderTargetViews:PID3D10RenderTargetView; (* __out_ecount_opt(NumViews) *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilView:PID3D10DepthStencilView{$ELSE}out DepthStencilView:ID3D10DepthStencilView{$ENDIF} (* __out_opt *)
    ); stdcall;

    procedure OMGetBlendState
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBlendState:PID3D10BlendState{$ELSE}out BlendState:ID3D10BlendState{$ENDIF}; (* __out_opt *)
      const BlendFactor:TdxColorArray; (* __out_opt *)
      pSampleMask:PLongWord (* __out_opt *)
    ); stdcall;

    procedure OMGetDepthStencilState
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilState:PID3D10DepthStencilState{$ELSE}out DepthStencilState:ID3D10DepthStencilState{$ENDIF}; (* __out_opt *)
      pStencilRef:PLongWord (* __out_opt *)
    ); stdcall;

    procedure SOGetTargets
    (
      NumBuffers:LongWord; (* __in_range( 0, D3D10_SO_BUFFER_SLOT_COUNT ) *)
      pSOTargets:PID3D10Buffer; (* __out_ecount_opt(NumBuffers) *)
      pOffsets:PLongWord (* __out_ecount_opt(NumBuffers) *)
    ); stdcall;

    procedure RSGetState
    (
      out RasterizerState:ID3D10RasterizerState (* __out *)
    ); stdcall;

    procedure RSGetViewports
    (
      var NumViewports:LongWord; (* __inout *)
      pViewports:PTD3D10_Viewport (* __out_ecount_opt(*NumViewports) *)
    ); stdcall;

    procedure RSGetScissorRects
    (
      var NumRects:LongWord; (* __inout *)
      pRects:PTD3D10_Rect (* __out_ecount_opt(*NumRects) *)
    ); stdcall;

    function GetDeviceRemovedReason:HResult; stdcall;

    function SetExceptionMode
    (
      RaiseFlags:LongWord
    ):HResult; stdcall;

    function GetExceptionMode:LongWord; stdcall;

    function GetPrivateData
    (
      const Guid:TGUID; (* __in *)
      var DataSize:LongWord; (* __inout *)
      pData:Pointer (* __out_bcount_opt(*pDataSize) *)
    ):HResult; stdcall;

    function SetPrivateData
    (
      const Guid:TGUID; (* __in *)
      DataSize:LongWord; (* __in *)
      pData:Pointer (* __in_bcount_opt(DataSize) *)
    ):HResult; stdcall;

    function SetPrivateDataInterface
    (
      const Guid:TGUID; (* __in *)
      Data:IUnknown (* __in_opt *)
    ):HResult; stdcall;

    procedure ClearState; stdcall;

    procedure Flush; stdcall;

    function CreateBuffer
    (
      const Desc:TD3D10_BufferDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBuffer:PID3D10Buffer{$ELSE}out Buffer:ID3D10Buffer{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateTexture1D
    (
      const Desc:TD3D10_Texture1DDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_xcount_opt(pDesc->MipLevels * pDesc->ArraySize) *)
      out Texture1D:ID3D10Texture1D (* __out *)
    ):HResult; stdcall;

    function CreateTexture2D
    (
      const Desc:TD3D10_Texture2DDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_xcount_opt(pDesc->MipLevels * pDesc->ArraySize) *)
      out Texture2D:ID3D10Texture2D (* __out *)
    ):HResult; stdcall;

    function CreateTexture3D
    (
      const Desc:TD3D10_Texture3DDesc; (* __in *)
      pInitialData:PTD3D10_SubresourceData; (* __in_xcount_opt(pDesc->MipLevels) *)
      out Texture3D:ID3D10Texture3D (* __out *)
    ):HResult; stdcall;

    function CreateShaderResourceView
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_ShaderResourceViewDesc; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pSRView:PID3D10ShaderResourceView{$ELSE}out SRView:ID3D10ShaderResourceView{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateRenderTargetView
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_RenderTargetViewDesc; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pRTView:PID3D10RenderTargetView{$ELSE}out RTView:ID3D10RenderTargetView{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateDepthStencilView
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_DepthStencilViewDesc; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilView:PID3D10DepthStencilView{$ELSE}out DepthStencilView:ID3D10DepthStencilView{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateInputLayout
    (
      pInputElementDescs:PTD3D10_InputElementDesc; (* __in_ecount(NumElements) *)
      NumElements:LongWord; (* __in_range( 0, D3D10_1_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT ) *)
      pShaderBytecodeWithInputSignature:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pInputLayout:PID3D10InputLayout{$ELSE}out InputLayout:ID3D10InputLayout{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateVertexShader
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pVertexShader:PID3D10VertexShader{$ELSE}out VertexShader:ID3D10VertexShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateGeometryShader
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pGeometryShader:PID3D10GeometryShader{$ELSE}out GeometryShader:ID3D10GeometryShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateGeometryShaderWithStreamOutput
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      pSODeclaration:PTD3D10_SoDeclarationEntry; (* __in_ecount_opt(NumEntries) *)
      NumEntries:LongWord; (* __in_range( 0, D3D10_SO_SINGLE_BUFFER_COMPONENT_LIMIT ) *)
      OutputStreamStride:LongWord; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pGeometryShader:PID3D10GeometryShader{$ELSE}out GeometryShader:ID3D10GeometryShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreatePixelShader
    (
      pShaderBytecode:Pointer; (* __in *)
      BytecodeLength:SIZE_T; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pPixelShader:PID3D10PixelShader{$ELSE}out PixelShader:ID3D10PixelShader{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateBlendState
    (
      const BlendStateDesc:TD3D10_BlendDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBlendState:PID3D10BlendState{$ELSE}out BlendState:ID3D10BlendState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateDepthStencilState
    (
      const DepthStencilDesc:TD3D10_DepthStencilDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pDepthStencilState:PID3D10DepthStencilState{$ELSE}out DepthStencilState:ID3D10DepthStencilState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateRasterizerState
    (
      const RasterizerDesc:TD3D10_RasterizerDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pRasterizerState:PID3D10RasterizerState{$ELSE}out RasterizerState:ID3D10RasterizerState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateSamplerState
    (
      const SamplerDesc:TD3D10_SamplerDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pSamplerState:PID3D10SamplerState{$ELSE}out SamplerState:ID3D10SamplerState{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateQuery
    (
      const QueryDesc:TD3D10_QueryDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pQuery:PID3D10Query{$ELSE}out Query:ID3D10Query{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreatePredicate
    (
      const PredicateDesc:TD3D10_QueryDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pPredicate:PID3D10Predicate{$ELSE}out Predicate:ID3D10Predicate{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateCounter
    (
      const CounterDesc:TD3D10_CounterDesc; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pCounter:PID3D10Counter{$ELSE}out Counter:ID3D10Counter{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CheckFormatSupport
    (
      Format:TDXGI_Format; (* __in *)
      out FormatSupport:LongWord (* __out *)
    ):HResult; stdcall;

    function CheckMultisampleQualityLevels
    (
      Format:TDXGI_Format; (* __in *)
      SampleCount:LongWord; (* __in *)
      out NumQualityLevels:LongWord (* __out *)
    ):HResult; stdcall;

    procedure CheckCounterInfo
    (
      out CounterInfo:TD3D10_CounterInfo (* __out *)
    ); stdcall;

    function CheckCounter
    (
      const Desc:TD3D10_CounterDesc; (* __in *)
      out _Type:TD3D10_CounterType; (* __out *)
      out ActiveCounters:LongWord; (* __out *)
      Name:PAnsiChar; (* __out_ecount_opt(*pNameLength) *)
      pNameLength:PLongWord; (* __inout_opt *)
      Units:PAnsiChar; (* __out_ecount_opt(*pUnitsLength) *)
      pUnitsLength:PLongWord; (* __inout_opt *)
      Description:PAnsiChar; (* __out_ecount_opt(*pDescriptionLength) *)
      pDescriptionLength:PLongWord (* __inout_opt *)
    ):HResult; stdcall;

    function GetCreationFlags:LongWord; stdcall;

    function OpenSharedResource
    (
      hResource:THANDLE; (* __in *)
      const ReturnedInterface:TGUID; (* __in *)
      out Resource {Interface} (* __out_opt *)
    ):HResult; stdcall;

    procedure SetTextFilterSize
    (
      Width:LongWord; (* __in *)
      Height:LongWord (* __in *)
    ); stdcall;

    procedure GetTextFilterSize
    (
      pWidth:PLongWord; (* __out_opt *)
      pHeight:PLongWord (* __out_opt *)
    ); stdcall;
  end;

  ID3D10Multithread=interface(IUnknown)
    ['{9B7E4E00-342C-4106-A19F-4F2704F689F0}']
    procedure Enter; stdcall;

    procedure Leave; stdcall;

    function SetMultithreadProtected
    (
      MTProtect:LongBool (* __in *)
    ):LongBool; stdcall;

    function GetMultithreadProtected:LongBool; stdcall;
  end;

  TD3D10_CreateDeviceFlag=
  (
    D3D10_CREATE_DEVICE_SINGLETHREADED=$1,
    D3D10_CREATE_DEVICE_DEBUG=$2,
    D3D10_CREATE_DEVICE_SWITCH_TO_REF=$4,
    D3D10_CREATE_DEVICE_PREVENT_INTERNAL_THREADING_OPTIMIZATIONS=$8,
    D3D10_CREATE_DEVICE_ALLOW_NULL_FROM_MAP=$10,
    D3D10_CREATE_DEVICE_BGRA_SUPPORT=$20,
    D3D10_CREATE_DEVICE_STRICT_VALIDATION=$200
  );
  PTD3D10_CreateDeviceFlag=^TD3D10_CreateDeviceFlag;
  D3D10_CREATE_DEVICE_FLAG=TD3D10_CreateDeviceFlag;
  PD3D10_CREATE_DEVICE_FLAG=^TD3D10_CreateDeviceFlag;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10Misc.h"
///////////////////////////////////////////////////////////////////////////////

type
  ID3D10Blob=interface;
  PID3D10Blob=^ID3D10Blob;

  ID3D10Blob=interface(IUnknown)
    ['{8BA5FB08-5195-40E2-AC58-0D989C3A0102}']
    function GetBufferPointer:Pointer; stdcall;
    function GetBufferSize:SIZE_T; stdcall;
  end;

  TD3D10_DriverType=
  (
    D3D10_DRIVER_TYPE_HARDWARE=0,
    D3D10_DRIVER_TYPE_REFERENCE=1,
    D3D10_DRIVER_TYPE_NULL=2,
    D3D10_DRIVER_TYPE_SOFTWARE=3,
    D3D10_DRIVER_TYPE_WARP=5
  );
  PTD3D10_DriverType=^TD3D10_DriverType;
  D3D10_DRIVER_TYPE=TD3D10_DriverType;
  PD3D10_DRIVER_TYPE=^TD3D10_DriverType;

var
  D3D10CreateDevice: function (Adapter:IDXGIAdapter;DriverType:TD3D10_DriverType;Software:HMODULE;Flags:LongWord;SDKVersion:LongWord;out Device:ID3D10Device):HResult; stdcall;
  D3D10CreateDeviceAndSwapChain: function (Adapter:IDXGIAdapter;DriverType:TD3D10_DriverType;Software:HMODULE;Flags:LongWord;SDKVersion:LongWord;pSwapChainDesc:PTDXGI_SwapChainDesc;out SwapChain:IDXGISwapChain;out Device:ID3D10Device):HResult; stdcall;
  D3D10CreateBlob: function (NumBytes:SIZE_T;out Buffer:ID3D10Blob):HResult; stdcall;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10Misc.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10Shader.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_SHADER_DEBUG=(1 shl 0);
  D3D10_SHADER_SKIP_VALIDATION=(1 shl 1);
  D3D10_SHADER_SKIP_OPTIMIZATION=(1 shl 2);
  D3D10_SHADER_PACK_MATRIX_ROW_MAJOR=(1 shl 3);
  D3D10_SHADER_PACK_MATRIX_COLUMN_MAJOR=(1 shl 4);
  D3D10_SHADER_PARTIAL_PRECISION=(1 shl 5);
  D3D10_SHADER_FORCE_VS_SOFTWARE_NO_OPT=(1 shl 6);
  D3D10_SHADER_FORCE_PS_SOFTWARE_NO_OPT=(1 shl 7);
  D3D10_SHADER_NO_PRESHADER=(1 shl 8);
  D3D10_SHADER_AVOID_FLOW_CONTROL=(1 shl 9);
  D3D10_SHADER_PREFER_FLOW_CONTROL=(1 shl 10);
  D3D10_SHADER_ENABLE_STRICTNESS=(1 shl 11);
  D3D10_SHADER_ENABLE_BACKWARDS_COMPATIBILITY=(1 shl 12);
  D3D10_SHADER_IEEE_STRICTNESS=(1 shl 13);
  D3D10_SHADER_WARNINGS_ARE_ERRORS=(1 shl 18);
  D3D10_SHADER_OPTIMIZATION_LEVEL0=(1 shl 14);
  D3D10_SHADER_OPTIMIZATION_LEVEL1=0;
  D3D10_SHADER_OPTIMIZATION_LEVEL2=((1 shl 14) or (1 shl 15));
  D3D10_SHADER_OPTIMIZATION_LEVEL3=(1 shl 15);

type
  TD3D10_ShaderMacro=record
    Name:PAnsiChar;
    Definition:PAnsiChar;
  end;
  PTD3D10_ShaderMacro=^TD3D10_ShaderMacro;
  D3D10_SHADER_MACRO=TD3D10_ShaderMacro;
  PD3D10_SHADER_MACRO=^TD3D10_ShaderMacro;

  TD3D10_ShaderVariableClass=
  (
    D3D10_SVC_SCALAR,
    D3D10_SVC_VECTOR,
    D3D10_SVC_MATRIX_ROWS,
    D3D10_SVC_MATRIX_COLUMNS,
    D3D10_SVC_OBJECT,
    D3D10_SVC_STRUCT,
    D3D11_SVC_INTERFACE_CLASS,
    D3D11_SVC_INTERFACE_POINTER
  );
  PTD3D10_ShaderVariableClass=^TD3D10_ShaderVariableClass;
  D3D10_SHADER_VARIABLE_CLASS=TD3D10_ShaderVariableClass;
  PD3D10_SHADER_VARIABLE_CLASS=^TD3D10_ShaderVariableClass;

  TD3D10_ShaderVariableFlags=
  (
    D3D10_SVF_USERPACKED=1,
    D3D10_SVF_USED=2,
    D3D11_SVF_INTERFACE_POINTER=4,
    D3D11_SVF_INTERFACE_PARAMETER=8
  );
  PTD3D10_ShaderVariableFlags=^TD3D10_ShaderVariableFlags;
  D3D10_SHADER_VARIABLE_FLAGS=TD3D10_ShaderVariableFlags;
  PD3D10_SHADER_VARIABLE_FLAGS=^TD3D10_ShaderVariableFlags;

  TD3D10_ShaderVariableType=
  (
    D3D10_SVT_VOID=0,
    D3D10_SVT_BOOL=1,
    D3D10_SVT_INT=2,
    D3D10_SVT_FLOAT=3,
    D3D10_SVT_STRING=4,
    D3D10_SVT_TEXTURE=5,
    D3D10_SVT_TEXTURE1D=6,
    D3D10_SVT_TEXTURE2D=7,
    D3D10_SVT_TEXTURE3D=8,
    D3D10_SVT_TEXTURECUBE=9,
    D3D10_SVT_SAMPLER=10,
    D3D10_SVT_PIXELSHADER=15,
    D3D10_SVT_VERTEXSHADER=16,
    D3D10_SVT_UINT=19,
    D3D10_SVT_UINT8=20,
    D3D10_SVT_GEOMETRYSHADER=21,
    D3D10_SVT_RASTERIZER=22,
    D3D10_SVT_DEPTHSTENCIL=23,
    D3D10_SVT_BLEND=24,
    D3D10_SVT_BUFFER=25,
    D3D10_SVT_CBUFFER=26,
    D3D10_SVT_TBUFFER=27,
    D3D10_SVT_TEXTURE1DARRAY=28,
    D3D10_SVT_TEXTURE2DARRAY=29,
    D3D10_SVT_RENDERTARGETVIEW=30,
    D3D10_SVT_DEPTHSTENCILVIEW=31,
    D3D10_SVT_TEXTURE2DMS=32,
    D3D10_SVT_TEXTURE2DMSARRAY=33,
    D3D10_SVT_TEXTURECUBEARRAY=34,
    D3D11_SVT_HULLSHADER=35,
    D3D11_SVT_DOMAINSHADER=36,
    D3D11_SVT_INTERFACE_POINTER=37,
    D3D11_SVT_COMPUTESHADER=38,
    D3D11_SVT_DOUBLE=39,
    D3D11_SVT_RWTEXTURE1D,
    D3D11_SVT_RWTEXTURE1DARRAY,
    D3D11_SVT_RWTEXTURE2D,
    D3D11_SVT_RWTEXTURE2DARRAY,
    D3D11_SVT_RWTEXTURE3D,
    D3D11_SVT_RWBUFFER,
    D3D11_SVT_BYTEADDRESS_BUFFER,
    D3D11_SVT_RWBYTEADDRESS_BUFFER,
    D3D11_SVT_STRUCTURED_BUFFER,
    D3D11_SVT_RWSTRUCTURED_BUFFER,
    D3D11_SVT_APPEND_STRUCTURED_BUFFER,
    D3D11_SVT_CONSUME_STRUCTURED_BUFFER
  );
  PTD3D10_ShaderVariableType=^TD3D10_ShaderVariableType;
  D3D10_SHADER_VARIABLE_TYPE=TD3D10_ShaderVariableType;
  PD3D10_SHADER_VARIABLE_TYPE=^TD3D10_ShaderVariableType;

  TD3D10_ShaderInputFlags=
  (
    D3D10_SIF_USERPACKED=1,
    D3D10_SIF_COMPARISON_SAMPLER=2,
    D3D10_SIF_TEXTURE_COMPONENT_0=4,
    D3D10_SIF_TEXTURE_COMPONENT_1=8,
    D3D10_SIF_TEXTURE_COMPONENTS=12
  );
  PTD3D10_ShaderInputFlags=^TD3D10_ShaderInputFlags;
  D3D10_SHADER_INPUT_FLAGS=TD3D10_ShaderInputFlags;
  PD3D10_SHADER_INPUT_FLAGS=^TD3D10_ShaderInputFlags;

  TD3D10_ShaderInputType=
  (
    D3D10_SIT_CBUFFER,
    D3D10_SIT_TBUFFER,
    D3D10_SIT_TEXTURE,
    D3D10_SIT_SAMPLER,
    D3D11_SIT_UAV_RWTYPED,
    D3D11_SIT_STRUCTURED,
    D3D11_SIT_UAV_RWSTRUCTURED,
    D3D11_SIT_BYTEADDRESS,
    D3D11_SIT_UAV_RWBYTEADDRESS,
    D3D11_SIT_UAV_APPEND_STRUCTURED,
    D3D11_SIT_UAV_CONSUME_STRUCTURED,
    D3D11_SIT_UAV_RWSTRUCTURED_WITH_COUNTER
  );
  PTD3D10_ShaderInputType=^TD3D10_ShaderInputType;
  D3D10_SHADER_INPUT_TYPE=TD3D10_ShaderInputType;
  PD3D10_SHADER_INPUT_TYPE=^TD3D10_ShaderInputType;

  TD3D10_ShaderCbufferFlags=
  (
    D3D10_CBF_USERPACKED=1
  );
  PTD3D10_ShaderCbufferFlags=^TD3D10_ShaderCbufferFlags;
  D3D10_SHADER_CBUFFER_FLAGS=TD3D10_ShaderCbufferFlags;
  PD3D10_SHADER_CBUFFER_FLAGS=^TD3D10_ShaderCbufferFlags;

  TD3D10_CBufferType=
  (
    D3D10_CT_CBUFFER,
    D3D10_CT_TBUFFER
  );
  PTD3D10_CBufferType=^TD3D10_CBufferType;
  D3D10_CBUFFER_TYPE=TD3D10_CBufferType;
  PD3D10_CBUFFER_TYPE=^TD3D10_CBufferType;

  TD3D10_Name=
  (
    D3D10_NAME_UNDEFINED=0,
    D3D10_NAME_POSITION=1,
    D3D10_NAME_CLIP_DISTANCE=2,
    D3D10_NAME_CULL_DISTANCE=3,
    D3D10_NAME_RENDER_TARGET_ARRAY_INDEX=4,
    D3D10_NAME_VIEWPORT_ARRAY_INDEX=5,
    D3D10_NAME_VERTEX_ID=6,
    D3D10_NAME_PRIMITIVE_ID=7,
    D3D10_NAME_INSTANCE_ID=8,
    D3D10_NAME_IS_FRONT_FACE=9,
    D3D10_NAME_SAMPLE_INDEX=10,
    D3D11_NAME_FINAL_QUAD_EDGE_TESSFACTOR=11,
    D3D11_NAME_FINAL_QUAD_INSIDE_TESSFACTOR=12,
    D3D11_NAME_FINAL_TRI_EDGE_TESSFACTOR=13,
    D3D11_NAME_FINAL_TRI_INSIDE_TESSFACTOR=14,
    D3D11_NAME_FINAL_LINE_DETAIL_TESSFACTOR=15,
    D3D11_NAME_FINAL_LINE_DENSITY_TESSFACTOR=16,
    D3D10_NAME_TARGET=64,
    D3D10_NAME_DEPTH=65,
    D3D10_NAME_COVERAGE=66,
    D3D11_NAME_DEPTH_GREATER_EQUAL=67,
    D3D11_NAME_DEPTH_LESS_EQUAL=68
  );
  PTD3D10_Name=^TD3D10_Name;
  D3D10_NAME=TD3D10_Name;
  PD3D10_NAME=^TD3D10_Name;

  TD3D10_ResourceReturnType=
  (
    D3D10_RETURN_TYPE_UNORM=1,
    D3D10_RETURN_TYPE_SNORM=2,
    D3D10_RETURN_TYPE_SINT=3,
    D3D10_RETURN_TYPE_UINT=4,
    D3D10_RETURN_TYPE_FLOAT=5,
    D3D10_RETURN_TYPE_MIXED=6
  );
  PTD3D10_ResourceReturnType=^TD3D10_ResourceReturnType;
  D3D10_RESOURCE_RETURN_TYPE=TD3D10_ResourceReturnType;
  PD3D10_RESOURCE_RETURN_TYPE=^TD3D10_ResourceReturnType;

  TD3D10_RegisterComponentType=
  (
    D3D10_REGISTER_COMPONENT_UNKNOWN=0,
    D3D10_REGISTER_COMPONENT_UINT32=1,
    D3D10_REGISTER_COMPONENT_SINT32=2,
    D3D10_REGISTER_COMPONENT_FLOAT32=3
  );
  PTD3D10_RegisterComponentType=^TD3D10_RegisterComponentType;
  D3D10_REGISTER_COMPONENT_TYPE=TD3D10_RegisterComponentType;
  PD3D10_REGISTER_COMPONENT_TYPE=^TD3D10_RegisterComponentType;

  TD3D10_IncludeType=
  (
    D3D10_INCLUDE_LOCAL,
    D3D10_INCLUDE_SYSTEM
  );
  PTD3D10_IncludeType=^TD3D10_IncludeType;
  D3D10_INCLUDE_TYPE=TD3D10_IncludeType;
  PD3D10_INCLUDE_TYPE=^TD3D10_IncludeType;

  ID3D10Include=class;
  PID3D10Include=^ID3D10Include;

  ID3D10Include=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function Open(IncludeType:TD3D10_IncludeType;pFileName:PAnsiChar;pParentData:Pointer;ppData:PPointer;pBytes:PLongWord):HResult; virtual; stdcall; abstract;
    function Close(pData:Pointer):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_ShaderDesc=record
    Version:LongWord;
    Creator:PAnsiChar;
    Flags:LongWord;
    ConstantBuffers:LongWord;
    BoundResources:LongWord;
    InputParameters:LongWord;
    OutputParameters:LongWord;
    InstructionCount:LongWord;
    TempRegisterCount:LongWord;
    TempArrayCount:LongWord;
    DefCount:LongWord;
    DclCount:LongWord;
    TextureNormalInstructions:LongWord;
    TextureLoadInstructions:LongWord;
    TextureCompInstructions:LongWord;
    TextureBiasInstructions:LongWord;
    TextureGradientInstructions:LongWord;
    FloatInstructionCount:LongWord;
    IntInstructionCount:LongWord;
    UintInstructionCount:LongWord;
    StaticFlowControlCount:LongWord;
    DynamicFlowControlCount:LongWord;
    MacroInstructionCount:LongWord;
    ArrayInstructionCount:LongWord;
    CutInstructionCount:LongWord;
    EmitInstructionCount:LongWord;
    GSOutputTopology:TD3D10_PrimitiveTopology;
    GSMaxOutputVertexCount:LongWord;
  end;
  PTD3D10_ShaderDesc=^TD3D10_ShaderDesc;
  D3D10_SHADER_DESC=TD3D10_ShaderDesc;
  PD3D10_SHADER_DESC=^TD3D10_ShaderDesc;

  TD3D10_ShaderBufferDesc=record
    Name:PAnsiChar;
    _Type:TD3D10_CBufferType;
    Variables:LongWord;
    Size:LongWord;
    Flags:LongWord;
  end;
  PTD3D10_ShaderBufferDesc=^TD3D10_ShaderBufferDesc;
  D3D10_SHADER_BUFFER_DESC=TD3D10_ShaderBufferDesc;
  PD3D10_SHADER_BUFFER_DESC=^TD3D10_ShaderBufferDesc;

  TD3D10_ShaderVariableDesc=record
    Name:PAnsiChar;
    StartOffset:LongWord;
    Size:LongWord;
    Flags:LongWord;
    DefaultValue:Pointer;
  end;
  PTD3D10_ShaderVariableDesc=^TD3D10_ShaderVariableDesc;
  D3D10_SHADER_VARIABLE_DESC=TD3D10_ShaderVariableDesc;
  PD3D10_SHADER_VARIABLE_DESC=^TD3D10_ShaderVariableDesc;

  TD3D10_ShaderTypeDesc=record
    _Class:TD3D10_ShaderVariableClass;
    _Type:TD3D10_ShaderVariableType;
    Rows:LongWord;
    Columns:LongWord;
    Elements:LongWord;
    Members:LongWord;
    Offset:LongWord;
  end;
  PTD3D10_ShaderTypeDesc=^TD3D10_ShaderTypeDesc;
  D3D10_SHADER_TYPE_DESC=TD3D10_ShaderTypeDesc;
  PD3D10_SHADER_TYPE_DESC=^TD3D10_ShaderTypeDesc;

  TD3D10_ShaderInputBindDesc=record
    Name:PAnsiChar;
    _Type:TD3D10_ShaderInputType;
    BindPoint:LongWord;
    BindCount:LongWord;
    uFlags:LongWord;
    ReturnType:TD3D10_ResourceReturnType;
    Dimension:TD3D10_SrvDimension;
    NumSamples:LongWord;
  end;
  PTD3D10_ShaderInputBindDesc=^TD3D10_ShaderInputBindDesc;
  D3D10_SHADER_INPUT_BIND_DESC=TD3D10_ShaderInputBindDesc;
  PD3D10_SHADER_INPUT_BIND_DESC=^TD3D10_ShaderInputBindDesc;

  TD3D10_SignatureParameterDesc=record
    SemanticName:PAnsiChar;
    SemanticIndex:LongWord;
    _Register:LongWord;
    SystemValueType:TD3D10_Name;
    ComponentType:TD3D10_RegisterComponentType;
    Mask:Byte;
    ReadWriteMask:Byte;
  end;
  PTD3D10_SignatureParameterDesc=^TD3D10_SignatureParameterDesc;
  D3D10_SIGNATURE_PARAMETER_DESC=TD3D10_SignatureParameterDesc;
  PD3D10_SIGNATURE_PARAMETER_DESC=^TD3D10_SignatureParameterDesc;

  ID3D10ShaderReflectionType=class;
  PID3D10ShaderReflectionType=^ID3D10ShaderReflectionType;

  ID3D10ShaderReflectionType=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function GetDesc(out Desc:TD3D10_ShaderTypeDesc):HResult; virtual; stdcall; abstract;
    function GetMemberTypeByIndex(Index:LongWord):ID3D10ShaderReflectionType; virtual; stdcall; abstract;
    function GetMemberTypeByName(Name:PAnsiChar):ID3D10ShaderReflectionType; virtual; stdcall; abstract;
    function GetMemberTypeName(Index:LongWord):PAnsiChar; virtual; stdcall; abstract;
  end;

  ID3D10ShaderReflectionVariable=class;
  PID3D10ShaderReflectionVariable=^ID3D10ShaderReflectionVariable;

  ID3D10ShaderReflectionVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function GetDesc(out Desc:TD3D10_ShaderVariableDesc):HResult; virtual; stdcall; abstract;
    function GetType:ID3D10ShaderReflectionType; virtual; stdcall; abstract;
  end;

  ID3D10ShaderReflectionConstantBuffer=class;
  PID3D10ShaderReflectionConstantBuffer=^ID3D10ShaderReflectionConstantBuffer;

  ID3D10ShaderReflectionConstantBuffer=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function GetDesc(out Desc:TD3D10_ShaderBufferDesc):HResult; virtual; stdcall; abstract;
    function GetVariableByIndex(Index:LongWord):ID3D10ShaderReflectionVariable; virtual; stdcall; abstract;
    function GetVariableByName(Name:PAnsiChar):ID3D10ShaderReflectionVariable; virtual; stdcall; abstract;
  end;

  ID3D10ShaderReflection=interface;
  PID3D10ShaderReflection=^ID3D10ShaderReflection;

  ID3D10ShaderReflection=interface(IUnknown)
    ['{D40E20B6-F8F7-42AD-AB20-4BAF8F15DFAA}']
    function GetDesc(out Desc:TD3D10_ShaderDesc):HResult; stdcall;
    function GetConstantBufferByIndex(Index:LongWord):ID3D10ShaderReflectionConstantBuffer; stdcall;
    function GetConstantBufferByName(Name:PAnsiChar):ID3D10ShaderReflectionConstantBuffer; stdcall;
    function GetResourceBindingDesc(ResourceIndex:LongWord;out Desc:TD3D10_ShaderInputBindDesc):HResult; stdcall;
    function GetInputParameterDesc(ParameterIndex:LongWord;out Desc:TD3D10_SignatureParameterDesc):HResult; stdcall;
    function GetOutputParameterDesc(ParameterIndex:LongWord;out Desc:TD3D10_SignatureParameterDesc):HResult; stdcall;
  end;

var
  D3D10CompileShader: function (pSrcData:PAnsiChar;SrcDataLen:SIZE_T;pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;pFunctionName:PAnsiChar;pProfile:PAnsiChar;Flags:LongWord;out Shader:ID3D10Blob;out ErrorMsgs:ID3D10Blob):HResult; stdcall;
  D3D10DisassembleShader: function (pShader:Pointer;BytecodeLength:SIZE_T;EnableColorCode:LongBool;pComments:PAnsiChar;out Disassembly:ID3D10Blob):HResult; stdcall;
  D3D10GetPixelShaderProfile: function (Device:ID3D10Device):PAnsiChar; stdcall;
  D3D10GetVertexShaderProfile: function (Device:ID3D10Device):PAnsiChar; stdcall;
  D3D10GetGeometryShaderProfile: function (Device:ID3D10Device):PAnsiChar; stdcall;
  D3D10ReflectShader: function (pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out Reflector:ID3D10ShaderReflection):HResult; stdcall;
  D3D10PreprocessShader: function (pSrcData:PAnsiChar;SrcDataSize:SIZE_T;pFileName:PAnsiChar;const Defines:TD3D10_ShaderMacro;Include:ID3D10Include;out ShaderText:ID3D10Blob;out ErrorMsgs:ID3D10Blob):HResult; stdcall;
  D3D10GetInputSignatureBlob: function (pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out SignatureBlob:ID3D10Blob):HResult; stdcall;
  D3D10GetOutputSignatureBlob: function (pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out SignatureBlob:ID3D10Blob):HResult; stdcall;
  D3D10GetInputAndOutputSignatureBlob: function (pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out SignatureBlob:ID3D10Blob):HResult; stdcall;
  D3D10GetShaderDebugInfo: function (pShaderBytecode:Pointer;BytecodeLength:SIZE_T;out DebugInfo:ID3D10Blob):HResult; stdcall;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10Shader.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10Effect.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_EFFECT_COMPILE_CHILD_EFFECT=(1 shl 0);
  D3D10_EFFECT_COMPILE_ALLOW_SLOW_OPS=(1 shl 1);
  D3D10_EFFECT_SINGLE_THREADED=(1 shl 3);
  D3D10_EFFECT_VARIABLE_POOLED=(1 shl 0);
  D3D10_EFFECT_VARIABLE_ANNOTATION=(1 shl 1);
  D3D10_EFFECT_VARIABLE_EXPLICIT_BIND_POINT=(1 shl 2);

type
  TD3D10_DeviceStateTypes=
  (
    D3D10_DST_SO_BUFFERS=1,
    D3D10_DST_OM_RENDER_TARGETS,
    D3D10_DST_OM_DEPTH_STENCIL_STATE,
    D3D10_DST_OM_BLEND_STATE,
    D3D10_DST_VS,
    D3D10_DST_VS_SAMPLERS,
    D3D10_DST_VS_SHADER_RESOURCES,
    D3D10_DST_VS_CONSTANT_BUFFERS,
    D3D10_DST_GS,
    D3D10_DST_GS_SAMPLERS,
    D3D10_DST_GS_SHADER_RESOURCES,
    D3D10_DST_GS_CONSTANT_BUFFERS,
    D3D10_DST_PS,
    D3D10_DST_PS_SAMPLERS,
    D3D10_DST_PS_SHADER_RESOURCES,
    D3D10_DST_PS_CONSTANT_BUFFERS,
    D3D10_DST_IA_VERTEX_BUFFERS,
    D3D10_DST_IA_INDEX_BUFFER,
    D3D10_DST_IA_INPUT_LAYOUT,
    D3D10_DST_IA_PRIMITIVE_TOPOLOGY,
    D3D10_DST_RS_VIEWPORTS,
    D3D10_DST_RS_SCISSOR_RECTS,
    D3D10_DST_RS_RASTERIZER_STATE,
    D3D10_DST_PREDICATION
  );
  PTD3D10_DeviceStateTypes=^TD3D10_DeviceStateTypes;
  D3D10_DEVICE_STATE_TYPES=TD3D10_DeviceStateTypes;
  PD3D10_DEVICE_STATE_TYPES=^TD3D10_DeviceStateTypes;

  TD3D10_StateBlockMask=record
    VS:Byte;
    VSSamplers:array [0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT)
    VSShaderResources:array[0..15] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    VSConstantBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)

    GS:Byte;
    GSSamplers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT)
    GSShaderResources:array[0..15] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    GSConstantBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)

    PS:Byte;
    PSSamplers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_SAMPLER_SLOT_COUNT)
    PSShaderResources:array[0..15] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT)
    PSConstantBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT)

    IAVertexBuffers:array[0..1] of Byte; // D3D10_BYTES_FROM_BITS(D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT)
    IAIndexBuffer:Byte;
    IAInputLayout:Byte;
    IAPrimitiveTopology:Byte;

    OMRenderTargets:Byte;
    OMDepthStencilState:Byte;
    OMBlendState:Byte;

    RSViewports:Byte;
    RSScissorRects:Byte;
    RSRasterizerState:Byte;

    SOBuffers:Byte;

    Predication:Byte;
  end;
  PTD3D10_StateBlockMask=^TD3D10_StateBlockMask;
  D3D10_STATE_BLOCK_MASK=TD3D10_StateBlockMask;
  PD3D10_STATE_BLOCK_MASK=^TD3D10_StateBlockMask;

  ID3D10StateBlock=interface;
  PID3D10StateBlock=^ID3D10StateBlock;

  ID3D10StateBlock=interface(IUnknown)
    ['{0803425A-57F5-4DD6-9465-A87570834A08}']
    function Capture:HResult; stdcall;
    function Apply:HResult; stdcall;
    function ReleaseAllDeviceObjects:HResult; stdcall;
    function GetDevice(out Device:ID3D10Device):HResult; stdcall;
  end;


type
  TD3D10_EffectTypeDesc=record
    TypeName:PAnsiChar;
    _Class:TD3D10_ShaderVariableClass;
    _Type:TD3D10_ShaderVariableType;
    Elements:LongWord;
    Members:LongWord;
    Rows:LongWord;
    Columns:LongWord;
    PackedSize:LongWord;
    UnpackedSize:LongWord;
    Stride:LongWord;
  end;
  PTD3D10_EffectTypeDesc=^TD3D10_EffectTypeDesc;
  D3D10_EFFECT_TYPE_DESC=TD3D10_EffectTypeDesc;
  PD3D10_EFFECT_TYPE_DESC=^TD3D10_EffectTypeDesc;

  ID3D10EffectType=class;
  PID3D10EffectType=^ID3D10EffectType;

  ID3D10EffectType=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectTypeDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetMemberTypeByIndex
    (
      Index:LongWord
    ):ID3D10EffectType; virtual; stdcall; abstract;

    function GetMemberTypeByName
    (
      Name:PAnsiChar
    ):ID3D10EffectType; virtual; stdcall; abstract;

    function GetMemberTypeBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectType; virtual; stdcall; abstract;

    function GetMemberName
    (
      Index:LongWord
    ):PAnsiChar; virtual; stdcall; abstract;

    function GetMemberSemantic
    (
      Index:LongWord
    ):PAnsiChar; virtual; stdcall; abstract;
  end;

  TD3D10_EffectVariableDesc=record
    Name:PAnsiChar;
    Semantic:PAnsiChar;
    Flags:LongWord;
    Annotations:LongWord;
    BufferOffset:LongWord;
    ExplicitBindPoint:LongWord;
  end;
  PTD3D10_EffectVariableDesc=^TD3D10_EffectVariableDesc;
  D3D10_EFFECT_VARIABLE_DESC=TD3D10_EffectVariableDesc;
  PD3D10_EFFECT_VARIABLE_DESC=^TD3D10_EffectVariableDesc;

  ID3D10EffectVariable=class;
  PID3D10EffectVariable=^ID3D10EffectVariable;

  ID3D10EffectScalarVariable=class;
  PID3D10EffectScalarVariable=^ID3D10EffectScalarVariable;

  ID3D10EffectVectorVariable=class;
  PID3D10EffectVectorVariable=^ID3D10EffectVectorVariable;

  ID3D10EffectMatrixVariable=class;
  PID3D10EffectMatrixVariable=^ID3D10EffectMatrixVariable;

  ID3D10EffectStringVariable=class;
  PID3D10EffectStringVariable=^ID3D10EffectStringVariable;

  ID3D10EffectShaderResourceVariable=class;
  PID3D10EffectShaderResourceVariable=^ID3D10EffectShaderResourceVariable;

  ID3D10EffectRenderTargetViewVariable=class;
  PID3D10EffectRenderTargetViewVariable=^ID3D10EffectRenderTargetViewVariable;

  ID3D10EffectDepthStencilViewVariable=class;
  PID3D10EffectDepthStencilViewVariable=^ID3D10EffectDepthStencilViewVariable;

  ID3D10EffectConstantBuffer=class;
  PID3D10EffectConstantBuffer=^ID3D10EffectConstantBuffer;

  ID3D10EffectShaderVariable=class;
  PID3D10EffectShaderVariable=^ID3D10EffectShaderVariable;

  ID3D10EffectBlendVariable=class;
  PID3D10EffectBlendVariable=^ID3D10EffectBlendVariable;

  ID3D10EffectDepthStencilVariable=class;
  PID3D10EffectDepthStencilVariable=^ID3D10EffectDepthStencilVariable;

  ID3D10EffectRasterizerVariable=class;
  PID3D10EffectRasterizerVariable=^ID3D10EffectRasterizerVariable;

  ID3D10EffectSamplerVariable=class;
  PID3D10EffectSamplerVariable=^ID3D10EffectSamplerVariable;

  ID3D10EffectVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectScalarVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetFloat
    (
      Value:Single
    ):HResult; virtual; stdcall; abstract;

    function GetFloat
    (
      pValue:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetFloatArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetFloatArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetInt
    (
      Value:Integer
    ):HResult; virtual; stdcall; abstract;

    function GetInt
    (
      pValue:PInteger
    ):HResult; virtual; stdcall; abstract;

    function SetIntArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetIntArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetBool
    (
      Value:LongBool
    ):HResult; virtual; stdcall; abstract;

    function GetBool
    (
      pValue:PLongBool
    ):HResult; virtual; stdcall; abstract;

    function SetBoolArray
    (
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBoolArray
    (
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectVectorVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetBoolVector
    (
      pData:PLongBool
    ):HResult; virtual; stdcall; abstract;

    function SetIntVector
    (
      pData:PInteger
    ):HResult; virtual; stdcall; abstract;

    function SetFloatVector
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function GetBoolVector
    (
      pData:PLongBool
    ):HResult; virtual; stdcall; abstract;

    function GetIntVector
    (
      pData:PInteger
    ):HResult; virtual; stdcall; abstract;

    function GetFloatVector
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetBoolVectorArray
    (
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetIntVectorArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetFloatVectorArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBoolVectorArray
    (
      pData:PLongBool;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetIntVectorArray
    (
      pData:PInteger;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetFloatVectorArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectMatrixVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      ByteOffset:LongWord;
      ByteCount:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetMatrix
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function GetMatrix
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixTranspose
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixTranspose
    (
      pData:PSingle
    ):HResult; virtual; stdcall; abstract;

    function SetMatrixTransposeArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetMatrixTransposeArray
    (
      pData:PSingle;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectStringVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetString
    (
      ppString:PPAnsiChar
    ):HResult; virtual; stdcall; abstract;

    function GetStringArray
    (
      ppStrings:PPAnsiChar;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectShaderResourceVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetResource
    (
      Resource:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function GetResource
    (
      out Resource:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function SetResourceArray
    (
      pResources:PID3D10ShaderResourceView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetResourceArray
    (
      pResources:PID3D10ShaderResourceView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectRenderTargetViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetRenderTarget
    (
      Resource:ID3D10RenderTargetView
    ):HResult; virtual; stdcall; abstract;

    function GetRenderTarget
    (
      out Resource:ID3D10RenderTargetView
    ):HResult; virtual; stdcall; abstract;

    function SetRenderTargetArray
    (
      pResources:PID3D10RenderTargetView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRenderTargetArray
    (
      pResources:PID3D10RenderTargetView; (* __in_ecount(0) *)
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectDepthStencilViewVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetDepthStencil
    (
      Resource:ID3D10DepthStencilView
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencil
    (
      out Resource:ID3D10DepthStencilView
    ):HResult; virtual; stdcall; abstract;

    function SetDepthStencilArray
    (
      out Resources:ID3D10DepthStencilView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencilArray
    (
      out Resources:ID3D10DepthStencilView;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectConstantBuffer=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function SetConstantBuffer
    (
      ConstantBuffer:ID3D10Buffer
    ):HResult; virtual; stdcall; abstract;

    function GetConstantBuffer
    (
      out ConstantBuffer:ID3D10Buffer
    ):HResult; virtual; stdcall; abstract;

    function SetTextureBuffer
    (
      TextureBuffer:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;

    function GetTextureBuffer
    (
      out TextureBuffer:ID3D10ShaderResourceView
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_EffectShaderDesc=record
    pInputSignature:PByte;
    IsInline:LongBool;
    pBytecode:PByte;
    BytecodeLength:LongWord;
    SODecl:PAnsiChar;
    NumInputSignatureEntries:LongWord;
    NumOutputSignatureEntries:LongWord;
  end;
  PTD3D10_EffectShaderDesc=^TD3D10_EffectShaderDesc;
  D3D10_EFFECT_SHADER_DESC=TD3D10_EffectShaderDesc;
  PD3D10_EFFECT_SHADER_DESC=^TD3D10_EffectShaderDesc;

  ID3D10EffectShaderVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetShaderDesc
    (
      ShaderIndex:LongWord;
      out Desc:TD3D10_EffectShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetVertexShader
    (
      ShaderIndex:LongWord;
      out VS:ID3D10VertexShader
    ):HResult; virtual; stdcall; abstract;

    function GetGeometryShader
    (
      ShaderIndex:LongWord;
      out GS:ID3D10GeometryShader
    ):HResult; virtual; stdcall; abstract;

    function GetPixelShader
    (
      ShaderIndex:LongWord;
      out PS:ID3D10PixelShader
    ):HResult; virtual; stdcall; abstract;

    function GetInputSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D10_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;

    function GetOutputSignatureElementDesc
    (
      ShaderIndex:LongWord;
      Element:LongWord;
      pDesc:PTD3D10_SignatureParameterDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectBlendVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetBlendState
    (
      Index:LongWord;
      out BlendState:ID3D10BlendState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pBlendDesc:PTD3D10_BlendDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectDepthStencilVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetDepthStencilState
    (
      Index:LongWord;
      out DepthStencilState:ID3D10DepthStencilState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pDepthStencilDesc:PTD3D10_DepthStencilDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectRasterizerVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRasterizerState
    (
      Index:LongWord;
      out RasterizerState:ID3D10RasterizerState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pRasterizerDesc:PTD3D10_RasterizerDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  ID3D10EffectSamplerVariable=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.(ID3D10EffectVariable)
    function GetType:ID3D10EffectType; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_EffectVariableDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetMemberBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetElement
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetParentConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsScalar:ID3D10EffectScalarVariable; virtual; stdcall; abstract;

    function AsVector:ID3D10EffectVectorVariable; virtual; stdcall; abstract;

    function AsMatrix:ID3D10EffectMatrixVariable; virtual; stdcall; abstract;

    function AsString:ID3D10EffectStringVariable; virtual; stdcall; abstract;

    function AsShaderResource:ID3D10EffectShaderResourceVariable; virtual; stdcall; abstract;

    function AsRenderTargetView:ID3D10EffectRenderTargetViewVariable; virtual; stdcall; abstract;

    function AsDepthStencilView:ID3D10EffectDepthStencilViewVariable; virtual; stdcall; abstract;

    function AsConstantBuffer:ID3D10EffectConstantBuffer; virtual; stdcall; abstract;

    function AsShader:ID3D10EffectShaderVariable; virtual; stdcall; abstract;

    function AsBlend:ID3D10EffectBlendVariable; virtual; stdcall; abstract;

    function AsDepthStencil:ID3D10EffectDepthStencilVariable; virtual; stdcall; abstract;

    function AsRasterizer:ID3D10EffectRasterizerVariable; virtual; stdcall; abstract;

    function AsSampler:ID3D10EffectSamplerVariable; virtual; stdcall; abstract;

    function SetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetRawValue
    (
      pData:Pointer;
      Offset:LongWord;
      Count:LongWord
    ):HResult; virtual; stdcall; abstract;

    function GetSampler
    (
      Index:LongWord;
      out Sampler:ID3D10SamplerState
    ):HResult; virtual; stdcall; abstract;

    function GetBackingStore
    (
      Index:LongWord;
      pSamplerDesc:PTD3D10_SamplerDesc
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_PassDesc=record
    Name:PAnsiChar;
    Annotations:LongWord;
    pIAInputSignature:PByte;
    IAInputSignatureSize:SIZE_T;
    StencilRef:LongWord;
    SampleMask:LongWord;
    BlendFactor:array[0..3] of Single;
  end;
  PTD3D10_PassDesc=^TD3D10_PassDesc;
  D3D10_PASS_DESC=TD3D10_PassDesc;
  PD3D10_PASS_DESC=^TD3D10_PassDesc;

  TD3D10_PassShaderDesc=record
    pShaderVariable:ID3D10EffectShaderVariable;
    ShaderIndex:LongWord;
  end;
  PTD3D10_PassShaderDesc=^TD3D10_PassShaderDesc;
  D3D10_PASS_SHADER_DESC=TD3D10_PassShaderDesc;
  PD3D10_PASS_SHADER_DESC=^TD3D10_PassShaderDesc;

  ID3D10EffectPass=class;
  PID3D10EffectPass=^ID3D10EffectPass;

  ID3D10EffectPass=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_PassDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetVertexShaderDesc
    (
      out Desc:TD3D10_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetGeometryShaderDesc
    (
      out Desc:TD3D10_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetPixelShaderDesc
    (
      out Desc:TD3D10_PassShaderDesc (* __out *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function Apply
    (
      Flags:LongWord
    ):HResult; virtual; stdcall; abstract;

    function ComputeStateBlockMask
    (
      out StateBlockMask:TD3D10_StateBlockMask (* __out *)
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_TechniqueDesc=record
    Name:PAnsiChar;
    Passes:LongWord;
    Annotations:LongWord;
  end;
  PTD3D10_TechniqueDesc=^TD3D10_TechniqueDesc;
  D3D10_TECHNIQUE_DESC=TD3D10_TechniqueDesc;
  PD3D10_TECHNIQUE_DESC=^TD3D10_TechniqueDesc;

  ID3D10EffectTechnique=class;
  PID3D10EffectTechnique=^ID3D10EffectTechnique;

  ID3D10EffectTechnique=class // Cannot use 'interface' as the QueryInterface, AddRef and Release methods are missing.
    function IsValid:LongBool; virtual; stdcall; abstract;

    function GetDesc
    (
      const Desc:TD3D10_TechniqueDesc (* __in *)
    ):HResult; virtual; stdcall; abstract;

    function GetAnnotationByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetAnnotationByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; virtual; stdcall; abstract;

    function GetPassByIndex
    (
      Index:LongWord
    ):ID3D10EffectPass; virtual; stdcall; abstract;

    function GetPassByName
    (
      Name:PAnsiChar
    ):ID3D10EffectPass; virtual; stdcall; abstract;

    function ComputeStateBlockMask
    (
      out StateBlockMask:TD3D10_StateBlockMask (* __out *)
    ):HResult; virtual; stdcall; abstract;
  end;

  TD3D10_EffectDesc=record
    IsChildEffect:LongBool;
    ConstantBuffers:LongWord;
    SharedConstantBuffers:LongWord;
    GlobalVariables:LongWord;
    SharedGlobalVariables:LongWord;
    Techniques:LongWord;
  end;
  PTD3D10_EffectDesc=^TD3D10_EffectDesc;
  D3D10_EFFECT_DESC=TD3D10_EffectDesc;
  PD3D10_EFFECT_DESC=^TD3D10_EffectDesc;

  ID3D10Effect=interface;
  PID3D10Effect=^ID3D10Effect;

  ID3D10Effect=interface(IUnknown)
    ['{51B0CA8B-EC0B-4519-870D-8EE1CB5017C7}']
    function IsValid:LongBool; stdcall;

    function IsPool:LongBool; stdcall;

    function GetDevice
    (
      out Device:ID3D10Device
    ):HResult; stdcall;

    function GetDesc
    (
      out Desc:TD3D10_EffectDesc (* __out *)
    ):HResult; stdcall;

    function GetConstantBufferByIndex
    (
      Index:LongWord
    ):ID3D10EffectConstantBuffer; stdcall;

    function GetConstantBufferByName
    (
      Name:PAnsiChar
    ):ID3D10EffectConstantBuffer; stdcall;

    function GetVariableByIndex
    (
      Index:LongWord
    ):ID3D10EffectVariable; stdcall;

    function GetVariableByName
    (
      Name:PAnsiChar
    ):ID3D10EffectVariable; stdcall;

    function GetVariableBySemantic
    (
      Semantic:PAnsiChar
    ):ID3D10EffectVariable; stdcall;

    function GetTechniqueByIndex
    (
      Index:LongWord
    ):ID3D10EffectTechnique; stdcall;

    function GetTechniqueByName
    (
      Name:PAnsiChar
    ):ID3D10EffectTechnique; stdcall;

    function Optimize:HResult; stdcall;

    function IsOptimized:LongBool; stdcall;
  end;

  ID3D10EffectPool=interface;
  PID3D10EffectPool=^ID3D10EffectPool;

  ID3D10EffectPool=interface(IUnknown)
    ['{9537AB04-3250-412E-8213-FCD2F8677933}']
    function AsEffect:ID3D10Effect; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10Effect.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10SDKLayers.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_SDK_LAYERS_VERSION=11;
  D3D10_DEBUG_FEATURE_FLUSH_PER_RENDER_OP=$1;
  D3D10_DEBUG_FEATURE_FINISH_PER_RENDER_OP=$2;
  D3D10_DEBUG_FEATURE_PRESENT_PER_RENDER_OP=$4;
  D3D10_INFO_QUEUE_DEFAULT_MESSAGE_COUNT_LIMIT=1024;
  D3D10_REGKEY_PATH='Software\Microsoft\Direct3D';
  D3D10_MUTE_DEBUG_OUTPUT='MuteDebugOutput';
  D3D10_ENABLE_BREAK_ON_MESSAGE='EnableBreakOnMessage';
  D3D10_INFOQUEUE_STORAGE_FILTER_OVERRIDE='InfoQueueStorageFilterOverride';
  D3D10_MUTE_CATEGORY='Mute_CATEGORY_%s';
  D3D10_MUTE_SEVERITY='Mute_SEVERITY_%s';
  D3D10_MUTE_ID_STRING='Mute_ID_%s';
  D3D10_MUTE_ID_DECIMAL='Mute_ID_%d';
  D3D10_UNMUTE_SEVERITY_INFO='Unmute_SEVERITY_INFO';
  D3D10_BREAKON_CATEGORY='BreakOn_CATEGORY_%s';
  D3D10_BREAKON_SEVERITY='BreakOn_SEVERITY_%s';
  D3D10_BREAKON_ID_STRING='BreakOn_ID_%s';
  D3D10_BREAKON_ID_DECIMAL='BreakOn_ID_%d';
  D3D10_APPSIZE_STRING='Size';
  D3D10_APPNAME_STRING='Name';

type
  ID3D10Debug=interface;
  PID3D10Debug=^ID3D10Debug;

  ID3D10SwitchToRef=interface;
  PID3D10SwitchToRef=^ID3D10SwitchToRef;

  ID3D10InfoQueue=interface;
  PID3D10InfoQueue=^ID3D10InfoQueue;

  ID3D10Debug=interface(IUnknown)
    ['{9B7E4E01-342C-4106-A19F-4F2704F689F0}']
    function SetFeatureMask
    (
      Mask:LongWord
    ):HResult; stdcall;

    function GetFeatureMask:LongWord; stdcall;

    function SetPresentPerRenderOpDelay
    (
      Milliseconds:LongWord
    ):HResult; stdcall;

    function GetPresentPerRenderOpDelay:LongWord; stdcall;

    function SetSwapChain
    (
      SwapChain:IDXGISwapChain (* __in_opt *)
    ):HResult; stdcall;

    function GetSwapChain
    (
      out SwapChain:IDXGISwapChain (* __out *)
    ):HResult; stdcall;

    function Validate:HResult; stdcall;
  end;

  ID3D10SwitchToRef=interface(IUnknown)
    ['{9B7E4E02-342C-4106-A19F-4F2704F689F0}']
    function SetUseRef(UseRef:LongBool):LongBool; stdcall;
    function GetUseRef:LongBool; stdcall;
  end;

  TD3D10_MessageCategory=
  (
    D3D10_MESSAGE_CATEGORY_APPLICATION_DEFINED=0,
    D3D10_MESSAGE_CATEGORY_MISCELLANEOUS=D3D10_MESSAGE_CATEGORY_APPLICATION_DEFINED + 1,
    D3D10_MESSAGE_CATEGORY_INITIALIZATION=D3D10_MESSAGE_CATEGORY_MISCELLANEOUS + 1,
    D3D10_MESSAGE_CATEGORY_CLEANUP=D3D10_MESSAGE_CATEGORY_INITIALIZATION + 1,
    D3D10_MESSAGE_CATEGORY_COMPILATION=D3D10_MESSAGE_CATEGORY_CLEANUP + 1,
    D3D10_MESSAGE_CATEGORY_STATE_CREATION=D3D10_MESSAGE_CATEGORY_COMPILATION + 1,
    D3D10_MESSAGE_CATEGORY_STATE_SETTING=D3D10_MESSAGE_CATEGORY_STATE_CREATION + 1,
    D3D10_MESSAGE_CATEGORY_STATE_GETTING=D3D10_MESSAGE_CATEGORY_STATE_SETTING + 1,
    D3D10_MESSAGE_CATEGORY_RESOURCE_MANIPULATION=D3D10_MESSAGE_CATEGORY_STATE_GETTING + 1,
    D3D10_MESSAGE_CATEGORY_EXECUTION=D3D10_MESSAGE_CATEGORY_RESOURCE_MANIPULATION + 1
  );
  PTD3D10_MessageCategory=^TD3D10_MessageCategory;
  D3D10_MESSAGE_CATEGORY=TD3D10_MessageCategory;
  PD3D10_MESSAGE_CATEGORY=^TD3D10_MessageCategory;

  TD3D10_MessageSeverity=
  (
    D3D10_MESSAGE_SEVERITY_CORRUPTION=0,
    D3D10_MESSAGE_SEVERITY_ERROR=D3D10_MESSAGE_SEVERITY_CORRUPTION + 1,
    D3D10_MESSAGE_SEVERITY_WARNING=D3D10_MESSAGE_SEVERITY_ERROR + 1,
    D3D10_MESSAGE_SEVERITY_INFO=D3D10_MESSAGE_SEVERITY_WARNING + 1
  );
  PTD3D10_MessageSeverity=^TD3D10_MessageSeverity;
  D3D10_MESSAGE_SEVERITY=TD3D10_MessageSeverity;
  PD3D10_MESSAGE_SEVERITY=^TD3D10_MessageSeverity;

  TD3D10_MessageID=
  (
    D3D10_MESSAGE_ID_UNKNOWN=0,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_HAZARD=D3D10_MESSAGE_ID_UNKNOWN + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_HAZARD=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_HAZARD=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_HAZARD=D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_HAZARD=D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_HAZARD=D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_HAZARD=D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_HAZARD=D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_OMSETRENDERTARGETS_HAZARD=D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_HAZARD + 1,
    D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_HAZARD=D3D10_MESSAGE_ID_DEVICE_OMSETRENDERTARGETS_HAZARD + 1,
    D3D10_MESSAGE_ID_STRING_FROM_APPLICATION=D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_HAZARD + 1,
    D3D10_MESSAGE_ID_CORRUPTED_THIS=D3D10_MESSAGE_ID_STRING_FROM_APPLICATION + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER1=D3D10_MESSAGE_ID_CORRUPTED_THIS + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER2=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER1 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER3=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER2 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER4=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER3 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER5=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER4 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER6=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER5 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER7=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER6 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER8=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER7 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER9=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER8 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER10=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER9 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER11=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER10 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER12=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER11 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER13=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER12 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER14=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER13 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_PARAMETER15=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER14 + 1,
    D3D10_MESSAGE_ID_CORRUPTED_MULTITHREADING=D3D10_MESSAGE_ID_CORRUPTED_PARAMETER15 + 1,
    D3D10_MESSAGE_ID_MESSAGE_REPORTING_OUTOFMEMORY=D3D10_MESSAGE_ID_CORRUPTED_MULTITHREADING + 1,
    D3D10_MESSAGE_ID_IASETINPUTLAYOUT_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_MESSAGE_REPORTING_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_IASETINPUTLAYOUT_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_IASETINDEXBUFFER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETSHADER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_IASETINDEXBUFFER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETSHADERRESOURCES_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETSHADER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETSHADERRESOURCES_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_VSSETSAMPLERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETSHADER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_VSSETSAMPLERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETSHADERRESOURCES_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETSHADER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETSHADERRESOURCES_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GSSETSAMPLERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_SOSETTARGETS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_GSSETSAMPLERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETSHADER_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_SOSETTARGETS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETSHADERRESOURCES_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETSHADER_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETSHADERRESOURCES_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_PSSETSAMPLERS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_RSSETSTATE_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_PSSETSAMPLERS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_OMSETBLENDSTATE_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_RSSETSTATE_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_OMSETDEPTHSTENCILSTATE_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_OMSETBLENDSTATE_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_OMSETDEPTHSTENCILSTATE_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_SETPREDICATION_UNBINDDELETINGOBJECT=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_GETPRIVATEDATA_MOREDATA=D3D10_MESSAGE_ID_SETPREDICATION_UNBINDDELETINGOBJECT + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFREEDATA=D3D10_MESSAGE_ID_GETPRIVATEDATA_MOREDATA + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDIUNKNOWN=D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFREEDATA + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFLAGS=D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDIUNKNOWN + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_CHANGINGPARAMS=D3D10_MESSAGE_ID_SETPRIVATEDATA_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_SETPRIVATEDATA_OUTOFMEMORY=D3D10_MESSAGE_ID_SETPRIVATEDATA_CHANGINGPARAMS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_SETPRIVATEDATA_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_NULLDESC=D3D10_MESSAGE_ID_CREATEBUFFER_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCONSTANTBUFFERBINDINGS=D3D10_MESSAGE_ID_CREATEBUFFER_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEBUFFER_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATEBUFFER_INVALIDCONSTANTBUFFERBINDINGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATEBUFFER_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE1D_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_NULLDESC=D3D10_MESSAGE_ID_CREATETEXTURE1D_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE1D_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATETEXTURE1D_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE1D_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE2D_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_NULLDESC=D3D10_MESSAGE_ID_CREATETEXTURE2D_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE2D_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATETEXTURE2D_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE2D_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDSAMPLES=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDUSAGE=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDSAMPLES + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDUSAGE + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDCPUACCESSFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_UNRECOGNIZEDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDBINDFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDCPUACCESSFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDINITIALDATA=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDBINDFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDINITIALDATA + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMIPLEVELS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMIPLEVELS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATETEXTURE3D_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_NULLDESC=D3D10_MESSAGE_ID_CREATETEXTURE3D_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATETEXTURE3D_LARGEALLOCATION=D3D10_MESSAGE_ID_CREATETEXTURE3D_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATETEXTURE3D_LARGEALLOCATION + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDESC=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDESC + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDRESOURCE=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDRESOURCE + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNSUPPORTEDFORMAT=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDESC=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_UNSUPPORTEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDESC + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDRESOURCE=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDRESOURCE + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_UNRECOGNIZEDFORMAT=D3D10_MESSAGE_ID_CREATERENDERTARGETVIEW_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDESC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_UNRECOGNIZEDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDESC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDRESOURCE=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDRESOURCE + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDARG_RETURN=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILVIEW_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TOOMANYELEMENTS=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDFORMAT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TOOMANYELEMENTS + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INCOMPATIBLEFORMAT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INCOMPATIBLEFORMAT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDINPUTSLOTCLASS=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_STEPRATESLOTCLASSMISMATCH=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDINPUTSLOTCLASS + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOTCLASSCHANGE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_STEPRATESLOTCLASSMISMATCH + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSTEPRATECHANGE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSLOTCLASSCHANGE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDALIGNMENT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDSTEPRATECHANGE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_DUPLICATESEMANTIC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_INVALIDALIGNMENT + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNPARSEABLEINPUTSIGNATURE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_DUPLICATESEMANTIC + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLSEMANTIC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNPARSEABLEINPUTSIGNATURE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_MISSINGELEMENT=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLSEMANTIC + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLDESC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_MISSINGELEMENT + 1,
    D3D10_MESSAGE_ID_CREATEVERTEXSHADER_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEVERTEXSHADER_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEVERTEXSHADER_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADER_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDNUMENTRIES=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSTREAMSTRIDEUNUSED=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDNUMENTRIES + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_UNEXPECTEDDECL=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSTREAMSTRIDEUNUSED + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_EXPECTEDDECL=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_UNEXPECTEDDECL + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSLOT0EXPECTED=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_EXPECTEDDECL + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSLOT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_OUTPUTSLOT0EXPECTED + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_ONLYONEELEMENTPERSLOT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSLOT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDCOMPONENTCOUNT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_ONLYONEELEMENTPERSLOT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSTARTCOMPONENTANDCOMPONENTCOUNT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDCOMPONENTCOUNT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDGAPDEFINITION=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDSTARTCOMPONENTANDCOMPONENTCOUNT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_REPEATEDOUTPUT=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDGAPDEFINITION + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSTREAMSTRIDE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_REPEATEDOUTPUT + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGSEMANTIC=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_INVALIDOUTPUTSTREAMSTRIDE + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MASKMISMATCH=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGSEMANTIC + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_CANTHAVEONLYGAPS=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MASKMISMATCH + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_DECLTOOCOMPLEX=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_CANTHAVEONLYGAPS + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGOUTPUTSIGNATURE=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_DECLTOOCOMPLEX + 1,
    D3D10_MESSAGE_ID_CREATEPIXELSHADER_OUTOFMEMORY=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_MISSINGOUTPUTSIGNATURE + 1,
    D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERBYTECODE=D3D10_MESSAGE_ID_CREATEPIXELSHADER_OUTOFMEMORY + 1,
    D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERTYPE=D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERBYTECODE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDFILLMODE=D3D10_MESSAGE_ID_CREATEPIXELSHADER_INVALIDSHADERTYPE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDCULLMODE=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDFILLMODE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDDEPTHBIASCLAMP=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDCULLMODE + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDSLOPESCALEDDEPTHBIAS=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDDEPTHBIASCLAMP + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_INVALIDSLOPESCALEDDEPTHBIAS + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHWRITEMASK=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHFUNC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHWRITEMASK + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDDEPTHFUNC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILZFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILPASSOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILZFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFUNC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILPASSOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDFRONTFACESTENCILFUNC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILZFAILOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILPASSOP=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILZFAILOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFUNC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILPASSOP + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_INVALIDBACKFACESTENCILFUNC + 1,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLEND=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLEND=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLEND + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOP=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLEND + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLENDALPHA=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOP + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLENDALPHA=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDSRCBLENDALPHA + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOPALPHA=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDDESTBLENDALPHA + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDRENDERTARGETWRITEMASK=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDBLENDOPALPHA + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATEBLENDSTATE_INVALIDRENDERTARGETWRITEMASK + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATEBLENDSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDFILTER=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSU=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDFILTER + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSV=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSU + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSW=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSV + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMIPLODBIAS=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDADDRESSW + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXANISOTROPY=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMIPLODBIAS + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDCOMPARISONFUNC=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXANISOTROPY + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMINLOD=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDCOMPARISONFUNC + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXLOD=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMINLOD + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_TOOMANYOBJECTS=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_INVALIDMAXLOD + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NULLDESC=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_TOOMANYOBJECTS + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDQUERY=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDMISCFLAGS=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDQUERY + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_UNEXPECTEDMISCFLAG=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_INVALIDMISCFLAGS + 1,
    D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_NULLDESC=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_UNEXPECTEDMISCFLAG + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNRECOGNIZED=D3D10_MESSAGE_ID_CREATEQUERYORPREDICATE_NULLDESC + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNDEFINED=D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNRECOGNIZED + 1,
    D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_TOPOLOGY_UNDEFINED + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_OFFSET_TOO_LARGE=D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_OFFSET_TOO_LARGE + 1,
    D3D10_MESSAGE_ID_IASETINDEXBUFFER_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_FORMAT_INVALID=D3D10_MESSAGE_ID_IASETINDEXBUFFER_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_TOO_LARGE=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_FORMAT_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_TOO_LARGE + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_IASETINDEXBUFFER_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_VSSETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_VSSETCONSTANTBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_VSSETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSSETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSSETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_GSSETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_GSSETCONSTANTBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_GSSETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSSETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_SOSETTARGETS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_GSSETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_SOSETTARGETS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_SOSETTARGETS_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_INVALIDBUFFER=D3D10_MESSAGE_ID_DEVICE_PSSETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_PSSETCONSTANTBUFFERS_INVALIDBUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_PSSETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSSETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_INVALIDVIEWPORT=D3D10_MESSAGE_ID_DEVICE_PSSETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_INVALIDSCISSOR=D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_INVALIDVIEWPORT + 1,
    D3D10_MESSAGE_ID_CLEARRENDERTARGETVIEW_DENORMFLUSH=D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_INVALIDSCISSOR + 1,
    D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_DENORMFLUSH=D3D10_MESSAGE_ID_CLEARRENDERTARGETVIEW_DENORMFLUSH + 1,
    D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_INVALID=D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_DENORMFLUSH + 1,
    D3D10_MESSAGE_ID_DEVICE_IAGETVERTEXBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_CLEARDEPTHSTENCILVIEW_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_VSGETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_IAGETVERTEXBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_VSGETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSGETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_VSGETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSGETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSGETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_VSGETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSGETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSGETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GSGETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSGETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_SOGETTARGETS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_GSGETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSGETSHADERRESOURCES_VIEWS_EMPTY=D3D10_MESSAGE_ID_DEVICE_SOGETTARGETS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSGETCONSTANTBUFFERS_BUFFERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSGETSHADERRESOURCES_VIEWS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_PSGETSAMPLERS_SAMPLERS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSGETCONSTANTBUFFERS_BUFFERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSGETVIEWPORTS_VIEWPORTS_EMPTY=D3D10_MESSAGE_ID_DEVICE_PSGETSAMPLERS_SAMPLERS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_RSGETSCISSORRECTS_RECTS_EMPTY=D3D10_MESSAGE_ID_DEVICE_RSGETVIEWPORTS_VIEWPORTS_EMPTY + 1,
    D3D10_MESSAGE_ID_DEVICE_GENERATEMIPS_RESOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RSGETSCISSORRECTS_RECTS_EMPTY + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSUBRESOURCE=D3D10_MESSAGE_ID_DEVICE_GENERATEMIPS_RESOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESUBRESOURCE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCEBOX=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESUBRESOURCE + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCEBOX + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSTATE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCE + 1,
    D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESTATE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDDESTINATIONSTATE + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCE=D3D10_MESSAGE_ID_COPYSUBRESOURCEREGION_INVALIDSOURCESTATE + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDDESTINATIONSTATE=D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCE + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCESTATE=D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDDESTINATIONSTATE + 1,
    D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSUBRESOURCE=D3D10_MESSAGE_ID_COPYRESOURCE_INVALIDSOURCESTATE + 1,
    D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONBOX=D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSTATE=D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONBOX + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_INVALID=D3D10_MESSAGE_ID_UPDATESUBRESOURCE_INVALIDDESTINATIONSTATE + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_SUBRESOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_DESTINATION_SUBRESOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_SUBRESOURCE_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_FORMAT_INVALID=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_SOURCE_SUBRESOURCE_INVALID + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_DEVICE_RESOLVESUBRESOURCE_FORMAT_INVALID + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_BUFFER_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_BUFFER_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_BUFFER_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_BUFFER_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_BUFFER_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_BUFFER_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_TEXTURE1D_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_TEXTURE1D_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE1D_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_TEXTURE1D_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_TEXTURE2D_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_TEXTURE2D_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE2D_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDMAPTYPE=D3D10_MESSAGE_ID_TEXTURE2D_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDMAPTYPE + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDFLAGS=D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_ALREADYMAPPED=D3D10_MESSAGE_ID_TEXTURE3D_MAP_INVALIDFLAGS + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_MAP_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_TEXTURE3D_MAP_ALREADYMAPPED + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_INVALIDSUBRESOURCE=D3D10_MESSAGE_ID_TEXTURE3D_MAP_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_NOTMAPPED=D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_INVALIDSUBRESOURCE + 1,
    D3D10_MESSAGE_ID_CHECKFORMATSUPPORT_FORMAT_DEPRECATED=D3D10_MESSAGE_ID_TEXTURE3D_UNMAP_NOTMAPPED + 1,
    D3D10_MESSAGE_ID_CHECKMULTISAMPLEQUALITYLEVELS_FORMAT_DEPRECATED=D3D10_MESSAGE_ID_CHECKFORMATSUPPORT_FORMAT_DEPRECATED + 1,
    D3D10_MESSAGE_ID_SETEXCEPTIONMODE_UNRECOGNIZEDFLAGS=D3D10_MESSAGE_ID_CHECKMULTISAMPLEQUALITYLEVELS_FORMAT_DEPRECATED + 1,
    D3D10_MESSAGE_ID_SETEXCEPTIONMODE_INVALIDARG_RETURN=D3D10_MESSAGE_ID_SETEXCEPTIONMODE_UNRECOGNIZEDFLAGS + 1,
    D3D10_MESSAGE_ID_SETEXCEPTIONMODE_DEVICEREMOVED_RETURN=D3D10_MESSAGE_ID_SETEXCEPTIONMODE_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_REF_SIMULATING_INFINITELY_FAST_HARDWARE=D3D10_MESSAGE_ID_SETEXCEPTIONMODE_DEVICEREMOVED_RETURN + 1,
    D3D10_MESSAGE_ID_REF_THREADING_MODE=D3D10_MESSAGE_ID_REF_SIMULATING_INFINITELY_FAST_HARDWARE + 1,
    D3D10_MESSAGE_ID_REF_UMDRIVER_EXCEPTION=D3D10_MESSAGE_ID_REF_THREADING_MODE + 1,
    D3D10_MESSAGE_ID_REF_KMDRIVER_EXCEPTION=D3D10_MESSAGE_ID_REF_UMDRIVER_EXCEPTION + 1,
    D3D10_MESSAGE_ID_REF_HARDWARE_EXCEPTION=D3D10_MESSAGE_ID_REF_KMDRIVER_EXCEPTION + 1,
    D3D10_MESSAGE_ID_REF_ACCESSING_INDEXABLE_TEMP_OUT_OF_RANGE=D3D10_MESSAGE_ID_REF_HARDWARE_EXCEPTION + 1,
    D3D10_MESSAGE_ID_REF_PROBLEM_PARSING_SHADER=D3D10_MESSAGE_ID_REF_ACCESSING_INDEXABLE_TEMP_OUT_OF_RANGE + 1,
    D3D10_MESSAGE_ID_REF_OUT_OF_MEMORY=D3D10_MESSAGE_ID_REF_PROBLEM_PARSING_SHADER + 1,
    D3D10_MESSAGE_ID_REF_INFO=D3D10_MESSAGE_ID_REF_OUT_OF_MEMORY + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEXPOS_OVERFLOW=D3D10_MESSAGE_ID_REF_INFO + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINDEXED_INDEXPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_VERTEXPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINDEXED_INDEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_INSTANCEPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_VERTEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INSTANCEPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINSTANCED_INSTANCEPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INDEXPOS_OVERFLOW=D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INSTANCEPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_SHADER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAWINDEXEDINSTANCED_INDEXPOS_OVERFLOW + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SEMANTICNAME_NOT_FOUND=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_SHADER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERINDEX=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SEMANTICNAME_NOT_FOUND + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_COMPONENTTYPE=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERINDEX + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERMASK=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_COMPONENTTYPE + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SYSTEMVALUE=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_REGISTERMASK + 1,
    D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_NEVERWRITTEN_ALWAYSREADS=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_SYSTEMVALUE + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_SHADER_LINKAGE_NEVERWRITTEN_ALWAYSREADS + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INPUTLAYOUT_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_INPUTLAYOUT_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_CONSTANT_BUFFER_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SHADERRESOURCEVIEW_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VIEW_DIMENSION_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_SHADERRESOURCEVIEW_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_STRIDE_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_VIEW_DIMENSION_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_STRIDE_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_BUFFER_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_FORMAT_INVALID=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_TOO_SMALL=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_FORMAT_INVALID + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_GS_INPUT_PRIMITIVE_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_BUFFER_TOO_SMALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_RETURN_TYPE_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_GS_INPUT_PRIMITIVE_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_POSITION_NOT_PRESENT=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_RETURN_TYPE_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_NOT_SET=D3D10_MESSAGE_ID_DEVICE_DRAW_POSITION_NOT_PRESENT + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_BOUND_RESOURCE_MAPPED=D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_NOT_SET + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_PRIMITIVETOPOLOGY=D3D10_MESSAGE_ID_DEVICE_DRAW_BOUND_RESOURCE_MAPPED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_PRIMITIVETOPOLOGY + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_STRIDE_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_VERTEX_STRIDE_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_OFFSET_UNALIGNED=D3D10_MESSAGE_ID_DEVICE_DRAW_INDEX_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_LD_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_OUTPUT_STREAM_OFFSET_UNALIGNED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_LD_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_C_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_MULTISAMPLE_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_SAMPLE_C_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SO_TARGETS_BOUND_WITHOUT_SOURCE=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_MULTISAMPLE_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SO_STRIDE_LARGER_THAN_BUFFER=D3D10_MESSAGE_ID_DEVICE_DRAW_SO_TARGETS_BOUND_WITHOUT_SOURCE + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OM_RENDER_TARGET_DOES_NOT_SUPPORT_BLENDING=D3D10_MESSAGE_ID_DEVICE_DRAW_SO_STRIDE_LARGER_THAN_BUFFER + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_OM_DUAL_SOURCE_BLENDING_CAN_ONLY_HAVE_RENDER_TARGET_0=D3D10_MESSAGE_ID_DEVICE_DRAW_OM_RENDER_TARGET_DOES_NOT_SUPPORT_BLENDING + 1,
    D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_AT_FAULT=D3D10_MESSAGE_ID_DEVICE_DRAW_OM_DUAL_SOURCE_BLENDING_CAN_ONLY_HAVE_RENDER_TARGET_0 + 1,
    D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_POSSIBLY_AT_FAULT=D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_AT_FAULT + 1,
    D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_NOT_AT_FAULT=D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_POSSIBLY_AT_FAULT + 1,
    D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_INVALIDARG_RETURN=D3D10_MESSAGE_ID_DEVICE_REMOVAL_PROCESS_NOT_AT_FAULT + 1,
    D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_INVALIDARG_RETURN + 1,
    D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_BADINTERFACE_RETURN=D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_VIEWPORT_NOT_SET=D3D10_MESSAGE_ID_DEVICE_OPEN_SHARED_RESOURCE_BADINTERFACE_RETURN + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TRAILING_DIGIT_IN_SEMANTIC=D3D10_MESSAGE_ID_DEVICE_DRAW_VIEWPORT_NOT_SET + 1,
    D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_TRAILING_DIGIT_IN_SEMANTIC=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TRAILING_DIGIT_IN_SEMANTIC + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_DENORMFLUSH=D3D10_MESSAGE_ID_CREATEGEOMETRYSHADERWITHSTREAMOUTPUT_TRAILING_DIGIT_IN_SEMANTIC + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_INVALIDVIEW=D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_DENORMFLUSH + 1,
    D3D10_MESSAGE_ID_DEVICE_SETTEXTFILTERSIZE_INVALIDDIMENSIONS=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_INVALIDVIEW + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_MISMATCH=D3D10_MESSAGE_ID_DEVICE_SETTEXTFILTERSIZE_INVALIDDIMENSIONS + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TYPE_MISMATCH=D3D10_MESSAGE_ID_DEVICE_DRAW_SAMPLER_MISMATCH + 1,
    D3D10_MESSAGE_ID_BLENDSTATE_GETDESC_LEGACY=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_TYPE_MISMATCH + 1,
    D3D10_MESSAGE_ID_SHADERRESOURCEVIEW_GETDESC_LEGACY=D3D10_MESSAGE_ID_BLENDSTATE_GETDESC_LEGACY + 1,
    D3D10_MESSAGE_ID_CREATEQUERY_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_SHADERRESOURCEVIEW_GETDESC_LEGACY + 1,
    D3D10_MESSAGE_ID_CREATEPREDICATE_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATEQUERY_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFRANGE_COUNTER=D3D10_MESSAGE_ID_CREATEPREDICATE_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_SIMULTANEOUS_ACTIVE_COUNTERS_EXHAUSTED=D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFRANGE_COUNTER + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER=D3D10_MESSAGE_ID_CREATECOUNTER_SIMULTANEOUS_ACTIVE_COUNTERS_EXHAUSTED + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFMEMORY_RETURN=D3D10_MESSAGE_ID_CREATECOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_NONEXCLUSIVE_RETURN=D3D10_MESSAGE_ID_CREATECOUNTER_OUTOFMEMORY_RETURN + 1,
    D3D10_MESSAGE_ID_CREATECOUNTER_NULLDESC=D3D10_MESSAGE_ID_CREATECOUNTER_NONEXCLUSIVE_RETURN + 1,
    D3D10_MESSAGE_ID_CHECKCOUNTER_OUTOFRANGE_COUNTER=D3D10_MESSAGE_ID_CREATECOUNTER_NULLDESC + 1,
    D3D10_MESSAGE_ID_CHECKCOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER=D3D10_MESSAGE_ID_CHECKCOUNTER_OUTOFRANGE_COUNTER + 1,
    D3D10_MESSAGE_ID_SETPREDICATION_INVALID_PREDICATE_STATE=D3D10_MESSAGE_ID_CHECKCOUNTER_UNSUPPORTED_WELLKNOWN_COUNTER + 1,
    D3D10_MESSAGE_ID_QUERY_BEGIN_UNSUPPORTED=D3D10_MESSAGE_ID_SETPREDICATION_INVALID_PREDICATE_STATE + 1,
    D3D10_MESSAGE_ID_PREDICATE_BEGIN_DURING_PREDICATION=D3D10_MESSAGE_ID_QUERY_BEGIN_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_QUERY_BEGIN_DUPLICATE=D3D10_MESSAGE_ID_PREDICATE_BEGIN_DURING_PREDICATION + 1,
    D3D10_MESSAGE_ID_QUERY_BEGIN_ABANDONING_PREVIOUS_RESULTS=D3D10_MESSAGE_ID_QUERY_BEGIN_DUPLICATE + 1,
    D3D10_MESSAGE_ID_PREDICATE_END_DURING_PREDICATION=D3D10_MESSAGE_ID_QUERY_BEGIN_ABANDONING_PREVIOUS_RESULTS + 1,
    D3D10_MESSAGE_ID_QUERY_END_ABANDONING_PREVIOUS_RESULTS=D3D10_MESSAGE_ID_PREDICATE_END_DURING_PREDICATION + 1,
    D3D10_MESSAGE_ID_QUERY_END_WITHOUT_BEGIN=D3D10_MESSAGE_ID_QUERY_END_ABANDONING_PREVIOUS_RESULTS + 1,
    D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_DATASIZE=D3D10_MESSAGE_ID_QUERY_END_WITHOUT_BEGIN + 1,
    D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_FLAGS=D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_DATASIZE + 1,
    D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_CALL=D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_FLAGS + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_PS_OUTPUT_TYPE_MISMATCH=D3D10_MESSAGE_ID_QUERY_GETDATA_INVALID_CALL + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_GATHER_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_DRAW_PS_OUTPUT_TYPE_MISMATCH + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_USE_OF_CENTER_MULTISAMPLE_PATTERN=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_FORMAT_GATHER_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_STRIDE_TOO_LARGE=D3D10_MESSAGE_ID_DEVICE_DRAW_INVALID_USE_OF_CENTER_MULTISAMPLE_PATTERN + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_INVALIDRANGE=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_STRIDE_TOO_LARGE + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_EMPTY_LAYOUT=D3D10_MESSAGE_ID_DEVICE_IASETVERTEXBUFFERS_INVALIDRANGE + 1,
    D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_SAMPLE_COUNT_MISMATCH=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_EMPTY_LAYOUT + 1,
    D3D10_MESSAGE_ID_D3D10_MESSAGES_END=D3D10_MESSAGE_ID_DEVICE_DRAW_RESOURCE_SAMPLE_COUNT_MISMATCH + 1,
    D3D10_MESSAGE_ID_D3D10L9_MESSAGES_START=$100000,
    D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_STENCIL_NO_TWO_SIDED=D3D10_MESSAGE_ID_D3D10L9_MESSAGES_START + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthBiasClamp_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATEDEPTHSTENCILSTATE_STENCIL_NO_TWO_SIDED + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_COMPARISON_SUPPORT=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthBiasClamp_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_EXCESSIVE_ANISOTROPY=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_COMPARISON_SUPPORT + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_OUT_OF_RANGE=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_EXCESSIVE_ANISOTROPY + 1,
    D3D10_MESSAGE_ID_VSSETSAMPLERS_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_OUT_OF_RANGE + 1,
    D3D10_MESSAGE_ID_VSSETSAMPLERS_TOO_MANY_SAMPLERS=D3D10_MESSAGE_ID_VSSETSAMPLERS_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_PSSETSAMPLERS_TOO_MANY_SAMPLERS=D3D10_MESSAGE_ID_VSSETSAMPLERS_TOO_MANY_SAMPLERS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_ARRAYS=D3D10_MESSAGE_ID_PSSETSAMPLERS_TOO_MANY_SAMPLERS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_VB_AND_IB_BIND=D3D10_MESSAGE_ID_CREATERESOURCE_NO_ARRAYS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_TEXTURE_1D=D3D10_MESSAGE_ID_CREATERESOURCE_NO_VB_AND_IB_BIND + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_OUT_OF_RANGE=D3D10_MESSAGE_ID_CREATERESOURCE_NO_TEXTURE_1D + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_SHADER_RESOURCE=D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_OUT_OF_RANGE + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_TOO_MANY_RENDER_TARGETS=D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_SHADER_RESOURCE + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_DIFFERING_BIT_DEPTHS=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_TOO_MANY_RENDER_TARGETS + 1,
    D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_BAD_BUFFER_INDEX=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_DIFFERING_BIT_DEPTHS + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_TOO_MANY_VIEWPORTS=D3D10_MESSAGE_ID_IASETVERTEXBUFFERS_BAD_BUFFER_INDEX + 1,
    D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_ADJACENCY_UNSUPPORTED=D3D10_MESSAGE_ID_DEVICE_RSSETVIEWPORTS_TOO_MANY_VIEWPORTS + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_TOO_MANY_SCISSORS=D3D10_MESSAGE_ID_DEVICE_IASETPRIMITIVETOPOLOGY_ADJACENCY_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_ONLY_TEXTURE_2D_WITHIN_GPU_MEMORY=D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_TOO_MANY_SCISSORS + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_3D_READBACK=D3D10_MESSAGE_ID_COPYRESOURCE_ONLY_TEXTURE_2D_WITHIN_GPU_MEMORY + 1,
    D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_ONLY_READBACK=D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_3D_READBACK + 1,
    D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNSUPPORTED_FORMAT=D3D10_MESSAGE_ID_COPYRESOURCE_NO_TEXTURE_ONLY_READBACK + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_ALPHA_TO_COVERAGE=D3D10_MESSAGE_ID_CREATEINPUTLAYOUT_UNSUPPORTED_FORMAT + 1,
    D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthClipEnable_MUST_BE_TRUE=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_ALPHA_TO_COVERAGE + 1,
    D3D10_MESSAGE_ID_DRAWINDEXED_STARTINDEXLOCATION_MUST_BE_POSITIVE=D3D10_MESSAGE_ID_CREATERASTERIZERSTATE_DepthClipEnable_MUST_BE_TRUE + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_MUST_USE_LOWEST_LOD=D3D10_MESSAGE_ID_DRAWINDEXED_STARTINDEXLOCATION_MUST_BE_POSITIVE + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MINLOD_MUST_NOT_BE_FRACTIONAL=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_MUST_USE_LOWEST_LOD + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MAXLOD_MUST_BE_FLT_MAX=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MINLOD_MUST_NOT_BE_FRACTIONAL + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_FIRSTARRAYSLICE_MUST_BE_ZERO=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_MAXLOD_MUST_BE_FLT_MAX + 1,
    D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_CUBES_MUST_HAVE_6_SIDES=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_FIRSTARRAYSLICE_MUST_BE_ZERO + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_RENDER_TARGET=D3D10_MESSAGE_ID_CREATESHADERRESOURCEVIEW_CUBES_MUST_HAVE_6_SIDES + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_DWORD_INDEX_BUFFER=D3D10_MESSAGE_ID_CREATERESOURCE_NOT_BINDABLE_AS_RENDER_TARGET + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_MSAA_PRECLUDES_SHADER_RESOURCE=D3D10_MESSAGE_ID_CREATERESOURCE_NO_DWORD_INDEX_BUFFER + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_PRESENTATION_PRECLUDES_SHADER_RESOURCE=D3D10_MESSAGE_ID_CREATERESOURCE_MSAA_PRECLUDES_SHADER_RESOURCE + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_BLEND_ENABLE=D3D10_MESSAGE_ID_CREATERESOURCE_PRESENTATION_PRECLUDES_SHADER_RESOURCE + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_WRITE_MASKS=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_BLEND_ENABLE + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_STREAM_OUT=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_INDEPENDENT_WRITE_MASKS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_VB_IB_FOR_BUFFERS=D3D10_MESSAGE_ID_CREATERESOURCE_NO_STREAM_OUT + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NO_AUTOGEN_FOR_VOLUMES=D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_VB_IB_FOR_BUFFERS + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_DXGI_FORMAT_R8G8B8A8_CANNOT_BE_SHARED=D3D10_MESSAGE_ID_CREATERESOURCE_NO_AUTOGEN_FOR_VOLUMES + 1,
    D3D10_MESSAGE_ID_VSSHADERRESOURCES_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATERESOURCE_DXGI_FORMAT_R8G8B8A8_CANNOT_BE_SHARED + 1,
    D3D10_MESSAGE_ID_GEOMETRY_SHADER_NOT_SUPPORTED=D3D10_MESSAGE_ID_VSSHADERRESOURCES_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_STREAM_OUT_NOT_SUPPORTED=D3D10_MESSAGE_ID_GEOMETRY_SHADER_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_TEXT_FILTER_NOT_SUPPORTED=D3D10_MESSAGE_ID_STREAM_OUT_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_SEPARATE_ALPHA_BLEND=D3D10_MESSAGE_ID_TEXT_FILTER_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_MRT_BLEND=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_SEPARATE_ALPHA_BLEND + 1,
    D3D10_MESSAGE_ID_CREATEBLENDSTATE_OPERATION_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATEBLENDSTATE_NO_MRT_BLEND + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_MIRRORONCE=D3D10_MESSAGE_ID_CREATEBLENDSTATE_OPERATION_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_DRAWINSTANCED_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_NO_MIRRORONCE + 1,
    D3D10_MESSAGE_ID_DRAWINDEXEDINSTANCED_NOT_SUPPORTED_BELOW_9_3=D3D10_MESSAGE_ID_DRAWINSTANCED_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_DRAWINDEXED_POINTLIST_UNSUPPORTED=D3D10_MESSAGE_ID_DRAWINDEXEDINSTANCED_NOT_SUPPORTED_BELOW_9_3 + 1,
    D3D10_MESSAGE_ID_SETBLENDSTATE_SAMPLE_MASK_CANNOT_BE_ZERO=D3D10_MESSAGE_ID_DRAWINDEXED_POINTLIST_UNSUPPORTED + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_EXCEEDS_FEATURE_LEVEL_DEFINITION=D3D10_MESSAGE_ID_SETBLENDSTATE_SAMPLE_MASK_CANNOT_BE_ZERO + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_SINGLE_MIP_LEVEL_DEPTH_STENCIL_SUPPORTED=D3D10_MESSAGE_ID_CREATERESOURCE_DIMENSION_EXCEEDS_FEATURE_LEVEL_DEFINITION + 1,
    D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_NEGATIVESCISSOR=D3D10_MESSAGE_ID_CREATERESOURCE_ONLY_SINGLE_MIP_LEVEL_DEPTH_STENCIL_SUPPORTED + 1,
    D3D10_MESSAGE_ID_SLOT_ZERO_MUST_BE_D3D10_INPUT_PER_VERTEX_DATA=D3D10_MESSAGE_ID_DEVICE_RSSETSCISSORRECTS_NEGATIVESCISSOR + 1,
    D3D10_MESSAGE_ID_CREATERESOURCE_NON_POW_2_MIPMAP=D3D10_MESSAGE_ID_SLOT_ZERO_MUST_BE_D3D10_INPUT_PER_VERTEX_DATA + 1,
    D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_NOT_SUPPORTED=D3D10_MESSAGE_ID_CREATERESOURCE_NON_POW_2_MIPMAP + 1,
    D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_SRGB_MRT=D3D10_MESSAGE_ID_CREATESAMPLERSTATE_BORDER_NOT_SUPPORTED + 1,
    D3D10_MESSAGE_ID_D3D10L9_MESSAGES_END=D3D10_MESSAGE_ID_OMSETRENDERTARGETS_NO_SRGB_MRT + 1
  );
  PTD3D10_MessageID=^TD3D10_MessageID;
  D3D10_MESSAGE_ID=TD3D10_MessageID;
  PD3D10_MESSAGE_ID=^TD3D10_MessageID;

  TD3D10_Message=record
    Category:TD3D10_MessageCategory;
    Severity:TD3D10_MessageSeverity;
    ID:TD3D10_MessageID;
    pDescription:PAnsiChar;
    DescriptionByteLength:SIZE_T;
  end;
  PTD3D10_Message=^TD3D10_Message;
  D3D10_MESSAGE=TD3D10_Message;
  PD3D10_MESSAGE=^TD3D10_Message;

  TD3D10_InfoQueueFilterDesc=record
    NumCategories:LongWord;
    pCategoryList:PTD3D10_MessageCategory;
    NumSeverities:LongWord;
    pSeverityList:PTD3D10_MessageSeverity;
    NumIDs:LongWord;
    pIDList:PTD3D10_MessageID;
  end;
  PTD3D10_InfoQueueFilterDesc=^TD3D10_InfoQueueFilterDesc;
  D3D10_INFO_QUEUE_FILTER_DESC=TD3D10_InfoQueueFilterDesc;
  PD3D10_INFO_QUEUE_FILTER_DESC=^TD3D10_InfoQueueFilterDesc;

  TD3D10_InfoQueueFilter=record
    AllowList:TD3D10_InfoQueueFilterDesc;
    DenyList:TD3D10_InfoQueueFilterDesc;
  end;
  PTD3D10_InfoQueueFilter=^TD3D10_InfoQueueFilter;
  D3D10_INFO_QUEUE_FILTER=TD3D10_InfoQueueFilter;
  PD3D10_INFO_QUEUE_FILTER=^TD3D10_InfoQueueFilter;

  ID3D10InfoQueue=interface(IUnknown)
    ['{1B940B17-2642-4D1F-AB1F-B99BAD0C395F}']
    function SetMessageCountLimit
    (
      MessageCountLimit:LargeInt (* __in *)
    ):HResult; stdcall;

    procedure ClearStoredMessages; stdcall;

    function GetMessage
    (
      MessageIndex:LargeInt; (* __in *)
      pMessage:PTD3D10_Message; (* __out_bcount_opt(*pMessageByteLength) *)
      var MessageByteLength:SIZE_T (* __inout *)
    ):HResult; stdcall;

    function GetNumMessagesAllowedByStorageFilter:LargeInt; stdcall;

    function GetNumMessagesDeniedByStorageFilter:LargeInt; stdcall;

    function GetNumStoredMessages:LargeInt; stdcall;

    function GetNumStoredMessagesAllowedByRetrievalFilter:LargeInt; stdcall;

    function GetNumMessagesDiscardedByMessageCountLimit:LargeInt; stdcall;

    function GetMessageCountLimit:LargeInt; stdcall;

    function AddStorageFilterEntries
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    function GetStorageFilter
    (
      pFilter:PTD3D10_InfoQueueFilter; (* __out_bcount_opt(*pFilterByteLength) *)
      var FilterByteLength:SIZE_T (* __inout *)
    ):HResult; stdcall;

    procedure ClearStorageFilter; stdcall;

    function PushEmptyStorageFilter:HResult; stdcall;

    function PushCopyOfStorageFilter:HResult; stdcall;

    function PushStorageFilter
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    procedure PopStorageFilter; stdcall;

    function GetStorageFilterStackSize:LongWord; stdcall;

    function AddRetrievalFilterEntries
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    function GetRetrievalFilter
    (
      pFilter:PTD3D10_InfoQueueFilter; (* __out_bcount_opt(*pFilterByteLength) *)
      var FilterByteLength:SIZE_T (* __inout *)
    ):HResult; stdcall;

    procedure ClearRetrievalFilter; stdcall;

    function PushEmptyRetrievalFilter:HResult; stdcall;

    function PushCopyOfRetrievalFilter:HResult; stdcall;

    function PushRetrievalFilter
    (
      const Filter:TD3D10_InfoQueueFilter (* __in *)
    ):HResult; stdcall;

    procedure PopRetrievalFilter; stdcall;

    function GetRetrievalFilterStackSize:LongWord; stdcall;

    function AddMessage
    (
      Category:TD3D10_MessageCategory; (* __in *)
      Severity:TD3D10_MessageSeverity; (* __in *)
      ID:TD3D10_MessageID; (* __in *)
      pDescription:PAnsiChar (* __in *)
    ):HResult; stdcall;

    function AddApplicationMessage
    (
      Severity:TD3D10_MessageSeverity; (* __in *)
      pDescription:PAnsiChar (* __in *)
    ):HResult; stdcall;

    function SetBreakOnCategory
    (
      Category:TD3D10_MessageCategory; (* __in *)
      Enable:LongBool (* __in *)
    ):HResult; stdcall;

    function SetBreakOnSeverity
    (
      Severity:TD3D10_MessageSeverity; (* __in *)
      Enable:LongBool (* __in *)
    ):HResult; stdcall;

    function SetBreakOnID
    (
      ID:TD3D10_MessageID; (* __in *)
      Enable:LongBool (* __in *)
    ):HResult; stdcall;

    function GetBreakOnCategory
    (
      Category:TD3D10_MessageCategory (* __in *)
    ):LongBool; stdcall;

    function GetBreakOnSeverity
    (
      Severity:TD3D10_MessageSeverity (* __in *)
    ):LongBool; stdcall;

    function GetBreakOnID
    (
      ID:TD3D10_MessageID (* __in *)
    ):LongBool; stdcall;

    procedure SetMuteDebugOutput
    (
      Mute:LongBool (* __in *)
    ); stdcall;

    function GetMuteDebugOutput:LongBool; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10SDKLayers.h"
///////////////////////////////////////////////////////////////////////////////


function MAKE_D3D10_HRESULT(i_Code:LongWord):LongWord;
begin
  Result:=D3D10_HRESULT_Base or i_Code;
end;

function MAKE_D3D10_STATUS(i_Code:LongWord):LongWord;
begin
  Result:=D3D10_STATUS_Base or i_Code;
end;

function D3D10_BYTES_FROM_BITS(x:LongWord):LongWord;
begin
  Result:=(x+7) mod 8;
end;

function D3D10_TX_VERSION(_Major,_Minor:Byte):LongWord;
begin
  Result:=(Byte('T') shl 24) or (Byte('X') shl 16) or (_Major shl 8) or _Minor;
end;

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10_1.h"
///////////////////////////////////////////////////////////////////////////////

const
  D3D10_1_DEFAULT_SAMPLE_MASK=$ffffffff;
  D3D10_1_FLOAT16_FUSED_TOLERANCE_IN_ULP=0.6;
  D3D10_1_FLOAT32_TO_INTEGER_TOLERANCE_IN_ULP=0.6;
  D3D10_1_GS_INPUT_REGISTER_COUNT=32;
  D3D10_1_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT=32;
  D3D10_1_IA_VERTEX_INPUT_STRUCTURE_ELEMENTS_COMPONENTS=128;
  D3D10_1_IA_VERTEX_INPUT_STRUCTURE_ELEMENT_COUNT=32;
  D3D10_1_PS_OUTPUT_MASK_REGISTER_COMPONENTS=1;
  D3D10_1_PS_OUTPUT_MASK_REGISTER_COMPONENT_BIT_COUNT=32;
  D3D10_1_PS_OUTPUT_MASK_REGISTER_COUNT=1;
  D3D10_1_SHADER_MAJOR_VERSION=4;
  D3D10_1_SHADER_MINOR_VERSION=1;
  D3D10_1_SO_BUFFER_MAX_STRIDE_IN_BYTES=2048;
  D3D10_1_SO_BUFFER_MAX_WRITE_WINDOW_IN_BYTES=256;
  D3D10_1_SO_BUFFER_SLOT_COUNT=4;
  D3D10_1_SO_MULTIPLE_BUFFER_ELEMENTS_PER_BUFFER=1;
  D3D10_1_SO_SINGLE_BUFFER_COMPONENT_LIMIT=64;
  D3D10_1_STANDARD_VERTEX_ELEMENT_COUNT=32;
  D3D10_1_SUBPIXEL_FRACTIONAL_BIT_COUNT=8;
  D3D10_1_VS_INPUT_REGISTER_COUNT=32;
  D3D10_1_VS_OUTPUT_REGISTER_COUNT=32;
  D3D10_1_SDK_VERSION=( 0 + $20 ) ;

type
  ID3D10BlendState1=interface;
  PID3D10BlendState1=^ID3D10BlendState1;

  ID3D10ShaderResourceView1=interface;
  PID3D10ShaderResourceView1=^ID3D10ShaderResourceView1;

  ID3D10Device1=interface;
  PID3D10Device1=^ID3D10Device1;

  TD3D10_FeatureLevel1=
  (
    D3D10_FEATURE_LEVEL_10_0=$a000,
    D3D10_FEATURE_LEVEL_10_1=$a100,
    D3D10_FEATURE_LEVEL_9_1=$9100,
    D3D10_FEATURE_LEVEL_9_2=$9200,
    D3D10_FEATURE_LEVEL_9_3=$9300
  );
  PTD3D10_FeatureLevel1=^TD3D10_FeatureLevel1;
  D3D10_FEATURE_LEVEL1=TD3D10_FeatureLevel1;
  PD3D10_FEATURE_LEVEL1=^TD3D10_FeatureLevel1;

  TD3D10_RenderTargetBlendDesc1=record
    BlendEnable:LongBool;
    SrcBlend:TD3D10_Blend;
    DestBlend:TD3D10_Blend;
    BlendOp:TD3D10_BlendOp;
    SrcBlendAlpha:TD3D10_Blend;
    DestBlendAlpha:TD3D10_Blend;
    BlendOpAlpha:TD3D10_BlendOp;
    RenderTargetWriteMask:Byte;
  end;
  PTD3D10_RenderTargetBlendDesc1=^TD3D10_RenderTargetBlendDesc1;
  D3D10_RENDER_TARGET_BLEND_DESC1=TD3D10_RenderTargetBlendDesc1;
  PD3D10_RENDER_TARGET_BLEND_DESC1=^TD3D10_RenderTargetBlendDesc1;

  TD3D10_BlendDesc1=record
    AlphaToCoverageEnable:LongBool;
    IndependentBlendEnable:LongBool;
    RenderTarget:array[0..7] of TD3D10_RenderTargetBlendDesc1;
  end;
  PTD3D10_BlendDesc1=^TD3D10_BlendDesc1;
  D3D10_BLEND_DESC1=TD3D10_BlendDesc1;
  PD3D10_BLEND_DESC1=^TD3D10_BlendDesc1;

  ID3D10BlendState1=interface(ID3D10BlendState)
    ['{EDAD8D99-8A35-4D6D-8566-2EA276CDE161}']
    procedure GetDesc1
    (
      out Desc:TD3D10_BlendDesc1 (* __out *)
    ); stdcall;
  end;

  TD3D10_TexcubeArraySrv1=record
    MostDetailedMip:LongWord;
    MipLevels:LongWord;
    First2DArrayFace:LongWord;
    NumCubes:LongWord;
  end;
  PTD3D10_TexcubeArraySrv1=^TD3D10_TexcubeArraySrv1;
  D3D10_TEXCUBE_ARRAY_SRV1=TD3D10_TexcubeArraySrv1;
  PD3D10_TEXCUBE_ARRAY_SRV1=^TD3D10_TexcubeArraySrv1;

  TD3D10_SrvDimension1=
  (
    D3D10_1_SRV_DIMENSION_UNKNOWN=0,
    D3D10_1_SRV_DIMENSION_BUFFER=1,
    D3D10_1_SRV_DIMENSION_TEXTURE1D=2,
    D3D10_1_SRV_DIMENSION_TEXTURE1DARRAY=3,
    D3D10_1_SRV_DIMENSION_TEXTURE2D=4,
    D3D10_1_SRV_DIMENSION_TEXTURE2DARRAY=5,
    D3D10_1_SRV_DIMENSION_TEXTURE2DMS=6,
    D3D10_1_SRV_DIMENSION_TEXTURE2DMSARRAY=7,
    D3D10_1_SRV_DIMENSION_TEXTURE3D=8,
    D3D10_1_SRV_DIMENSION_TEXTURECUBE=9,
    D3D10_1_SRV_DIMENSION_TEXTURECUBEARRAY=10
  );
  PTD3D10_SrvDimension1=^TD3D10_SrvDimension1;
  D3D10_SRV_DIMENSION1=TD3D10_SrvDimension1;
  PD3D10_SRV_DIMENSION1=^TD3D10_SrvDimension1;

  TD3D10_ShaderResourceViewDesc1=record
    Format:TDXGI_FORMAT;
    ViewDimension:TD3D10_SrvDimension1;
    case Integer of
      0: (Buffer:D3D10_BUFFER_SRV);
      1: (Texture1D:D3D10_TEX1D_SRV);
      2: (Texture1DArray:D3D10_TEX1D_ARRAY_SRV);
      3: (Texture2D:D3D10_TEX2D_SRV);
      4: (Texture2DArray:D3D10_TEX2D_ARRAY_SRV);
      5: (Texture2DMS:D3D10_TEX2DMS_SRV);
      6: (Texture2DMSArray:D3D10_TEX2DMS_ARRAY_SRV);
      7: (Texture3D:D3D10_TEX3D_SRV);
      8: (TextureCube:D3D10_TEXCUBE_SRV);
      9: (TextureCubeArray:D3D10_TEXCUBE_ARRAY_SRV1);
  end;
  PTD3D10_ShaderResourceViewDesc1=^TD3D10_ShaderResourceViewDesc1;
  D3D10_SHADER_RESOURCE_VIEW_DESC1=TD3D10_ShaderResourceViewDesc1;
  PD3D10_SHADER_RESOURCE_VIEW_DESC1=^TD3D10_ShaderResourceViewDesc1;

  ID3D10ShaderResourceView1=interface(ID3D10ShaderResourceView)
    ['{9B7E4C87-342C-4106-A19F-4F2704F689F0}']
    procedure GetDesc1
    (
      out Desc:TD3D10_ShaderResourceViewDesc1 (* __out *)
    ); stdcall;
  end;

  TD3D10_StandardMultisampleQualityLevels=
  (
    D3D10_STANDARD_MULTISAMPLE_PATTERN=Integer($ffffffff),
    D3D10_CENTER_MULTISAMPLE_PATTERN=Integer($fffffffe)
  );
  PTD3D10_StandardMultisampleQualityLevels=^TD3D10_StandardMultisampleQualityLevels;
  D3D10_STANDARD_MULTISAMPLE_QUALITY_LEVELS=TD3D10_StandardMultisampleQualityLevels;
  PD3D10_STANDARD_MULTISAMPLE_QUALITY_LEVELS=^TD3D10_StandardMultisampleQualityLevels;

  ID3D10Device1=interface(ID3D10Device)
    ['{9B7E4C8F-342C-4106-A19F-4F2704F689F0}']
    function CreateShaderResourceView1
    (
      Resource:ID3D10Resource; (* __in *)
      pDesc:PTD3D10_ShaderResourceViewDesc1; (* __in_opt *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pSRView:PID3D10ShaderResourceView1{$ELSE}out SRView:ID3D10ShaderResourceView1{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function CreateBlendState1
    (
      const BlendStateDesc:TD3D10_BlendDesc1; (* __in *)
      {$IFDEF UsePointersForOptionalOutputInterfaces}pBlendState:PID3D10BlendState1{$ELSE}out BlendState:ID3D10BlendState1{$ENDIF} (* __out_opt *)
    ):HResult; stdcall;

    function GetFeatureLevel:TD3D10_FeatureLevel1; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10_1.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D3D10_1Shader.h"
///////////////////////////////////////////////////////////////////////////////

type
  TD3D10_ShaderDebugRegtype=
  (
    D3D10_SHADER_DEBUG_REG_INPUT,
    D3D10_SHADER_DEBUG_REG_OUTPUT,
    D3D10_SHADER_DEBUG_REG_CBUFFER,
    D3D10_SHADER_DEBUG_REG_TBUFFER,
    D3D10_SHADER_DEBUG_REG_TEMP,
    D3D10_SHADER_DEBUG_REG_TEMPARRAY,
    D3D10_SHADER_DEBUG_REG_TEXTURE,
    D3D10_SHADER_DEBUG_REG_SAMPLER,
    D3D10_SHADER_DEBUG_REG_IMMEDIATECBUFFER,
    D3D10_SHADER_DEBUG_REG_LITERAL,
    D3D10_SHADER_DEBUG_REG_UNUSED,
    D3D11_SHADER_DEBUG_REG_INTERFACE_POINTERS
  );
  PTD3D10_ShaderDebugRegtype=^TD3D10_ShaderDebugRegtype;
  D3D10_SHADER_DEBUG_REGTYPE=TD3D10_ShaderDebugRegtype;
  PD3D10_SHADER_DEBUG_REGTYPE=^TD3D10_ShaderDebugRegtype;

  TD3D10_ShaderDebugScopetype=
  (
    D3D10_SHADER_DEBUG_SCOPE_GLOBAL,
    D3D10_SHADER_DEBUG_SCOPE_BLOCK,
    D3D10_SHADER_DEBUG_SCOPE_FORLOOP,
    D3D10_SHADER_DEBUG_SCOPE_STRUCT,
    D3D10_SHADER_DEBUG_SCOPE_FUNC_PARAMS,
    D3D10_SHADER_DEBUG_SCOPE_STATEBLOCK,
    D3D10_SHADER_DEBUG_SCOPE_NAMESPACE,
    D3D10_SHADER_DEBUG_SCOPE_ANNOTATION
  );
  PTD3D10_ShaderDebugScopetype=^TD3D10_ShaderDebugScopetype;
  D3D10_SHADER_DEBUG_SCOPETYPE=TD3D10_ShaderDebugScopetype;
  PD3D10_SHADER_DEBUG_SCOPETYPE=^TD3D10_ShaderDebugScopetype;

  TD3D10_ShaderDebugVartype=
  (
    D3D10_SHADER_DEBUG_VAR_VARIABLE,
    D3D10_SHADER_DEBUG_VAR_FUNCTION
  );
  PTD3D10_ShaderDebugVartype=^TD3D10_ShaderDebugVartype;
  D3D10_SHADER_DEBUG_VARTYPE=TD3D10_ShaderDebugVartype;
  PD3D10_SHADER_DEBUG_VARTYPE=^TD3D10_ShaderDebugVartype;

  TD3D10_ShaderDebugTokenInfo=record
    _File:LongWord;
    Line:LongWord;
    Column:LongWord;
    TokenLength:LongWord;
    TokenId:LongWord;
  end;
  PTD3D10_ShaderDebugTokenInfo=^TD3D10_ShaderDebugTokenInfo;
  D3D10_SHADER_DEBUG_TOKEN_INFO=TD3D10_ShaderDebugTokenInfo;
  PD3D10_SHADER_DEBUG_TOKEN_INFO=^TD3D10_ShaderDebugTokenInfo;

  TD3D10_ShaderDebugVarInfo=record
    TokenId:LongWord;
    _Type:TD3D10_ShaderVariableType;
    _Register:LongWord;
    Component:LongWord;
    ScopeVar:LongWord;
    ScopeVarOffset:LongWord;
  end;
  PTD3D10_ShaderDebugVarInfo=^TD3D10_ShaderDebugVarInfo;
  D3D10_SHADER_DEBUG_VAR_INFO=TD3D10_ShaderDebugVarInfo;
  PD3D10_SHADER_DEBUG_VAR_INFO=^TD3D10_ShaderDebugVarInfo;

  TD3D10_ShaderDebugInputInfo=record
    _Var:LongWord;
    InitialRegisterSet:TD3D10_ShaderDebugRegtype;
    InitialBank:LongWord;
    InitialRegister:LongWord;
    InitialComponent:LongWord;
    InitialValue:LongWord;
  end;
  PTD3D10_ShaderDebugInputInfo=^TD3D10_ShaderDebugInputInfo;
  D3D10_SHADER_DEBUG_INPUT_INFO=TD3D10_ShaderDebugInputInfo;
  PD3D10_SHADER_DEBUG_INPUT_INFO=^TD3D10_ShaderDebugInputInfo;

  TD3D10_ShaderDebugScopevarInfo=record
    TokenId:LongWord;
    VarType:TD3D10_ShaderDebugVartype;
    _Class:TD3D10_ShaderVariableClass;
    Rows:LongWord;
    Columns:LongWord;
    StructMemberScope:LongWord;
    ArrayIndices:LongWord;
    ArrayElements:LongWord;
    ArrayStrides:LongWord;
    Variables:LongWord;
    FirstVariable:LongWord;
  end;
  PTD3D10_ShaderDebugScopevarInfo=^TD3D10_ShaderDebugScopevarInfo;
  D3D10_SHADER_DEBUG_SCOPEVAR_INFO=TD3D10_ShaderDebugScopevarInfo;
  PD3D10_SHADER_DEBUG_SCOPEVAR_INFO=^TD3D10_ShaderDebugScopevarInfo;

  TD3D10_ShaderDebugScopeInfo=record
    ScopeType:TD3D10_ShaderDebugScopetype;
    Name:LongWord;
    NameLen:LongWord;
    Variables:LongWord;
    VariableData:LongWord;
  end;
  PTD3D10_ShaderDebugScopeInfo=^TD3D10_ShaderDebugScopeInfo;
  D3D10_SHADER_DEBUG_SCOPE_INFO=TD3D10_ShaderDebugScopeInfo;
  PD3D10_SHADER_DEBUG_SCOPE_INFO=^TD3D10_ShaderDebugScopeInfo;

  TD3D10_ShaderDebugOutputVar=record
    _Var:LongWord;
    uValueMin:LongWord;
    uValueMax:LongWord;
    iValueMin:Integer;
    iValueMax:Integer;
    fValueMin:Single;
    fValueMax:Single;
    bNaNPossible:LongBool;
    bInfPossible:LongBool;
  end;
  PTD3D10_ShaderDebugOutputVar=^TD3D10_ShaderDebugOutputVar;
  D3D10_SHADER_DEBUG_OUTPUTVAR=TD3D10_ShaderDebugOutputVar;
  PD3D10_SHADER_DEBUG_OUTPUTVAR=^TD3D10_ShaderDebugOutputVar;

  TD3D10_ShaderDebugOutputregInfo=record
    OutputRegisterSet:TD3D10_ShaderDebugRegtype;
    OutputReg:LongWord;
    TempArrayReg:LongWord;
    OutputComponents:array[0..3] of LongWord;
    OutputVars:array[0..3] of TD3D10_ShaderDebugOutputVar;
    IndexReg:LongWord;
    IndexComp:LongWord;
  end;
  PTD3D10_ShaderDebugOutputregInfo=^TD3D10_ShaderDebugOutputregInfo;
  D3D10_SHADER_DEBUG_OUTPUTREG_INFO=TD3D10_ShaderDebugOutputregInfo;
  PD3D10_SHADER_DEBUG_OUTPUTREG_INFO=^TD3D10_ShaderDebugOutputregInfo;

  TD3D10_ShaderDebugInstInfo=record
    ID:LongWord;
    Opcode:LongWord;
    NumOutputs:LongWord;
    pOutputs:array[0..1] of TD3D10_ShaderDebugOutputregInfo;
    TokenID:LongWord;
    NestingLevel:LongWord;
    Scopes:LongWord;
    ScopeInfo:LongWord;
    AccessedVars:LongWord;
    AccessedVarsInfo:LongWord;
  end;
  PTD3D10_ShaderDebugInstInfo=^TD3D10_ShaderDebugInstInfo;
  D3D10_SHADER_DEBUG_INST_INFO=TD3D10_ShaderDebugInstInfo;
  PD3D10_SHADER_DEBUG_INST_INFO=^TD3D10_ShaderDebugInstInfo;

  TD3D10_ShaderDebugFileInfo=record
    FileName:LongWord;
    FileNameLen:LongWord;
    FileData:LongWord;
    FileLen:LongWord;
  end;
  PTD3D10_ShaderDebugFileInfo=^TD3D10_ShaderDebugFileInfo;
  D3D10_SHADER_DEBUG_FILE_INFO=TD3D10_ShaderDebugFileInfo;
  PD3D10_SHADER_DEBUG_FILE_INFO=^TD3D10_ShaderDebugFileInfo;

  TD3D10_ShaderDebugInfo=record
    Size:LongWord;
    Creator:LongWord;
    EntrypointName:LongWord;
    ShaderTarget:LongWord;
    CompileFlags:LongWord;
    Files:LongWord;
    FileInfo:LongWord;
    Instructions:LongWord;
    InstructionInfo:LongWord;
    Variables:LongWord;
    VariableInfo:LongWord;
    InputVariables:LongWord;
    InputVariableInfo:LongWord;
    Tokens:LongWord;
    TokenInfo:LongWord;
    Scopes:LongWord;
    ScopeInfo:LongWord;
    ScopeVariables:LongWord;
    ScopeVariableInfo:LongWord;
    UintOffset:LongWord;
    StringOffset:LongWord;
  end;
  PTD3D10_ShaderDebugInfo=^TD3D10_ShaderDebugInfo;
  D3D10_SHADER_DEBUG_INFO=TD3D10_ShaderDebugInfo;
  PD3D10_SHADER_DEBUG_INFO=^TD3D10_ShaderDebugInfo;

  ID3D10ShaderReflection1=interface;
  PID3D10ShaderReflection1=^ID3D10ShaderReflection1;

  ID3D10ShaderReflection1=interface(IUnknown)
    ['{C3457783-A846-47CE-9520-CEA6F66E7447}']
    function GetDesc(pDesc:PTD3D10_ShaderDesc):HResult; stdcall;
    function GetConstantBufferByIndex(Index:LongWord):ID3D10ShaderReflectionConstantBuffer; stdcall;
    function GetConstantBufferByName(Name:PAnsiChar):ID3D10ShaderReflectionConstantBuffer; stdcall;
    function GetResourceBindingDesc(ResourceIndex:LongWord;pDesc:PTD3D10_ShaderInputBindDesc):HResult; stdcall;
    function GetInputParameterDesc(ParameterIndex:LongWord;pDesc:PTD3D10_SignatureParameterDesc):HResult; stdcall;
    function GetOutputParameterDesc(ParameterIndex:LongWord;pDesc:PTD3D10_SignatureParameterDesc):HResult; stdcall;
    function GetVariableByName(Name:PAnsiChar):ID3D10ShaderReflectionVariable; stdcall;
    function GetResourceBindingDescByName(Name:PAnsiChar;pDesc:PTD3D10_ShaderInputBindDesc):HResult; stdcall;
    function GetMovInstructionCount(pCount:PLongWord):HResult; stdcall;
    function GetMovcInstructionCount(pCount:PLongWord):HResult; stdcall;
    function GetConversionInstructionCount(pCount:PLongWord):HResult; stdcall;
    function GetBitwiseInstructionCount(pCount:PLongWord):HResult; stdcall;
    function GetGSInputPrimitive(pPrim:PTD3D10_Primitive):HResult; stdcall;
    function IsLevel9Shader(pValue:PLongBool):HResult; stdcall;
    function IsSampleFrequencyShader(pValue:PLongBool):HResult; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
// End "D3D10_1Shader.h"
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// Begin "DCommon.h"
///////////////////////////////////////////////////////////////////////////////

type
  TDWrite_MeasuringMode=
  (
    DWRITE_MEASURING_MODE_NATURAL,
    DWRITE_MEASURING_MODE_GDI_CLASSIC,
    DWRITE_MEASURING_MODE_GDI_NATURAL
  );
  PTDWrite_MeasuringMode=^TDWrite_MeasuringMode;
  DWRITE_MEASURING_MODE=TDWrite_MeasuringMode;
  PDWRITE_MEASURING_MODE=^TDWrite_MeasuringMode;

///////////////////////////////////////////////////////////////////////////////
// End "DCommon.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "DWrite.h"
///////////////////////////////////////////////////////////////////////////////

const
  DWRITE_ALPHA_MAX=255;
  FACILITY_DWRITE=$898;
  DWRITE_HR_ERR_Base=(1 shl 31) or (FACILITY_DWRITE shl 16)+$5000;
  //
  DWRITE_ERROR_FILEFORMAT=HResult(DWRITE_HR_ERR_Base or $000);
  DWRITE_ERROR_UNEXPECTED=HResult(DWRITE_HR_ERR_Base or $001);
  DWRITE_ERROR_NOFONT=HResult(DWRITE_HR_ERR_Base or $002);
  DWRITE_ERROR_FILENOTFOUND=HResult(DWRITE_HR_ERR_Base or $003);
  DWRITE_ERROR_FILEACCESS=HResult(DWRITE_HR_ERR_Base or $004);
  DWRITE_ERROR_FONTCOLLECTIONOBSOLETE=HResult(DWRITE_HR_ERR_Base or $005);
  DWRITE_ERROR_ALREADYREGISTERED=HResult(DWRITE_HR_ERR_Base or $006);

type
  TDWrite_FontFileType=
  (
    DWRITE_FONT_FILE_TYPE_UNKNOWN,
    DWRITE_FONT_FILE_TYPE_CFF,
    DWRITE_FONT_FILE_TYPE_TRUETYPE,
    DWRITE_FONT_FILE_TYPE_TRUETYPE_COLLECTION,
    DWRITE_FONT_FILE_TYPE_TYPE1_PFM,
    DWRITE_FONT_FILE_TYPE_TYPE1_PFB,
    DWRITE_FONT_FILE_TYPE_VECTOR,
    DWRITE_FONT_FILE_TYPE_BITMAP
  );
  PTDWrite_FontFileType=^TDWrite_FontFileType;
  DWRITE_FONT_FILE_TYPE=TDWrite_FontFileType;
  PDWRITE_FONT_FILE_TYPE=^TDWrite_FontFileType;

  TDWrite_FontFaceType=
  (
    DWRITE_FONT_FACE_TYPE_CFF,
    DWRITE_FONT_FACE_TYPE_TRUETYPE,
    DWRITE_FONT_FACE_TYPE_TRUETYPE_COLLECTION,
    DWRITE_FONT_FACE_TYPE_TYPE1,
    DWRITE_FONT_FACE_TYPE_VECTOR,
    DWRITE_FONT_FACE_TYPE_BITMAP,
    DWRITE_FONT_FACE_TYPE_UNKNOWN
  );
  PTDWrite_FontFaceType=^TDWrite_FontFaceType;
  DWRITE_FONT_FACE_TYPE=TDWrite_FontFaceType;
  PDWRITE_FONT_FACE_TYPE=^TDWrite_FontFaceType;

  TDWrite_FontSimulations=
  (
    DWRITE_FONT_SIMULATIONS_NONE=$0000,
    DWRITE_FONT_SIMULATIONS_BOLD=$0001,
    DWRITE_FONT_SIMULATIONS_OBLIQUE=$0002
  );
  PTDWrite_FontSimulations=^TDWrite_FontSimulations;
  DWRITE_FONT_SIMULATIONS=TDWrite_FontSimulations;
  PDWRITE_FONT_SIMULATIONS=^TDWrite_FontSimulations;

  TDWrite_FontWeight=
  (
    DWRITE_FONT_WEIGHT_THIN=100,
    DWRITE_FONT_WEIGHT_EXTRA_LIGHT=200,
    DWRITE_FONT_WEIGHT_ULTRA_LIGHT=200,
    DWRITE_FONT_WEIGHT_LIGHT=300,
    DWRITE_FONT_WEIGHT_NORMAL=400,
    DWRITE_FONT_WEIGHT_REGULAR=400,
    DWRITE_FONT_WEIGHT_MEDIUM=500,
    DWRITE_FONT_WEIGHT_DEMI_BOLD=600,
    DWRITE_FONT_WEIGHT_SEMI_BOLD=600,
    DWRITE_FONT_WEIGHT_BOLD=700,
    DWRITE_FONT_WEIGHT_EXTRA_BOLD=800,
    DWRITE_FONT_WEIGHT_ULTRA_BOLD=800,
    DWRITE_FONT_WEIGHT_BLACK=900,
    DWRITE_FONT_WEIGHT_HEAVY=900,
    DWRITE_FONT_WEIGHT_EXTRA_BLACK=950,
    DWRITE_FONT_WEIGHT_ULTRA_BLACK=950
  );
  PTDWrite_FontWeight=^TDWrite_FontWeight;
  DWRITE_FONT_WEIGHT=TDWrite_FontWeight;
  PDWRITE_FONT_WEIGHT=^TDWrite_FontWeight;

  TDWrite_FontStretch=
  (
    DWRITE_FONT_STRETCH_UNDEFINED=0,
    DWRITE_FONT_STRETCH_ULTRA_CONDENSED=1,
    DWRITE_FONT_STRETCH_EXTRA_CONDENSED=2,
    DWRITE_FONT_STRETCH_CONDENSED=3,
    DWRITE_FONT_STRETCH_SEMI_CONDENSED=4,
    DWRITE_FONT_STRETCH_NORMAL=5,
    DWRITE_FONT_STRETCH_MEDIUM=5,
    DWRITE_FONT_STRETCH_SEMI_EXPANDED=6,
    DWRITE_FONT_STRETCH_EXPANDED=7,
    DWRITE_FONT_STRETCH_EXTRA_EXPANDED=8,
    DWRITE_FONT_STRETCH_ULTRA_EXPANDED=9
  );
  PTDWrite_FontStretch=^TDWrite_FontStretch;
  DWRITE_FONT_STRETCH=TDWrite_FontStretch;
  PDWRITE_FONT_STRETCH=^TDWrite_FontStretch;

  TDWrite_FontStyle=
  (
    DWRITE_FONT_STYLE_NORMAL,
    DWRITE_FONT_STYLE_OBLIQUE,
    DWRITE_FONT_STYLE_ITALIC
  );
  PTDWrite_FontStyle=^TDWrite_FontStyle;
  DWRITE_FONT_STYLE=TDWrite_FontStyle;
  PDWRITE_FONT_STYLE=^TDWrite_FontStyle;

  TDWrite_InformationalStringId=
  (
    DWRITE_INFORMATIONAL_STRING_NONE,
    DWRITE_INFORMATIONAL_STRING_COPYRIGHT_NOTICE,
    DWRITE_INFORMATIONAL_STRING_VERSION_STRINGS,
    DWRITE_INFORMATIONAL_STRING_TRADEMARK,
    DWRITE_INFORMATIONAL_STRING_MANUFACTURER,
    DWRITE_INFORMATIONAL_STRING_DESIGNER,
    DWRITE_INFORMATIONAL_STRING_DESIGNER_URL,
    DWRITE_INFORMATIONAL_STRING_DESCRIPTION,
    DWRITE_INFORMATIONAL_STRING_FONT_VENDOR_URL,
    DWRITE_INFORMATIONAL_STRING_LICENSE_DESCRIPTION,
    DWRITE_INFORMATIONAL_STRING_LICENSE_INFO_URL,
    DWRITE_INFORMATIONAL_STRING_WIN32_FAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_WIN32_SUBFAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_PREFERRED_FAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_PREFERRED_SUBFAMILY_NAMES,
    DWRITE_INFORMATIONAL_STRING_SAMPLE_TEXT
  );
  PTDWrite_InformationalStringId=^TDWrite_InformationalStringId;
  DWRITE_INFORMATIONAL_STRING_ID=TDWrite_InformationalStringId;
  PDWRITE_INFORMATIONAL_STRING_ID=^TDWrite_InformationalStringId;

  TDWrite_FontMetrics=record
    DesignUnitsPerEm:Word;
    Ascent:Word;
    Descent:Word;
    LineGap:SmallInt;
    CapHeight:Word;
    XHeight:Word;
    UnderlinePosition:SmallInt;
    UnderlineThickness:Word;
    StrikethroughPosition:SmallInt;
    StrikethroughThickness:Word;
  end;
  PTDWrite_FontMetrics=^TDWrite_FontMetrics;
  DWRITE_FONT_METRICS=TDWrite_FontMetrics;
  PDWRITE_FONT_METRICS=^TDWrite_FontMetrics;

  TDWrite_GlyphMetrics=record
    LeftSideBearing:Integer;
    AdvanceWidth:LongWord;
    RightSideBearing:Integer;
    TopSideBearing:Integer;
    AdvanceHeight:LongWord;
    BottomSideBearing:Integer;
    VerticalOriginY:Integer;
  end;
  PTDWrite_GlyphMetrics=^TDWrite_GlyphMetrics;
  DWRITE_GLYPH_METRICS=TDWrite_GlyphMetrics;
  PDWRITE_GLYPH_METRICS=^TDWrite_GlyphMetrics;

  TDWrite_GlyphOffset=record
    AdvanceOffset:Single;
    AscenderOffset:Single;
  end;
  PTDWrite_GlyphOffset=^TDWrite_GlyphOffset;
  DWRITE_GLYPH_OFFSET=TDWrite_GlyphOffset;
  PDWRITE_GLYPH_OFFSET=^TDWrite_GlyphOffset;

  TDWrite_FactoryType=
  (
    DWRITE_FACTORY_TYPE_SHARED,
    DWRITE_FACTORY_TYPE_ISOLATED
  );
  PTDWrite_FactoryType=^TDWrite_FactoryType;
  DWRITE_FACTORY_TYPE=TDWrite_FactoryType;
  PDWRITE_FACTORY_TYPE=^TDWrite_FactoryType;

  IDWriteFontFileStream=interface;
  PIDWriteFontFileStream=^IDWriteFontFileStream;

  IDWriteFontFileLoader=interface(IUnknown)
    ['{727CAD4E-D6AF-4C9E-8A08-D695B11CAA49}']
    function CreateStreamFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      out FontFileStream:IDWriteFontFileStream (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteLocalFontFileLoader=interface(IDWriteFontFileLoader)
    ['{B2D9F3EC-C9FE-4A11-A2EC-D86208F7C0A2}']
    function GetFilePathLengthFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      out FilePathLength:LongWord (* __out *)
    ):HResult; stdcall;

    function GetFilePathFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      FilePath:PWideChar; (* __out_ecount_z(filePathSize) *)
      FilePathSize:LongWord
    ):HResult; stdcall;

    function GetLastWriteTimeFromKey
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      out LastWriteTime:FILETIME (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFileStream=interface(IUnknown)
    ['{6D4865FE-0AB8-4D91-8F62-5DD6BE34A3E0}']
    function ReadFileFragment
    (
      FragmentStart:PPointer; (* __deref_out_bcount(fragmentSize) *)
      FileOffset:LargeInt;
      FragmentSize:LargeInt;
      out FragmentContext:Pointer (* __out *)
    ):HResult; stdcall;

    procedure ReleaseFileFragment
    (
      FragmentContext:Pointer
    ); stdcall;

    function GetFileSize
    (
      out FileSize:LargeInt (* __out *)
    ):HResult; stdcall;

    function GetLastWriteTime
    (
      out LastWriteTime:LargeInt (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFile=interface(IUnknown)
    ['{739D886A-CEF5-47DC-8769-1A8B41BEBBB0}']
    function GetReferenceKey
    (
      FontFileReferenceKey:PPointer; (* __deref_out_bcount(*fontFileReferenceKeySize) *)
      out FontFileReferenceKeySize:LongWord (* __out *)
    ):HResult; stdcall;

    function GetLoader
    (
      out FontFileLoader:IDWriteFontFileLoader (* __out *)
    ):HResult; stdcall;

    function Analyze
    (
      out IsSupportedFontType:LongBool; (* __out *)
      out FontFileType:TDWrite_FontFileType; (* __out *)
      FontFaceType:PTDWrite_FontFaceType; (* __out_opt *)
      out NumberOfFaces:LongWord (* __out *)
    ):HResult; stdcall;
  end;
  PIDWriteFontFile=^IDWriteFontFile;

  TDWrite_PixelGeometry=
  (
    DWRITE_PIXEL_GEOMETRY_FLAT,
    DWRITE_PIXEL_GEOMETRY_RGB,
    DWRITE_PIXEL_GEOMETRY_BGR
  );
  PTDWrite_PixelGeometry=^TDWrite_PixelGeometry;
  DWRITE_PIXEL_GEOMETRY=TDWrite_PixelGeometry;
  PDWRITE_PIXEL_GEOMETRY=^TDWrite_PixelGeometry;

  TDWrite_RenderingMode=
  (
    DWRITE_RENDERING_MODE_DEFAULT,
    DWRITE_RENDERING_MODE_ALIASED,
    DWRITE_RENDERING_MODE_CLEARTYPE_GDI_CLASSIC,
    DWRITE_RENDERING_MODE_CLEARTYPE_GDI_NATURAL,
    DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL,
    DWRITE_RENDERING_MODE_CLEARTYPE_NATURAL_SYMMETRIC,
    DWRITE_RENDERING_MODE_OUTLINE
  );
  PTDWrite_RenderingMode=^TDWrite_RenderingMode;
  DWRITE_RENDERING_MODE=TDWrite_RenderingMode;
  PDWRITE_RENDERING_MODE=^TDWrite_RenderingMode;

  TDWrite_Matrix=record
    M11:Single;
    M12:Single;
    M21:Single;
    M22:Single;
    Dx:Single;
    Dy:Single;
  end;
  PTDWrite_Matrix=^TDWrite_Matrix;
  DWRITE_MATRIX=TDWrite_Matrix;
  PDWRITE_MATRIX=^TDWrite_Matrix;

  IDWriteRenderingParams=interface(IUnknown)
    ['{2F0DA53A-2ADD-47CD-82EE-D9EC34688E75}']
    function GetGamma:Single; stdcall;
    function GetEnhancedContrast:Single; stdcall;
    function GetClearTypeLevel:Single; stdcall;
    function GetPixelGeometry:TDWrite_PixelGeometry; stdcall;
    function GetRenderingMode:TDWrite_RenderingMode; stdcall;
  end;

  IDWriteGeometrySink=IUnknown; // JSB: Acutally ID2D1SimplifiedGeometrySink.

  IDWriteFontFace=interface(IUnknown)
    ['{5F49804D-7024-4D43-BFA9-D25984F53849}']
    function GetType:TDWrite_FontFaceType; stdcall;

    function GetFiles
    (
      var NumberOfFiles:LongWord; (* __inout *)
      FontFiles:PIDWriteFontFile (* __out_ecount_opt(*numberOfFiles) *)
    ):HResult; stdcall;

    function GetIndex:LongWord; stdcall;

    function GetSimulations:TDWrite_FontSimulations; stdcall;

    function IsSymbolFont:LongBool; stdcall;

    procedure GetMetrics
    (
      out FontFaceMetrics:TDWrite_FontMetrics (* __out *)
    ); stdcall;

    function GetGlyphCount:Word; stdcall;

    function GetDesignGlyphMetrics
    (
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      GlyphMetrics:PTDWrite_GlyphMetrics; (* __out_ecount(glyphCount) *)
      IsSideways:LongBool=FALSE
    ):HResult; stdcall;

    function GetGlyphIndices
    (
      CodePoints:PLongWord; (* __in_ecount(codePointCount) *)
      CodePointCount:LongWord;
      GlyphIndices:PWord (* __out_ecount(codePointCount) *)
    ):HResult; stdcall;

    function TryGetFontTable
    (
      OpenTypeTableTag:LongWord; (* __in *)
      TableData:PPointer; (* __deref_out_bcount(*tableSize) *)
      out TableSize:LongWord; (* __out *)
      out TableContext:Pointer; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    procedure ReleaseFontTable
    (
      TableContext:Pointer (* __in *)
    ); stdcall;

    function GetGlyphRunOutline
    (
      EmSize:Single;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphAdvances:PSingle; (* __in_ecount_opt(glyphCount) *)
      GlyphOffsets:PTDWrite_GlyphOffset; (* __in_ecount_opt(glyphCount) *)
      GlyphCount:LongWord;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      GeometrySink:IDWriteGeometrySink
    ):HResult; stdcall;

    function GetRecommendedRenderingMode
    (
      EmSize:Single;
      PixelsPerDip:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      RenderingParams:IDWriteRenderingParams;
      out RenderingMode:TDWrite_RenderingMode (* __out *)
    ):HResult; stdcall;

    function GetGdiCompatibleMetrics
    (
      EmSize:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      out FontFaceMetrics:TDWrite_FontMetrics (* __out *)
    ):HResult; stdcall;

    function GetGdiCompatibleGlyphMetrics
    (
      EmSize:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      UseGdiNatural:LongBool;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      GlyphMetrics:PTDWrite_GlyphMetrics; (* __out_ecount(glyphCount) *)
      IsSideways:LongBool=FALSE
    ):HResult; stdcall;
  end;

  IDWriteFactory=interface;
  PIDWriteFactory=^IDWriteFactory;

  IDWriteFontFileEnumerator=interface;
  PIDWriteFontFileEnumerator=^IDWriteFontFileEnumerator;

  IDWriteFontCollectionLoader=interface(IUnknown)
    ['{CCA920E4-52F0-492B-BFA8-29C72EE0A468}']
    function CreateEnumeratorFromKey
    (
      Factory:IDWriteFactory;
      CollectionKey:Pointer; (* __in_bcount(collectionKeySize) *)
      CollectionKeySize:LongWord;
      out FontFileEnumerator:IDWriteFontFileEnumerator (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFileEnumerator=interface(IUnknown)
    ['{72755049-5FF7-435D-8348-4BE97CFA6C7C}']
    function MoveNext
    (
      out HasCurrentFile:LongBool (* __out *)
    ):HResult; stdcall;

    function GetCurrentFontFile
    (
      out FontFile:IDWriteFontFile (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteLocalizedStrings=interface(IUnknown)
    ['{08256209-099A-4B34-B86D-C22B110E7771}']
    function GetCount:LongWord; stdcall;

    function FindLocaleName
    (
      LocaleName:PWideChar; (* __in_z *)
      out Index:LongWord; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function GetLocaleNameLength
    (
      Index:LongWord;
      out Length:LongWord (* __out *)
    ):HResult; stdcall;

    function GetLocaleName
    (
      Index:LongWord;
      LocaleName:PWideChar; (* __out_ecount_z(size) *)
      Size:LongWord
    ):HResult; stdcall;

    function GetStringLength
    (
      Index:LongWord;
      out Length:LongWord (* __out *)
    ):HResult; stdcall;

    function GetString
    (
      Index:LongWord;
      StringBuffer:PWideChar; (* __out_ecount_z(size) *)
      Size:LongWord
    ):HResult; stdcall;
  end;

  IDWriteFontFamily=interface;
  PIDWriteFontFamily=^IDWriteFontFamily;

  IDWriteFont=interface;
  PIDWriteFont=^IDWriteFont;

  IDWriteFontCollection=interface(IUnknown)
    ['{A84CEE02-3EEA-4EEE-A827-87C1A02A0FCC}']
    function GetFontFamilyCount:LongWord; stdcall;

    function GetFontFamily
    (
      Index:LongWord;
      out FontFamily:IDWriteFontFamily (* __out *)
    ):HResult; stdcall;

    function FindFamilyName
    (
      FamilyName:PWideChar; (* __in_z *)
      out Index:LongWord; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function GetFontFromFontFace
    (
      FontFace:IDWriteFontFace;
      out Font:IDWriteFont (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontList=interface(IUnknown)
    ['{1A0D8438-1D97-4EC1-AEF9-A2FB86ED6ACB}']
    function GetFontCollection
    (
      out FontCollection:IDWriteFontCollection (* __out *)
    ):HResult; stdcall;

    function GetFontCount:LongWord; stdcall;

    function GetFont
    (
      Index:LongWord;
      out Font:IDWriteFont (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFontFamily=interface(IDWriteFontList)
    ['{DA20D8EF-812A-4C43-9802-62EC4ABD7ADD}']
    function GetFamilyNames
    (
      out Names:IDWriteLocalizedStrings (* __out *)
    ):HResult; stdcall;

    function GetFirstMatchingFont
    (
      Weight:TDWrite_FontWeight;
      Stretch:TDWrite_FontStretch;
      Style:TDWrite_FontStyle;
      out MatchingFont:IDWriteFont (* __out *)
    ):HResult; stdcall;

    function GetMatchingFonts
    (
      Weight:TDWrite_FontWeight;
      Stretch:TDWrite_FontStretch;
      Style:TDWrite_FontStyle;
      out MatchingFonts:IDWriteFontList (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFont=interface(IUnknown)
    ['{ACD16696-8C14-4F5D-877E-FE3FC1D32737}']
    function GetFontFamily
    (
      out FontFamily:IDWriteFontFamily (* __out *)
    ):HResult; stdcall;

    function GetWeight:TDWrite_FontWeight; stdcall;

    function GetStretch:TDWrite_FontStretch; stdcall;

    function GetStyle:TDWrite_FontStyle; stdcall;

    function IsSymbolFont:LongBool; stdcall;

    function GetFaceNames
    (
      out Names:IDWriteLocalizedStrings (* __out *)
    ):HResult; stdcall;

    function GetInformationalStrings
    (
      InformationalStringID:TDWrite_InformationalStringId;
      out InformationalStrings:IDWriteLocalizedStrings; (* __out *)
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function GetSimulations:TDWrite_FontSimulations; stdcall;

    procedure GetMetrics
    (
      out FontMetrics:TDWrite_FontMetrics (* __out *)
    ); stdcall;

    function HasCharacter
    (
      UnicodeValue:LongWord;
      out Exists:LongBool (* __out *)
    ):HResult; stdcall;

    function CreateFontFace
    (
      out FontFace:IDWriteFontFace (* __out *)
    ):HResult; stdcall;
  end;

  TDWrite_ReadingDirection=
  (
    DWRITE_READING_DIRECTION_LEFT_TO_RIGHT,
    DWRITE_READING_DIRECTION_RIGHT_TO_LEFT
  );
  PTDWrite_ReadingDirection=^TDWrite_ReadingDirection;
  DWRITE_READING_DIRECTION=TDWrite_ReadingDirection;
  PDWRITE_READING_DIRECTION=^TDWrite_ReadingDirection;

  TDWrite_FlowDirection=
  (
    DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM
  );
  PTDWrite_FlowDirection=^TDWrite_FlowDirection;
  DWRITE_FLOW_DIRECTION=TDWrite_FlowDirection;
  PDWRITE_FLOW_DIRECTION=^TDWrite_FlowDirection;

  TDWrite_TextAlignment=
  (
    DWRITE_TEXT_ALIGNMENT_LEADING,
    DWRITE_TEXT_ALIGNMENT_TRAILING,
    DWRITE_TEXT_ALIGNMENT_CENTER
  );
  PTDWrite_TextAlignment=^TDWrite_TextAlignment;
  DWRITE_TEXT_ALIGNMENT=TDWrite_TextAlignment;
  PDWRITE_TEXT_ALIGNMENT=^TDWrite_TextAlignment;

  TDWrite_ParagraphAlignment=
  (
    DWRITE_PARAGRAPH_ALIGNMENT_NEAR,
    DWRITE_PARAGRAPH_ALIGNMENT_FAR,
    DWRITE_PARAGRAPH_ALIGNMENT_CENTER
  );
  PTDWrite_ParagraphAlignment=^TDWrite_ParagraphAlignment;
  DWRITE_PARAGRAPH_ALIGNMENT=TDWrite_ParagraphAlignment;
  PDWRITE_PARAGRAPH_ALIGNMENT=^TDWrite_ParagraphAlignment;

  TDWrite_WordWrapping=
  (
    DWRITE_WORD_WRAPPING_WRAP,
    DWRITE_WORD_WRAPPING_NO_WRAP
  );
  PTDWrite_WordWrapping=^TDWrite_WordWrapping;
  DWRITE_WORD_WRAPPING=TDWrite_WordWrapping;
  PDWRITE_WORD_WRAPPING=^TDWrite_WordWrapping;

  TDWrite_LineSpacingMethod=
  (
    DWRITE_LINE_SPACING_METHOD_DEFAULT,
    DWRITE_LINE_SPACING_METHOD_UNIFORM
  );
  PTDWrite_LineSpacingMethod=^TDWrite_LineSpacingMethod;
  DWRITE_LINE_SPACING_METHOD=TDWrite_LineSpacingMethod;
  PDWRITE_LINE_SPACING_METHOD=^TDWrite_LineSpacingMethod;

  TDWrite_TrimmingGranularity=
  (
    DWRITE_TRIMMING_GRANULARITY_NONE,
    DWRITE_TRIMMING_GRANULARITY_CHARACTER,
    DWRITE_TRIMMING_GRANULARITY_WORD
  );
  PTDWrite_TrimmingGranularity=^TDWrite_TrimmingGranularity;
  DWRITE_TRIMMING_GRANULARITY=TDWrite_TrimmingGranularity;
  PDWRITE_TRIMMING_GRANULARITY=^TDWrite_TrimmingGranularity;

  TDWrite_FontFeatureTag=
  (
    DWRITE_FONT_FEATURE_TAG_ALTERNATIVE_FRACTIONS=$63726661,
    DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS_FROM_CAPITALS=$63703263,
    DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS_FROM_CAPITALS=$63733263,
    DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES=$746c6163,
    DWRITE_FONT_FEATURE_TAG_CASE_SENSITIVE_FORMS=$65736163,
    DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION=$706d6363,
    DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES=$67696c63,
    DWRITE_FONT_FEATURE_TAG_CAPITAL_SPACING=$70737063,
    DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_SWASH=$68777363,
    DWRITE_FONT_FEATURE_TAG_CURSIVE_POSITIONING=$73727563,
    DWRITE_FONT_FEATURE_TAG_DEFAULT=$746c6664,
    DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES=$67696c64,
    DWRITE_FONT_FEATURE_TAG_EXPERT_FORMS=$74707865,
    DWRITE_FONT_FEATURE_TAG_FRACTIONS=$63617266,
    DWRITE_FONT_FEATURE_TAG_FULL_WIDTH=$64697766,
    DWRITE_FONT_FEATURE_TAG_HALF_FORMS=$666c6168,
    DWRITE_FONT_FEATURE_TAG_HALANT_FORMS=$6e6c6168,
    DWRITE_FONT_FEATURE_TAG_ALTERNATE_HALF_WIDTH=$746c6168,
    DWRITE_FONT_FEATURE_TAG_HISTORICAL_FORMS=$74736968,
    DWRITE_FONT_FEATURE_TAG_HORIZONTAL_KANA_ALTERNATES=$616e6b68,
    DWRITE_FONT_FEATURE_TAG_HISTORICAL_LIGATURES=$67696c68,
    DWRITE_FONT_FEATURE_TAG_HALF_WIDTH=$64697768,
    DWRITE_FONT_FEATURE_TAG_HOJO_KANJI_FORMS=$66a668,
    DWRITE_FONT_FEATURE_TAG_JIS04_FORMS=$3430706a,
    DWRITE_FONT_FEATURE_TAG_JIS78_FORMS=$3837706a,
    DWRITE_FONT_FEATURE_TAG_JIS83_FORMS=$3338706a,
    DWRITE_FONT_FEATURE_TAG_JIS90_FORMS=$3039706a,
    DWRITE_FONT_FEATURE_TAG_KERNING=$6e72656b,
    DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES=$6167696c,
    DWRITE_FONT_FEATURE_TAG_LINING_FIGURES=$6d756e6c,
    DWRITE_FONT_FEATURE_TAG_LOCALIZED_FORMS=$6c6366c,
    DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING=$6b72616d,
    DWRITE_FONT_FEATURE_TAG_MATHEMATICAL_GREEK=$6b72676d,
    DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING=$6b6d6b6d,
    DWRITE_FONT_FEATURE_TAG_ALTERNATE_ANNOTATION_FORMS=$746c616e,
    DWRITE_FONT_FEATURE_TAG_NLC_KANJI_FORMS=$6b636c6e,
    DWRITE_FONT_FEATURE_TAG_OLD_STYLE_FIGURES=$6d756e6,
    DWRITE_FONT_FEATURE_TAG_ORDINALS=$6e64726,
    DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_ALTERNATE_WIDTH=$746c6170,
    DWRITE_FONT_FEATURE_TAG_PETITE_CAPITALS=$70616370,
    DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_FIGURES=$6d756e70,
    DWRITE_FONT_FEATURE_TAG_PROPORTIONAL_WIDTHS=$64697770,
    DWRITE_FONT_FEATURE_TAG_QUARTER_WIDTHS=$64697771,
    DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES=$67696c72,
    DWRITE_FONT_FEATURE_TAG_RUBY_NOTATION_FORMS=$79627572,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_ALTERNATES=$746c6173,
    DWRITE_FONT_FEATURE_TAG_SCIENTIFIC_INFERIORS=$666e6973,
    DWRITE_FONT_FEATURE_TAG_SMALL_CAPITALS=$70636d73,
    DWRITE_FONT_FEATURE_TAG_SIMPLIFIED_FORMS=$6c706d73,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_1=$31307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_2=$32307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_3=$33307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_4=$34307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_5=$35307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_6=$36307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_7=$37307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_8=$38307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_9=$39307373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_10=$30317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_11=$31317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_12=$32317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_13=$33317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_14=$34317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_15=$35317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_16=$36317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_17=$37317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_18=$38317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_19=$39317373,
    DWRITE_FONT_FEATURE_TAG_STYLISTIC_SET_20=$30327373,
    DWRITE_FONT_FEATURE_TAG_SUBSCRIPT=$73627573,
    DWRITE_FONT_FEATURE_TAG_SUPERSCRIPT=$73707573,
    DWRITE_FONT_FEATURE_TAG_SWASH=$68737773,
    DWRITE_FONT_FEATURE_TAG_TITLING=$6c746974,
    DWRITE_FONT_FEATURE_TAG_TRADITIONAL_NAME_FORMS=$6d616e74,
    DWRITE_FONT_FEATURE_TAG_TABULAR_FIGURES=$6d756e74,
    DWRITE_FONT_FEATURE_TAG_TRADITIONAL_FORMS=$64617274,
    DWRITE_FONT_FEATURE_TAG_THIRD_WIDTHS=$64697774,
    DWRITE_FONT_FEATURE_TAG_UNICASE=$63696e75,
    DWRITE_FONT_FEATURE_TAG_SLASHED_ZERO=$672657a
  );
  PTDWrite_FontFeatureTag=^TDWrite_FontFeatureTag;
  DWRITE_FONT_FEATURE_TAG=TDWrite_FontFeatureTag;
  PDWRITE_FONT_FEATURE_TAG=^TDWrite_FontFeatureTag;

  TDWrite_TextRange=record
    StartPosition:LongWord;
    Length:LongWord;
  end;
  PTDWrite_TextRange=^TDWrite_TextRange;
  DWRITE_TEXT_RANGE=TDWrite_TextRange;
  PDWRITE_TEXT_RANGE=^TDWrite_TextRange;

  TDWrite_FontFeature=record
    NameTag:TDWrite_FontFeatureTag;
    Parameter:LongWord;
  end;
  PTDWrite_FontFeature=^TDWrite_FontFeature;
  DWRITE_FONT_FEATURE=TDWrite_FontFeature;
  PDWRITE_FONT_FEATURE=^TDWrite_FontFeature;

  TDWrite_TypographicFeatures=record
    Features:PTDWrite_FontFeature; (* __field_ecount(featureCount) *)
    FeatureCount:LongWord;
  end;
  PTDWrite_TypographicFeatures=^TDWrite_TypographicFeatures;
  PPTDWrite_TypographicFeatures=^PTDWrite_TypographicFeatures;
  DWRITE_TYPOGRAPHIC_FEATURES=TDWrite_TypographicFeatures;
  PDWRITE_TYPOGRAPHIC_FEATURES=^TDWrite_TypographicFeatures;

  TDWrite_Trimming=record
    Granularity:TDWrite_TrimmingGranularity;
    Delimiter:LongWord;
    DelimiterCount:LongWord;
  end;
  PTDWrite_Trimming=^TDWrite_Trimming;
  DWRITE_TRIMMING=TDWrite_Trimming;
  PDWRITE_TRIMMING=^TDWrite_Trimming;

  IDWriteTypography=interface;
  PIDWriteTypography=^IDWriteTypography;

  IDWriteInlineObject=interface;
  PIDWriteInlineObject=^IDWriteInlineObject;

  IDWriteTextFormat=interface(IUnknown)
    ['{9C906818-31D7-4FD3-A151-7C5E225DB55A}']
    function SetTextAlignment
    (
      TextAlignment:TDWrite_TextAlignment
    ):HResult; stdcall;

    function SetParagraphAlignment
    (
      ParagraphAlignment:TDWrite_ParagraphAlignment
    ):HResult; stdcall;

    function SetWordWrapping
    (
      WordWrapping:TDWrite_WordWrapping
    ):HResult; stdcall;

    function SetReadingDirection
    (
      ReadingDirection:TDWrite_ReadingDirection
    ):HResult; stdcall;

    function SetFlowDirection
    (
      FlowDirection:TDWrite_FlowDirection
    ):HResult; stdcall;

    function SetIncrementalTabStop
    (
      IncrementalTabStop:Single
    ):HResult; stdcall;

    function SetTrimming
    (
      const TrimmingOptions:TDWrite_Trimming; (* __in *)
      TrimmingSign:IDWriteInlineObject
    ):HResult; stdcall;

    function SetLineSpacing
    (
      LineSpacingMethod:TDWrite_LineSpacingMethod;
      LineSpacing:Single;
      Baseline:Single
    ):HResult; stdcall;

    function GetTextAlignment:TDWrite_TextAlignment; stdcall;

    function GetParagraphAlignment:TDWrite_ParagraphAlignment; stdcall;

    function GetWordWrapping:TDWrite_WordWrapping; stdcall;

    function GetReadingDirection:TDWrite_ReadingDirection; stdcall;

    function GetFlowDirection:TDWrite_FlowDirection; stdcall;

    function GetIncrementalTabStop:Single; stdcall;

    function GetTrimming
    (
      out TrimmingOptions:TDWrite_Trimming; (* __out *)
      out TrimmingSign:IDWriteInlineObject (* __out *)
    ):HResult; stdcall;

    function GetLineSpacing
    (
      out LineSpacingMethod:TDWrite_LineSpacingMethod; (* __out *)
      out LineSpacing:Single; (* __out *)
      out Baseline:Single (* __out *)
    ):HResult; stdcall;

    function GetFontCollection
    (
      out FontCollection:IDWriteFontCollection (* __out *)
    ):HResult; stdcall;

    function GetFontFamilyNameLength:LongWord; stdcall;

    function GetFontFamilyName
    (
      FontFamilyName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord
    ):HResult; stdcall;

    function GetFontWeight:TDWrite_FontWeight; stdcall;

    function GetFontStyle:TDWrite_FontStyle; stdcall;

    function GetFontStretch:TDWrite_FontStretch; stdcall;

    function GetFontSize:Single; stdcall;

    function GetLocaleNameLength:LongWord; stdcall;

    function GetLocaleName
    (
      LocaleName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord
    ):HResult; stdcall;
  end;

  IDWriteTypography=interface(IUnknown)
    ['{55F1112B-1DC2-4B3C-9541-F46894ED85B6}']
    function AddFontFeature
    (
      FontFeature:TDWrite_FontFeature
    ):HResult; stdcall;

    function GetFontFeatureCount:LongWord; stdcall;

    function GetFontFeature
    (
      FontFeatureIndex:LongWord;
      out FontFeature:TDWrite_FontFeature (* __out *)
    ):HResult; stdcall;
  end;

  TDWrite_ScriptShapes=
  (
    DWRITE_SCRIPT_SHAPES_DEFAULT=0,
    DWRITE_SCRIPT_SHAPES_NO_VISUAL=1
  );
  PTDWrite_ScriptShapes=^TDWrite_ScriptShapes;
  DWRITE_SCRIPT_SHAPES=TDWrite_ScriptShapes;
  PDWRITE_SCRIPT_SHAPES=^TDWrite_ScriptShapes;

  TDWrite_ScriptAnalysis=record
    Script:Word;
    Shapes:TDWrite_ScriptShapes;
  end;
  PTDWrite_ScriptAnalysis=^TDWrite_ScriptAnalysis;
  DWRITE_SCRIPT_ANALYSIS=TDWrite_ScriptAnalysis;
  PDWRITE_SCRIPT_ANALYSIS=^TDWrite_ScriptAnalysis;

  TDWrite_BreakCondition=
  (
    DWRITE_BREAK_CONDITION_NEUTRAL,
    DWRITE_BREAK_CONDITION_CAN_BREAK,
    DWRITE_BREAK_CONDITION_MAY_NOT_BREAK,
    DWRITE_BREAK_CONDITION_MUST_BREAK
  );
  PTDWrite_BreakCondition=^TDWrite_BreakCondition;
  DWRITE_BREAK_CONDITION=TDWrite_BreakCondition;
  PDWRITE_BREAK_CONDITION=^TDWrite_BreakCondition;

  TDWrite_LineBreakpoint=record
    LineBreakpointConditions:Byte;
  end;
  PTDWrite_LineBreakpoint=^TDWrite_LineBreakpoint;
  DWRITE_LINE_BREAKPOINT=TDWrite_LineBreakpoint;
  PDWRITE_LINE_BREAKPOINT=^TDWrite_LineBreakpoint;

  TDWrite_NumberSubstitutionMethod=
  (
    DWRITE_NUMBER_SUBSTITUTION_METHOD_FROM_CULTURE,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_CONTEXTUAL,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_NONE,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_NATIONAL,
    DWRITE_NUMBER_SUBSTITUTION_METHOD_TRADITIONAL
  );
  PTDWrite_NumberSubstitutionMethod=^TDWrite_NumberSubstitutionMethod;
  DWRITE_NUMBER_SUBSTITUTION_METHOD=TDWrite_NumberSubstitutionMethod;
  PDWRITE_NUMBER_SUBSTITUTION_METHOD=^TDWrite_NumberSubstitutionMethod;

  IDWriteNumberSubstitution=interface(IUnknown)
    ['{14885CC9-BAB0-4F90-B6ED-5C366A2CD03D}']
  end;

  TDWrite_ShapingTextProperties=record
    ShapingTextProperties:Word;
  end;
  PTDWrite_ShapingTextProperties=^TDWrite_ShapingTextProperties;
  DWRITE_SHAPING_TEXT_PROPERTIES=TDWrite_ShapingTextProperties;
  PDWRITE_SHAPING_TEXT_PROPERTIES=^TDWrite_ShapingTextProperties;

  TDWrite_ShapingGlyphProperties=record
    ShapingGlyphProperties:Word;
  end;
  PTDWrite_ShapingGlyphProperties=^TDWrite_ShapingGlyphProperties;
  DWRITE_SHAPING_GLYPH_PROPERTIES=TDWrite_ShapingGlyphProperties;
  PDWRITE_SHAPING_GLYPH_PROPERTIES=^TDWrite_ShapingGlyphProperties;

  IDWriteTextAnalysisSource=interface(IUnknown)
    ['{688E1A58-5094-47C8-ADC8-FBCEA60AE92B}']
    function GetTextAtPosition
    (
      TextPosition:LongWord;
      out TextString:PWideChar; (* __out *)
      out TextLength:LongWord (* __out *)
    ):HResult; stdcall;

    function GetTextBeforePosition
    (
      TextPosition:LongWord;
      out TextString:PWideChar; (* __out *)
      out TextLength:LongWord (* __out *)
    ):HResult; stdcall;

    function GetParagraphReadingDirection:TDWrite_ReadingDirection; stdcall;

    function GetLocaleName
    (
      TextPosition:LongWord;
      out TextLength:LongWord; (* __out *)
      LocaleName:PPWideChar (* __out_z *)
    ):HResult; stdcall;

    function GetNumberSubstitution
    (
      TextPosition:LongWord;
      out TextLength:LongWord; (* __out *)
      out NumberSubstitution:IDWriteNumberSubstitution (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteTextAnalysisSink=interface(IUnknown)
    ['{5810CD44-0CA0-4701-B3FA-BEC5182AE4F6}']
    function SetScriptAnalysis
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      const ScriptAnalysis:TDWrite_ScriptAnalysis (* __in *)
    ):HResult; stdcall;

    function SetLineBreakpoints
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      LineBreakpoints:PTDWrite_LineBreakpoint (* __in_ecount(textLength) *)
    ):HResult; stdcall;

    function SetBidiLevel
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      ExplicitLevel:Byte;
      ResolvedLevel:Byte
    ):HResult; stdcall;

    function SetNumberSubstitution
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      NumberSubstitution:IDWriteNumberSubstitution (* __notnull *)
    ):HResult; stdcall;
  end;

  IDWriteTextAnalyzer=interface(IUnknown)
    ['{B7E6163E-7F46-43B4-84B3-E4E6249C365D}']
    function AnalyzeScript
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function AnalyzeBidi
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function AnalyzeNumberSubstitution
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function AnalyzeLineBreakpoints
    (
      AnalysisSource:IDWriteTextAnalysisSource;
      TextPosition:LongWord;
      TextLength:LongWord;
      AnalysisSink:IDWriteTextAnalysisSink
    ):HResult; stdcall;

    function GetGlyphs
    (
      TextString:PWideChar; (* __in_ecount(textLength) *)
      TextLength:LongWord;
      FontFace:IDWriteFontFace;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      const ScriptAnalysis:TDWrite_ScriptAnalysis; (* __in *)
      LocaleName:PWideChar; (* __in_z_opt *)
      NumberSubstitution:IDWriteNumberSubstitution; (* __maybenull *)
      Features:PPTDWrite_TypographicFeatures; (* __in_ecount_opt(featureRanges) *)
      FeatureRangeLengths:PLongWord; (* __in_ecount_opt(featureRanges) *)
      FeatureRanges:LongWord;
      MaxGlyphCount:LongWord;
      ClusterMap:PWord; (* __out_ecount(textLength) *)
      TextProps:PTDWrite_ShapingTextProperties; (* __out_ecount(textLength) *)
      GlyphIndices:PWord; (* __out_ecount(maxGlyphCount) *)
      GlyphProps:PTDWrite_ShapingGlyphProperties; (* __out_ecount(maxGlyphCount) *)
      out ActualGlyphCount:LongWord (* __out *)
    ):HResult; stdcall;

    function GetGlyphPlacements
    (
      TextString:PWideChar; (* __in_ecount(textLength) *)
      ClusterMap:PWord; (* __in_ecount(textLength) *)
      TextProps:PTDWrite_ShapingTextProperties; (* __in_ecount(textLength) *)
      TextLength:LongWord;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphProps:PTDWrite_ShapingGlyphProperties; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      FontFace:IDWriteFontFace;
      FontEmSize:Single;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      const ScriptAnalysis:TDWrite_ScriptAnalysis; (* __in *)
      LocaleName:PWideChar; (* __in_z_opt *)
      Features:PPTDWrite_TypographicFeatures; (* __in_ecount_opt(featureRanges) *)
      FeatureRangeLengths:PLongWord; (* __in_ecount_opt(featureRanges) *)
      FeatureRanges:LongWord;
      GlyphAdvances:PSingle; (* __out_ecount(glyphCount) *)
      GlyphOffsets:PTDWrite_GlyphOffset (* __out_ecount(glyphCount) *)
    ):HResult; stdcall;

    function GetGdiCompatibleGlyphPlacements
    (
      TextString:PWideChar; (* __in_ecount(textLength) *)
      ClusterMap:PWord; (* __in_ecount(textLength) *)
      TextProps:PTDWrite_ShapingTextProperties; (* __in_ecount(textLength) *)
      TextLength:LongWord;
      GlyphIndices:PWord; (* __in_ecount(glyphCount) *)
      GlyphProps:PTDWrite_ShapingGlyphProperties; (* __in_ecount(glyphCount) *)
      GlyphCount:LongWord;
      FontFace:IDWriteFontFace;
      FontEmSize:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      UseGdiNatural:LongBool;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      const ScriptAnalysis:TDWrite_ScriptAnalysis; (* __in *)
      LocaleName:PWideChar; (* __in_z_opt *)
      Features:PPTDWrite_TypographicFeatures; (* __in_ecount_opt(featureRanges) *)
      FeatureRangeLengths:PLongWord; (* __in_ecount_opt(featureRanges) *)
      FeatureRanges:LongWord;
      GlyphAdvances:PSingle; (* __out_ecount(glyphCount) *)
      GlyphOffsets:PTDWrite_GlyphOffset (* __out_ecount(glyphCount) *)
    ):HResult; stdcall;
  end;

  TDWrite_GlyphRun=record
    FontFace:IDWriteFontFace; (* __notnull *)
    FontEmSize:Single;
    GlyphCount:LongWord;
    GlyphIndices:PWord; (* __field_ecount(glyphCount) *)
    GlyphAdvances:PSingle; (* __field_ecount_opt(glyphCount) *)
    GlyphOffsets:PTDWrite_GlyphOffset; (* __field_ecount_opt(glyphCount) *)
    IsSideways:LongBool;
    BidiLevel:LongWord;
  end;
  PTDWrite_GlyphRun=^TDWrite_GlyphRun;
  DWRITE_GLYPH_RUN=TDWrite_GlyphRun;
  PDWRITE_GLYPH_RUN=^TDWrite_GlyphRun;

  TDWrite_GlyphRunDescription=record
    LocaleName:PWideChar; (* __nullterminated *)
    _String:PWideChar; (* __field_ecount(stringLength) *)
    StringLength:LongWord;
    ClusterMap:PWord; (* __field_ecount(stringLength) *)
    TextPosition:LongWord;
  end;
  PTDWrite_GlyphRunDescription=^TDWrite_GlyphRunDescription;
  DWRITE_GLYPH_RUN_DESCRIPTION=TDWrite_GlyphRunDescription;
  PDWRITE_GLYPH_RUN_DESCRIPTION=^TDWrite_GlyphRunDescription;

  TDWrite_Underline=record
    Width:Single;
    Thickness:Single;
    Offset:Single;
    RunHeight:Single;
    ReadingDirection:TDWrite_ReadingDirection;
    FlowDirection:TDWrite_FlowDirection;
    LocaleName:PWideChar; (* __nullterminated *)
    MeasuringMode:TDWrite_MeasuringMode;
  end;
  PTDWrite_Underline=^TDWrite_Underline;
  DWRITE_UNDERLINE=TDWrite_Underline;
  PDWRITE_UNDERLINE=^TDWrite_Underline;

  TDWrite_Strikethrough=record
    Width:Single;
    Thickness:Single;
    Offset:Single;
    ReadingDirection:TDWrite_ReadingDirection;
    FlowDirection:TDWrite_FlowDirection;
    LocaleName:PWideChar; (* __nullterminated *)
    MeasuringMode:TDWrite_MeasuringMode;
  end;
  PTDWrite_Strikethrough=^TDWrite_Strikethrough;
  DWRITE_STRIKETHROUGH=TDWrite_Strikethrough;
  PDWRITE_STRIKETHROUGH=^TDWrite_Strikethrough;

  TDWrite_LineMetrics=record
    Length:LongWord;
    TrailingWhitespaceLength:LongWord;
    NewlineLength:LongWord;
    Height:Single;
    Baseline:Single;
    IsTrimmed:LongBool;
  end;
  PTDWrite_LineMetrics=^TDWrite_LineMetrics;
  DWRITE_LINE_METRICS=TDWrite_LineMetrics;
  PDWRITE_LINE_METRICS=^TDWrite_LineMetrics;

  TDWrite_ClusterMetrics=record
    Width:Single;
    Length:Word;
    ClusterMetrics:Word;
  end;
  PTDWrite_ClusterMetrics=^TDWrite_ClusterMetrics;
  DWRITE_CLUSTER_METRICS=TDWrite_ClusterMetrics;
  PDWRITE_CLUSTER_METRICS=^TDWrite_ClusterMetrics;

  TDWrite_TextMetrics=record
    Left:Single;
    Top:Single;
    Width:Single;
    WidthIncludingTrailingWhitespace:Single;
    Height:Single;
    LayoutWidth:Single;
    LayoutHeight:Single;
    MaxBidiReorderingDepth:LongWord;
    LineCount:LongWord;
  end;
  PTDWrite_TextMetrics=^TDWrite_TextMetrics;
  DWRITE_TEXT_METRICS=TDWrite_TextMetrics;
  PDWRITE_TEXT_METRICS=^TDWrite_TextMetrics;

  TDWrite_InlineObjectMetrics=record
    Width:Single;
    Height:Single;
    Baseline:Single;
    SupportsSideways:LongBool;
  end;
  PTDWrite_InlineObjectMetrics=^TDWrite_InlineObjectMetrics;
  DWRITE_INLINE_OBJECT_METRICS=TDWrite_InlineObjectMetrics;
  PDWRITE_INLINE_OBJECT_METRICS=^TDWrite_InlineObjectMetrics;

  TDWrite_OverhangMetrics=record
    Left:Single;
    Top:Single;
    Right:Single;
    Bottom:Single;
  end;
  PTDWrite_OverhangMetrics=^TDWrite_OverhangMetrics;
  DWRITE_OVERHANG_METRICS=TDWrite_OverhangMetrics;
  PDWRITE_OVERHANG_METRICS=^TDWrite_OverhangMetrics;

  TDWrite_HitTestMetrics=record
    TextPosition:LongWord;
    Length:LongWord;
    Left:Single;
    Top:Single;
    Width:Single;
    Height:Single;
    BidiLevel:LongWord;
    IsText:LongBool;
    IsTrimmed:LongBool;
  end;
  PTDWrite_HitTestMetrics=^TDWrite_HitTestMetrics;
  DWRITE_HIT_TEST_METRICS=TDWrite_HitTestMetrics;
  PDWRITE_HIT_TEST_METRICS=^TDWrite_HitTestMetrics;

  IDWriteTextRenderer=interface;
  PIDWriteTextRenderer=^IDWriteTextRenderer;

  IDWriteInlineObject=interface(IUnknown)
    ['{8339FDE3-106F-47AB-8373-1C6295EB10B3}']
    function Draw
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      Renderer:IDWriteTextRenderer;
      OriginX:Single;
      OriginY:Single;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function GetMetrics
    (
      out Metrics:TDWrite_InlineObjectMetrics (* __out *)
    ):HResult; stdcall;

    function GetOverhangMetrics
    (
      out Overhangs:TDWrite_OverhangMetrics (* __out *)
    ):HResult; stdcall;

    function GetBreakConditions
    (
      out BreakConditionBefore:TDWrite_BreakCondition; (* __out *)
      out BreakConditionAfter:TDWrite_BreakCondition (* __out *)
    ):HResult; stdcall;
  end;

  IDWritePixelSnapping=interface(IUnknown)
    ['{EAF3A2DA-ECF4-4D24-B644-B34F6842024B}']
    function IsPixelSnappingDisabled
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out IsDisabled:LongBool (* __out *)
    ):HResult; stdcall;

    function GetCurrentTransform
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out Transform:TDWrite_Matrix (* __out *)
    ):HResult; stdcall;

    function GetPixelsPerDip
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      out PixelsPerDip:Single (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteTextRenderer=interface(IDWritePixelSnapping)
    ['{EF8A8135-5CC6-45FE-8825-C5A0724EB819}']
    function DrawGlyphRun
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      const GlyphRunDescription:TDWrite_GlyphRunDescription; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawUnderline
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      const Underline:TDWrite_Underline; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawStrikethrough
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      const Strikethrough:TDWrite_Strikethrough; (* __in *)
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;

    function DrawInlineObject
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      OriginX:Single;
      OriginY:Single;
      InlineObject:IDWriteInlineObject;
      IsSideways:LongBool;
      IsRightToLeft:LongBool;
      ClientDrawingEffect:IUnknown (* __maybenull *)
    ):HResult; stdcall;
  end;

  IDWriteTextLayout=interface(IDWriteTextFormat)
    ['{53737037-6D14-410B-9BFE-0B182BB70961}']
    function SetMaxWidth
    (
      MaxWidth:Single
    ):HResult; stdcall;

    function SetMaxHeight
    (
      MaxHeight:Single
    ):HResult; stdcall;

    function SetFontCollection
    (
      FontCollection:IDWriteFontCollection;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontFamilyName
    (
      FontFamilyName:PWideChar; (* __in_z *)
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontWeight
    (
      FontWeight:TDWrite_FontWeight;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontStyle
    (
      FontStyle:TDWrite_FontStyle;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontStretch
    (
      FontStretch:TDWrite_FontStretch;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetFontSize
    (
      FontSize:Single;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetUnderline
    (
      HasUnderline:LongBool;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetStrikethrough
    (
      HasStrikethrough:LongBool;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetDrawingEffect
    (
      DrawingEffect:IUnknown;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetInlineObject
    (
      InlineObject:IDWriteInlineObject;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetTypography
    (
      Typography:IDWriteTypography;
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function SetLocaleName
    (
      LocaleName:PWideChar; (* __in_z *)
      TextRange:TDWrite_TextRange
    ):HResult; stdcall;

    function GetMaxWidth:Single; stdcall;

    function GetMaxHeight:Single; stdcall;

    function GetFontCollection
    (
      CurrentPosition:LongWord;
      out FontCollection:IDWriteFontCollection; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontFamilyNameLength
    (
      CurrentPosition:LongWord;
      out NameLength:LongWord; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontFamilyName
    (
      CurrentPosition:LongWord;
      FontFamilyName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord;
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontWeight
    (
      CurrentPosition:LongWord;
      out FontWeight:TDWrite_FontWeight; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontStyle
    (
      CurrentPosition:LongWord;
      out FontStyle:TDWrite_FontStyle; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontStretch
    (
      CurrentPosition:LongWord;
      out FontStretch:TDWrite_FontStretch; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetFontSize
    (
      CurrentPosition:LongWord;
      out FontSize:Single; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetUnderline
    (
      CurrentPosition:LongWord;
      out HasUnderline:LongBool; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetStrikethrough
    (
      CurrentPosition:LongWord;
      out HasStrikethrough:LongBool; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetDrawingEffect
    (
      CurrentPosition:LongWord;
      out DrawingEffect:IUnknown; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetInlineObject
    (
      CurrentPosition:LongWord;
      out InlineObject:IDWriteInlineObject; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetTypography
    (
      CurrentPosition:LongWord;
      out Typography:IDWriteTypography; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetLocaleNameLength
    (
      CurrentPosition:LongWord;
      out NameLength:LongWord; (* __out *)
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetLocaleName
    (
      CurrentPosition:LongWord;
      LocaleName:PWideChar; (* __out_ecount_z(nameSize) *)
      NameSize:LongWord;
      TextRange:PTDWrite_TextRange=nil (* __out_opt *)
    ):HResult; stdcall;

    function Draw
    (
      ClientDrawingContext:Pointer; (* __maybenull *)
      Renderer:IDWriteTextRenderer;
      OriginX:Single;
      OriginY:Single
    ):HResult; stdcall;

    function GetLineMetrics
    (
      LineMetrics:PTDWrite_LineMetrics; (* __out_ecount_opt(maxLineCount) *)
      MaxLineCount:LongWord;
      out ActualLineCount:LongWord (* __out *)
    ):HResult; stdcall;

    function GetMetrics
    (
      out TextMetrics:TDWrite_TextMetrics (* __out *)
    ):HResult; stdcall;

    function GetOverhangMetrics
    (
      out Overhangs:TDWrite_OverhangMetrics (* __out *)
    ):HResult; stdcall;

    function GetClusterMetrics
    (
      ClusterMetrics:PTDWrite_ClusterMetrics; (* __out_ecount_opt(maxClusterCount) *)
      MaxClusterCount:LongWord;
      out ActualClusterCount:LongWord (* __out *)
    ):HResult; stdcall;

    function DetermineMinWidth
    (
      out MinWidth:Single (* __out *)
    ):HResult; stdcall;

    function HitTestPoint
    (
      PointX:Single;
      PointY:Single;
      out IsTrailingHit:LongBool; (* __out *)
      out IsInside:LongBool; (* __out *)
      out HitTestMetrics:TDWrite_HitTestMetrics (* __out *)
    ):HResult; stdcall;

    function HitTestTextPosition
    (
      TextPosition:LongWord;
      IsTrailingHit:LongBool;
      out PointX:Single; (* __out *)
      out PointY:Single; (* __out *)
      out HitTestMetrics:TDWrite_HitTestMetrics (* __out *)
    ):HResult; stdcall;

    function HitTestTextRange
    (
      TextPosition:LongWord;
      TextLength:LongWord;
      OriginX:Single;
      OriginY:Single;
      HitTestMetrics:PTDWrite_HitTestMetrics; (* __out_ecount_opt(maxHitTestMetricsCount) *)
      MaxHitTestMetricsCount:LongWord;
      out ActualHitTestMetricsCount:LongWord (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteBitmapRenderTarget=interface(IUnknown)
    ['{5E5A32A3-8DFF-4773-9FF6-0696EAB77267}']
    function DrawGlyphRun
    (
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      MeasuringMode:TDWrite_MeasuringMode;
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      RenderingParams:IDWriteRenderingParams;
      TextColor:TCOLORREF;
      BlackBoxRect:PTRect=nil (* __out_opt *)
    ):HResult; stdcall;

    function GetMemoryDC:HDC; stdcall;

    function GetPixelsPerDip:Single; stdcall;

    function SetPixelsPerDip
    (
      PixelsPerDip:Single
    ):HResult; stdcall;

    function GetCurrentTransform
    (
      out Transform:TDWrite_Matrix (* __out *)
    ):HResult; stdcall;

    function SetCurrentTransform
    (
      Transform:PTDWrite_Matrix (* __in_opt *)
    ):HResult; stdcall;

    function GetSize
    (
      out Size:TSIZE (* __out *)
    ):HResult; stdcall;

    function Resize
    (
      Width:LongWord;
      Height:LongWord
    ):HResult; stdcall;
  end;

  IDWriteGdiInterop=interface(IUnknown)
    ['{1EDD9491-9853-4299-898F-6432983B6F3A}']
    function CreateFontFromLOGFONT
    (
      const LogFont:TLOGFONTW; (* __in *)
      out Font:IDWriteFont (* __out *)
    ):HResult; stdcall;

    function ConvertFontToLOGFONT
    (
      Font:IDWriteFont;
      out LogFont:TLOGFONTW; (* __out *)
      out IsSystemFont:LongBool (* __out *)
    ):HResult; stdcall;

    function ConvertFontFaceToLOGFONT
    (
      Font:IDWriteFontFace;
      out LogFont:TLOGFONTW (* __out *)
    ):HResult; stdcall;

    function CreateFontFaceFromHdc
    (
      hDC:HDC;
      out FontFace:IDWriteFontFace (* __out *)
    ):HResult; stdcall;

    function CreateBitmapRenderTarget
    (
      hDC:HDC; (* __in_opt *)
      Width:LongWord;
      Height:LongWord;
      out RenderTarget:IDWriteBitmapRenderTarget (* __out *)
    ):HResult; stdcall;
  end;

  TDWrite_TextureType=
  (
    DWRITE_TEXTURE_ALIASED_1x1,
    DWRITE_TEXTURE_CLEARTYPE_3x1
  );
  PTDWrite_TextureType=^TDWrite_TextureType;
  DWRITE_TEXTURE_TYPE=TDWrite_TextureType;
  PDWRITE_TEXTURE_TYPE=^TDWrite_TextureType;

  IDWriteGlyphRunAnalysis=interface(IUnknown)
    ['{7D97DBF7-E085-42D4-81E3-6A883BDED118}']
    function GetAlphaTextureBounds
    (
      TextureType:TDWrite_TextureType;
      out TextureBounds:TRect (* __out *)
    ):HResult; stdcall;

    function CreateAlphaTexture
    (
      TextureType:TDWrite_TextureType;
      const TextureBounds:TRect; (* __in *)
      AlphaValues:PByte; (* __out_bcount(bufferSize) *)
      BufferSize:LongWord
    ):HResult; stdcall;

    function GetAlphaBlendParams
    (
      RenderingParams:IDWriteRenderingParams;
      out BlendGamma:Single; (* __out *)
      out BlendEnhancedContrast:Single; (* __out *)
      out BlendClearTypeLevel:Single (* __out *)
    ):HResult; stdcall;
  end;

  IDWriteFactory=interface(IUnknown)
    ['{B859EE5A-D838-4B5B-A2E8-1ADC7D93DB48}']
    function GetSystemFontCollection
    (
      out FontCollection:IDWriteFontCollection; (* __out *)
      CheckForUpdates:LongBool=FALSE
    ):HResult; stdcall;

    function CreateCustomFontCollection
    (
      CollectionLoader:IDWriteFontCollectionLoader;
      CollectionKey:Pointer; (* __in_bcount(collectionKeySize) *)
      CollectionKeySize:LongWord;
      out FontCollection:IDWriteFontCollection (* __out *)
    ):HResult; stdcall;

    function RegisterFontCollectionLoader
    (
      FontCollectionLoader:IDWriteFontCollectionLoader
    ):HResult; stdcall;

    function UnregisterFontCollectionLoader
    (
      FontCollectionLoader:IDWriteFontCollectionLoader
    ):HResult; stdcall;

    function CreateFontFileReference
    (
      FilePath:PWideChar; (* __in_z *)
      LastWriteTime:PFILETIME; (* __in_opt *)
      out FontFile:IDWriteFontFile (* __out *)
    ):HResult; stdcall;

    function CreateCustomFontFileReference
    (
      FontFileReferenceKey:Pointer; (* __in_bcount(fontFileReferenceKeySize) *)
      FontFileReferenceKeySize:LongWord;
      FontFileLoader:IDWriteFontFileLoader;
      out FontFile:IDWriteFontFile (* __out *)
    ):HResult; stdcall;

    function CreateFontFace
    (
      FontFaceType:TDWrite_FontFaceType;
      NumberOfFiles:LongWord;
      FontFiles:PIDWriteFontFile; (* __in_ecount(numberOfFiles) *)
      FaceIndex:LongWord;
      FontFaceSimulationFlags:TDWrite_FontSimulations;
      out FontFace:IDWriteFontFace (* __out *)
    ):HResult; stdcall;

    function CreateRenderingParams
    (
      out RenderingParams:IDWriteRenderingParams (* __out *)
    ):HResult; stdcall;

    function CreateMonitorRenderingParams
    (
      Monitor:HMONITOR;
      out RenderingParams:IDWriteRenderingParams (* __out *)
    ):HResult; stdcall;

    function CreateCustomRenderingParams
    (
      Gamma:Single;
      EnhancedContrast:Single;
      ClearTypeLevel:Single;
      PixelGeometry:TDWrite_PixelGeometry;
      RenderingMode:TDWrite_RenderingMode;
      out RenderingParams:IDWriteRenderingParams (* __out *)
    ):HResult; stdcall;

    function RegisterFontFileLoader
    (
      FontFileLoader:IDWriteFontFileLoader
    ):HResult; stdcall;

    function UnregisterFontFileLoader
    (
      FontFileLoader:IDWriteFontFileLoader
    ):HResult; stdcall;

    function CreateTextFormat
    (
      FontFamilyName:PWideChar; (* __in_z *)
      FontCollection:IDWriteFontCollection; (* __maybenull *)
      FontWeight:TDWrite_FontWeight;
      FontStyle:TDWrite_FontStyle;
      FontStretch:TDWrite_FontStretch;
      FontSize:Single;
      LocaleName:PWideChar; (* __in_z *)
      out TextFormat:IDWriteTextFormat (* __out *)
    ):HResult; stdcall;

    function CreateTypography
    (
      out Typography:IDWriteTypography (* __out *)
    ):HResult; stdcall;

    function GetGdiInterop
    (
      out gdiInterop:IDWriteGdiInterop (* __out *)
    ):HResult; stdcall;

    function CreateTextLayout
    (
      _String:PWideChar; (* __in_ecount(stringLength) *)
      StringLength:LongWord;
      TextFormat:IDWriteTextFormat;
      MaxWidth:Single;
      MaxHeight:Single;
      out TextLayout:IDWriteTextLayout (* __out *)
    ):HResult; stdcall;

    function CreateGdiCompatibleTextLayout
    (
      _String:PWideChar; (* __in_ecount(stringLength) *)
      StringLength:LongWord;
      TextFormat:IDWriteTextFormat;
      LayoutWidth:Single;
      LayoutHeight:Single;
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      UseGdiNatural:LongBool;
      out TextLayout:IDWriteTextLayout (* __out *)
    ):HResult; stdcall;

    function CreateEllipsisTrimmingSign
    (
      TextFormat:IDWriteTextFormat;
      out TrimmingSign:IDWriteInlineObject (* __out *)
    ):HResult; stdcall;

    function CreateTextAnalyzer
    (
      out TextAnalyzer:IDWriteTextAnalyzer (* __out *)
    ):HResult; stdcall;

    function CreateNumberSubstitution
    (
      SubstitutionMethod:TDWrite_NumberSubstitutionMethod; (* __in *)
      LocaleName:PWideChar; (* __in_z *)
      IgnoreUserOverride:LongBool; (* __in *)
      out NumberSubstitution:IDWriteNumberSubstitution (* __out *)
    ):HResult; stdcall;

    function CreateGlyphRunAnalysis
    (
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      PixelsPerDip:Single;
      Transform:PTDWrite_Matrix; (* __in_opt *)
      RenderingMode:TDWrite_RenderingMode;
      MeasuringMode:TDWrite_MeasuringMode;
      BaselineOriginX:Single;
      BaselineOriginY:Single;
      out GlyphRunAnalysis:IDWriteGlyphRunAnalysis (* __out *)
    ):HResult; stdcall;
  end;

var
  DWriteCreateFactory: function
(
  FactoryType:TDWrite_FactoryType; (* __in *)
  const Iid:TGUID; (* __in *)
  out Factory {Interface} (* __out *)
):HResult; stdcall;

///////////////////////////////////////////////////////////////////////////////
// End "DWrite.h"
///////////////////////////////////////////////////////////////////////////////

function DWRITE_MAKE_OPENTYPE_TAG(a,b,c,d:LongWord):LongWord;
begin
  Result:=(d shl 24) or (c shl 16) or (b shl 8) or a;
end;

///////////////////////////////////////////////////////////////////////////////
// Begin "D2DBaseTypes.h"
///////////////////////////////////////////////////////////////////////////////

type
  TD2D_Point2U=record
    X:LongWord;
    Y:LongWord;
  end;
  PTD2D_Point2U=^TD2D_Point2U;
  D2D_POINT_2U=TD2D_Point2U;
  PD2D_POINT_2U=^TD2D_Point2U;

  TD2D_Point2F=record
    X:Single;
    Y:Single;
  end;
  PTD2D_Point2F=^TD2D_Point2F;
  D2D_POINT_2F=TD2D_Point2F;
  PD2D_POINT_2F=^TD2D_Point2F;

  TD2D_RectF=record
    Left:Single;
    Top:Single;
    Right:Single;
    Bottom:Single;
  end;
  PTD2D_RectF=^TD2D_RectF;
  D2D_RECT_F=TD2D_RectF;
  PD2D_RECT_F=^TD2D_RectF;

  TD2D_RectU=record
    Left:LongWord;
    Top:LongWord;
    Right:LongWord;
    Bottom:LongWord;
  end;
  PTD2D_RectU=^TD2D_RectU;
  D2D_RECT_U=TD2D_RectU;
  PD2D_RECT_U=^TD2D_RectU;

  TD2D_SizeF=record
    Width:Single;
    Height:Single;
  end;
  PTD2D_SizeF=^TD2D_SizeF;
  D2D_SIZE_F=TD2D_SizeF;
  PD2D_SIZE_F=^TD2D_SizeF;

  TD2D_SizeU=record
    Width:LongWord;
    Height:LongWord;
  end;
  PTD2D_SizeU=^TD2D_SizeU;
  D2D_SIZE_U=TD2D_SizeU;
  PD2D_SIZE_U=^TD2D_SizeU;

  D2D_COLOR_F=D3DCOLORVALUE;

  TD2D_Matrix3X2F=record
    _11:Single;
    _12:Single;
    _21:Single;
    _22:Single;
    _31:Single;
    _32:Single;
  end;
  PTD2D_Matrix3X2F=^TD2D_Matrix3X2F;
  D2D_MATRIX_3X2_F=TD2D_Matrix3X2F;
  PD2D_MATRIX_3X2_F=^TD2D_Matrix3X2F;

///////////////////////////////////////////////////////////////////////////////
// End "D2DBaseTypes.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D2D1.h"
///////////////////////////////////////////////////////////////////////////////

  ID2D1Factory=interface;
  PID2D1Factory=^ID2D1Factory;

  ID2D1RenderTarget=interface;
  PID2D1RenderTarget=^ID2D1RenderTarget;

  ID2D1BitmapRenderTarget=interface;
  PID2D1BitmapRenderTarget=^ID2D1BitmapRenderTarget;

  ID2D1TessellationSink=interface;
  PID2D1TessellationSink=^ID2D1TessellationSink;

  ID2D1Geometry=interface;
  PID2D1Geometry=^ID2D1Geometry;

  ID2D1Brush=interface;
  PID2D1Brush=^ID2D1Brush;

  ID2D1SimplifiedGeometrySink=interface;

  TD2D1_AlphaMode=
  (
    D2D1_ALPHA_MODE_UNKNOWN=0,
    D2D1_ALPHA_MODE_PREMULTIPLIED=1,
    D2D1_ALPHA_MODE_STRAIGHT=2,
    D2D1_ALPHA_MODE_IGNORE=3
  );
  PTD2D1_AlphaMode=^TD2D1_AlphaMode;
  D2D1_ALPHA_MODE=TD2D1_AlphaMode;
  PD2D1_ALPHA_MODE=^TD2D1_AlphaMode;

  TD2D1_Gamma=
  (
    D2D1_GAMMA_2_2=0,
    D2D1_GAMMA_1_0=1
  );
  PTD2D1_Gamma=^TD2D1_Gamma;
  D2D1_GAMMA=TD2D1_Gamma;
  PD2D1_GAMMA=^TD2D1_Gamma;

  TD2D1_OpacityMaskContent=
  (
    D2D1_OPACITY_MASK_CONTENT_GRAPHICS=0,
    D2D1_OPACITY_MASK_CONTENT_TEXT_NATURAL=1,
    D2D1_OPACITY_MASK_CONTENT_TEXT_GDI_COMPATIBLE=2
  );
  PTD2D1_OpacityMaskContent=^TD2D1_OpacityMaskContent;
  D2D1_OPACITY_MASK_CONTENT=TD2D1_OpacityMaskContent;
  PD2D1_OPACITY_MASK_CONTENT=^TD2D1_OpacityMaskContent;

  TD2D1_ExtendMode=
  (
    D2D1_EXTEND_MODE_CLAMP=0,
    D2D1_EXTEND_MODE_WRAP=1,
    D2D1_EXTEND_MODE_MIRROR=2
  );
  PTD2D1_ExtendMode=^TD2D1_ExtendMode;
  D2D1_EXTEND_MODE=TD2D1_ExtendMode;
  PD2D1_EXTEND_MODE=^TD2D1_ExtendMode;

  TD2D1_AntialiasMode=
  (
    D2D1_ANTIALIAS_MODE_PER_PRIMITIVE=0,
    D2D1_ANTIALIAS_MODE_ALIASED=1
  );
  PTD2D1_AntialiasMode=^TD2D1_AntialiasMode;
  D2D1_ANTIALIAS_MODE=TD2D1_AntialiasMode;
  PD2D1_ANTIALIAS_MODE=^TD2D1_AntialiasMode;

  TD2D1_TextAntialiasMode=
  (
    D2D1_TEXT_ANTIALIAS_MODE_DEFAULT=0,
    D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE=1,
    D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE=2,
    D2D1_TEXT_ANTIALIAS_MODE_ALIASED=3
  );
  PTD2D1_TextAntialiasMode=^TD2D1_TextAntialiasMode;
  D2D1_TEXT_ANTIALIAS_MODE=TD2D1_TextAntialiasMode;
  PD2D1_TEXT_ANTIALIAS_MODE=^TD2D1_TextAntialiasMode;

  TD2D1_BitmapInterpolationMode=
  (
    D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR=0,
    D2D1_BITMAP_INTERPOLATION_MODE_LINEAR=1
  );
  PTD2D1_BitmapInterpolationMode=^TD2D1_BitmapInterpolationMode;
  D2D1_BITMAP_INTERPOLATION_MODE=TD2D1_BitmapInterpolationMode;
  PD2D1_BITMAP_INTERPOLATION_MODE=^TD2D1_BitmapInterpolationMode;

  TD2D1_DrawTextOptions=
  (
    D2D1_DRAW_TEXT_OPTIONS_NO_SNAP=$00000001,
    D2D1_DRAW_TEXT_OPTIONS_CLIP=$00000002,
    D2D1_DRAW_TEXT_OPTIONS_NONE=$00000000
  );
  PTD2D1_DrawTextOptions=^TD2D1_DrawTextOptions;
  D2D1_DRAW_TEXT_OPTIONS=TD2D1_DrawTextOptions;
  PD2D1_DRAW_TEXT_OPTIONS=^TD2D1_DrawTextOptions;

  TD2D1_PixelFormat=record
    Format:TDXGI_Format;
    AlphaMode:TD2D1_AlphaMode;
  end;
  PTD2D1_PixelFormat=^TD2D1_PixelFormat;
  D2D1_PIXEL_FORMAT=TD2D1_PixelFormat;
  PD2D1_PIXEL_FORMAT=^TD2D1_PixelFormat;

  D2D1_POINT_2U=D2D_POINT_2U;
  TD2D1_Point2U=D2D1_POINT_2U;
  PTD2D1_Point2U=^TD2D1_Point2U;
  //
  D2D1_POINT_2F=D2D_POINT_2F;
  TD2D1_Point2F=D2D1_POINT_2F;
  PTD2D1_Point2F=^TD2D1_Point2F;
  //
  D2D1_RECT_F=D2D_RECT_F;
  TD2D1_RectF=D2D1_RECT_F;
  PTD2D1_RectF=^TD2D1_RectF;
  //
  D2D1_RECT_U=D2D_RECT_U;
  TD2D1_RectU=D2D1_RECT_U;
  PTD2D1_RectU=^TD2D1_RectU;
  //
  D2D1_SIZE_F=D2D_SIZE_F;
  TD2D1_SizeF=D2D1_SIZE_F;
  PTD2D1_SizeF=^TD2D1_SizeF;
  //
  D2D1_SIZE_U=D2D_SIZE_U;
  TD2D1_SizeU=D2D1_SIZE_U;
  PTD2D1_SizeU=^TD2D1_SizeU;
  //
  D2D1_COLOR_F=D2D_COLOR_F;
  TD2D1_ColorF=D2D1_COLOR_F;
  PTD2D1_ColorF=^TD2D1_ColorF;
  //
  D2D1_MATRIX_3X2_F=D2D_MATRIX_3X2_F;
  TD2D1_Matrix3X2F=D2D1_MATRIX_3X2_F;
  PTD2D1_Matrix3X2F=^TD2D1_Matrix3X2F;
  //
  TD2D1_Tag=UINT64;
  PTD2D1_Tag=^TD2D1_Tag;

  TD2D1_BitmapProperties=record
    PixelFormat:TD2D1_PixelFormat;
    DpiX:Single;
    DpiY:Single;
  end;
  PTD2D1_BitmapProperties=^TD2D1_BitmapProperties;
  D2D1_BITMAP_PROPERTIES=TD2D1_BitmapProperties;
  PD2D1_BITMAP_PROPERTIES=^TD2D1_BitmapProperties;

  TD2D1_GradientStop=record
    Position:Single;
    Color:TD2D1_ColorF;
  end;
  PTD2D1_GradientStop=^TD2D1_GradientStop;
  D2D1_GRADIENT_STOP=TD2D1_GradientStop;
  PD2D1_GRADIENT_STOP=^TD2D1_GradientStop;

  TD2D1_BrushProperties=record
    Opacity:Single;
    Transform:TD2D1_Matrix3X2F;
  end;
  PTD2D1_BrushProperties=^TD2D1_BrushProperties;
  D2D1_BRUSH_PROPERTIES=TD2D1_BrushProperties;
  PD2D1_BRUSH_PROPERTIES=^TD2D1_BrushProperties;

  TD2D1_BitmapBrushProperties=record
    ExtendModeX:TD2D1_ExtendMode;
    ExtendModeY:TD2D1_ExtendMode;
    InterpolationMode:TD2D1_BitmapInterpolationMode;
  end;
  PTD2D1_BitmapBrushProperties=^TD2D1_BitmapBrushProperties;
  D2D1_BITMAP_BRUSH_PROPERTIES=TD2D1_BitmapBrushProperties;
  PD2D1_BITMAP_BRUSH_PROPERTIES=^TD2D1_BitmapBrushProperties;

  TD2D1_LinearGradientBrushProperties=record
    StartPoint:TD2D1_Point2F;
    EndPoint:TD2D1_Point2F;
  end;
  PTD2D1_LinearGradientBrushProperties=^TD2D1_LinearGradientBrushProperties;
  D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES=TD2D1_LinearGradientBrushProperties;
  PD2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES=^TD2D1_LinearGradientBrushProperties;

  TD2D1_RadialGradientBrushProperties=record
    Center:TD2D1_Point2F;
    GradientOriginOffset:TD2D1_Point2F;
    RadiusX:Single;
    RadiusY:Single;
  end;
  PTD2D1_RadialGradientBrushProperties=^TD2D1_RadialGradientBrushProperties;
  D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES=TD2D1_RadialGradientBrushProperties;
  PD2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES=^TD2D1_RadialGradientBrushProperties;

  TD2D1_ArcSize=
  (
    D2D1_ARC_SIZE_SMALL=0,
    D2D1_ARC_SIZE_LARGE=1
  );
  PTD2D1_ArcSize=^TD2D1_ArcSize;
  D2D1_ARC_SIZE=TD2D1_ArcSize;
  PD2D1_ARC_SIZE=^TD2D1_ArcSize;

  TD2D1_CapStyle=
  (
    D2D1_CAP_STYLE_FLAT=0,
    D2D1_CAP_STYLE_SQUARE=1,
    D2D1_CAP_STYLE_ROUND=2,
    D2D1_CAP_STYLE_TRIANGLE=3
  );
  PTD2D1_CapStyle=^TD2D1_CapStyle;
  D2D1_CAP_STYLE=TD2D1_CapStyle;
  PD2D1_CAP_STYLE=^TD2D1_CapStyle;

  TD2D1_DashStyle=
  (
    D2D1_DASH_STYLE_SOLID=0,
    D2D1_DASH_STYLE_DASH=1,
    D2D1_DASH_STYLE_DOT=2,
    D2D1_DASH_STYLE_DASH_DOT=3,
    D2D1_DASH_STYLE_DASH_DOT_DOT=4,
    D2D1_DASH_STYLE_CUSTOM=5
  );
  PTD2D1_DashStyle=^TD2D1_DashStyle;
  D2D1_DASH_STYLE=TD2D1_DashStyle;
  PD2D1_DASH_STYLE=^TD2D1_DashStyle;

  TD2D1_LineJoin=
  (
    D2D1_LINE_JOIN_MITER=0,
    D2D1_LINE_JOIN_BEVEL=1,
    D2D1_LINE_JOIN_ROUND=2,
    D2D1_LINE_JOIN_MITER_OR_BEVEL=3
  );
  PTD2D1_LineJoin=^TD2D1_LineJoin;
  D2D1_LINE_JOIN=TD2D1_LineJoin;
  PD2D1_LINE_JOIN=^TD2D1_LineJoin;

  TD2D1_CombineMode=
  (
    D2D1_COMBINE_MODE_UNION=0,
    D2D1_COMBINE_MODE_INTERSECT=1,
    D2D1_COMBINE_MODE_XOR=2,
    D2D1_COMBINE_MODE_EXCLUDE=3
  );
  PTD2D1_CombineMode=^TD2D1_CombineMode;
  D2D1_COMBINE_MODE=TD2D1_CombineMode;
  PD2D1_COMBINE_MODE=^TD2D1_CombineMode;

  TD2D1_GeometryRelation=
  (
    D2D1_GEOMETRY_RELATION_UNKNOWN=0,
    D2D1_GEOMETRY_RELATION_DISJOINT=1,
    D2D1_GEOMETRY_RELATION_IS_CONTAINED=2,
    D2D1_GEOMETRY_RELATION_CONTAINS=3,
    D2D1_GEOMETRY_RELATION_OVERLAP=4
  );
  PTD2D1_GeometryRelation=^TD2D1_GeometryRelation;
  D2D1_GEOMETRY_RELATION=TD2D1_GeometryRelation;
  PD2D1_GEOMETRY_RELATION=^TD2D1_GeometryRelation;

  TD2D1_GeometrySimplificationOption=
  (
    D2D1_GEOMETRY_SIMPLIFICATION_OPTION_CUBICS_AND_LINES=0,
    D2D1_GEOMETRY_SIMPLIFICATION_OPTION_LINES=1
  );
  PTD2D1_GeometrySimplificationOption=^TD2D1_GeometrySimplificationOption;
  D2D1_GEOMETRY_SIMPLIFICATION_OPTION=TD2D1_GeometrySimplificationOption;
  PD2D1_GEOMETRY_SIMPLIFICATION_OPTION=^TD2D1_GeometrySimplificationOption;

  TD2D1_FigureBegin=
  (
    D2D1_FIGURE_BEGIN_FILLED=0,
    D2D1_FIGURE_BEGIN_HOLLOW=1
  );
  PTD2D1_FigureBegin=^TD2D1_FigureBegin;
  D2D1_FIGURE_BEGIN=TD2D1_FigureBegin;
  PD2D1_FIGURE_BEGIN=^TD2D1_FigureBegin;

  TD2D1_FigureEnd=
  (
    D2D1_FIGURE_END_OPEN=0,
    D2D1_FIGURE_END_CLOSED=1
  );
  PTD2D1_FigureEnd=^TD2D1_FigureEnd;
  D2D1_FIGURE_END=TD2D1_FigureEnd;
  PD2D1_FIGURE_END=^TD2D1_FigureEnd;

  TD2D1_BezierSegment=record
    Point1:TD2D1_Point2F;
    Point2:TD2D1_Point2F;
    Point3:TD2D1_Point2F;
  end;
  PTD2D1_BezierSegment=^TD2D1_BezierSegment;
  D2D1_BEZIER_SEGMENT=TD2D1_BezierSegment;
  PD2D1_BEZIER_SEGMENT=^TD2D1_BezierSegment;

  TD2D1_Triangle=record
    Point1:TD2D1_Point2F;
    Point2:TD2D1_Point2F;
    Point3:TD2D1_Point2F;
  end;
  PTD2D1_Triangle=^TD2D1_Triangle;
  D2D1_TRIANGLE=TD2D1_Triangle;
  PD2D1_TRIANGLE=^TD2D1_Triangle;

  TD2D1_PathSegment=
  (
    D2D1_PATH_SEGMENT_NONE=$00000000,
    D2D1_PATH_SEGMENT_FORCE_UNSTROKED=$00000001,
    D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN=$00000002
  );
  PTD2D1_PathSegment=^TD2D1_PathSegment;
  D2D1_PATH_SEGMENT=TD2D1_PathSegment;
  PD2D1_PATH_SEGMENT=^TD2D1_PathSegment;

  TD2D1_SweepDirection=
  (
    D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE=0,
    D2D1_SWEEP_DIRECTION_CLOCKWISE=1
  );
  PTD2D1_SweepDirection=^TD2D1_SweepDirection;
  D2D1_SWEEP_DIRECTION=TD2D1_SweepDirection;
  PD2D1_SWEEP_DIRECTION=^TD2D1_SweepDirection;

  TD2D1_FillMode=
  (
    D2D1_FILL_MODE_ALTERNATE=0,
    D2D1_FILL_MODE_WINDING=1
  );
  PTD2D1_FillMode=^TD2D1_FillMode;
  D2D1_FILL_MODE=TD2D1_FillMode;
  PD2D1_FILL_MODE=^TD2D1_FillMode;

  TD2D1_ArcSegment=record
    Point:TD2D1_Point2F;
    Size:TD2D1_SizeF;
    RotationAngle:Single;
    SweepDirection:TD2D1_SweepDirection;
    ArcSize:TD2D1_ArcSize;
  end;
  PTD2D1_ArcSegment=^TD2D1_ArcSegment;
  D2D1_ARC_SEGMENT=TD2D1_ArcSegment;
  PD2D1_ARC_SEGMENT=^TD2D1_ArcSegment;

  TD2D1_QuadraticBezierSegment=record
    Point1:TD2D1_Point2F;
    Point2:TD2D1_Point2F;
  end;
  PTD2D1_QuadraticBezierSegment=^TD2D1_QuadraticBezierSegment;
  D2D1_QUADRATIC_BEZIER_SEGMENT=TD2D1_QuadraticBezierSegment;
  PD2D1_QUADRATIC_BEZIER_SEGMENT=^TD2D1_QuadraticBezierSegment;

  TD2D1_Ellipse=record
    Point:TD2D1_Point2F;
    RadiusX:Single;
    RadiusY:Single;
  end;
  PTD2D1_Ellipse=^TD2D1_Ellipse;
  D2D1_ELLIPSE=TD2D1_Ellipse;
  PD2D1_ELLIPSE=^TD2D1_Ellipse;

  TD2D1_RoundedRect=record
    Rect:TD2D1_RectF;
    RadiusX:Single;
    RadiusY:Single;
  end;
  PTD2D1_RoundedRect=^TD2D1_RoundedRect;
  D2D1_ROUNDED_RECT=TD2D1_RoundedRect;
  PD2D1_ROUNDED_RECT=^TD2D1_RoundedRect;

  TD2D1_StrokeStyleProperties=record
    StartCap:TD2D1_CapStyle;
    EndCap:TD2D1_CapStyle;
    DashCap:TD2D1_CapStyle;
    LineJoin:TD2D1_LineJoin;
    MiterLimit:Single;
    DashStyle:TD2D1_DashStyle;
    DashOffset:Single;
  end;
  PTD2D1_StrokeStyleProperties=^TD2D1_StrokeStyleProperties;
  D2D1_STROKE_STYLE_PROPERTIES=TD2D1_StrokeStyleProperties;
  PD2D1_STROKE_STYLE_PROPERTIES=^TD2D1_StrokeStyleProperties;

  TD2D1_LayerOptions=
  (
    D2D1_LAYER_OPTIONS_NONE=$00000000,
    D2D1_LAYER_OPTIONS_INITIALIZE_FOR_CLEARTYPE=$00000001
  );
  PTD2D1_LayerOptions=^TD2D1_LayerOptions;
  D2D1_LAYER_OPTIONS=TD2D1_LayerOptions;
  PD2D1_LAYER_OPTIONS=^TD2D1_LayerOptions;

  TD2D1_LayerParameters=record
    ContentBounds:TD2D1_RectF;
    GeometricMask:ID2D1Geometry; (* __field_ecount_opt(1) *)
    MaskAntialiasMode:TD2D1_AntialiasMode;
    MaskTransform:TD2D1_Matrix3X2F;
    Opacity:Single;
    OpacityBrush:ID2D1Brush; (* __field_ecount_opt(1) *)
    LayerOptions:TD2D1_LayerOptions;
  end;
  PTD2D1_LayerParameters=^TD2D1_LayerParameters;
  D2D1_LAYER_PARAMETERS=TD2D1_LayerParameters;
  PD2D1_LAYER_PARAMETERS=^TD2D1_LayerParameters;

  TD2D1_WindowState=
  (
    D2D1_WINDOW_STATE_NONE=$0000000,
    D2D1_WINDOW_STATE_OCCLUDED=$0000001
  );
  PTD2D1_WindowState=^TD2D1_WindowState;
  D2D1_WINDOW_STATE=TD2D1_WindowState;
  PD2D1_WINDOW_STATE=^TD2D1_WindowState;

  TD2D1_RenderTargetType=
  (
    D2D1_RENDER_TARGET_TYPE_DEFAULT=0,
    D2D1_RENDER_TARGET_TYPE_SOFTWARE=1,
    D2D1_RENDER_TARGET_TYPE_HARDWARE=2
  );
  PTD2D1_RenderTargetType=^TD2D1_RenderTargetType;
  D2D1_RENDER_TARGET_TYPE=TD2D1_RenderTargetType;
  PD2D1_RENDER_TARGET_TYPE=^TD2D1_RenderTargetType;

  TD2D1_FeatureLevel=
  (
    D2D1_FEATURE_LEVEL_DEFAULT=0,
    D2D1_FEATURE_LEVEL_9=Integer(D3D10_FEATURE_LEVEL_9_1),
    D2D1_FEATURE_LEVEL_10=Integer(D3D10_FEATURE_LEVEL_10_0)
  );
  PTD2D1_FeatureLevel=^TD2D1_FeatureLevel;
  D2D1_FEATURE_LEVEL=TD2D1_FeatureLevel;
  PD2D1_FEATURE_LEVEL=^TD2D1_FeatureLevel;

  TD2D1_RenderTargetUsage=
  (
    D2D1_RENDER_TARGET_USAGE_NONE=$00000000,
    D2D1_RENDER_TARGET_USAGE_FORCE_BITMAP_REMOTING=$00000001,
    D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE=$00000002
  );
  PTD2D1_RenderTargetUsage=^TD2D1_RenderTargetUsage;
  D2D1_RENDER_TARGET_USAGE=TD2D1_RenderTargetUsage;
  PD2D1_RENDER_TARGET_USAGE=^TD2D1_RenderTargetUsage;

  TD2D1_PresentOptions=
  (
    D2D1_PRESENT_OPTIONS_NONE=$00000000,
    D2D1_PRESENT_OPTIONS_RETAIN_CONTENTS=$00000001,
    D2D1_PRESENT_OPTIONS_IMMEDIATELY=$00000002
  );
  PTD2D1_PresentOptions=^TD2D1_PresentOptions;
  D2D1_PRESENT_OPTIONS=TD2D1_PresentOptions;
  PD2D1_PRESENT_OPTIONS=^TD2D1_PresentOptions;

  TD2D1_RenderTargetProperties=record
    _Type:TD2D1_RenderTargetType;
    PixelFormat:TD2D1_PixelFormat;
    DpiX:Single;
    DpiY:Single;
    Usage:TD2D1_RenderTargetUsage;
    MinLevel:TD2D1_FeatureLevel;
  end;
  PTD2D1_RenderTargetProperties=^TD2D1_RenderTargetProperties;
  D2D1_RENDER_TARGET_PROPERTIES=TD2D1_RenderTargetProperties;
  PD2D1_RENDER_TARGET_PROPERTIES=^TD2D1_RenderTargetProperties;

  TD2D1_HwndRenderTargetProperties=record
    hWnd:HWND;
    PixelSize:TD2D1_SizeU;
    PresentOptions:TD2D1_PresentOptions;
  end;
  PTD2D1_HwndRenderTargetProperties=^TD2D1_HwndRenderTargetProperties;
  D2D1_HWND_RENDER_TARGET_PROPERTIES=TD2D1_HwndRenderTargetProperties;
  PD2D1_HWND_RENDER_TARGET_PROPERTIES=^TD2D1_HwndRenderTargetProperties;

  TD2D1_CompatibleRenderTargetOptions=
  (
    D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_NONE=$00000000,
    D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_GDI_COMPATIBLE=$00000001
  );
  PTD2D1_CompatibleRenderTargetOptions=^TD2D1_CompatibleRenderTargetOptions;
  D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS=TD2D1_CompatibleRenderTargetOptions;
  PD2D1_COMPATIBLE_RENDER_TARGET_OPTIONS=^TD2D1_CompatibleRenderTargetOptions;

  TD2D1_DrawingStateDescription=record
    AntialiasMode:TD2D1_AntialiasMode;
    TextAntialiasMode:TD2D1_TextAntialiasMode;
    Tag1:TD2D1_Tag;
    Tag2:TD2D1_Tag;
    Transform:TD2D1_Matrix3X2F;
  end;
  PTD2D1_DrawingStateDescription=^TD2D1_DrawingStateDescription;
  D2D1_DRAWING_STATE_DESCRIPTION=TD2D1_DrawingStateDescription;
  PD2D1_DRAWING_STATE_DESCRIPTION=^TD2D1_DrawingStateDescription;

  TD2D1_DcInitializeMode=
  (
    D2D1_DC_INITIALIZE_MODE_COPY=0,
    D2D1_DC_INITIALIZE_MODE_CLEAR=1
  );
  PTD2D1_DcInitializeMode=^TD2D1_DcInitializeMode;
  D2D1_DC_INITIALIZE_MODE=TD2D1_DcInitializeMode;
  PD2D1_DC_INITIALIZE_MODE=^TD2D1_DcInitializeMode;

  TD2D1_DebugLevel=
  (
    D2D1_DEBUG_LEVEL_NONE=0,
    D2D1_DEBUG_LEVEL_ERROR=1,
    D2D1_DEBUG_LEVEL_WARNING=2,
    D2D1_DEBUG_LEVEL_INFORMATION=3
  );
  PTD2D1_DebugLevel=^TD2D1_DebugLevel;
  D2D1_DEBUG_LEVEL=TD2D1_DebugLevel;
  PD2D1_DEBUG_LEVEL=^TD2D1_DebugLevel;

  TD2D1_FactoryType=
  (
    D2D1_FACTORY_TYPE_SINGLE_THREADED=0,
    D2D1_FACTORY_TYPE_MULTI_THREADED=1
  );
  PTD2D1_FactoryType=^TD2D1_FactoryType;
  D2D1_FACTORY_TYPE=TD2D1_FactoryType;
  PD2D1_FACTORY_TYPE=^TD2D1_FactoryType;

  TD2D1_FactoryOptions=record
    DebugLevel:TD2D1_DebugLevel;
  end;
  PTD2D1_FactoryOptions=^TD2D1_FactoryOptions;
  D2D1_FACTORY_OPTIONS=TD2D1_FactoryOptions;
  PD2D1_FACTORY_OPTIONS=^TD2D1_FactoryOptions;

  ID2D1Resource=interface(IUnknown)
    ['{2CD90691-12E2-11DC-9FED-001143A055F9}']
    procedure GetFactory
    (
      out Factory:ID2D1Factory (* __deref_out *)
    ); stdcall;
  end;

  ID2D1Bitmap=interface(ID2D1Resource)
    ['{A2296057-EA42-4099-983B-539FB6505426}']
    procedure GetSize
    (
      out o_Result:TD2D1_SizeF (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)
    ); stdcall;

    procedure GetPixelSize
    (
      out o_Result:TD2D1_SizeU (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)
    ); stdcall;

    procedure GetPixelFormat
    (
      out o_Result:TD2D1_PixelFormat (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)
    ); stdcall;

    procedure GetDpi
    (
      out DpiX:Single; (* __out *)
      out DpiY:Single (* __out *)
    ); stdcall;

    function CopyFromBitmap
    (
      DestPoint:PTD2D1_Point2U; (* __in_opt *)
      Bitmap:ID2D1Bitmap; (* __in *)
      SrcRect:PTD2D1_RectU (* __in_opt *)
    ):HResult; stdcall;

    function CopyFromRenderTarget
    (
      DestPoint:PTD2D1_Point2U; (* __in_opt *)
      RenderTarget:ID2D1RenderTarget; (* __in *)
      SrcRect:PTD2D1_RectU (* __in_opt *)
    ):HResult; stdcall;

    function CopyFromMemory
    (
      DstRect:PTD2D1_RectU; (* __in_opt *)
      SrcData:Pointer; (* __in *)
      Pitch:LongWord
    ):HResult; stdcall;
  end;

  ID2D1GradientStopCollection=interface(ID2D1Resource)
    ['{2CD906A7-12E2-11DC-9FED-001143A055F9}']
    function GetGradientStopCount:LongWord; stdcall;

    procedure GetGradientStops
    (
      GradientStops:PTD2D1_GradientStop; (* __out_ecount(gradientStopsCount) *)
      GradientStopsCount:LongWord
    ); stdcall;

    function GetColorInterpolationGamma:TD2D1_Gamma; stdcall;

    function GetExtendMode:TD2D1_ExtendMode; stdcall;
  end;

  ID2D1Brush=interface(ID2D1Resource)
    ['{2CD906A8-12E2-11DC-9FED-001143A055F9}']
    procedure SetOpacity
    (
      Opacity:Single
    ); stdcall;

    procedure SetTransform
    (
      const Transform:TD2D1_Matrix3X2F (* __in *)
    ); stdcall;

    function GetOpacity:Single; stdcall;

    procedure GetTransform
    (
      out Transform:TD2D1_Matrix3X2F (* __out *)
    ); stdcall;
  end;

  ID2D1BitmapBrush=interface(ID2D1Brush)
    ['{2CD906AA-12E2-11DC-9FED-001143A055F9}']
    procedure SetExtendModeX
    (
      ExtendModeX:TD2D1_ExtendMode
    ); stdcall;

    procedure SetExtendModeY
    (
      ExtendModeY:TD2D1_ExtendMode
    ); stdcall;

    procedure SetInterpolationMode
    (
      InterpolationMode:TD2D1_BitmapInterpolationMode
    ); stdcall;

    procedure SetBitmap
    (
      Bitmap:ID2D1Bitmap (* __in *)
    ); stdcall;

    function GetExtendModeX:TD2D1_ExtendMode; stdcall;

    function GetExtendModeY:TD2D1_ExtendMode; stdcall;

    function GetInterpolationMode:TD2D1_BitmapInterpolationMode; stdcall;

    procedure GetBitmap
    (
      out Bitmap:ID2D1Bitmap (* __deref_out *)
    ); stdcall;
  end;

  ID2D1SolidColorBrush=interface(ID2D1Brush)
    ['{2CD906A9-12E2-11DC-9FED-001143A055F9}']
    procedure SetColor
    (
      const Color:TD2D1_ColorF (* __in *)
    ); stdcall;

    function GetColor:TD2D1_ColorF; stdcall;
  end;

  ID2D1LinearGradientBrush=interface(ID2D1Brush)
    ['{2CD906AB-12E2-11DC-9FED-001143A055F9}']
    procedure SetStartPoint
    (
      StartPoint:TD2D1_Point2F
    ); stdcall;

    procedure SetEndPoint
    (
      EndPoint:TD2D1_Point2F
    ); stdcall;

    function GetStartPoint:TD2D1_Point2F; stdcall;

    function GetEndPoint:TD2D1_Point2F; stdcall;

    procedure GetGradientStopCollection
    (
      out GradientStopCollection:ID2D1GradientStopCollection (* __deref_out *)
    ); stdcall;
  end;

  ID2D1RadialGradientBrush=interface(ID2D1Brush)
    ['{2CD906AC-12E2-11DC-9FED-001143A055F9}']
    procedure SetCenter
    (
      Center:TD2D1_Point2F
    ); stdcall;

    procedure SetGradientOriginOffset
    (
      GradientOriginOffset:TD2D1_Point2F
    ); stdcall;

    procedure SetRadiusX
    (
      RadiusX:Single
    ); stdcall;

    procedure SetRadiusY
    (
      RadiusY:Single
    ); stdcall;

    function GetCenter:TD2D1_Point2F; stdcall;

    function GetGradientOriginOffset:TD2D1_Point2F; stdcall;

    function GetRadiusX:Single; stdcall;

    function GetRadiusY:Single; stdcall;

    procedure GetGradientStopCollection
    (
      out GradientStopCollection:ID2D1GradientStopCollection (* __deref_out *)
    ); stdcall;
  end;

  ID2D1StrokeStyle=interface(ID2D1Resource)
    ['{2CD9069D-12E2-11DC-9FED-001143A055F9}']
    function GetStartCap:TD2D1_CapStyle; stdcall;

    function GetEndCap:TD2D1_CapStyle; stdcall;

    function GetDashCap:TD2D1_CapStyle; stdcall;

    function GetMiterLimit:Single; stdcall;

    function GetLineJoin:TD2D1_LineJoin; stdcall;

    function GetDashOffset:Single; stdcall;

    function GetDashStyle:TD2D1_DashStyle; stdcall;

    function GetDashesCount:LongWord; stdcall;

    procedure GetDashes
    (
      Dashes:PSingle; (* __out_ecount(dashesCount) *)
      DashesCount:LongWord
    ); stdcall;
  end;

  ID2D1Geometry=interface(ID2D1Resource)
    ['{2CD906A1-12E2-11DC-9FED-001143A055F9}']
    function GetBounds
    (
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      out Bounds:TD2D1_RectF (* __out *)
    ):HResult; stdcall;

    function GetWidenedBounds
    (
      StrokeWidth:Single;
      StrokeStyle:ID2D1StrokeStyle; (* __in_opt *)
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      out Bounds:TD2D1_RectF (* __out *)
    ):HResult; stdcall;

    function StrokeContainsPoint
    (
      Point:TD2D1_Point2F;
      StrokeWidth:Single;
      StrokeStyle:ID2D1StrokeStyle; (* __in_opt *)
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      out Contains:LongBool (* __out *)
    ):HResult; stdcall;

    function FillContainsPoint
    (
      Point:TD2D1_Point2F;
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      out Contains:LongBool (* __out *)
    ):HResult; stdcall;

    function CompareWithGeometry
    (
      InputGeometry:ID2D1Geometry; (* __in *)
      InputGeometryTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      out Relation:TD2D1_GeometryRelation (* __out *)
    ):HResult; stdcall;

    function Simplify
    (
      SimplificationOption:TD2D1_GeometrySimplificationOption;
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      GeometrySink:ID2D1SimplifiedGeometrySink (* __in *)
    ):HResult; stdcall;

    function Tessellate
    (
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      TessellationSink:ID2D1TessellationSink (* __in *)
    ):HResult; stdcall;

    function CombineWithGeometry
    (
      InputGeometry:ID2D1Geometry; (* __in *)
      CombineMode:TD2D1_CombineMode;
      const InputGeometryTransform: PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      GeometrySink:ID2D1SimplifiedGeometrySink (* __in *)
    ):HResult; stdcall;

    function Outline
    (
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      GeometrySink:ID2D1SimplifiedGeometrySink (* __in *)
    ):HResult; stdcall;

    function ComputeArea
    (
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      out Area:Single (* __out *)
    ):HResult; stdcall;

    function ComputeLength
    (
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      out Length:Single (* __out *)
    ):HResult; stdcall;

    function ComputePointAtLength
    (
      Length:Single;
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      Point:PTD2D1_Point2F; (* __out_opt *)
      UnitTangentVector:PTD2D1_Point2F (* __out_opt *)
    ):HResult; stdcall;

    function Widen
    (
      StrokeWidth:Single;
      StrokeStyle:ID2D1StrokeStyle; (* __in_opt *)
      WorldTransform:PTD2D1_Matrix3X2F; (* __in_opt *)
      FlatteningTolerance:Single;
      GeometrySink:ID2D1SimplifiedGeometrySink (* __in *)
    ):HResult; stdcall;
  end;

  ID2D1RectangleGeometry=interface(ID2D1Geometry)
    ['{2CD906A2-12E2-11DC-9FED-001143A055F9}']
    procedure GetRect
    (
      out Rect:TD2D1_RectF (* __out *)
    ); stdcall;
  end;

  ID2D1RoundedRectangleGeometry=interface(ID2D1Geometry)
    ['{2CD906A3-12E2-11DC-9FED-001143A055F9}']
    procedure GetRoundedRect
    (
      out RoundedRect:TD2D1_RoundedRect (* __out *)
    ); stdcall;
  end;

  ID2D1EllipseGeometry=interface(ID2D1Geometry)
    ['{2CD906A4-12E2-11DC-9FED-001143A055F9}']
    procedure GetEllipse
    (
      out Ellipse:TD2D1_Ellipse (* __out *)
    ); stdcall;
  end;

  ID2D1GeometryGroup=interface(ID2D1Geometry)
    ['{2CD906A6-12E2-11DC-9FED-001143A055F9}']
    function GetFillMode:TD2D1_FillMode; stdcall;

    function GetSourceGeometryCount:LongWord; stdcall;

    procedure GetSourceGeometries
    (
      Geometries:PID2D1Geometry; (* __out_ecount(geometriesCount) *)
      GeometriesCount:LongWord
    ); stdcall;
  end;

  ID2D1TransformedGeometry=interface(ID2D1Geometry)
    ['{2CD906BB-12E2-11DC-9FED-001143A055F9}']
    procedure GetSourceGeometry
    (
      out SourceGeometry:ID2D1Geometry (* __deref_out *)
    ); stdcall;

    procedure GetTransform
    (
      out Transform:TD2D1_Matrix3X2F (* __out *)
    ); stdcall;
  end;

  ID2D1SimplifiedGeometrySink=interface(IUnknown)
    ['{2CD9069E-12E2-11DC-9FED-001143A055F9}']
    procedure SetFillMode
    (
      FillMode:TD2D1_FillMode
    ); stdcall;

    procedure SetSegmentFlags
    (
      VertexFlags:TD2D1_PathSegment
    ); stdcall;

    procedure BeginFigure
    (
      StartPoint:TD2D1_Point2F;
      FigureBegin:TD2D1_FigureBegin
    ); stdcall;

    procedure AddLines
    (
      Points:PTD2D1_Point2F; (* __in_ecount(pointsCount) *)
      PointsCount:LongWord
    ); stdcall;

    procedure AddBeziers
    (
      Beziers:PTD2D1_BezierSegment; (* __in_ecount(beziersCount) *)
      BeziersCount:LongWord
    ); stdcall;

    procedure EndFigure
    (
      FigureEnd:TD2D1_FigureEnd
    ); stdcall;

    function Close:HResult; stdcall;
  end;

  ID2D1GeometrySink=interface(ID2D1SimplifiedGeometrySink)
    ['{2CD9069F-12E2-11DC-9FED-001143A055F9}']
    procedure AddLine
    (
      Point:TD2D1_Point2F
    ); stdcall;

    procedure AddBezier
    (
      const Bezier:TD2D1_BezierSegment (* __in *)
    ); stdcall;

    procedure AddQuadraticBezier
    (
      const Bezier:TD2D1_QuadraticBezierSegment (* __in *)
    ); stdcall;

    procedure AddQuadraticBeziers
    (
      Beziers:PTD2D1_QuadraticBezierSegment; (* __in_ecount(beziersCount) *)
      BeziersCount:LongWord
    ); stdcall;

    procedure AddArc
    (
      const Arc:TD2D1_ArcSegment (* __in *)
    ); stdcall;
  end;

  ID2D1TessellationSink=interface(IUnknown)
    ['{2CD906C1-12E2-11DC-9FED-001143A055F9}']
    procedure AddTriangles
    (
      Triangles:PTD2D1_Triangle; (* __in_ecount(trianglesCount) *)
      TrianglesCount:LongWord
    ); stdcall;

    function Close:HResult; stdcall;
  end;

  ID2D1PathGeometry=interface(ID2D1Geometry)
    ['{2CD906A5-12E2-11DC-9FED-001143A055F9}']
    function Open
    (
      out GeometrySink:ID2D1GeometrySink (* __deref_out *)
    ):HResult; stdcall;

    function Stream
    (
      GeometrySink:ID2D1GeometrySink (* __in *)
    ):HResult; stdcall;

    function GetSegmentCount
    (
      out Count:LongWord (* __out *)
    ):HResult; stdcall;

    function GetFigureCount
    (
      out Count:LongWord (* __out *)
    ):HResult; stdcall;
  end;

  ID2D1Mesh=interface(ID2D1Resource)
    ['{2CD906C2-12E2-11DC-9FED-001143A055F9}']
    function Open
    (
      out TessellationSink:ID2D1TessellationSink (* __deref_out *)
    ):HResult; stdcall;
  end;

  ID2D1Layer=interface(ID2D1Resource)
    ['{2CD9069B-12E2-11DC-9FED-001143A055F9}']
    procedure GetSize(
      out o_Result:TD2D1_SizeF (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)); stdcall;
  end;

  ID2D1DrawingStateBlock=interface(ID2D1Resource)
    ['{28506E39-EBF6-46A1-BB47-FD85565AB957}']
    procedure GetDescription
    (
      out StateDescription:TD2D1_DrawingStateDescription (* __out *)
    ); stdcall;

    procedure SetDescription
    (
      const StateDescription:TD2D1_DrawingStateDescription (* __in *)
    ); stdcall;

    procedure SetTextRenderingParams
    (
      TextRenderingParams:IDWriteRenderingParams=nil (* __in_opt *)
    ); stdcall;

    procedure GetTextRenderingParams
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}textRenderingParams:PIDWriteRenderingParams{$ELSE}out TextRenderingParams:IDWriteRenderingParams{$ENDIF} (* __deref_out_opt *)
    ); stdcall;
  end;

  ID2D1RenderTarget=interface(ID2D1Resource)
    ['{2CD90694-12E2-11DC-9FED-001143A055F9}']
    function CreateBitmap
    (
      Size:TD2D1_SizeU;
      SrcData:Pointer; (* __in_opt *)
      Pitch:LongWord;
      const BitmapProperties:TD2D1_BitmapProperties; (* __in *)
      out Bitmap:ID2D1Bitmap (* __deref_out *)
    ):HResult; stdcall;

    function CreateBitmapFromWicBitmap
    (
      WicBitmapSource:IUnknown {JSB: To do: IWICBitmapSource}; (* __in *)
      BitmapProperties:PTD2D1_BitmapProperties; (* __in_opt *)
      out Bitmap:ID2D1Bitmap (* __deref_out *)
    ):HResult; stdcall;

    function CreateSharedBitmap
    (
      const IID:TGUID; (* __in *)
      Data:Pointer; (* __inout *)
      BitmapProperties:PTD2D1_BitmapProperties; (* __in_opt *)
      out Bitmap:ID2D1Bitmap (* __deref_out *)
    ):HResult; stdcall;

    function CreateBitmapBrush
    (
      Bitmap:ID2D1Bitmap; (* __in *)
      BitmapBrushProperties:PTD2D1_BitmapBrushProperties; (* __in_opt *)
      BrushProperties:PTD2D1_BrushProperties; (* __in_opt *)
      out BitmapBrush:ID2D1BitmapBrush (* __deref_out *)
    ):HResult; stdcall;

    function CreateSolidColorBrush
    (
      const Color:TD2D1_ColorF; (* __in *)
      BrushProperties:PTD2D1_BrushProperties; (* __in_opt *)
      out SolidColorBrush:ID2D1SolidColorBrush (* __deref_out *)
    ):HResult; stdcall;

    function CreateGradientStopCollection
    (
      GradientStops:PTD2D1_GradientStop; (* __in_ecount(gradientStopsCount) *)
      GradientStopsCount:LongWord; (* __range(>=,1) *)
      ColorInterpolationGamma:TD2D1_Gamma;
      ExtendMode:TD2D1_ExtendMode;
      out GradientStopCollection:ID2D1GradientStopCollection (* __deref_out *)
    ):HResult; stdcall;

    function CreateLinearGradientBrush
    (
      const LinearGradientBrushProperties:TD2D1_LinearGradientBrushProperties; (* __in *)
      BrushProperties:PTD2D1_BrushProperties; (* __in_opt *)
      GradientStopCollection:ID2D1GradientStopCollection; (* __in *)
      out LinearGradientBrush:ID2D1LinearGradientBrush (* __deref_out *)
    ):HResult; stdcall;

    function CreateRadialGradientBrush
    (
      const RadialGradientBrushProperties:TD2D1_RadialGradientBrushProperties; (* __in *)
      BrushProperties:PTD2D1_BrushProperties; (* __in_opt *)
      GradientStopCollection:ID2D1GradientStopCollection; (* __in *)
      out RadialGradientBrush:ID2D1RadialGradientBrush (* __deref_out *)
    ):HResult; stdcall;

    function CreateCompatibleRenderTarget
    (
      DesiredSize:PTD2D1_SizeF; (* __in_opt *)
      DesiredPixelSize:PTD2D1_SizeU; (* __in_opt *)
      DesiredFormat:PTD2D1_PixelFormat; (* __in_opt *)
      Options:TD2D1_CompatibleRenderTargetOptions;
      out BitmapRenderTarget:ID2D1BitmapRenderTarget (* __deref_out *)
    ):HResult; stdcall;

    function CreateLayer
    (
      Size:PTD2D1_SizeF; (* __in_opt *)
      out Layer:ID2D1Layer (* __deref_out *)
    ):HResult; stdcall;

    function CreateMesh
    (
      out Mesh:ID2D1Mesh (* __deref_out *)
    ):HResult; stdcall;

    procedure DrawLine
    (
      Point0:TD2D1_Point2F;
      Point1:TD2D1_Point2F;
      Brush:ID2D1Brush; (* __in *)
      StrokeWidth:Single=1.0;
      StrokeStyle:ID2D1StrokeStyle=nil (* __in_opt *)
    ); stdcall;

    procedure DrawRectangle
    (
      const Rect:TD2D1_RectF; (* __in *)
      Brush:ID2D1Brush; (* __in *)
      StrokeWidth:Single=1.0;
      StrokeStyle:ID2D1StrokeStyle=nil (* __in_opt *)
    ); stdcall;

    procedure FillRectangle
    (
      const Rect:TD2D1_RectF; (* __in *)
      Brush:ID2D1Brush (* __in *)
    ); stdcall;

    procedure DrawRoundedRectangle
    (
      const RoundedRect:TD2D1_RoundedRect; (* __in *)
      Brush:ID2D1Brush; (* __in *)
      StrokeWidth:Single=1.0;
      StrokeStyle:ID2D1StrokeStyle=nil (* __in_opt *)
    ); stdcall;

    procedure FillRoundedRectangle
    (
      const RoundedRect:TD2D1_RoundedRect; (* __in *)
      Brush:ID2D1Brush (* __in *)
    ); stdcall;

    procedure DrawEllipse
    (
      const Ellipse:TD2D1_Ellipse; (* __in *)
      Brush:ID2D1Brush; (* __in *)
      StrokeWidth:Single=1.0;
      StrokeStyle:ID2D1StrokeStyle=nil (* __in_opt *)
    ); stdcall;

    procedure FillEllipse
    (
      const Ellipse:TD2D1_Ellipse; (* __in *)
      Brush:ID2D1Brush (* __in *)
    ); stdcall;

    procedure DrawGeometry
    (
      Geometry:ID2D1Geometry; (* __in *)
      Brush:ID2D1Brush; (* __in *)
      StrokeWidth:Single=1.0;
      StrokeStyle:ID2D1StrokeStyle=nil (* __in_opt *)
    ); stdcall;

    procedure FillGeometry
    (
      Geometry:ID2D1Geometry; (* __in *)
      Brush:ID2D1Brush; (* __in *)
      OpacityBrush:ID2D1Brush=nil (* __in_opt *)
    ); stdcall;

    procedure FillMesh
    (
      Mesh:ID2D1Mesh; (* __in *)
      Brush:ID2D1Brush (* __in *)
    ); stdcall;

    procedure FillOpacityMask
    (
      OpacityMask:ID2D1Bitmap; (* __in *)
      Brush:ID2D1Brush; (* __in *)
      Content:TD2D1_OpacityMaskContent;
      DestinationRectangle:PTD2D1_RectF=nil; (* __in_opt *)
      SourceRectangle:PTD2D1_RectF=nil (* __in_opt *)
    ); stdcall;

    procedure DrawBitmap
    (
      Bitmap:ID2D1Bitmap; (* __in *)
      DestinationRectangle:PTD2D1_RectF=nil; (* __in_opt *)
      Opacity:Single=1.0;
      InterpolationMode:TD2D1_BitmapInterpolationMode=D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
      SourceRectangle:PTD2D1_RectF=nil (* __in_opt *)
    ); stdcall;

    procedure DrawText
    (
      _String:PWideChar; (* __in_ecount(stringLength) *)
      StringLength:LongWord;
      TextFormat:IDWriteTextFormat; (* __in *)
      const LayoutRect:TD2D1_RectF; (* __in *)
      DefaultForegroundBrush:ID2D1Brush; (* __in *)
      Options:TD2D1_DrawTextOptions=D2D1_DRAW_TEXT_OPTIONS_NONE;
      MeasuringMode:TDWrite_MeasuringMode=DWRITE_MEASURING_MODE_NATURAL
    ); stdcall;

    procedure DrawTextLayout
    (
      Origin:TD2D1_Point2F;
      TextLayout:IDWriteTextLayout; (* __in *)
      DefaultForegroundBrush:ID2D1Brush; (* __in *)
      Options:TD2D1_DrawTextOptions=D2D1_DRAW_TEXT_OPTIONS_NONE
    ); stdcall;

    procedure DrawGlyphRun
    (
      BaselineOrigin:TD2D1_Point2F;
      const GlyphRun:TDWrite_GlyphRun; (* __in *)
      ForegroundBrush:ID2D1Brush; (* __in *)
      MeasuringMode:TDWrite_MeasuringMode=DWRITE_MEASURING_MODE_NATURAL
    ); stdcall;

    procedure SetTransform
    (
      const Transform:TD2D1_Matrix3X2F (* __in *)
    ); stdcall;

    procedure GetTransform
    (
      out Transform:TD2D1_Matrix3X2F (* __out *)
    ); stdcall;

    procedure SetAntialiasMode
    (
      AntialiasMode:TD2D1_AntialiasMode
    ); stdcall;

    function GetAntialiasMode:TD2D1_AntialiasMode; stdcall;

    procedure SetTextAntialiasMode
    (
      TextAntialiasMode:TD2D1_TextAntialiasMode
    ); stdcall;

    function GetTextAntialiasMode:TD2D1_TextAntialiasMode; stdcall;

    procedure SetTextRenderingParams
    (
      TextRenderingParams:IDWriteRenderingParams=nil (* __in_opt *)
    ); stdcall;

    procedure GetTextRenderingParams
    (
      {$IFDEF UsePointersForOptionalOutputInterfaces}textRenderingParams:PIDWriteRenderingParams{$ELSE}out TextRenderingParams:IDWriteRenderingParams{$ENDIF} (* __deref_out_opt *)
    ); stdcall;

    procedure SetTags
    (
      Tag1:TD2D1_Tag;
      Tag2:TD2D1_Tag
    ); stdcall;

    procedure GetTags
    (
      Tag1:PTD2D1_Tag=nil; (* __out_opt *)
      Tag2:PTD2D1_Tag=nil (* __out_opt *)
    ); stdcall;

    procedure PushLayer
    (
      const LayerParameters:TD2D1_LayerParameters; (* __in *)
      Layer:ID2D1Layer (* __in *)
    ); stdcall;

    procedure PopLayer; stdcall;

    function Flush
    (
      Tag1:PTD2D1_Tag=nil; (* __out_opt *)
      Tag2:PTD2D1_Tag=nil (* __out_opt *)
    ):HResult; stdcall;

    procedure SaveDrawingState
    (
      DrawingStateBlock:ID2D1DrawingStateBlock (* __in *)
    ); stdcall;

    procedure RestoreDrawingState
    (
      DrawingStateBlock:ID2D1DrawingStateBlock (* __in *)
    ); stdcall;

    procedure PushAxisAlignedClip
    (
      const ClipRect:TD2D1_RectF; (* __in *)
      AntialiasMode:TD2D1_AntialiasMode
    ); stdcall;

    procedure PopAxisAlignedClip; stdcall;

    procedure Clear
    (
      const ClearColor:TD2D1_ColorF (* __in_opt *) // JSB: Added 'const' for compatibility with Embarcado version. Use TD2D1_ColorF(nil^) for nil.
    ); stdcall;

    procedure BeginDraw; stdcall;

    function EndDraw
    (
      Tag1:PTD2D1_Tag=nil; (* __out_opt *)
      Tag2:PTD2D1_Tag=nil (* __out_opt *)
    ):HResult; stdcall;

    procedure GetPixelFormat
    (
      out o_Result:TD2D1_PixelFormat (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)
    ); stdcall;

    procedure SetDpi
    (
      DpiX:Single;
      DpiY:Single
    ); stdcall;

    procedure GetDpi
    (
      out DpiX:Single; (* __out *)
      out DpiY:Single (* __out *)
    ); stdcall;

    procedure GetSize
    (
      out o_Result:TD2D1_SizeF (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)
    ); stdcall;

    procedure GetPixelSize
    (
      out o_Result:TD2D1_SizeU (* Workaround for the Delphi parameter passing order bug for stdcall methods that return a struct/record. *)
    ); stdcall;

    function GetMaximumBitmapSize:LongWord; stdcall;

    function IsSupported
    (
      const RenderTargetProperties:TD2D1_RenderTargetProperties (* __in *)
    ):LongBool; stdcall;
  end;

  ID2D1BitmapRenderTarget=interface(ID2D1RenderTarget)
    ['{2CD90695-12E2-11DC-9FED-001143A055F9}']
    function GetBitmap
    (
      out Bitmap:ID2D1Bitmap (* __deref_out *)
    ):HResult; stdcall;
  end;

  ID2D1HwndRenderTarget=interface(ID2D1RenderTarget)
    ['{2CD90698-12E2-11DC-9FED-001143A055F9}']
    function CheckWindowState:TD2D1_WindowState; stdcall;

    function Resize
    (
      const PixelSize:TD2D1_SizeU (* __in *)
    ):HResult; stdcall;

    function GetHwnd:HWND; stdcall;
  end;

  ID2D1GdiInteropRenderTarget=interface(IUnknown)
    ['{E0DB51C3-6F77-4BAE-B3D5-E47509B35838}']
    function GetDC
    (
      Mode:TD2D1_DcInitializeMode;
      out hDC:HDC (* __out *)
    ):HResult; stdcall;

    function ReleaseDC
    (
      Update:PTRect (* __in_opt *)
    ):HResult; stdcall;
  end;

  ID2D1DCRenderTarget=interface(ID2D1RenderTarget)
    ['{1C51BC64-DE61-46FD-9899-63A5D8F03950}']
    function BindDC
    (
      hDC:HDC; (* __in *)
      const SubRect:TRect (* __in *)
    ):HResult; stdcall;
  end;

  ID2D1Factory=interface(IUnknown)
    ['{06152247-6F50-465A-9245-118BFD3B6007}']
    function ReloadSystemMetrics:HResult; stdcall;

    procedure GetDesktopDpi
    (
      out DpiX:Single; (* __out *)
      out DpiY:Single (* __out *)
    ); stdcall;

    function CreateRectangleGeometry
    (
      const Rectangle:TD2D1_RectF; (* __in *)
      out RectangleGeometry:ID2D1RectangleGeometry (* __deref_out *)
    ):HResult; stdcall;

    function CreateRoundedRectangleGeometry
    (
      const RoundedRectangle:TD2D1_RoundedRect; (* __in *)
      out RoundedRectangleGeometry:ID2D1RoundedRectangleGeometry (* __deref_out *)
    ):HResult; stdcall;

    function CreateEllipseGeometry
    (
      const Ellipse:TD2D1_Ellipse; (* __in *)
      out EllipseGeometry:ID2D1EllipseGeometry (* __deref_out *)
    ):HResult; stdcall;

    function CreateGeometryGroup
    (
      FillMode:TD2D1_FillMode;
      Geometries:PID2D1Geometry; (* __in_ecount(geometriesCount) *)
      GeometriesCount:LongWord;
      out GeometryGroup:ID2D1GeometryGroup (* __deref_out *)
    ):HResult; stdcall;

    function CreateTransformedGeometry
    (
      SourceGeometry:ID2D1Geometry; (* __in *)
      const Transform:TD2D1_Matrix3X2F; (* __in *)
      out TransformedGeometry:ID2D1TransformedGeometry (* __deref_out *)
    ):HResult; stdcall;

    function CreatePathGeometry
    (
      out PathGeometry:ID2D1PathGeometry (* __deref_out *)
    ):HResult; stdcall;

    function CreateStrokeStyle
    (
      const StrokeStyleProperties:TD2D1_StrokeStyleProperties; (* __in *)
      Dashes:PSingle; (* __in_ecount_opt(dashesCount) *)
      DashesCount:LongWord;
      out StrokeStyle:ID2D1StrokeStyle (* __deref_out *)
    ):HResult; stdcall;

    function CreateDrawingStateBlock
    (
      DrawingStateDescription:PTD2D1_DrawingStateDescription; (* __in_opt *)
      TextRenderingParams:IDWriteRenderingParams; (* __in_opt *)
      out DrawingStateBlock:ID2D1DrawingStateBlock (* __deref_out *)
    ):HResult; stdcall;

    function CreateWicBitmapRenderTarget
    (
      Target:IUnknown {JSB: To do: IWICBitmap}; (* __in *)
      const RenderTargetProperties:TD2D1_RenderTargetProperties; (* __in *)
      out RenderTarget:ID2D1RenderTarget (* __deref_out *)
    ):HResult; stdcall;

    function CreateHwndRenderTarget
    (
      const RenderTargetProperties:TD2D1_RenderTargetProperties; (* __in *)
      const HwndRenderTargetProperties:TD2D1_HwndRenderTargetProperties; (* __in *)
      out HwndRenderTarget:ID2D1HwndRenderTarget (* __deref_out *)
    ):HResult; stdcall;

    function CreateDxgiSurfaceRenderTarget
    (
      Surface:IDXGISurface; (* __in *)
      const RenderTargetProperties:TD2D1_RenderTargetProperties; (* __in *)
      out RenderTarget:ID2D1RenderTarget (* __deref_out *)
    ):HResult; stdcall;

    function CreateDCRenderTarget
    (
      const RenderTargetProperties:TD2D1_RenderTargetProperties; (* __in *)
      out RenderTarget:ID2D1DCRenderTarget (* __deref_out *)
    ):HResult; stdcall;
  end;

var

D2D1CreateFactory: function
(
  FactoryType:TD2D1_FactoryType; (* __in *)
  const IID:TGUID; (* __in *)
  pFactoryOptions:PTD2D1_FactoryOptions; (* __in_opt *)
  out pIFactory {IUnknown} (* __out *)
):HResult; stdcall;


///////////////////////////////////////////////////////////////////////////////
// End "D2D1.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D2Derr.h"
///////////////////////////////////////////////////////////////////////////////

const
  FACILITY_D2D=$899;
  D2D_HRESULT_Base=(1 shl 31) or (FACILITY_D2D shl 16);

//!!!  D2D_ERROR_UNSUPPORTED_PIXEL_FORMAT=WINCODEC_ERR_UNSUPPORTEDPIXELFORMAT;
//!!!  D2D_ERROR_INSUFFICIENT_BUFFER=HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER);
  D2D_ERROR_WRONG_STATE=HResult(D2D_HRESULT_Base or $001);
  D2D_ERROR_NOT_INITIALIZED=HResult(D2D_HRESULT_Base or $002);
  D2D_ERROR_UNSUPPORTED_OPERATION=HResult(D2D_HRESULT_Base or $003);
  D2D_ERROR_SCANNER_FAILED=HResult(D2D_HRESULT_Base or $004);
  D2D_ERROR_SCREEN_ACCESS_DENIED=HResult(D2D_HRESULT_Base or $005);
  D2D_ERROR_DISPLAY_STATE_INVALID=HResult(D2D_HRESULT_Base or $006);
  D2D_ERROR_ZERO_VECTOR=HResult(D2D_HRESULT_Base or $007);
  D2D_ERROR_INTERNAL_ERROR=HResult(D2D_HRESULT_Base or $008);
  D2D_ERROR_DISPLAY_FORMAT_NOT_SUPPORTED=HResult(D2D_HRESULT_Base or $009);
  D2D_ERROR_INVALID_CALL=HResult(D2D_HRESULT_Base or $00A);
  D2D_ERROR_NO_HARDWARE_DEVICE=HResult(D2D_HRESULT_Base or $00B);
  D2D_ERROR_RECREATE_TARGET=HResult(D2D_HRESULT_Base or $00C);
  D2D_ERROR_TOO_MANY_SHADER_ELEMENTS=HResult(D2D_HRESULT_Base or $00D);
  D2D_ERROR_SHADER_COMPILE_FAILED=HResult(D2D_HRESULT_Base or $00E);
  D2D_ERROR_MAX_TEXTURE_SIZE_EXCEEDED=HResult(D2D_HRESULT_Base or $00F);
  D2D_ERROR_UNSUPPORTED_VERSION=HResult(D2D_HRESULT_Base or $010);
  D2D_ERROR_BAD_NUMBER=HResult(D2D_HRESULT_Base or $0011);
  D2D_ERROR_WRONG_FACTORY=HResult(D2D_HRESULT_Base or $012);
  D2D_ERROR_LAYER_ALREADY_IN_USE=HResult(D2D_HRESULT_Base or $013);
  D2D_ERROR_POP_CALL_DID_NOT_MATCH_PUSH=HResult(D2D_HRESULT_Base or $014);
  D2D_ERROR_WRONG_RESOURCE_DOMAIN=HResult(D2D_HRESULT_Base or $015);
  D2D_ERROR_PUSH_POP_UNBALANCED=HResult(D2D_HRESULT_Base or $016);
  D2D_ERROR_RENDER_TARGET_HAS_LAYER_OR_CLIPRECT=HResult(D2D_HRESULT_Base or $017);
  D2D_ERROR_INCOMPATIBLE_BRUSH_TYPES=HResult(D2D_HRESULT_Base or $018);
  D2D_ERROR_WIN32_ERROR=HResult(D2D_HRESULT_Base or $019);
  D2D_ERROR_TARGET_NOT_GDI_COMPATIBLE=HResult(D2D_HRESULT_Base or $01A);
  D2D_ERROR_TEXT_EFFECT_IS_WRONG_TYPE=HResult(D2D_HRESULT_Base or $01B);
  D2D_ERROR_TEXT_RENDERER_NOT_RELEASED=HResult(D2D_HRESULT_Base or $01C);
  D2D_ERROR_EXCEEDS_MAX_BITMAP_SIZE=HResult(D2D_HRESULT_Base or $01D);

///////////////////////////////////////////////////////////////////////////////
// End "D2Derr.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D2D1Helper.h"
///////////////////////////////////////////////////////////////////////////////

type
  // Predefined colors. The item prefix is required so that "Tan" does not clash with "Math.Tan"!
  // It is a shame that Delphi enumerations do not work the same way as .Net and insist on the enumeration type being used with the item e.g. "TD2D1_Color.Tan".
  TD2D1_Color=
  (
    D2D1_Color_AliceBlue=$F0F8FF,
    D2D1_Color_AntiqueWhite=$FAEBD7,
    D2D1_Color_Aqua=$00FFFF,
    D2D1_Color_Aquamarine=$7FFFD4,
    D2D1_Color_Azure=$F0FFFF,
    D2D1_Color_Beige=$F5F5DC,
    D2D1_Color_Bisque=$FFE4C4,
    D2D1_Color_Black=$000000,
    D2D1_Color_BlanchedAlmond=$FFEBCD,
    D2D1_Color_Blue=$0000FF,
    D2D1_Color_BlueViolet=$8A2BE2,
    D2D1_Color_Brown=$A52A2A,
    D2D1_Color_BurlyWood=$DEB887,
    D2D1_Color_CadetBlue=$5F9EA0,
    D2D1_Color_Chartreuse=$7FFF00,
    D2D1_Color_Chocolate=$D2691E,
    D2D1_Color_Coral=$FF7F50,
    D2D1_Color_CornflowerBlue=$6495ED,
    D2D1_Color_Cornsilk=$FFF8DC,
    D2D1_Color_Crimson=$DC143C,
    D2D1_Color_Cyan=$00FFFF,
    D2D1_Color_DarkBlue=$00008B,
    D2D1_Color_DarkCyan=$008B8B,
    D2D1_Color_DarkGoldenrod=$B8860B,
    D2D1_Color_DarkGray=$A9A9A9,
    D2D1_Color_DarkGreen=$006400,
    D2D1_Color_DarkKhaki=$BDB76B,
    D2D1_Color_DarkMagenta=$8B008B,
    D2D1_Color_DarkOliveGreen=$556B2F,
    D2D1_Color_DarkOrange=$FF8C00,
    D2D1_Color_DarkOrchid=$9932CC,
    D2D1_Color_DarkRed=$8B0000,
    D2D1_Color_DarkSalmon=$E9967A,
    D2D1_Color_DarkSeaGreen=$8FBC8F,
    D2D1_Color_DarkSlateBlue=$483D8B,
    D2D1_Color_DarkSlateGray=$2F4F4F,
    D2D1_Color_DarkTurquoise=$00CED1,
    D2D1_Color_DarkViolet=$9400D3,
    D2D1_Color_DeepPink=$FF1493,
    D2D1_Color_DeepSkyBlue=$00BFFF,
    D2D1_Color_DimGray=$696969,
    D2D1_Color_DodgerBlue=$1E90FF,
    D2D1_Color_Firebrick=$B22222,
    D2D1_Color_FloralWhite=$FFFAF0,
    D2D1_Color_ForestGreen=$228B22,
    D2D1_Color_Fuchsia=$FF00FF,
    D2D1_Color_Gainsboro=$DCDCDC,
    D2D1_Color_GhostWhite=$F8F8FF,
    D2D1_Color_Gold=$FFD700,
    D2D1_Color_Goldenrod=$DAA520,
    D2D1_Color_Gray=$808080,
    D2D1_Color_Green=$008000,
    D2D1_Color_GreenYellow=$ADFF2F,
    D2D1_Color_Honeydew=$F0FFF0,
    D2D1_Color_HotPink=$FF69B4,
    D2D1_Color_IndianRed=$CD5C5C,
    D2D1_Color_Indigo=$4B0082,
    D2D1_Color_Ivory=$FFFFF0,
    D2D1_Color_Khaki=$F0E68C,
    D2D1_Color_Lavender=$E6E6FA,
    D2D1_Color_LavenderBlush=$FFF0F5,
    D2D1_Color_LawnGreen=$7CFC00,
    D2D1_Color_LemonChiffon=$FFFACD,
    D2D1_Color_LightBlue=$ADD8E6,
    D2D1_Color_LightCoral=$F08080,
    D2D1_Color_LightCyan=$E0FFFF,
    D2D1_Color_LightGoldenrodYellow=$FAFAD2,
    D2D1_Color_LightGreen=$90EE90,
    D2D1_Color_LightGray=$D3D3D3,
    D2D1_Color_LightPink=$FFB6C1,
    D2D1_Color_LightSalmon=$FFA07A,
    D2D1_Color_LightSeaGreen=$20B2AA,
    D2D1_Color_LightSkyBlue=$87CEFA,
    D2D1_Color_LightSlateGray=$778899,
    D2D1_Color_LightSteelBlue=$B0C4DE,
    D2D1_Color_LightYellow=$FFFFE0,
    D2D1_Color_Lime=$00FF00,
    D2D1_Color_LimeGreen=$32CD32,
    D2D1_Color_Linen=$FAF0E6,
    D2D1_Color_Magenta=$FF00FF,
    D2D1_Color_Maroon=$800000,
    D2D1_Color_MediumAquamarine=$66CDAA,
    D2D1_Color_MediumBlue=$0000CD,
    D2D1_Color_MediumOrchid=$BA55D3,
    D2D1_Color_MediumPurple=$9370DB,
    D2D1_Color_MediumSeaGreen=$3CB371,
    D2D1_Color_MediumSlateBlue=$7B68EE,
    D2D1_Color_MediumSpringGreen=$00FA9A,
    D2D1_Color_MediumTurquoise=$48D1CC,
    D2D1_Color_MediumVioletRed=$C71585,
    D2D1_Color_MidnightBlue=$191970,
    D2D1_Color_MintCream=$F5FFFA,
    D2D1_Color_MistyRose=$FFE4E1,
    D2D1_Color_Moccasin=$FFE4B5,
    D2D1_Color_NavajoWhite=$FFDEAD,
    D2D1_Color_Navy=$000080,
    D2D1_Color_OldLace=$FDF5E6,
    D2D1_Color_Olive=$808000,
    D2D1_Color_OliveDrab=$6B8E23,
    D2D1_Color_Orange=$FFA500,
    D2D1_Color_OrangeRed=$FF4500,
    D2D1_Color_Orchid=$DA70D6,
    D2D1_Color_PaleGoldenrod=$EEE8AA,
    D2D1_Color_PaleGreen=$98FB98,
    D2D1_Color_PaleTurquoise=$AFEEEE,
    D2D1_Color_PaleVioletRed=$DB7093,
    D2D1_Color_PapayaWhip=$FFEFD5,
    D2D1_Color_PeachPuff=$FFDAB9,
    D2D1_Color_Peru=$CD853F,
    D2D1_Color_Pink=$FFC0CB,
    D2D1_Color_Plum=$DDA0DD,
    D2D1_Color_PowderBlue=$B0E0E6,
    D2D1_Color_Purple=$800080,
    D2D1_Color_Red=$FF0000,
    D2D1_Color_RosyBrown=$BC8F8F,
    D2D1_Color_RoyalBlue=$4169E1,
    D2D1_Color_SaddleBrown=$8B4513,
    D2D1_Color_Salmon=$FA8072,
    D2D1_Color_SandyBrown=$F4A460,
    D2D1_Color_SeaGreen=$2E8B57,
    D2D1_Color_SeaShell=$FFF5EE,
    D2D1_Color_Sienna=$A0522D,
    D2D1_Color_Silver=$C0C0C0,
    D2D1_Color_SkyBlue=$87CEEB,
    D2D1_Color_SlateBlue=$6A5ACD,
    D2D1_Color_SlateGray=$708090,
    D2D1_Color_Snow=$FFFAFA,
    D2D1_Color_SpringGreen=$00FF7F,
    D2D1_Color_SteelBlue=$4682B4,
    D2D1_Color_Tan=$D2B48C,
    D2D1_Color_Teal=$008080,
    D2D1_Color_Thistle=$D8BFD8,
    D2D1_Color_Tomato=$FF6347,
    D2D1_Color_Turquoise=$40E0D0,
    D2D1_Color_Violet=$EE82EE,
    D2D1_Color_Wheat=$F5DEB3,
    D2D1_Color_White=$FFFFFF,
    D2D1_Color_WhiteSmoke=$F5F5F5,
    D2D1_Colors_Yellow=$FFFF00,
    D2D1_Color_YellowGreen=$9ACD32
  );

const
  D2D1Matrix3x2F_Identity:TD2D1_Matrix3X2F=(_11:1;_12:0;_21:0;_22:1;_31:0;_32:1);

///////////////////////////////////////////////////////////////////////////////
// End "D2D1Helper.h"
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Begin "D2D1Helper.h"
///////////////////////////////////////////////////////////////////////////////

(*!!!
    //
    // Forward declared IdentityMatrix function to allow matrix class to use
    // these constructors.
    //
    D2D1FORCEINLINE
    D2D1_MATRIX_3X2_F
    IdentityMatrix();

    //
    // The default trait type for objects in D2D is float.
    //
    template<typename Type>
    struct TypeTraits
    {
        typedef D2D1_POINT_2F Point;
        typedef D2D1_SIZE_F   Size;
        typedef D2D1_RECT_F   Rect;
    };

    template<>
    struct TypeTraits<UINT32>
    {
        typedef D2D1_POINT_2U Point;
        typedef D2D1_SIZE_U   Size;
        typedef D2D1_RECT_U   Rect;
    };

    static inline
    FLOAT FloatMax()
    {
        #ifdef FLT_MAX
            return FLT_MAX;
        #else
            return 3.402823466e+38F;
        #endif
    }

    //
    // Construction helpers
    //
    template<typename Type>
    D2D1FORCEINLINE
    typename TypeTraits<Type>::Point
    Point2(
        Type x,
        Type y
        )
    {
        typename TypeTraits<Type>::Point point = { x, y };

        return point;
    }

    D2D1FORCEINLINE
    D2D1_POINT_2F
    Point2F(
        FLOAT x = 0.f,
        FLOAT y = 0.f
        )
    {
        return Point2<FLOAT>(x, y);
    }

    D2D1FORCEINLINE
    D2D1_POINT_2U
    Point2U(
        UINT32 x = 0,
        UINT32 y = 0
        )
    {
        return Point2<UINT32>(x, y);
    }

    template<typename Type>
    D2D1FORCEINLINE
    typename TypeTraits<Type>::Size
    Size(
        Type width,
        Type height
        )
    {
        typename TypeTraits<Type>::Size size = { width, height };

        return size;
    }

    D2D1FORCEINLINE
    D2D1_SIZE_F
    SizeF(
        FLOAT width = 0.f,
        FLOAT height = 0.f
        )
    {
        return Size<FLOAT>(width, height);
    }

    D2D1FORCEINLINE
    D2D1_SIZE_U
    SizeU(
        UINT32 width = 0,
        UINT32 height = 0
        )
    {
        return Size<UINT32>(width, height);
    }

    template<typename Type>
    D2D1FORCEINLINE
    typename TypeTraits<Type>::Rect
    Rect(
        Type left,
        Type top,
        Type right,
        Type bottom
        )
    {
        typename TypeTraits<Type>::Rect rect = { left, top, right, bottom };

        return rect;
    }
*)

function D2D1RectF(i_Left,i_Top,i_Right,i_Bottom:Single):TD2D1_RectF;
begin
  Result.Left:=i_Left;
  Result.Top:=i_Top;
  Result.Right:=i_Right;
  Result.Bottom:=i_Bottom;
end;

function D2D1RectU(i_Left,i_Top,i_Right,i_Bottom:LongWord):TD2D1_RectU;
begin
  Result.Left:=i_Left;
  Result.Top:=i_Top;
  Result.Right:=i_Right;
  Result.Bottom:=i_Bottom;
end;

function InfiniteRect: TD2D1_RectF;
begin
  Result := D2D1RectF(-MaxSingle, -MaxSingle, MaxSingle, MaxSingle);
end;

(*
    D2D1FORCEINLINE
    D2D1_RECT_F
    InfiniteRect()
    {
        D2D1_RECT_F rect = { -FloatMax(), -FloatMax(), FloatMax(),  FloatMax() };

        return rect;
    }

    D2D1FORCEINLINE
    D2D1_ARC_SEGMENT
    ArcSegment(
        __in CONST D2D1_POINT_2F &point,
        __in CONST D2D1_SIZE_F &size,
        __in FLOAT rotationAngle,
        __in D2D1_SWEEP_DIRECTION sweepDirection,
        __in D2D1_ARC_SIZE arcSize
        )
    {
        D2D1_ARC_SEGMENT arcSegment = { point, size, rotationAngle, sweepDirection, arcSize };

        return arcSegment;
    }

    D2D1FORCEINLINE
    D2D1_BEZIER_SEGMENT
    BezierSegment(
        __in CONST D2D1_POINT_2F &point1,
        __in CONST D2D1_POINT_2F &point2,
        __in CONST D2D1_POINT_2F &point3
        )
    {
        D2D1_BEZIER_SEGMENT bezierSegment = { point1, point2, point3 };

        return bezierSegment;
    }

    D2D1FORCEINLINE
    D2D1_ELLIPSE
    Ellipse(
        __in CONST D2D1_POINT_2F &center,
        FLOAT radiusX,
        FLOAT radiusY
        )
    {
        D2D1_ELLIPSE ellipse;

        ellipse.point = center;
        ellipse.radiusX = radiusX;
        ellipse.radiusY = radiusY;

        return ellipse;
    }

    D2D1FORCEINLINE
    D2D1_ROUNDED_RECT
    RoundedRect(
        __in CONST D2D1_RECT_F &rect,
        FLOAT radiusX,
        FLOAT radiusY
        )
    {
        D2D1_ROUNDED_RECT roundedRect;

        roundedRect.rect = rect;
        roundedRect.radiusX = radiusX;
        roundedRect.radiusY = radiusY;

        return roundedRect;
    }

    D2D1FORCEINLINE
    D2D1_BRUSH_PROPERTIES
    BrushProperties(
        __in FLOAT opacity = 1.0,
        __in CONST D2D1_MATRIX_3X2_F &transform = D2D1::IdentityMatrix()
        )
    {
        D2D1_BRUSH_PROPERTIES brushProperties;

        brushProperties.opacity = opacity;
        brushProperties.transform = transform;

        return brushProperties;
    }

    D2D1FORCEINLINE
    D2D1_GRADIENT_STOP
    GradientStop(
        FLOAT position,
        __in CONST D2D1_COLOR_F &color
        )
    {
        D2D1_GRADIENT_STOP gradientStop = { position, color };

        return gradientStop;
    }

    D2D1FORCEINLINE
    D2D1_QUADRATIC_BEZIER_SEGMENT
    QuadraticBezierSegment(
        __in CONST D2D1_POINT_2F &point1,
        __in CONST D2D1_POINT_2F &point2
        )
    {
        D2D1_QUADRATIC_BEZIER_SEGMENT quadraticBezier = { point1, point2 };

        return quadraticBezier;
    }

    D2D1FORCEINLINE
    D2D1_STROKE_STYLE_PROPERTIES
    StrokeStyleProperties(
        D2D1_CAP_STYLE startCap = D2D1_CAP_STYLE_FLAT,
        D2D1_CAP_STYLE endCap = D2D1_CAP_STYLE_FLAT,
        D2D1_CAP_STYLE dashCap = D2D1_CAP_STYLE_FLAT,
        D2D1_LINE_JOIN lineJoin = D2D1_LINE_JOIN_MITER,
        FLOAT miterLimit = 10.0f,
        D2D1_DASH_STYLE dashStyle = D2D1_DASH_STYLE_SOLID,
        FLOAT dashOffset = 0.0f
        )
    {
        D2D1_STROKE_STYLE_PROPERTIES strokeStyleProperties;

        strokeStyleProperties.startCap = startCap;
        strokeStyleProperties.endCap = endCap;
        strokeStyleProperties.dashCap = dashCap;
        strokeStyleProperties.lineJoin = lineJoin;
        strokeStyleProperties.miterLimit = miterLimit;
        strokeStyleProperties.dashStyle = dashStyle;
        strokeStyleProperties.dashOffset = dashOffset;

        return strokeStyleProperties;
    }

    D2D1FORCEINLINE
    D2D1_BITMAP_BRUSH_PROPERTIES
    BitmapBrushProperties(
        D2D1_EXTEND_MODE extendModeX = D2D1_EXTEND_MODE_CLAMP,
        D2D1_EXTEND_MODE extendModeY = D2D1_EXTEND_MODE_CLAMP,
        D2D1_BITMAP_INTERPOLATION_MODE interpolationMode = D2D1_BITMAP_INTERPOLATION_MODE_LINEAR
        )
    {
        D2D1_BITMAP_BRUSH_PROPERTIES bitmapBrushProperties;

        bitmapBrushProperties.extendModeX = extendModeX;
        bitmapBrushProperties.extendModeY = extendModeY;
        bitmapBrushProperties.interpolationMode = interpolationMode;

        return bitmapBrushProperties;
    }

    D2D1FORCEINLINE
    D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES
    LinearGradientBrushProperties(
        __in CONST D2D1_POINT_2F &startPoint,
        __in CONST D2D1_POINT_2F &endPoint
        )
    {
        D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES linearGradientBrushProperties;

        linearGradientBrushProperties.startPoint = startPoint;
        linearGradientBrushProperties.endPoint = endPoint;

        return linearGradientBrushProperties;
    }

    D2D1FORCEINLINE
    D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES
    RadialGradientBrushProperties(
        __in CONST D2D1_POINT_2F &center,
        __in CONST D2D1_POINT_2F &gradientOriginOffset,
        FLOAT radiusX,
        FLOAT radiusY
        )
    {
        D2D1_RADIAL_GRADIENT_BRUSH_PROPERTIES radialGradientBrushProperties;

        radialGradientBrushProperties.center = center;
        radialGradientBrushProperties.gradientOriginOffset = gradientOriginOffset;
        radialGradientBrushProperties.radiusX = radiusX;
        radialGradientBrushProperties.radiusY = radiusY;

        return radialGradientBrushProperties;
    }
*)

function D2D1PixelFormat(i_dxgiFormat:TDXGI_Format;i_AlphaMode:TD2D1_AlphaMode):TD2D1_PixelFormat;
begin
  Result.Format:=i_dxgiFormat;
  Result.AlphaMode:=i_AlphaMode;
end;

(*
    //
    // Bitmaps
    //
    D2D1FORCEINLINE
    D2D1_BITMAP_PROPERTIES
    BitmapProperties(
        CONST D2D1_PIXEL_FORMAT &pixelFormat = D2D1::PixelFormat(),
        FLOAT dpiX = 96.0f,
        FLOAT dpiY = 96.0f
        )
    {
        D2D1_BITMAP_PROPERTIES bitmapProperties;

        bitmapProperties.pixelFormat = pixelFormat;
        bitmapProperties.dpiX = dpiX;
        bitmapProperties.dpiY = dpiY;

        return bitmapProperties;
    }
*)

function D2D1RenderTargetProperties(i_Type:TD2D1_RenderTargetType;const i_PixelFormat:TD2D1_PixelFormat;i_dpiX:Single = 0;i_dpiY:Single = 0;
  i_Usage:TD2D1_RenderTargetUsage = D2D1_RENDER_TARGET_USAGE_NONE;i_MinLevel:TD2D1_FeatureLevel=D2D1_FEATURE_LEVEL_DEFAULT):TD2D1_RenderTargetProperties;
begin
  Result._Type:=i_Type;
  Result.PixelFormat:=i_PixelFormat;
  Result.DpiX:=i_dpiX;
  Result.DpiY:=i_dpiY;
  Result.Usage:=i_Usage;
  Result.MinLevel:=i_MinLevel;
end;

function D2D1HwndRenderTargetProperties(i_hWnd:HWND;i_PixelSize:TD2D1_SizeU;i_PresentOptions:TD2D1_PresentOptions = D2D1_PRESENT_OPTIONS_NONE):TD2D1_HwndRenderTargetProperties;
begin
  Result.hWnd:=i_hWnd;
  Result.PixelSize:=i_PixelSize;
  Result.PresentOptions:=i_PresentOptions;
end;

(*
    D2D1FORCEINLINE
    D2D1_LAYER_PARAMETERS
    LayerParameters(
        __in CONST D2D1_RECT_F &contentBounds = D2D1::InfiniteRect(),
        __in_opt ID2D1Geometry *geometricMask = NULL,
        D2D1_ANTIALIAS_MODE maskAntialiasMode = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE,
        D2D1_MATRIX_3X2_F maskTransform = D2D1::IdentityMatrix(),
        FLOAT opacity = 1.0,
        __in_opt ID2D1Brush *opacityBrush = NULL,
        D2D1_LAYER_OPTIONS layerOptions = D2D1_LAYER_OPTIONS_NONE
        )
    {
        D2D1_LAYER_PARAMETERS layerParameters = { 0 };

        layerParameters.contentBounds = contentBounds;
        layerParameters.geometricMask = geometricMask;
        layerParameters.maskAntialiasMode = maskAntialiasMode;
        layerParameters.maskTransform = maskTransform;
        layerParameters.opacity = opacity;
        layerParameters.opacityBrush = opacityBrush;
        layerParameters.layerOptions = layerOptions;

        return layerParameters;
    }

    D2D1FORCEINLINE
    D2D1_DRAWING_STATE_DESCRIPTION
    DrawingStateDescription(
        D2D1_ANTIALIAS_MODE antialiasMode = D2D1_ANTIALIAS_MODE_PER_PRIMITIVE,
        D2D1_TEXT_ANTIALIAS_MODE textAntialiasMode = D2D1_TEXT_ANTIALIAS_MODE_DEFAULT,
        D2D1_TAG tag1 = 0,
        D2D1_TAG tag2 = 0,
        __in const D2D1_MATRIX_3X2_F &transform = D2D1::IdentityMatrix()
        )
    {
        D2D1_DRAWING_STATE_DESCRIPTION drawingStateDescription;

        drawingStateDescription.antialiasMode = antialiasMode;
        drawingStateDescription.textAntialiasMode = textAntialiasMode;
        drawingStateDescription.tag1 = tag1;
        drawingStateDescription.tag2 = tag2;
        drawingStateDescription.transform = transform;

        return drawingStateDescription;
    }
*)

const
  sc_RedShift:LongWord=16;
  sc_GreenShift:LongWord=8;
  sc_BlueShift:LongWord=0;
  //
  sc_RedMask:LongWord=$FF shl 16; // sc_RedShift;
  sc_GreenMask:LongWord=$FF shl 8; // sc_GreenShift;
  sc_BlueMask:LongWord=$FF shl 0; // sc_BlueShift;

function D2D1ColorF(r,g,b:Single;a:Single=1.0):TD2D1_ColorF; overload;
begin
  Result.r:=r;
  Result.g:=g;
  Result.b:=b;
  Result.a:=a;
end;

function D2D1ColorF(rgb:LongWord;a:Single=1.0):TD2D1_ColorF; overload;
// Construct a color, note that the alpha value from the "rgb" component is never used.
begin
  Result.r:=((rgb and sc_RedMask) shr sc_RedShift) / 255.0;
  Result.g:=((rgb and sc_GreenMask) shr sc_GreenShift) / 255.0;
  Result.b:=((rgb and sc_BlueMask) shr sc_BlueShift) / 255.0;
  Result.a:=a;
end;

function D2D1ColorF(Color:TD2D1_Color;a:Single=1.0):TD2D1_ColorF; overload;
begin
  Result:=D2D1ColorF(LongWord(Color),a);
end;

function D2D1Matrix3x2F(_11,_12,_21,_22,_31,_32:Single):TD2D1_Matrix3X2F;
begin
  Result._11:=_11;
  Result._12:=_12;
  Result._21:=_21;
  Result._22:=_22;
  Result._31:=_31;
  Result._32:=_32;
end;

function D2D1Matrix3x2F_Translation(const i_X,i_Y:Single):TD2D1_Matrix3X2F; overload;
begin
  Result._11:=1.0; Result._12:=0.0;
  Result._21:=0.0; Result._22:=1.0;
  Result._31:=i_X; Result._32:=i_Y;
end;

function D2D1Matrix3x2F_Translation(const i_Size:TD2D1_SizeF):TD2D1_Matrix3X2F; overload;
begin
  Result:=D2D1Matrix3x2F_Translation(i_Size.Width,i_Size.Height);
end;

(*
    class Matrix3x2F : public D2D1_MATRIX_3X2_F
    {
    public:
        static D2D1FORCEINLINE
        Matrix3x2F
        Scale(
            D2D1_SIZE_F size,
            D2D1_POINT_2F center = D2D1::Point2F()
            )
        {
            Matrix3x2F scale;

            scale._11 = size.width; scale._12 = 0.0;
            scale._21 = 0.0; scale._22 = size.height;
            scale._31 = center.x - size.width * center.x;
            scale._32 = center.y - size.height * center.y;

            return scale;
        }

        static D2D1FORCEINLINE
        Matrix3x2F
        Scale(
            FLOAT x,
            FLOAT y,
            D2D1_POINT_2F center = D2D1::Point2F()
            )
        {
            return Scale(SizeF(x, y), center);
        }

        static D2D1FORCEINLINE
        Matrix3x2F
        Rotation(
            FLOAT angle,
            D2D1_POINT_2F center = D2D1::Point2F()
            )
        {
            Matrix3x2F rotation;

            D2D1MakeRotateMatrix(angle, center, &rotation);

            return rotation;
        }

        static D2D1FORCEINLINE
        Matrix3x2F
        Skew(
            FLOAT angleX,
            FLOAT angleY,
            D2D1_POINT_2F center = D2D1::Point2F()
            )
        {
            Matrix3x2F skew;

            D2D1MakeSkewMatrix(angleX, angleY, center, &skew);

            return skew;
        }

        //
        // Functions for convertion from the base D2D1_MATRIX_3X2_F to this type
        // without making a copy
        //
        static inline const Matrix3x2F* ReinterpretBaseType(const D2D1_MATRIX_3X2_F *pMatrix)
        {
            return static_cast<const Matrix3x2F *>(pMatrix);
        }

        static inline Matrix3x2F* ReinterpretBaseType(D2D1_MATRIX_3X2_F *pMatrix)
        {
            return static_cast<Matrix3x2F *>(pMatrix);
        }

        inline
        FLOAT
        Determinant() const
        {
            return (_11 * _22) - (_12 * _21);
        }

        inline
        bool
        IsInvertible() const
        {
            return !!D2D1IsMatrixInvertible(this);
        }

        inline
        bool
        Invert()
        {
            return !!D2D1InvertMatrix(this);
        }

        inline
        bool
        IsIdentity() const
        {
            return     _11 == 1.f && _12 == 0.f
                    && _21 == 0.f && _22 == 1.f
                    && _31 == 0.f && _32 == 0.f;
        }

        inline
        void SetProduct(
            const Matrix3x2F &a,
            const Matrix3x2F &b
            )
        {
            _11 = a._11 * b._11 + a._12 * b._21;
            _12 = a._11 * b._12 + a._12 * b._22;
            _21 = a._21 * b._11 + a._22 * b._21;
            _22 = a._21 * b._12 + a._22 * b._22;
            _31 = a._31 * b._11 + a._32 * b._21 + b._31;
            _32 = a._31 * b._12 + a._32 * b._22 + b._32;
        }

        D2D1FORCEINLINE
        Matrix3x2F
        operator*(
            const Matrix3x2F &matrix
            ) const
        {
            Matrix3x2F result;

            result.SetProduct(*this, matrix);

            return result;
        }

        D2D1FORCEINLINE
        D2D1_POINT_2F
        TransformPoint(
            D2D1_POINT_2F point
            ) const
        {
            D2D1_POINT_2F result =
            {
                point.x * _11 + point.y * _21 + _31,
                point.x * _12 + point.y * _22 + _32
            };

            return result;
        }
    };

    D2D1FORCEINLINE
    D2D1_POINT_2F
    operator*(
        const D2D1_POINT_2F &point,
        const D2D1_MATRIX_3X2_F &matrix
        )
    {
        return Matrix3x2F::ReinterpretBaseType(&matrix)->TransformPoint(point);
    }

} // namespace D2D1

D2D1FORCEINLINE
D2D1_MATRIX_3X2_F
operator*(
    const D2D1_MATRIX_3X2_F &matrix1,
    const D2D1_MATRIX_3X2_F &matrix2
    )
{
    return
        (*D2D1::Matrix3x2F::ReinterpretBaseType(&matrix1)) *
        (*D2D1::Matrix3x2F::ReinterpretBaseType(&matrix2));
}

*)

///////////////////////////////////////////////////////////////////////////////
// End "D2D1Helper.h"
///////////////////////////////////////////////////////////////////////////////


var
  D2DLibrary: Cardinal = 0;
  DWriteLibrary: Cardinal = 0;

procedure LoadD2D;
const
  DLL_DWrite = 'DWrite.dll';
  DLL_D2D1='d2d1.dll';
begin
  D2DLibrary := LoadLibrary(PChar(DLL_D2D1));
  if D2DLibrary <> 0 then
  begin
    D2D1CreateFactory := GetProcAddress(D2DLibrary, 'D2D1CreateFactory');
  end;
  DWriteLibrary := LoadLibrary(PChar(DLL_DWrite));
  if DWriteLibrary <> 0 then
  begin
    DWriteCreateFactory := GetProcAddress(DWriteLibrary, 'DWriteCreateFactory');
  end
end;

procedure FreeD2D;
begin
  if (D2DLibrary <> 0) then
  begin
    D2D1CreateFactory := nil;
    FreeLibrary(D2DLibrary);
    D2DLibrary := 0;
  end;
  if DWriteLibrary <> 0 then
  begin
    DWriteCreateFactory := nil;
    FreeLibrary(DWriteLibrary);
    DWriteLibrary := 0;
  end
end;

{$IFDEF ENABLE_TYPEINFO}
  {$M+}
{$ENDIF}