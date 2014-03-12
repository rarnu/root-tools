object vgLangDesigner: TvgLangDesigner
  Left = 505
  Top = 155
  ActiveControl = vgScene1
  BorderStyle = bsNone
  Caption = 'VGScene Lang Designer'
  ClientHeight = 488
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object vgScene1: TvgScene
    Left = 0
    Top = 0
    Width = 586
    Height = 488
    Align = alClient
    Transparency = True
    Style = vgResources1
    DesignSnapGridShow = False
    DesignSnapToGrid = False
    DesignSnapToLines = False
    object Root1: TvgHudWindow
      Width = 586.000000000000000000
      Height = 488.000000000000000000
      Margins.Rect = '(13,35,13,13)'
      HitTest = False
      AutoTranslate = False
      Font.Style = vgFontBold
      TextAlign = vgTextAlignNear
      Text = 'Language Designer'
      object btnAddItem: TvgHudCornerButton
        Align = vaTopRight
        Position.Point = '(537,88)'
        Width = 29.000000000000000000
        Height = 21.000000000000000000
        OnClick = btnAddItemClick
        TabOrder = 1
        AutoTranslate = False
        StaysPressed = False
        IsPressed = False
        Font.Size = 18.000000000000000000
        Font.Style = vgFontBold
        TextAlign = vgTextAlignCenter
        Text = '+'
        xRadius = 7.000000000000000000
        yRadius = 7.000000000000000000
        Corners = [vgCornerTopRight, vgCornerBottomRight]
        Sides = [vgSideTop, vgSideLeft, vgSideBottom, vgSideRight]
      end
      object btnRemoveItem: TvgHudCornerButton
        Align = vaTopRight
        Position.Point = '(537,112)'
        Width = 29.000000000000000000
        Height = 21.000000000000000000
        OnClick = btnRemoveItemClick
        TabOrder = 2
        AutoTranslate = False
        StaysPressed = False
        IsPressed = False
        Font.Size = 18.000000000000000000
        Font.Style = vgFontBold
        TextAlign = vgTextAlignCenter
        Text = '-'
        xRadius = 7.000000000000000000
        yRadius = 7.000000000000000000
        Corners = [vgCornerTopRight, vgCornerBottomRight]
        Sides = [vgSideTop, vgSideLeft, vgSideBottom, vgSideRight]
      end
      object ToolBar1: TvgToolBar
        Position.Point = '(13,35)'
        Width = 560.000000000000000000
        Height = 32.000000000000000000
        Margins.Rect = '(4,4,4,4)'
        TabOrder = 3
        object layoutSelect: TvgLayout
          Align = vaClient
          Position.Point = '(4,4)'
          Width = 552.000000000000000000
          Height = 24.000000000000000000
          DesignHide = True
          object langList: TvgHudPopupBox
            Align = vaLeft
            Position.Point = '(140,0)'
            Width = 99.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(10,0,0,0)'
            TabOrder = 0
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            ItemIndex = -1
            OnChange = langListChange
          end
          object HudLabel2: TvgHudLabel
            Align = vaLeft
            Width = 130.000000000000000000
            Height = 24.000000000000000000
            TabOrder = 1
            AutoTranslate = False
            TextAlign = vgTextAlignFar
            Text = 'Select Language:'
          end
          object btnAddNewLang: TvgHudButton
            Align = vaLeft
            Position.Point = '(259,0)'
            Width = 123.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(20,0,0,0)'
            OnClick = btnAddNewLangClick
            TabOrder = 2
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            Text = 'Add Language...'
          end
        end
        object layoutAdd: TvgLayout
          Align = vaClient
          Position.Point = '(4,4)'
          Width = 552.000000000000000000
          Height = 24.000000000000000000
          object btnAddLang: TvgHudButton
            Align = vaLeft
            Position.Point = '(257,0)'
            Width = 57.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(10,0,0,0)'
            OnClick = btnAddLangClick
            TabOrder = 0
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            Text = 'Add'
          end
          object inputLang: TvgHudTextBox
            Align = vaLeft
            Position.Point = '(147,0)'
            Width = 100.000000000000000000
            Height = 24.000000000000000000
            TabOrder = 1
            ReadOnly = False
          end
          object HudLabel1: TvgHudLabel
            Align = vaLeft
            Width = 147.000000000000000000
            Height = 24.000000000000000000
            TabOrder = 2
            AutoTranslate = False
            TextAlign = vgTextAlignFar
            Text = 'Two letter of language:'
            WordWrap = False
          end
          object btnCancalAdd: TvgHudButton
            Align = vaLeft
            Position.Point = '(400,0)'
            Width = 29.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(10,0,0,0)'
            Visible = False
            OnClick = btnCancalAddClick
            TabOrder = 3
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            Font.Size = 14.000000000000000000
            Font.Style = vgFontBold
            TextAlign = vgTextAlignCenter
            Text = 'x'
          end
          object btnLoadTxt: TvgHudButton
            Align = vaLeft
            Position.Point = '(324,0)'
            Width = 104.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(10,0,0,0)'
            OnClick = btnLoadTxtClick
            TabOrder = 4
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            Text = 'From txt-file...'
          end
        end
        object layoutAddText: TvgLayout
          Align = vaClient
          Position.Point = '(4,4)'
          Width = 552.000000000000000000
          Height = 24.000000000000000000
          Visible = False
          DesignHide = True
          object btnAddText: TvgHudButton
            Align = vaRight
            Position.Point = '(456,0)'
            Width = 57.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(10,0,0,0)'
            OnClick = btnAddTextClick
            TabOrder = 0
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            Text = 'Add'
          end
          object inputAddText: TvgHudTextBox
            Align = vaClient
            Position.Point = '(120,0)'
            Width = 326.000000000000000000
            Height = 24.000000000000000000
            TabOrder = 1
            ReadOnly = False
          end
          object TvgHudLabel
            Align = vaLeft
            Width = 120.000000000000000000
            Height = 24.000000000000000000
            TabOrder = 2
            AutoTranslate = False
            TextAlign = vgTextAlignFar
            Text = 'New original text:'
            WordWrap = False
          end
          object btnCancelAddText: TvgHudButton
            Align = vaRight
            Position.Point = '(523,0)'
            Width = 29.000000000000000000
            Height = 24.000000000000000000
            Padding.Rect = '(10,0,0,0)'
            OnClick = btnCancelAddTextClick
            TabOrder = 3
            AutoTranslate = False
            StaysPressed = False
            IsPressed = False
            Font.Size = 14.000000000000000000
            Font.Style = vgFontBold
            TextAlign = vgTextAlignCenter
            Text = 'x'
          end
        end
      end
      object btnCollect: TvgHudCornerButton
        Align = vaBottomLeft
        Position.Point = '(27,444)'
        Width = 105.000000000000000000
        Height = 21.000000000000000000
        OnClick = btnCollectClick
        TabOrder = 5
        AutoTranslate = False
        StaysPressed = False
        IsPressed = False
        TextAlign = vgTextAlignCenter
        Text = 'Scan for strings'
        xRadius = 7.000000000000000000
        yRadius = 7.000000000000000000
        Corners = [vgCornerBottomLeft, vgCornerBottomRight]
        Sides = [vgSideTop, vgSideLeft, vgSideBottom, vgSideRight]
      end
      object OriginalList: TvgHudListBox
        Align = vaClient
        Position.Point = '(18,72)'
        Width = 520.000000000000000000
        Height = 373.000000000000000000
        Padding.Rect = '(5,5,35,30)'
        TabOrder = 0
        HideSelectionUnfocused = False
        ItemIndex = -1
      end
      object btnCreateTemplate: TvgHudCornerButton
        Align = vaBottomRight
        Position.Point = '(141,444)'
        Width = 158.000000000000000000
        Height = 21.000000000000000000
        OnClick = btnCreateTemplateClick
        TabOrder = 4
        AutoTranslate = False
        StaysPressed = False
        IsPressed = False
        TextAlign = vgTextAlignCenter
        Text = 'Create txt-file template....'
        xRadius = 7.000000000000000000
        yRadius = 7.000000000000000000
        Corners = [vgCornerBottomLeft, vgCornerBottomRight]
        Sides = [vgSideTop, vgSideLeft, vgSideBottom, vgSideRight]
      end
      object btnLoadLng: TvgHudCornerButton
        Align = vaBottomRight
        Position.Point = '(305,444)'
        Width = 107.000000000000000000
        Height = 21.000000000000000000
        OnClick = btnLoadLngClick
        TabOrder = 7
        AutoTranslate = False
        StaysPressed = False
        IsPressed = False
        TextAlign = vgTextAlignCenter
        Text = 'Load lng-file...'
        xRadius = 7.000000000000000000
        yRadius = 7.000000000000000000
        Corners = [vgCornerBottomLeft, vgCornerBottomRight]
        Sides = [vgSideTop, vgSideLeft, vgSideBottom, vgSideRight]
      end
      object btnSaveLng: TvgHudCornerButton
        Align = vaBottomRight
        Position.Point = '(413,444)'
        Width = 107.000000000000000000
        Height = 21.000000000000000000
        OnClick = btnSaveLngClick
        TabOrder = 6
        AutoTranslate = False
        StaysPressed = False
        IsPressed = False
        TextAlign = vgTextAlignCenter
        Text = 'Save lng-file...'
        xRadius = 7.000000000000000000
        yRadius = 7.000000000000000000
        Corners = [vgCornerBottomLeft, vgCornerBottomRight]
        Sides = [vgSideTop, vgSideLeft, vgSideBottom, vgSideRight]
      end
    end
  end
  object vgResources1: TvgResources
    Left = 288
    Top = 312
    ResourcesBin = {
      545046300C54766752656374616E676C65000557696474680500000000000000
      8908400648656967687405000000000000C08808401646696C6C2E4269746D61
      702E4269746D61702E504E470A9B070000FFD8FFE000104A4649460001010100
      6000600000FFDB00430001010101010101010101010101010101010101010101
      0101010101010101010101010101010101010101010101010101010101010101
      01010101010101010101FFDB0043010101010101010101010101010101010101
      0101010101010101010101010101010101010101010101010101010101010101
      010101010101010101010101010101FFC000110800C001400301220002110103
      1101FFC4001F0000010501010101010100000000000000000102030405060708
      090A0BFFC400B5100002010303020403050504040000017D0102030004110512
      2131410613516107227114328191A1082342B1C11552D1F02433627282090A16
      1718191A25262728292A3435363738393A434445464748494A53545556575859
      5A636465666768696A737475767778797A838485868788898A92939495969798
      999AA2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4
      D5D6D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F0100
      030101010101010101010000000000000102030405060708090A0BFFC400B511
      0002010204040304070504040001027700010203110405213106124151076171
      1322328108144291A1B1C109233352F0156272D10A162434E125F11718191A26
      2728292A35363738393A434445464748494A535455565758595A636465666768
      696A737475767778797A82838485868788898A92939495969798999AA2A3A4A5
      A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DA
      E2E3E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00
      FE5DEB3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8
      AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3
      E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002
      B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A0
      02B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0
      A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8A
      D0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E
      8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B
      3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A00
      2B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A
      002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD
      0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8
      AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3
      E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002
      B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A0
      02B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0
      A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8A
      D0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E
      8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B
      3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A00
      2B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A
      002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD
      0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8
      AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3
      E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002
      B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A0
      02B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0
      A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8A
      D0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E
      8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B
      3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A00
      2B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A
      002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD
      0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8
      AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3
      E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002
      B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A0
      02B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0
      A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8A
      D0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E
      8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B3E8AD0A002B
      3E8AD0A002B3E8AD0A00FFD91446696C6C2E4269746D61702E577261704D6F64
      65070A76675772617054696C650A46696C6C2E5374796C65070D766742727573
      684269746D61700C5374726F6B652E5374796C65070B766742727573684E6F6E
      650553696465730B09766753696465546F700A7667536964654C6566740C7667
      53696465426F74746F6D0B76675369646552696768740000095476674C61796F
      7574000C5265736F757263654E616D6506086C616E676974656D0E506F736974
      696F6E2E506F696E74060728392C323536290557696474680500000000000080
      8408400648656967687405000000000000008804400C4D617267696E732E5265
      6374060928332C332C332C3329000A54766754657874426F78000B42696E6469
      6E674E616D6506097472616E736C61746505416C69676E07087661436C69656E
      740E506F736974696F6E2E506F696E740607283232312C332905576964746805
      00000000000000990740064865696768740500000000000000E003400C506164
      64696E672E52656374060928362C302C302C3029085265736F75726365060B6C
      616E6774657874626F78085461624F72646572020008526561644F6E6C790808
      50617373776F72640800000754766754657874000C5265736F757263654E616D
      6506047465787405416C69676E070676614C6566740E506F736974696F6E2E50
      6F696E74060528372C34290557696474680500000000000000CC064006486569
      6768740500000000000000D003400C50616464696E672E52656374060928342C
      312C342C31290748697454657374080A46696C6C2E436F6C6F72060923464645
      45454545450D486F727A54657874416C69676E070F766754657874416C69676E
      4E6561720454657874060454657874000000095476674C61796F7574000C5265
      736F757263654E616D65060B6C616E6774657874626F780E506F736974696F6E
      2E506F696E740609283232382C323631290557696474680500000000000000B6
      0540064865696768740500000000000000C00340000C54766752656374616E67
      6C65000C5265736F757263654E616D65060A6261636B67726F756E6405416C69
      676E070A7661436F6E74656E7473064C6F636B65640905576964746805000000
      00000000B60540064865696768740500000000000000C0034007486974546573
      74080A46696C6C2E436F6C6F7206092338444646464646460C5374726F6B652E
      5374796C65070B766742727573684E6F6E650778526164697573050000000000
      0000C0014007795261646975730500000000000000C001400553696465730B09
      766753696465546F700A7667536964654C6566740C766753696465426F74746F
      6D0B7667536964655269676874000011547667436F6C6F72416E696D6174696F
      6E00084475726174696F6E050000000000CDCCCCFC3F0C486964654F6E46696E
      697368080754726967676572060E4973466F63757365643D747275650A537461
      727456616C756506092338444646464646460953746F7056616C756506092341
      334646464646460C50726F70657274794E616D65060A46696C6C2E436F6C6F72
      000011547667436F6C6F72416E696D6174696F6E00084475726174696F6E0500
      00000000CDCCCCFC3F0C486964654F6E46696E69736808075472696767657206
      0F4973466F63757365643D66616C73650A537461727456616C75650609234133
      4646464646460953746F7056616C756506092338444646464646460C50726F70
      657274794E616D65060A46696C6C2E436F6C6F720000000A547667436F6E7465
      6E74000C5265736F757263654E616D650607636F6E74656E7405416C69676E07
      0A7661436F6E74656E74730E506F736974696F6E2E506F696E74060528362C32
      29064C6F636B65640905576964746805000000000000009E0540064865696768
      740500000000000000A003400C50616464696E672E52656374060928362C322C
      362C322907486974546573740800000E54766742727573684F626A656374000C
      5265736F757263654E616D65060A666F726567726F756E640B42727573682E43
      6F6C6F72060923464630323032303200000000}
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files|*.txt'
    Left = 336
    Top = 352
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files|*.txt'
    Left = 368
    Top = 352
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'lng'
    Filter = 'Lang file|*.lng'
    Left = 336
    Top = 392
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'lng'
    Filter = 'Lang file|*.lng'
    Left = 368
    Top = 392
  end
end
