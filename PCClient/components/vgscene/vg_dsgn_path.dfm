object vgPathDataDesigner: TvgPathDataDesigner
  Left = 435
  Top = 227
  Width = 682
  Height = 493
  ActiveControl = vgScene1
  Caption = 'Path Designer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object vgScene1: TvgScene
    Left = 0
    Top = 0
    Width = 666
    Height = 455
    Align = alClient
    ActiveControl = PathData
    DesignSnapGridShow = False
    DesignSnapToGrid = False
    DesignSnapToLines = True
    object Root1: TvgBackground
      Width = 666.000000000000000000
      Height = 455.000000000000000000
      HitTest = False
      object labelMemo: TvgLabel
        Align = vaTop
        Position.Point = '(9,0)'
        Width = 474.000000000000000000
        Height = 23.000000000000000000
        Padding.Rect = '(9,0,0,0)'
        TabOrder = 0
        Font.Size = 11.000000953674320000
        TextAlign = vgTextAlignNear
        Text = 'Type path Data (like SVG or XAML):'
      end
      object previewLayout: TvgLayout
        Align = vaMostRight
        Position.Point = '(483,0)'
        Width = 183.000000000000000000
        Height = 455.000000000000000000
        object Label3: TvgLabel
          Align = vaTop
          Position.Point = '(9,0)'
          Width = 174.000000000000000000
          Height = 23.000000000000000000
          Padding.Rect = '(9,0,0,0)'
          TabOrder = 0
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignNear
          Text = 'Preview:'
        end
        object previewPath: TvgPath
          Align = vaTop
          Position.Point = '(0,93)'
          Width = 183.000000000000000000
          Height = 210.000000000000000000
          Padding.Rect = '(0,70,0,0)'
        end
      end
      object Layout2: TvgLayout
        Align = vaBottom
        Position.Point = '(0,411)'
        Width = 483.000000000000000000
        Height = 44.000000000000000000
        object Button2: TvgButton
          Position.Point = '(10,8)'
          Width = 73.000000000000000000
          Height = 26.000000000000000000
          OnClick = Button2Click
          TabOrder = 0
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'OK'
        end
        object Button3: TvgButton
          Position.Point = '(98,8)'
          Width = 78.000000000000000000
          Height = 26.000000000000000000
          OnClick = Button3Click
          TabOrder = 1
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'Cancel'
        end
        object Button1: TvgButton
          Position.Point = '(190,8)'
          Width = 77.000000000000000000
          Height = 26.000000000000000000
          OnClick = Button1Click
          TabOrder = 2
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'Paste'
        end
      end
      object PathData: TvgMemo
        Align = vaClient
        Position.Point = '(9,23)'
        Width = 465.000000000000000000
        Height = 388.000000000000000000
        Padding.Rect = '(9,0,9,0)'
        TabOrder = 3
        DisableFocusEffect = True
        AutoSelect = False
        OnChange = PathDataChange
        WordWrap = False
        Font.Size = 11.000000953674320000
      end
    end
  end
end
