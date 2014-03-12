object vgBitmapEditor: TvgBitmapEditor
  Left = 564
  Top = 228
  Width = 600
  Height = 508
  Caption = 'Bitmap Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object vgScene1: TvgScene
    Left = 0
    Top = 0
    Width = 584
    Height = 470
    Align = alClient
    DesignSnapGridShow = False
    DesignSnapToGrid = False
    DesignSnapToLines = True
    object Root1: TvgBackground
      Width = 584.000000000000000000
      Height = 470.000000000000000000
      HitTest = False
      object Layout1: TvgLayout
        Align = vaRight
        Position.Point = '(468,0)'
        Width = 116.000007629394500000
        Height = 470.000000000000000000
        Margins.Rect = '(9,9,9,9)'
        object Button1: TvgButton
          Align = vaTop
          Position.Point = '(9,9)'
          Width = 98.000007629394530000
          Height = 25.000000000000000000
          Padding.Rect = '(0,0,0,9)'
          OnClick = Button1Click
          TabOrder = 0
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'Load...'
        end
        object Button2: TvgButton
          Align = vaTop
          Position.Point = '(9,111)'
          Width = 98.000007629394530000
          Height = 25.000000000000000000
          Padding.Rect = '(0,0,0,9)'
          OnClick = Button2Click
          TabOrder = 1
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'Cancel'
        end
        object btnOk: TvgButton
          Align = vaTop
          Position.Point = '(9,77)'
          Width = 98.000007629394530000
          Height = 25.000000000000000000
          Padding.Rect = '(0,0,0,9)'
          OnClick = btnOkClick
          TabOrder = 2
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'OK'
        end
        object btnPaste: TvgButton
          Align = vaTop
          Position.Point = '(9,204)'
          Width = 98.000007629394530000
          Height = 26.000000000000000000
          Padding.Rect = '(0,9,0,0)'
          Visible = False
          OnClick = btnPasteClick
          TabOrder = 3
          StaysPressed = False
          IsPressed = False
          Font.Size = 11.000000953674320000
          TextAlign = vgTextAlignCenter
          Text = 'Paste'
        end
        object editControl: TvgControl
          Align = vaTop
          Enabled = False
          Position.Point = '(9,145)'
          Width = 98.000007629394530000
          Height = 141.000000000000000000
          TabOrder = 4
          object labelScale: TvgLabel
            Align = vaTop
            Position.Point = '(0,69)'
            Width = 98.000007629394530000
            Height = 26.000000000000000000
            TabOrder = 0
            Font.Size = 11.000000953674320000
            TextAlign = vgTextAlignNear
            Text = 'Scale: 100%'
          end
          object trackScale: TvgTrack
            Align = vaTop
            Position.Point = '(0,95)'
            Width = 98.000007629394530000
            Height = 15.000000000000000000
            OnMouseDown = trackScaleMouseDown
            Resource = 'trackbarstyle'
            TabOrder = 1
            Min = 0.100000001490116100
            Max = 4.000000000000000000
            Orientation = vgHorizontal
            Value = 1.000000000000000000
            ViewportSize = 1.000000000000000000
            Tracking = True
            OnChange = trackScaleChange
          end
          object Layout2: TvgLayout
            Align = vaTop
            Position.Point = '(0,118)'
            Width = 98.000007629394530000
            Height = 27.000000000000000000
            Padding.Rect = '(0,8,0,0)'
            object btnFit: TvgButton
              Align = vaLeft
              Width = 43.000000000000000000
              Height = 27.000000000000000000
              OnClick = btnFitClick
              TabOrder = 0
              StaysPressed = False
              IsPressed = False
              TextAlign = vgTextAlignCenter
              Text = 'Fit'
            end
            object btnOriginal: TvgButton
              Align = vaLeft
              Position.Point = '(52,0)'
              Width = 43.000000000000000000
              Height = 27.000000000000000000
              Padding.Rect = '(9,0,0,0)'
              OnClick = btnOriginalClick
              TabOrder = 1
              StaysPressed = False
              IsPressed = False
              TextAlign = vgTextAlignCenter
              Text = '1:1'
            end
          end
          object cropButton: TvgButton
            Align = vaTop
            Position.Point = '(0,35)'
            Width = 98.000007629394530000
            Height = 34.000000000000000000
            Padding.Rect = '(0,9,0,0)'
            OnClick = cropButtonClick
            TabOrder = 3
            StaysPressed = False
            IsPressed = False
            Font.Size = 11.000000953674320000
            TextAlign = vgTextAlignCenter
            Text = 'Crop'
            object Image1: TvgImage
              Align = vaLeft
              Position.Point = '(4,4)'
              Width = 25.000000000000000000
              Height = 26.000000000000000000
              Padding.Rect = '(4,4,4,4)'
              ClipChildren = True
              HitTest = False
              Bitmap.PNG = {
                89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
                87000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
                00097048597300000EC300000EC301C76FA8640000011A494441546843ED98CB
                0DC230104473A20C4E9C2CE1C4B14401944101F44401744019344125600B2221
                94B0789995219A487BB23DF6BC899D4FD3F0220112501158A7ABEFFBE89C5BA8
                046A0FF2DE5FBAAEBBC61897B5D7A29A9F0654D88083980010A64A8A09A8B015
                0C4A47E4A96DDBF354E523D4AA0A9639DD75B845AC16F94E17626078D2E6A7ED
                58591A831890449E0D487D3F6947EB8973A22744EBD18048E0A50313A84D8C09
                30815202DCC43F468C9BB834103431B49EE8073D215A8F0644023C464B111913
                ABBA07D09F97E947C27EAA4208AB2FD9DF87A3175DA0B7FB0703876468B45232
                1B8801B448FA8D737CA480218C5EA0A447031221EB7626604D58D267021221EB
                F63924B0CDAF0FB057056BE2D49F0B811B7DA5F45310A0C93B0000000049454E
                44AE426082}
              WrapMode = vgImageFit
            end
          end
          object Button3: TvgButton
            Align = vaTop
            Width = 98.000007629394530000
            Height = 26.000000000000000000
            OnClick = Button3Click
            TabOrder = 4
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            Text = 'Clear'
          end
          object btnResize: TvgButton
            Align = vaTop
            Position.Point = '(0,153)'
            Width = 98.000007629394530000
            Height = 26.000000000000000000
            Padding.Rect = '(0,8,0,0)'
            OnClick = btnResizeClick
            TabOrder = 5
            StaysPressed = False
            IsPressed = False
            TextAlign = vgTextAlignCenter
            Text = 'Resize...'
          end
        end
        object btnSave: TvgButton
          Align = vaTop
          Position.Point = '(9,43)'
          Width = 98.000007629394530000
          Height = 25.000000000000000000
          Padding.Rect = '(0,0,0,9)'
          OnClick = btnSaveClick
          TabOrder = 5
          StaysPressed = False
          IsPressed = False
          TextAlign = vgTextAlignCenter
          Text = 'Save...'
        end
      end
      object Rectangle1: TvgPanel
        Align = vaClient
        Position.Point = '(9,9)'
        Width = 450.000000000000000000
        Height = 452.000000000000000000
        Padding.Rect = '(9,9,9,9)'
        TabOrder = 1
        object ScrollBox1: TvgScrollBox
          Align = vaClient
          Position.Point = '(4,4)'
          Width = 442.000000000000000000
          Height = 444.000000000000000000
          Padding.Rect = '(4,4,4,4)'
          ClipChildren = True
          TabOrder = 0
          object Preview: TvgPaintBox
            Width = 10.000000000000000000
            Height = 10.000000000000000000
            OnPaint = PreviewPaint
          end
        end
      end
      object resizeLayout: TvgBackground
        Position.Point = '(149,0)'
        Width = 319.000000000000000000
        Height = 176.000000000000000000
        TabOrder = 2
        object ShadowEffect1: TvgShadowEffect
          Distance = 3.000000000000000000
          Direction = 45.000000000000000000
          Softness = 1.000000000000000000
          Opacity = 0.599999964237213100
          ShadowColor = '#FF000000'
        end
        object GroupBox1: TvgGroupBox
          Position.Point = '(13,12)'
          Width = 172.000000000000000000
          Height = 116.999992370605500000
          Margins.Rect = '(16,26,56,4)'
          TabOrder = 0
          TextAlign = vgTextAlignCenter
          Text = 'Resize Bitmap:'
          object newWidth: TvgNumberBox
            Align = vaTop
            Position.Point = '(16,41)'
            Width = 100.000000000000000000
            Height = 21.000000000000000000
            TabOrder = 0
            ReadOnly = False
            OnChange = newWidthChange
            Min = 1.000000000000000000
            Max = 4000.000000000000000000
            Value = 1.000000000000000000
            ValueType = vgValueInteger
            HorzIncrement = 1.000000000000000000
            VertIncrement = 5.000000000000000000
          end
          object Label1: TvgLabel
            Align = vaTop
            Position.Point = '(16,26)'
            Width = 100.000000000000000000
            Height = 15.000000000000000000
            TabOrder = 1
            TextAlign = vgTextAlignNear
            Text = 'Width:'
          end
          object Label2: TvgLabel
            Align = vaTop
            Position.Point = '(16,69)'
            Width = 100.000000000000000000
            Height = 15.000000000000000000
            Padding.Rect = '(0,7,0,0)'
            TabOrder = 2
            TextAlign = vgTextAlignNear
            Text = 'Height:'
          end
          object newHeight: TvgNumberBox
            Align = vaTop
            Position.Point = '(16,84)'
            Width = 100.000000000000000000
            Height = 21.000000000000000000
            TabOrder = 3
            ReadOnly = False
            OnChange = newHeightChange
            Min = 1.000000000000000000
            Max = 4000.000000000000000000
            Value = 1.000000000000000000
            ValueType = vgValueInteger
            HorzIncrement = 1.000000000000000000
            VertIncrement = 5.000000000000000000
          end
        end
        object Button4: TvgButton
          Position.Point = '(87,139)'
          Width = 71.000000000000000000
          Height = 26.000000000000000000
          OnClick = Button4Click
          TabOrder = 1
          StaysPressed = False
          IsPressed = False
          TextAlign = vgTextAlignCenter
          Text = 'OK'
        end
        object Button5: TvgButton
          Position.Point = '(166,139)'
          Width = 79.000000000000000000
          Height = 26.000000000000000000
          OnClick = Button5Click
          TabOrder = 2
          StaysPressed = False
          IsPressed = False
          TextAlign = vgTextAlignCenter
          Text = 'Cancel'
        end
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'png'
    Filter = 'PNG Image|*.png|JPEG Image|*.jpeg|BMP Image|*.bmp'
    Left = 512
    Top = 400
  end
end
