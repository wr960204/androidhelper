object vgBitmapEditor: TvgBitmapEditor
  Left = 580
  Top = 238
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
    DesignSnapGridSize = 0.999999984306749000
    DesignSnapGridShow = False
    DesignSnapToGrid = False
    DesignSnapToLines = True
    object Root1: TvgBackground
      Width = 583.999966096130000000
      Height = 469.999989188198400000
      HitTest = False
      object Layout1: TvgLayout
        Align = vaRight
        Position.Point = '(468,0)'
        Width = 116.000005738725400000
        Height = 469.999989188198400000
        Margins.Rect = '(9,9,9,9)'
        object Button1: TvgButton
          Align = vaTop
          Position.Point = '(9,9)'
          Width = 98.000007395593420000
          Height = 24.999999607668740000
          Padding.Rect = '(0,0,0,9)'
          CanFocused = True
          OnClick = Button1Click
          IsPressed = False
          StaysPressed = False
          Font.Size = 11.000000514569010000
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'Load...'
        end
        object Button2: TvgButton
          Align = vaTop
          Position.Point = '(9,77)'
          Width = 98.000007395593420000
          Height = 24.999999607668740000
          Padding.Rect = '(0,0,0,9)'
          CanFocused = True
          OnClick = Button2Click
          IsPressed = False
          StaysPressed = False
          Font.Size = 11.000000514569010000
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'Cancel'
        end
        object btnOk: TvgButton
          Align = vaTop
          Position.Point = '(9,43)'
          Width = 98.000007395593420000
          Height = 24.999999607668740000
          Padding.Rect = '(0,0,0,9)'
          CanFocused = True
          OnClick = btnOkClick
          IsPressed = False
          StaysPressed = False
          Font.Size = 11.000000514569010000
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'OK'
        end
        object btnPaste: TvgButton
          Align = vaTop
          Position.Point = '(9,204)'
          Width = 98.000007395593420000
          Height = 26.000000279170250000
          Padding.Rect = '(0,9,0,0)'
          CanFocused = True
          Visible = False
          OnClick = btnPasteClick
          IsPressed = False
          StaysPressed = False
          Font.Size = 11.000000514569010000
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'Paste'
        end
        object editControl: TvgControl
          Align = vaTop
          Enabled = False
          Position.Point = '(9,111)'
          Width = 98.000007395593420000
          Height = 140.999996756459500000
          object labelScale: TvgLabel
            Align = vaTop
            Position.Point = '(0,69)'
            Width = 98.000007395593420000
            Height = 26.000000279170250000
            Font.Size = 11.000000514569010000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignNear
            Text = 'Scale: 100%'
          end
          object trackScale: TvgTrack
            Align = vaTop
            Position.Point = '(0,95)'
            Width = 98.000007395593420000
            Height = 14.999999764601240000
            CanFocused = True
            OnMouseDown = trackScaleMouseDown
            Resource = 'trackbarstyle'
            Min = 0.099999998430674940
            Max = 3.999999937226998000
            Orientation = vgHorizontal
            Value = 0.999999984306749000
            ViewportSize = 0.999999984306749000
            Tracking = True
            OnChange = trackScaleChange
          end
          object Layout2: TvgLayout
            Align = vaTop
            Position.Point = '(0,118)'
            Width = 98.000007395593420000
            Height = 26.999999232684850000
            Padding.Rect = '(0,8,0,0)'
            object btnFit: TvgButton
              Align = vaLeft
              Width = 42.999999668787610000
              Height = 26.999999232684850000
              CanFocused = True
              OnClick = btnFitClick
              IsPressed = False
              StaysPressed = False
              Font.Style = vgFontRegular
              TextAlign = vgTextAlignCenter
              Text = 'Fit'
            end
            object btnOriginal: TvgButton
              Align = vaLeft
              Position.Point = '(52,0)'
              Width = 42.999999668787610000
              Height = 26.999999232684850000
              Padding.Rect = '(9,0,0,0)'
              CanFocused = True
              OnClick = btnOriginalClick
              IsPressed = False
              StaysPressed = False
              Font.Style = vgFontRegular
              TextAlign = vgTextAlignCenter
              Text = '1:1'
            end
          end
          object cropButton: TvgButton
            Align = vaTop
            Position.Point = '(0,35)'
            Width = 98.000007395593420000
            Height = 33.999998779234710000
            Padding.Rect = '(0,9,0,0)'
            CanFocused = True
            OnClick = cropButtonClick
            IsPressed = False
            StaysPressed = False
            Font.Size = 11.000000514569010000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Crop'
            object Image1: TvgImage
              Align = vaLeft
              Position.Point = '(4,4)'
              Width = 24.999999607668740000
              Height = 26.000000279170250000
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
            Width = 98.000007395593420000
            Height = 26.000000279170250000
            CanFocused = True
            OnClick = Button3Click
            IsPressed = False
            StaysPressed = False
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Clear'
          end
          object btnResize: TvgButton
            Align = vaTop
            Position.Point = '(0,153)'
            Width = 98.000007395593420000
            Height = 26.000000279170250000
            Padding.Rect = '(0,8,0,0)'
            CanFocused = True
            OnClick = btnResizeClick
            IsPressed = False
            StaysPressed = False
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Resize...'
          end
        end
      end
      object Rectangle1: TvgPanel
        Align = vaClient
        Position.Point = '(9,9)'
        Width = 450.000010117906400000
        Height = 451.999987409092600000
        Padding.Rect = '(9,9,9,9)'
        object ScrollBox1: TvgScrollBox
          Align = vaClient
          Position.Point = '(4,4)'
          Width = 441.999997873946600000
          Height = 444.000009524871200000
          Padding.Rect = '(4,4,4,4)'
          ClipChildren = True
          DisableMouseWheel = False
          object Preview: TvgPaintBox
            Width = 9.999999843067494000
            Height = 9.999999843067494000
            OnPaint = PreviewPaint
          end
        end
      end
      object resizeLayout: TvgBackground
        Position.Point = '(149,0)'
        Width = 319.000002896592900000
        Height = 175.999994489208800000
        object ShadowEffect1: TvgShadowEffect
          Distance = 2.999999952920248000
          Direction = 45.000001011790640000
          Softness = 0.999999984306749000
          Opacity = 0.599999990584049700
          ShadowColor = '#FF000000'
        end
        object GroupBox1: TvgGroupBox
          Position.Point = '(13,12)'
          Width = 172.000005547098100000
          Height = 116.999994384318500000
          Margins.Rect = '(16,26,56,4)'
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'Resize Bitmap:'
          object newWidth: TvgNumberBox
            Align = vaTop
            Cursor = crIBeam
            Position.Point = '(16,41)'
            Width = 99.999998430674940000
            Height = 21.000000357636510000
            CanFocused = True
            Font.Style = vgFontRegular
            Password = False
            Text = '1'
            ReadOnly = False
            OnChange = newWidthChange
            Min = 0.999999984306749000
            Max = 3999.999937226998000000
            Value = 0.999999984306749000
            ValueType = vgValueInteger
            HorzIncrement = 0.999999984306749000
            VertIncrement = 4.999999921533747000
          end
          object Label1: TvgLabel
            Align = vaTop
            Position.Point = '(16,26)'
            Width = 99.999998430674940000
            Height = 14.999999764601240000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignNear
            Text = 'Width:'
          end
          object Label2: TvgLabel
            Align = vaTop
            Position.Point = '(16,69)'
            Width = 99.999998430674940000
            Height = 14.999999764601240000
            Padding.Rect = '(0,7,0,0)'
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignNear
            Text = 'Height:'
          end
          object newHeight: TvgNumberBox
            Align = vaTop
            Cursor = crIBeam
            Position.Point = '(16,84)'
            Width = 99.999998430674940000
            Height = 21.000000357636510000
            CanFocused = True
            Font.Style = vgFontRegular
            Password = False
            Text = '1'
            ReadOnly = False
            OnChange = newHeightChange
            Min = 0.999999984306749000
            Max = 3999.999937226998000000
            Value = 0.999999984306749000
            ValueType = vgValueInteger
            HorzIncrement = 0.999999984306749000
            VertIncrement = 4.999999921533747000
          end
        end
        object Button4: TvgButton
          Position.Point = '(87,139)'
          Width = 71.000001290960900000
          Height = 26.000000279170250000
          CanFocused = True
          OnClick = Button4Click
          IsPressed = False
          StaysPressed = False
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'OK'
        end
        object Button5: TvgButton
          Position.Point = '(166,139)'
          Width = 78.999999791025360000
          Height = 26.000000279170250000
          CanFocused = True
          OnClick = Button5Click
          IsPressed = False
          StaysPressed = False
          Font.Style = vgFontRegular
          TextAlign = vgTextAlignCenter
          Text = 'Cancel'
        end
      end
    end
  end
end
