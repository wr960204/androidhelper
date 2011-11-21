object vgBrushDesign: TvgBrushDesign
  Left = 589
  Top = 218
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'Brush Designer'
  ClientHeight = 461
  ClientWidth = 335
  Color = clBtnFace
  Constraints.MinHeight = 420
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object vgDesigner: TvgScene
    Left = 0
    Top = 0
    Width = 335
    Height = 461
    Align = alClient
    AllowDrag = True
    DesignSnapGridSize = 1.000000000000000000
    Transparency = True
    DesignSnapGridShow = False
    DesignSnapToGrid = False
    DesignSnapToLines = True
    object dsgnRoot: TvgBackground
      Align = vaClient
      Width = 335.000000000000000000
      Height = 461.000000000000000000
      Margins.Rect = '(4,4,4,4)'
      HitTest = False
      object HudWindow1: TvgHudWindow
        Align = vaContents
        Width = 335.000000000000000000
        Height = 461.000000000000000000
        Margins.Rect = '(20,40,20,13)'
        HitTest = False
        Font.Size = 11.000000953674320000
        Font.Style = vgFontBold
        TextAlign = vgTextAlignNear
        Text = 'Brush Designer'
        Fill.Color = '#F5333333'
        object brushList: TvgListBox
          Align = vaTop
          Position.Point = '(20,40)'
          Width = 295.000000000000000000
          Height = 100.000000000000000000
          Padding.Rect = '(0,0,0,4)'
          ClipChildren = True
          CanFocused = True
          Resource = 'hudlistboxstyle'
          DisableMouseWheel = False
          HideSelectionUnfocused = False
          ScrollBarSize = 18.000000000000000000
          ItemIndex = -1
          OnChange = brushListChange
        end
        object brushTabControl: TvgTabControl
          Align = vaTop
          Position.Point = '(20,147)'
          Width = 295.000000000000000000
          Height = 259.000000000000000000
          Padding.Rect = '(0,3,0,0)'
          ClipChildren = True
          CanFocused = True
          Resource = 'hudtabcontrolstyle'
          ItemIndex = 1
          OnChange = brushTabControlChange
          object tabNone: TvgTabItem
            Width = 49.000000000000000000
            Height = 20.000000000000000000
            Resource = 'hudtabitemstyle'
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'None'
          end
          object tabSolid: TvgTabItem
            Position.Point = '(49,0)'
            Width = 44.000000000000000000
            Height = 20.000000000000000000
            Resource = 'hudtabitemstyle'
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Solid'
            Layout = panelSolid
          end
          object tabGradient: TvgTabItem
            Position.Point = '(93,0)'
            Width = 64.000000000000000000
            Height = 20.000000000000000000
            Resource = 'hudtabitemstyle'
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Gradient'
            Layout = panelGradient
          end
          object tabBitmap: TvgTabItem
            Position.Point = '(157,0)'
            Width = 54.000000000000000000
            Height = 20.000000000000000000
            Resource = 'hudtabitemstyle'
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Bitmap'
            Layout = panelBitmap
          end
          object tabRes: TvgTabItem
            Position.Point = '(211,0)'
            Width = 66.000000000000000000
            Height = 20.000000000000000000
            Resource = 'hudtabitemstyle'
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Resource'
            Layout = panerRes
          end
          object panelBitmap: TvgLayout
            Position.Point = '(0,20)'
            Width = 295.000000000000000000
            Height = 239.000000000000000000
            ClipChildren = True
            Visible = False
            DesignHide = True
            object Rectangle1: TvgRectangle
              Align = vaClient
              Position.Point = '(4,36)'
              Width = 287.000000000000000000
              Height = 199.000000000000000000
              Padding.Rect = '(4,2,4,4)'
              ClipChildren = True
              Fill.Style = vgBrushNone
              Stroke.Style = vgBrushNone
              xRadius = 4.000000000000000000
              yRadius = 4.000000000000000000
              CornerType = vgCornerRound
              object bitmapImage: TvgImage
                Align = vaClient
                Position.Point = '(4,4)'
                Width = 279.000000000000000000
                Height = 191.000000000000000000
                Padding.Rect = '(4,4,4,4)'
                ClipChildren = True
                Bitmap.PNG = {
                  89504E470D0A1A0A0000000D49484452000000010000000108060000001F15C4
                  89000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
                  00097048597300000EC300000EC301C76FA8640000000B494441541857636000
                  020000050001AAD5C8510000000049454E44AE426082}
                WrapMode = vgImageFit
              end
            end
            object Layout5: TvgLayout
              Align = vaTop
              Position.Point = '(8,8)'
              Width = 279.000000000000000000
              Height = 26.000000000000000000
              Padding.Rect = '(8,8,8,0)'
              object btnSelectBitmap: TvgButton
                Position.Point = '(0,-1)'
                Width = 76.000000000000000000
                Height = 24.000000000000000000
                CanFocused = True
                OnClick = btnSelectBitmapClick
                Resource = 'hudbuttonstyle'
                IsPressed = False
                StaysPressed = False
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'Select...'
              end
              object Label1: TvgLabel
                Position.Point = '(82,-1)'
                Width = 56.000000000000000000
                Height = 25.000000000000000000
                Padding.Rect = '(7,8,0,0)'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignNear
                Text = 'TileMode'
              end
              object tileModeList: TvgPopupBox
                Position.Point = '(137,-1)'
                Width = 128.000000000000000000
                Height = 23.000000000000000000
                CanFocused = True
                Resource = 'hudpopupboxstyle'
                IsPressed = False
                StaysPressed = False
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'vgWrapTile'
                Items.strings = (
                  'vgWrapTile'
                  'vgWrapTileOriginal'
                  'vgWrapTileStretch')
                ItemIndex = 0
                OnChange = tileModeListChange
              end
            end
          end
          object panelGradient: TvgRectangle
            Position.Point = '(0,20)'
            Width = 295.000000000000000000
            Height = 239.000000000000000000
            Margins.Rect = '(8,8,8,8)'
            ClipChildren = True
            Visible = False
            DesignHide = True
            Fill.Style = vgBrushNone
            Stroke.Style = vgBrushNone
            xRadius = 5.000000000000000000
            yRadius = 5.000000000000000000
            CornerType = vgCornerRound
            object Layout3: TvgLayout
              Align = vaBottom
              Position.Point = '(8,157)'
              Width = 279.000000000000000000
              Height = 24.000000000000000000
              Margins.Rect = '(2,2,2,2)'
              object gradColorRect: TvgColorBox
                Align = vaLeft
                Position.Point = '(2,2)'
                Width = 136.000000000000000000
                Height = 20.000000000000000000
                Fill.Color = '#FF000000'
                Stroke.Style = vgBrushNone
                CornerType = vgCornerRound
              end
            end
            object gradCont: TvgRectangle
              Align = vaRight
              Position.Point = '(193,8)'
              Width = 94.000000000000000000
              Height = 149.000000000000000000
              Margins.Rect = '(4,4,0,0)'
              Fill.Style = vgBrushNone
              Stroke.Style = vgBrushNone
              CornerType = vgCornerRound
              object TvgLabel
                Position.Point = '(7,0)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'R'
              end
              object textGradR: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,0)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textGradRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object textGradG: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,25)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textGradRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object textGradB: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,50)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textGradRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object textGradA: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,77)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textGradRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object TvgLabel
                Position.Point = '(7,25)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'G'
              end
              object TvgLabel
                Position.Point = '(7,50)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'B'
              end
              object gradAlabel: TvgLabel
                Position.Point = '(7,77)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'A'
              end
              object textGradHex: TvgTextBox
                Cursor = crIBeam
                Position.Point = '(7,112)'
                Width = 89.000000000000000000
                Height = 22.000000000000000000
                CanFocused = True
                Resource = 'hudtextboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                ReadOnly = False
                OnChange = textGradHexChange
              end
            end
            object gradQuad: TvgColorQuad
              Align = vaClient
              Position.Point = '(8,8)'
              Width = 162.000000000000000000
              Height = 149.000000000000000000
              Alpha = 1.000000000000000000
              ColorBox = gradColorRect
              OnChange = gradQuadChange
            end
            object gradPicker: TvgColorPicker
              Align = vaRight
              Position.Point = '(171,8)'
              Width = 22.000000000000000000
              Height = 149.000000000000000000
              Padding.Rect = '(1,0,0,0)'
              ColorQuad = gradQuad
            end
            object TvgLayout
              Align = vaBottom
              Position.Point = '(8,181)'
              Width = 279.000000000000000000
              Height = 50.000000000000000000
              Margins.Rect = '(0,6,0,0)'
              object gradEditor: TvgGradientEdit
                Align = vaTop
                Position.Point = '(-8,16)'
                Width = 295.000000000000000000
                Height = 26.000000000000000000
                Padding.Rect = '(-8,10,-8,-4)'
                OnChange = gradEditorChange
                ColorPicker = gradPicker
              end
            end
            object gradKind: TvgPopupBox
              Align = vaTopRight
              Position.Point = '(155,163)'
              Width = 89.000000000000000000
              Height = 22.000000000000000000
              CanFocused = True
              Resource = 'hudpopupboxstyle'
              IsPressed = False
              StaysPressed = False
              Font.Size = 11.000000953674320000
              Font.Style = vgFontRegular
              TextAlign = vgTextAlignCenter
              Text = 'Linear'
              Items.strings = (
                'Linear'
                'Radial')
              ItemIndex = 0
              OnChange = gradKindChange
            end
            object gradAngle: TvgAngleButton
              Align = vaTopRight
              Position.Point = '(248,155)'
              Width = 39.000000000000000000
              Height = 38.000000000000000000
              CanFocused = True
              Resource = 'hudanglebuttonstyle'
              Frequency = 5.000000000000000000
              Value = -90.000000000000000000
              OnChange = gradAngleChange
              object gradAngleLabel: TvgLabel
                Align = vaCenter
                Position.Point = '(3,10)'
                Width = 32.000000000000000000
                Height = 17.000000000000000000
                Resource = 'hudlabelstyle'
                Font.Size = 8.000000000000000000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'Label'
              end
            end
          end
          object panerRes: TvgLayout
            Position.Point = '(0,20)'
            Width = 295.000000000000000000
            Height = 239.000000000000000000
            ClipChildren = True
            Visible = False
            DesignHide = True
            object resList: TvgListBox
              Align = vaClient
              Position.Point = '(8,8)'
              Width = 279.000000000000000000
              Height = 223.000000000000000000
              Padding.Rect = '(8,8,8,8)'
              CanFocused = True
              Resource = 'hudlistboxstyle'
              DisableMouseWheel = False
              HideSelectionUnfocused = False
              ScrollBarSize = 18.000000000000000000
              ItemIndex = -1
              OnChange = resListChange
            end
          end
          object panelSolid: TvgRectangle
            Position.Point = '(0,20)'
            Width = 295.000000000000000000
            Height = 239.000000000000000000
            Margins.Rect = '(8,8,8,8)'
            ClipChildren = True
            Fill.Style = vgBrushNone
            Stroke.Style = vgBrushNone
            xRadius = 5.000000000000000000
            yRadius = 5.000000000000000000
            CornerType = vgCornerRound
            object solidQuad: TvgColorQuad
              Align = vaClient
              Position.Point = '(8,8)'
              Width = 162.000000000000000000
              Height = 149.000000000000000000
              Hue = 0.500000000000000000
              ColorBox = solidColorRect
              OnChange = solidQuadChange
            end
            object solidCont: TvgRectangle
              Align = vaRight
              Position.Point = '(193,8)'
              Width = 94.000000000000000000
              Height = 149.000000000000000000
              Margins.Rect = '(4,4,0,0)'
              Fill.Style = vgBrushNone
              Stroke.Style = vgBrushNone
              CornerType = vgCornerRound
              object ext1: TvgLabel
                Position.Point = '(7,0)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'R'
              end
              object textSolidR: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,0)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textSolidRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object textSolidG: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,25)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textSolidRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object textSolidB: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,50)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textSolidRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object textSolidA: TvgNumberBox
                Cursor = crIBeam
                Position.Point = '(26,77)'
                Width = 70.000000000000000000
                Height = 19.000000000000000000
                CanFocused = True
                Resource = 'hudnumberboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                Text = '0'
                ReadOnly = False
                OnChange = textSolidRChange
                Max = 255.000015258789100000
                ValueType = vgValueInteger
                HorzIncrement = 1.000000000000000000
                VertIncrement = 5.000000000000000000
              end
              object Text1: TvgLabel
                Position.Point = '(7,25)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'G'
              end
              object Text2: TvgLabel
                Position.Point = '(7,50)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'B'
              end
              object Text3: TvgLabel
                Position.Point = '(7,77)'
                Width = 19.000000000000000000
                Height = 19.000000000000000000
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                TextAlign = vgTextAlignCenter
                Text = 'A'
              end
              object textSolidHex: TvgTextBox
                Cursor = crIBeam
                Position.Point = '(7,112)'
                Width = 89.000000000000000000
                Height = 22.000000000000000000
                CanFocused = True
                Resource = 'hudtextboxstyle'
                Font.Size = 11.000000953674320000
                Font.Style = vgFontRegular
                Password = False
                ReadOnly = False
                OnChange = textSolidHexChange
              end
            end
            object Layout1: TvgLayout
              Align = vaBottom
              Position.Point = '(8,157)'
              Width = 279.000000000000000000
              Height = 24.000000000000000000
              Margins.Rect = '(1,1,1,1)'
              object solidColorRect: TvgColorBox
                Align = vaLeft
                Position.Point = '(1,1)'
                Width = 161.000000000000000000
                Height = 22.000000000000000000
                Stroke.Style = vgBrushNone
                CornerType = vgCornerRound
              end
            end
            object solidPicker: TvgColorPicker
              Align = vaRight
              Position.Point = '(171,8)'
              Width = 22.000000000000000000
              Height = 149.000000000000000000
              Padding.Rect = '(1,0,0,0)'
              Hue = 0.500000000000000000
              ColorQuad = solidQuad
            end
            object Layout2: TvgLayout
              Align = vaBottom
              Position.Point = '(8,181)'
              Width = 279.000000000000000000
              Height = 50.000000000000000000
            end
          end
        end
        object Layout6: TvgLayout
          Align = vaTop
          Position.Point = '(20,406)'
          Width = 295.000000000000000000
          Height = 33.000000000000000000
          Margins.Rect = '(5,5,5,5)'
          object btnCancel: TvgButton
            Align = vaRight
            Position.Point = '(229,5)'
            Width = 64.000000000000000000
            Height = 23.000000000000000000
            CanFocused = True
            Visible = False
            OnClick = btnCancelClick
            Resource = 'hudbuttonstyle'
            IsPressed = False
            StaysPressed = False
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'Cancel'
          end
          object btnOK: TvgButton
            Align = vaRight
            Position.Point = '(146,5)'
            Width = 72.000000000000000000
            Height = 23.000000000000000000
            Padding.Rect = '(0,0,8,0)'
            CanFocused = True
            Visible = False
            OnClick = btnOKClick
            Resource = 'hudbuttonstyle'
            IsPressed = False
            StaysPressed = False
            Font.Size = 11.000000953674320000
            Font.Style = vgFontRegular
            TextAlign = vgTextAlignCenter
            Text = 'OK'
          end
          object makeResLayout: TvgLayout
            Align = vaLeft
            Position.Point = '(5,5)'
            Width = 115.000000000000000000
            Height = 23.000000000000000000
            object btnMakeRes: TvgButton
              Align = vaLeft
              Width = 107.999992370605500000
              Height = 23.000000000000000000
              CanFocused = True
              OnClick = btnMakeResClick
              Resource = 'hudbuttonstyle'
              IsPressed = False
              StaysPressed = False
              Font.Size = 11.000000953674320000
              Font.Style = vgFontRegular
              TextAlign = vgTextAlignCenter
              Text = 'Make a Resource'
            end
          end
        end
      end
    end
  end
end
