object frmDebugUnitView: TfrmDebugUnitView
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmDebugUnitView'
  ClientHeight = 391
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edSource: TSynEdit
    Left = 0
    Top = 0
    Width = 651
    Height = 391
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    BookMarkOptions.Xoffset = 8
    BorderStyle = bsNone
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn1
    ReadOnly = True
    OnGutterClick = edSourceGutterClick
    FontSmoothing = fsmNone
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    NumberAttri.Foreground = clNavy
    Left = 424
    Top = 264
  end
end
