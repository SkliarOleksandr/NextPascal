object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'CRVM GUI Test'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Run VM bin '
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 242
      Top = 10
      Width = 105
      Height = 25
      Caption = 'Run VM 1000 times'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 353
      Top = 10
      Width = 137
      Height = 25
      Caption = 'test StretchUInt'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 496
      Top = 10
      Width = 129
      Height = 25
      Caption = 'Test MPC script'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 97
      Top = 10
      Width = 96
      Height = 25
      Caption = 'Run VM Method'
      TabOrder = 4
      OnClick = Button5Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 296
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 208
    Top = 56
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    Left = 496
    Top = 72
  end
end
