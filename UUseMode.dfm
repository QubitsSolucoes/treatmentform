object fUseMode: TfUseMode
  Left = 0
  Top = 0
  ClientHeight = 322
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 19
    Height = 13
    Caption = 'CPF'
  end
  object Label2: TLabel
    Left = 160
    Top = 13
    Width = 25
    Height = 13
    Caption = 'CNPJ'
  end
  object Label3: TLabel
    Left = 8
    Top = 69
    Width = 54
    Height = 13
    Caption = 'Data V'#225'lida'
  end
  object Label4: TLabel
    Left = 160
    Top = 69
    Width = 54
    Height = 13
    Caption = 'Data V'#225'lida'
  end
  object Label5: TLabel
    Left = 8
    Top = 125
    Width = 64
    Height = 13
    Caption = 'Data Inv'#225'lida'
  end
  object Label6: TLabel
    Left = 160
    Top = 125
    Width = 64
    Height = 13
    Caption = 'Data Inv'#225'lida'
  end
  object Label7: TLabel
    Left = 8
    Top = 221
    Width = 79
    Height = 13
    Caption = 'ComboBox Vazio'
  end
  object lbStatus: TLabel
    Left = 89
    Top = 293
    Width = 31
    Height = 13
    Caption = 'Status'
  end
  object LabeledEdit3: TLabeledEdit
    Left = 8
    Top = 192
    Width = 121
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'Campo Vazio'
    TabOrder = 2
  end
  object LabeledEdit4: TLabeledEdit
    Left = 160
    Top = 192
    Width = 121
    Height = 21
    EditLabel.Width = 80
    EditLabel.Height = 13
    EditLabel.Caption = 'Campo Ignorado'
    TabOrder = 3
    OnExit = LabeledEdit4Exit
  end
  object Button1: TButton
    Left = 8
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Validar'
    TabOrder = 4
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 240
    Width = 273
    Height = 21
    TabOrder = 5
    TextHint = 'ComboBox'
  end
  object MaskEdit1: TMaskEdit
    Left = 8
    Top = 32
    Width = 119
    Height = 21
    EditMask = '999.999.999-99;0;_'
    MaxLength = 14
    TabOrder = 0
    Text = '84888143641'
  end
  object MaskEdit2: TMaskEdit
    Left = 160
    Top = 32
    Width = 118
    Height = 21
    EditMask = '99.999.999/9999-99;0;_'
    MaxLength = 18
    TabOrder = 1
    Text = '70288942000100'
  end
  object MaskEdit3: TMaskEdit
    Left = 8
    Top = 88
    Width = 104
    Height = 21
    EditMask = '99/99/99;1;_'
    MaxLength = 8
    TabOrder = 6
    Text = '24/07/91'
  end
  object MaskEdit4: TMaskEdit
    Left = 160
    Top = 88
    Width = 115
    Height = 21
    EditMask = '99/99/9999;1;_'
    MaxLength = 10
    TabOrder = 7
    Text = '24/07/1991'
  end
  object MaskEdit5: TMaskEdit
    Left = 8
    Top = 144
    Width = 106
    Height = 21
    EditMask = '99/99/99;1;_'
    MaxLength = 8
    TabOrder = 8
    Text = '99/99/99'
  end
  object MaskEdit6: TMaskEdit
    Left = 160
    Top = 144
    Width = 115
    Height = 21
    EditMask = '99/99/9999;1;_'
    MaxLength = 10
    TabOrder = 9
    Text = '99/99/9999'
  end
end
