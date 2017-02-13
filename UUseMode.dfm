object fUseMode: TfUseMode
  Left = 0
  Top = 0
  ClientHeight = 408
  ClientWidth = 887
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label8: TLabel
    Left = 401
    Top = 13
    Width = 63
    Height = 13
    Caption = 'Cor da Zebra'
  end
  object GroupBox1: TGroupBox
    Left = 121
    Top = 0
    Width = 265
    Height = 408
    Align = alLeft
    Caption = 'Formul'#225'rio de Valida'#231#227'o'
    TabOrder = 3
    object Label1: TLabel
      Left = 10
      Top = 162
      Width = 19
      Height = 13
      Caption = 'CPF'
    end
    object Label2: TLabel
      Left = 149
      Top = 162
      Width = 25
      Height = 13
      Caption = 'CNPJ'
    end
    object Label3: TLabel
      Left = 10
      Top = 206
      Width = 54
      Height = 13
      Caption = 'Data V'#225'lida'
    end
    object Label4: TLabel
      Left = 149
      Top = 206
      Width = 54
      Height = 13
      Caption = 'Data V'#225'lida'
    end
    object Label5: TLabel
      Left = 10
      Top = 248
      Width = 64
      Height = 13
      Caption = 'Data Inv'#225'lida'
    end
    object Label6: TLabel
      Left = 149
      Top = 248
      Width = 64
      Height = 13
      Caption = 'Data Inv'#225'lida'
    end
    object Label7: TLabel
      Left = 10
      Top = 330
      Width = 79
      Height = 13
      Caption = 'ComboBox Vazio'
    end
    object lbStatus: TLabel
      Left = 89
      Top = 381
      Width = 31
      Height = 13
      Caption = 'Status'
    end
    object Label9: TLabel
      Left = 10
      Top = 133
      Width = 63
      Height = 13
      Caption = 'Cor da Borda'
    end
    object LabeledEdit3: TLabeledEdit
      Left = 10
      Top = 306
      Width = 109
      Height = 21
      EditLabel.Width = 61
      EditLabel.Height = 13
      EditLabel.Caption = 'Campo Vazio'
      TabOrder = 0
    end
    object LabeledEdit4: TLabeledEdit
      Left = 149
      Top = 306
      Width = 110
      Height = 21
      EditLabel.Width = 80
      EditLabel.Height = 13
      EditLabel.Caption = 'Campo Ignorado'
      TabOrder = 1
    end
    object Button1: TButton
      Left = 8
      Top = 376
      Width = 63
      Height = 25
      Caption = 'Validar'
      TabOrder = 2
      OnClick = Button1Click
    end
    object ComboBox1: TComboBox
      Left = 10
      Top = 349
      Width = 249
      Height = 21
      TabOrder = 3
      TextHint = 'ComboBox'
      Items.Strings = (
        'Todos'
        'Teste1'
        'Teste2'
        'Teste3')
    end
    object MaskEdit1: TMaskEdit
      Left = 10
      Top = 181
      Width = 109
      Height = 21
      EditMask = '999.999.999-99;0;_'
      MaxLength = 14
      TabOrder = 4
      Text = '84888143641'
    end
    object MaskEdit2: TMaskEdit
      Left = 149
      Top = 181
      Width = 110
      Height = 21
      EditMask = '99.999.999/9999-99;0;_'
      MaxLength = 18
      TabOrder = 5
      Text = '70288942000100'
    end
    object MaskEdit3: TMaskEdit
      Left = 10
      Top = 225
      Width = 109
      Height = 21
      EditMask = '99/99/99;1;_'
      MaxLength = 8
      TabOrder = 6
      Text = '24/07/91'
    end
    object MaskEdit4: TMaskEdit
      Left = 149
      Top = 225
      Width = 110
      Height = 21
      EditMask = '99/99/9999;1;_'
      MaxLength = 10
      TabOrder = 7
      Text = '24/07/1991'
    end
    object MaskEdit5: TMaskEdit
      Left = 10
      Top = 267
      Width = 109
      Height = 21
      EditMask = '99/99/99;1;_'
      MaxLength = 8
      TabOrder = 8
      Text = '99/99/99'
    end
    object MaskEdit6: TMaskEdit
      Left = 149
      Top = 267
      Width = 110
      Height = 21
      EditMask = '99/99/9999;1;_'
      MaxLength = 10
      TabOrder = 9
      Text = '99/99/9999'
    end
    object RadioGroup1: TRadioGroup
      Left = 2
      Top = 15
      Width = 261
      Height = 109
      Align = alTop
      Caption = 'Types of Icon'
      ItemIndex = 0
      Items.Strings = (
        'Info'
        'Info Large'
        'Warning'
        'Large Warning'
        'Error'
        'Large Error'
        'None')
      TabOrder = 10
      OnClick = RadioGroup1Click
    end
    object ColorBox2: TColorBox
      Left = 80
      Top = 130
      Width = 89
      Height = 22
      DefaultColorColor = clRed
      TabOrder = 11
    end
    object Button3: TButton
      Left = 175
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Aplicar'
      TabOrder = 12
      OnClick = Button3Click
    end
  end
  object ColorBox1: TColorBox
    Left = 401
    Top = 32
    Width = 162
    Height = 22
    TabOrder = 0
  end
  object Button2: TButton
    Left = 569
    Top = 29
    Width = 75
    Height = 25
    Caption = 'Aplicar'
    TabOrder = 1
    OnClick = Button2Click
  end
  object DBGrid1: TDBGrid
    Left = 401
    Top = 60
    Width = 468
    Height = 340
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object RadioGroup2: TRadioGroup
    Left = 0
    Top = 0
    Width = 121
    Height = 408
    Align = alLeft
    Caption = 'Tipos de Valida'#231#227'o'
    ItemIndex = 0
    Items.Strings = (
      'Default'
      'Bal'#227'o em 1'
      'Bal'#227'o em todos'
      'Pintar em 1'
      'Pintar em todos')
    TabOrder = 4
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 789
    Top = 8
    object ClientDataSet1id: TAutoIncField
      FieldName = 'id'
    end
    object ClientDataSet1nome: TStringField
      FieldName = 'nome'
      Size = 50
    end
    object ClientDataSet1idade: TIntegerField
      FieldName = 'idade'
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 829
    Top = 8
  end
end
