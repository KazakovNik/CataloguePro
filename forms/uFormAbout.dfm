inherited FormAbout: TFormAbout
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 162
  ClientWidth = 398
  OnCreate = FormCreate
  ExplicitWidth = 404
  ExplicitHeight = 193
  PixelsPerInch = 106
  TextHeight = 14
  object lblProgramName: TLabel [0]
    Left = 128
    Top = 16
    Width = 87
    Height = 14
    Caption = 'lblProgramName'
  end
  object lblVersion: TLabel [1]
    Left = 128
    Top = 39
    Width = 51
    Height = 14
    Caption = 'lblVersion'
  end
  object lblCopyright: TLabel [2]
    Left = 128
    Top = 63
    Width = 63
    Height = 14
    Caption = 'lblCopyright'
  end
  object lblUrl: TLabel [3]
    Left = 128
    Top = 87
    Width = 25
    Height = 14
    Cursor = crHandPoint
    Caption = 'lblUrl'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblUrlClick
  end
  object Label1: TLabel [4]
    Left = 32
    Top = 16
    Width = 56
    Height = 14
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077':'
  end
  object Label2: TLabel [5]
    Left = 32
    Top = 39
    Width = 45
    Height = 14
    Caption = #1042#1077#1088#1089#1080#1103':'
  end
  object Label3: TLabel [6]
    Left = 32
    Top = 63
    Width = 75
    Height = 14
    Caption = #1056#1072#1079#1088#1072#1073#1086#1090#1095#1080#1082':'
  end
  object Label4: TLabel [7]
    Left = 32
    Top = 87
    Width = 78
    Height = 14
    Cursor = crHandPoint
    Caption = #1056#1077#1087#1086#1079#1080#1090#1086#1088#1080#1081':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = lblUrlClick
  end
  inherited pnlBtnPanel: TPanel
    Top = 118
    Width = 392
    ExplicitLeft = 3
    ExplicitTop = 118
    ExplicitWidth = 392
    inherited btnOk: TButton
      Left = 240
      ExplicitLeft = 240
      ExplicitTop = 6
      ExplicitHeight = 29
    end
    inherited btnCancel: TButton
      Left = 316
      Visible = False
      ExplicitLeft = 316
      ExplicitTop = 6
      ExplicitHeight = 29
    end
  end
end
