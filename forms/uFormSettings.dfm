inherited FormSettings: TFormSettings
  Caption = 'FormSettings'
  ClientHeight = 344
  ExplicitHeight = 375
  PixelsPerInch = 106
  TextHeight = 14
  inherited pnlBtnPanel: TPanel
    Top = 300
    ExplicitTop = 303
  end
  inline frmLastOpen: TFrame1
    Left = 0
    Top = 0
    Width = 513
    Height = 76
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 515
    inherited lblCurrentDirOpen: TLabel
      Width = 102
      Caption = #1050#1072#1090#1072#1083#1086#1075' '#1086#1090#1082#1088#1099#1090#1080#1103
      ExplicitWidth = 102
    end
    inherited edtPatch: TEdit
      Width = 413
      ExplicitWidth = 415
    end
    inherited btnOpenDir: TButton
      Left = 429
      Caption = #1054#1090#1082#1088#1099#1090#1100
      ExplicitLeft = 431
    end
  end
  inline frmLastSave: TFrame1
    Left = 0
    Top = 76
    Width = 513
    Height = 76
    Align = alTop
    TabOrder = 2
    ExplicitTop = 76
    ExplicitWidth = 515
    inherited lblCurrentDirOpen: TLabel
      Width = 115
      Caption = #1050#1072#1090#1072#1083#1086#1075' '#1089#1086#1093#1088#1072#1085#1077#1085#1080#1103
      ExplicitWidth = 115
    end
    inherited edtPatch: TEdit
      Width = 413
      ExplicitWidth = 415
    end
    inherited btnOpenDir: TButton
      Left = 429
      Caption = #1054#1090#1082#1088#1099#1090#1100
      ExplicitLeft = 431
    end
  end
  inline frmLogFile: TFrame1
    Left = 0
    Top = 152
    Width = 513
    Height = 76
    Align = alTop
    TabOrder = 3
    ExplicitTop = 152
    ExplicitWidth = 515
    inherited lblCurrentDirOpen: TLabel
      Width = 20
      Caption = #1051#1086#1075
      ExplicitWidth = 20
    end
    inherited edtPatch: TEdit
      Width = 413
      ExplicitWidth = 415
    end
    inherited btnOpenDir: TButton
      Left = 429
      Caption = #1054#1090#1082#1088#1099#1090#1100
      ExplicitLeft = 431
    end
  end
  object pnlMaxCountFileHistopy: TPanel
    Left = 0
    Top = 228
    Width = 513
    Height = 76
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitWidth = 515
    object lblCurrentDirOpen: TLabel
      Left = 8
      Top = 8
      Width = 246
      Height = 14
      Caption = #1048#1089#1090#1086#1088#1080#1103' '#1086#1090#1082#1088#1099#1090#1080#1103'. '#1047#1072#1087#1086#1084#1080#1085#1072#1090#1100' '#1087#1086#1089#1083#1077#1076#1085#1080#1080
    end
    object edtMaxCountFileHistopy: TEdit
      Left = 8
      Top = 28
      Width = 185
      Height = 22
      TabOrder = 0
      Text = '10'
      OnKeyPress = edtMaxCountFileHistopyKeyPress
    end
  end
end
