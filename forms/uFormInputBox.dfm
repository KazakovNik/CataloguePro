inherited FormInputBox: TFormInputBox
  Caption = 'FormInputBox'
  ClientHeight = 102
  ClientWidth = 476
  ExplicitWidth = 482
  ExplicitHeight = 133
  PixelsPerInch = 106
  TextHeight = 14
  object lblTextLabel: TLabel [0]
    Left = 8
    Top = 8
    Width = 65
    Height = 14
    Caption = 'lblTextLabel'
  end
  inherited pnlBtnPanel: TPanel
    Top = 58
    Width = 470
    ParentBackground = False
    ExplicitTop = 58
    ExplicitWidth = 470
    inherited btnOk: TButton
      Left = 318
      ExplicitLeft = 318
    end
    inherited btnCancel: TButton
      Left = 394
      ExplicitLeft = 394
    end
  end
  object edtText: TEdit
    Left = 8
    Top = 28
    Width = 449
    Height = 22
    TabOrder = 1
    Text = 'edtText'
  end
end
