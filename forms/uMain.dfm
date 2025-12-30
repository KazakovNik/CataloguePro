object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 297
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 281
    Top = 24
    Width = 6
    Height = 273
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 635
    Height = 24
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object TreeView1: TTreeView
    Left = 287
    Top = 24
    Width = 348
    Height = 273
    Align = alClient
    Indent = 19
    TabOrder = 1
    ExplicitLeft = 432
    ExplicitTop = 88
    ExplicitWidth = 121
    ExplicitHeight = 97
  end
  object lbHeap: TListBox
    Left = 0
    Top = 24
    Width = 281
    Height = 273
    Align = alLeft
    ItemHeight = 14
    TabOrder = 2
  end
  object ImageList1: TImageList
    Height = 24
    Width = 24
    Left = 480
    Top = 64
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actLoadFile
            Caption = '&actLoadFile'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 504
    Top = 192
    StyleName = 'Platform Default'
    object actLoadFile: TAction
      Caption = 'actLoadFile'
      OnExecute = actLoadFileExecute
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083'|*.txt'
    Left = 128
    Top = 32
  end
end
