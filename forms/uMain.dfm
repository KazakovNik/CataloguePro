object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'CataloguePro'
  ClientHeight = 351
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
  OnResize = FormResize
  PixelsPerInch = 106
  TextHeight = 14
  object Splitter: TSplitter
    Left = 281
    Top = 24
    Width = 6
    Height = 327
    ExplicitHeight = 273
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
  object TreeView: TTreeView
    Left = 332
    Top = 24
    Width = 303
    Height = 327
    Align = alClient
    HideSelection = False
    Indent = 19
    PopupMenu = pmTree
    TabOrder = 1
  end
  object lbHeap: TListBox
    Left = 0
    Top = 24
    Width = 281
    Height = 327
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 14
    TabOrder = 2
  end
  object pnlСenter: TPanel
    Left = 287
    Top = 24
    Width = 45
    Height = 327
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
    object pnlСenterBtn: TPanel
      Left = 0
      Top = 0
      Width = 42
      Height = 81
      BevelOuter = bvNone
      TabOrder = 0
      object btnInsert: TButton
        Left = 6
        Top = 14
        Width = 30
        Height = 25
        Action = actInsertData
        TabOrder = 0
      end
      object btnReturnBack: TButton
        Left = 6
        Top = 45
        Width = 30
        Height = 25
        Action = actReturnBack
        TabOrder = 1
      end
    end
  end
  object ImageList1: TImageList
    Height = 24
    Width = 24
    Left = 136
    Top = 56
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actLoadFile
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 136
    Top = 120
    StyleName = 'Platform Default'
    object actLoadFile: TAction
      Caption = #55357#56514' '#1054#1090#1082#1088#1099#1090#1100' '#1092#1072#1081#1083
      Hint = #1054#1090#1082#1088#1099#1090#1100' '#1092#1072#1081#1083' '#1089' '#1085#1077' '#1086#1073#1088#1072#1073#1086#1090#1072#1085#1099#1084' '#1089#1087#1080#1089#1082#1086#1084
      OnExecute = actLoadFileExecute
    end
    object actAddNode: TAction
      Caption = #10010' '#1044#1086#1073#1072#1074#1080#1090#1100
      OnExecute = actAddNodeExecute
    end
    object actAddRootNode: TAction
      Caption = #55357#56636#10010' '#1044#1086#1073#1072#1074#1080#1090#1100' '#1074' '#1085#1072#1095#1072#1083#1086
      OnExecute = actAddRootNodeExecute
    end
    object actEditNode: TAction
      Caption = #9999#65039' '#1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
      OnExecute = actEditNodeExecute
    end
    object actDeleteNode: TAction
      Caption = #8617#65039' '#1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074' '#1089#1087#1080#1089#1086#1082
      OnExecute = actDeleteNodeExecute
    end
    object actSaveTreeToFile: TAction
      Caption = #55357#56510' '#1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1076#1077#1088#1077#1074#1086' '#1074' '#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083
      OnExecute = actSaveTreeToFileExecute
    end
    object actInsertData: TAction
      Caption = #10010
      Hint = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074' '#1076#1077#1088#1077#1074#1086
      OnExecute = actInsertDataExecute
      OnUpdate = actInsertDataUpdate
    end
    object actReturnBack: TAction
      Caption = #8617#65039
      Hint = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074' '#1089#1087#1080#1089#1086#1082
      OnExecute = actReturnBackExecute
      OnUpdate = actReturnBackUpdate
    end
    object actSettings: TAction
      Caption = #9881#65039' '#1053#1072#1089#1090#1088#1086#1081#1082#1080
    end
  end
  object OpenDialog: TOpenDialog
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083'|*.txt'
    Left = 40
    Top = 48
  end
  object dlgSaveTextFile: TSaveDialog
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083'|*.txt'
    Left = 40
    Top = 104
  end
  object pmTree: TPopupMenu
    Left = 432
    Top = 200
    object actAddNode1: TMenuItem
      Action = actAddNode
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    end
    object actAddRootNode1: TMenuItem
      Action = actAddRootNode
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1074' '#1085#1072#1095#1072#1083#1086
    end
    object actEditNode1: TMenuItem
      Action = actEditNode
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
    end
    object actDeleteNode1: TMenuItem
      Action = actDeleteNode
      Caption = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074' '#1089#1087#1080#1089#1086#1082
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object actSaveTreeToFile1: TMenuItem
      Action = actSaveTreeToFile
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
    end
  end
end
