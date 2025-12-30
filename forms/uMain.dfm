object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'CataloguePro'
  ClientHeight = 500
  ClientWidth = 800
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 106
  TextHeight = 14
  object Splitter: TSplitter
    Left = 265
    Top = 0
    Width = 6
    Height = 500
    ExplicitLeft = 281
    ExplicitTop = 24
    ExplicitHeight = 273
  end
  object pnlСenter: TPanel
    AlignWithMargins = True
    Left = 271
    Top = 41
    Width = 45
    Height = 459
    Margins.Left = 0
    Margins.Top = 41
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
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
        Caption = #8617#65039
        TabOrder = 1
      end
    end
  end
  object pnlHeap: TPanel
    Left = 0
    Top = 0
    Width = 265
    Height = 500
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'pnlHeap'
    TabOrder = 1
    object lbHeap: TListBox
      Left = 0
      Top = 41
      Width = 265
      Height = 459
      Style = lbOwnerDrawFixed
      Align = alClient
      DragMode = dmAutomatic
      ItemHeight = 14
      MultiSelect = True
      TabOrder = 0
      OnDragOver = lbHeapDragOver
    end
    object pnlHeaputils: TPanel
      Left = 0
      Top = 0
      Width = 265
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object btnLoadFile: TButton
        Left = 8
        Top = 10
        Width = 113
        Height = 25
        Action = actLoadFile
        TabOrder = 0
      end
    end
  end
  object pnlTree: TPanel
    Left = 316
    Top = 0
    Width = 484
    Height = 500
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlTree'
    TabOrder = 2
    object TreeView: TTreeView
      Left = 0
      Top = 41
      Width = 484
      Height = 459
      Align = alClient
      DragMode = dmAutomatic
      HideSelection = False
      Indent = 19
      PopupMenu = pmTree
      TabOrder = 0
      OnDragDrop = TreeViewDragDrop
      OnDragOver = TreeViewDragOver
      OnStartDrag = TreeViewStartDrag
    end
    object pnlTreeUnils: TPanel
      Left = 0
      Top = 0
      Width = 484
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
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
      end
      item
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
      Hint = #1044#1072#1073#1072#1074#1080#1090#1100' '#1075#1088#1091#1087#1087#1091' '#1074' '#1074#1099#1073#1088#1072#1085#1085#1086#1077
      OnExecute = actAddNodeExecute
      OnUpdate = actAddNodeUpdate
    end
    object actAddRootNode: TAction
      Caption = #55357#56636#10010' '#1044#1086#1073#1072#1074#1080#1090#1100' '#1074' '#1085#1072#1095#1072#1083#1086
      Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1074' '#1085#1072#1095#1072#1083#1086
      OnExecute = actAddRootNodeExecute
    end
    object actEditNode: TAction
      Caption = #9999#65039' '#1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
      Hint = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
      OnExecute = actEditNodeExecute
      OnUpdate = actEditNodeUpdate
    end
    object actSaveTreeToFile: TAction
      Caption = #55357#56510' '#1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1076#1077#1088#1077#1074#1086' '#1074' '#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083
      OnExecute = actSaveTreeToFileExecute
      OnUpdate = actSaveTreeToFileUpdate
    end
    object actInsertData: TAction
      Caption = #10010
      Hint = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074' '#1076#1077#1088#1077#1074#1086
      OnExecute = actInsertDataExecute
      OnUpdate = actInsertDataUpdate
    end
    object actReturnBack: TAction
      Caption = #8617#65039' '#1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074' '#1089#1087#1080#1089#1086#1082
      Hint = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074#1099#1073#1088#1072#1085#1085#1086#1077' '#1074' '#1089#1087#1080#1089#1086#1082
      OnExecute = actReturnBackExecute
      OnUpdate = actReturnBackUpdate
    end
    object actSettings: TAction
      Caption = #9881#65039' '#1053#1072#1089#1090#1088#1086#1081#1082#1080
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
      OnExecute = actSettingsExecute
    end
    object actExpandAll: TAction
      Caption = #8594' '#8593' '#1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Hint = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1076#1077#1088#1077#1074#1086
      OnExecute = actExpandAllExecute
      OnUpdate = actExpandAllUpdate
    end
    object actCollapseAll: TAction
      Caption = #8594' '#8595' '#1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Hint = #1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1076#1077#1088#1077#1074#1086
      OnExecute = actCollapseAllExecute
      OnUpdate = actCollapseAllUpdate
    end
    object actDeleteNode: TAction
      Caption = #10006#65039' '#1059#1076#1072#1083#1080#1090#1100
      Hint = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074#1099#1073#1088#1072#1085#1085#1086#1077' '#1074' '#1089#1087#1080#1089#1086#1082
      OnExecute = actDeleteNodeExecute
      OnUpdate = actDeleteNodeUpdate
    end
    object actAbout: TAction
      Caption = #55357#56492' '#1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      OnExecute = actAboutExecute
    end
    object actDeleteAllNode: TAction
      Caption = #10006#65039' '#1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1077
      Hint = #1055#1077#1088#1077#1085#1077#1089#1090#1080' '#1074#1089#1077' '#1074' '#1089#1087#1080#1089#1086#1082
      OnExecute = actDeleteAllNodeExecute
      OnUpdate = actDeleteAllNodeUpdate
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
    object N5: TMenuItem
      Action = actDeleteNode
      Caption = #1059#1076#1072#1083#1080#1090#1100
    end
    object N22: TMenuItem
      Action = actDeleteAllNode
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1077
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object N3: TMenuItem
      Action = actCollapseAll
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
    end
    object N4: TMenuItem
      Action = actExpandAll
      Caption = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object actSaveTreeToFile1: TMenuItem
      Action = actSaveTreeToFile
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
    end
  end
  object mmMain: TMainMenu
    Left = 48
    Top = 216
    object N6: TMenuItem
      Caption = #1060#1072#1081#1083
      object N7: TMenuItem
        Action = actLoadFile
      end
    end
    object N8: TMenuItem
      Caption = #1044#1077#1088#1077#1074#1086
      object N9: TMenuItem
        Action = actAddNode
      end
      object N10: TMenuItem
        Action = actAddRootNode
      end
      object N11: TMenuItem
        Action = actEditNode
      end
      object N12: TMenuItem
        Action = actReturnBack
      end
      object N13: TMenuItem
        Action = actDeleteNode
      end
      object N14: TMenuItem
        Action = actSaveTreeToFile
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object N16: TMenuItem
        Action = actExpandAll
      end
      object N17: TMenuItem
        Action = actCollapseAll
      end
    end
    object N18: TMenuItem
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      object N19: TMenuItem
        Action = actSettings
      end
    end
    object N20: TMenuItem
      Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      object N21: TMenuItem
        Action = actAbout
      end
    end
  end
end
