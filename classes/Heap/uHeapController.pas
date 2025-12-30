unit uHeapController;

interface

uses
  Vcl.StdCtrls, uILogger;

type
  THeapController = class
  private
    FListBox: TListBox;
    FLogger: ILogger;
  public
    constructor Create(ListBox: TListBox; Logger: ILogger);
    destructor Destroy(); override;

    procedure LoadFile(filename: string);
    procedure Delete(itemIndex: integer);
    procedure Add(text: string);
    function GetCurrentItem: string;
    procedure DeleteCurrent;
  end;

implementation

uses
  System.SysUtils, System.Classes;

procedure THeapController.Add(text: string);
begin
  FListBox.Items.Add(Text);
  FLogger.AddInfo('Добавили запись в кучу: ' + text);
end;

constructor THeapController.Create(ListBox: TListBox; Logger: ILogger);
begin
  inherited Create();
  FLogger := Logger;
  FListBox := ListBox;
end;

procedure THeapController.Delete(itemIndex: integer);
var
  index: integer;
  oldText: string;
begin
  index := itemIndex;
  oldText := FListBox.Items[index];
  FListBox.Items.Delete(index);
  if index > 0 then
    index := index - 1;
  if index <= FListBox.Count - 1 then
    FListBox.ItemIndex := index;
  FLogger.AddInfo('Удалили запись из кучи: ' + oldText);
end;

procedure THeapController.DeleteCurrent;
begin
  Delete(FListBox.ItemIndex);
end;

destructor THeapController.Destroy();
begin
  inherited;
end;

function THeapController.GetCurrentItem: string;
begin
  Result:= FListBox.Items[FListBox.ItemIndex];
end;

procedure THeapController.LoadFile(filename: string);
var
  sl: TStringList;
begin
  if not FileExists(filename) then
  begin
    raise Exception.Create('Файл не найден:'#13#10 + filename);
    FLogger.AddError('Файл не найден:' + filename);
  end;

  sl := TStringList.Create();
  try
    sl.LoadFromFile(filename);

    FListBox.Items.BeginUpdate;
    try
      FListBox.Items.Clear;
      FListBox.Items.Text := sl.Text;
    finally
      FListBox.Items.EndUpdate;
    end;
    if FListBox.Count > 0 then
      FListBox.ItemIndex := 0;
  finally
    sl.Free;
  end;
  FLogger.AddInfo('Загрузили файл: ' + filename);
end;

end.
