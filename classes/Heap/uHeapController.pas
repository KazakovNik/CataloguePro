unit uHeapController;

interface

uses
  Vcl.StdCtrls;

type
  THeapController = class
  private
    FListBox: TListBox;
  public
    constructor Create(ListBox: TListBox);
    destructor Destroy(); override;

    procedure LoadFile(filename: string);
    procedure Delete(itemIndex: integer);
  end;

implementation

uses
  System.SysUtils, System.Classes;

constructor THeapController.Create(ListBox: TListBox);
begin
  inherited Create();

  FListBox := ListBox;
end;

procedure THeapController.Delete(itemIndex: integer);
var
  index: integer;
begin
  index := itemIndex;
  FListBox.Items.Delete(index);
  if index > 0 then
    index := index - 1;
  if index <= FListBox.Count - 1 then
    FListBox.ItemIndex := index;
end;

destructor THeapController.Destroy();
begin
  inherited;
end;

procedure THeapController.LoadFile(filename: string);
var
  sl: TStringList;
begin
  if not FileExists(filename) then
    raise Exception.Create('Файл не найден:'#13#10 + filename);

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
end;

end.
