unit uHeapController;

interface

uses
  Vcl.StdCtrls;

type
  THeapController = class
  private
    FListBox: TListBox;
  private

  public
    constructor Create(ListBox: TListBox);
    destructor Destroy(); override;

    procedure LoadFile(filename: string);
  end;

implementation

uses
  System.SysUtils, System.Classes;

constructor THeapController.Create(ListBox: TListBox);
begin
  inherited Create();

  FListBox := ListBox;
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
    raise Exception.Create('Файл не найден');

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
