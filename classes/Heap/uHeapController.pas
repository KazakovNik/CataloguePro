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
    constructor Create(aListBox: TListBox; aLogger: ILogger);
    destructor Destroy(); override;

    procedure LoadFile(aFileName: string);
    procedure Delete(aItemIndex: integer);
    procedure Add(aText: string);
    procedure DeleteCurrent;
    procedure Clear;

    function GetCurrentItem: string;
    function IsEmpty: Boolean;
  end;

implementation

uses
  System.SysUtils, System.Classes, uRsControls;

procedure THeapController.Add(aText: string);
begin
  FListBox.Items.Add(aText);
  FLogger.AddInfo(resHeapAdd + aText);
end;

procedure THeapController.Clear;
begin
  FLogger.AddInfo(resHeapClear);

  FListBox.Items.Clear;
end;

constructor THeapController.Create(aListBox: TListBox; aLogger: ILogger);
begin
  inherited Create();
  FLogger := aLogger;
  FListBox := aListBox;
end;

procedure THeapController.Delete(aItemIndex: integer);
var
  vIndex: integer;
  vOldText: string;
begin
  vIndex := aItemIndex;
  vOldText := FListBox.Items[vIndex];
  FListBox.Items.Delete(vIndex);
  if vIndex > 0 then
    vIndex := vIndex - 1;
  if vIndex <= FListBox.Count - 1 then
    FListBox.ItemIndex := vIndex;
  FLogger.AddInfo(resHeapDelete + vOldText);
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

function THeapController.IsEmpty: Boolean;
begin
  Result := FListBox.Items.Count = 0;
end;

procedure THeapController.LoadFile(aFileName: string);
var
  vFile: TStringList;
begin
  if not FileExists(aFileName) then
    raise Exception.Create(resHeapFileNotFound + #13#10 + aFileName);

  vFile := TStringList.Create();
  try
    vFile.LoadFromFile(aFileName);

    FListBox.Items.BeginUpdate;
    try
      FListBox.Items.Clear;
      FListBox.Items.Text := vFile.Text;
    finally
      FListBox.Items.EndUpdate;
    end;
    if FListBox.Count > 0 then
      FListBox.ItemIndex := 0;
  finally
    vFile.Free;
  end;
  FLogger.AddInfo(resHeapLoadFile + aFileName);
end;

end.
