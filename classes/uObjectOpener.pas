unit uObjectOpener;

interface

type
  TObjectOpener = class
  public
    class procedure OpenFile(aFileName: string);
    class procedure OpenFolder(aPath: string);
    class procedure OpenUrl(aUrl: string);
  end;

implementation

uses
  ShellAPI, Winapi.Windows;

{ TObjectOpener }

class procedure TObjectOpener.OpenFile(aFileName: string);
begin
  ShellExecute(0, 'open', PChar(aFileName), nil, nil, SW_SHOWNORMAL);
end;

class procedure TObjectOpener.OpenFolder(aPath: string);
begin
  ShellExecute(0, 'explore', PChar(aPath), nil, nil, SW_SHOWNORMAL)
end;

class procedure TObjectOpener.OpenUrl(aUrl: string);
begin
  ShellExecute(0, 'open', PChar(aUrl), nil, nil, SW_SHOWNORMAL);
end;

end.
