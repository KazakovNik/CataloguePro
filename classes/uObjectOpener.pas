unit uObjectOpener;

interface

type
  TObjectOpener = class
  public
    class procedure OpenFile(filename: string);
    class procedure OpenFolder(patch: string);
    class procedure OpenUrl(url: string);
  end;

implementation

uses
  ShellAPI, Winapi.Windows;

{ TObjectOpener }

class procedure TObjectOpener.OpenFile(filename: string);
begin
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOWNORMAL);
end;

class procedure TObjectOpener.OpenFolder(patch: string);
begin
  ShellExecute(0, 'explore', PChar(patch), nil, nil, SW_SHOWNORMAL)
end;

class procedure TObjectOpener.OpenUrl(url: string);
begin
  ShellExecute(0, 'open', PChar(url), nil, nil, SW_SHOWNORMAL);
end;

end.
