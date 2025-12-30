unit uAboutFacade;

interface

uses
  uFormAbout;

type
  TAboutFacade = class
  public
    procedure ShowModal;
  end;

implementation

{ TAboutFacade }

procedure TAboutFacade.ShowModal;
var
  Form: TFormAbout;
begin
  Form := TFormAbout.Create(nil);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

end.
