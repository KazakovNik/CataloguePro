unit uAboutFacade;

interface

uses
  uFormAbout, uILogger;

type
  TAboutFacade = class
  private
    FLogger: ILogger;
    procedure lblUrlClick(Sender: TObject);
  private
    FForm: TFormAbout;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowModal;

    property Logger: ILogger read FLogger write FLogger;
  end;

implementation

uses
  uObjectOpener;

{ TAboutFacade }

procedure TAboutFacade.ShowModal;
begin
  FForm.ShowModal;
end;

constructor TAboutFacade.Create;
begin
  FForm := TFormAbout.Create(nil);
  FForm.lblUrl.OnClick := lblUrlClick;
end;

destructor TAboutFacade.Destroy;
begin
  FForm.Free;

  inherited;
end;

procedure TAboutFacade.lblUrlClick(Sender: TObject);
begin
  inherited;
  TObjectOpener.OpenUrl(FForm.lblUrl.Caption);
end;

end.
