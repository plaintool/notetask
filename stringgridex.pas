unit stringgridex;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids, Messages, LMessages;

type
  TStringGridEx = class(TStringGrid)
  private
    FOnScroll: TNotifyEvent;  // Custom scroll event
  protected
    procedure WndProc(var Msg: TLMessage); override;  // Override WndProc to handle messages
  published
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;  // Event property
  end;

implementation

procedure Register;
begin
  RegisterComponents('MyComponents', [TStringGridEx]);  // Register the component
end;

procedure TStringGridEx.WndProc(var Msg: TLMessage);
begin
  inherited WndProc(Msg);  // Call the inherited WndProc for standard message processing

  // Handle scroll messages
  case Msg.msg of
    LM_HScroll, LM_VScroll:
    begin
      // Trigger the OnScroll event if assigned
      if Assigned(FOnScroll) then
        FOnScroll(Self);  // Call the custom event
    end;
  end;
end;

end.

