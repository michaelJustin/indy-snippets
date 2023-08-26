unit Unit1;

interface

uses
  IdCustomTCPServer, IdTCPClient, IdContext,
  SysUtils, Classes, Forms, StdCtrls, Controls;

type
  TMyTCPClientThread = class(TThread)
  private
    TCPClient: TIdTCPClient;
    FClientLog: TStrings;
    FClientRequestHolder: TEdit;
    procedure LogInMainThread(Line1, Line2: string);
  public
    constructor Create(AHost: string; APort: Word; ALog: TStrings;
      ARequestHolder: TEdit);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TMyTCPServer = class(TIdCustomTCPServer)
  private
    FServerLog: TStrings;
    FServerRequestHolder: TEdit;
    procedure LogInMainThread(Line1, Line2: string);
  protected
    function DoExecute(AContext: TIdContext): Boolean; override;
  public
    constructor Create(AServerLog: TStrings; ARequestHolder: TEdit);
  end;

  TExampleForm = class(TForm)
    ClientLog: TMemo;
    ServerLog: TMemo;
    ClientSendMessage: TButton;
    ServerSendMessage: TButton;
    ClientRequest: TEdit;
    ServerRequest: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClientSendMessageClick(Sender: TObject);
    procedure ServerSendMessageClick(Sender: TObject);
  private
    ExampleClient: TMyTCPClientThread;
    ExampleServer: TMyTCPServer;
  end;

var
  ExampleForm: TExampleForm;

implementation

uses
  IdGlobal, StrUtils;

const
  READ_TIMEOUT = 200;

{$R *.dfm}

procedure TExampleForm.ClientSendMessageClick(Sender: TObject);
begin
  ClientRequest.ReadOnly := True;
end;

procedure TExampleForm.FormCreate(Sender: TObject);
begin
  ExampleServer := TMyTCPServer.Create(ServerLog.Lines, ServerRequest);
  ExampleServer.DefaultPort := 8088;
  ExampleServer.Active := True;

  ExampleClient := TMyTCPClientThread.Create('localhost', 8088,
    ClientLog.Lines, ClientRequest);
end;

procedure TExampleForm.FormDestroy(Sender: TObject);
begin
  ExampleServer.Free;
  ExampleClient.Terminate;
  ExampleClient.WaitFor;
  ExampleClient.Free;
end;

procedure TExampleForm.ServerSendMessageClick(Sender: TObject);
begin
  ServerRequest.ReadOnly := True;
end;

{ TMyTCPServer }

constructor TMyTCPServer.Create(AServerLog: TStrings; ARequestHolder: TEdit);
begin
  inherited Create;

  FServerLog := AServerLog;
  FServerRequestHolder := ARequestHolder;
end;

procedure TMyTCPServer.LogInMainThread(Line1, Line2: string);
begin
  TThread.Synchronize(nil,
   procedure
   begin
     FServerLog.Append('Request: ' + Line1);
     FServerLog.Append('Response: ' + Line2);
  end);
end;

function TMyTCPServer.DoExecute(AContext: TIdContext): Boolean;
var
  Request: string;
  Response: string;
begin
  Result := inherited;

  // server sends request to client and receives response from client
  if FServerRequestHolder.ReadOnly then
  begin
    TThread.Synchronize(nil,
    procedure
    begin
      FServerRequestHolder.ReadOnly := False;
    end);
    Request := FServerRequestHolder.Text;
    AContext.Connection.IOHandler.WriteLn(Request);
    Response := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    if AContext.Connection.IOHandler.ReadLnTimedout then
      LogInMainThread(Request, '* read timed out *')
    else
      LogInMainThread(Request, Response);
  end;

  // server receives request from client and sends response to client
  if AContext.Connection.IOHandler.CheckForDataOnSource(READ_TIMEOUT) then
  begin
    Request := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    if not AContext.Connection.IOHandler.ReadLnTimedout then
    begin
      Response := ReverseString(Request);
      LogInMainThread(Request, Response);
      AContext.Connection.IOHandler.WriteLn(Response);
    end;
  end;
end;

{ TMyTCPClientThread }

constructor TMyTCPClientThread.Create;
begin
  inherited Create(False);

  FClientLog := ALog;
  FClientRequestHolder := ARequestHolder;

  TCPClient := TIdTCPClient.Create;
  TCPClient.Host := AHost;
  TCPClient.Port := APort;
  TCPClient.ReadTimeout := READ_TIMEOUT;
end;

destructor TMyTCPClientThread.Destroy;
begin
  TCPClient.Free;
  inherited;
end;

procedure TMyTCPClientThread.LogInMainThread(Line1, Line2: string);
begin
  TThread.Synchronize(nil,
  procedure
  begin
     FClientLog.Append('Request: ' + Line1);
     FClientLog.Append('Response: ' + Line2);
  end);
end;

procedure TMyTCPClientThread.Execute;
var
  Request: string;
  Response: string;
begin
  TCPClient.Connect;

  while not Terminated do
  begin
    if FClientRequestHolder.ReadOnly then
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        FClientRequestHolder.ReadOnly := False;
      end);
      // send request to server and receive response
      Request := FClientRequestHolder.Text;
      TCPClient.IOHandler.WriteLn(Request);
      Response := TCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
      if TCPClient.IOHandler.ReadLnTimedout then
        LogInMainThread(Request, '* read timed out *')
      else
        LogInMainThread(Request, Response);
    end
    else
    begin
      // receive request from server and send response
      Request := TCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
      if not TCPClient.IOHandler.ReadLnTimedout then
      begin
        Response := ReverseString(Request);
        TCPClient.IOHandler.WriteLn(Response);
        LogInMainThread(Request, Response);
      end;
    end;
  end;

  TCPClient.Disconnect;
end;

end.
