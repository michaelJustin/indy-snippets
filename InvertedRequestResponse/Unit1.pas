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
    Send: Boolean;

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
    Send: Boolean;
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
  READ_TIMEOUT = 100;

{$R *.dfm}

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

procedure TExampleForm.ClientSendMessageClick(Sender: TObject);
begin
  ExampleClient.Send := True;
end;

procedure TExampleForm.ServerSendMessageClick(Sender: TObject);
begin
  ExampleServer.Send := True;
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
  TThread.Queue(nil,
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

  AContext.Connection.IOHandler.ReadTimeout := READ_TIMEOUT;

  if Send then
  begin
    // server sends request to client and receives response from client
    Send := False;
    Request := 'REQ:' + FServerRequestHolder.Text;
    AContext.Connection.IOHandler.WriteLn(Request);
    repeat
      Response := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    until Response <> '';
    LogInMainThread(Request, Response);
  end
  else
  begin
    // server receives request from client and sends response to client
    Request := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    if StartsStr('REQ:', Request) then
    begin
      Response := 'from server ' + ReverseString(Request);
      AContext.Connection.IOHandler.WriteLn(Response);
    end;
  end;

  Sleep(100);
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
  TThread.Queue(nil,
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
    if Send then
    begin
      // send request to server and receive response
      Send := False;
      Request := 'REQ:' + FClientRequestHolder.Text;
      TCPClient.IOHandler.WriteLn(Request);
      repeat
        Response := TCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
      until Response <> '';
      LogInMainThread(Request, Response);
    end
    else
    begin
      // receive request from server and send response
      Request := TCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
      if StartsStr('REQ:', Request) then
      begin
        Response := 'from client ' + ReverseString(Request);
        TCPClient.IOHandler.WriteLn(Response);
      end;
    end;

    Sleep(100);
  end;

  TCPClient.Disconnect;
end;

end.
