(*
   Copyright 2016 Michael Justin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

unit Unit1;

interface

uses
  IdCustomTCPServer, IdTCPClient, IdContext,
  SysUtils, Classes, Forms, StdCtrls, Controls;

type
  TMyPushClientThread = class(TThread)
  private
    TCPClient: TIdTCPClient;
    FLog: TStrings;
  public
    constructor Create(AHost: string; APort: Word; ALog: TStrings);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TMyPushServer = class (TIdCustomTCPServer)
  protected
    function DoExecute(AContext: TIdContext): Boolean; override;
  end;

  TServerPushExampleForm = class(TForm)
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ExampleClient: TMyPushClientThread;
    ExampleServer: TMyPushServer;
  end;

var
  ServerPushExampleForm: TServerPushExampleForm;

implementation

uses
  IdGlobal;

{$R *.dfm}

procedure TServerPushExampleForm.FormCreate(Sender: TObject);
begin
  ExampleServer := TMyPushServer.Create;
  ExampleServer.DefaultPort := 8088;
  ExampleServer.Active := True;

  ExampleClient := TMyPushClientThread.Create('localhost', 8088, MemoLog.Lines);
end;

procedure TServerPushExampleForm.FormDestroy(Sender: TObject);
begin
  ExampleServer.Free;
  ExampleClient.Terminate;
  ExampleClient.WaitFor;
  ExampleClient.Free;
end;

{ TMyPushServer }

function TMyPushServer.DoExecute(AContext: TIdContext): Boolean;
begin
  Result := inherited;

  // simulate hard work
  Sleep(Random(3000));

  AContext.Connection.IOHandler.WriteLn(
    'Completed at ' + TimeToStr(Now), IndyTextEncoding_UTF8);
end;

{ TMyPushClientThread }

constructor TMyPushClientThread.Create(AHost: string; APort: Word; ALog: TStrings);
begin
  inherited Create(False);

  FLog := ALog;

  TCPClient := TIdTCPClient.Create;
  TCPClient.Host := AHost;
  TCPClient.Port := APort;
  TCPClient.ReadTimeout := 500;
end;

destructor TMyPushClientThread.Destroy;
begin
  TCPClient.Free;
  inherited;
end;

procedure TMyPushClientThread.Execute;
var
  S: string;
begin
  TCPClient.Connect;

  while not Terminated do
  begin
    S := TCPClient.IOHandler.ReadLn(IndyTextEncoding_UTF8);

    if not TCPClient.IOHandler.ReadLnTimedout then
    begin
      TThread.Queue(nil,
        procedure
        begin
          FLog.Append(S);
        end);
    end;

  end;

  TCPClient.Disconnect;
end;

end.
