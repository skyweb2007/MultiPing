unit mpClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pingsend;

type

  TmpLogPingStatus = procedure(ip: string; status: boolean) of object;

  { TmpThread }

  TmpThread = class(TThread)
  private
    FIPAddr: string;
    FPingStatus: boolean;
    FLogProc: TmpLogPingStatus;
    procedure LogStatus;
  public
    constructor CreatePinger(const ip: string; logproc: TmpLogPingStatus);
    procedure Execute; override;
    property IPAddr: string read FIPAddr;
    property PingStatus: boolean read FPingStatus;
  end;

procedure PingIpSeg(ipseg: string; logproc: TmpLogPingStatus);

implementation

procedure PingIpSeg(ipseg: string; logproc: TmpLogPingStatus);
var
  i: integer;
  thread: TmpThread;
begin
  for i := 1 to 254 do
  begin
    thread := TmpThread.CreatePinger(Format('%s.%d', [ipseg, i]), logproc);
    thread.Start;
  end;
end;

{ TmpThread }

procedure TmpThread.LogStatus;
begin
  FLogProc(FIPAddr, FPingStatus);
end;

constructor TmpThread.CreatePinger(const ip: string; logproc: TmpLogPingStatus);
begin
  inherited Create(true);
  FreeOnTerminate := True;
  FIPAddr := ip;
  FLogProc := logproc;
end;

procedure TmpThread.Execute;
begin
  with TPINGSend.Create do
  try
    FPingStatus := Ping(FIPAddr) and (ReplyError = IE_NoError);
    Synchronize(@LogStatus);
  finally
    Free;
  end;
end;

end.

